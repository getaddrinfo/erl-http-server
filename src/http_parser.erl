-module(http_parser).
-behaviour(gen_statem).

-export([callback_mode/0]).
-export([init/1, start_link/1]).
-export([process/2]).
-export([protocol/3, headers/3, body/3, finished/3, terminate/3]).

%% Client API
start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

process(Pid, Data) ->
    gen_statem:cast(Pid, {data, Data}).

%% Server API
callback_mode() -> state_functions.

init([ParentPid]) ->
    process_flag(trap_exit, true),
    {ok, protocol, #{
        parent => ParentPid,
        http_version => nil,
        path => nil,
        method => nil,
        remaining_content_length => nil,
        headers => #{}
    }}.

% We need to consider a few quirks when parsing a http message:
% - Headers or deliminators split across a packet boundary (e.g., [..., '\r\n'], ['\r\n', ...]) - Not sure how best to handle this?
% - Wrong `Content-Length` values (via Timeout)

% First step is to parse the protocol, path, and method.
protocol(cast, {data, Bin}, Data) ->
    [FirstLine, Rest] = binary:split(Bin, <<"\r\n">>),
    [RawMethod, RawPath, RawHttpVersion] = binary:split(FirstLine, <<" ">>, [global]),

    {ok, HttpVersion} = case RawHttpVersion of
        << "HTTP/1.1" >> -> {ok, '1_1'};
        << "HTTP/1.0" >> -> {ok, '1'};
        _ -> {error, no_match}
    end,

    {ok, Method} = case RawMethod of
        <<"GET">> -> {ok, get};
        <<"POST">> -> {ok, post};
        <<"PUT">> -> {ok, put};
        <<"PATCH">> -> {ok, patch};
        <<"DELETE">> -> {ok, delete};
        <<"OPTIONS">> -> {ok, options};
        _ -> {error, no_match}
    end,

    <<"/", RawPathRest/binary>> = RawPath,
    Path = binary:split(RawPathRest, <<"/">>, [global]),
    NewData = Data#{
        http_version := HttpVersion,
        path := Path,
        method := Method
    },

    {next_state, headers, NewData, [
        {next_event, cast, {data, Rest}}
    ]}.

headers(_, {data, Bin}, Data) ->
    [RawKeyValue, Rest] = binary:split(Bin, <<"\r\n">>),
    [Key, Value] = binary:split(RawKeyValue, <<": ">>),

    Headers = maps:get(headers, Data),
    NewHeaders = maps:put(string:lowercase(Key), Value, Headers),
    NewData = Data#{headers := NewHeaders},

    NextState = case Rest of
        <<"\r\n", _/binary>> -> body;
        <<_/binary>> -> headers
    end,

    Actions = case NextState of
        body ->
            <<"\r\n", Body/binary>> = Rest,
            [
                {next_event, cast, {internal, parse_content_length}},
                {next_event, cast, {data, Body}}
            ];
        headers ->
            [{next_event, cast, {data, Rest}}]
    end,

    {next_state, NextState, NewData, Actions}.

body(_, {internal, parse_content_length}, Data) ->
    RawContentLength = maps:get(<<"content-length">>, maps:get(headers, Data), nil),

    case RawContentLength of
        nil ->
            {next_state, finished, Data, [{next_event, cast, inform}]};
        Value ->
            {ContentLength, <<>>} = string:to_integer(Value),
            NextData = maps:put(remaining_content_length, ContentLength, Data),

            {keep_state, NextData}
    end;

body(_, {data, Bin}, Data) ->
    CurrentBody = maps:get(body, Data, <<>>),
    CurrentRemainingContentLength = maps:get(remaining_content_length, Data),

    BinaryLen = erlang:byte_size(Bin),
    NewRemainingContentLength = CurrentRemainingContentLength - BinaryLen,

    NewData = maps:put(data, <<CurrentBody/binary, Bin/binary>>, Data),
    NewData2 = NewData#{remaining_content_length := NewRemainingContentLength},

    case NewRemainingContentLength of
        0 -> {next_state, finished, NewData2, [{next_event, cast, inform}]};
        _ -> {keep_state, NewData2}
    end.

finished(_, {data, <<>>}, _Data) ->
    {keep_state_and_data, [{next_event, cast, inform}]};

finished(_, inform, Data) ->
    ParentPid = maps:get(parent, Data),
    http_server_conn:inform_ok(ParentPid),
    keep_state_and_data.

terminate(Reason, _State, Data) ->
    Pid = maps:get(parent, Data),

    Type = case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        Why -> {error, Why}
    end,

    case Type of
        {error, Reason} -> http_server_conn:inform_error(Pid, Reason);
        _ -> ok
    end.
