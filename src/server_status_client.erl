-module(server_status_client).
-export([working/1, done/0, clear/0]).
-export([state_dump/0, text_state_dump/0]).
-include("include/server_status.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(WORKER, server_status_worker).
-define(NUM_WIDTH, 12).
-define(PID_HEADING, "PID").
-define(STARTED_AT_HEADING, "Started at").
-define(WALL_CLOCK_US_HEADING, "Wall clock [s]").
-define(PATH_HEADING, "Path").
-define(QUERY_STRING_HEADING, "Query String").

working([{path, _Path} = Path, {query_string, _QueryString} = Qs]) ->
    working([Path, Qs, {started_at, now()}]);
working([{path, Path}, {query_string, QueryString}, {started_at, StartedAt}]) ->
    Worker = #server_status{pid = self(),
                            started_at = StartedAt,
                            path = Path,
                            query_string = QueryString},
    gen_server:cast(?WORKER, {working, Worker#server_status.pid, Worker}).

done() ->
    gen_server:cast(?WORKER, {done, self()}).

state_dump() ->
    gen_server:call(?WORKER, state_dump).

clear() ->
    gen_server:call(?WORKER, clear).

flatten_format(Format, Paddings) ->
    lists:flatten(io_lib:format(Format, Paddings)).

get_field(pid,              #server_status{pid = R})            -> R;
get_field(started_at,       #server_status{started_at = R})     -> R;
get_field(wall_clock_us,    #server_status{wall_clock_us = R})  -> R;
get_field(path,             #server_status{path = R})           -> R;
get_field(query_string,     #server_status{query_string = R})   -> R.

now_to_local_time(Now) ->
    {Ymd, Hms} = calendar:now_to_local_time(Now),
    String = string:join([integer_to_list(X) || X <- tuple_to_list(Ymd)], "-")
             ++ " "
             ++ string:join([integer_to_list(X) || X <- tuple_to_list(Hms)], ":"),
    String.

calc_mean([]) ->
    0;
calc_mean(Nums) ->
    lists:sum(Nums) / length(Nums).

now_to_time(Now) ->
    {_Ymd, Hms} = calendar:now_to_local_time(Now),
    String = string:join(lists:foldr(fun(I, L) ->
                                         [flatten_format("~2.2. w", [I]) | L]
                                     end,
                                     [],
                                     tuple_to_list(Hms)), ":"),
    String.

to_string(undefined, _Field) ->
    "undefined";
to_string(Pid, pid) ->
    pid_to_list(Pid);
to_string(StartedAt, started_at) ->
    now_to_time(StartedAt);
to_string(WallClockUs, wall_clock_us) ->
    flatten_format("~6.2. f", [WallClockUs / 1.0E6]);
to_string(Path, path) ->
    binary_to_list(Path);
to_string(QueryString, query_string) ->
    binary_to_list(QueryString).

get_float_format(MaxWidth) ->
    Format = flatten_format("~~.~w. s~~~w.2. f", [?NUM_WIDTH, MaxWidth]),
    Format.

get_int_format(MaxWidth) ->
    Format = flatten_format("~~.~w. s~~~w.2. w", [?NUM_WIDTH, MaxWidth]),
    Format.

format_count_summary(count, Count, MaxWidth) ->
    Defininition = "- count: ",
    flatten_format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean, Count, MaxWidth) ->
    Defininition = "- mean: ",
    flatten_format(get_float_format(MaxWidth), [Defininition, Count]).

horizontal_line(L) ->
    "|-" ++ string:join([string:copies("-", W) || W <- L], "-+-") ++ "-|".

format_table(Columns) ->
    SwitchFormat = fun(Value) when is_number(Value) ->
            "~~~ws";
        (_) ->
            "~~-~ws"
    end,
    Columns2 = [{S, flatten_format(SwitchFormat(V), [W])} || {S, W, V} <- Columns],
    Columns3 = [flatten_format(F, [S]) || {S, F} <- Columns2],
    Row = "| " ++ string:join([C || C <- Columns3], " | ") ++ " |",
    Row.

format_process(Worker, Widths) when is_record(Worker, server_status) ->
    Names = [K || {K, _V} <- Widths],
    Formatted = format_table([{to_string(get_field(N, Worker), N),
                               proplists:get_value(N, Widths),
                               get_field(N, Worker)}
                              || N <- Names]),
    Formatted.

text_state_dump() ->
    Workers = [W || {_Pid, W} <- state_dump()],
    Now = now(),
    Localtime = now_to_local_time(Now),
    Count = length(Workers),
    Workers2 = [S#server_status{wall_clock_us = timer:now_diff(Now, S#server_status.started_at)}
                || S <- Workers],
    Mean = calc_mean([S#server_status.wall_clock_us || S <- Workers2]),
    MeanSeconds = Mean / 1.0E6,
    PidWidth = lists:max([length(X)
                          || X <- [?PID_HEADING | [to_string(P, pid) || #server_status{pid = P} <- Workers2]]]),
    StartedAtWidth = lists:max([length(?STARTED_AT_HEADING), length(to_string(Now, started_at))]),
    WallClockWidth = length(?WALL_CLOCK_US_HEADING), % Wall clock heading is always longer than body.
    PathWidth = lists:max([length(X)
                           || X <- [?PATH_HEADING | [to_string(P, path) || #server_status{path = P} <- Workers2]]]),
    QueryWidth = lists:max([length(X)
                            || X <- [?QUERY_STRING_HEADING | [to_string(Q, query_string) || #server_status{query_string = Q} <- Workers2]]]),
    CountWidth = 6,
    Widths = [{pid, PidWidth},
              {started_at, StartedAtWidth},
              {wall_clock_us, WallClockWidth},
              {path, PathWidth},
              {query_string, QueryWidth}],
    Lines = ["SERVER STATUS",
             string:copies("=", length("SERVER STATUS")),
             "",
             flatten_format("Dumped at ~s", [Localtime]),
             "",
             "SUMMARY",
             string:copies("=", length("SUMMARY")),
             "",
             format_count_summary(count, Count, CountWidth),
             format_count_summary(mean, MeanSeconds, CountWidth),
             "",
             "LIST PROCESSES",
             string:copies("=", length("LIST PROCESSES")),
             "",
             horizontal_line([V || {_K, V} <- Widths]),
             format_table([{?PID_HEADING, PidWidth, ""},
                           {?STARTED_AT_HEADING, StartedAtWidth, ""},
                           {?WALL_CLOCK_US_HEADING, WallClockWidth, ""},
                           {?PATH_HEADING, PathWidth, ""},
                           {?QUERY_STRING_HEADING, QueryWidth, ""}]),
             horizontal_line([V || {_K, V} <- Widths])],
    Lines2 = [format_process(S, Widths) || S <- Workers2],
    Lines3 = Lines ++ Lines2 ++ [horizontal_line([V || {_K, V} <- Widths])] ++ [""],
    string:join(Lines3, "\n").

%% SERVER STATUS
%% =============
%% 
%% Dumped at 2013-10-17 2:58:26
%% 
%% SUMMARY
%% =======
%% 
%% - count:     1
%% - mean:   2.56
%% 
%% LIST PROCESSES
%% ==============
%% 
%% |----------+------------+----------------+------------+--------------|
%% | PID      | Started at | Wall clock [s] | Path       | Query String |
%% |----------+------------+----------------+------------+--------------|
%% | <0.32.0> |  2:58:24   |           2.56 | fooooooooo | ?bar=baz     |
%% |----------+------------+----------------+------------+--------------|
