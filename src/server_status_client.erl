-module(server_status_client).
-export([working/1, done/0, done_with/1, clear/0]).
-export([started_at/0, wall_clock_us/0, code/0]).
-export([state_dump/0, text_state_dump/0]).
-include("include/server_status.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(WORKER, server_status_worker).
-define(NUM_WIDTH, 32).
-define(PID_HEADING, "PID").
-define(STATE_HEADING, "State").
-define(STARTED_AT_HEADING, "Started at").
-define(ENDED_AT_HEADING, "Ended at").
-define(WALL_CLOCK_US_HEADING, "Wall clock [s]").
-define(HOST_HEADING, "Host").
-define(CODE_HEADING, "Code").
-define(PORT_HEADING, "Port").
-define(HOST_PORT_HEADING, "Host:Port").
-define(PATH_HEADING, "Path").
-define(QUERY_STRING_HEADING, "Query String").

%% -compile(export_all).

working([{path, Path}, {query_string, QueryString}]) ->
    Worker = #server_status{pid = self(),
                            state = working,
                            started_at = now(),
                            path = Path,
                            query_string = QueryString},
    gen_server:cast(?WORKER, {working, Worker#server_status.pid, Worker}).

done() ->
    gen_server:cast(?WORKER, {done, self()}).

done_with(Code) ->
    done(),
    gen_server:cast(?WORKER, {done_with, Code, self()}).

state_dump() ->
    gen_server:call(?WORKER, state_dump).

clear() ->
    gen_server:call(?WORKER, clear).

get_my_state() ->
    Workers = state_dump(),
    Worker = proplists:get_value(self(), Workers),
    Worker.

started_at() ->
    (get_my_state())#server_status.started_at.

wall_clock_us() ->
    (get_my_state())#server_status.wall_clock_us.

code() ->
    (get_my_state())#server_status.code.

flatten_format(Format, Paddings) ->
    lists:flatten(io_lib:format(Format, Paddings)).

get_field(pid,              #server_status{pid = R})            -> R;
get_field(state,            #server_status{state = R})          -> R;
get_field(started_at,       #server_status{started_at = R})     -> R;
get_field(ended_at,         #server_status{ended_at = R})       -> R;
get_field(wall_clock_us,    #server_status{wall_clock_us = R})  -> R;
get_field(code,             #server_status{code = R})           -> R;
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
to_string(State, state) ->
    atom_to_list(State);
to_string(StartedAt, started_at) ->
    now_to_time(StartedAt);
to_string(EndedAt, ended_at) ->
    now_to_time(EndedAt);
to_string(WallClockUs, wall_clock_us) ->
    flatten_format("~6.2. f", [WallClockUs / 1.0E6]);
to_string(Code, code) ->
    integer_to_list(Code);
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

format_count_summary(count_of_all, Count, MaxWidth) ->
    Defininition = "- count of all processes: ",
    flatten_format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(count_of_working, Count, MaxWidth) ->
    Defininition = "- count of working: ",
    flatten_format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(count_of_waiting, Count, MaxWidth) ->
    Defininition = "- count of waiting: ",
    flatten_format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean_of_working, Count, MaxWidth) ->
    Defininition = "- mean of working: ",
    flatten_format(get_float_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean_of_worked, Count, MaxWidth) ->
    Defininition = "- mean of worked: ",
    flatten_format(get_float_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean_of_both, Count, MaxWidth) ->
    Defininition = "- mean of both: ",
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
    CountOfAll = length(Workers),
    CountOfWorking = length([S || S <- Workers, S#server_status.state =:= working]),
    CountOfWaiting = length([S || S <- Workers, S#server_status.state =:= done])
                     + length([S || S <- Workers, S#server_status.state =:= undefined]),
    Workers2 = lists:map(fun(#server_status{state = working} = S) ->
                            S#server_status{wall_clock_us = timer:now_diff(Now, S#server_status.started_at)};
                        (S) ->
                            S
                        end,
                        Workers),
    MeanOfWorking = calc_mean([S#server_status.wall_clock_us
                               || S <- Workers2, S#server_status.state =:= working]),
    WorkingMeanSeconds = MeanOfWorking / 1.0E6,
    MeanOfWorked = calc_mean([S#server_status.wall_clock_us
                              || S <- Workers2, S#server_status.state =:= done]),
    WorkedMeanSeconds = MeanOfWorked / 1.0E6,
    MeanOfBoth = calc_mean([X || X <- [MeanOfWorking, MeanOfWorked], X > 0]),
    BothMeanSeconds = MeanOfBoth / 1.0E6,
    PidWidth = lists:max([length(X)
                          || X <- [?PID_HEADING | [to_string(P, pid) || #server_status{pid = P} <- Workers2]]]),
    StateWidth = lists:max([length(X)
                            || X <- [?STATE_HEADING | ["working", "done", "undefined"]]]),
    StartedAtWidth = lists:max([length(?STARTED_AT_HEADING), length(to_string(Now, started_at))]),
    EndedAtWidth = lists:max([length(?ENDED_AT_HEADING), length(to_string(Now, ended_at))]),
    WallClockWidth = length(?WALL_CLOCK_US_HEADING), % Wall clock heading is always longer than body.
    CodeWidth = lists:max([length(?CODE_HEADING), length("undefined")]),
    PathWidth = lists:max([length(X)
                           || X <- [?PATH_HEADING | [to_string(P, path) || #server_status{path = P} <- Workers2]]]),
    QueryWidth = lists:max([length(X)
                            || X <- [?QUERY_STRING_HEADING | [to_string(Q, query_string) || #server_status{query_string = Q} <- Workers2]]]),
    CountWidth = 6,
    Widths = [{pid, PidWidth},
              {state, StateWidth},
              {started_at, StartedAtWidth},
              {ended_at, EndedAtWidth},
              {wall_clock_us, WallClockWidth},
              {code, CodeWidth},
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
             format_count_summary(count_of_all, CountOfAll, CountWidth),
             format_count_summary(count_of_working, CountOfWorking, CountWidth),
             format_count_summary(count_of_waiting, CountOfWaiting, CountWidth),
             format_count_summary(mean_of_working, WorkingMeanSeconds, CountWidth),
             format_count_summary(mean_of_worked, WorkedMeanSeconds, CountWidth),
             format_count_summary(mean_of_both, BothMeanSeconds, CountWidth),
             "",
             "LIST PROCESSES",
             string:copies("=", length("LIST PROCESSES")),
             "",
             horizontal_line([V || {_K, V} <- Widths]),
             format_table([{?PID_HEADING, PidWidth, ""},
                           {?STATE_HEADING, StateWidth, ""},
                           {?STARTED_AT_HEADING, StartedAtWidth, ""},
                           {?ENDED_AT_HEADING, EndedAtWidth, ""},
                           {?WALL_CLOCK_US_HEADING, WallClockWidth, ""},
                           {?CODE_HEADING, CodeWidth, ""},
                           {?PATH_HEADING, PathWidth, ""},
                           {?QUERY_STRING_HEADING, QueryWidth, ""}]),
             horizontal_line([V || {_K, V} <- Widths])],
    Lines2 = [format_process(S, Widths) || S <- Workers2],
    Lines3 = Lines ++ Lines2 ++ [horizontal_line([V || {_K, V} <- Widths])] ++ [""],
    string:join(Lines3, "\n").

%% SERVER STATUS
%% =============
%%
%% Dumped at {{ local_time }}.
%%
%% SUMMARY
%% =======
%%
%% - count of all processes: {{ count_of_all }}
%% - count of working: {{ count_of_working }}
%% - count of waiting: {{ count_of_waiting }}
%% - mean of working: {{ mean_of_working }}
%% - mean of worked: {{ mean_of_worked }}
%% - mean of both: {{ mean_of_both }}
%%
%% LIST PROCESSES
%% ==============
%%
%% |-----+-------+------------+----------+---------------+-----------+------+-------|
%% | pid | state | started at | ended at | wallclock [s] | host:port | path | query |
%% |-----+-------+------------+----------+---------------+-----------+------+-------|
%% | xxx | xxxxx | xxxxxx     | xxxx     |           xxx | xxxx      | xxx  | xx    |
%% |-----+-------+------------+----------+---------------+-----------+------+-------|
