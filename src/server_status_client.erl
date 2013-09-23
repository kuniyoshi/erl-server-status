-module(server_status_client).
-export([working/1, done/0, clear/0]).
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
-define(PORT_HEADING, "Port").
-define(HOST_PORT_HEADING, "Host:Port").
-define(PATH_HEADING, "Path").
-define(QUERY_STRING_HEADING, "Query String").

%% -compile(export_all).

working([{host, Host}, {port, Port}, {path, Path}, {query_string, QueryString}]) ->
    Query = #server_status{pid = self(),
                           state = working,
                           started_at = now(),
                           host = Host,
                           port = Port,
                           path = Path,
                           query_string = QueryString},
    gen_server:cast(?WORKER, {working, Query#server_status.pid, Query});
working([{path, Path}, {query_string, QueryString}]) ->
    Query = #server_status{pid = self(),
                           state = working,
                           started_at = now(),
                           path = Path,
                           query_string = QueryString},
    gen_server:cast(?WORKER, {working, Query#server_status.pid, Query}).

done() ->
    gen_server:cast(?WORKER, {done, self()}).

state_dump() ->
    gen_server:call(?WORKER, state_dump).

clear() ->
    gen_server:call(?WORKER, clear).

flatten_format(Format, Paddings) ->
    lists:flatten(io_lib:format(Format, Paddings)).

get_field(pid,              #server_status{pid = R})            -> R;
get_field(state,            #server_status{state = R})          -> R;
get_field(started_at,       #server_status{started_at = R})     -> R;
get_field(ended_at,         #server_status{ended_at = R})       -> R;
get_field(wall_clock_us,    #server_status{wall_clock_us = R})  -> R;
get_field(host,             #server_status{host = R})           -> R;
get_field(port,             #server_status{port = R})           -> R;
get_field(host_port,        #server_status{host_port = R})      -> R;
get_field(path,             #server_status{path = R})           -> R;
get_field(query_string,     #server_status{query_string = R})   -> R.

to_string(Value) when is_pid(Value) ->
    pid_to_list(Value);
to_string(Value) when is_tuple(Value) ->
    {_Ymd, Hms} = calendar:now_to_local_time(Value),
    String = string:join(lists:foldr(fun(I, L) ->
                                         [flatten_format("~2.2. w", [I]) | L]
                                     end,
                                     [],
                                     tuple_to_list(Hms)), ":"),
    String;
to_string(Value) when is_integer(Value) ->
    flatten_format("~6.2. f", [Value / 1.0E6]);
to_string(Value) when is_list(Value) ->
    Value;
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) ->
    flatten_format("~p", [Value]).

now_to_local_time(Now) ->
    {Ymd, Hms} = calendar:now_to_local_time(Now),
    String = string:join([integer_to_list(X) || X <- tuple_to_list(Ymd)], "-")
             ++ " "
             ++ string:join([integer_to_list(X) || X <- tuple_to_list(Hms)], ":"),
    String.

server_status_to_host_port(#server_status{host = undefined, port = undefined}) ->
    "undefined:undefined";
server_status_to_host_port(#server_status{host = Host, port = Port}) ->
    string:join([Host, Port], ":").

calc_mean([]) ->
    0;
calc_mean(Nums) ->
    lists:sum(Nums) / length(Nums).

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

format_process(P, WidthRecord) when is_record(P, server_status) ->
    Fields = [F || F <- record_info(fields, server_status), get_field(F, WidthRecord) =/= undefined],
    Formatted = format_table([{to_string(get_field(F, P)),
                               get_field(F, WidthRecord),
                               get_field(F, P)}
                              || F <- Fields]),
    Formatted.

horizontal_line(L) ->
    "|-" ++ string:join([string:copies("-", W) || W <- L], "-+-") ++ "-|".

text_state_dump() ->
    States = [S || {_Pid, S} <- state_dump()],
    Now = now(),
    Localtime = now_to_local_time(Now),
    CountOfAll = length(States),
    CountOfWorking = length([S || S <- States, S#server_status.state =:= working]),
    CountOfWaiting = length([S || S <- States, S#server_status.state =:= done])
                     + length([S || S <- States, S#server_status.state =:= undefined]),
    States2 = lists:map(fun(#server_status{state = working} = S) ->
                            S#server_status{wall_clock_us = timer:now_diff(Now, S#server_status.started_at)};
                        (S) ->
                            S
                        end,
                        States),
    MeanOfWorking = calc_mean([S#server_status.wall_clock_us
                               || S <- States2, S#server_status.state =:= working]),
    WorkingMeanSeconds = MeanOfWorking / 1.0E6,
    MeanOfWorked = calc_mean([S#server_status.wall_clock_us
                              || S <- States2, S#server_status.state =:= done]),
    WorkedMeanSeconds = MeanOfWorked / 1.0E6,
    MeanOfBoth = calc_mean([X || X <- [MeanOfWorking, MeanOfWorked], X > 0]),
    BothMeanSeconds = MeanOfBoth / 1.0E6,
    PidWidth = lists:max([length(X)
                          || X <- [?PID_HEADING | [to_string(P) || #server_status{pid = P} <- States2]]]),
    StateWidth = lists:max([length(X)
                            || X <- [?STATE_HEADING | ["working", "done", "undefined"]]]),
    StartedAtWidth = lists:max([length(?STARTED_AT_HEADING), length(to_string(Now))]),
    EndedAtWidth = lists:max([length(?ENDED_AT_HEADING), length(to_string(Now))]),
    WallclockWidth = length(?WALL_CLOCK_US_HEADING), % Wall clock heading is always longer than body.
    HostPortWidth = lists:max([length(X)
                               || X <- [?HOST_PORT_HEADING | [server_status_to_host_port(S) || S <- States2]]]),
    PathWidth = lists:max([length(X)
                           || X <- [?PATH_HEADING | [to_string(P) || #server_status{path = P} <- States2]]]),
    QueryWidth = lists:max([length(X)
                            || X <- [?QUERY_STRING_HEADING | [to_string(Q) || #server_status{query_string = Q} <- States2]]]),
    CountWidth = 6,
    Widths = [PidWidth, StateWidth, StartedAtWidth, EndedAtWidth, WallclockWidth, HostPortWidth, PathWidth, QueryWidth],
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
             horizontal_line(Widths),
             format_table([{"PID", PidWidth, ""},
                           {"State", StateWidth, ""},
                           {"Started at", StartedAtWidth, ""},
                           {"Ended at", EndedAtWidth, ""},
                           {"Wallclock [s]", WallclockWidth, ""},
                           {"Host:Port", HostPortWidth, ""},
                           {"Path", PathWidth, ""},
                           {"Query", QueryWidth, ""}]),
             horizontal_line(Widths)],
    WidthRecord = #server_status{pid = PidWidth,
                                 state = StateWidth,
                                 started_at = StartedAtWidth,
                                 ended_at = EndedAtWidth,
                                 wall_clock_us = WallclockWidth,
                                 host_port = HostPortWidth,
                                 path = PathWidth,
                                 query_string = QueryWidth},
    Lines2 = [format_process(S, WidthRecord) || S <- States2],
    Lines3 = Lines ++ Lines2 ++ [horizontal_line(Widths)] ++ [""],
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
%% +--------------------------------------------------------------------------------+
%% | pid | state | started at | ended at | wallclock [s] | host:port | path | query |
%% +--------------------------------------------------------------------------------+
%% | xxx | xxxxx | xxxxxx     | xxxx     |           xxx | xxxx      | xxx  | xx    |
%% +--------------------------------------------------------------------------------+
