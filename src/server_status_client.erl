-module(server_status_client).
-export([working/1, done/0]).
-export([state_dump/0, text_state_dump/0]).
-include("include/server_status.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(WORKER, server_status_worker).

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
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_local_time(Value),
    String = integer_to_list(Y) ++ "-" ++ integer_to_list(Mon) ++ "-" ++ integer_to_list(D)
             ++ " "
             ++ io_lib:format("~2w", [H]) ++ io_lib:format("~2w", [Min]) ++ io_lib:format("~2w", [S]),
    lists:flatten(String);
to_string(Value) when is_integer(Value) ->
    io_lib:format("~6.2. f", [Value / 1.0E6]);
to_string(Value) when is_list(Value) ->
    Value;
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) ->
    io_lib:format("~p", [Value]).

server_status_to_host_port(#server_status{host = undefined, port = undefined}) ->
    "undefined:undefined";
server_status_to_host_port(#server_status{host = Host, port = Port}) ->
    string:join([Host, Port], ":").

calc_mean([]) ->
    0;
calc_mean(Nums) ->
    lists:sum(Nums) / length(Nums).

get_float_format(MaxWidth) ->
    Format = io_lib:format("~~.~w. s~~~w.2. f", [32, MaxWidth]),
    lists:flatten(Format).

get_int_format(MaxWidth) ->
    Format = io_lib:format("~~.~w. s~~~w.2. w", [32, MaxWidth]),
    lists:flatten(Format).

format_count_summary(count_of_all, Count, MaxWidth) ->
    Defininition = "- count of all processes: ",
    io_lib:format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(count_of_working, Count, MaxWidth) ->
    Defininition = "- count of working: ",
    io_lib:format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(count_of_waiting, Count, MaxWidth) ->
    Defininition = "- count of waiting: ",
    io_lib:format(get_int_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean_of_working, Count, MaxWidth) ->
    Defininition = "- mean of working: ",
    io_lib:format(get_float_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean_of_worked, Count, MaxWidth) ->
    Defininition = "- mean of worked: ",
    io_lib:format(get_float_format(MaxWidth), [Defininition, Count]);
format_count_summary(mean_of_both, Count, MaxWidth) ->
    Defininition = "- mean of both: ",
    io_lib:format(get_float_format(MaxWidth), [Defininition, Count]).

format_table(Columns) ->
    SwitchFormat = fun(Value) when is_number(Value) ->
            "~~~ws";
        (_) ->
            "~~-~ws"
    end,
    Columns2 = [{S, io_lib:format(SwitchFormat(V), [W])} || {S, W, V} <- Columns],
    Columns3 = [io_lib:format(lists:flatten(F), [S]) || {S, F} <- Columns2],
    Row = "| " ++ string:join([C || C <- Columns3], " | ") ++ " |",
    Row.

format_process(P, WidthRecord) when is_record(P, server_status) ->
    Fields = [F || F <- record_info(fields, server_status), get_field(F, WidthRecord) =/= undefined],
    Formatted = format_table([{to_string(get_field(F, P)), get_field(F, WidthRecord), get_field(F, P)} || F <- Fields]),
    Formatted.

text_state_dump() ->
    States = [S || {_Pid, S} <- state_dump()],
    Localtime = string:join([string:join([integer_to_list(X) || X <- tuple_to_list(date())], "-"),
                             string:join([integer_to_list(X) || X <- tuple_to_list(time())], ":")],
                            " "),
    CountOfAll = length(proplists:get_keys(States)),
    CountOfWorking = length([S || S <- States, S#server_status.state =:= working]),
    CountOfWaiting = length([S || S <- States, S#server_status.state =:= done]
                            ++ [S || S <- States, S#server_status.state =:= undefined]),
    Now = now(),
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
    PidWidth = lists:max([length(pid_to_list(P)) || #server_status{pid = P} <- States2]),
    StateWidth = lists:max([length(X) || X <- ["working", "done", "undefined"]]),
    StartedAtWidth = lists:max([length(X) || X <- ["started at", "03:07:36"]]),
    EndedAtWidth = lists:max([length(X) || X <- ["ended at", "03:07:59"]]),
    WallclockWidth = length("wallclock [s]"),
    HostPortWidth = lists:max([length(server_status_to_host_port(S)) || S <- States2]),
    PathWidth = lists:max([length(binary_to_list(S#server_status.path))
                           || S <- States2]),
    QueryWidth = lists:max([length(binary_to_list(S#server_status.query_string))
                            || S <- States2]),
%%     ?debugVal([PidWidth, StateWidth, StartedAtWidth, EndedAtWidth, WallclockWidth, HostPortWidth, PathWidth, QueryWidth]),
%%     CountWidth = lists:max([byte_size(integer_to_binary(X))
%%                             || X <- [CountOfAll, CountOfWorking, CountOfWaiting, MeanOfWorking, MeanOfWorked, MeanOfBoth]]),
    CountWidth = 6,
    Lines = ["SERVER STATUS",
             string:copies("=", length("SERVER STATUS")),
             "",
             io_lib:format("Dumped at ~s", [Localtime]),
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
             "+-" ++ string:join([string:copies("-", W)
                                 || W<- [PidWidth,
                                         StateWidth,
                                         StartedAtWidth,
                                         EndedAtWidth,
                                         WallclockWidth,
                                         HostPortWidth,
                                         PathWidth,
                                         QueryWidth]], "-+-") ++ "-+",
             format_table([{"PID", PidWidth, ""},
                           {"State", StateWidth, ""},
                           {"Started at", StartedAtWidth, ""},
                           {"Ended at", EndedAtWidth, ""},
                           {"Wallclock [s]", WallclockWidth, ""},
                           {"Host:Port", HostPortWidth, ""},
                           {"Path", PathWidth, ""},
                           {"Query", QueryWidth, ""}]),
             "+-" ++ string:join([string:copies("-", W)
                                 || W<- [PidWidth,
                                         StateWidth,
                                         StartedAtWidth,
                                         EndedAtWidth,
                                         WallclockWidth,
                                         HostPortWidth,
                                         PathWidth,
                                         QueryWidth]], "-+-") ++ "-+"],
    WidthRecord = #server_status{pid = PidWidth,
                                 state = StateWidth,
                                 started_at = StartedAtWidth,
                                 ended_at = EndedAtWidth,
                                 wall_clock_us = WallclockWidth,
                                 host_port = HostPortWidth,
                                 path = PathWidth,
                                 query_string = QueryWidth},
    Lines2 = [format_process(S, WidthRecord) || S <- States2],
    Lines3 = Lines ++ Lines2
             ++ ["+-" ++ string:join([string:copies("-", W)
                                      || W<- [PidWidth,
                                              StateWidth,
                                              StartedAtWidth,
                                              EndedAtWidth,
                                              WallclockWidth,
                                              HostPortWidth,
                                              PathWidth,
                                              QueryWidth]], "-+-") ++ "-+",
                 ""],
    lists:flatten(string:join(Lines3, "\n")).

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
