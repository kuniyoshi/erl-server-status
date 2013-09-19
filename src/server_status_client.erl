-module(server_status_client).
-export([working/1, done/0, state_dump/0]).
-include("include/server_status.hrl").
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
