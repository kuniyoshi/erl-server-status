-module(server_status_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    server_status_sup:start_link().

stop(_Workers) ->
    ok.
