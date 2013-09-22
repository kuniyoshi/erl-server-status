-module(server_status_worker).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-define(SERVER, ?MODULE).
-include("include/server_status.hrl").
-include_lib("eunit/include/eunit.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([] = Args) ->
    {ok, Args}.

handle_call(state_dump, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({working, Pid, Query}, State) ->
    State2 = lists:keystore(Pid, 1, State, {Pid, Query}),
    {noreply, State2};
handle_cast({done, Pid}, State) ->
    Query = proplists:get_value(Pid, State),
    Now   = now(),
    Query2 = Query#server_status{state = done,
                                 ended_at = Now,
                                 wall_clock_us = timer:now_diff(Now,
                                                                Query#server_status.started_at)},
    State2 = lists:keystore(Pid, 1, State, {Pid, Query2}),
    {noreply, State2}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
