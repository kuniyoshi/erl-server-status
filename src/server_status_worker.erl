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

init([] = Workers) ->
    {ok, Workers}.

handle_call(state_dump, _From, Workers) ->
    {reply, Workers, Workers};
handle_call(clear, _From, _Workers) ->
    {reply, ok, []};
handle_call(_Request, _From, Workers) ->
    {reply, ok, Workers}.

handle_cast({working, Pid, Worker}, Workers) ->
    Workers2 = lists:keystore(Pid, 1, Workers, {Pid, Worker}),
    {noreply, Workers2};
handle_cast({done, {with, Code}, {at, Time}, Pid}, Workers) ->
    Worker = proplists:get_value(Pid, Workers),
    StartedAt = Worker#server_status.started_at,
    Worker2 = Worker#server_status{state = done,
                                   ended_at = Time,
                                   wall_clock_us = timer:now_diff(Time, StartedAt),
                                   code = Code},
    Workers2 = lists:keystore(Pid, 1, Workers, {Pid, Worker2}),
    {noreply, Workers2}.

handle_info(_Info, Workers) ->
    {noreply, Workers}.

terminate(_Reason, _Workers) ->
    ok.

code_change(_OldVsn, Workers, _Extra) ->
    {ok, Workers}.
