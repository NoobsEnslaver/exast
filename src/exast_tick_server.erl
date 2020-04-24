%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_tick_server).

-behaviour(gen_server).

%% API
-export([start_link/1,
         reg/2,
         unreg/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(N) ->
    gen_server:start_link({local, name(N)}, ?MODULE, [], []).

-spec reg(term(), counters:counters_ref()) -> ok.
reg(Name, Counter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {reg, Counter}).

unreg(Name, Counter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {unreg, Counter}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Time until start next sec -100ms
    Diff = (erlang:system_time(second) + 2) * 1000 - erlang:system_time(millisecond) - 100,
    erlang:send_after(Diff, self(), init),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast({reg, Ref}, State) ->
    #{size := FL} = counters:info(Ref),
    {noreply, State#{FL => [Ref | maps:get(FL, State, [])]}};
handle_cast({unreg, Ref}, State) ->
    #{size := FL} = counters:info(Ref),
    case maps:get(FL, State, []) -- [FL] of
        [] ->
            {noreply, maps:remove(FL, State)};
        NewFLState ->
            {noreply, State#{FL => NewFLState}}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    Now = erlang:system_time(second) + 1,
    [counters:put(Ref, Idx, 0) || {FL, List} <- maps:to_list(State),
                                  Idx <- [Now rem FL + 1],
                                  Ref <- List],
    {noreply, State};
handle_info(init, State) ->
    timer:send_interval(1000, tick),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

name(N) ->
    list_to_atom(lists:concat(["$exast_tick_server_", N])).
