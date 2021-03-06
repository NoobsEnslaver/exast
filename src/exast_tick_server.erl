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
         reg_meter/2,
         unreg/2,
         unreg_meter/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(state, {tsma = #{}    :: #{pos_integer() => counters:counters_ref()},
                meters = [] :: [exast_meter:meter()]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(N) ->
    gen_server:start_link({local, name(N)}, ?MODULE, [], []).

-spec reg(term(), counters:counters_ref()) -> ok.
reg(Name, Counter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {reg, Counter}).

-spec reg_meter(term(), exast_meter:meter()) -> ok.
reg_meter(Name, Meter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {reg_meter, Meter}).

unreg(Name, Counter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {unreg, Counter}).

unreg_meter(Name, Meter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {unreg_meter, Meter}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Time until start next sec -100ms
    Diff = (erlang:system_time(second) + 2) * 1000 - erlang:system_time(millisecond) - 100,
    erlang:send_after(Diff, self(), init),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast({reg, Ref}, #state{tsma = T} = State) ->
    #{size := FL} = counters:info(Ref),
    {noreply, State#state{tsma = T#{FL => [Ref | maps:get(FL, T, [])]}}};
handle_cast({reg_meter, Meter}, #state{meters = M} = State) ->
    {noreply, State#state{meters = [Meter | M]}};

handle_cast({unreg, Ref}, #state{tsma = T} = State) ->
    #{size := FL} = counters:info(Ref),
    T1 = case maps:get(FL, T, []) -- [Ref] of
             [] -> maps:remove(FL, T);
             NewFLState -> T#{FL => NewFLState}
         end,
    {noreply, State#state{tsma = T1}};
handle_cast({unreg_meter, Meter}, #state{meters = M} = State) ->
    {noreply, State#state{meters = M -- [Meter]}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, #state{tsma = T} = State) ->
    handle_meters(State),

    Now = erlang:system_time(second) + 1,
    [counters:put(Ref, Idx, 0) || {FL, List} <- maps:to_list(T),
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

handle_meters(#state{meters = Meters}) ->
    Now = erlang:system_time(second),
    if Now rem 60 == 0  -> [exast_meter:tick_one(M) || M <- Meters],
    if Now rem 900 == 0 -> [exast_meter:tick_fifteen(M) || M <- Meters];
       true -> skip end;
       true -> skip end,
    ok.
