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
         reg_metric/2,
         unreg/2,
         unreg_metric/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(state, {tsma = #{}    :: #{pos_integer() => counters:counters_ref()},
                metrics = [] :: [exast_metric:metric()]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(N) ->
    gen_server:start_link({local, name(N)}, ?MODULE, [], []).

-spec reg(term(), counters:counters_ref()) -> ok.
reg(Name, Counter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {reg, Counter}).

-spec reg_metric(term(), exast_metric:metric()) -> ok.
reg_metric(Name, Metric) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {reg_metric, Metric}).

unreg(Name, Counter) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {unreg, Counter}).

unreg_metric(Name, Metric) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    gen_server:cast(name(Hash), {unreg_metric, Metric}).
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
handle_cast({reg_metric, Metric}, #state{metrics = M} = State) ->
    {noreply, State#state{metrics = [Metric | M]}};

handle_cast({unreg, Ref}, #state{tsma = T} = State) ->
    #{size := FL} = counters:info(Ref),
    T1 = case maps:get(FL, T, []) -- [Ref] of
             [] -> maps:remove(FL, T);
             NewFLState -> T#{FL => NewFLState}
         end,
    {noreply, State#state{tsma = T1}};
handle_cast({unreg_metric, Metric}, #state{metrics = M} = State) ->
    {noreply, State#state{metrics = M -- [Metric]}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, #state{tsma = T} = State) ->
    handle_metrics(State),

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

handle_metrics(#state{metrics = Metrics}) ->
    Now = erlang:system_time(second),
    if Now rem 60 == 0  -> [exast_metric:tick_one(M) || M <- Metrics],
    if Now rem 900 == 0 -> [exast_metric:tick_fifteen(M) || M <- Metrics];
       true -> skip end;
       true -> skip end,
    ok.
