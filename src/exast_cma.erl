%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_cma).

-export([new/0,
         notify/2,
         get/1,
         delete/2]).

-define(SUM_IDX, 1).
-define(COUNT_IDX, 2).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> counters:counters_ref().
new() ->
    counters:new(2, [write_concurrency]).

-spec notify(counters:counters_ref(), integer()) -> ok.
notify(Ref, Value) ->
    counters:add(Ref, ?SUM_IDX, Value),
    counters:add(Ref, ?COUNT_IDX, 1).

-spec get(counters:counters_ref()) -> float().
get(Ref) ->
    Sum = counters:get(Ref, ?SUM_IDX),
    Count = counters:get(Ref, ?COUNT_IDX),
    if Count == 0 -> 0.0;
       true -> Sum / Count
    end.

delete(_Name, _Ref) ->
    ok.
