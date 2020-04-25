%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_counter).

-export([new/0,
         notify/2,
         get/1,
         delete/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> counters:counters_ref().
new() ->
    counters:new(1, [write_concurrency]).

-spec notify(counters:counters_ref(), integer()) -> ok.
notify(Ref, Value) ->
    counters:add(Ref, 1, Value).

-spec get(counters:counters_ref()) -> integer().
get(Ref) ->
    counters:get(Ref, 1).

delete(_Name, _Ref) ->
    ok.
