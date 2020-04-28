%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_rate).

-export([new/2,
         notify/2,
         get/1,
         delete/2]).

-record(rate, {sum :: counters:counters_ref(),
               frame_length :: pos_integer()}).

-type rate() :: #rate{}.

-export_type([rate/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(term(), pos_integer()) -> rate().
new(Name, FL) ->
    Sum@Ref = counters:new(FL, [write_concurrency]),
    exast_tick_server:reg(Name, Sum@Ref),
    #rate{sum = Sum@Ref,
          frame_length = FL}.

-spec notify(rate(), integer()) -> ok.
notify(#rate{sum=Sum@Ref, frame_length=FL}, Value) ->
    Idx = erlang:system_time(second) rem FL + 1,
    counters:add(Sum@Ref, Idx, Value).

-spec get(rate()) -> float().
get(#rate{sum=Sum@Ref, frame_length=FL}) ->
    Sum = lists:sum([counters:get(Sum@Ref, Idx) || Idx <- lists:seq(1, FL)]),
    Sum / FL.

delete(Name, #rate{sum=Sum@Ref}) ->
    exast_tick_server:unreg(Name, Sum@Ref),
    ok.
