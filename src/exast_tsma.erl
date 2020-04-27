%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_tsma).

-export([new/2,
         notify/2,
         get/1,
         delete/2]).

-record(tsma, {sum :: counters:counters_ref(),
               count :: counters:counters_ref(),
               frame_length :: pos_integer()}).

-type tsma() :: #tsma{}.

-export_type([tsma/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(term(), pos_integer()) -> tsma().
new(Name, FL) ->
    Count@Ref = counters:new(FL, [write_concurrency]),
    Sum@Ref = counters:new(FL, [write_concurrency]),
    exast_tick_server:reg(Name, Count@Ref),
    exast_tick_server:reg(Name, Sum@Ref),
    #tsma{count = Count@Ref,
          sum = Sum@Ref,
          frame_length = FL}.

-spec notify(tsma(), integer()) -> ok.
notify(#tsma{sum=Sum, count=Count, frame_length=FL}, Value) ->
    Idx = erlang:system_time(second) rem FL + 1,
    counters:add(Sum, Idx, Value),
    counters:add(Count, Idx, 1).

-spec get(tsma()) -> float().
get(#tsma{sum=Sum@Ref, count=Count@Ref, frame_length=FL}) ->
    Sum = lists:sum([counters:get(Sum@Ref, Idx) || Idx <- lists:seq(1, FL)]),
    Count = lists:sum([counters:get(Count@Ref, Idx) || Idx <- lists:seq(1, FL)]),
    if Count == 0 -> 0.0;
       true -> Sum / Count
    end.

delete(Name, #tsma{sum=Sum@Ref, count=Count@Ref}) ->
    exast_tick_server:unreg(Name, Count@Ref),
    exast_tick_server:unreg(Name, Sum@Ref),
    ok.
