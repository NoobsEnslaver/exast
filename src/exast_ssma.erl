%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_ssma).

-export([new/1,
         notify/2,
         get/1,
         delete/2]).

-define(POINTER, 1).

-record(ssma, {ref :: atomics:atomics_ref(),
               frame_length :: pos_integer()}).

-type ssma() :: #ssma{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(pos_integer()) -> ssma().
new(FL) ->
    #ssma{ref = atomics:new(FL+1, []), frame_length = FL}.

-spec notify(ssma(), integer()) -> ok.
notify(#ssma{ref=Ref, frame_length=FL}, Value) ->
    Idx = atomics:add_get(Ref, ?POINTER, 1) rem FL + 2,
    atomics:put(Ref, Idx, Value).

-spec get(ssma()) -> float().
get(#ssma{ref=Ref, frame_length=FL}) ->
    Sum = lists:sum([atomics:get(Ref, Idx) || Idx <- lists:seq(2, FL+1)]),
    Sum / FL.

delete(_Name, _SSMA) ->
    ok.
