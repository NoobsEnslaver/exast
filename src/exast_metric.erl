%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_metric).

-export([new/1,
         notify/2,
         get/1,
         delete/2,
         tick_one/1,
         tick_fifteen/1]).

-record(metric, {one :: exast_tsma:tsma(),
                 five :: exast_ssma:ssma(),
                 fifteen :: exast_ssma:ssma(),
                 day :: exast_ssma:ssma()}).

-type metric() :: #metric{}.
-type metric_ret() :: #{one := float(),
                        five := float(),
                        fifteen := float(),
                        day := float()}.

-export_type([metric/0,
              metric_ret/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(term()) -> metric().
new(Name) ->
    One = exast_tsma:new(Name, 60),     % 60 sec
    Five = exast_ssma:new(5),           % x5 One
    Fifteen = exast_ssma:new(15),       % x15 One
    Day = exast_ssma:new(96),           % x24 x4 Fifteen
    Metric = #metric{one = One,
                     five = Five,
                     fifteen = Fifteen,
                     day = Day},
    exast_tick_server:reg_metric(Name, Metric),
    Metric.

-spec notify(metric(), integer()) -> ok.
notify(#metric{one=One}, Value) ->
    exast_tsma:notify(One, Value).

-spec get(metric()) -> metric_ret().
get(#metric{one=One, five=Five, fifteen=Fifteen, day=Day}) ->
    #{one => exast_tsma:get(One),
      five => exast_ssma:get(Five),
      fifteen => exast_ssma:get(Fifteen),
      day => exast_ssma:get(Day)}.

delete(Name, #metric{one=One} = Metric) ->
    exast_tick_server:unreg_metric(Name, Metric),
    exast_tsma:delete(Name, One),
    ok.

tick_one(#metric{one=One, five = Five, fifteen = Fifteen}) ->
    OneData = round(exast_tsma:get(One)),
    exast_ssma:notify(Five, OneData),
    exast_ssma:notify(Fifteen, OneData).
tick_fifteen(#metric{fifteen = Fifteen, day = Day}) ->
    exast_ssma:notify(Day, round(exast_ssma:get(Fifteen))).
