%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_meter).

-export([new/1,
         notify/2,
         get/1,
         delete/2,
         tick_one/1,
         tick_fifteen/1]).

-record(meter, {one :: exast_rate:rate(),
                 five :: exast_ssma:ssma(),
                 fifteen :: exast_ssma:ssma(),
                 day :: exast_ssma:ssma()}).

-type meter() :: #meter{}.
-type meter_ret() :: #{one := float(),
                        five := float(),
                        fifteen := float(),
                        day := float()}.

-export_type([meter/0,
              meter_ret/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(term()) -> meter().
new(Name) ->
    One = exast_rate:new(Name, 60),     % 60 sec
    Five = exast_ssma:new(5),           % x5 One
    Fifteen = exast_ssma:new(15),       % x15 One
    Day = exast_ssma:new(96),           % x24 x4 Fifteen
    Meter = #meter{one = One,
                     five = Five,
                     fifteen = Fifteen,
                     day = Day},
    exast_tick_server:reg_meter(Name, Meter),
    Meter.

-spec notify(meter(), integer()) -> ok.
notify(#meter{one=One}, Value) ->
    exast_rate:notify(One, Value).

-spec get(meter()) -> meter_ret().
get(#meter{one=One, five=Five, fifteen=Fifteen, day=Day}) ->
    #{one => exast_rate:get(One),
      five => exast_ssma:get(Five),
      fifteen => exast_ssma:get(Fifteen),
      day => exast_ssma:get(Day)}.

delete(Name, #meter{one=One} = Meter) ->
    exast_tick_server:unreg_meter(Name, Meter),
    exast_rate:delete(Name, One),
    ok.

tick_one(#meter{one=One, five = Five, fifteen = Fifteen}) ->
    OneData = round(exast_rate:get(One)),
    exast_ssma:notify(Five, OneData),
    exast_ssma:notify(Fifteen, OneData).
tick_fifteen(#meter{fifteen = Fifteen, day = Day}) ->
    exast_ssma:notify(Day, round(exast_ssma:get(Fifteen))).
