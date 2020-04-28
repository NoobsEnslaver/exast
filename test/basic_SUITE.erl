%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap,{seconds,60}}].

init_per_suite(Config) ->
    application:ensure_all_started(exast),
    Config.

end_per_suite(_Config) ->
    application:stop(exast),
    ok.

all() ->
    [{group, basic}].

groups() ->
    [{basic, [parallel],
      [tsma_basic,
       ssma_basic,
       cma_basic,
       gauge_basic,
       counter_basic,
       rate_basic]}].

%%--------------------------------------------------------------------
tsma_basic(_Config) ->
    N = ?FUNCTION_NAME,
    ok = exast:new_tsma(N, 3),
    ?assertEqual({error, already_registred}, exast:new_tsma(N, 5)),

    ?assertEqual(0.0, exast:get(N)),

    exast:notify(N, 10),
    ?assertEqual(10.0, exast:get(N)),

    exast:notify(N, 20),
    ?assertEqual(15.0, exast:get(N)),

    timer:sleep(2000),
    ?assertEqual(15.0, exast:get(N)),

    exast:notify(N, 30),
    ?assertEqual(20.0, exast:get(N)),

    timer:sleep(1200),
    ?assertEqual(30.0, exast:get(N)),

    timer:sleep(2000),
    ?assertEqual(0.0, exast:get(N)),

    exast:delete(N),
    ?assertError(badarg, exast:get(N)),

    ok.

ssma_basic(_Config) ->
    N = ?FUNCTION_NAME,
    ok = exast:new_ssma(N, 3),
    ?assertEqual({error, already_registred}, exast:new_ssma(N, 5)),

    ?assertEqual(0.0, exast:get(N)),

    exast:notify(N, 3),
    ?assertEqual(1.0, exast:get(N)),

    exast:notify(N, 6),
    ?assertEqual(3.0, exast:get(N)),

    exast:notify(N, 9),
    ?assertEqual(6.0, exast:get(N)),

    exast:notify(N, 12),
    ?assertEqual(9.0, exast:get(N)),

    exast:notify(N, 0),
    exast:notify(N, 0),
    exast:notify(N, 0),
    ?assertEqual(0.0, exast:get(N)),

    exast:delete(N),
    ?assertError(badarg, exast:get(N)),

    ok.

cma_basic(_Config) ->
    N = ?FUNCTION_NAME,
    ok = exast:new_cma(N),
    ?assertEqual({error, already_registred}, exast:new_cma(N)),

    ?assertEqual(0.0, exast:get(N)),

    exast:notify(N, 3),
    ?assertEqual(3.0, exast:get(N)),

    exast:notify(N, 6),
    ?assertEqual(4.5, exast:get(N)),

    exast:notify(N, 9),
    ?assertEqual(6.0, exast:get(N)),

    exast:notify(N, 12),
    ?assertEqual(7.5, exast:get(N)),

    exast:notify(N, 0),
    exast:notify(N, 0),
    ?assertEqual(5.0, exast:get(N)),

    exast:delete(N),
    ?assertError(badarg, exast:get(N)),

    ok.

gauge_basic(_Config) ->
    N = ?FUNCTION_NAME,
    ok = exast:new_gauge(N),
    ?assertEqual({error, already_registred}, exast:new_gauge(N)),

    ?assertEqual(0.0, exast:get(N)),

    exast:notify(N, 3.0),
    ?assertEqual(3.0, exast:get(N)),

    exast:notify(N, 6.0),
    ?assertEqual(6.0, exast:get(N)),

    exast:notify(N, infinity),
    ?assertEqual(infinity, exast:get(N)),

    exast:notify(N, <<"У попа была собака, он её любил">>),
    ?assertEqual(<<"У попа была собака, он её любил">>, exast:get(N)),

    exast:delete(N),
    ?assertError(badarg, exast:get(N)),

    ok.

counter_basic(_Config) ->
    N = ?FUNCTION_NAME,
    ok = exast:new_counter(N),
    ?assertEqual({error, already_registred}, exast:new_counter(N)),

    ?assertEqual(0, exast:get(N)),

    Data = lists:seq(1, 100),
    Sum = lists:sum(Data),
    [exast:notify(N, X) || X <- Data],
    ?assertEqual(Sum, exast:get(N)),

    exast:notify(N, 0),
    ?assertEqual(Sum, exast:get(N)),

    exast:delete(N),
    ?assertError(badarg, exast:get(N)),

    ok.

rate_basic(_Config) ->
    N = ?FUNCTION_NAME,
    FL = 10,
    ok = exast:new_rate(N, FL),
    ?assertEqual({error, already_registred}, exast:new_rate(N, FL)),
    ?assertEqual(0.0, exast:get(N)),

    WorkerF = fun Loop()-> exast:notify(N, 10), timer:sleep(500), Loop() end,
    ExpextedRate = 20.0,
    MinRate = ExpextedRate - (ExpextedRate / FL),

    Worker1 = spawn(WorkerF),
    timer:sleep(FL * 1100),
    Rate0 = exast:get(N),
    ?assert(Rate0 >= MinRate andalso Rate0 =< ExpextedRate),
    Worker2 = spawn(WorkerF),
    timer:sleep(FL * 1100),
    Rate1 = exast:get(N),
    ?assert(Rate1 >= 2*MinRate andalso Rate1 =< 2*ExpextedRate),
    exit(Worker1, kill),
    exit(Worker2, kill),
    timer:sleep(FL * 1100),
    ?assertEqual(0.0, exast:get(N)),
    ok.
