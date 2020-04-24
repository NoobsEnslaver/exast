%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast_gauge).

-export([new/1,
         notify/2,
         get/1,
         delete/2,
         init/0]).

-define(ETS_OPTS, [public, named_table, {write_concurrency, true}]).
-record(gauge, {table :: atom(),
                idx :: reference()}).

-type gauge() :: #gauge{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(term()) -> gauge().
new(Name) ->
    Hash = erlang:phash(Name, erlang:system_info(schedulers)),
    Table = table_name(Hash),
    Idx = make_ref(),
    ets:insert_new(Table, {Idx, 0.0}),
    #gauge{table = Table, idx = Idx}.

-spec notify(gauge(), term()) -> ok.
notify(#gauge{table = Table, idx = Idx}, Value) ->
    ets:insert(Table, {Idx, Value}),
    ok.

-spec get(gauge()) -> term().
get(#gauge{table = Table, idx = Idx}) ->
    ets:lookup_element(Table, Idx, 2).

-spec delete(term(), gauge()) -> ok.
delete(_Name, #gauge{table = Table, idx = Idx}) ->
    ets:delete(Table, Idx),
    ok.

init() ->
    [ets:new(table_name(N), ?ETS_OPTS) || N <- lists:seq(1, erlang:system_info(schedulers))],
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

table_name(N) ->
    list_to_atom(lists:concat(["$exast_gauge_table_", N])).
