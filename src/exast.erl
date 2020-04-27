%%%-------------------------------------------------------------------
%%% @author Nikita Vorontsov <vorontsov.nstu@yandex.ru>
%%% @copyright (C) 2020, Nikita Vorontsov
%%% @doc
%%%
%%% @end
%%% Created : 23 Apr 2020
%%%-------------------------------------------------------------------
-module(exast).

-export([new_tsma/2,
         new_ssma/2,
         new_cma/1,
         new_gauge/1,
         new_counter/1,
         new_meter/1,
         notify/2,
         get/0, get/1,
         delete/1]).

-define(KEY(Name), {?MODULE, Name}).
-type name() :: term().

%%%===================================================================
%%% API
%%%===================================================================

%% Simple Moving Average (Time limited)
-spec new_tsma(name(), pos_integer()) -> ok | {error, already_registred}.
new_tsma(Name, FL) ->
    new(Name, exast_tsma, [Name, FL]).

%% Simple Moving Average (Size limited)
-spec new_ssma(name(), pos_integer()) -> ok | {error, already_registred}.
new_ssma(Name, FL) ->
    new(Name, exast_ssma, [FL]).

%% Cumulative Moving Average
-spec new_cma(name()) -> ok | {error, already_registred}.
new_cma(Name) ->
    new(Name, exast_cma, []).

-spec new_gauge(name()) -> ok | {error, already_registred}.
new_gauge(Name) ->
    new(Name, exast_gauge, [Name]).

-spec new_counter(name()) -> ok | {error, already_registred}.
new_counter(Name) ->
    new(Name, exast_counter, []).

-spec new_meter(name()) -> ok | {error, already_registred}.
new_meter(Name) ->
    new(Name, exast_meter, [Name]).

-spec notify(term(), number()) -> ok.
notify(Name, Value) ->
    {Mod, State} = persistent_term:get(?KEY(Name)),
    Mod:notify(State, Value).

-spec get() -> [{name(), number() | exast_meter:meter_ret()}].
get() ->
    [{Name, Mod:get(State)} || {?KEY(Name), {Mod, State}} <- persistent_term:get()].

-spec get(term()) -> number() | exast_meter:meter_ret().
get(Name) ->
    {Mod, State} = persistent_term:get(?KEY(Name)),
    Mod:get(State).

-spec delete(term()) -> ok.
delete(Name) ->
    {Mod, State} = persistent_term:get(?KEY(Name)),
    Mod:delete(Name, State),
    persistent_term:erase(?KEY(Name)),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================
new(RegName, Module, Args) ->
    case persistent_term:get(?KEY(RegName), undefined) of
        undefined ->
            State = apply(Module, new, Args),
            persistent_term:put(?KEY(RegName), {Module, State});
        _ ->
            {error, already_registred}
    end.
