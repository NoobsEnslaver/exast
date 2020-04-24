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
         notify/2,
         get/1,
         delete/1]).

-define(KEY(Name), {?MODULE, Name}).

%%%===================================================================
%%% API
%%%===================================================================

%% Simple Moving Average (Time limited)
new_tsma(Name, FL) ->
    new(Name, exast_tsma, [Name, FL]).

%% Simple Moving Average (Size limited)
new_ssma(Name, FL) ->
    new(Name, exast_ssma, [FL]).

%% Cumulative Moving Average
new_cma(Name) ->
    new(Name, exast_cma, []).

new_gauge(Name) ->
    new(Name, exast_gauge, [Name]).

-spec notify(term(), number()) -> ok.
notify(Name, Value) ->
    {Mod, State} = persistent_term:get(?KEY(Name)),
    Mod:notify(State, Value).

-spec get(term()) -> number().
get(Name) ->
    {Mod, State} = persistent_term:get(?KEY(Name)),
    Mod:get(State).

-spec delete(term()) -> ok.
delete(Name) ->
    {Mod, State} = persistent_term:get(?KEY(Name)),
    Mod:delete(Name, State),
    persistent_term:erase(?KEY(Name)).

%%%===================================================================
%%% Internal
%%%===================================================================
new(RegName, Module, Args) ->
    case persistent_term:get(?KEY(RegName), undefined) of
        undefined ->
            State = apply(Module, new, Args),
            persistent_term:put(?KEY(RegName), {Module, State});
        _ ->
            error(already_registred)
    end.
