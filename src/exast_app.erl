%%%-------------------------------------------------------------------
%% @doc exast public API
%% @end
%%%-------------------------------------------------------------------

-module(exast_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    exast_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
