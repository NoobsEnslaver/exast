%%%-------------------------------------------------------------------
%% @doc exast top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(exast_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1000,
                 period => 1},

    Childs = [#{id => {exast_tick_server, N},
                start => {exast_tick_server, start_link, [N]}} || N <- lists:seq(1, erlang:system_info(schedulers))],

    exast_gauge:init(),

    {ok, {SupFlags, Childs}}.
