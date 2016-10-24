-module(dota_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
        ws_handler:init_ets(),
	Procs = [
                {
                 tick_server, 
                 {tick_server, start_link, []},
                 permanent, 
                 2000, 
                 worker, 
                 [tick_server]
                }
                ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
