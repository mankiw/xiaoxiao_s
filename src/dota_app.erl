-module(dota_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
                        {"/", cowboy_static, {priv_file, dota, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, dota, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 100, [{port, 8091}],
	       	maps:from_list([{env, maps:from_list([{dispatch, Dispatch}])}])),
	dota_sup:start_link().

stop(_State) ->
	ok.
