-module(ws_handler).

-export([init/2, init_ets/0]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([send_tick_history/1]).
-compile(export_all).
-include("record.hrl").
init_ets() ->
   ets:new(user_ets, [set, public, named_table, {keypos, #user.uid}]),
   ets:new(tick_ets, [set, public, named_table, {keypos, 1}]),
   ets:insert(user_ets, {user, user_index, 1, 1,1, 0}),
   ets:insert(tick_ets, {tickcount,1, []}).


insert_tick(Tick) -> 
   tick_server:insert_tick(Tick).
    
send_tick_history(Uid) ->
  case ets:lookup(user_ets, Uid) of
    [] ->
       ok;
    [#user{process = Pid}] ->
       erlang:send( Pid, {send_history,Uid})
  end.
send_tick_history2(Uid) ->
  Ticks = ets:tab2list(tick_ets),
  [tick_server:send(Id, T, Uid)||{Id,T}<-Ticks, is_integer(Id)].
  


init_user(Name) ->
   {Index, _IsNew} = 
      case get(uid) of
         undefined ->
            {ets:update_counter(user_ets, user_index, {3, 1}), true};
         UiD ->
            {UiD, false}
      end,
   X = random:uniform(3000) + 1,
   Y = random:uniform(3000) + 1,
   put(uid, Index),
   ets:insert(user_ets, {user, Index, X, Y, Name, self()}),
   Tick = #tick{uid = Index, x = X, y = Y, action = <<"login">>, name = Name},
   insert_tick(Tick),
   Index.
   
init(Req, Opts) ->
        {H,M,S} = time(),
         random:seed(H,M,S),
	{cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
        J = jsx:decode(Msg),
        io:format("J si ~p~n", [J]),
        R = 
        case lists:keyfind(<<"type">>, 1, J) of
          {_, <<"login">>} ->
              {_, Name} =  lists:keyfind(<<"name">>, 1, J),
              [{<<"type">>, <<"login_reply">>}, {<<"uid">> , init_user(Name)}, {<<"name">>, Name}];
          {_, <<"equip">>} ->
              {_, ID} =  lists:keyfind(<<"id">>, 1, J),
              {_, IsOn} = lists:keyfind(<<"action">>, 1, J),
              {_, X} =  lists:keyfind(<<"x">>, 1, J),
              {_, Y} = lists:keyfind(<<"y">>, 1, J),
              insert_tick(#tick{uid = get(uid), name = ID, action = IsOn, x = X, y = Y}),
              [{<<"type">>, <<"ok">>}];
          {_, <<"move">>} ->
              {_, X} =  lists:keyfind(<<"x">>, 1, J),
              {_, Y} = lists:keyfind(<<"y">>, 1, J),
              insert_tick(#tick{uid = get(uid), x = X, y = Y, action = <<"move">>}),
              [{<<"type">>, <<"ok">>}];
          _ ->
             [{<<"type">>, <<"ok">>}]
         end,
         io:format("R is ~p~n", [R]),
	{reply, {text,jsx:encode(R)}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({send_msg,  Msg}, Req, State) ->
   %io:format("Msg is ~p~n", [Msg]),
	{reply, {text, Msg}, Req, State};
websocket_info({send_history,Uid}, Req, State) ->
        case get(send_history) of
           undefined ->
              send_tick_history2(Uid),
              put(send_history, true);
           _ ->
              ok
        end,
	{ok, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
