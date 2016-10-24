-module(tick_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([insert_tick/1]).
-export([send/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-record(state, {}).
-include("record.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert_tick(Tick) ->
    gen_server:cast(?MODULE, {insert_tick, Tick}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    erlang:send_after(50, self(), update_tick),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({insert_tick, Tick}, State) ->
     insert_tick2(Tick),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(update_tick, State) ->
    erlang:send_after(50, self(), update_tick),
   TickId = ets:update_counter(tick_ets, tickcount, {2,1}),
   case TickId > 20*60*10 of
       true ->
           board(TickId, [#tick{action = <<"stop">>}]),
           ets:delete_all_objects(user_ets),
           ets:delete_all_objects(tick_ets),
           ets:insert(user_ets, {user, user_index, 1, 1,1, 0}),
           ets:insert(tick_ets, {tickcount,1, []});
       _ ->
           case ets:lookup(tick_ets, TickId-1) of
               [] ->
                   board(TickId, []);
               [{_, TickList}] ->
                   board(TickId, TickList)
           end
    end, 
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
record_to_js(TickId, #tick{uid = UId, x = X, y = Y, action = Action, name = Name}) ->
    [{<<"tick">>, TickId}, {<<"uid">>,UId}, {<<"x">>, X}, {<<"y">>, Y}, {<<"action">>, Action}, {<<"name">>, Name}].

board(TickId,Ticks) ->
  Ticks1 = lists:map(fun(T) -> record_to_js(TickId,T) end, Ticks),
  MsgStr = jsx:encode([{<<"type">>, <<"tick">>},{<<"tick">>, TickId}, {<<"list">>, Ticks1}]),
  L = ets:tab2list(user_ets),
  [begin
     case is_pid(P#user.process) of
          true ->
            %io:format("MsgStr is ~p~n", [MsgStr]),
            erlang:send( P#user.process, {send_msg,MsgStr});
          _ ->
            ok
     end
   end||P<-L],
  ok.

send(TickId,Ticks, ID) ->
  Ticks1 = lists:map(fun(T) -> record_to_js(TickId,T) end, Ticks),
  MsgStr = jsx:encode([{<<"type">>, <<"tick">>},{<<"tick">>, TickId}, {<<"list">>, Ticks1}]),
  L = ets:lookup(user_ets, ID),
  [begin
     case is_pid(P#user.process) of
          true ->
            erlang:send( P#user.process, {send_msg,MsgStr});
          _ ->
            ok
     end
   end||P<-L],
  ok.

insert_tick2(Tick) ->
  TickId = ets:lookup_element(tick_ets, tickcount, 2),
  case ets:lookup(tick_ets, TickId) of
    [] ->
       ets:insert(tick_ets, {TickId, [Tick]});
    [{_, TickList}] ->
       ets:insert(tick_ets, {TickId, [Tick|TickList]})
  end,
  case Tick#tick.action == <<"login">> of
       true ->
           io:format("user ~p login ", [Tick#tick.uid]),
           ws_handler:send_tick_history(Tick#tick.uid);
       _ ->
           ok
  end.
