-module(ws_client).

-behaviour(websocket_client_handler).

-include("yunbi_common.hrl").
-include("debugger.hrl").

-export([
         start_link/0,
         start_link/1,
         send_text/2,
         send_binary/2,
         recv/2,
         recv/1,
         stop/1
        ]).

-export([
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {
		  last_heartbeat_time,	%% 最后的心跳时间
		  last_msg_time,		%% 最后消息的时间
          buffer = [] :: list(),
          waiting = undefined :: undefined | pid()
         }).

start_link() ->
	Url = "wss://slanger.yunbi.com:18080/app/d2e734a0694b3cb3ed8cdcadcc6f346e?protocol=7&client=js&version=2.2.0&flash=false",
    Subscibe = #yunbi_subscibe{
								url = Url,
								sub_command_list = [<<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"market-btccny-global\"}}">>]
								},
	start_link(Subscibe).

start_link(Subscibe) ->
    Res = 
	case catch websocket_client:start_link(Subscibe#yunbi_subscibe.url, ?MODULE, [Subscibe]) of
		{ok, Pid} -> 
			RegName = list_to_atom("ws_" ++ atom_to_list(Subscibe#yunbi_subscibe.name)),
			erlang:register(RegName, Pid),
			{ok, Pid};
		Error -> 
			timer:sleep(1*1000),
			Error
	end,
%% 	?DG("~p",[Res]),
	Res.

stop(Pid) ->
    Pid ! stop.

send_text(Pid, Msg) ->
    websocket_client:cast(Pid, {text, Msg}).

send_binary(Pid, Msg) ->
    websocket_client:cast(Pid, {binary, Msg}).

recv(Pid) ->
    recv(Pid, 5000).

recv(Pid, Timeout) ->
    Pid ! {recv, self()},
    receive
        M -> M
    after
        Timeout -> error
    end.

init([Subscibe], _WSReq) ->
	Now = util:unixtime(),
	State = #state{last_heartbeat_time = Now, last_msg_time = Now},
    ping(),
%% 	check_heartbeat(State),
	check_last_msg(State),
	[send_text(self(), Command)||Command<-Subscibe#yunbi_subscibe.sub_command_list],
	{ok, State}.

%% 接受到心跳
websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State#state{last_heartbeat_time = util:unixtime()}};
websocket_handle(Frame, _, State = #state{waiting = undefined}) ->
    yunbi_mod_msg:msg(Frame),
    {ok, State#state{last_msg_time = util:unixtime()}};
websocket_handle(Frame, _, State = #state{waiting = From}) ->
    ?DG("Client received frame2 ~p~n",["b"]),
    From ! Frame,
    {ok, State#state{waiting = undefined, buffer = []}};
websocket_handle(Info, _, State) ->
    ?DG("undefine handle:~p~n", [Info]),
    {ok, State}.

websocket_info({send_text, Text}, WSReq, State) ->
    websocket_client:send({text, Text}, WSReq),
    {ok, State};
websocket_info({recv, From}, _, State = #state{buffer = []}) ->
    {ok, State#state{waiting = From}};
websocket_info({recv, From}, _, State = #state{buffer = [Top|Rest]}) ->
    From ! Top,
    {ok, State#state{buffer = Rest}};
websocket_info({check_heartbeat}, _, State) ->
	case check_heartbeat(State) of
		true -> 
			{ok, State};
		false ->
			{close, <<"heartbeat timeout">>, State}
	end;
websocket_info({check_last_msg}, _, State) ->
	case check_last_msg(State) of
		true -> 
			{ok, State};
		false ->
			{close, <<"timeout">>, State}
	end;
websocket_info(stop, _, State) ->
    {close, <<>>, State};
websocket_info(ping, _, State) ->
    ping(),
	{ok, State};
websocket_info(Any, _, State) ->
    ?DG("info:~p~n", [Any]),
	{ok, State}.

websocket_terminate(_Close, _, _State) ->
	?DG("~p", [_Close]),
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
ping() ->
	erlang:send_after(1*1000, self(), ping),
	websocket_client:cast(self(), {ping, <<>>}).

%% 检查心跳
%% 每秒一次
%% 超过3秒没有接受到心跳的结束进程
%% return normal正常| false 不正常
check_heartbeat(State) ->
	Now = util:unixtime(),
	case Now >= State#state.last_heartbeat_time + 3 of
		true -> false;
		false -> 
			erlang:send_after(1*1000, self(), {check_heartbeat}),
			true
	end.

check_last_msg(State) ->
	Now = util:unixtime(),
	case Now >= State#state.last_msg_time + 3 of
		true -> false;
		false -> 
			erlang:send_after(1*1000, self(), {check_last_msg}),
			true
	end.
