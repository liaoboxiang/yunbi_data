%% @author sscf
%% @doc @todo Add description to ws_handler.


-module(ws_handler).
-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link() ->
    crypto:start(),
    ssl:start(),
	Url = "wss://slanger.yunbi.com:18080/app/d2e734a0694b3cb3ed8cdcadcc6f346e?protocol=7&client=js&version=2.2.0&flash=false",
    websocket_client:start_link(Url, ?MODULE, []).

init([], _ConnState) ->
	io:format("init\n"),
    websocket_client:cast(self(), {text, <<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"market-btccny-global\"}}">>}),
    {ok, 2}.

websocket_handle({pong, _}, _ConnState, State) ->
	io:format("pong\n"),
    {ok, State};
websocket_handle({text, Msg}, _ConnState, State) ->
	io:format("bbbbb\n"),
    io:format("Received msg ~p~n", [Msg]),
    {ok, State};
websocket_handle(Info, _, State) ->
	io:format("handle_info ~p~n", [Info]),
	{ok, State}.

websocket_info(start, _ConnState, State) ->
	io:format("ccc\n"),
    {reply, {text, <<"erlang message received">>}, State};
websocket_info(Info, _ConnState, State) ->
	io:format("ccc ~p \n ",[Info]),
    {ok, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih code ~p and payload ~p~n",
              [State, Code, Payload]),
    ok.
