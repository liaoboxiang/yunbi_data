%% @author box
%% @doc @todo Add description to yunbi_app.

-module(yunbi_app).
-behaviour(application).
-export([start/2, stop/1]).

-include("debugger.hrl").

-define(APP, yunbi).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-export([
		 start_server/0,
		 stop_server/0,
		 stop_server_call/1
		]).

%% 外部调用
%% 应用的启动函数
start_server() ->
	application:start(?APP),
	ok.

%% 应用的停止函数
stop_server() ->
	application:stop(?APP),
	erlang:halt().

%% 外部调用
stop_server_call([Node]) ->
	rpc:call(Node, ?MODULE, stop_server, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	%% top sup
	TopSup = erlang:list_to_atom(erlang:atom_to_list(?APP) ++ "_sup"),
	case TopSup:start_link() of
		{ok, Pid} ->
			case start_sub_sup(TopSup) of 
				ok ->
					?INFO("all sup started"),
					start_mods(),	
					?INFO("all started"),
					{ok, Pid};
				Other ->
					io:format("start sup trees error, ~p",[Other]),
					{error, Other}
			end;
		Error ->
			Error
    end.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% start_sub_sup/1
%% @doc top监控树之下的所有子树的启动
-spec start_sub_sup(TopSup :: atom()) ->
	ok | {error, Reason::term()}.
start_sub_sup(TopSup) ->
	start_msg_sup(TopSup),
	start_redis_sup(TopSup),
	start_ws_sup(TopSup),
	ok.

start_msg_sup(Sup) ->
	{ok,_} = supervisor:start_child(
               Sup,
               {yunbi_mod_msg,
                {yunbi_mod_msg, start_link, []},
                permanent, brutal_kill, worker, [yunbi_mod_msg]}),
	ok.

start_redis_sup(Sup) ->
	{ok, ConfigList} = application:get_env(?APP, redis_config),
  	{ok,_} = supervisor:start_child(
               Sup,
               {redis,
                {redis, start_link, [ConfigList]},
                permanent, brutal_kill, worker, [redis]}),
	ok.

start_ws_sup(Sup) ->
	{ok,_} = supervisor:start_child(
               Sup,
               {ws_sup,
                {ws_sup, start_link, []},
                permanent, brutal_kill, supervisor, [ws_sup]}),
	ok.

start_mods() ->
	hot_md5:init_ets(),
	ok.
