%% @author box
%% @doc @todo Add description to ws_sup.


-module(ws_sup).
-behaviour(supervisor).
-export([init/1]).

-include("yunbi_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    F = fun(Subscibe) ->
				ChildName = Subscibe#yunbi_subscibe.name,
			{ChildName, {ws_client,start_link,[Subscibe]},
	      permanent,brutal_kill,worker,[ws_client]}
	end,
	ChildList = lists:map(F, get_subscibe_list()),
    {ok,{{one_for_one,200000,1}, ChildList}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 订阅的队列
%% return [#yunbi_subscibe{}|...]
get_subscibe_list() ->
	[
%% 	 #yunbi_subscibe{
%% 					 name = sell,
%% 					 url = "wss://slanger.yunbi.com:18080/app/d2e734a0694b3cb3ed8cdcadcc6f346e?protocol=7&client=js&version=2.2.0&flash=false",
%% 					 sub_command_list = [<<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"market-btccny-global\"}}">>]
%% 					},
	#yunbi_subscibe{
					name = all,
					 url = "wss://slanger.yunbi.com:18080/app/d2e734a0694b3cb3ed8cdcadcc6f346e?protocol=7&client=js&version=2.2.0&flash=false",
					 sub_command_list = [<<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"market-global\"}}">>]
					}
	].

