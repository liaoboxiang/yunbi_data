%% @author box
%% @doc 对eredis封装的接口


-module(redis).
-include("debugger.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		 start_link/1,
		 stop/0,
		 set/2,
		 get/1,
		 mset/1,
		 mget/1,
		 hash_set/2,
		 hash_get/1,
		 command/1,
		 async_command_noreply/1,
		 async_command/2,
		 multi_command/1
		]).


-type return_value() :: undefined | binary() | [binary() | nonempty_list()].

start_link() ->
	{ok, Pid} = eredis:start_link(),
	erlang:register(?MODULE, Pid),
	{ok, Pid}.

start_link(Configs) ->
	{_, Host} = lists:keyfind(host, 1, Configs),
	{_, Port} = lists:keyfind(port, 1, Configs),
	{_, Database} = lists:keyfind(database, 1, Configs),
	{_, Password} = lists:keyfind(password, 1, Configs),
	{ok, Pid} = eredis:start_link(Host, Port, Database, Password),
	erlang:register(?MODULE, Pid),
	{ok, Pid}.

stop() ->
	eredis:stop(?MODULE).

-spec set(Key::atom()|string()|binary(), Val::term()) -> {ok, return_value()} | {error, Reason::binary() | no_connection}.
set(Key, Val) ->
	eredis:q(?MODULE, ['SET', Key, Val]).

get(Key) ->
	eredis:q(?MODULE, ['GET', Key]).

mset(KeyValPairList) ->
	List = lists:foldr(fun({Key, Val}, L) -> 
						[Key, Val | L]
				end, [], KeyValPairList),
	eredis:q(?MODULE, ['MSET' | List]).

mget(KeyList) ->
	eredis:q(?MODULE, ['MGET' | KeyList]).

hash_set(Key, ValList) ->
	eredis:q(?MODULE, ['HMSET', Key | ValList]).

hash_get(Key) ->
	eredis:q(?MODULE, ['HGETALL', Key]).

%% command(['SET', Key, Val])
command(Command) ->
	eredis:q(?MODULE, Command).

async_command_noreply(Command) ->
	eredis:q_noreply(?MODULE, Command).

async_command(Command, Pid) ->
	eredis:q_async(?MODULE, Command, Pid).


multi_command(CommandList) ->
	eredis:qp(?MODULE, CommandList).

%% ====================================================================
%% Internal functions
%% ====================================================================


