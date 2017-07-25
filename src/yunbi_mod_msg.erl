%% @author box
%% @doc 处理ws获取到的数据


-module(yunbi_mod_msg).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("debugger.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 start_link/0,
		 msg/1
		]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

msg(Msg) ->
	gen_server:cast(?MODULE, {msg, Msg}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

-define(ETS, ets_msg).
-record(ets_msg, {
				  name = "",
				  sell = <<0.0>>,
				  buy = <<0.0>>,
				  time = 0
				  }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	ets:new(?ETS, [public, named_table, {keypos, #ets_msg.name}]),
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({msg, {_text, Msg}}, State) ->
	DataList = unpack(Msg),
%% 	?DG("~p",[DataList]),
	case lists:keyfind("event", 1, DataList) of
		{_, <<"update">>} ->
			catch msg_sell(DataList);
		{_, <<"tickers">>} ->
			catch msg_all(DataList);
		_ -> skip
	end,
	{noreply, State};
handle_cast(_Msg, State) ->
	?DG("~p",[_Msg]),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
unpack(Bin) ->
	case catch rfc4627:decode(Bin) of
		{ok, {obj, DataList}, _Remain} ->
			DataList;
		{err, Reason} ->
			?ERR("parse json error:~p", [Reason]),
			[];
		_Other -> 
			?ERR("parse json error:~p,~p", [_Other, erlang:process_info(self(), current_stacktrace)]),
			[]
	end.

%% 接受到卖出信息
msg_sell(_DataList) ->
	ok.
%% 
%% msg_buy(_DataList) ->
%% 	ok.

%% 所有币种的信息
msg_all(DataList) ->
	case lists:keyfind("data", 1, DataList) of
		false -> skip;
		{_, Bin} ->
			CoinList = unpack(Bin),
			[msg_all_item(K, V)||{K, V}<-CoinList]
	end,
	ok.

msg_all_item(NameStr, {obj, DataList}) ->
	{_, Time} = lists:keyfind("at", 1, DataList),
	{_, Buy} = lists:keyfind("buy", 1, DataList),
	{_, Sell} = lists:keyfind("sell", 1, DataList),
%% 	Buy1 = util:bitstring_to_term(Buy),
	case NameStr == "btccny" of
		true ->
			?DG("Time:~p, Buy:~p, Sell:~p",[Time, Buy, Sell]);
		false -> skip
	end,
	
	case ets:lookup(?ETS, NameStr) of
		[] -> 
			ets:insert(?ETS, #ets_msg{name=NameStr, sell = Sell, buy=Buy,time=Time});
		[EtsMsg|_] ->
			case Time > EtsMsg#ets_msg.time of
				true -> 
					ets:insert(?ETS, #ets_msg{name=NameStr, sell = Sell, buy=Buy,time=Time}),
					%% TODO 保存到Redis
					RedisKey = get_redis_key(NameStr),
					redis:hash_set(RedisKey, ["name", NameStr, "sell", Sell, "buy",Buy, "time", Time]),
					ok;
				false -> skip
			end
	end,
	
	ok;
msg_all_item(_, _) -> skip.

get_redis_key(NameStr) ->
	{ok, Pre} = application:get_env(yunbi, redis_key_pre),
	Pre ++ NameStr.

