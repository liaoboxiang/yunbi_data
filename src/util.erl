%% @author box
%% @doc 常用方法


-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([unixtime/0,
		 unixtime_ms/0,
		 unixtime/1,
		 unixdate/0,
		 unixdate/1,
		 unixtime_to_now/1,
		 unixtime_to_time_string/1,
		 time_string_to_unixtime/1,
		 date_to_time_string/1,
		 term_to_string/1,
		 string_to_term/1,
		 term_to_bitstring/1,
		 bitstring_to_term/1,
		 ceil/1,
		 floor/1,
		 cancel_timer/1,
		 md5/1,
		 md5_bin/1,
		 rand/2,
		 rand_weight/1,
		 get_ip_port/1,
		 hex_list_to_bin/1,
		 bin_to_hex_list/1,
		 integer_to_float_string/2,
		 del_dir/1
		 ]).


unixtime() ->
    {M, S, _} = os:timestamp(),  
    M * 1000000 + S.

unixtime_ms() ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
 	1000000000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.

%% 取得某个时间点的unix时间戳
%% @paramLocalTime = {{Y,M,D},{H,M,S}} 
unixtime(LocalTime) ->
    [UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),
    S1 = calendar:datetime_to_gregorian_seconds(UniversalTime),
    S2 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    S1 - S2.

%% 取得当天零点的unix时间戳
unixdate() ->
	Now = os:timestamp(),
    {_, Time} = calendar:now_to_local_time(Now),
    Ds = calendar:time_to_seconds(Time),
    {M, S, _} = Now,
    M * 1000000 + S - Ds.

%% 取得指定日期的0点时间
unixdate(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {_, Time} = calendar:now_to_local_time(Now),
    Ds = calendar:time_to_seconds(Time),
    {M, S, _} = Now,
    M * 1000000 + S - Ds.

unixtime_to_now(Time) ->
    M = Time div 1000000,
    S = Time rem 1000000,
    {M, S, 0}.

%% 时间戳 格式化
%% return "YYYYMMDDHHMMSS"
unixtime_to_time_string(UnixTime) ->
	Now = unixtime_to_now(UnixTime),
    {{Y,M,D}, {H,Min,S}} = calendar:now_to_local_time(Now),
	Year = io_lib:format("~4.10.0w", [Y]),
	Month = io_lib:format("~2.10.0w", [M]),
	Day = io_lib:format("~2.10.0w", [D]),
	Hour = io_lib:format("~2.10.0w", [H]),
	Minute = io_lib:format("~2.10.0w", [Min]),
	Second = io_lib:format("~2.10.0w", [S]),
	lists:flatten([Year, Month, Day, Hour, Minute, Second]).

%% "YYYYMMDDHHMMSS" 转成unixtime()
time_string_to_unixtime(String) ->
	{Y, MLast} = lists:split(4, String),
	{M, DLast} = lists:split(2, MLast),
	{D, HLast} = lists:split(2, DLast),
	{H, MinLast} = lists:split(2, HLast),
	{Min, SLast} = lists:split(2, MinLast),
	{S, _} = lists:split(2, SLast),
	LocalTime = {
				 {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
				 {list_to_integer(H), list_to_integer(Min), list_to_integer(S)}
				 },
	unixtime(LocalTime).

%% date_to_time_string(erlang:date()).
%% return "YYYYMMDD"
date_to_time_string({Y,M,D}) ->
	Year = io_lib:format("~4.10.0w", [Y]),
	Month = io_lib:format("~2.10.0w", [M]),
	Day = io_lib:format("~2.10.0w", [D]),
	lists:flatten([Year, Month, Day]).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.


%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) when is_list(BitString) -> BitString;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).



%%向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.


cancel_timer(Timer) ->
	case erlang:is_reference(Timer) of
		true -> erlang:cancel_timer(Timer) ;
		false -> skip
	end.


%% 32位字符 小写
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 16byte
-spec md5_bin(S::string()|binary()) -> binary(). 
md5_bin(S) ->
	erlang:md5(S).



%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    %% 以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            rand:seed(exs1024,os:timestamp()),
            put("rand_seed", 1);
        _ -> skip
    end,
    M = Min - 1,
    if
        Max - M =< 0 ->
            0;
        true ->
            rand:uniform(Max - M) + M
    end.

%% 根据权重随机一个
%% List = [{Key, Weight}|...]
%% rand_weight(List) -> Key.
rand_weight(List) ->
	rand_weight1(List, 0, []).

rand_weight1([], Sum, L) ->
	Rand = rand(1, Sum),
	rand_weight2(lists:reverse(L), Rand);
rand_weight1([{K,V}|T], Sum, L) ->
	rand_weight1(T, Sum+V, [{K,V+Sum}|L]).

rand_weight2([{K,N}|T], Rand) ->
	case Rand =< N of
		true -> K;
		false -> rand_weight2(T, Rand)
	end.

%% 获取socket的ip和端口
-spec get_ip_port(Socket::port()) -> {ok, Ip, Port::integer()} | error when
		  Ip::{integer(),integer(),integer(),integer()} | 
			  {integer(),integer(),integer(),integer(),integer(),integer()}.
get_ip_port(Socket) ->
	case inet:peername(Socket) of
		{ok, Ip, Port} -> {ok, Ip, Port};
		_ -> error
	end.

%% 十六进制转二进制
%% "aa1f" => <<170,31>>
hex_list_to_bin(L) ->
    hex_list_to_bin(L, []).

hex_list_to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
hex_list_to_bin([C1, C2 | Rest], Acc) ->
    hex_list_to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

%% @spec dehex(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% 二进制转16进制
bin_to_hex_list(Bin) ->
	bin_to_hex_list(Bin, []).

bin_to_hex_list(<<>>, List) -> 
	L = lists:reverse(List),
	string:join(L, ":");
bin_to_hex_list(<<H:8, LastBin/binary>>, List) ->
	Hex = integer_to_list(H, 16),
	HexStr = string:to_lower(Hex),
	HexStr1 = case length(HexStr) < 2 of
				  true -> "0" ++ HexStr;
				  false -> HexStr
			  end,
	bin_to_hex_list(LastBin, [HexStr1|List]).


%% 整数转为小数字符串
%% Offset::integer() 小数点后保留位数
integer_to_float_string(Int, Offset) when Int >= 0->
	Str = integer_to_list(Int),
	Len = length(Str),
	if
		Offset == 0 -> 
			Str;
		Len > Offset -> %% 位数足够 
			Left = string:left(Str, Len - Offset),
			Right = string:right(Str, Offset),
			Left ++ "." ++ Right;
		true -> %% 位数不足，补0
			"0." ++ string:copies("0", Offset-Len) ++ Str
	end;
integer_to_float_string(Int, Offset) ->
	"-1" ++ integer_to_float_string(Int*-1, Offset).


del_dir(Dir) -> 
	case filelib:is_dir(Dir) of
		true ->
			case file:list_dir(Dir) of
				{ok, L} ->
					F = fun(File) ->
								del_dir(filename:join(Dir, File))
						end,
					lists:foreach(F, L);
				_ -> skip
			end,
			file:del_dir(Dir);
		false -> 
			file:delete(Dir)
	end.

