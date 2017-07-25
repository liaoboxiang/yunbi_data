%% @author box
%% @doc @todo Add description to debugger.


-module(debugger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
        info/1
        ,info/2
        ,info/4
        ,debug/1
        ,debug/2
        ,debug/4
        ,error/1
        ,error/2
        ,error/4
    ]
).


%% 普通信息
info(Msg) ->
    info(Msg, []).
info(Format, Args) ->
    info(Format, Args, null, null).
info(Format, Args, Mod, Line) ->
    try
		Msg = format("info", Format, Args, Mod, Line),
    	io:format("~ts~n", [Msg])
	catch _:_ ->
		io:format("info format error ~p~n",[erlang:get_stacktrace()])
	end.

%% 调试信息
debug(Msg) ->
    debug(Msg, []).
debug(Format, Args) ->
    debug(Format, Args, null, null).
debug(Format, Args, Mod, Line) ->
    Msg = format("debug", Format, Args, Mod, Line),
    io:format("~ts~n", [Msg]).

%% 错误信息
error(Msg) ->
    ?MODULE:error(Msg, []).
error(Format, Args) ->
    ?MODULE:error(Format, Args, null, null).
error(Format, Args, Mod, Line) ->
    try 
		Msg = format("error", Format, Args, Mod, Line),
    	io:format("~ts~n", [Msg])
	catch _:_ ->
		io:format("error format error ~p~n",[erlang:get_stacktrace()])
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 格式化日志信息
%% T = error | info | debug 类型
%% F = list() 格式
%% A = list() 参数
%% Mod = list() 模块名
%% Line = int() 所在行
format(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([Y, "/", M, "/", D, " ", H, ":", I, ":", S]),
    case Line of
        null -> 
			io_lib:format(lists:concat(["## ", T, " ~s ", F, "~n"]), [Date] ++ A);			
%% 			erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~s ", F, "~n"]), [Date] ++ A));
        _ ->
			X1 = lists:concat(["## ", T, " ~s[~w:~w] ", F, "~n"]),
			X2 = io_lib:format(X1, [Date, Mod, Line] ++ A),
			X2
%% 			iolist_to_binary(X2),
%% 			erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~s[~w:~w] ", F, "~n"]), [Date, Mod, Line] ++ A))
    end.
