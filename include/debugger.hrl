%% 在代码引入debug头文件之前定义debug：  -define(debug, true).  则会打开调试开关，打印调试信息
-ifdef(debug).
    -define(DG(Msg), debugger:debug(Msg, [], ?MODULE, ?LINE)).
    -define(DG(F, A), debugger:debug(F, A, ?MODULE, ?LINE)).
-else.
    -define(DG(_Msg), ok).
    -define(DG(_F, _A), ok).
-endif.

-define(INFO(Msg), debugger:info(Msg, [], ?MODULE, ?LINE)).
-define(ERR(Msg), debugger:error(Msg, [], ?MODULE, ?LINE)).

-define(INFO(F, A), debugger:info(F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), debugger:error(F, A, ?MODULE, ?LINE)).
