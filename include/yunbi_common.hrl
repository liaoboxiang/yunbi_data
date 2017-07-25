%% yunbi websocket 订阅
-record(yunbi_subscibe, {
						 name = all,		%% register name ::atom()
						 url = "",			%% 
						 sub_command_list = []	%% 连接成功后发送的命令[<<>>|....]			
						 }).