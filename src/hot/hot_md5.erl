%% @author box
%% @doc @todo 使用对比beam的md5的方式处理热更


-module(hot_md5).

-include("debugger.hrl").

-define(ETS_FILE_MD5, ets_file_md5).
-define(PATH_EBIN, "../ebin/").

-record(ets_file_md5, {
					   file_name,
					   md5
					   }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_ets/0,
		 hot/0
		 ]).


%% 初始化ets
init_ets() ->
%% 	c:cd(?PATH_EBIN),
	ets:new(?ETS_FILE_MD5, [public, named_table, {keypos, #ets_file_md5.file_name}]),
	Filemd5List = get_need_update_files(),
	update_file_list_md5(Filemd5List).
 
%% 热更文件
hot() ->
	Filemd5List = get_need_update_files(),
	FileList = [File||{File, _Md5}<-Filemd5List],
	hot:u(FileList),
	update_file_list_md5(Filemd5List),
	spawn(fun() -> after_hot_handle(FileList) end),
	ok.
	

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 需要更新的文件列表级新的md5的值
%% return -> [{BeamFileName, Md5}|...]
get_need_update_files() ->
	%% 遍历所有文件，对比旧的MD5
	Path = ?PATH_EBIN,
	{ok, FileList} = file:list_dir(Path),
	%% 过滤.beam文件
	F = fun(File, L) ->
				case filename:extension(File) of
					".beam" -> 
						FileName = filename:rootname(File),
						[erlang:list_to_atom(FileName)|L];
					_ -> L
				end
		end,
	FilterFileList = lists:foldl(F, [], FileList),
	F2 = fun(BeamFileName, HotFileList) ->
				 case get_old_file_md5(BeamFileName) of
					 none -> 
					 	NewMd5 = get_file_md5(Path, BeamFileName),
						[{BeamFileName, NewMd5}|HotFileList];
					 OldMd5 ->
						 NewMd5 = get_file_md5(Path, BeamFileName),
						 case OldMd5 == NewMd5 of
							 true -> 
								 HotFileList;
							 false -> 
								 [{BeamFileName, NewMd5}|HotFileList]
						 end
				 end
		 end,
	lists:foldl(F2, [], FilterFileList).

%% return -> Md5
get_file_md5(Path, BeamFileName) ->
	case beam_lib:md5(Path ++ BeamFileName) of
		{ok, {_, Md5}} -> Md5;
		_Other -> 
			?DG("~p,~p",[BeamFileName, _Other]),
			0
	end.


%% return -> none | Md5
get_old_file_md5(BeamFileName) -> 
	case ets:lookup(?ETS_FILE_MD5, BeamFileName) of
		[] -> none;
		[EtsFileMd5] -> EtsFileMd5#ets_file_md5.md5
	end.

%% FileList:[{BeamFileName, Md5} |...]
update_file_list_md5(FileList) ->
	F = fun(File) ->
			update_file_md5(File)
		end,
	lists:foreach(F, FileList).
	

update_file_md5({BeamFileName, Md5}) ->
	EtsFileMd5 = #ets_file_md5{file_name = BeamFileName, 
							   md5 = Md5},
	ets:insert(?ETS_FILE_MD5, EtsFileMd5).

%% 热更后执行
after_hot_handle(_FileList) ->
	ok.



