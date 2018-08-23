%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 先将json文件中冒号转化为逗号，文件最后加上英文句号，这样调整为erlang读取元素的格式文件
%%% @end
%%% Created : 17. 八月 2018 17:16
%%%-------------------------------------------------------------------
-module(make_cfg).
-author("chenlong").
-include("common.hrl").

%% API
-export([make_cfg/1]).

make_cfg([FileNameAtom]) ->
	try
		FullName = atom_to_list(FileNameAtom),
		Path = filename:dirname(FullName),
		OnlyFileName = filename:basename(FullName, ".json"),
%%			io:format("FullName=~s,Path=~s,OnlyFileName=~s~n",[FullName,Path,OnlyFileName]),
		put_recordName(OnlyFileName),
		{ok, Terms} = file:consult(FullName),
%%			io:format("Terms=~p~n",[Terms]),
		FileContent = make_content(Terms),
		FileName2 = Path ++ "/"++ OnlyFileName ++".erl",
%%			io:format("FileName2=~s,FileContent=~p,~n",[FileName2,FileContent]),
		file:write_file(FileName2, FileContent, [write]),
%%		halt(),
		ok
	catch
		_:Why:Stacktrace ->
			error_logger:error_msg("Why=~p,Stacktrace=~p~n", [Why, Stacktrace])
	end.

make_content(Terms) ->
	make_content(Terms, "").
make_content([], Content) -> Content;
make_content([Args | T], Content) ->
	AppendStr = make_single_content(Args, ""),
	make_content(T, Content ++ AppendStr).

make_single_content([], Content) ->
	AppendStr = "get(_)->{}.",
	Content++AppendStr;
make_single_content([Tuple | T], Content) ->
	Func = fun(Term,{AccStr,AccIndex}) ->
		case AccIndex of
			1 -> {AccStr++util:tryTerm2String(Term)++")->",2};
			2 -> {AccStr++"{"++get_recordName(),3};
			_ -> {AccStr++","++util:tryTerm2String(Term),AccIndex}
		end
		end,
	{TempStr,_}=lists:foldl(Func,{"get(",1},tuple_to_list(Tuple)),
	AppendStr = TempStr++"};\r\n",
	make_single_content(T, Content ++ AppendStr).

put_recordName(Name) -> put(recordName,Name).
get_recordName() -> get(recordName).