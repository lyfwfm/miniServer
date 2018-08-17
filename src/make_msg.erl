%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 八月 2018 17:16
%%%-------------------------------------------------------------------
-module(make_msg).
-author("chenlong").

%% API
-export([make_msg/1]).

make_msg([FileNameAtom]) ->
	FullName = atom_to_list(FileNameAtom),
	Path = filename:dirname(FullName),
%%	OnlyFileName = filename:basename(FullName, ".proto"),
	{ok,Terms}=file:consult(FullName),
	FileContent=make_content(Terms),
	FileName2 = Path ++ "\\msg.hrl",
	file:write_file(FileName2, FileContent, [write]),
%%	halt(),
	ok.

make_content(Terms) ->
	make_content(Terms,"").
make_content([],Content) -> Content;
make_content([Args|T],Content) ->
	{CR,CJ}=make_single_content(Args,"",""),
	make_content(T,Content++CR++CJ).

make_single_content([],ContentR,ContentJ) ->
	{ContentR++"}).\r\n",ContentJ++"}).\r\n"};
make_single_content([Term|T],ContentR,ContentJ) ->
	HaveNext = T=/=[],
	{AppendR,AppendJ}=
	case Term of
		{Name,Type} ->
			{
				atom_to_list(Name)++util:getTernaryValue(HaveNext,",",""),
				",{"++atom_to_list(Type)++",\""++atom_to_list(Name)++"\"}"
			};
		{Name,_Type,SubName} ->
			{
					atom_to_list(Name)++util:getTernaryValue(HaveNext,",",""),
					",{list,\""++atom_to_list(Name)++"\",[{type,"++atom_to_list(SubName)++"}]}"
			};
		Atom when is_atom(Atom) ->
			{
				"-record("++atom_to_list(Atom)++",{",
				"-json({"++atom_to_list(Atom)
			};
		_ ->
			io:format("***************error Term=~p~n",[Term]),
			{"",""}
	end,
	make_single_content(T,ContentR++AppendR,ContentJ++AppendJ).