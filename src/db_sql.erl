%% @author crimoon11
%% @doc


-module(db_sql).
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([getRole/1,setRole/1,isRoleExist/1,getAllRoleRankInfo/0]).

-define(GET_SQL_MAIL_NUM, 70).% the max num of mails get from sql once

%% 获取日志表的表名,日志表按月来分表
%%get_log_table_name(t_gold_pay_add) ->
%%    t_gold_pay_add;
%%get_log_table_name(TableName) when erlang:is_atom(TableName) ->
%%	{{Y, M, _}, _} = erlang:localtime(),
%%	lists:flatten(io_lib:format("~w_~w_~w", [TableName,Y,M])).

getRole(RoleID) ->
	Sql = io_lib:format("select * from gRole where roleID = ~w",[RoleID]),
	case get_row(Sql) of
		[_RoleID,Name,Money,Gold,FishList,LoginDays,UnlockFishCfgID,FishBuyList,
			LoginTimestamp,OfflineTimestamp,LastRewardLoginTimestamp,SpeedTimestamp,
			IncFishID] ->
			#role{deviceID = RoleID,roleName = binary_to_list(Name),money = Money,gold = Gold,fishList = to_term(FishList),
				loginDays = LoginDays,unlockFishCfgID = UnlockFishCfgID,fishBuyList = to_term(FishBuyList),
				loginTimestamp = LoginTimestamp,offlineTimestamp = OfflineTimestamp,lastRewardLoginTimestamp = LastRewardLoginTimestamp,
				speedTimestamp = SpeedTimestamp,incFishID = IncFishID};
		_ -> #role{deviceID = RoleID}
	end.
setRole(Role) ->
	Sql = io_lib:format("replace into gRole values (~w,~s,~w,~w,~s,~w,~w,~s,~w,~w,~w,~w,~w)",
		[
			Role#role.deviceID,quote(Role#role.roleName),
			Role#role.money,Role#role.gold,
			to_bin(Role#role.fishList),Role#role.loginDays,
			Role#role.unlockFishCfgID,to_bin(Role#role.fishBuyList),
			Role#role.loginTimestamp,Role#role.offlineTimestamp,
			Role#role.lastRewardLoginTimestamp,Role#role.speedTimestamp,
			Role#role.incFishID
		]),
	sql_execute_with_log(Sql).
isRoleExist(RoleID)->
	Sql = io_lib:format("select * from gRole where roleID = ~w",[RoleID]),
	get_row(Sql) =/= [].

getAllRoleRankInfo() ->
	Sql = io_lib:format("select roleID,roleName,money from gRole",[]),
	List = get_rows(Sql),
	[{RoleID,binary_to_list(Name),Money} || [RoleID,Name,Money] <- List].
%%-------------钓鱼------------------
%%getPlayerFishing(RoleID) ->
%%	Sql = io_lib:format("select * from gfishing where roleID = ~w",[RoleID]),
%%	get_row(Sql).
%%
%%setPlayerFishing(RoleID, #playerFishing{}=PlayerFishing) ->
%%	Sql = io_lib:format("replace into gfishing values (~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~s,~w,~w,~w,~s)",
%%		[
%%			RoleID,
%%			PlayerFishing#playerFishing.poolID,
%%			PlayerFishing#playerFishing.fishingLv,
%%			PlayerFishing#playerFishing.fishingExp,
%%			PlayerFishing#playerFishing.fishingCount,
%%			PlayerFishing#playerFishing.leftCount,
%%			PlayerFishing#playerFishing.buyCount,
%%			PlayerFishing#playerFishing.buyLeftCount,
%%			PlayerFishing#playerFishing.time0,
%%			PlayerFishing#playerFishing.time1,
%%			PlayerFishing#playerFishing.time2,
%%			PlayerFishing#playerFishing.autoTime0,
%%			PlayerFishing#playerFishing.autoTime1,
%%			PlayerFishing#playerFishing.autoTime2,
%%			PlayerFishing#playerFishing.rodLv,
%%			PlayerFishing#playerFishing.creelLv,
%%			to_bin(role_fishing:reward2tuple(PlayerFishing#playerFishing.creelList)),
%%			PlayerFishing#playerFishing.autoExp,
%%			PlayerFishing#playerFishing.fishingState,
%%			PlayerFishing#playerFishing.curSkinID,
%%			to_bin(PlayerFishing#playerFishing.skinIDList)
%%		]),
%%	sql_execute_with_log(Sql);
%%setPlayerFishing(_RoleID,_) ->
%%	ok.

%%-------------钓鱼 END------------------

%% ====================================================================
%% 玩家数据 持久化
%% ====================================================================
sql_execute_with_log(Sql)	->
	case emysql:execute(?DB,Sql) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
	end.

%% 传入的Sql变量为多条sql语句的集合,最好加入事务的保护,保证整个数据操作的原子性
sql_execute_with_log2(Sql)   ->
    case emysql:execute(?DB,Sql) of
        [{ok_packet, _,_,_RetId,_,_,_},{ok_packet, _,_,RetId,_,_,_}]    ->
            {ok,RetId};
        {result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
            {ok,Rows};
        {error_packet, _, _,ErrCode,Reason2}    ->
            ?ERR("sql ****~s*****execute with err:~p,~s",[Sql,ErrCode,Reason2]),
            {error,{ErrCode,Reason2}};
        Exception ->
            ?ERR("sql ****~s*****execute with err:~p~n",[Sql,Exception])
    end.

sql_execute_sqls(Sql) ->
	Result = emysql:execute(?DB, Sql),
	lists:foldl(fun(E, CntAcc)->
						case E of
							{ok_packet, _,_,_RetId,_,_,_}	->
								CntAcc+1;
							{result_packet, _SeqNum,_FieldList,_Rows,_Extra} ->
								CntAcc+1;
							{error_packet, _, _,ErrCode,Reason2} ->
								?ERR("sql ****~s*****execute ~wth sqls with err:~p,~s",[Sql,CntAcc,ErrCode,Reason2]),
								CntAcc+1;
							Exception ->
								?ERR("sql ****~s*****execute ~wth sqls with err:~p~n",[Sql,CntAcc,Exception])
						end
					end,0, Result),
	ok.

sql_execute_with_log(Statement, Args)	->
	case emysql:execute(?DB,Statement,Args) of
		{ok_packet, _,_,RetId,_,_,_}	->
			{ok,RetId};
		{result_packet, _SeqNum,_FieldList,Rows,_Extra} ->
			{ok,Rows};
		{error_packet, _, _,ErrCode,Reason2}	->
			?ERR("sql ****~p,~p*****execute with err:~p,~s",[Statement,Args,ErrCode,Reason2]),
			{error,{ErrCode,Reason2}};
		Exception ->
			?ERR("sql ****~p,~p*****execute with err:~p~n",[Statement,Args,Exception])
	end.

get_all(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			List;
		_ ->
			[]
	end.

get_row(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_row(Statement, Sql) ->
	case sql_execute_with_log(Statement, Sql) of
		{ok,List} ->
			case length(List) of
				1 ->
					[List2] = List,
					List2;
				0 ->
					[];
				Num when Num > 1 ->
					[List2|_] = List,
					List2;
				_ ->	
					[]
			end;	 
		_ ->
			[]
	end.

get_rows(Sql) ->
	case sql_execute_with_log(Sql) of
		{ok,Lists} ->
			Lists;	 
		_ ->
			[]
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 批量插入
make_sql_batch(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, ";", List),
	Sql++tl(Str).

make_sql_batch2(Sql, Format, List) when List =/= []->
	Str = lists:foldl(fun(E,Acc) ->
							  ","++io_lib:format(Format,E)++Acc
					  end, "", List),
	Sql++tl(Str).

%% 分段批量插入
make_sql_batch_by_piece(Sql, Format, List, PieceNum) ->
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece(Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
	if Acc == "" ->
		   ignore;
	   true ->
		   Sql2 = Sql ++ tl(Acc),
		   sql_execute_with_log(Sql2)
	end;
make_sql_batch_by_piece(Sql, Format, List, PieceNum, PieceNum, Acc) ->
	Sql2 = Sql ++ tl(Acc),
	sql_execute_with_log(Sql2),
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece(Sql, Format, [E|List], PieceNum, AccNum, Acc) ->
	Acc2 = ","++io_lib:format(Format,E)++Acc,
	make_sql_batch_by_piece(Sql, Format, List, PieceNum, AccNum+1, Acc2).
	
	
to_term(Bin)->
	to_term(Bin,[]).
to_term(Bin, Default) ->
	case catch binary_to_term(Bin) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

to_bin(Term) ->
	quote(term_to_binary(Term)).

compress_encode(Term) ->
	zlib:compress(term_to_binary(Term)).

uncompress_decode(Bin) ->
	uncompress_decode(Bin,[]).
uncompress_decode(Bin, Default) ->
	case catch binary_to_term(zlib:uncompress(Bin)) of
		{'EXIT',_} ->
			Default;
		Term ->
			Term
	end.

datetime({{A,B,C},{D,E,F}}) ->
	io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[A,B,C,D,E,F]);
datetime(Err) ->
	?ERR("datetime err:~p~n",[Err]).
date({A,B,C}) ->
	io_lib:format("~w-~.2.0w-~.2.0w",[A,B,C]);
date(Err) ->
	?ERR("date err:~p~n",[Err]).
time({A,B,C}) ->
	io_lib:format("~.2.0w:~.2.0w:~.2.0w",[A,B,C]).
minute({A,B,_C}) ->
	io_lib:format("~.2.0w:~.2.0w",[A,B]).
	

bool2int(true)->
	1;
bool2int(false)->
	0.

int2bool(1)->
	true;
int2bool(0)->
	false.

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote2(String) when is_list(String) ->
    lists:reverse(quote(String, []));
quote2(Bin) when is_binary(Bin) ->
    list_to_binary(quote2(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).