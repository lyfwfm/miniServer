%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 玩家公共信息进程
%%% @end
%%% Created : 17. 八月 2018 11:42
%%%-------------------------------------------------------------------
-module(role_server).
-author("chenlong").

-behaviour(gen_server).

-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([getRole/1, getRoleProperty/2, isRoleExist/1,operateRole/2,insertRole/1,offlineRole/1,isRoleEtsExist/1]).

-define(SERVER, ?MODULE).

-record(state, {}).
-define(LOOP_TIME, 1 * 1000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	?INFO("~p begin init",[?MODULE]),
	ets:new(?ETS_ROLE, [named_table, {keypos, #role.deviceID}, set, protected]),
	ets:new(?ETS_ROLE_DOUBLE,[named_table,{keypos,1},set,public]),
	ets:new(?ETS_ROLE_RANK,[named_table,{keypos,1},set,protected]),
	erlang:send_after(?LOOP_TIME, self(), heartbeat),
	initRoleRank(),
	?INFO("~p init success",[?MODULE]),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
	try
		case Info of
			heartbeat ->
				do_heartbeat();
			{operateRole, RoleID, OperateList} ->
				doOperateRole(RoleID, OperateList);
			{insertRole, Role} ->
				setRole(Role);
			{offlineRole, RoleID} ->
				doRoleOffline(getRole(RoleID));
			_ ->
				?ERR("no match Info=~p", [Info])
		end
	catch
		_:Why:StackTrace ->
			?ERR("role_server handle_info Info=~p,Why=~p,StackTrace=~p", [Info, Why,StackTrace])
	end,
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% exported functions
%%%===================================================================
%%获取用户数据，如果ETS没有，则从数据库加载
getRole(RoleID) ->
	case ets:lookup(?ETS_ROLE, RoleID) of
		[Role] ->Role;
		_ ->
			Role = db_sql:getRole(RoleID),
			insertRole(Role),
			Role
	end.

getRoleProperty(RoleID, PropertyIndex) ->
	Role = getRole(RoleID),
	element(PropertyIndex, Role).

setRole(Role) ->
	ets:insert(?ETS_ROLE, Role),
	spawn(db_sql,setRole,[Role]).

%%inner
isRoleExist(RoleID) ->
	case ets:member(?ETS_ROLE, RoleID) of
		?FALSE ->
			db_sql:isRoleExist(RoleID);
		_ -> ?TRUE
	end.
isRoleEtsExist(RoleID)->
	ets:member(?ETS_ROLE,RoleID).
operateRole(RoleID,OperateList) ->
	?SERVER ! {operateRole, RoleID, [{set,#role.heartbeatTimestamp,util:now()}]++OperateList}.
insertRole(Role) ->
	Self = self(),
	case whereis(?SERVER) of
		Self -> setRole(Role);
		_ -> ?SERVER ! {insertRole, Role}
	end.
offlineRole(RoleID) ->
	?SERVER ! {offlineRole, RoleID}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
do_heartbeat() ->
	erlang:send_after(?LOOP_TIME, self(), heartbeat),
	Now = util:now(),
	Func = fun(RoleID) ->
		#role{heartbeatTimestamp = Heartbeat,
			money = OldMoney, fishList = FishList,speedTimestamp = SpeedTimestamp} = Role = getRole(RoleID),
%%		?INFO("-------------RoleID=~p,OldMoney=~p",[RoleID,OldMoney]),
		case Now - Heartbeat > ?HEART_BEAT_OFF_TIME of
			?TRUE ->%%判定离线
				doRoleOffline(Role);
			_ ->%%计算每条鱼收益
				IsWorkingFishFull = role:isWorkingFishFull(Role),
				FishFunc = fun(#fish{state = FishState, worktimestamp = WorkTime,cfgID = FishCfgID,fishID = FishID} = Fish, {AccMoney, AccFishList}) ->
					case FishState of
						?FISH_STATE_WORKING ->
							IsSpeedUp = SpeedTimestamp >= Now,
							FishCfg = fish_cfg:get(FishCfgID),
							NormalTime = util:getTupleValue(FishCfg,#fish_cfg.time,4),
							MakeMoneyInternalTime = util:getTernaryValue(IsSpeedUp,trunc(NormalTime/2),NormalTime),%%鱼的赚钱间隔时间
							case Now - WorkTime >= MakeMoneyInternalTime of
								?TRUE ->
									Income = util:getTupleValue(FishCfg,#fish_cfg.income,0),%%配置钱数量
									AddMoney = trunc(util:getTernaryValue(IsWorkingFishFull,Income*1.1,Income)),%%满鱼工作的情况，增加10%的收益
%%									?INFO("IsSpeedUp=~p,FishID=~p,NormalTime=~p,fishCfgID = ~p,AddMoney=~p,TotalAdd=~p",[IsSpeedUp,FishID,NormalTime,FishCfgID,AddMoney,AccMoney + AddMoney]),
									{AccMoney + AddMoney, [Fish#fish{worktimestamp = Now} | AccFishList]};
								_ -> {AccMoney, [Fish | AccFishList]}
							end;
						_ -> {AccMoney, [Fish | AccFishList]}
					end
				           end,
				{FishMoney, NewFishList} = lists:foldl(FishFunc, {0, []}, FishList),
				case FishMoney > 0 of
					?TRUE ->
						NewRole = Role#role{money = OldMoney + FishMoney, fishList = NewFishList},
%%						?INFO("+++++++++++++++++RoleID=~p,NewMoney=~p",[RoleID,OldMoney + FishMoney]),
						setRole(NewRole),
						updateRankInfo(Role, FishMoney);
					_ -> ok
				end
		end
	       end,
	util:ets_foreach_key(Func, ?ETS_ROLE),
	ok.

doRoleOffline(Role) ->
	ets:delete(?ETS_ROLE, Role#role.deviceID),
	Now = util:now(),
%%	ets:insert(?ETS_ROLE,Role#role{offlineTimestamp = Now}),
	spawn(db_sql,setRole,[Role#role{offlineTimestamp = Now}]).

doOperateRole(RoleID, OperateList) ->
	Role = getRole(RoleID),
	Func = fun({add, PropertyIndex, Value}, AccRole) ->
		OldValue = element(PropertyIndex, AccRole),
		%%这里处理增加的财富，因为财富榜是财富总榜不是当前的财富值
		case PropertyIndex =:= #role.money of
			?TRUE -> updateRankInfo(Role, Value);
			_ -> ok
		end,
		setelement(PropertyIndex, AccRole, OldValue + Value);
		({set, PropertyIndex, Value}, AccRole) ->
			setelement(PropertyIndex, AccRole, Value);
		({dec, PropertyIndex, Value}, AccRole) ->
			OldValue = element(PropertyIndex, AccRole),
			setelement(PropertyIndex, AccRole, OldValue - Value);
		(_, AccRole) -> AccRole
	       end,
	NewRole = lists:foldl(Func, Role, OperateList),
	setRole(NewRole).

%%启动服务器，初始化排行榜
initRoleRank() ->
	List = db_sql:getAllRoleRankInfo(),
	ets:insert(?ETS_ROLE_RANK,List).

%%这里处理增加的财富，因为财富榜是财富总榜不是当前的财富值
updateRankInfo(Role, AddValue)when is_integer(AddValue) andalso AddValue>0 ->
	TRankOldValue = util:getEtsElement(?ETS_ROLE_RANK,Role#role.deviceID,3,0),
	RankOldValue = util:getTernaryValue(is_integer(TRankOldValue),TRankOldValue,0),
	ets:insert(?ETS_ROLE_RANK,{Role#role.deviceID,Role#role.roleName,RankOldValue+AddValue});
updateRankInfo(_Role,_) -> ok.