%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2018 17:35
%%%-------------------------------------------------------------------
-module(role).
-author("chenlong").
-include("common.hrl").

%% API
-export([cs_login/3,cs_create_role/3]).

cs_login(Req,FuncName,[RoleID]) ->
	case role_server:isRoleExist(RoleID) of
		?FALSE ->web_util:send(Req,FuncName,"no_role",{});
		_ ->
			OldRole=role_server:getRole(RoleID),
			%%计算离线收益
			OfflineMoney =calcOfflineMoney(OldRole),
			%%检测连续登陆天数
			NewLoginDays = checkLoginDays(OldRole),
			%%检测发放连续登陆奖励
			LoginMoney = checkLoginReward(OldRole#role.lastRewardLoginTimestamp,NewLoginDays),
			NewMoney = OldRole#role.money+OfflineMoney+LoginMoney,
			Now = util:now(),
			OperateList = [
				{add,#role.money,OfflineMoney+LoginMoney},
				{set,#role.loginTimestamp,Now},
				{set,#role.loginDays,NewLoginDays},
				{set,#role.lastRewardLoginTimestamp,Now},
				{set,#role.heartbeatTimestamp,Now}
			],
			role_server:operateRole(RoleID, OperateList),
			Msg=#msg_login{
				userName = OldRole#role.roleName,
				packageFishlist = [#msg_fish{id = Fish#fish.fishID,cfg_id = Fish#fish.cfgID,isWorking = Fish#fish.state=:=?FISH_STATE_WORKING}
					|| Fish <- OldRole#role.fishList],
				gold = NewMoney,
				offline_gold = OfflineMoney,
				login_days = NewLoginDays,
				unlocked_fishes = OldRole#role.unlockFishCfgID,
				fish_buy_list = [#msg_fish_buy{cfg_id = FishCfgID,buy_count = BuyCount}
				|| {FishCfgID,BuyCount} <- OldRole#role.fishBuyList]

			},
			web_util:send(Req,FuncName,?SUCCESS,Msg)
	end.

cs_create_role(Req,FuncName,[RoleID,RoleName]) ->
	case role_server:isRoleExist(RoleID) of
		?TRUE -> web_util:send(Req,FuncName,"have_role",{});
		_ ->
			Now = util:now(),
			LoginMoney = checkLoginReward(0,1),
			Role = #role{
				deviceID = RoleID,
				roleName = RoleName,
				loginDays = 1,
				money = LoginMoney,
				unlockFishCfgID = 0,%%todo
				loginTimestamp = Now,
				lastRewardLoginTimestamp = Now,
				heartbeatTimestamp = Now
			},
%%			role_server:insertRole(Role),
			Msg=#msg_login{
				userName = RoleName,
				packageFishlist = [],
				gold = LoginMoney,
				offline_gold = 0,
				login_days = 1,
				unlocked_fishes = 0,%%todo
				fish_buy_list = []
			},
			web_util:send(Req,FuncName,?SUCCESS,Msg)
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%计算离线收益
calcOfflineMoney(Role) ->
	Now = util:now(),
	OfflineTime = Now - Role#role.offlineTimestamp,
	CalcFunc = fun(#fish{cfgID = CfgID,state = FishState}) ->
		case FishState of
			?FISH_STATE_WORKING -> CfgID * OfflineTime;%%todo 读取配置
			_ -> 0
		end
		end,
	TotalMoney = lists:sum(lists:map(CalcFunc,Role#role.fishList)),
	TotalMoney.

checkLoginDays(#role{loginDays = OldDays,offlineTimestamp = OffTime}) ->
	{OffDate,_} = util:seconds_to_datetime(OffTime+?ONE_DAY_SECONDS),
	NowDate = erlang:date(),
	case OffTime == 0 orelse NowDate =:= OffDate of
		?TRUE -> OldDays+1;
		_ -> 1
	end.

checkLoginReward(LastRewardTimestamp,LoginDays) ->
	{LastDate,_}=util:seconds_to_datetime(LastRewardTimestamp),
	NowDate = erlang:date(),
	case NowDate =:= LastDate of
		?TRUE -> 0;%%已经领取过了
		_ -> LoginDays*100%%todo 读取配置
	end.