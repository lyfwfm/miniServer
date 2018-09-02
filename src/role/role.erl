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
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([cs_login/3, cs_create_role/3, cs_put_fish/3, cs_remove_fish/3, cs_merge_fish/3,
	cs_sell_fish/3, cs_buy_fish/3, cs_speed_up/3, cs_heart_beat/3, cs_offline/3, cs_get_rank/3,
	cs_double/3,
	cs_gm_24h/3
]).

-export([sortAndSend/4, isWorkingFishFull/1]).

cs_login(Req, FuncName, [RoleID]) ->
	case role_server:isRoleExist(RoleID) of
		?FALSE -> web_util:send(Req, FuncName, "no_role", {});
		_ ->
			OldRole = role_server:getRole(RoleID),
			%%计算离线收益
			OfflineMoney = calcOfflineMoney(OldRole),
			%%检测连续登陆天数
			NewLoginDays = checkLoginDays(OldRole),
			%%检测发放连续登陆奖励
			LoginGold = checkLoginReward(OldRole#role.lastRewardLoginTimestamp, NewLoginDays),
			NewMoney = OldRole#role.money + OfflineMoney,
			NewGold = OldRole#role.gold + LoginGold,
			Now = util:now(),
			OperateList = [
				{add, #role.money, OfflineMoney},
				{add, #role.gold, LoginGold},
				{set, #role.loginTimestamp, Now},
				{set, #role.loginDays, NewLoginDays},
				{set, #role.lastRewardLoginTimestamp, Now}
			],
			role_server:operateRole(RoleID, OperateList),
			%%将离线收益加入翻倍ETS
			insertRoleDouble(RoleID, 0, OfflineMoney),
			Msg = #sc_login{
				userName = list_to_binary(OldRole#role.roleName),
				packageFishlist = [#pk_fish{id = Fish#fish.fishID, cfg_id = Fish#fish.cfgID, isWorking = Fish#fish.state =:= ?FISH_STATE_WORKING}
					|| Fish <- OldRole#role.fishList],
				gold = NewMoney,
				diamond = NewGold,
				offline_gold = OfflineMoney,
				login_days = NewLoginDays,
				is_login_reward = LoginGold > 0,
				unlocked_fishes = OldRole#role.unlockFishCfgID,
				fish_buy_list = [#pk_fish_buy{cfg_id = FishCfgID, buy_count = BuyCount}
					|| {FishCfgID, BuyCount} <- OldRole#role.fishBuyList]

			},
			web_util:send(Req, FuncName, ?SUCCESS, Msg)
	end.

cs_create_role(Req, FuncName, [TRoleID, TRoleName]) ->
	RoleID = util:tryTerm2String(TRoleID),
	RoleName = util:tryTerm2String(TRoleName),
	case role_server:isRoleExist(RoleID) of
		?TRUE -> web_util:send(Req, FuncName, "have_role", {});
		_ ->
			Now = util:now(),
			LoginGold = checkLoginReward(0, 1),
			Role = #role{
				deviceID = RoleID,
				roleName = RoleName,
				loginDays = 1,
				money = 0,
				gold = LoginGold,
				unlockFishCfgID = 1,
				loginTimestamp = Now,
				lastRewardLoginTimestamp = Now,
				heartbeatTimestamp = Now,
				fishList = [#fish{fishID = 1, cfgID = 1}],
				fishBuyList = [{1, 1}]
			},
			role_server:insertRole(Role),
			Msg = #sc_login{
				userName = list_to_binary(RoleName),
				packageFishlist = [#pk_fish{id = Fish#fish.fishID, cfg_id = Fish#fish.cfgID, isWorking = Fish#fish.state =:= ?FISH_STATE_WORKING}
					|| Fish <- Role#role.fishList],
				gold = 0,
				diamond = LoginGold,
				offline_gold = 0,
				login_days = 1,
				is_login_reward = LoginGold > 0,
				unlocked_fishes = Role#role.unlockFishCfgID,
				fish_buy_list = [#pk_fish_buy{cfg_id = FishCfgID, buy_count = BuyCount}
					|| {FishCfgID, BuyCount} <- Role#role.fishBuyList]
			},
			web_util:send(Req, FuncName, ?SUCCESS, Msg)
	end.

cs_put_fish(Req, FuncName, [RoleID, FishID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		%%魚是否滿
		Role = role_server:getRole(RoleID),
		case isWorkingFishFull(Role) of
			?TRUE -> web_util:send(Req, FuncName, "woring_fish_full", {});
			_ ->
				case lists:keytake(FishID, #fish.fishID, Role#role.fishList) of
					?FALSE -> web_util:send(Req, FuncName, "no_this_fish", {});
					{value, #fish{state = OldState} = Fish, T} ->
						case OldState =:= ?FISH_STATE_IDLE of
							?TRUE ->
								Now = util:now(),
								NewFishList = [Fish#fish{state = ?FISH_STATE_WORKING, worktimestamp = Now} | T],
								role_server:operateRole(RoleID, [{set, #role.fishList, NewFishList}]),
								web_util:send(Req, FuncName, ?SUCCESS, {});
							_ -> web_util:send(Req, FuncName, "fish_not_idle", {})
						end
				end
		end
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_remove_fish(Req, FuncName, [RoleID, FishID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		Role = role_server:getRole(RoleID),
		case lists:keytake(FishID, #fish.fishID, Role#role.fishList) of
			{value, #fish{state = FishState} = Fish, T} ->
				case FishState =:= ?FISH_STATE_WORKING of
					?TRUE ->
						NewFishList = [Fish#fish{state = ?FISH_STATE_IDLE} | T],
						role_server:operateRole(RoleID, [{set, #role.fishList, NewFishList}]),
						web_util:send(Req, FuncName, ?SUCCESS, {});
					_ -> web_util:send(Req, FuncName, "fish_not_working", {})
				end;
			_ -> web_util:send(Req, FuncName, "no_this_fish", {})
		end
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_merge_fish(Req, FuncName, [RoleID, FishID1, FishID2]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		Role = role_server:getRole(RoleID),
		case lists:keytake(FishID1, #fish.fishID, Role#role.fishList) of
			{value, #fish{state = FishState1, cfgID = CfgID1}, T1} ->
				case FishState1 =:= ?FISH_STATE_IDLE of
					?TRUE ->
						case lists:keytake(FishID2, #fish.fishID, T1) of
							{value, #fish{state = FishState2, cfgID = CfgID2}, T2} ->
								case FishState2 =:= ?FISH_STATE_IDLE of
									?TRUE ->
										%%两条鱼是否配置相同
										case CfgID1 =:= CfgID2 of
											?TRUE ->
												NewFishID = Role#role.incFishID + 1,
												NewFishCfgID = CfgID1 + 1,
												NewFish = #fish{fishID = NewFishID, cfgID = NewFishCfgID},%%产生新鱼
												NewFishList = [NewFish | T2],
												HigherCfgID = case NewFishCfgID > Role#role.unlockFishCfgID of
													              ?TRUE ->%%解锁新的鱼
														              NewFishCfg = fish_cfg:get(NewFishCfgID),
														              NewPrice = util:getTupleValue(NewFishCfg, #fish_cfg.price, 0),
														              role_server:operateRole(RoleID, [
															              {add, #role.money, NewPrice},
															              {add, #role.incFishID, 1},
															              {set, #role.fishList, NewFishList},
															              {set, #role.unlockFishCfgID, NewFishCfgID}]),
														              %%将解锁新鱼奖励放入翻倍ETS
														              insertRoleDouble(RoleID, NewFishCfgID, NewPrice),
														              NewFishCfgID;
													              _ ->
														              role_server:operateRole(RoleID, [
															              {add, #role.incFishID, 1},
															              {set, #role.fishList, NewFishList}]),
														              Role#role.unlockFishCfgID
												              end,
												Msg = #sc_merge_fish{
													id = NewFishID,
													cfg_id = NewFishCfgID,
													unlock_cfg_id = HigherCfgID
												},
												web_util:send(Req, FuncName, ?SUCCESS, Msg);
											_ -> web_util:send(Req, FuncName, "fish_not_the_same", {})
										end;
									_ -> web_util:send(Req, FuncName, "fish_not_idle", {})
								end;
							_ -> web_util:send(Req, FuncName, "no_this_fish", {})
						end;
					_ -> web_util:send(Req, FuncName, "fish_not_idle", {})
				end;
			_ -> web_util:send(Req, FuncName, "no_this_fish", {})
		end
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_sell_fish(Req, FuncName, [RoleID, FishID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		Role = role_server:getRole(RoleID),
		case lists:keytake(FishID, #fish.fishID, Role#role.fishList) of
			{value, #fish{cfgID = FishCfgID}, T} ->
				FishCfg = fish_cfg:get(FishCfgID),
				AddMoney = trunc(util:getTupleValue(FishCfg, #fish_cfg.price, 0) / 2),
				role_server:operateRole(RoleID, [{add, #role.money, AddMoney}, {set, #role.fishList, T}]),
				Msg = #sc_sell_fish{gold = AddMoney},
				web_util:send(Req, FuncName, ?SUCCESS, Msg);
			_ -> web_util:send(Req, FuncName, "no_this_fish", {})
		end
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_buy_fish(Req, FuncName, [RoleID, FishCfgID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		Role = role_server:getRole(RoleID),
		FishBuyList = Role#role.fishBuyList,
		%%背包总共现在拥有不超过12
		case length(Role#role.fishList) >= 12 of
			?TRUE -> throw("out_of_buy_count");
			_ -> ok
		end,
		CostMoney = getFishCostMoney(FishBuyList, FishCfgID),
		case Role#role.money >= CostMoney of
			?TRUE ->
				%%产生新鱼
				NewFishID = Role#role.incFishID + 1,
				NewFish = #fish{fishID = NewFishID, cfgID = FishCfgID},
				NewFishBuyList = case lists:keytake(FishCfgID, 1, Role#role.fishBuyList) of
					                 {value, {_, OldCount}, T} -> [{FishCfgID, OldCount + 1} | T];
					                 _ -> [{FishCfgID, 1} | Role#role.fishBuyList]
				                 end,
				role_server:operateRole(RoleID, [
					{add, #role.incFishID, 1},
					{dec, #role.money, CostMoney},
					{set, #role.fishList, [NewFish | Role#role.fishList]},
					{set, #role.fishBuyList, NewFishBuyList}
				]),
				Msg = #sc_buy_fish{id = NewFishID, cfg_id = FishCfgID},
				web_util:send(Req, FuncName, ?SUCCESS, Msg);
			_ -> web_util:send(Req, FuncName, "not_enough_money", {})
		end
	catch
		throw:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_speed_up(Req, FuncName, [RoleID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		Now = util:now(),
		SpeedTime = 180,
		role_server:operateRole(RoleID, [{set, #role.speedTimestamp, Now + SpeedTime}]),
		web_util:send(Req, FuncName, ?SUCCESS, {})
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_heart_beat(Req, FuncName, [RoleID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		role_server:operateRole(RoleID, []),
    Role = role_server:getRole(RoleID),
		web_util:send(Req, FuncName, ?SUCCESS, #sc_heart_beat{money = Role#role.money})
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_offline(Req, FuncName, [RoleID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		role_server:offlineRole(RoleID),
		web_util:send(Req, FuncName, ?SUCCESS, {})
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_get_rank(Req, FuncName, [RoleID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		RoleList = ets:tab2list(?ETS_ROLE_RANK),
		spawn(?MODULE, sortAndSend, [Req, FuncName, RoleID, RoleList]),
		ok
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

cs_double(Req, FuncName, [RoleID, FishCfgID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		case ets:lookup(?ETS_ROLE_DOUBLE, RoleID) of
			[{_, List}] ->
				case lists:keytake(FishCfgID, 1, List) of
					{value, {_, Value}, T} ->
						role_server:operateRole(RoleID, [{add, #role.money, Value}]),
						case length(T) > 0 of
							?TRUE -> ets:insert(?ETS_ROLE_DOUBLE, {RoleID, T});
							_ -> ets:delete(?ETS_ROLE_DOUBLE, RoleID)
						end,
						web_util:send(Req, FuncName, ?SUCCESS, #sc_double{cfg_id = FishCfgID, addmoney = Value});
					_ -> web_util:send(Req, FuncName, "no_such_double", {})
				end;
			_ -> web_util:send(Req, FuncName, "no_such_double", {})
		end
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

%%----------------------------------------------------------------
cs_gm_24h(Req, FuncName, [RoleID]) ->
	try
		case role_server:isRoleExist(RoleID) of
			?TRUE -> ok;
			_ -> throw("role_not_login")
		end,
		Role = role_server:getRole(RoleID),
		IsWorkingFishFull = role:isWorkingFishFull(Role),
		FishFunc = fun(#fish{state = FishState, cfgID = FishCfgID}) ->
			case FishState of
				?FISH_STATE_WORKING ->
					FishCfg = fish_cfg:get(FishCfgID),
					NormalTime = util:getTupleValue(FishCfg, #fish_cfg.time, 4),
					Income = util:getTupleValue(FishCfg, #fish_cfg.income, 0),%%配置钱数量
					OneDayMoney = (Income / NormalTime) * 86400,
					AddMoney = util:getTernaryValue(IsWorkingFishFull, OneDayMoney * 1.1, OneDayMoney),%%满鱼工作的情况，增加10%的收益
					AddMoney;
				_ -> 0
			end
		           end,
		FishMoney = trunc(lists:sum(lists:map(FishFunc, Role#role.fishList))),
		role_server:operateRole(RoleID, [{add, #role.money, FishMoney}]),
		web_util:send(Req, FuncName, ?SUCCESS, #sc_gm_24h{addmoney = FishMoney})
	catch
		_:Error -> web_util:send(Req, FuncName, Error, {})
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%计算离线收益
calcOfflineMoney(Role) ->
	Now = util:now(),
	OfflineTime = Now - lists:max([Role#role.offlineTimestamp, Role#role.loginTimestamp, Role#role.heartbeatTimestamp]),
	{SpeedTime, NormalTime} = case Role#role.speedTimestamp > Now of
		                          ?TRUE ->
			                          {OfflineTime, 0};
		                          _ ->
			                          TSpeedTime = max(0, Role#role.speedTimestamp - Role#role.offlineTimestamp),
			                          {
				                          TSpeedTime,
				                          OfflineTime - TSpeedTime
			                          }
	                          end,
	CalcFunc = fun(#fish{cfgID = CfgID, state = FishState}) ->
		case FishState of
			?FISH_STATE_WORKING ->
				FishCfg = fish_cfg:get(CfgID),
				EachMoney = util:getTupleValue(FishCfg, #fish_cfg.income, 0) / util:getTupleValue(FishCfg, #fish_cfg.time, 1),%%算出每秒产出
				trunc(EachMoney * NormalTime + EachMoney * 2 * SpeedTime);
			_ -> 0
		end
	           end,
	TotalMoney = lists:sum(lists:map(CalcFunc, Role#role.fishList)),
	TotalMoney.

checkLoginDays(#role{loginDays = OldDays, offlineTimestamp = OffTime}) ->
	{OffDate, _} = util:seconds_to_datetime(OffTime + ?ONE_DAY_SECONDS),
	NowDate = erlang:date(),
	case OffTime == 0 orelse NowDate =:= OffDate of
		?TRUE -> min(OldDays + 1, 7);%%到7天后，每次连续登陆就是7的奖励
		_ -> 1
	end.

checkLoginReward(LastRewardTimestamp, LoginDays) ->
	{LastDate, _} = util:seconds_to_datetime(LastRewardTimestamp),
	NowDate = erlang:date(),
	case NowDate =:= LastDate of
		?TRUE -> 0;%%已经领取过了
		_ ->
			LoginRewardCfg = reward:get(LoginDays),
			util:getTupleValue(LoginRewardCfg, #reward.diamond, 0)
	end.

isWorkingFishFull(#role{fishList = FishList}) ->
	WorkingList = lists:filter(fun(#fish{state = State}) ->
		State =:= ?FISH_STATE_WORKING
	                           end, FishList),
	length(WorkingList) >= 10.

sortAndSend(Req, FuncName, RoleID, RoleList) ->
	SortList = lists:reverse(lists:keysort(3, RoleList)),
	Func = fun({TRoleID, RoleName, Money}, {AccRank, AccList, AccMyRank, AccMyMoney}) ->
		PlayerMsg = #pk_rank{
			rank = AccRank,
			userName = list_to_binary(RoleName),
			gold = Money,
			head_url = ""
		},
		{AccRank + 1, [PlayerMsg | AccList],
      util:getTernaryValue(TRoleID =:= RoleID, AccRank, AccMyRank),
      util:getTernaryValue(TRoleID =:= RoleID, Money, AccMyMoney)}
	       end,
	{_, MsgRankList, MyRank,TMyMoney} = lists:foldl(Func, {1, [], 0, 0}, SortList),
  MyMoney = case MyRank =< 0 of
    ?TRUE ->
      util:getEtsElement(?ETS_ROLE_RANK,RoleID,3,0);
    _ -> TMyMoney
  end,
	Msg = #sc_rank{
		my_rank = MyRank,
    my_money = MyMoney,
		rank_list = lists:sublist(MsgRankList, 100)
	},
	web_util:send(Req, FuncName, ?SUCCESS, Msg).

getFishCostMoney(FishBuyList, FishCfgID) ->
	FishCfg = fish_cfg:get(FishCfgID),
	NormalCost = util:getTupleValue(FishCfg, #fish_cfg.price, 0),
	case lists:keyfind(FishCfgID, 1, FishBuyList) of
		{_, Count} -> trunc(NormalCost * math:pow(1.18, Count));
		_ -> trunc(NormalCost)
	end.

insertRoleDouble(_RoleID, _CfgID, OriginMoney) when OriginMoney =< 0 ->
	ok;
insertRoleDouble(RoleID, CfgID, OriginMoney) ->
	case ets:lookup(?ETS_ROLE_DOUBLE, RoleID) of
		[{_, List}] ->
			NewList = lists:keystore(CfgID, 1, List, {CfgID, OriginMoney}),
			ets:insert(?ETS_ROLE_DOUBLE, {RoleID, NewList});
		_ ->
			ets:insert(?ETS_ROLE_DOUBLE, {RoleID, [{CfgID, OriginMoney}]})
	end.