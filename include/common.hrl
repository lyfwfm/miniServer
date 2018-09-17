%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2018 17:32
%%%-------------------------------------------------------------------
-author("chenlong").
-include("msg.hrl").
-include("cfg_record.hrl").

-define(UNDEFINED, undefined).
-define(SUCCESS, "success").

-define(ETS_ROLE, ets_role).%%进程保存所有玩家信息的ETS
-define(ETS_ROLE_DOUBLE,ets_role_double).%%保存玩家翻倍收益信息{RoleID,[{CfgID,OriginMoney}]} CfgID为鱼配置ID，为0表示离线收益
-define(ETS_ROLE_RANK,ets_role_rank).%%排行榜ets
-define(HEART_BEAT_OFF_TIME, 30).

-define(FISH_STATE_IDLE,0).
-define(FISH_STATE_WORKING,1).
-record(fish,{
	fishID=0,%%鱼的唯一ID
	cfgID=0,
	state=?FISH_STATE_IDLE,
	worktimestamp=0%%每轮工作的起始时间戳
}).
-record(role,{
	deviceID="",%%玩家设备ID 作为唯一ID
	roleName="",
	money=0,
	gold=0,
	fishList=[],%%玩家所有鱼的情况[#fish{}]
	loginDays=0,%%连续登陆的天数
	unlockFishCfgID=0,
	fishBuyList=[],%%玩家购买鱼的情况[{CfgID,Count}]
	loginTimestamp=0,
	offlineTimestamp=0,
	lastRewardLoginTimestamp=0,%%领取登陆奖励的时间戳
	heartbeatTimestamp=0,
	speedTimestamp=0,%%加速的结束时间戳
	incFishID=1,%%自增的鱼唯一ID，自己计数
	vedioCount=0,%%今日已看视频次数
	dayTimestamp=0,%%用来判断跨天
	headurl = ""%%头像网址
}).
-json({role,{string,"name"}}).



-define(DB, tk).
-define(ONE_DAY_SECONDS, 86400).
-define(ONE_HOUR_SECONDS,3600).
-define(TEN_MINUTES_SECONDS,600).

-define(CATCH(Expression), (
	try Expression
	catch
		ErrType:Reason ->
			?ERR("ErrType:~1000p, ErrReason:~1000p, Expression=~s",[ErrType, Reason, ??Expression]),
			{'EXIT',{ErrType, Reason}}
	end
)).
-define(TRUE, true).
-define(FALSE, false).


%%-------------------------------------------------------------------------
%% 信息
-define(INFO(Format), hdlt_logger:log("[~p:~p]"++Format,[?MODULE,?LINE])).
-define(INFO(Format, Args), hdlt_logger:log("[~p:~p]"++Format,[?MODULE,?LINE]++Args)).

%% 调试信息
-define(DEBUG(Format), hdlt_logger:debug("[~p:~p]"++Format,[?MODULE,?LINE])).
-define(DEBUG(Format, Args), hdlt_logger:debug("[~p:~p]"++Format,[?MODULE,?LINE]++Args)).

%% 错误信息
-define(ERR(Format), hdlt_logger:error_out("[~p:~p]"++Format,[?MODULE,?LINE])).
-define(ERR(Format, Args), hdlt_logger:error_out("[~p:~p]"++Format,[?MODULE,?LINE]++Args)).
%%-------------------------------------------------------------------------
