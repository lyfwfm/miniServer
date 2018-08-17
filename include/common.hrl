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

-define(UNDEFINED, undefined).
-define(SUCCESS, success).

-define(FISH_STATE_IDLE,0).
-define(FISH_STATE_WORKING,1).
-record(fish,{
	fishID=0,%%鱼的唯一ID
	cfgID=0,
	state=?FISH_STATE_IDLE
}).
-record(role,{
	deviceID=0,%%玩家设备ID 作为唯一ID
	roleName="",
	money=0,
	gold=0,
	fishList=[],%%玩家所有鱼的情况[#fish{}]
	loginDays=0,%%连续登陆的天数
	unlockFishCfgID=0,
	fishBuyList=[]%%玩家购买鱼的情况[{CfgID,Count}]
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
%%{0, no_log, "No log"}
%%,{1, critical, "Critical"}
%%,{2, error, "Error"}
%%,{3, warning, "Warning"}
%%,{4, info, "Info"}
%%,{5, debug, "Debug"}
%%,{6, dev, "Dev"}
-define(LOG_LEVEL, 5).
-define(LOG_FILE_DIR, "./log").

%% 致命错误
-define(CRITICAL(Format), logger:critical_msg(?MODULE,?LINE, Format, [])).
-define(CRITICAL(Format, Args), logger:critical_msg(?MODULE,?LINE, Format, Args)).

%% 信息
-define(INFO(Format), logger:info_msg(?MODULE,?LINE, Format, [])).
-define(INFO(Format, Args), logger:info_msg(?MODULE,?LINE, Format, Args)).

%% 警告
-define(WARNING(Format), logger:warning_msg(?MODULE,?LINE, Format, [])).
-define(WARNING(Format, Args), logger:warning_msg(?MODULE,?LINE, Format, Args)).

%% 开发信息
-define(DEV(Format), logger:dev_msg(?MODULE,?LINE, Format, [])).
-define(DEV(Format, Args), logger:dev_msg(?MODULE,?LINE, Format, Args)).

%% 调试信息
-define(DEBUG(Format), logger:debug_msg(?MODULE,?LINE, Format, [], [{module, ?MODULE}])).
-define(DEBUG(Format, Args), logger:debug_msg(?MODULE,?LINE, Format, Args, [{module, ?MODULE}])).

%% 错误信息
%%-define(ERR(Format), logger:error_msg(?MODULE,?LINE, Format, [])).
%%-define(ERR(Format, Args), logger:error_msg(?MODULE,?LINE, Format, Args)).
-define(ERR(Format), error_logger:error_msg(Format, [])).
-define(ERR(Format, Args), error_logger:error_msg(Format, Args)).
%%-------------------------------------------------------------------------
