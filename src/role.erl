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
-export([getRoleInfo/1]).

getRoleInfo(Req) ->
	%%todo 从本地读取数据
	Role = #role{roleName = "yourRole"},
	web_util:send(Req,?SUCCESS,Role).