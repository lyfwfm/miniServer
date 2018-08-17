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
-export([cs_login/2]).

cs_login(Req,[RoleID]) ->
	case role_server:isRoleExist(RoleID) of
		?FALSE ->web_util:send(Req,"login","no_role",<<>>);
		_ ->
			#role{}=Role=role_server:getRole(RoleID),
			Msg=#msg_login{
				userName = Role#role.roleName,
packageFishlist = todo,
				gold = Role#role.gold

			},
			web_util:send(Req,"login",?SUCCESS,Msg)
	end.