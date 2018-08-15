%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2018 17:27
%%%-------------------------------------------------------------------
-module(web_util).
-author("chenlong").

-include("common.hrl").

%% API
-export([route/1,send/2,send/3]).

send(Req, Ret, Msg) ->
	{ok, Json} = to_json(Msg),
	RetTuple = [{"ret":integer_to_list(Ret)}],
	Json2=list_to_tuple(RetTuple++tuple_to_list(Json)),
	Req:ok({"text/html",Json2}).
send(Req, Json) ->
	Req:ok({"text/html",Json}).


route(Req) ->
	Json = Req:parse_qs(),
	FuncName = proplists:get_value("funname",Json),
	case FuncName of
		"getRoleInfo" -> role:getRoleInfo(Req);
		_ -> todo
	end,
	ok.