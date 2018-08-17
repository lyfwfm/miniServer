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
-export([route/1,send/2,send/4]).

send(Req,FuncName, Ret, Msg) ->
	{ok, Json} = to_json(Msg),
	%%{"name":Name},{"code":Code},{"data":Data}
	RetString = "{\"name\":"++FuncName++"},{\"code\":"++Ret++"},{\"data\":",
	Json2=binary:list_to_bin(RetString++binary:bin_to_list(Json)++"}"),
	Req:ok({"text/html",Json2}).
send(Req, Json) ->
	Req:ok({"text/html",Json}).


route(Req) ->
	try
	Json = Req:parse_qs(),
	FuncName = proplists:get_value("name",Json),
	case FuncName of
		"login" ->%%登陆，获取玩家数据
			role:cs_login(Req,getValueListFromReq(Json));
		"create_role" ->%%创建玩家信息
			todo;
		"put_fish" ->%%放入鱼工作
			todo;
		"remove_fish" -> %%收回鱼，不工作
			todo;
		"merge" -> %%合成鱼
			todo;
		"sell_fish" -> %%售卖鱼
			todo;
		"buy_fish" -> %%购买鱼
			todo;
		"speed_up" -> %%加速
			todo;
		"heart_beat"->%%心跳
			todo;
		"offline"->%%离线
			todo;
		_ -> todo
	end,
	ok
catch
		_:Why ->
			?ERR("route function error Why=~p",[Why]),
			Req:ok({"text/html",<<"error_function">>})

	end.

%%依次返回客户端传来的值
getValueListFromReq(Json) ->
	Value = proplists:get_value("value",Json),
	Keys = proplists:get_keys(Value),
	Values = lists:map(fun(Key) ->
		proplists:get_value(Key,Value)
		end,Keys),
	io:format("Json=~p,Value=~p,Keys=~p,Values=~p~n",[Json,Value,Keys,Values]),
	Values.