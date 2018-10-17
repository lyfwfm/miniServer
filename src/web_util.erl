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
-define(RESPOND(Req,Json),Req:respond({200,[{"Access-Control-Allow-Origin","*"},{"Content-Type","text/html;charset=utf-8"}], Json})).
-define(OK(Req,Json),Req:ok({"text/html",Json})).

%% API
-export([route/1, send/2, send/4]).

send(Req, FuncName, Ret, {}) ->
	RetString = "{\"ret\":{\"name\":\"" ++ FuncName ++ "\",\"code\":\"" ++ Ret ++ "\",\"data\":{}}}",
	Json2 = binary:list_to_bin(RetString),
	?INFO("Json = ~s",[Json2]),
	?RESPOND(Req,Json2);
send(Req, FuncName, Ret, Msg) ->
	{ok, Json} = to_json(Msg),
	RetString = "{\"ret\":{\"name\":\"" ++ FuncName ++ "\",\"code\":\"" ++ Ret ++ "\",\"data\":",
	Json2 = binary:list_to_bin(RetString ++ binary:bin_to_list(Json) ++ "}}"),
	?INFO("Json = ~s",[Json2]),
	?RESPOND(Req,Json2).
send(Req, Json) ->
	?RESPOND(Req,Json).


route(Req) ->
	try
		Json = mochiweb_request:parse_qs(Req),
		FuncName = proplists:get_value("funcname", Json),
		case FuncName of
			"login" ->%%登陆，获取玩家数据
				spawn_out(role,cs_login,Req);
			"create_role" ->%%创建玩家信息
				spawn_out(role,cs_create_role,Req);
			"put_fish" ->%%放入鱼工作
				spawn_out(role,cs_put_fish,Req);
			"remove_fish" -> %%收回鱼，不工作
				spawn_out(role,cs_remove_fish,Req);
			"merge" -> %%合成鱼
				spawn_out(role,cs_merge_fish,Req);
			"sell_fish" -> %%售卖鱼
				spawn_out(role,cs_sell_fish,Req);
			"buy_fish" -> %%购买鱼
				spawn_out(role,cs_buy_fish,Req);
			"speed_up" -> %%加速
				spawn_out(role,cs_speed_up,Req);
			"heart_beat" ->%%心跳
				spawn_out(role,cs_heart_beat,Req);
			"offline" ->%%离线
				spawn_out(role,cs_offline,Req);
			"get_rank" ->%%获取排行榜
				spawn_out(role,cs_get_rank,Req);
			"double" ->%%看完广告后，收益翻倍
				spawn_out(role,cs_double,Req);
			"watch_vedio"->%%看视频，获得对应鱼的购买金钱
				spawn_out(role,cs_watch_vedio,Req);
			"client_data"->%%同步客户端数据
				spawn_out(role,cs_client_data,Req);
%%				spawn(role,cs_client_data,[Req,FuncName,[proplists:get_value("data", Json),proplists:get_value("data", Json)]]);

			"gm_24h" ->%%GM 快速获取24小时收益
				spawn_out(role,cs_gm_24h,Req);
			_ -> ?RESPOND(Req,<<"no_match_function">>)
		end,
			case FuncName of
				"heart_beat" -> ok;
				_ -> printJson(Json)
			end,
		ok
	catch
		_:Why:Stacktrace ->
			?ERR("route function error Why=~p, Stacktrace=~p", [Why,Stacktrace]),
			?RESPOND(Req,<<"error_function">>)

	end.

%%依次返回客户端传来的值
getValueListFromJson(Json) ->
	%%todo 先不包装，不用proplist:get_keys 因为它不保证顺序
%%	Value = proplists:get_value("value", Json),
%%	Keys = proplists:get_keys(Value),
%%	Values = lists:map(fun(Key) ->
%%		proplists:get_value(Key, Value)
%%	                   end, Keys),
%%	?INFO("Json=~p,Value=~p,Keys=~p,Values=~p~n", [Json, Value, Keys, Values]),
%%	Values.
	Fun = fun({_KeyStr,ValueStr},{AccIndex,AccValues}) ->
		if
			AccIndex =:= 2 -> {AccIndex+1,[ValueStr|AccValues]};
			AccIndex > 2 -> {AccIndex+1,[util:tryString2int(ValueStr)|AccValues]};
			?TRUE -> {AccIndex+1,AccValues}
		end
		end,
	{_,Values}=lists:foldl(Fun,{1,[]},Json),
	lists:reverse(Values).

spawn_out(Module,Function,Req) ->
	Json = mochiweb_request:parse_qs(Req),
	FuncName = proplists:get_value("funcname", Json),
	spawn(Module,Function,[Req,FuncName,getValueListFromJson(Json)]).

printJson(Json) ->
	Func = fun({KeyStr,ValueStr},AccStr) ->
		AccStr++"{"++KeyStr++","++ValueStr++"}\r\n"
		end,
	Str=lists:foldl(Func,"",Json),
	catch(?INFO("from client client_data data=~s",[Str])).