-module(tk).

-export([start/0,stop/0, run/0, startAndRun/0]).

-export([web_loop/1, testJson/0]).
%%-compile({parse_transform, ejson_trans}).
-record(person, {name, birth_year, projects}).
-record(project, {name = "中午", budget = 99, successful = true}).

-json({person, {string, "name"}, {number, "yearOfBirth"},
	{list, "projects", [{type, project}]}}).
-json({project, {string, "name"}, {number, "budget"},
	{boolean, "isSuccessful"}}).

-include("common.hrl").
-define(APPS,[
	sasl, kernel, stdlib, crypto, inets, asn1, public_key, ssl, compiler, xmerl, syntax_tools, jsx, emysql, db,
	mochiweb, ejson,
	role]).

startAndRun() ->
  start(),
  spawn_link(fun run/0).

start() ->
	StartFunc = fun(App, _Acc) ->
		R = application:start(App),
		case R of
			ok -> ok;
			{error, {already_started, _}} -> ok;
			_ -> ?ERR("~p start ~p~n", [App, R])
		end,
		[]
	            end,
	lists:foldl(StartFunc, [], ?APPS).

stop() ->
	StopFunc = fun(App) ->
		application:stop(App)
		end,
	lists:foreach(StopFunc,[role,mochiweb]).
%%	halt().

run() ->
	Loopfun = fun(Req) -> ?MODULE:web_loop(Req) end,
	Port = 446,
  try
	  mochiweb_http:start([{loop, Loopfun}, {port, Port}])
%%	mochiweb_http:start([{loop, Loopfun}, {port, Port},{ssl,true},
%%		{ssl_opts,[
%%			{certfile,"H5C.pem"},
%%			{keyfile,"H5.pem"}
%%		]}])
catch
    _:Why:Stack->?ERR("Why=~p,Stack=~p",[Why,Stack])
  end.

web_loop(Req) ->
		"/" ++ Path = Req:get(path),
	case Req:get(method) of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case Path of
				_ ->
					web_util:route(Req)
			end;
		'POST' ->
			case Path of
				_ ->
					Req:not_found()
			end;
		_ ->
			Req:respond({501, [], []})
	end.


%% parse_transform generates to_json/1 and from_json/{1,2} local functions

testJson() ->
	Jim = #person{
		name = "Jim",
		birth_year = 1967,
		projects = [#project{}]},
	{ok, Json} = to_json(Jim),
	?ERR("Json=~s~n", [Json]),
	{ok, Person} = from_json(Json, person),
	?ERR("Person=~p~n", [Person]).  %% Specify the rule name here