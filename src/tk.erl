-module(tk).

-export([start/0,run/0]).

-export([web_loop/1,testJson/0]).
%%-compile({parse_transform, ejson_trans}).
-record(person, {name, birth_year, projects}).
-record(project, {name="中午", budget=99, successful=true}).

-json({person, {string, "name"}, {number, "yearOfBirth"},
	{list, "projects",[{type,project}]}}).
-json({project, {string, "name"}, {number, "budget"},
	{boolean, "isSuccessful"}}).

start()->
	StartFunc = fun(App) ->
		R = application:start(App),
		case R of
			ok -> ok;
			{error,{already_started,_}} -> ok;
			_ -> io:format("~p start ~p~n",[App,R])
		end
		end,
	lists:foreach(StartFunc,[
		kernel,stdlib,crypto,inets,asn1,public_key,ssl,compiler,xmerl,syntax_tools,jsx,
	mochiweb,ejson]).
	
run() ->
	Loopfun = fun(Req) -> ?MODULE:web_loop(Req) end,
	mochiweb_http:start([{loop,Loopfun}]).

web_loop(Req) ->
%%	io:format("Req = ~p~n",[Req]),
 "/" ++ Path = Req:get(path),
	case Req:get(method) of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case Path of
				_ ->
					Json = Req:parse_qs(),
					io:format("Json=~p~n",[Json]),
					Jim = #person{
						name = "Jim",
						birth_year = 1967,
						projects = [#project{}]},
					{ok, Json2} = to_json(Jim),
					Req:ok({"text/html",Json2})
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
	io:format("~s~n", [Json]),
	{ok, Person} = from_json(Json, person),
	io:format("Person=~p~n",[Person]).  %% Specify the rule name here