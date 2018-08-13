-module(tk).

-export([start/0,run/0]).

start()->
	StartFunc = fun(App) ->
		R = application:start(App),
		case R of
			ok -> ok;
			{error,{already_started,_}} -> ok;
			_ -> io:format("~p start ~p~n",[App,R])
		end
		end,
	lists:foreach(StartFunc,[kernel,stdlib,crypto,inets,asn1,public_key,ssl,compiler,xmerl,syntax_tools,
	mochiweb,mysql]).
	
run() ->
	websocket:start().