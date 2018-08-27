%%%-------------------------------------------------------------------
%%% @author chenlong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 八月 2018 18:30
%%%-------------------------------------------------------------------
-module(user_default).
-author("chenlong").

%% API
-export([start/0,run/0,startAndRun/0]).

start() ->
	tk:start().

run() ->
	tk:run().

startAndRun() ->
	start(),
	timer:sleep(5000),
	run().