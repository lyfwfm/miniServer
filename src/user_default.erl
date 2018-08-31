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
-export([cl/0]).

start() ->
	tk:start().

run() ->
	tk:run().

startAndRun() ->
	start(),
	timer:sleep(5000),
	run().

%%清除log文件夹的所有日志
cl() ->
	filelib:fold_files("log",".+\.log",false,fun(FileName,_Acc) -> file:delete(FileName) end,ok).