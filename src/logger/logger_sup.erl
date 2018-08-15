%%% -------------------------------------------------------------------
%%% Author  : caohongyang
%%% Description :
%%%
%%% Created : 2012-7-13
%%% -------------------------------------------------------------------
-module(logger_sup).

-behaviour(supervisor).

-export([]).

-export([
	start_link/0,
	 init/1
        ]).


-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = {logger_dump,{logger_dump,start_link,[]},
	      permanent,2000,worker,[logger_dump]},
    {ok,{{one_for_one,0,1}, [AChild]}}.


