%% application: logger
-module(logger_app).

-behaviour(application).

-export([
	 start/0,
	 start/2,
	 stop/1
        ]).

-export([]).

-include("common.hrl").

start() ->
	application:start(server_logger).

start(_, _) ->
	ErrorLogHandlers = gen_event:which_handlers(error_logger),
	lists:foreach(fun(H) ->gen_event:delete_handler(error_logger, H, []) end, ErrorLogHandlers),
	gen_event:add_handler(error_logger,logger_handler, []),
	logger_gen:set(?LOG_LEVEL),
	{ok, SupPid} = logger_sup:start_link(),
	{ok, SupPid}.
	
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

