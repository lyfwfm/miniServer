%% 动态生成logger.erl的源代码，并热更新。
-module(logger_gen).
-export([get/0, set/1, set2/2, logger_src/2]).

-include("common.hrl").

-define(LOGMODULE, "logger").

%% Error levels:
-define(LOG_LEVELS,[ {0, no_log, "No log"}
                     ,{1, critical, "Critical"}
                     ,{2, error, "Error"}
                     ,{3, warning, "Warning"}
                     ,{4, info, "Info"}
                     ,{5, debug, "Debug"}
                     ,{6, dev, "Dev"}
                    ]).

get() ->
    Level = logger:get(),
    case lists:keysearch(Level, 1, ?LOG_LEVELS) of
        {value, Result} -> Result;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));

set(Loglevel) when is_integer(Loglevel) ->
    case catch logger:get_opened_flags() of
        Flags when is_list(Flags) ->
            next;
        _ ->
            Flags = []
    end,
    set2(Loglevel, Flags);

set(Flag) when is_tuple(Flag)->
	set([Flag]);
set(Flags) when is_list(Flags) ->
	CurFlags = logger:get_opened_flags(),
	NewFlags = combine_flag(CurFlags, Flags),
	LogLevel = logger:get(),
	set2(LogLevel, NewFlags);

set(_) ->
    exit("Loglevel must be an integer").

combine_flag(TupleList1, TupleList2) ->
	lists:foldl(fun({Tag, ValList}, Acc) ->
					  case lists:keyfind(Tag, 1, TupleList1) of
						  false ->
							  [{Tag, ValList}|Acc];
						  {Tag, ValList2} ->
							  [{Tag, sets:to_list(sets:from_list(ValList++ValList2))} | Acc]
					  end
				end, [], TupleList2).

set2(LogLevel, Flags) ->
    try
        {Mod,Code} = dynamic_compile:from_string(logger_src(LogLevel, Flags)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> 
            Trace = erlang:get_stacktrace(),
            io:format("Error compiling logger (~1000000000p): ~p, StackTrace = ~1000000000p ~n", [Type, Error, Trace])
    end.

level_to_integer(Level) ->
    case lists:keysearch(Level, 2, ?LOG_LEVELS) of
        {value, {Int, Level, _Desc}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the common logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
logger_src(Loglevel, OpenedFlags) ->
    L = integer_to_list(Loglevel),
    StrOfOpenedFlags = lists:flatten(io_lib:format("~w", [OpenedFlags])),
    "-module(logger).

    -export([
            debug_msg/5,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4,
             dev/4,
             get_opened_flags/0,
             get/0]).

    get() -> "++ L ++".
    get_opened_flags() -> " ++ StrOfOpenedFlags ++ ".
    %% Helper functions
    debug_msg(Module, Line, Format, Args, _Flags) when " ++ L ++ " >= 5 ->
            notify(debug,
                   \"D(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    debug_msg(Module, Line, Format, Args, Flags) ->
			Pass =  lists:any(fun({Tag, Value}) -> 
								case lists:keyfind(Tag, 1, " ++ StrOfOpenedFlags ++ ") of
										false ->
											false;
										{_, ValList} ->
											lists:member(Value, ValList)
									end
							end, Flags),
			case Pass of
					true ->
						   notify(debug,
                   						\"D(~p:~p:~p) : \"++Format++\"~n\",
                   						[self(), Module, Line]++Args);
					false ->
							ignore
			end.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(warning,
                   \"W(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            notify(error,
                   \"E(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    error_msg(_,_,_,_) -> ok.


    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    dev(Module, Line, Format, Args) when " ++ L ++ " >= 6 ->
            notify(info_msg,
                   \"DEV(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    dev(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
