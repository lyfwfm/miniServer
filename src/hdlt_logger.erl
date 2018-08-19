%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
%% Purpose: This is a simple logger utility for the HDLT toolkit.
%%          It assumesd that the debug level and the "name" of the 
%%          logging entity has been put in process environment
%%          (using the set_level and set_name functions respectively).
%%----------------------------------------------------------------------

%%

-module(hdlt_logger).

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {monitorRef, logCnt}).



-export([
	error_out/2, log/2, debug/2
]).



-define(LOGGER, ?MODULE).
-define(MSG, hdlt_logger_msg).
-define(LEVEL, hdlt_logger_level).
-define(NAME, hdlt_logger_name).
-define(ERROR_STR, "ERROR").
-define(LOG_STR, "LOG ").
-define(DEBUG_STR, "DBG ").
-define(MAX_LOG_CNT_ONE_FILE, 500000).	%%日志文件最大行数
-define(Start_Link_TimeOut_ms, 120000).
-define(LOG_LEVEL, log).
-define(DefaultMsgLevel, 0).
-define(DebugMsgLevel, 1).
-define(ErrorMsgLevel, 2).





start_link(Log_File_Name) ->
	ParentPid = self(),
	gen_server:start_link({local, ?LOGGER}, ?MODULE, [Log_File_Name, ParentPid], [{timeout, ?Start_Link_TimeOut_ms}]).



init([Log_File_Name, ParentPid]) ->
	erlang:system_flag(backtrace_depth,5), %%最大栈回溯层数设置为5层，默认是8层太多
	Ref = initLogger(Log_File_Name, ParentPid),
	{ok, #state{monitorRef = Ref, logCnt = 0}}.


handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% set_level(Level) ->
%%     case lists:member(Level, [silence, error, log, debug]) of
%% 	true ->
%% 	    put(?LEVEL, Level),
%% 	    ok;
%% 	false ->
%% 	    erlang:error({bad_debug_level, Level})
%%     end.


error_out(F, A) ->
	CurLogLevel = ?LOG_LEVEL,
	case CurLogLevel of
		silence -> ok;
		error -> do_log(error, CurLogLevel, F, A);
		log -> do_log(error, CurLogLevel, F, A);
		debug -> do_log(error, CurLogLevel, F, A);
		_ -> do_log(error, debug, F, A)
	end.

log(F, A) ->
	CurLogLevel = ?LOG_LEVEL,
	case CurLogLevel of
		silence -> ok;
		error -> do_log(log, CurLogLevel, F, A);
		log -> do_log(log, CurLogLevel, F, A);
		debug -> do_log(log, CurLogLevel, F, A);
		_ -> do_log(log, debug, F, A)
	end.

debug(F, A) ->
	CurLogLevel = ?LOG_LEVEL,
	case CurLogLevel of
		silence -> ok;
		error -> do_log(debug, CurLogLevel, F, A);
		log -> do_log(debug, CurLogLevel, F, A);
		debug -> do_log(debug, CurLogLevel, F, A);
		_ -> do_log(debug, debug, F, A)
	end.

initLogger(Log_File_Name, Parent) ->
	put(?NAME, Log_File_Name),

	%set_level( debug ),
	Ref = erlang:monitor(process, Parent),
	%%Pid = self(),

%% 	{ Result, FileHandle,FileHandleForError } = make_log_file( Log_File_Name ),
%% 	case Result of
%% 		ok->
%% 			put( 'Log_File_Handle', FileHandle ),
%% 			put( 'Log_File_Handle_For_Err', FileHandleForError );
%% 		error->
%% 			put( 'Log_File_Handle', 0 ),
%% 			put( 'Log_File_Handle_For_Err', 0 )
%% 	end,
	make_log_file(Log_File_Name),
	Ref.


handle_info(Info, #state{monitorRef = Ref, logCnt = Cnt} = StateData) ->
	try
		case Info of
			{?MSG, Level, F, A} ->
				{_MsgLevel, LogFileHandle} = case Level of
												error ->
													{?ErrorMsgLevel, get('Log_File_Handle_For_Err')};
												debug ->
													{?DebugMsgLevel, get('Log_File_Handle')};
												log ->
													{?DefaultMsgLevel, get('Log_File_Handle')}
											end,

				try
					case isWindowsOS() of
						true ->
							io:format(F, A);
						false -> ok
					end,

					if LogFileHandle > 0 ->
						io:format(LogFileHandle, F, A);
						true ->
							ok
					end
				catch
					Throw ->
						io:format(LogFileHandle, "~p", [Throw])
				end,

				Cnt1 = Cnt + 1,
				case Cnt1 > ?MAX_LOG_CNT_ONE_FILE of
					true ->
						Cnt2 = 0,
						make_log_file(get(?NAME));
					false ->
						Cnt2 = Cnt1
				end,
				{noreply, #state{monitorRef = Ref, logCnt = Cnt2}};
			{'DOWN', Ref, process, _Object, _Info} ->
				%% start the stop timer
				erlang:send_after(timer:seconds(5), self(), stop),
				{noreply, StateData};
			stop ->
				{stop, normal, StateData};
			Unkown ->
				io:format("hdlt_looger recv unkown msg:~p ~n", [Unkown])
		end

	catch
		_:_Why:StackTrace ->
			error_out("ExceptionFunc_Module:[~p] ExceptionFunc[hande_info] Why[~p] ~n stack[~p] ~n Info ~p",
				[?MODULE, _Why, StackTrace, Info]),
			{noreply, StateData}
	end.


formated_timestamp() ->
	{Date, Time} = erlang:localtime(),
	{YYYY, MM, DD} = Date,
	{Hour, Min, Sec} = Time,
	FormatDate =
		io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
			[YYYY, MM, DD, Hour, Min, Sec]),
	lists:flatten(FormatDate).



do_log(_, silence, _, _) ->
	ok;
do_log(error, error, F, A) ->
	do_log(error, F, A);
do_log(error, log, F, A) ->
	do_log(error, F, A);
do_log(log, log, F, A) ->
	do_log(log, F, A);
do_log(error, debug, F, A) ->
	do_log(error, F, A);
do_log(log, debug, F, A) ->
	do_log(log, F, A);
do_log(debug, debug, F, A) ->
	do_log(debug, F, A);
do_log(_, _, _F, _A) ->
	ok.

do_log(Level, F, A) ->
	{SEV, A1} = case Level of
					   error ->
						   {?ERROR_STR, A};
					   log ->
						   {?LOG_STR, A};

					   debug ->
						   {?DEBUG_STR, A};
					   _ ->  {"", ""}
				   end,
	Msg ={?MSG, Level, "[~s] [~p] [~p] ->" ++ F ++ "\r\n", [formated_timestamp(), SEV, self() | A1]},
	(catch ?LOGGER ! Msg).

make_log_file(Log_File_Name) ->
	case get('Log_File_Handle') of
		0 -> ok;
		undefined -> ok;
		Handle1 -> file:close(Handle1)
	end,

	case get('Log_File_Handle_For_Err') of
		0 -> ok;
		undefined -> ok;
		Handle2 -> file:close(Handle2)
	end,


	{ok, Cur_Dir} = file:get_cwd(),
	Log_Dir = Cur_Dir ++ "/log",

	case file:read_file_info(Log_Dir) of
		{ok, FileInfo} when FileInfo#file_info.type == directory ->
			io:format("log_dir[~s] exist~n", [Log_Dir]);
		{error, _Reason} ->
			case file:make_dir(Log_Dir) of
				ok ->
					io:format("create log_dir[~s] succ.~n", [Log_Dir]);
				{error, Reason} ->
					io:format("create log_dir[~s] fail[~w].~n", [Log_Dir, Reason])
			end
	end,

	{Date, Time} = erlang:localtime(),
	{YYYY, MM, DD} = Date,
	{Hour, Min, Sec} = Time,
	Time_Format = io_lib:format("~.4w-~.2.0w-~.2.0w_~.2.0w_~.2.0w_~.2.0w.log",
		[YYYY, MM, DD, Hour, Min, Sec]),
	Log_Full_FileName = Log_Dir ++ "/" ++ Log_File_Name ++ "-" ++ lists:flatten(Time_Format),
	Log_Full_FileName_For_Err = Log_Dir ++ "/err-" ++ Log_File_Name ++ "-" ++ lists:flatten(Time_Format),

	io:format("create log file[~s]~n", [Log_Full_FileName]),

	{Result, File_Handle} = file:open(Log_Full_FileName, [append]),
	case Result of
		ok ->
			put('Log_File_Handle', File_Handle),
			io:format("create log_file[~s] succ.~n", [Log_Full_FileName]);
		error ->
			io:format("create log_file[~s] fail[~w].~n", [Log_Full_FileName, File_Handle])
	end,

	{Result1, File_Handle_ForError} = file:open(Log_Full_FileName_For_Err, [append]),
	case Result1 of
		ok ->
			put('Log_File_Handle_For_Err', File_Handle_ForError),
			io:format("create err_log_file[~s] succ.~n", [Log_Full_FileName_For_Err]);
		error ->
			io:format("create err_log_file[~s] fail[~w].~n", [Log_Full_FileName_For_Err, File_Handle_ForError])
	end.

isWindowsOS() ->
	element(1,os:type())=:=win32.