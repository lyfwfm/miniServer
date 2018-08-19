%% gen_server:日志定时写文件 
-module(logger_dump).


%% 有一个优化项是：收到｛event，E}时，receive_all(),然后timer:sleep(1000).
%% 为什么没有做这个？因为错误日志，在大部分时候应该是很小的。

-behaviour(gen_server).
-include("common.hrl").
%% API
-export([
         start/1, 
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {fd}).


start(Supervisor) ->
    {ok, _} = supervisor:start_child(Supervisor, {?MODULE, {?MODULE, start_link, []},
                                                 transient, brutal_kill, worker, [?MODULE]}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
init([]) ->
    File = make_log_file(),
    trunk_at_next_day(),
    {ok, #state{fd=File}}.


%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------

handle_info({event, Event}, State) ->
    write_event(State#state.fd, {erlang:localtime(), Event}),
    {noreply, State};
handle_info(trunk_file, State) ->
    File = make_log_file(),
    trunk_at_next_day(),
    {noreply, State#state{fd=File}};
handle_info(Info, State) ->
    ?ERR("~ts:~w", ["未知的消息", Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


log_time() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:localtime(), 
    io_lib:format("~w/~w/~w ~w:~w:~w",
		  [Mo, D, Y, H, Mi, S]).

do_write(Fd, Time, Type, Format, Args) ->
    {{Y,Mo,D},{H,Mi,S}} = Time, 
    Time2 = io_lib:format("== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
		  [Y, Mo, D, H, Mi, S]),
    write_file(Fd, [Type, "\n", Time2]),
    try 
        M = io_lib:format(Format, Args),
        write_file(Fd, [M, "</FONT></div>"])
    catch _:Error ->
            ?ERR("log error ~p ~p ~p", [Error, Format, Args])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% Copied from erlang_logger_file_h.erl
write_event(Fd, {Time, {error, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#FF0000\">", Format, Args);

write_event(Fd, {Time, {warning, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#FFCC33\">", Format, Args);

write_event(Fd, {Time, {debug, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#00FFD2\">", Format, Args);

write_event(Fd, {Time, {info_msg, _GL, {_Pid, Format, Args}}}) ->
    do_write(Fd, Time, "<div><font size=\"2\" color=\"#00FF00\">", Format, Args);

write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(Time),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    write_file(Fd, [T, S]);
	_ ->
	    write_file(Fd, [T, io_lib:format("ERROR: ~p ~n", [Chars])])
    end;


write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(Time),
    write_file(Fd, [T , io_lib:format(add_node("~p~n",Pid), [Info])]);


write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(Time),
    S = format_report(Rep),
    write_file(Fd, [T , S ++ add_node("", Pid)]);


write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(Time, "INFO REPORT"),
    S = format_report(Rep),
    write_file(Fd, [T , S , add_node("", Pid) ]);





write_event(_, _) ->
    ok.

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
	true ->
	    io_lib:format("~s~n",[Rep]);
	_ ->
	    format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);


format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);


format_rep(_) ->
    [].

add_node(X, Pid) when node(Pid) /= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").

write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n=~s==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===",
		  [Type, Y, Mo, D, H, Mi, S]).


%%生成日志文件名
make_log_file() ->
    {{Year, Month, Day}, {_Hour, _, _}} = erlang:localtime(),
    filename:join([?LOG_FILE_DIR, io_lib:format("error_log_~p_~p_~p.html", [Year, Month, Day]) ]).

%%通知服务器在下一个整点刷新日志文件
trunk_at_next_day() ->
    {_, {H, M, S}} = erlang:localtime(),
    Time = ((23 - H) * 3600 + (59 - M) * 60 + (59 - S) + 2) * 1000,
    erlang:send_after(Time, self(), trunk_file).            

%% 封装write_file的参数
write_file(Fd, IoList) ->
	check_head(Fd),
	file:write_file(Fd, IoList, [raw,append, delayed_write]).

check_head(File) ->
	case filelib:file_size(File) of
		0 ->
			write_head(File);
		_ ->
			ignore
	end.

write_head(File) ->
	Head = io_lib:format("<BODY bgcolor=\"#000000\">\n<pre>
<font color=\"#00FF00\">
<style type=\"text/css\">
	div {text-align:left;margin-left:4px;word-wrap:break-word;}
</style>
<CENTER><meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" /> <TITLE>Log</TITLE> <H3><font color=\"#FFFF97\">Log for ~s</font><br><br></CENTER>
<hr>",[log_time()]),
	file:write_file(File, Head, [raw, append
%, delayed_write
]).
