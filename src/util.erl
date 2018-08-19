%% @author caohongyang
%% @doc test edoc.
%% created 2013-2-18
-module(util).
-include("common.hrl").

-export([
         pow/1,
		 log/4,
		 now/0,
		 now_mili/0,
		 md5/1,
		 ceil/1,
		 floor/1,
		 sleep/1,
		 sleep/2,
		 get_list/2,
		 implode/2,
		 implode/3,
		 explode/2,
		 explode/3,
		 for/3,
		 for/4,
		 string_to_term/1,
		 bitstring_to_term/1,
		 term_to_string/1,
		 term_to_bitstring/1,
		 to_string/1,
		 null_proc/0,
		 to_atom/1,
		 toSqlDate/1,
		 toUnixTime/1,
		 datetime_to_seconds/1,
		 seconds_to_datetime/1,
		 foldl/3,
		 nth_take/2,
		 fun_take/2,
		 fun_find/2,
		 fun_replace/3,
		 keymax/2,
		 keymin/2,
		 is_duplicate/1,
		 copy_list/2,
		 to_list/1,
		 append_kvList/2,
		 append_kvList/1,
		 nth/2,
		 element_pos/2,
		 random_int/2,
		 random_list/2,
		 random_list2/2,
		 random_list2/1,
		 random_weigh_list/2,
		 random_weigh_list2/2,
		 tc/2,
		 catch_binary_to_term/1,
		 ip_to_str/1,
		 words_exchange/1,
         check_blankName/1,
         calc_name_length/1,
         gen_utf8_decode_list/2,
		 get_total_weight/1,
		 boolean2int/1,
	to_hex/1,
	to_hex2/1,
	safe_nth/2,
	safe_split/2,
	random_one_from_weigh_list/1,
	random_one_from_list/1,
	random_list_quick/1,
	random_list/1,
	nth_replace/3,
	upmap/2,
	dateTimeIntervel_to_second/1,
	listSelect/2
		]).

-export([sqlDayToDate/1
		,dateToSqlDay/1
		,int_format_two/1
		 ]).

-export([
		 read_timer/1
		]).

-export([
		 ets_foreach_key/2
		,ets_all_key/1
		 ]).

-export([
		 count_src_line_num/1,
		 gen_random_seed/0,
		 latin1/1,
		 is_name_en/1
		 ]).

-export([
	getProcDictDataProperty/3,
	updateProcDictDataProperty/3,
	updateProcDictDataProperty/2,
	getTernaryValue/3,
	getTernaryValueEx/3,
	formap/3,
	tupleAdd/3,
	getValueInSectionList/3,
	make_proto/1,
	tryString2int/1
]).


-define(TIME_ZONE, {{1970,1,1}, {8,0,0}}).
-define(GREGORIAN_INTERVIAL_TIME,  (calendar:datetime_to_gregorian_seconds(?TIME_ZONE))).


%% 2的N次方
pow(N) ->
    pow(N,1).
pow(0,R) ->
    R;
pow(N,R)when N > 0 ->
    pow(N-1,2*R).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
	implode(S, L, []).
implode(_S, [H], NList) ->
	lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
	L = [thing_to_list(H) | NList],
	implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
	re:split(B, S, [{return, list}]).
explode(S, B, int) ->
	[list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 日志记录函数
log(F, A, Mod, Line) ->
     {ok, Fl} = file:open("log/error_log.txt", [write, append]),
     Format = list_to_binary("~s[~w:~w] " ++ F ++ "\r\n~n"),
     {{Y, M, D},{H, I, S}} = erlang:localtime(),
     Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
     io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
     file:close(Fl).

%% @doc 取得当前的unix时间戳,单位：秒
now() ->
	{M,S,_} = os:timestamp(),
	M * 1000000 + S.

%% @doc 当前时间戳，单位：毫秒
now_mili() ->
	{M, S, Ms} = os:timestamp(),
	M * 1000000000 + S*1000 + Ms div 1000.

%% 转换成HEX格式的md5
md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 将binary打包成SQL语句中的HEX格式
to_hex(Binary) ->
	<< <<(hd(integer_to_list(E,16))):8>> || <<E:4>> <= Binary>>. 


to_hex2(Binary) ->
	<< <<(integer_to_binary(E,16))/binary>> || <<E:4>> <= Binary>>.

%%向上取整
ceil(N) ->
	T = trunc(N),
	case N =< T of
		true  -> T;
		false -> 1 + T
	end.

%%向下取整
floor(X) ->
	T = trunc(X),
	case (X < T) of
		true -> T - 1;
		_ -> T
	end.

sleep(T) ->
	receive
		after T -> ok
	end.

sleep(T, F) ->
	receive
		after T -> F()
	end.

get_list([], _) ->
	[];
get_list(X, F) ->
	F(X).

%% for循环
for(Max, Max, F) ->
	F(Max);
for(I, Max, F)  when I =< Max ->
	F(I),
	for(I+1, Max, F);
for(_I, _Max, _F) ->
	nil.

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
	binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
	erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
	case erl_scan:string(String++".") of
		{ok, Tokens, _} ->
			case erl_parse:parse_term(Tokens) of
				{ok, Term} -> Term;
				_Err -> undefined
			end;
		_Error ->
			undefined
	end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
	string_to_term(binary_to_list(BitString)).

to_string(Term) when is_atom(Term) ->
	atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
	integer_to_list(Term);
to_string(Term) when is_list(Term) ->
	Term.

null_loop() ->
	receive 
		_ ->
			null_loop()
	end.

null_proc() ->
	spawn(fun null_loop/0).

to_atom(A) when is_list(A) ->
	list_to_atom(A);
to_atom(A) when is_integer(A) ->
	list_to_atom(integer_to_list(A));
to_atom(A) when is_atom(A) ->
	A;
to_atom(A) ->
	[List] = io_lib:format("~w", [A]),
	list_to_atom(List).

datetime_to_seconds({_Date,_Time}=Datetime)->
    calendar:datetime_to_gregorian_seconds(Datetime)
        - ?GREGORIAN_INTERVIAL_TIME.

seconds_to_datetime(MTime)->
    calendar:gregorian_seconds_to_datetime( 
      ?GREGORIAN_INTERVIAL_TIME+ MTime).

toSqlDate({A,B,_C}) when A < 1900->
	UnixTime = A *1000000 +B,
	toSqlDate(UnixTime);
toSqlDate({{A,B,C},{D,E,F}}) ->
	A*10000000000 + B*100000000 + C*1000000 + D*10000 + E*100 + F;
toSqlDate(UnixTime) when is_integer(UnixTime)->
	{{A,B,C},{D,E,F}} = seconds_to_datetime(UnixTime),
	A*10000000000 + B*100000000 + C*1000000 + D*10000 + E*100 + F.

int_format_two(A) ->
	lists:flatten(io_lib:format("~2..0w",[A])).

dateToSqlDay({A,B,C}) ->
	integer_to_list(A)++"-" ++ int_format_two(B) ++ "-" ++ int_format_two(C).

sqlDayToDate(String) ->
	case string:tokens(String, "-") of
		[A,B,C] ->
			{list_to_integer(A), list_to_integer(B), list_to_integer(C)};
		_ ->
			{0,0,0}
	end.

toUnixTime({A,B,_C}) ->
	A*1000000 + B;
toUnixTime({datetime, Time}) ->
	datetime_to_seconds(Time);
toUnixTime(SqlDate) ->
	A = SqlDate div 10000000000,
	Rest = SqlDate rem 10000000000,
	B = Rest div 100000000,
	Rest2 = Rest rem 100000000,
	C = Rest2 div 1000000,
	Rest3 = Rest2 rem 1000000,
	D = Rest3 div 10000,
	Rest4 = Rest3 rem 10000,
	E = Rest4 div 100,
	F = Rest rem 100,
	datetime_to_seconds({{A,B,C},{D,E,F}}).

		   
foldl(_F, {return,Acc}, _L) ->
    Acc;
foldl(F, Acc, [Tail|L]) ->
    foldl(F,F(Tail,Acc), L);
foldl(F, Acc, []) when is_function(F,2)->
    Acc.

%%-define(seg_size, 16).
%%-define(max_seg, 32).
%%-define(expand_load, 5).
%%-define(contract_load, 3).
%%-define(exp_size, (?seg_size * ?expand_load)).
%%-define(con_size, (?seg_size * ?contract_load)).
%%-record(dict,
%%	{size=0		      :: non_neg_integer(),   	% Number of elements
%%	 n=?seg_size	      :: non_neg_integer(),   	% Number of active slots
%%	 maxn=?seg_size	      :: non_neg_integer(),	% Maximum slots
%%	 bso=?seg_size div 2  :: non_neg_integer(),   	% Buddy slot offset
%%	 exp_size=?exp_size   :: non_neg_integer(),   	% Size to expand at
%%	 con_size=?con_size   :: non_neg_integer(),   	% Size to contract at
%%	 empty		      :: tuple(),		% Empty segment
%%	 segs		      :: tuple()	      	% Segments
%%	}).
%%-define(kv(K,V), [K|V]).			% Key-Value pair format
%%fold_dict(F, Acc, D) ->
%%    Segs = D#dict.segs,
%%    fold_segs(F, Acc, Segs, tuple_size(Segs)).

%%fold_segs(_, {return, Acc}, _, _) -> Acc;
%%fold_segs(F, Acc, Segs, I) when I >= 1 ->
%%    Seg = element(I, Segs),
%%    fold_segs(F, fold_seg(F, Acc, Seg, tuple_size(Seg)), Segs, I-1);
%%fold_segs(F, Acc, _, 0) when is_function(F, 3) -> Acc.
%%
%%fold_seg(_, {return, _}=Acc, _, _) -> Acc;
%%fold_seg(F, Acc, Seg, I) when I >= 1 ->
%%    fold_seg(F, fold_bucket(F, Acc, element(I, Seg)), Seg, I-1);
%%fold_seg(F, Acc, _, 0) when is_function(F, 3) -> Acc.
%%
%%fold_bucket(_, {return, _}=Acc, _) -> Acc;
%%fold_bucket(F, Acc, [?kv(Key,Val)|Bkt]) ->
%%    fold_bucket(F, F(Key, Val, Acc), Bkt);
%%fold_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

to_list(A) when is_binary(A) ->
	binary_to_list(A);
to_list(A) when is_integer(A) ->
    integer_to_list(A);
to_list(A) when is_atom(A) ->
	atom_to_list(A);
to_list(A) when is_list(A) ->
	A.

append_kvList(List) ->
	lists:foldl(fun({K,V}, Acc) ->
						case lists:keytake(K, 1, Acc) of
							false ->
								[{K,V}|Acc];
							{value, {K,V1}, Acc1} ->
								[{K,V+V1}|Acc1]
						end
				end, [], List).

append_kvList(List1,List2) when length(List1) >= length(List2) ->
	append_kvList2(List2, List1);
append_kvList(List1, List2) ->
	append_kvList2(List1, List2).

append_kvList2([], List) ->
	List;
append_kvList2([KV | Rest], List) ->
	append_kvList2(Rest, insert_kv(KV, List)).

insert_kv({Key, Value}, List) ->
	case lists:keytake(Key, 1, List) of
		{value, {Key, OldValue}, List2} ->
			[{Key, OldValue+Value} | List2];
		false ->
			[{Key, Value} | List]
	end.

%% 超过范围返回最后一个，非列表时直接返回
nth(_N, [H]) ->H;
nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T);
nth(_N, H) ->
	H.

%% 超过范围时,返回空
safe_nth(_, []) ->
    null;
safe_nth(1, [H|_]) -> H;
safe_nth(N, [_|T]) ->
    case N > 1 of
        true ->
            safe_nth(N - 1, T);
        _ ->
            null
    end.

%% 安全分割
safe_split(N, List) ->
    safe_split_1(N, List, []).

safe_split_1(_, [], Acc) ->
    {Acc, []};
safe_split_1(1, [H|T], Acc) ->
    {[H|Acc], T};
safe_split_1(N, [H|T], Acc) ->
    safe_split_1(N - 1, T, [H|Acc]).

%% 获得权值列表总权重
get_total_weight( List ) ->
	get_total_weigh( List, 0 ).

%% 从一个带全中的列表中随机选取SelectNum个元素，可以相同
random_weigh_list2(List, SelectNum) ->
	TotalWeigh = get_total_weigh(List,0),
	random_weigh_list2(List, SelectNum, TotalWeigh, []).


random_weigh_list2(_List, 0, _, Result) ->
	Result;
random_weigh_list2(List, SelectNum, TotalWeigh, Result) ->
	Random = rand:uniform() * TotalWeigh,
	{Weigh, _} = Element = 
	foldl(fun({UnitWeigh, _}=Ele, Acc) ->
						if UnitWeigh + Acc > Random ->
							   {return, Ele};
						   true ->
							   Acc + UnitWeigh
						end
		  end, 0, List),
	random_weigh_list2(List, SelectNum -1, TotalWeigh-Weigh, [Element| Result]).


%% 从一个带权重的列表List中随机选取SelectNum个不同元素，结果列表顺序随机
%% weighed_list:  [{weigh1, value1}, {weigh2, value2}]
random_weigh_list(List, SelectNum) ->
	Len = length(List),
	TotalWeigh = get_total_weigh(List,0),
	if Len =< SelectNum ->
		   List;
	   true ->
		   random_weigh_list(List, SelectNum, TotalWeigh, [])
	end.


random_weigh_list(_List, 0, _, Result) ->
	Result;
random_weigh_list(List, SelectNum, TotalWeigh, Result) ->
	Random = rand:uniform() * TotalWeigh,
	{Weigh, _} = Element = 
	foldl(fun({UnitWeigh, _}=Ele, Acc) ->
						if UnitWeigh + Acc > Random ->
							   {return, Ele};
						   true ->
							   Acc + UnitWeigh
						end
		  end, 0, List),
	NewList = lists:delete(Element, List),
	random_weigh_list(NewList, SelectNum -1, TotalWeigh-Weigh, [Element| Result]).

random_one_from_weigh_list(List) ->
	TotalWeigh = get_total_weigh(List,0),
	Random = rand:uniform() * TotalWeigh,
	foldl(fun({UnitWeigh, E}, Acc) ->
						if UnitWeigh + Acc > Random ->
							   {return, E};
						   true ->
							   Acc + UnitWeigh
						end
		  end, 0, List).
	
random_one_from_list(List) ->
	Value = rand:uniform(length(List)),
	lists:nth(Value,List).

get_total_weigh([], Weigh) ->
	Weigh;
get_total_weigh([{WeighUnit, _} | Rest],Weigh) ->
	get_total_weigh(Rest, Weigh + WeighUnit).


random_list_quick(List) ->
    [Elem||{_,Elem}<-lists:keysort(1, [{rand:uniform(), Elem}||Elem<-List])].
%% 将List的中元素随机排列
random_list(List) ->
    random_list(List, erlang:length(List), []).

random_list([], 0, RandomList) ->
    RandomList;
random_list(List, Len, AccRandomList) ->
    Elem = lists:nth(rand:uniform(Len), List),
    random_list(lists:delete(Elem, List), Len - 1, [Elem|AccRandomList]).
	
	
%% 从列表List中随机选取SelectNum个元素，组成新的列表，新列表的元素排列顺序与其在List中顺序相同
random_list(List, SelectNum) ->
	Len = length(List),
	if Len =< SelectNum ->
		   List;
	   true ->
		   random_list(List, SelectNum, Len, [])
	end.


random_list(_, 0, _, Result) ->
	lists:reverse(Result);
random_list([Head| Rest], SelectNum, Len, Result) ->
	case rand:uniform() =< SelectNum / Len of
		true ->
			random_list(Rest, SelectNum-1, Len-1, [Head|Result]);
		false ->
			random_list(Rest, SelectNum, Len-1, Result)
	end.
	
%% 将一个列表元素随机一遍
random_list2(List) ->
	Len = length(List),
	random_list2(List, Len, Len, []).
%% 从一个列表中随机抽取N个，顺序随机 ，N可以超过界限
random_list2(List, N) ->
	random_list2(List, N, length(List),[]).

random_list2(_List, 0, _Length, Result) ->
	Result;
random_list2(List, N, Length, Result) ->
	if Length =:= 1 ->
		   Select = hd(List),
		   Rest = [],
		   random_list2(Rest, N-1, Length-1, [Select|Result]);
	   Length =:= 0 ->
		   Result;
	   true ->
		   
		   Rand = rand:uniform(Length),
		   {value, Select, Rest} = nth_take(Rand, List),		   
		   random_list2(Rest, N-1, Length-1, [Select|Result])
	end.

%% 计算一个元素在一个列表或tuple中第一次出现的位置
%% 元素在集合中不存在时，返回0
element_pos(Element, List) when is_list(List) ->
	element_pos(Element, List, 1);
element_pos(Element, Tuple) when is_tuple(Tuple) ->
	element_pos(Element, tuple_to_list(Tuple), 1);
element_pos(_, _) ->
	0.

element_pos(Element, [Element|_Rest], Index) ->
	Index;
element_pos(Element, [_|Rest], Index) ->
	element_pos(Element, Rest, Index+1);
element_pos(_, [], _) ->
	0.

%% 从[Lower...Higher]包括边界的整数区间中随机一个数
random_int(Lower, Higher) when Lower =< Higher ->
	rand:uniform(Higher -Lower+1) +Lower-1;
random_int(Higher, Lower) ->
	random_int(Lower, Higher).

fun_take(F,L) ->
    fun_take(F, L, []).

fun_take(F, [H|T], L)  ->
	case F(H) of
		true ->
    		{value, H, lists:reverse(L, T)};
		false ->
    		fun_take(F, T, [H|L])
	end;
fun_take(_F, [], _L) -> false.

fun_find(_F,[]) ->
	false;
fun_find(F,[E|Rest]) ->
	case F(E) of
		true ->
			E;
		_ ->
			fun_find(F,Rest)
	end.


fun_replace(F, [Tup|Tail], New) ->
	case F(Tup) of
		true ->
    		[New|Tail];
		_ ->
			[Tup|fun_replace(F, Tail, New)]
	end;
fun_replace(_F, [], _) -> [].

%% 替换第n个
nth_replace(_, [], Info, Head) ->
    lists:reverse(Head) ++ [Info];
nth_replace(1, [_|Rest], Info, Head) ->
    lists:reverse(Head) ++ [Info|Rest];
nth_replace(N, [H|T], Info, Head) ->
    nth_replace(N - 1, T, Info, [H|Head]).

nth_replace(_, [], Info) ->
    [Info];
nth_replace(1, [_|T], Info) ->
    [Info|T];
nth_replace(N, List, Info) ->
    case N < 1 of
        true ->
            List;
        _ ->
            nth_replace(N, List, Info, []) 
    end.

%% 读timer
read_timer(Timer) when is_reference(Timer)->
	case erlang:read_timer(Timer) of
		false ->
			0;
		A ->
			A
	end;
read_timer(_) ->
	0.

%% 判断列表中是否有重复项
is_duplicate(List) ->
	is_duplicate(List, []).

is_duplicate([], _) ->
	false;
is_duplicate([H|T], List) ->
	case lists:member(H, List) of
		true ->
			true;
		false ->
			is_duplicate(T, [H|List])
	end.

%% 将列表复制N分，并组合成一个新的列表
copy_list(List,N) ->
	copy_list(List,N,[]).

copy_list(_List, 0, Result) ->
	Result;
copy_list(List, N, Result) ->
	copy_list(List, N-1, List++Result).

%% 找到tupleList中某字段最大的tuple
%% return: {MaxKey, Tuple}
keymax(List, Pos) ->
	[Head|Tail] = List,
	HeadKey = element(Pos, Head),
	lists:foldl(fun(E, {MaxKey, Tuple}) ->
						Key = element(Pos, E),
						if Key > MaxKey ->
							   {Key, E};
						   true ->
							   {MaxKey, Tuple}
						end
				end, {HeadKey, Head}, Tail).

%% 找到tupleList中某字段最大的tuple
%% return: {MinKey, Tuple}
keymin(List, Pos) ->
	[Head|Tail] = List,
	HeadKey = element(Pos, Head),
	lists:foldl(fun(E, {MaxKey, Tuple}) ->
						Key = element(Pos, E),
						if Key < MaxKey ->
							   {Key, E};
						   true ->
							   {MaxKey, Tuple}
						end
				end, {HeadKey, Head}, Tail).

%% 删除第N个，并返回新列表
%% return: {value, NthVar, NewList} | false
nth_take(N, List) ->
	nth_take(N, List, []).
nth_take(1, [NthVar|Tail], Temp) ->
	{value, NthVar, lists:reverse(Temp, Tail)};
nth_take(_N, [], _Temp) ->
	false;
nth_take(N, [Hd | Tail], Temp) ->
	nth_take(N-1, Tail, [Hd|Temp]).

%% 测试
tc(F, N) ->
    Time1 = os:timestamp(),
    do_times(N, F),
    Time2 = os:timestamp(),
    MicroDiffPerTime = timer:now_diff(Time2, Time1) / N,
    io:format("Times :~w ,  Each time consume: ~w us\n", [N, MicroDiffPerTime]).

do_times(N,F) when N =< 0 ->
    F();
do_times(N,F) ->
    F(),
    do_times(N -1, F).

%% 遍历ets的所有key
ets_foreach_key(Fun, Table) ->
    ets:safe_fixtable(Table, true),
    First = ets:first(Table),
    try
        do_ets_foreach_key(Fun, First, Table)
    after
	ets:safe_fixtable(Table, false)
    end.

do_ets_foreach_key(F, Key, Table) ->
    case Key of
	'$end_of_table' ->
	    ok;
	_ ->
	    F(Key),
		do_ets_foreach_key(F, ets:next(Table, Key), Table)
	end.
%% 获取ets所有key
ets_all_key(Table) ->
    ets:safe_fixtable(Table, true),
    First = ets:first(Table),
    try
        do_ets_all_key(First, Table, [])
    after
	ets:safe_fixtable(Table, false)
    end.

do_ets_all_key(Key, Table, Result) ->
    case Key of
	'$end_of_table' ->
	    Result;
	_ ->
		do_ets_all_key(ets:next(Table, Key), Table, [Key|Result])
	end.

catch_binary_to_term(Binary) ->
	if is_binary(Binary) ->
		   case catch binary_to_term(Binary) of
			   {'EXIT', _Reason} ->
				   [];
			   Term ->
				   Term
		   end;
	   true ->
		   ?ERR("wrong use of function util:catch_binary_to_term,when arg is not binary..~w",[Binary]),
		   []
	end.
	
%% 计算源码行数
count_src_line_num(Dir) ->
	count_src_line_num(Dir, [".+data_.+\.erl"]).
count_src_line_num(Dir, DisCardRegularExpList) ->
	count_src_line_num(Dir, DisCardRegularExpList, ".+\.erl$").
count_src_line_num(Dir, DisCardRegularExpList, MatchExp) ->
	{TotalNum, Info} = 
	filelib:fold_files(Dir, MatchExp, true, fun(FileName, {LineAcc, InfoList}) ->
													case lists:any(fun(E) ->
																		   case re:run(FileName, E) of
																			   {match, _} ->
																				   true;
																			   _ ->
																				   false
																		   end
																   end, DisCardRegularExpList) of
														true ->
															{LineAcc, InfoList};
														false ->
															ThisFileLineNum = cacl_file_line(FileName),
															%io:format("~1000p ------ ~w\n",[filename:basename(FileName), ThisFileLineNum]),
															{LineAcc + ThisFileLineNum, [{ThisFileLineNum, FileName} |InfoList]}
													end
					   end, {0, []}),
	Info2 = lists:keysort(1, Info),
	lists:foreach(fun({Lines, Name}) ->
															io:format("~1000p ------ ~w\n",[filename:basename(Name), Lines])
				  end, Info2),
	io:format("TotalNum=~w\n",[TotalNum]).
						  

cacl_file_line(FileName) ->
	{ok, Bin} = file:read_file(FileName),
	cacl_file_line2(binary_to_list(Bin),0, false).
cacl_file_line2([], Acc, true) ->
	Acc+1;
cacl_file_line2([], Acc, false) ->
	Acc;
cacl_file_line2([$\n|Rest], Acc, true) ->
	cacl_file_line2(Rest, Acc+1, false);
cacl_file_line2([$\n|Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_E|Rest], Acc, true) ->
	cacl_file_line2(Rest, Acc, true);
cacl_file_line2([$\r|Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, false);
cacl_file_line2([$\ |Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_|Rest], Acc, false) ->
	cacl_file_line2(Rest, Acc, true).

%% @doc convert IP(tuple) to string()
ip_to_str(IP) ->
    case IP of
        {A, B, C, D} ->
            lists:concat([A, ".", B, ".", C, ".", D]);
        {A, B, C, D, E, F, G, H} ->
            lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
        Str when is_list(Str) ->
            Str;
        _ ->
            []
    end.

%% @doc 简单的mapreduce,仅供脚本使用
upmap(F, L) ->
	Parent = self(),
	Ref = make_ref(),
	[receive
		 {Ref,  Result} ->
			 Result 
	 end || _ <- [spawn(fun() -> Parent ! {Ref, F(X)} end)	
		 	|| X<-L]]. 


gen_random_seed() ->
	os:timestamp().

latin1(Name) ->
	Name2 = 
	if is_binary(Name) ->
		   binary_to_list(Name);
	   true ->
		   Name
	end,
	Name3 = 
		case is_name_en(Name2) of
			true ->
				Name2;
			false ->
				unicode:characters_to_binary(Name2)
		end,
	Name3.

is_name_en(Name)->
	lists:all(fun(E)->
					  if E =< 255->
							 true;
						 true ->
							 false 
					  end
			  end, Name).
words_exchange(Words_for_exchange) ->
	QQExchange = [{"390815602","315549698"}],
	binary:bin_to_list(lists:foldl(fun({Kword,S2}, Words_for_filter0)->
										   re:replace(Words_for_filter0,Kword,S2,[global,caseless,{return, binary}])
								   end,
								   Words_for_exchange,QQExchange)).

%% 输入的list为已经解码的utf8 list，检查字符串，只能为数字、大小写英文字母和汉字
check_blankName([]) ->
    true;
check_blankName([H|T]) ->
    case (H >=48 andalso H =< 57) orelse (H >=65 andalso H =< 90) orelse (H >=97 andalso H =< 122) orelse (H >= 16#4e00 andalso H =< 16#9fa5) of
        false ->
            false;
        true ->
            check_blankName(T)
    end.

%% 输入的list为已经解码的utf8 list，计算长度，汉字长度为2
calc_name_length(DecodeList) ->
    lists:foldr(fun(Val, Acc) ->
                    if
                        Val >= 16#10000 ->
                            Acc + 4;
                        Val >= 16#800 ->
                            Acc + 3;
                        Val >= 16#80 ->
                            Acc + 2;
                        true ->
                            Acc + 1
                    end
                end, 0, DecodeList).


%% 将utf8编码的二进制数据解码成字符编码list
gen_utf8_decode_list(<<>>, AccDecodeList) ->
    lists:reverse(AccDecodeList);
gen_utf8_decode_list(<<0:1,X1:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1,
                       Left/binary>>, AccDecodeList) ->
    Val = get_val([X1, X2, X3, X4, X5, X6, X7]),
    gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,X5:1,
                       1:1,0:1,X6:1,X7:1,X8:1,X9:1,X10:1,X11:1,
                       Left/binary>>, AccDecodeList) ->
    Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]),
    gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,
                       1:1,0:1,X5:1,X6:1,X7:1,X8:1,X9:1,X10:1,
                       1:1,0:1,X11:1,X12:1,X13:1,X14:1,X15:1,X16:1,
                       Left/binary>>, AccDecodeList) ->
    Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
    gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1, 
                       1:1,0:1,X4:1,X5:1,X6:1,X7:1,X8:1,X9:1, 
                       1:1,0:1,X10:1,X11:1,X12:1,X13:1,X14:1,X15:1, 
                       1:1,0:1,X16:1,X17:1,X18:1,X19:1,X20:1,X21:1, 
                       Left/binary>>, AccDecodeList) ->
    Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21]),
    gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,0:1,X1:1,X2:1, 
                       1:1,0:1,X3:1,X4:1,X5:1,X6:1,X7:1,X8:1, 
                       1:1,0:1,X9:1,X10:1,X11:1,X12:1,X13:1,X14:1, 
                       1:1,0:1,X15:1,X16:1,X17:1,X18:1,X19:1,X20:1, 
                       1:1,0:1,X21:1,X22:1,X23:1,X24:1,X25:1,X26:1,
                       Left/binary>>, AccDecodeList) ->
    Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26]),
    gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,1:1,0:1,X1:1, 
                       1:1,0:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1, 
                       1:1,0:1,X8:1,X9:1,X10:1,X11:1,X12:1,X13:1, 
                       1:1,0:1,X14:1,X15:1,X16:1,X17:1,X18:1,X19:1, 
                       1:1,0:1,X20:1,X21:1,X22:1,X23:1,X24:1,X25:1, 
                       1:1,0:1,X26:1,X27:1,X28:1,X29:1,X30:1,X31:1,
                       Left/binary>>, AccDecodeList) ->
    Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31]),
    gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(Binary, AccDecodeList) ->
    ?ERR("非法的utf8编码，Binary：~w, AccDecodeList：~w", [Binary, AccDecodeList]),
    [].

%% 生成单个字符的编码
get_val(List) ->
    {_, Val} = 
        lists:foldr(fun(Elem, {Count, AccVal}) ->
                            {Count + 1, AccVal + math:pow(2, Count) * Elem}
                    end, {0, 0}, List),
    erlang:trunc(Val).

getProcDictDataProperty(DictName, PropertyIndex, DefaultValue) ->
	Data = get(DictName),
	case ?CATCH(element(PropertyIndex,Data)) of
		{'EXIT',_} -> DefaultValue;
		Value -> Value
	end.
updateProcDictDataProperty(DictName, PropertyIndex, NewValue) ->
	Data = get(DictName),
	case ?CATCH(setelement(PropertyIndex,Data,NewValue)) of
		{'EXIT',_} -> ok;
		NewData -> put(DictName, NewData)
	end.
updateProcDictDataProperty(DictName,List) ->
	lists:foreach(fun({PropertyIndex, NewValue}) -> updateProcDictDataProperty(DictName, PropertyIndex, NewValue) end,List).

getTernaryValue(BoolValue, Value1, Value2) ->
	case BoolValue of
		?TRUE -> Value1;
		_ -> Value2
	end.

getTernaryValueEx(Value, EquleValue, DefaultValue) ->
	case Value =:= EquleValue of
		?TRUE -> DefaultValue;
		_ -> Value
	end.

formap(Max,Max,F) ->
	[F(Max)];
formap(I,Max,F)when I < Max ->
	[F(I)|formap(I+1,Max,F)];
formap(_,_,_) ->
	[].

dateTimeIntervel_to_second({Day,Hour,Min,Sec})->
	Day*?ONE_DAY_SECONDS+Hour*?ONE_HOUR_SECONDS+Min*60+Sec.

listSelect(Conditions, List) ->
	lists:filter(fun(Data) -> lists:all(fun({Index,Value}) -> element(Index,Data) == Value end, Conditions) end, List).


tupleAdd(Tuple1, Tuple2, FromPos) ->
	tupleAdd(Tuple1, Tuple2, FromPos, tuple_size(Tuple1), Tuple1).
tupleAdd(Tuple1, Tuple2, EndPos, EndPos, Tuple) ->
	V = element(EndPos,Tuple1) + element(EndPos,Tuple2),
	setelement(EndPos,Tuple,V);
tupleAdd(Tuple1, Tuple2, FromPos, EndPos, Tuple) when FromPos < EndPos ->
	V = element(FromPos,Tuple1) + element(FromPos,Tuple2),
	NewTuple = setelement(FromPos,Tuple,V),
	tupleAdd(Tuple1,Tuple2,FromPos+1,EndPos,NewTuple).

make_proto([FileNameAtom]) ->
	FullName = atom_to_list(FileNameAtom),
	Path = filename:dirname(FullName),
	OnlyFileName = filename:basename(FullName, ".proto"),
	IndexStr = string:substr(OnlyFileName, 1, 3),
	StartIndex = list_to_integer(IndexStr ++ "01"),
	FileName = FullName,
	FileName2 = Path ++ "\\" ++ OnlyFileName ++ "2.proto",
	{ok, IO} = file:open(FileName, [read]),
	FileContent1 = clean_proto_file(IO, "", {?FALSE, ""}),
	%%Args::{协议序号，字段序号}
	FileContent = read_proto_file(FileContent1, "", {StartIndex, 1, ?TRUE}),
	file:write_file(FileName2, FileContent, [write]),
	halt(),
	ok.
%%自动编号
read_proto_file([], File2, {_ProtoIndex, _WordIndex, _IsWrapLine}) ->
	File2;
read_proto_file([Term1 | T], File2, {ProtoIndex, WordIndex, IsWrapLine}) ->
	Term = hd(io_lib:format("~ts",[[Term1]])),
	{AddTerm, NewProtoIndex, NewWordIndex, NewIsWrapLine} =
		case Term of
			"[" -> {"[id=" ++ integer_to_list(ProtoIndex), ProtoIndex + 1, WordIndex, IsWrapLine};
			";" ->
				case IsWrapLine of
					?TRUE -> {"=" ++ integer_to_list(WordIndex) ++ ";", ProtoIndex, WordIndex + 1, ?FALSE};
					_ -> {Term, ProtoIndex, WordIndex, IsWrapLine}%%注释里面的分号
				end;
			"}" -> {Term, ProtoIndex, 1, IsWrapLine};
			"\n" -> {Term, ProtoIndex, WordIndex, ?TRUE};
			_ -> {Term, ProtoIndex, WordIndex, IsWrapLine}
		end,
	read_proto_file(T, File2 ++ AddTerm, {NewProtoIndex, NewWordIndex, NewIsWrapLine}).
%%清除老的编号
clean_proto_file(IO, File2, {IsCollectContent, TempDelContent}) ->
	case io:get_chars(IO, '', 1) of
		eof -> File2;
		{error, ErrorDescription} ->
			io:format("ErrorDescription=~p~n", [ErrorDescription]),
			File2;
		Term ->
			{AddTerm, NewDelContent, NewIsCollectContent} =
				case Term of
					"[" -> {Term, "", ?TRUE};
					"=" ->
						case IsCollectContent of
							?TRUE -> {"", TempDelContent ++ Term, IsCollectContent};
							_ -> {"", Term, ?TRUE}
						end;
					"]" ->
						case IsCollectContent of
							?TRUE -> {Term, "", ?FALSE};
							_ -> {Term, TempDelContent, IsCollectContent}
						end;
					"\n" ->
						case IsCollectContent of
							?TRUE -> {TempDelContent ++ Term, "", ?FALSE};
							_ -> {Term, TempDelContent, IsCollectContent}
						end;
					";" ->
						case IsCollectContent of
							?TRUE -> {Term, "", ?FALSE};
							_ -> {Term, TempDelContent, IsCollectContent}
						end;
					_ ->
						case IsCollectContent of
							?TRUE -> {"", TempDelContent ++ Term, IsCollectContent};
							_ -> {Term, TempDelContent, IsCollectContent}
						end
				end,
			clean_proto_file(IO, File2 ++ AddTerm, {NewIsCollectContent, NewDelContent})
	end.

%%列表元素，区间获取值
%%List::[{Section, Value}]
%%Key >= Section
%%return Value
getValueInSectionList(Key, List, DefaultValue) ->
	SortedList = lists:keysort(1,List),
	Func = fun({Section, Value}, Acc) ->
		case Key >= Section of
			?TRUE -> Value;
			_ -> Acc
		end
		end,
	lists:foldl(Func, DefaultValue, SortedList).

boolean2int(true)->
	1;
boolean2int(false)->
	0.

tryString2int(Str) ->
	try
		erlang:list_to_integer(Str)
	catch
		_:_Why -> Str
	end.