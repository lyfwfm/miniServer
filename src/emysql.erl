-module(emysql).

-export([start/0]).

start() ->
%% Connect (ssl is optional)
{ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},
                              {password, "ctl3926026"}, {database, "mini_server"}]),
  io:format("mysql pid = ~p~n",[Pid]),
ok.
