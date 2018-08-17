%% Emysql .app file template
%% This template is filled out by rebar, 
%% or make (Makefile made to fill modules in)
%% and then cp src/emysql.app.src ebin/emysql.app

%% Settings (defaults in include/emysql.hrl):
%% default_timeout (TIMEOUT = 8000)
%% lock_timeout (LOCK_TIMEOUT = 5000)

{application, emysql, [
    {description, "Emysql - Erlang MySQL driver"},
    {vsn, "0.3.0"},
    {{modules, [emysql, emysql_app, emysql_auth, emysql_conn, emysql_conn_mgr, emysql_statements, emysql_sup, emysql_tcp, emysql_tracer, emysql_util, emysql_worker]}}, 
    {mod, {emysql_app, ["2013年12月 4日 星期三 20时10分07秒 CST"]}},
    {registered, []},
    {applications, [kernel, stdlib, crypto]},
    {env, [{default_timeout, 30000}, {conn_test_period, 28000}, {lock_timeout, 15000}]}
]}.
