#!/bin/bash
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -env ERL_MAX_ETS_TABLES 999999 -name mini@127.0.0.1 -setcookie mini_server -pa ebin deps/ebin app -s tk start -detached
