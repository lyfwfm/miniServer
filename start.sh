#!/bin/bash
erl  +K true +t 5000000  +Q 1048576 +P 1048576 -name mini@127.0.0.1 -setcookie mini_server -pa ebin deps/ebin app -s user_default startAndRun -detached
