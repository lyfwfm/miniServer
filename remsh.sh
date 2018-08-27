#!/bin/bash
erl -name attach_mini@127.0.0.1 -setcookie mini_server -pa ebin deps/ebin -remsh mini@127.0.0.1
