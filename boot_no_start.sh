#!/bin/bash
erl -name mini_boot@127.0.0.1 -setcookie mini_server -pa ebin deps/ebin app