#!/bin/sh
erl -pa ebin deps/*/ebin -s calc -config priv/app.config\
	-eval "io:format(\"Run: telnet localhost 5555~n\")."
