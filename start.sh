#!/bin/sh
erl -sname todolist -pa apps/*/ebin -pa deps/*/ebin \
	-eval "application:start(cowboy)." \
	-eval "application:start(todolist)." \
	-eval "io:format(\"~n~nThe following examples are available:~n\")." \
	-eval "io:format(\"* Now check http://localhost:8080~n\")." \


