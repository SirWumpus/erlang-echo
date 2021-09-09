-module(echo).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(echo).

start(normal, []) ->
	echo_sup:start_link().

stop(_) ->
	ok.
