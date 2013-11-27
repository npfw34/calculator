-module(calc).

%% API.
-export([start/0]).

%% API.

start() ->
	ok = application:start(ranch),
  ok = application:start(compiler),
  ok = application:start(syntax_tools),
  ok = application:start(goldrush),
  ok = application:start(lager),
	ok = application:start(calc).
