%% @private
-module(calc_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  ets:new(vartable, [set,public, named_table]),
	{ok, _} = ranch:start_listener(calc, 1,
		ranch_tcp, [{port, 5555}], calc_handler, []),
	calc_sup:start_link().

stop(_State) ->
	ok.
