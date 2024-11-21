-module(project_name_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	project_name_sup:start_link().

stop(_State) ->
	ok.
