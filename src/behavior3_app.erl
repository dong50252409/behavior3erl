-module(behavior3_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    behavior3_app_sup:start_link().

stop(_State) ->
    ok.
