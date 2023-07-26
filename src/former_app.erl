-module(former_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    io:format("~p:~p - starting...~n", [?MODULE, ?LINE]),
    former_sup:start_link().

stop(_State) ->
    former:stop(),
    ok.

%% internal functions


