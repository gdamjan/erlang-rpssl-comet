-module(rpssl_app).
-author('gdamjan@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).


start() ->
    application:start(rpssl).


start(_Type, _StartArgs) ->
    webserver:start(getport()).

getport() ->
    case init:get_argument(port) of
        {ok, Values} ->
            [Val | _] = lists:last(Values),
            Val;
        _ ->
            "9999"
    end.


stop(_State) ->
    ok.
