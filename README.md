Comet RPSSL
===========


RPSSL - Rock-Paper-Scissors-Spock-Lizard is a simple but famous game for 2
players. Usually it's played with your own hands, in first person, but this is
the web version.

The web version uses the [Comet technique][comet] which is a long running HTTP
request that can be used for immediate server push of data to the client.

[comet]: http://en.wikipedia.org/wiki/Comet_%28programming%29

Each http request is a separate Erlang process. We can leave it to block
forever, holding the connection open, since it's using very little resources.

Compile
=======

This project now uses the "rebar" tool to compile and handle dependencies.
I've decided not to bundle the tool itself in the source tree, but it's easy to
get from the [rebar projects site][rebar3] (don't forget to make it executable).

[rebar]: https://www.rebar3.org/

Rebar can get the dependencies, compile everything at once and even install:

    rebar3 compile

(note: the Makefile is left as an example, but doesn't need to be used)



Game
====

And we are ready to start:

    rebar3 shell
    webserver:start("9999").

For a quick test without a browser, run the following commands in
two separate terminals:

    curl -d '{"attack": "Rock"}' http://localhost:9999/test-game/attack
    curl -d '{"attack": "Scissors"}' http://localhost:9999/test-game/attack

You will notice how the first one blocks until the second one plays, and then
both are immediately released. (test-game is the id of the game)


The code is MIT licensed.
