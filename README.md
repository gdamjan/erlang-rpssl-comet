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


Mochiweb
========

Comet-RPSSL uses mochiweb as a http server. If you don't already have it
installed in the Erlang library path, you can use the git submodule:

    git submodule init
    git submodule update
    make mochiweb

Now you can compile and run Comet-RPSSL:

    make
    erl -pa ebin/ -pa deps/mochiweb/ebin
    webserver:start(9999).

For a quick test without a browser, run the following commands in
two separate terminals:

    curl -d attack=rock http://localhost:9999/test/attack
    curl -d attack=scissors http://localhost:9999/test/attack

You will notice how the first one blocks until the second one plays, and then
both are immediately released.


The code is MIT licensed.
