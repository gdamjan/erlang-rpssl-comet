-module (webserver).
-export ([start/1]).
-export ([dispatch_requests/1]).

start(Args) ->
    [Port] = Args,
    io:format("Starting mochiweb on http://localhost:~p~n", [Port]),
    gameserver:start_link(),
    mochiweb_http:start([ {port, Port},
                        {loop, fun dispatch_requests/1}]).


%% This is called for each HTTP request in a new Erlang process
%% Req will be a parameterized module holding the state (like an object)
dispatch_requests(Req) ->
    Path = string:tokens(Req:get(path), "/"),
    case Path of
        ["static" | File] ->
            Req:serve_file(string:join(File, "/"), "./static");
        _ ->
            Method = Req:get(method),
            case (catch handle(Method, Path, Req)) of
                {'EXIT', _E} -> 
                    Req:not_found();
                Resp ->
                    Resp
            end
    end.


%% Home page (/): just serve a static file
handle('GET', [], Req) ->
    {ok, File} = file:open("static/index.html", read),
    Req:respond({200, [{"Content-Type", "text/html"}], {file, File}});

handle('POST', [], Req) ->
    %% create a uuid, and redirect to it
    Location = "http://" ++ Req:get_header_value("host") ++ "/" ++ uuid:to_string(uuid:v4()) ++ "/",
    Req:respond({302, [{"Location", Location }], "Redirecting to " ++ Location});

%% Game page (/some-uuid/): serve a simple HTML/JS page with the UI
handle('GET', [UUID], Req) ->
    % make sure the URL ends with a slash ("/")
    % if not, redirect to the same URL with / appended
    case string:right(Req:get(path), 1) of
        "/" ->
            {ok, File} = file:open("static/game.html", read),
            Req:respond({200, [{"Content-Type", "text/html"}], {file, File}});
        _ ->
            Location = "http://" ++ Req:get_header_value("host") ++ "/" ++ UUID ++ "/",
            Req:respond({302, [{"Location", Location }], "Redirecting to " ++ Location})
    end;



%% Comet callback from the page POST to /some-uuid
handle('POST', [Uuid], Req) ->
    %% it will play a hand for the user and wait 
    %% when the other user plays his hand too, the gameserver will notify both
    %% including the results of the game
    %% Request is a POST of: attack=rock/paper/scissors/spock/lizard
    %% Response is JSON of:
    %%  {"result":"win/draw/lose","game":"uuid","attack":"paper","opponent":"paper"}
    Data = mochiweb_util:parse_qs(Req:recv_body()),
    Attack = proplists:get_value("attack", Data),
    gameserver:play(Uuid, Attack),
    receive
        {result, Result, Game, Attack, Opponent} ->
            JSON = iolist_to_binary(mochijson2:encode({struct, [
                {"result", Result},
                {"game", list_to_binary(Game)},
                {"attack", list_to_binary(Attack)},
                {"opponent", list_to_binary(Opponent)}
            ]})),
            Req:ok({"application/javascript", JSON})
    end.
