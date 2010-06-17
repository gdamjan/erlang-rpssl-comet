-module (webserver).
-export ([start/1]).
-export ([dispatch_requests/1]).


start(Port) ->
    io:format("Starting mochiweb on http://localhost:~p~n", [Port]),
    gameserver:start_link(),
    uuid:start_link(),
    mochiweb_http:start([ {port, Port},
                        {loop, fun dispatch_requests/1}]).


%% Mochiweb callback, called for each request in a separate process
%% Req is an Erlang "parameterized module" (holds the state of the Request)
dispatch_requests(Req) ->
    Path = string:tokens(Req:get(path), "/"),
    case Path of
        ["static" | File] ->
            Req:serve_file(File, "./static");
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
    Req:serve_file("main.html", "./static");

handle('POST', [], Req) ->
    %% create a uuid, and redirect to it
    UUID = uuid:generate(),
    redirect(Req, UUID ++ "/");

%% Game page (/some-uuid/): serve a simple HTML/JS page with the UI
handle('GET', [UUID], Req) ->
    % make sure the URL ends with a slash ("/")
    % if not, redirect to the same URL with / appended
    case string:right(Req:get(path), 1) of
        "/" ->
            Req:serve_file("game.html", "./static");
        _ ->
            redirect(Req, UUID ++ "/")
    end;


%% Comet callback: join a game and wait for the other player
%% POST /<game-uuid>/join (data: name=AgentX)
handle('POST', [Uuid, "join"], Req) ->
    Data = mochiweb_util:parse_qs(Req:recv_body()),
    Name = proplists:get_value("name", Data),
    {Game, Opponent} = gameserver:join(Name, Uuid),
    JSON = iolist_to_binary(mochijson2:encode({struct, [
        {"game", list_to_binary(Game)},
        {"opponent", list_to_binary(Opponent)}
    ]})),
    Req:ok({"application/javascript", JSON});

%% Comet callback: play a hand and wait for the other players hand
%% POST /<game-uuid>/attack (data: attack=rock)
handle('POST', [Uuid, "attack"], Req) ->
    Data = mochiweb_util:parse_qs(Req:recv_body()),
    Attack = proplists:get_value("attack", Data),
    {Result, Game, Attack1, Attack2} = gameserver:play(Uuid, Attack),
    JSON = iolist_to_binary(mochijson2:encode({struct, [
        {"result", Result},
        {"game", list_to_binary(Game)},
        {"your-attack", list_to_binary(Attack1)},
        {"their-attack", list_to_binary(Attack2)}
    ]})),
    Req:ok({"application/javascript", JSON}).

redirect(Req, Path) ->
    Location = "http://" ++ Req:get_header_value("host") ++ "/" ++ Path,
    Req:respond({302, [{"Location", Location }], "Redirecting to " ++ Path ++ "\n"}).
