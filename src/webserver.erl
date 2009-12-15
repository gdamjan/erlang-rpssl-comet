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


%% Comet callback: play a hand and wait for the other players hand
%% POST /<game-uuid>/attack (data: attack=rock)
handle('POST', [Uuid, "attack"], Req) ->
    Data = mochiweb_util:parse_qs(Req:recv_body()),
    Attack = proplists:get_value("attack", Data),
    {Result, Game, MyAttack, TheirAttack} = gameserver:play(Uuid, Attack),
    JSON = iolist_to_binary(mochijson2:encode({struct, [
        {"result", Result},
        {"game", list_to_binary(Game)},
        {"my-attack", list_to_binary(MyAttack)},
        {"their-attack", list_to_binary(TheirAttack)}
    ]})),
    Req:ok({"application/javascript", JSON}).
