-module(gameserver).
-behaviour(gen_server).

%% API
-export([start_link/0, play/2, join/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


join(Name, GameUUID) ->
    gen_server:call(?SERVER, {join, Name, GameUUID}, infinity).

play(GameUUID, Attack) ->
    gen_server:call(?SERVER, {play, GameUUID, Attack}, infinity).


%% gen_server callbacks
init([]) ->
    {ok, dict:new()}.

handle_call({join, _Name, _GameUUID}, _From, State) ->
    {noreply, State};

handle_call({play, GameUUID, Attack}, From, State) ->
    io:format("play: ~p ~p~n", [GameUUID, Attack]),

    case dict:is_key(GameUUID, State) of
        % first player attacks, don't reply just save his hand
        false ->
            {noreply, dict:store(GameUUID, {From, Attack}, State)};
        % second player attacks, play the game, reply to both
        true ->
            {OtherFrom, OtherAttack} = dict:fetch(GameUUID, State),
            {R1, R2} = rpssl:play(OtherAttack, Attack),
            gen_server:reply(OtherFrom, {R1, GameUUID, OtherAttack, Attack}),
            Result = {R2, GameUUID, Attack, OtherAttack},
            {reply, Result, dict:erase(GameUUID, State)}
    end.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
