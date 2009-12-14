-module(gameserver).
-behaviour(gen_server).

%% API
-export([start_link/0, play/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


play(GameUUID, Attack) ->
    gen_server:cast(?SERVER, {play, self(), GameUUID, Attack}).
 
%% gen_server callbacks
init([]) ->
    {ok, dict:new()}.

handle_cast({play, Pid, GameUUID, Attack}, State) ->
    io:format("play: ~p ~p ~p~n", [Pid, GameUUID, Attack]),
    case dict:is_key(GameUUID, State) of
        true ->
            {PrevPid, PrevAttack} = dict:fetch(GameUUID, State),
            {R1, R2} = rpssl:play(PrevAttack, Attack),
            Pid ! {result, R2, GameUUID, Attack, PrevAttack},
            PrevPid ! {result, R1, GameUUID, PrevAttack, Attack},
            {noreply, dict:erase(GameUUID, State)};
        false ->
            {noreply, dict:store(GameUUID, {Pid, Attack}, State)}
    end.


handle_call(_Msg, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
