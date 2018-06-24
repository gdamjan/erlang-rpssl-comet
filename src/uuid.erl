% from http://stackoverflow.com/questions/1657204/erlang-uuid-generator
-module(uuid).
-behaviour(gen_server).

%% API
-export([start_link/0, generate/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

generate() ->
    gen_server:call(?SERVER, generate).

init([]) -> 
    rand:seed(exs1024s),
    {ok, []}.

handle_call(generate, _From, State) ->
    UUID = to_string(v4()),
    {reply, UUID, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.




v4() ->
    v4(rand:uniform(16#FFFFFFFFFFFF), rand:uniform(16#FFF),
       rand:uniform(16#FFFFFFFF), rand:uniform(16#3FFFFFFF)).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].
