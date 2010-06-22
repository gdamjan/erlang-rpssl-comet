-module(rpssl_game).
-export([play/1, play/2]).


%% Play a game of rock-paper-scissors
play(PlayerAttack) ->
    ComputerAttack = get_computer_attack(),
    case get_result(PlayerAttack, ComputerAttack) of
        {win,_} ->
            [": I chose ", atom_to_list(ComputerAttack), ". You win!"];
        {draw,_} ->
            [": I chose ", atom_to_list(ComputerAttack), ". It's a draw."];
        {lose,_} ->
            [": I chose ", atom_to_list(ComputerAttack), ". I WIN!"]
    end.

play(Player1, Player2) ->
    get_result(Player1, Player2).

%% choose a computer attack at random
get_computer_attack() ->
    %% Get an index position at random
    Index = random:uniform(5),
    %% Pull out an attack
    lists:nth(Index, ["rock", "paper", "scissors", "spock", "lizard"]).

%% Determine the result of an attack
get_result(Player1, Player2) ->
    case {Player1, Player2} of
        {"rock", "scissors"} -> {win, lose};
        {"rock", "lizard"} -> {win, lose};
        {"paper", "rock"} -> {win, lose};
        {"paper", "spock"} -> {win, lose};
        {"scissors", "paper"} -> {win, lose};
        {"scissors", "lizard"} -> {win, lose};
        {"spock", "rock"} -> {win, lose};
        {"spock", "scissors"} -> {win, lose};
        {"lizard", "spock"} -> {win, lose};
        {"lizard", "paper"} -> {win, lose};
        {Same, Same} -> {draw, draw};
        {_,_} -> {lose, win}
    end.
