-module(rpssl_game).
-export([play/1, play/2]).


%% Play a game of rock-paper-scissors
play(PlayerAttack) ->
    ComputerAttack = get_computer_attack(),
    case get_result(PlayerAttack, ComputerAttack) of
        {win,_} ->
            <<": I chose ", ComputerAttack, ". You win!">>;
        {draw,_} ->
            <<": I chose ", ComputerAttack, ". It's a draw.">>;
        {lose,_} ->
            <<": I chose ", ComputerAttack, ". I WIN!">>
    end.

play(Player1, Player2) ->
    get_result(Player1, Player2).

%% choose a computer attack at random
get_computer_attack() ->
    %% Get an index position at random
    Index = rand:uniform(5),
    %% Pull out an attack
    lists:nth(Index, [<<"rock">>, <<"paper">>, <<"scissors">>, <<"spock">>, <<"lizard">>]).

%% Determine the result of an attack
get_result(Player1, Player2) ->
    case {Player1, Player2} of
        {<<"Rock">>, <<"Scissors">>} -> {win, lose};
        {<<"Rock">>, <<"Lizard">>} -> {win, lose};
        {<<"Paper">>, <<"Rock">>} -> {win, lose};
        {<<"Paper">>, <<"Spock">>} -> {win, lose};
        {<<"Scissors">>, <<"Paper">>} -> {win, lose};
        {<<"Scissors">>, <<"Lizard">>} -> {win, lose};
        {<<"Spock">>, <<"Rock">>} -> {win, lose};
        {<<"Spock">>, <<"Scissors">>} -> {win, lose};
        {<<"Lizard">>, <<"Spock">>} -> {win, lose};
        {<<"Lizard">>, <<"Paper">>} -> {win, lose};
        {Same, Same} -> {draw, draw};
        {_,_} -> {lose, win}
    end.
