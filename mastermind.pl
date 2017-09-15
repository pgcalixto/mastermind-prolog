#!/usr/bin/swipl

:- module(mastermind, [play/0]).

:- use_module(check_answer).
:- use_module(player).
:- use_module(pool).

/**
 * first_try(-Correct, -Regular)
 *
 * Performs the first guess of the player (1,2,3,4).
 */
first_try(Correct, Regular) :-
    write([1,2,3,4]), nl,
    read([Correct, Regular]).

/**
 * try(+Pool, -Correct, -Regular)
 *
 * Given the current pool, the last feedback of Correct and Regular elements,
 * performs the steps needed to try to win the game.
 */
try(Pool, Correct, Regular) :-
    % If the pool is empty, prints an error code and the goal is satisfied.
    % Otherwise, make a new guess, write it and checks its response to see if
    % the guess was the correct one or if another guess needs to be made.
    (   Pool = [], write('erro'), nl
    ;   player:make_guess(Pool, Correct, Regular, Guess),
        write(Guess), nl,
        read([Correct1, Regular1]),
        (   Correct1 = 4, Regular1 = 0, write('ganhei'), nl
        ;   pool:update_pool(Pool, Guess, Correct1, Regular1, NewPool),
            try(NewPool, Correct1, Regular1)
        )
    ).

play :-
    % Makes the first guess. Based on it, checks if it was the correct guess. In
    % the case it was not, update the guesses pool and try another guess.
    FirstGuess = [1,2,3,4],
    first_try(Correct, Regular),
    (   Correct = 4, Regular = 0, write('ganhei'), nl
    ;   pool:pool(Pool),
        pool:update_pool(Pool, FirstGuess, Correct, Regular, NewPool),
        try(NewPool, Correct, Regular)
    ).

:- play, halt.
