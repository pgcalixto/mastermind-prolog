% #!/usr/bin/swipl

:- module(mastermind, [play/0]).

:- use_module(answer_generator).
:- use_module(check_answer).
:- use_module(player).
:- use_module(pool).

/**
 * first_try(+Answer, +Guess, -Correct, -Regular)
 *
 * Performs the first guess of the player (1,2,3,4).
 */
first_try(Answer, Guess, _, _) :-
    check_answer:check_answer(Answer, Guess, 4, 0),
    write('ganhei'), nl, halt.

first_try(Answer, Guess, Correct, Regular) :-
    check_answer:check_answer(Answer, Guess, Correct, Regular).

/**
 * try(+Answer, +Pool, -Correct, -Regular)
 *
 * Given the current pool, the last feedback of Correct and Regular elements,
 * performs the steps needed to try to win the game.
 */
try(Answer, Pool, Correct, Regular) :-
    player:make_guess(Pool, Correct, Regular, Guess),
    check_answer:check_answer(Answer, Guess, Correct1, Regular1),

    (
     (Correct1 = 4, Regular1 = 0, write('ganhei'), nl, halt) ;
     (
      pool:update_pool(Pool, Guess, Correct1, Regular1, NewPool),
      try(Answer, NewPool, Correct1, Regular1)
     )
    ).

play :-
    answer_generator:answer(Answer),

    FirstGuess = [1,2,3,4],
    first_try(Answer, FirstGuess, Correct, Regular),
    pool:pool(Pool),
    pool:update_pool(Pool, FirstGuess, Correct, Regular, NewPool),
    try(Answer, NewPool, Correct, Regular).

:- play.
