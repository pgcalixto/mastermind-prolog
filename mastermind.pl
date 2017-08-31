% #!/usr/bin/swipl

:- module(mastermind, [make_guess/4]).

:- use_module(answer_generator).
:- use_module(pool).
:- use_module(check_answer).

/**
 * make_guess(+Pool, +Correct, +Regular, -Guess)
 *
 * Given the current Pool of possible guesses and the last feedback on Correct
 * and Regular elements, make_guess returns the best Guess.
 */
make_guess(Pool, Correct, Regular, Guess) :-
    make_guess_aux(Pool, Pool, Correct, Regular, Guess, _).

/**
 * make_guess_aux(+WholePool, +Pool, +Correct, +Regular, -Guess, -Score)
 *
 * WholePool is the current whole pool of possible guesses.
 * Pool is WholePool being iterated element by element.
 * Given the Correct and Regular feedback, for every element of the WholePool,
 * iterated by Pool, Guess contains the best guess and Score contains this guess
 * Knuth algorithm's score.
 * The Knuth score contains the number of possible guesses, if Guess were the
 * code and Correct and Regular were the feedback for these guesses.
 */
make_guess_aux(WholePool, [Guess], Correct, Regular, Guess, Score) :-
    update_pool(WholePool, Guess, Correct, Regular, NewPool),
    length(NewPool, Score),
    !.

make_guess_aux(WholePool, [PoolH|PoolT], Correct, Regular, PoolH, Score) :-
    make_guess_aux(WholePool, PoolT, Correct, Regular, _, Score1),
    update_pool(WholePool, PoolH, Correct, Regular, NewPool),
    length(NewPool, Score),
    Score =< Score1,
    !.

make_guess_aux(WholePool, [_|PoolT], Correct, Regular, Guess, Score) :-
    make_guess_aux(WholePool, PoolT, Correct, Regular, Guess, Score).
