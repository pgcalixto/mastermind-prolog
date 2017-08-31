
:- module(pool, [pool/1, update_pool/5]).
:- use_module(check_answer).

/**
 * pool(-List)
 *
 * Unifies List with a list of all possible arrangements of [1,2,3,4,5,6] with
 * four elements.
 */
pool(Pool) :-
    findall(Elem, arrangements([1,2,3,4,5,6], 4, Elem), Pool).

/**
 * update_pool(+Pool, +Guess, +Correct, +Regular, -NewPool)
 *
 * Guess is the last Guess of the player, which has received the feedback of
 * Correct and Regular elements.
 * NewPool should receive the updated pool containing all possible guesses if
 * Guess was the code and the guesses feedback was Correct and Regular.
 */
update_pool([], _, _, _, []) :- !.

update_pool([PoolH|PoolT], Guess, Correct, Regular, [PoolH | NewPool1]) :-
    PoolH \= Guess,
    check_answer(Guess, PoolH, Correct, Regular),
    update_pool(PoolT, Guess, Correct, Regular, NewPool1),
    !.

update_pool([_|PoolT], Guess, Correct, Regular, NewPool) :-
    update_pool(PoolT, Guess, Correct, Regular, NewPool).

/**
 * arrangements(+List, +Size, ?Arrangement)
 *
 * Checks if Arrangement is an arrangement of List with size Size.
 * If Arrangement is a variable, list as possible arragements of Arrangement
 * with size Size.
 */
arrangements(L, 1, [X]) :- member(X, L).
arrangements(L, N, [H|T]) :-
    N > 1,
    select(H, L, M),        % Select an element "H" from "L", leaving "M"
    N1 is N-1,
    arrangements(M, N1, T). % Recurse with remaining elements and count-1
