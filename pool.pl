
:- module(pool, [pool/1]).

/**
 * pool(-List)
 *
 * Unifies List with a list of all possible arrangements of [1,2,3,4,5,6] with
 * four elements.
 */
pool(Pool) :-
    findall(Elem, arrangements([1,2,3,4,5,6], 4, Elem), Pool).

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
    select(H, L, M),            % Select an element "H" from "L", leaving "M"
    N1 is N-1,
    arrangements(M, N1, T). % Recurse with remaining elements and count-1
