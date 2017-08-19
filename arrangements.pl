
:- module(arrangements, [arrangements/1]).

/**
 * arrangements(?List)
 *
 * Checks if List is a arrangement of [1,2,3,4,5,6].
 * If List is a variable, it lists all possible arrangements of [1,2,3,4,5,6].
 */
arrangements(R):-
    arrangements_aux([1,2,3,4,5,6], 4, R).

/**
 * arrangements_aux(+List, +Size, ?Arrangement)
 *
 * Checks if Arrangement is an arrangement of List with size Size.
 * If Arrangement is a variable, list as possible arragements of Arrangement
 * with size Size.
 */
arrangements_aux(L, 1, [X]) :- member(X, L).
arrangements_aux(L, N, [H|T]) :-
    N > 1,
    select(H, L, M),            % Select an element "H" from "L", leaving "M"
    N1 is N-1,
    arrangements_aux(M, N1, T). % Recurse with remaining elements and count-1
