
/**
 * check_answer_aux(+AnswerSlice, +TrySlice, ++AnswerFull, ++TryFull, -Correct,
                    -Regular) is det
 *
 * Checks the number of correct and regular elements in the try.
 *
 * The first call of check_answer_aux contains all the elements of the answer
 * and the try in AnswerFull, AnswerSlice, TryFull and TrySlice.
 *
 * Every execution of the functions checks whether the heads of AnswerSlice and
 * TrySlice are equals (correct element), or if the head of TrySlice is
 * contained in AnswerFull (regular element).
 *
 * Then, it calls check_answer_aux recursively using the tails of AnswerSlice
 * and TrySlice as the new AnswerSlice and TrySlice lists.
 */
check_answer_aux([], [], _, _, 0, 0).

% case which the element is in the correct position
check_answer_aux([X1|X2], [X1|Y2], X, Y, Correct, Regular) :-
    check_answer_aux(X2, Y2, X, Y, Correct1, Regular),
    Correct is Correct1 + 1.

% case which the element is not in the correct position, but exists in other
% positions, i.e., it is a regular
check_answer_aux([_|X2], [Y1|Y2], X, Y, Correct, Regular) :-
    member(Y1, X),
    check_answer_aux(X2, Y2, X, Y, Correct, Regular1),
    Regular is Regular1 + 1.

% case which the element is neither correct nor regular
check_answer_aux([_|X2], [_|Y2], X, Y, Correct, Regular) :-
    check_answer_aux(X2, Y2, X, Y, Correct, Regular).

/**
 * check_answer(+Answer, +Try, -Correct, -Regular) is det
 *
 * Checks the number of correct and regular elements in the try.
 *
 * It calls `check_answer_aux()` to retrieve Correct and Regular.
 */
check_answer(X, Y, Correct, Regular) :-
    check_answer_aux(X, Y, X, Y, Correct, Regular).
