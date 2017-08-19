
:- begin_tests(check_answer).
:- use_module(check_answer).

test(correct_try) :-
    check_answer([a,b,c,d], [a,b,c,d], 4, 0).

test(even_answer_with_reversed_try) :-
    X = [a,b,c,d],
    reverse(X, Y),
    check_answer(X, Y, 0, 4).

test(odd_answer_with_reversed_try) :-
    X = [a,b,c,d,e],
    reverse(X, Y),
    check_answer(X, Y, 1, 3).

:- end_tests(check_answer).
