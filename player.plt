
:- begin_tests(player).
:- use_module(player).

test(make_guess) :-
    Pool = [[2,1,4,3], [2,3,4,1], [2,4,1,3], [3,1,4,2], [3,4,1,2],
            [3,4,2,1], [4,1,2,3], [4,3,1,2], [4,3,2,1]],
    Correct = 0,
    Regular = 4,
    Guess = [2,3,4,1],
    make_guess(Pool, Correct, Regular, Guess).

:- end_tests(player).
