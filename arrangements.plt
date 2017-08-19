
:- begin_tests(arrangements).
:- use_module(arrangements).

test(all_arrangements) :-
    open('arrangements.plt.in', read, _, [alias(input)]),
    read(input, List),
    findall(X, arrangements(X), List).

:- end_tests(arrangements).
