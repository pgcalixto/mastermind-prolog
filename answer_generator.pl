
:- module(answer_generator, [answer/1]).

:- use_module(pool).

answer(X) :-
    pool:pool(Pool),
    choose(Pool, X).

%% choose(List, Elt) - chooses a random element
%% in List and unifies it with Elt.
choose([], []).
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).
