
:- begin_tests(pool).
:- use_module(pool).

test(whole_pool) :-
    open('pool.plt.in', read, PoolInput),
    read(PoolInput, Pool),
    pool(Pool),
    close(PoolInput).

test(update_pool) :-
    pool(Pool),
    update_pool(Pool, [1,2,3,4], 0, 4, NewPool),
    NewPool = [[2,1,4,3], [2,3,4,1], [2,4,1,3], [3,1,4,2], [3,4,1,2], [3,4,2,1],
               [4,1,2,3], [4,3,1,2], [4,3,2,1]].

:- end_tests(pool).
