
:- begin_tests(pool).
:- use_module(pool).

test(whole_pool) :-
    open('pool.plt.in', read, PoolInput),
    read(PoolInput, Pool),
    pool(Pool),
    close(PoolInput).

:- end_tests(pool).
