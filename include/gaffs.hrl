-ifndef(GAFF_HRL).
-define(GAFF_HRL, true).

-record(hook,
    {
        module,
        func,
        arity = 0,
        priority = 0
    }).

-endif.
