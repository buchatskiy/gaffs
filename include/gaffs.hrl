-ifndef(GAFF_HRL).
-define(GAFF_HRL, true).

-record(hook,
    {
        module,
        func,
        arity = 0,
        options = [],
        priority = 0
    }).

-endif.
