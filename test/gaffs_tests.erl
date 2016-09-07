%%% @author O. Buchatskiy <buchatskiy2@gmail.com>
%%% @copyright 2016 O. Buchatskiy

-module(gaffs_tests).

-ifdef(TEST).
-compile(export_all).

-include("gaffs.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_FUN, hook).
-define(HOOK_NAME, ?TEST_FUN).
-define(TEST_FUN_FOLDL, foldl).
-define(HOOK_NAME_FOLDL, ?TEST_FUN_FOLDL).
-define(ARG, 2).
-define(ACC, 3).
-define(SETUP(F), {setup, fun start/0, fun stop/1, F}).
-define(DEFAULT_OPTIONS, [true]).

all_test_() ->
    error_logger:tty(false),
    [{"This is set of basic tests.",
        ?SETUP(fun all_basic/1)},
    {"This is foldl tests.",
        [?SETUP(fun check_foldl/1),
        ?SETUP(fun check_foldl_stop/1),
        ?SETUP(fun check_foldl_stop_acc/1)]},
    {"This is mregister tests.",
        ?SETUP(fun all_mregister/1)}].

all_basic(_Params) ->
    [
        register(),
        error_register_no_module(),
        error_register_no_func(),
        check_register(),
        unregister(),
        check_unregister(),
        register_with_pid(),
        check_register_with_pid(),
        check_pid_die()
    ].

all_mregister(_Params) ->
    [
        check_mregister_bad_hook(),
        check_mregister_bad_module(),
        check_mregister_bad_fun(),
        mregister(),
        check_mregister(),
        check_map()
    ].

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

register() ->
    ?_assertEqual(ok, gaffs:register(?MODULE, ?TEST_FUN, 1)).

error_register_no_module() ->
    ?_assertEqual({error, no_module}, gaffs:register(fake_module, ?TEST_FUN, 1)).

error_register_no_func() ->
    ?_assertEqual({error, no_func}, gaffs:register(?MODULE, fake_func, 0)).

check_register() ->
    ?_assertEqual({ok, [#hook{module = ?MODULE, func = ?TEST_FUN, arity = 1}]}, gaffs_cache:lookup(?HOOK_NAME)).

unregister() ->
    ?_assertEqual(ok, gaffs:unregister(?MODULE, ?TEST_FUN, 1)).

check_unregister() ->
    ?_assertEqual({ok, []}, gaffs_cache:lookup(?HOOK_NAME)).

register_with_pid() ->
    ?_assertEqual(ok, gaffs:register(?HOOK_NAME, ?MODULE, ?TEST_FUN, 1, ?DEFAULT_OPTIONS, _Priority = 0, spawn(fun() -> timer:sleep(20), exit(kill) end))).

check_register_with_pid() ->
    ?_assertEqual({ok, [#hook{module = ?MODULE, func = ?TEST_FUN, arity = 1, options = ?DEFAULT_OPTIONS}]}, gaffs_cache:lookup(?HOOK_NAME)).

check_pid_die() ->
    Fun = fun() -> timer:sleep(20), gaffs_cache:lookup(?HOOK_NAME) end,
    ?_assertEqual({ok, []}, Fun()).

check_foldl(_) ->
    reg_base_foldl(),
    ?_assertEqual({ok, ?ACC+?ARG*3}, gaffs:foldl(?HOOK_NAME_FOLDL, [?ARG], ?ACC)).

check_foldl_stop(_) ->
    reg_base_foldl(foldl_stop),
    ?_assertEqual({stop, ?ACC+?ARG*1, {?MODULE, foldl_stop}}, gaffs:foldl(?HOOK_NAME_FOLDL, [?ARG], ?ACC)).

check_foldl_stop_acc(_) ->
    reg_base_foldl(foldl_stop_acc),
    ?_assertEqual({stop, ?ACC+?ARG*2, {?MODULE, foldl_stop_acc}}, gaffs:foldl(?HOOK_NAME_FOLDL, [?ARG], ?ACC)).

check_mregister_bad_hook() ->
    ?_assertEqual([{?HOOK_NAME, {error, bad_hook}}], gaffs:mregister([{?HOOK_NAME, [some_bad_hook]}])).

check_mregister_bad_module() ->
    ?_assertEqual([{?HOOK_NAME, {error, no_module}}], gaffs:mregister([{?HOOK_NAME, [{some_bad_module, hook, 1}]}])).

check_mregister_bad_fun() ->
    ?_assertEqual([{?HOOK_NAME, {error, no_func}}], gaffs:mregister([{?HOOK_NAME, [{?MODULE, some_bad_fun, 0}]}])).

mregister() ->
    Priorities = [0,1,2,2],
    Hooks = [{?HOOK_NAME, [{?MODULE, ?TEST_FUN, 1, ?DEFAULT_OPTIONS, Priority} || Priority <- Priorities]}],
    ExceptionHook = {?HOOK_NAME, [{?MODULE, hook_exception, 1, ?DEFAULT_OPTIONS, 3}]},
    ?_assertEqual([{?HOOK_NAME, ok}, {?HOOK_NAME, ok}, {?HOOK_NAME, ok}], gaffs:mregister(Hooks++Hooks++[ExceptionHook])). %% only one with same priority can be registered

check_mregister() ->
    Priorities = [0,1,2],
    Hooks = [#hook{module = ?MODULE, func = ?TEST_FUN, arity = 1, options = ?DEFAULT_OPTIONS, priority = Priority} || Priority <- Priorities],
    ExceptionHook = #hook{module = ?MODULE, func = hook_exception, arity = 1, options = ?DEFAULT_OPTIONS, priority = 3},
    ?_assertEqual({ok, Hooks++[ExceptionHook]}, gaffs_cache:lookup(?HOOK_NAME)).

check_map() ->
    Res = {{?MODULE, ?TEST_FUN, 1}, {ok, []}},
    ExceptionRes = {{?MODULE, hook_exception, 1}, {error,unknown}},
    ?_assertEqual([Res, Res, Res, ExceptionRes], gaffs:map(?HOOK_NAME, [])).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
hook(?DEFAULT_OPTIONS) -> [].

hook_exception(?DEFAULT_OPTIONS) -> error(some_exception).

all() -> gaffs:register(?MODULE, ?TEST_FUN, 0).

foldl(Value, ?DEFAULT_OPTIONS, Acc) -> {ok, Value+Acc}.

foldl_stop(_Value, ?DEFAULT_OPTIONS, _Acc) -> stop.

foldl_stop_acc(Value, ?DEFAULT_OPTIONS, Acc) -> {stop, Value+Acc}.

reg_base_foldl() ->
    Priorities = [0,1,2,2], %% only one with same priority can be registered, so only three will be registered
    [ok = gaffs:register(?HOOK_NAME_FOLDL, ?MODULE, ?TEST_FUN_FOLDL, 3, ?DEFAULT_OPTIONS, Priority) || Priority <- Priorities].

reg_base_foldl(FunName) ->
    reg_base_foldl(),
    ok = gaffs:unregister(?HOOK_NAME_FOLDL, ?MODULE, ?TEST_FUN_FOLDL, 3, 1), %% change one fun name
    ok = gaffs:register(?HOOK_NAME_FOLDL, ?MODULE, FunName, 3, ?DEFAULT_OPTIONS, 1). %% to another

start() ->
    gaffs:start(),
    _SomeParams = [].

stop(_SomeParams) ->
    gaffs:stop().

-endif.
