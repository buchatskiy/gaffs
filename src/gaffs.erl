%%% @copyright 2016 Led <ledest@gmail.com>
%%% @doc gaffs: Yet another generic Erlang hooks application
%%%
%%% gaffs is free software: you can redistribute it and/or modify it
%%% under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% gaffs is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%% See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with gaffs. If not, see <a href="http://www.gnu.org/licenses/">Licenses</a>.
%%% @end

-module(gaffs).

-export([
        start/0,
        stop/0
    ]).
%% API for manual hook registration
-export([
        register/3,
        register/4,
        register/5,
        register/6,
        mregister/1,
        unregister/3,
        unregister/4,
        unregister/5
    ]).

%% API for running hooks
-export([
        all/2,
        foldl/3
    ]).

-include("gaffs.hrl").
%% ===================================================================
%% API functions
%% ===================================================================
%% @doc Start the gaffs.
start() ->
    application:start(gaffs, permanent).

%% @doc Stop the gaffs.
stop() ->
    application:stop(gaffs).

%% @doc Save hook with HookName like Fun and default Priority = 0.
-spec register(Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok | {error, Reason::atom()}.
register(Module, Fun, Arity) ->
    register(_HookName = Fun, Module, Fun, Arity).

%% @doc Save hook for HookName with default Priority = 0.
-spec register(HookName::any(), Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok | {error, Reason::atom()}.
register(HookName, Module, Fun, Arity) ->
    register(HookName, Module, Fun, Arity, _Priority = 0).

%% @doc Save hook for HookName with specified Priority.
-spec register(HookName::any(), Module::atom(), Fun::atom(), Arity::non_neg_integer(), Priority::integer()) -> ok | {error, Reason::atom()}.
register(HookName, Module, Fun, Arity, Priority) ->
    gaffs_cache:insert(HookName, {Module, Fun, Arity}, Priority).

%% @doc Same as previous, but automatically delete hook if Pid dies.
-spec register(HookName::any(), Module::atom(), Fun::atom(), Arity::non_neg_integer(), Priority::integer(), Pid::pid()) -> ok | {error, Reason::atom()}.
register(HookName, Module, Fun, Arity, Priority, Pid) ->
    case register(HookName, Module, Fun, Arity, Priority) of
        ok -> gaffs_srv:monitor(Pid, HookName, {Module, Fun, Arity}, Priority);
        Error -> Error
    end.

%% @doc Register list of Hooks for list of HookNames. List of hooks will be checked, if one Hook for HookName is bad, than no one will be registered.
-spec mregister([{HookName::any(), [MFA::tuple() | #hook{}]}]) -> [{HookName::any(), ok | {error, Reason::atom()}}].
mregister(Hooks) ->
    gaffs_cache:mregister(Hooks).

%% @doc Delete hook with HookName like Fun and default Priority = 0.
-spec unregister(Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok.
unregister(Module, Fun, Arity) ->
    unregister(_HookName = Fun, Module, Fun, Arity).

%% @doc Delete hook for HookName with default Priority = 0.
-spec unregister(HookName::any(), Module::atom(), Fun::atom(), Arity::non_neg_integer()) -> ok.
unregister(HookName, Module, Fun, Arity) ->
    unregister(HookName, Module, Fun, Arity, _Priority = 0).

%% @doc Delete hook for HookName with specified Priority.
-spec unregister(HookName::any(), Module::atom(), Fun::atom(), Arity::non_neg_integer(), Priority::integer()) -> ok.
unregister(HookName, Module, Fun, Arity, Priority) ->
    gaffs_cache:delete(HookName, {Module, Fun, Arity}, Priority).

%% @doc Run all hooks with Args for HookName, ignore exceptions.
-spec all(HookName::any(), Args::list()) -> [{{Module::atom(), Fun::atom(), Arity::non_neg_integer()}, Res::{ok, any()} | {error, unknown}}].
all(HookName, Args) ->
    {ok, Hooks} = gaffs_cache:lookup(HookName),
    lists:map(
        fun(#hook{module = Module, func = Fun, arity = Arity}) ->
            Res = try {ok, apply(Module, Fun, Args)}
            catch
                C:E ->
                    Stacktrace = erlang:get_stacktrace(),
                    error_logger:error_msg("Hook: ~p:~p/~p apply exception: ~p:~p~nArgs: ~p~nStacktrace: ~p",
                         [Module, Fun, Arity, C, E, Args, Stacktrace]),
                    {error, unknown}
            end,
            {{Module, Fun, Arity}, Res}
        end, Hooks).
%%--------------------------------------------------------------------
%% @doc
%% Run all hooks with Args++Acc for HookName ordered by priority, return accumulated result.
%% Can be stoped if hook result will be stop | {stop, Acc} | {stop, Acc, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec foldl(HookName::any(), Args::list(), Acc::any()) -> {ok, NewAcc::any()} | {stop, Acc::any(), {Module::atom(), Fun::atom()}}.
foldl(HookName, Args, Acc) ->
    {ok, Hooks} = gaffs_cache:lookup(HookName),
    internal_foldl(Hooks, Args, Acc).

%%%===================================================================
%%% Internal functions
%%%===================================================================

internal_foldl([], _BaseArgs, Acc) ->
    {ok, Acc};
internal_foldl([#hook{module = Module, func = Fun, arity = Arity} | T], BaseArgs, Acc) ->
    Args = BaseArgs ++ [Acc],
    try
        case apply(Module, Fun, Args) of
            stop -> {stop, Acc, {Module, Fun}};
            {stop, NewAcc} -> {stop, NewAcc, {Module, Fun}};
            {stop, NewAcc, Reason} -> {stop, NewAcc, {Module, Fun}, Reason};
            {ok, NewAcc} -> internal_foldl(T, BaseArgs, NewAcc)
        end
    catch
        C:E ->
            Stacktrace = erlang:get_stacktrace(),
            error_logger:error_msg("Hook: ~p:~p/~p apply exception: ~p:~p~nArgs: ~p~nStacktrace: ~p",
                 [Module, Fun, Arity, C, E, Args, Stacktrace]),
            {error, {Module, Fun}, {C, E}}
    end.
