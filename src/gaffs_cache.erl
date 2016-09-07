%%% Module for ets cache.
%%%
%%% @author O. Buchatskiy <buchatskiy2@gmail.com>
%%% @copyright 2016 O. Buchatskiy
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
%%% along with gaffs. If not, see <http://www.gnu.org/licenses/>.

%% @private
-module(gaffs_cache).

-export([
        start/0,
        insert/4,
        mregister/1,
        lookup/1,
        delete/3,
        stop/0
    ]).

-include("gaffs.hrl").

-define(TABLE, gaffs_ets).
%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Create ets table.
-spec start() -> TableName::atom().
start() ->
    ets:new(?TABLE, [public, named_table, set]).

%% @doc Save new hook for HookName.
-spec insert(HookName::any(), MFA::tuple(), Options::list(), Priority::integer()) -> ok | {error, binary()}.
insert(HookName, {_Module, _Fun, _Arity} = MFA, Options, Priority) ->
    case check_mfa(MFA) of
        ok ->
            gaffs_srv:insert_hook(?TABLE, HookName, get_hook(MFA, Priority, Options)),
            ok;
        {error, _} = E ->
            E
    end.

%% @doc Search for all hooks by HookName in ets, return list sorted by priority.
-spec mregister([{HookName::any(), [MFA::tuple() | #hook{}]}]) -> [{HookName::any(), ok | {error, Reason::atom()}}].
mregister(HooksWithName) ->
    F = fun({HookName, Hooks}) ->
        Res = try lists:map(
            fun(Hook) ->
                case get_hook_with_check(Hook) of
                    {ok, CheckedHook} -> CheckedHook;
                    {error, Reason} ->
                        error_logger:error_msg("Bad hook: ~p in mregister for hook_name: ~p error: ~p~n", [Hook, HookName, Reason]),
                        error(Reason)
                end
            end, Hooks)
        catch error:Reason -> {error, Reason}
        end,
        {HookName, Res}
    end,
    gaffs_srv:mregister(?TABLE, lists:map(F, HooksWithName)).

%% @doc Search for all hooks by HookName in ets, return list sorted by priority.
-spec lookup(HookName::any()) -> {ok, [#hook{}]}.
lookup(HookName) ->
    Hooks = case ets:lookup(?TABLE, HookName) of
        [{_HookName, List}] -> List;
        [] -> []
    end,
    {ok, Hooks}.

%% @doc Delete hook with same priority from ets.
-spec delete(HookName::any(), MFA::tuple(), Priority::integer()) -> ok.
delete(HookName, {_Module, _Fun, _Arity} = MFA, Priority) ->
    gaffs_srv:delete_hook(?TABLE, HookName, get_hook(MFA, Priority)),
    ok.

%% @doc Delete ets table.
-spec stop() -> true.
stop() ->
    ets:delete(?TABLE).


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_hook({Module, Fun, Arity}) ->
    get_hook({Module, Fun, Arity}, 0).

get_hook({Module, Fun, Arity}, Priority) ->
    get_hook({Module, Fun, Arity}, Priority, []).

get_hook({Module, Fun, Arity}, Priority, Options) ->
    #hook{module = Module, func = Fun, arity = Arity, options = Options, priority = Priority}.

get_hook_with_check({_Module, _Fun, _Arity} = MFA) ->
    get_hook_with_check(get_hook(MFA));
get_hook_with_check({Module, Fun, Arity, Options}) ->
    get_hook_with_check({Module, Fun, Arity, Options, 0});
get_hook_with_check({Module, Fun, Arity, Options, Priority}) ->
    MFA = {Module, Fun, Arity},
    get_hook_with_check(get_hook(MFA, Priority, Options));
get_hook_with_check(#hook{module = Module, func = Fun, arity = Arity} = Hook) ->
    case check_mfa({Module, Fun, Arity}) of
        ok -> {ok, Hook};
        {error, _Reason} = E -> E
    end;
get_hook_with_check(_) ->
    {error, bad_hook}.

check_mfa({Module, Fun, Arity}) ->
    try lists:member({Fun, Arity}, proplists:get_value(exports, Module:module_info(), [])) of
        false -> {error, no_func};
        true -> ok
    catch
        _:_ -> {error, no_module}
    end.
