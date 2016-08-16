%%% Gen_server module.
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
-module(gaffs_srv).
-behaviour(gen_server).

-include("gaffs.hrl").
-define(SERVER, ?MODULE).

-record(state, {
        pids = []
    }).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    start_link/0,
    monitor/4,
    insert_hook/3,
    mregister/2,
    delete_hook/3
    ]).
%%------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor(Pid, HookName, MFA, Priority) ->
    gen_server:call(?SERVER, {monitor, Pid, HookName, MFA, Priority}).

insert_hook(Table, HookName, NewHook) ->
    gen_server:call(?SERVER, {insert_hook, Table, HookName, NewHook}).

delete_hook(Table, HookName, NewHook) ->
    gen_server:call(?SERVER, {delete_hook, Table, HookName, NewHook}).

mregister(Table, Hooks) ->
    gen_server:call(?SERVER, {mregister, Table, Hooks}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Pid = self(),
    spawn(fun() -> init_hooks(Pid) end),
    {ok, #state{}}.

handle_call({monitor, Pid, HookName, MFA, Priority}, _From, State = #state{pids = Pids}) ->
    erlang:monitor(process, Pid),
    {reply, ok, State#state{pids = [{Pid, {HookName, MFA, Priority}} | Pids]}};

handle_call({insert_hook, Table, HookName, NewHook}, _From, State) ->
    Hooks = case ets:lookup(Table, HookName) of
        [{_HookName, Value}] -> Value;
        [] -> []
    end,
    NewHooks = lists:keysort(#hook.priority, lists:usort([NewHook | Hooks])),
    ets:insert(Table, [{HookName, NewHooks}]),
    {reply, ok, State};

handle_call({delete_hook, Table, HookName, Hook}, _From, State) ->
    Hooks = case ets:lookup(Table, HookName) of
        [{_HookName, Value}] -> Value;
        [] -> []
    end,
    NewHooks = lists:delete(Hook, Hooks),
    ets:insert(Table, [{HookName, NewHooks}]),
    {reply, ok, State};

handle_call({mregister, Table, Hooks}, _From, State) ->
    F = fun({_HookName, {error, _}} = Res) ->
                Res;
            ({HookName, CheckedHooks}) ->
                OldHooks = case ets:lookup(Table, HookName) of
                    [{_HookName, Value}] -> Value;
                    [] -> []
                end,
                NewHooks = lists:keysort(#hook.priority, lists:usort(CheckedHooks ++ OldHooks)),
                ets:insert(Table, [{HookName, NewHooks}]),
                {HookName, ok}
        end,
    {reply, lists:map(F, Hooks), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, _, Pid, _Reason}, State = #state{pids = Pids}) ->
    proc_lib:spawn(fun() ->
        try
            {_, {HookName, MFA, Priority}} = lists:keyfind(Pid, 1, Pids),
            gaffs_cache:delete(HookName, MFA, Priority)
        catch _:_ -> ok
        end
    end),
    {noreply, State#state{pids = lists:keydelete(Pid, 1, Pids)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_env(Application, Par, Def) ->
    case application:get_env(Application, Par) of
        undefined -> Def;
        {ok, Res} -> Res
    end.

init_hooks(Pid) ->
    try
        Hooks = get_env(gaffs, mandatory_hooks, []),
        lists:foreach(
            fun({_HookName, ok}) ->
                    ok;
                ({HookName, {error, Reason}}) ->
                    error_logger:error_msg("Mandatory hook: ~p register error: ~p~n", [HookName, Reason]),
                    throw(Reason)
        end, gaffs:mregister(Hooks))
    catch
        throw:_ ->
            exit(Pid, kill);
        C:E ->
            Stacktrace = erlang:get_stacktrace(),
            error_logger:error_msg("Mandatory hooks registration exception: ~p:~p~nStacktrace: ~p~n", [C, E, Stacktrace]),
            exit(Pid, kill)
    end.
