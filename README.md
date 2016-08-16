
# gaffs - Yet another generic Erlang hooks application #

Copyright (c) 2016 Led.

__Version:__ 0.1.0

# gaffs

`gaffs` is a generic Hooks system for Erlang applications. It allows you to
augment your application by adding hooks to your application aka
[Hooking](https://en.wikipedia.org/wiki/Hooking).

`gaffs` based on ideas of [hooks](https://github.com/barrel-db/hooks.git).

## Basic Usage of API

### Start gaffs app.
```
ok = gaffs:start().
```
### Save hook with HookName like Fun and default Priority = 0.
```
ok = gaffs:register(Module, Fun, Arity).
ok = gaffs:register(Fun, Module, Fun, Arity). %% this is the same
```
### Save hook with HookName and specific Priority.
```
ok = gaffs:register(HookName, Module, Fun, Arity, Priority).
```
### Run all registered hooks for HookName with Args, ignore exceptions. Return list of {MFA, Res}.
```
ListOfResults = gaffs:all(HookName, Args).
```
### Run all registered hooks ordered by priority for HookName with Args++[Acc]. Return accumulated result.
```
{ok, NewAcc} = gaffs:foldl(HookName, Args, Acc).
```
### Foldl can be stopped if hook result will be stop | {stop, Acc} | {stop, Acc, Reason}.
```
{stop, Acc, {Module, Fun}} = gaffs:foldl(HookName, Args, Acc).
```
### In application environment settings can be setted mandatory_hooks. Gaffs application won't start without their registration. Example:
```
{application, 'gaffs',
 [{description, ""},
  {vsn, "0.1.0"},

  ...

  {env,[
    {mandatory_hooks
        [
            {hook_name,
              [
                {module, fun, arity},
                {module, fun, arity, priority}
              ]}
        ]},
    ...
  ]},

  ...

 ]}.
```