-module(bo_task_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([first_task/1, all_tasks/1]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [first_task, all_tasks].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  case net_kernel:start(['bo_test@127.0.0.1']) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    {error, Error} -> throw(Error)
  end,
  {ok, _} = bo:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = bo:stop(),
  Config.

-spec first_task(config()) -> {comment, string()}.
first_task(_Config) ->

  ct:comment("All players start with bo_first_task"),
  FirstTaskName = bo_tasks:first(),
  FirstTaskName = bo_players:task(bo_players:new(<<"x">>, node())),
  FirstTaskName = bo_players:task(bo_players:new(<<"u">>, node())),

  {ok, Client} = bo_test_client:start(first_task),

  try
    ct:comment("At signup, user gets the first task description as well"),
    {ok, FirstTask} = bo_test_client:signup(Client, <<"first">>),
    {ok, FirstTask} = bo_test_client:task(Client, <<"first">>),

    ct:comment("It's a proper task"),
    #{ name := FirstTaskName
     , desc := FirstTaskDesc
     } = FirstTask,

    ct:comment("Task name is a module that implements bo_task behaviour"),
    true = is_task(FirstTaskName),

    ct:comment("Desc is a non-empty binary"),
    <<_:1/binary, _/binary>> = FirstTaskDesc,

    ok
  after
    ok = bo_test_client:stop(Client)
  end,

  {comment, ""}.

-spec all_tasks(config()) -> {comment, string()}.
all_tasks(_Config) ->

  ct:comment("First task is included in all tasks"),
  All = bo_tasks:all(),
  First = bo_tasks:first(),
  true = lists:member(First, All),

  ct:comment("All tasks can are modules implementing bo_task behaviour"),
  true = lists:all(fun is_task/1, All),

  {comment, ""}.

is_task(TaskName) ->
  ModAttrs = TaskName:module_info(attributes),
  [yes] == [yes || {behaviour, Behaviours} <- ModAttrs, bo_task <- Behaviours].
