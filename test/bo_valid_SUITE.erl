-module(bo_valid_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([ next_task/1
        , next_task_is_random/1
        , no_more_tasks/1
        ]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [next_task, next_task_is_random, no_more_tasks].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  case net_kernel:start(['bo_test@127.0.0.1']) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    {error, Error} -> throw(Error)
  end,
  _ = application:load(beam_olympics),
  application:set_env(
    beam_olympics, all_tasks, [bo_first_task, simple_task1, simple_task2]),
  {ok, _} = bo:start(),
  _ = sumo:delete_all(bo_players),
  {ok, Client} = bo_test_client:start(invalid_suite),
  [{client, Client} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  ok = bo_test_client:stop(Client),
  _ = sumo:delete_all(bo_players),
  application:unset_env(beam_olympics, all_tasks),
  ok = bo:stop(),
  Config.

-spec next_task(config()) -> {comment, string()}.
next_task(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, FirstTask} = bo_test_client:signup(Client, <<"next_task">>),

  ct:comment("The initial task can be solved"),
  #{name := bo_first_task} = FirstTask,
  {ok, NextTask} = bo_test_client:submit(Client, <<"next_task">>, fun id/1),

  ct:comment("A new task is provided"),
  case NextTask of
    FirstTask -> ct:fail("Different task expected");
    NextTask -> ok
  end,

  {comment, ""}.

-spec next_task_is_random(config()) -> {comment, string()}.
next_task_is_random(Config) ->
  Players = [<<Char>> || Char <- lists:seq($a, $z)],
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("The initial task is the same for all players"),
  [{ok, FirstTask}] =
    lists:usort([bo_test_client:signup(Client, Player) || Player <- Players]),
  #{name := bo_first_task} = FirstTask,

  ct:comment("Every player can solve that task"),
  [{ok, NextTask1}, {ok, NextTask2}] =
    lists:usort(
      [bo_test_client:submit(Client, Player, fun id/1) || Player <- Players]),

  ct:comment("Tasks come from the list"),
  case {NextTask1, NextTask2} of
    {#{name := simple_task1}, #{name := simple_task2}} -> ok;
    {#{name := simple_task2}, #{name := simple_task1}} -> ok;
    {NextTask1, NextTask2} ->
      ct:fail("Unexpected tasks: ~p", [{NextTask1, NextTask2}])
  end,

  {comment, ""}.

-spec no_more_tasks(config()) -> {comment, string()}.
no_more_tasks(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, FirstTask} = bo_test_client:signup(Client, <<"nmt">>),

  ct:comment("The initial task can be solved"),
  #{name := bo_first_task} = FirstTask,
  {ok, #{name := NextTask}} =
    bo_test_client:submit(Client, <<"nmt">>, fun id/1),

  ct:comment("The next task can be solved"),
  ExpectedTask =
    case NextTask of
      simple_task1 -> simple_task2;
      simple_task2 -> simple_task1
    end,
  {ok, #{name := ExpectedTask}} =
    bo_test_client:submit(Client, <<"nmt">>, fun NextTask:solution/1),

  ct:comment("Solving the final task user gets the end message"),
  the_end =
    bo_test_client:submit(Client, <<"nmt">>, fun ExpectedTask:solution/1),

  ct:comment("Once finished, the player task is undefined"),
  {error, ended} = bo_test_client:task(Client, <<"nmt">>),

  ct:comment("Once finished, the player can't submit new solutions"),
  {error, ended} = bo_test_client:submit(Client, <<"nmt">>, fun id/1),

  {comment, ""}.

id(X) ->
  ct:log("id evaluated for ~p", [X]),
  X.
