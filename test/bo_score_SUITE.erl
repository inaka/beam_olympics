-module(bo_score_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([ initial_score/1
        , solving_tasks_adds/1
        , skipping_tasks_removes/1
        ]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [initial_score, solving_tasks_adds, skipping_tasks_removes].

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
  {ok, Client} = bo_test_client:start(score_suite),
  [{client, Client} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  ok = bo_test_client:stop(Client),
  _ = sumo:delete_all(bo_players),
  application:unset_env(beam_olympics, all_tasks),
  ok = bo:stop(),
  Config.

-spec initial_score(config()) -> {comment, string()}.
initial_score(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, _} = bo_test_client:signup(Client, <<"initial_score">>),

  ct:comment("The initial score is 0"),
  {ok, 0} = bo_test_client:score(Client, <<"initial_score">>),

  {comment, ""}.

-spec solving_tasks_adds(config()) -> {comment, string()}.
solving_tasks_adds(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, #{name := FirstTask}} = bo_test_client:signup(Client, <<"sta">>),

  ct:comment("After solving first task, the score increases"),
  {ok, #{name := NextTask}} =
    bo_test_client:submit(Client, <<"sta">>, fun id/1),
  {ok, FirstScore} = bo_test_client:score(Client, <<"sta">>),
  FirstScore = FirstTask:score(),

  ct:comment("After solving next task, the score increases"),
  {ok, #{name := FinalTask}} =
    bo_test_client:submit(Client, <<"sta">>, fun NextTask:solution/1),
  {ok, NextScore} = bo_test_client:score(Client, <<"sta">>),
  NextScore = NextTask:score() + FirstScore,

  ct:comment("Failed attempts don't alter the score"),
  _ = bo_test_client:submit(Client, <<"sta">>, fun(_) -> wrong end),
  _ = bo_test_client:submit(Client, <<"sta">>, wrong),
  {ok, NextScore} = bo_test_client:score(Client, <<"sta">>),

  ct:comment("After solving last task, the score increases"),
  the_end = bo_test_client:submit(Client, <<"sta">>, fun FinalTask:solution/1),
  {ok, FinalScore} = bo_test_client:score(Client, <<"sta">>),
  FinalScore = FinalTask:score() + NextScore,

  ct:comment("Failed attempts don't alter the score"),
  _ = bo_test_client:submit(Client, <<"sta">>, fun(_) -> wrong end),
  _ = bo_test_client:submit(Client, <<"sta">>, wrong),
  {ok, FinalScore} = bo_test_client:score(Client, <<"sta">>),

  {comment, ""}.

-spec skipping_tasks_removes(config()) -> {comment, string()}.
skipping_tasks_removes(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, #{name := FirstTask}} = bo_test_client:signup(Client, <<"str">>),

  ct:comment("After solving first task, the score increases"),
  {ok, #{name := NextTask}} = bo_test_client:skip(Client, <<"str">>),
  {ok, FirstScore} = bo_test_client:score(Client, <<"str">>),
  {FirstScore, FirstScore} = {FirstScore, 0 - round(0.5 * FirstTask:score())},

  ct:comment("After solving next task, the score increases"),
  {ok, #{name := FinalTask}} = bo_test_client:skip(Client, <<"str">>),
  {ok, NextScore} = bo_test_client:score(Client, <<"str">>),
  NextScore = FirstScore - round(0.5 * NextTask:score()),

  ct:comment("After solving last task, the score increases"),
  the_end = bo_test_client:skip(Client, <<"str">>),
  {ok, FinalScore} = bo_test_client:score(Client, <<"str">>),
  FinalScore = NextScore - round(0.5 * FinalTask:score()),

  ct:comment("Failed attempts don't alter the score"),
  _ = bo_test_client:skip(Client, <<"str">>),
  {ok, FinalScore} = bo_test_client:score(Client, <<"str">>),

  {comment, ""}.

id(X) ->
  ct:log("id evaluated for ~p", [X]),
  X.
