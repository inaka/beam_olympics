-module(bo_stats_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([ initial_stats/1
        , players_playing/1
        ]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [initial_stats, players_playing].

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
  _ = sumo:delete_all(players),
  {ok, Client} = bo_test_client:start(stats_suite),
  [{client, Client} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  ok = bo_test_client:stop(Client),
  _ = sumo:delete_all(players),
  application:unset_env(beam_olympics, all_tasks),
  ok = bo:stop(),
  Config.

-spec initial_stats(config()) -> {comment, string()}.
initial_stats(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("Initial stats include no player"),
  #{ tasks := 3
   , players := []
   } = bo_test_client:stats(Client),

  {comment, ""}.

-spec players_playing(config()) -> {comment, string()}.
players_playing(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, #{name := FirstTask}} = bo_test_client:signup(Client, <<"pp1">>),
  {ok, #{name := FirstTask}} = bo_test_client:signup(Client, <<"pp2">>),

  ct:comment("All players in first task"),
  #{ tasks := 3
   , players := [ #{ name := <<$p, $p, _>>
                   , done := 0
                   , score := 0
                   }
                , #{ name := <<$p, $p, _>>
                   , done := 0
                   , score := 0
                   }
                ]
   } = bo_test_client:stats(Client),

  ct:comment("After solving first task, the player changes"),
  {ok, _} = bo_test_client:submit(Client, <<"pp2">>, fun id/1),
  SolveScore = bo_task:score(FirstTask),
  #{ tasks := 3
   , players := [ #{ name := <<"pp2">>
                   , done := 1
                   , score := SolveScore
                   }
                , #{ name := <<"pp1">>
                   , done := 0
                   , score := 0
                   }
                ]
   } = bo_test_client:stats(Client),

  ct:comment("After skipping task, the player changes"),
  {ok, _} = bo_test_client:skip(Client, <<"pp1">>),
  SkipScore = round(-0.5 * SolveScore),
  #{ tasks := 3
   , players := [ #{ name := <<"pp2">>
                   , done := 1
                   , score := SolveScore
                   }
                , #{ name := <<"pp1">>
                   , done := 1
                   , score := SkipScore
                   }
                ]
   } = bo_test_client:stats(Client),

  {comment, ""}.

id(X) ->
  ct:log("id evaluated for ~p", [X]),
  X.
