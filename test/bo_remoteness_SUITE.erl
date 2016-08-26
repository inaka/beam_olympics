-module(bo_remoteness_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([run/1, crash/1]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [run, crash].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  case net_kernel:start(['bo_test@127.0.0.1']) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    {error, Error} -> throw(Error)
  end,
  _ = application:load(beam_olympics),
  application:set_env(beam_olympics, all_tasks, [bo_test_node_task]),
  {ok, _} = bo:start(),
  _ = sumo:delete_all(players),
  {ok, Client} = bo_test_client:start(bo_remoteness),
  [{client, Client} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  ok = bo_test_client:stop(Client),
  _ = sumo:delete_all(players),
  application:unset_env(beam_olympics, all_tasks),
  ok = bo:stop(),
  Config.

-spec run(config()) -> {comment, string()}.
run(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, #{name := bo_test_node_task}} =
    bo_test_client:signup(Client, <<"remoteness">>),

  ct:comment("This node is not correct"),
  Node = node(),
  WrongFun = fun() -> Node end,
  {failures, [_|_]} = bo_test_client:submit(Client, <<"remoteness">>, WrongFun),

  ct:comment("Computing the node works, because test is run on client node"),
  RightFun = fun() -> node() end,
  the_end = bo_test_client:submit(Client, <<"remoteness">>, RightFun),

  {comment, ""}.

-spec crash(config()) -> {comment, string()}.
crash(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  {ok, #{name := bo_test_node_task}} =
    bo_test_client:signup(Client, <<"crash">>),

  ct:comment("Crashing crashes only the client node"),
  CrashFun =
    fun() -> erlang:apply(erlang, halt, ["Crashed by Remoteness Test"]) end,
  try bo_test_client:submit(Client, <<"crash">>, CrashFun) of
    R -> ct:fail("Unexpected result ~p", [R])
  catch
    _:{timeout, Client, _} -> ok
  end,

  {comment, ""}.
