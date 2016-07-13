-module(bo_player_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([register_ok/1, wrong_node/1, duplicated_username/1, wrong_username/1]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [register_ok, duplicated_username, wrong_node, wrong_username].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  case net_kernel:start(['bo_test@127.0.0.1']) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    {error, Error} -> throw(Error)
  end,
  {ok, _} = bo:start(),
  _ = sumo:delete_all(bo_players),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  _ = sumo:delete_all(bo_players),
  ok = bo:stop(),
  Config.

-spec register_ok(config()) -> {comment, string()}.
register_ok(_Config) ->
  {ok, Client} = bo_test_client:start(register_ok),

  try
    ct:comment("User can be created"),
    {ok, Task} = bo_test_client:signup(Client, <<"ok">>),

    ct:comment("User can retrieve current task"),
    {ok, Task} = bo_test_client:task(Client, <<"ok">>),

    ok
  after
    ok = bo_test_client:stop(Client)
  end,

  {comment, ""}.

-spec wrong_node(config()) -> {comment, string()}.
wrong_node(_Config) ->
  {ok, Client1} = bo_test_client:start(wrong_node1),
  {ok, Client2} = bo_test_client:start(wrong_node2),

  try
    ct:comment("User ok is created in node1"),
    {ok, Task} = bo_test_client:signup(Client1, <<"wrong_node">>),

    ct:comment("User ok can retreive task"),
    {ok, Task} = bo_test_client:task(Client1, <<"wrong_node">>),

    ct:comment("User ok is forbidden to operate in node2"),
    {error, forbidden} = bo_test_client:task(Client2, <<"wrong_node">>),

    ct:comment("User wrong is not found in node1"),
    {error, notfound} = bo_test_client:task(Client2, <<"wrong">>),

    ct:comment("User wrong is not found in node2"),
    {error, notfound} = bo_test_client:task(Client1, <<"wrong">>),

    ok
  after
    ok = bo_test_client:stop(Client1),
    ok = bo_test_client:stop(Client2)
  end,

  {comment, ""}.

-spec duplicated_username(config()) -> {comment, string()}.
duplicated_username(_Config) ->
  {ok, Client1} = bo_test_client:start(duplicated_username1),
  {ok, Client2} = bo_test_client:start(duplicated_username2),

  try
    ct:comment("User dup is created in node1"),
    {ok, _Task} = bo_test_client:signup(Client1, <<"dup">>),

    ct:comment("User dup can't be created in node1"),
    {error, conflict} = bo_test_client:signup(Client1, <<"dup">>),

    ct:comment("User dup can't be created in node2"),
    {error, conflict} = bo_test_client:signup(Client2, <<"dup">>),

    ok
  after
    ok = bo_test_client:stop(Client1),
    ok = bo_test_client:stop(Client2)
  end,

  {comment, ""}.

-spec wrong_username(config()) -> {comment, string()}.
wrong_username(_Config) ->
  {ok, Client1} = bo_test_client:start(duplicated_username1),
  {ok, Client2} = bo_test_client:start(duplicated_username2),

  try
    ct:comment("User with invalid name can't be created"),
    {error, invalid} = bo_test_client:signup(Client1, not_a_binary),

    ok
  after
    ok = bo_test_client:stop(Client1),
    ok = bo_test_client:stop(Client2)
  end,

  {comment, ""}.
