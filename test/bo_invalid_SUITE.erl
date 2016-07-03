-module(bo_invalid_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([ invalid_input/1
        , invalid_user/1
        , test_timeout/1
        , test_fails/1
        , test_error/1
        ]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [invalid_input, invalid_user, test_timeout, test_fails, test_error].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  case net_kernel:start(['bo_test@127.0.0.1']) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    {error, Error} -> throw(Error)
  end,
  {ok, _} = bo:start(),
  _ = sumo:delete_all(bo_players),
  {ok, Client} = bo_test_client:start(invalid_suite),
  {ok, _Task} = bo_test_client:signup(Client, player()),
  [{client, Client} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),
  ok = bo_test_client:stop(Client),
  _ = sumo:delete_all(bo_players),
  ok = bo:stop(),
  Config.

-spec invalid_input(config()) -> {comment, string()}.
invalid_input(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("Providing something that's not a fun fails"),
  {error, invalid} = submit(Client, not_a_fun),

  ct:comment("Providing a fun with the wrong arity fails"),
  {error, invalid} = submit(Client, fun() -> missing_args end),

  {comment, ""}.

-spec invalid_user(config()) -> {comment, string()}.
invalid_user(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("Providing a wrong user fails"),
  {error, notfound} = bo_test_client:submit(Client, <<"wrong">>, solution),
  {error, notfound} = bo_test_client:score(Client, <<"wrong">>),
  {error, notfound} = bo_test_client:skip(Client, <<"wrong">>),

  ct:comment("Providing a wrong node is forbidden"),
  {ok, Client2} = bo_test_client:start(invalid_node),

  {error, forbidden} =
    try
      {error, forbidden} = bo_test_client:submit(Client2, player(), solution),
      {error, forbidden} = bo_test_client:score(Client2, player()),
      bo_test_client:skip(Client2, player())
    after
      bo_test_client:stop(Client2)
    end,

  {comment, ""}.

-spec test_timeout(config()) -> {comment, string()}.
test_timeout(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("Providing a function that takes more than expected times out"),
  {error, timeout} = submit(Client, fun(X) -> receive X -> ok end end),

  {comment, ""}.

-spec test_fails(config()) -> {comment, string()}.
test_fails(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("Providing a function that fails the tests returns errors"),
  {failures, [_|_]} = submit(Client, fun(X) -> {'not', X} end),

  {comment, ""}.

-spec test_error(config()) -> {comment, string()}.
test_error(Config) ->
  {client, Client} = lists:keyfind(client, 1, Config),

  ct:comment("Providing a function that errors the tests returns errors"),
  {failures, Failures} = submit(Client, fun(X) -> X / 2 end),
  [_|_] = [Err || #{error := Err, stack := _} <- Failures],

  {comment, ""}.

player() -> <<"invalid_suite">>.
submit(Client, Solution) -> bo_test_client:submit(Client, player(), Solution).
