-module(bo_hooks_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([hooks/1]).

%% Hooks
-export([start/0, skipped1/1, skipped2/1]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [hooks].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  case net_kernel:start(['bo_test@127.0.0.1']) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    {error, Error} -> throw(Error)
  end,
  ok = application:set_env(beam_olympics, hooks, get_hooks()),
  {ok, _} = bo:start(),
  _ = sumo:delete_all(bo_players),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:unset_env(beam_olympics, hooks),
  _ = sumo:delete_all(bo_players),
  ok = bo:stop(),
  Config.

-spec hooks(config()) -> {comment, string()}.
hooks(_Config) ->
  {ok, Client} = bo_test_client:start(test_user),

  try
    {ok, Task1} = bo_test_client:signup(Client, <<"userName">>),
    {ok, Task1} = bo_test_client:task(Client, <<"userName">>),
    {ok, _Task2} = bo_test_client:skip(Client, <<"userName">>),

    EtsContents = lists:sort(ets:tab2list(hooks_SUITE_ets)),
    [{skipped1, <<"userName">>}, {skipped2, <<"userName">>}] = EtsContents,

    ok
  after
    _ = ets:delete(hooks_SUITE_ets),
    ok = bo_test_client:stop(Client)
  end,

  {comment, ""}.

%% Hooks
-spec start() -> any().
start() ->
  _ = ets:new(hooks_SUITE_ets, [set, public, named_table]),
  % The table will be destroyed after 500ms no matter what
  timer:sleep(500).

-spec skipped1(any()) -> true.
skipped1(PlayerName) ->
  ets:insert(hooks_SUITE_ets, {skipped1, PlayerName}).

-spec skipped2(any()) -> true.
skipped2(PlayerName) ->
  ets:insert(hooks_SUITE_ets, {skipped2, PlayerName}).

%% Utils
get_hooks() ->
  #{ app_started         => {?MODULE, start}
   , player_skipped_task => [{?MODULE, skipped1}, {?MODULE, skipped2}]
   }.