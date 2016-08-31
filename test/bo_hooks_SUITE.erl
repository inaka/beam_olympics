-module(bo_hooks_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([hooks/1]).

%% Hooks
-export([start/0, advanced1/2, advanced2/2]).

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
  _ = sumo:delete_all(players),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:unset_env(beam_olympics, hooks),
  _ = sumo:delete_all(players),
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
    [ {advanced1, skip, _}
    , {advanced2, skip, _}
    ] = EtsContents,

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

-spec advanced1(any(), any()) -> true.
advanced1(Event, Player) ->
  ets:insert(hooks_SUITE_ets, {advanced1, Event, Player}).

-spec advanced2(any(), any()) -> true.
advanced2(Event, Player) ->
  ets:insert(hooks_SUITE_ets, {advanced2, Event, Player}).

%% Utils
get_hooks() ->
  #{ app_started => {?MODULE, start}
   , advanced    => [{?MODULE, advanced1}, {?MODULE, advanced2}]
   }.