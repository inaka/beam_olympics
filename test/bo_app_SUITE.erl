-module(bo_app_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([run/1]).

-spec all() -> [atom()].
all() -> [run].

-spec run(proplists:proplist()) -> {comment, []}.
run(_Config) ->
  {ok, Apps} = bo:start(),
  [beam_olympics|_] = lists:reverse(Apps),
  ok = bo:stop(),
  {comment, ""}.
