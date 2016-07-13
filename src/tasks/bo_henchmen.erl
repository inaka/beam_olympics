-module(bo_henchmen).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

-spec description() -> binary().
description() ->
  <<"Henchmen: Provide a function that spawns a process\n"
    "The process should wait for messages\n"
    "When it receives a pid, even if its own one, it should kill that process\n"
    "but only if the victim lives in the same node as him"
    >>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [], output => <<"pid()">>}.

-spec score() -> 100.
score() -> 100.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() ->
  [ fun different_pids/1
  , fun kills_siblings/1
  , fun doesnt_kill_remotes/1
  , fun commits_suicide/1
  , fun doesnt_die_on_invalid_input/1
  ].

different_pids(Fun) ->
  Henchmen = [Fun() || _ <- lists:seq(1, 1000)],
  case length(lists:usort(Henchmen)) of
    1000 -> ok;
    _ -> {error, "Multiple executions of the function returned the same pid"}
  end.

kills_siblings(Fun) ->
  Bill = Fun(),
  Boby = Fun(),
  Dino = Fun(),
  Henchmen = [Bill, Boby, Dino],
  DeadProcs =
    fun(Pids) ->
      {dead_processes, [P || P <- Pids, not is_process_alive(P)]}
    end,

  try
    {dead_processes, []} = DeadProcs(Henchmen),

    Bill ! Boby,
    timer:sleep(250),
    {dead_processes, [Boby]} = DeadProcs(Henchmen),

    Dino ! Boby,
    timer:sleep(250),
    {dead_processes, [Boby]} = DeadProcs(Henchmen),

    Dino ! Bill,
    timer:sleep(250),
    {still_alive, [Dino]} =
      {still_alive, [P || P <- Henchmen, is_process_alive(P)]},

    ok
  catch
    _:Error -> {error, #{ test  => ?FUNCTION_NAME
                        , error => Error
                        , stack => erlang:get_stacktrace()
                        }}
  after
    _ = exit(Bill, kill),
    _ = exit(Boby, kill),
    _ = exit(Dino, kill)
  end.

doesnt_kill_remotes(Fun) ->
  Harry = Fun(),
  Node = 'olympics@192.168.3.6',
  Victim = spawn(Node, fun() -> receive x -> ok end end),
  try
    true = is_process_alive(Harry),
    true = rpc:pinfo(Victim) /= undefined,

    Harry ! Victim,
    timer:sleep(250),
    true = is_process_alive(Harry),
    true = rpc:pinfo(Victim) /= undefined,

    exit(Victim, kill),
    Harry ! Victim,
    timer:sleep(250),
    true = is_process_alive(Harry),

    ok
  catch
    _:Error -> {error, #{ test  => ?FUNCTION_NAME
                        , error => Error
                        , stack => erlang:get_stacktrace()
                        }}
  after
    _ = exit(Harry, kill),
    _ = exit(Victim, kill)
  end.

commits_suicide(Fun) ->
  Kamikaze = Fun(),
  try
    true = is_process_alive(Kamikaze),

    Kamikaze ! Kamikaze,
    timer:sleep(250),
    false = is_process_alive(Kamikaze),

    ok
  catch
    _:Error -> {error, #{ test  => ?FUNCTION_NAME
                        , error => Error
                        , stack => erlang:get_stacktrace()
                        }}
  after
    _ = exit(Kamikaze, kill)
  end.

doesnt_die_on_invalid_input(Fun) ->
  Larry = Fun(),
  try
    true = is_process_alive(Larry),

    Larry ! hello,
    timer:sleep(250),
    true = is_process_alive(Larry),

    Larry ! "is it me you're looking for?",
    timer:sleep(250),
    true = is_process_alive(Larry),

    ok
  catch
    _:Error -> {error, #{ test  => ?FUNCTION_NAME
                        , error => Error
                        , stack => erlang:get_stacktrace()
                        }}
  after
    _ = exit(Larry, kill)
  end.
