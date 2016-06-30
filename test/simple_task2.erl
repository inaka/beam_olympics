-module(simple_task2).

-behaviour(bo_task).

-export([description/0, expected_arity/0, timeout/0, tests/0, solution/1]).

-spec description() -> binary().
description() -> <<"Echo: Always return 2">>.

-spec expected_arity() -> 1.
expected_arity() -> 1.

-spec timeout() -> 1000.
timeout() -> 1000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Input) || Input <- [a, 1, 2.14, #{}]].

build_test(Something) ->
  fun(Fun) ->
    case Fun(Something) of
      2 -> ok;
      SomethingElse -> {error, #{ input => Something
                                , output => SomethingElse
                                , expected => 2
                                }}
    end
  end.

-spec solution(_) -> 2.
solution(_) -> 2.
