-module(simple_task1).

-behaviour(bo_task).

-export([ description/0
        , spec/0
        , score/0
        , timeout/0
        , tests/0
        , solution/1
        ]).

-spec description() -> binary().
description() -> <<"Always return 1">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"_">>], output => <<"1">>}.

-spec score() -> 100.
score() -> 100.

-spec timeout() -> 1000.
timeout() -> 1000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Input) || Input <- [a, 1, 2.14, #{}]].

build_test(Something) ->
  fun(Fun) ->
    case Fun(Something) of
      1 -> ok;
      SomethingElse -> {error, #{ input => Something
                                , output => SomethingElse
                                , expected => 1
                                }}
    end
  end.

-spec solution(_) -> 1.
solution(_) -> 1.
