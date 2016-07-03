-module(simple_task2).

-behaviour(bo_task).

-export([ description/0
        , spec/0
        , score/0
        , timeout/0
        , tests/0
        , solution/1
        ]).

-spec description() -> binary().
description() -> <<"Always return 2">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"_">>], output => <<"2">>}.

-spec score() -> 200.
score() -> 200.

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
