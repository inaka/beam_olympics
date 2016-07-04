-module(bo_test_node_task).

-behaviour(bo_task).

-export([ description/0
        , spec/0
        , score/0
        , timeout/0
        , tests/0
        ]).

-spec description() -> binary().
description() -> <<"Return the node you are on regardless of input">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [], output => <<"node()">>}.

-spec score() -> 100.
score() -> 100.

-spec timeout() -> 1000.
timeout() -> 1000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test()].

build_test() ->
  fun(Fun) ->
    Output = Fun(),
    case node() of
      Output -> ok;
      Expected -> {error, #{ output => Output
                           , expected => Expected
                           }}
    end
  end.
