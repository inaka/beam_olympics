-module(bo_tuple_counter).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"TupleStacking: How many tuples can you find in the list?">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"term()">>], output => <<"non_neg_integer()">>}.

-spec score() -> 100.
score() -> 100.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- cases()].

build_test(Case) ->
  fun(Fun) ->
    ProperAnswer = answer(Case),
    try Fun(Case) of
      ProperAnswer -> ok;
      BadAnswer    -> {error, #{ input => Case
                               , output => BadAnswer
                               , expected => ProperAnswer
                               }}
    catch
      _:Error ->
        {error, #{ input => Case
                 , error => Error
                 , stack => erlang:get_stacktrace()
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
cases() ->
  [make_cases(0), make_cases(5), make_cases(32), make_cases(100)].

make_cases(Length) ->
  _ = rand:seed(exs1024),
  [make_case(100) || _ <- lists:seq(1, Length)].

make_case(P) ->
  case rand:uniform(P) of
    N when N =< 5 ->
      [];
    N when N =< 15 ->
      rand:uniform(300);
    N when N =< 30 ->
      list_to_binary([$a + rand:uniform($z - $a) ||
                      _ <- lists:seq(1, 5 + rand:uniform(12))]);
    N when N =< 70 ->
      list_to_tuple([make_case(P - 10) ||
                     _ <- lists:seq(1, 2 + rand:uniform(6))]);
    _ ->
      [make_case(P - 15) ||
       _ <- lists:seq(1, 2 + rand:uniform(10))]
  end.

answer(L) when is_list(L) ->
  lists:sum([answer(E) || E <- L]);
answer(T) when is_tuple(T) ->
  1 + answer(tuple_to_list(T));
answer(_Other) ->
  0.
