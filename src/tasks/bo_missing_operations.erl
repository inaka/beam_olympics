-module(bo_missing_operations).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Missing operations: You are tasked with finding the correct"
                   " combination of operators to solve a simple problem. Create"
                   " a function that receives a list of integers and a solution"
                   " (another integer). Return a list of operators (the '+' and"
                   " '-' atoms) that will solve the equation.\n"
                   "For example, given ([2, 3, 1], 4) return ['+', '-'] because"
                   " 2 + 3 - 1 = 4. If there's no valid list of operators, just"
                   " return the atom 'notfound'.">>.

-spec spec() -> bo_task:spec().
spec() ->
  #{ input => [<<"[pos_integer()]">>, <<"pos_integer()">>]
   , output => <<"notfound | ['+', '-']">>
   }.

-spec score() -> 200.
score() -> 200.

-spec timeout() -> 1000.
timeout() -> 1000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- cases()].

build_test({Operands, Solution}) ->
  fun(Fun) ->
    try Fun(Operands, Solution) of
      notfound -> check_impossible(Operands, Solution);
      Answer -> check_answer(Operands, Solution, Answer)
    catch
      _:Error ->
        {error, #{ input => [Operands, Solution]
                 , output => Error
                 , expected => <<"Not an error, that's for sure.">>}
                 }
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
cases() ->
  [make_case(N) || N <- lists:seq(1, 8)].

make_case(Index) ->
  L = [1 + rand:uniform(256) || _ <- lists:seq(1, 2 * Index)],
  case rand:uniform() < 0.5 of
    true ->
      Operators = [case rand:uniform() < 0.5 of
                     true  -> '+';
                     false -> '-'
                   end || _ <- lists:seq(1, length(L) - 1)],
      {L, test_result(['+' | Operators], L, 0)};
    false ->
      {L, rand:uniform(256)}
  end.

check_answer(Operands, Solution, Answer) ->
  case test_result(['+' | Answer], Operands, 0) of
    bad_list ->
      {error, #{ input => [Operands, Solution]
               , output => Answer
               , expected => <<"A list of the right length.">>
               }};
    bad_operator ->
      {error, #{ input => [Operands, Solution]
               , output => Answer
               , expected => <<"A list with the right operators.">>
               }};
    Solution ->
      ok
  end.

test_result(['+' | Operators], [H | T], Acc) ->
  test_result(Operators, T, Acc + H);
test_result(['-' | Operators], [H | T], Acc) ->
  test_result(Operators, T, Acc - H);
test_result([_BadOperator | _Operations], _Operands, _Acc) ->
  bad_operator;
test_result([], [], Acc) ->
  Acc;
test_result([], _Operands, _Acc) ->
  bad_list;
test_result(_Operators, [], _Acc) ->
  bad_list.

check_impossible(Operands, Solution) ->
  Combinations = get_operator_combinations(length(Operands) - 1),
  case lists:filter(fun(C) ->
                      Solution =:= test_result(['+' | C], Operands, 0)
                    end, Combinations) of
    [] -> ok;
    [_ | _] -> {error, #{ input => [Operands, Solution]
                        , output => notfound
                        , expected => <<"Come on, there's a solution.">>
                        }}
  end.

get_operator_combinations(1) ->
  [['+'], ['_']];
get_operator_combinations(N) ->
  C = get_operator_combinations(N - 1),
  [['+' | O] || O <- C] ++ [['-' | O] || O <- C].
