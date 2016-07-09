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
                   " return an empty list.">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"X">>], output => <<"X">>}.

-spec score() -> 10.
score() -> 10.

-spec timeout() -> 1000.
timeout() -> 1000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- cases()].

build_test(Case = {Operands, Solution}) ->
  fun(Fun) ->
    try erlang:apply(Fun, [Operands, Solution]) of
      []     -> case answer(Operands, Solution) of
                  [] -> ok;
                  _  -> {error, #{ input => Case
                                 , output => []
                                 , expected => "Come on, there's a solution."}}
                end;
      Answer -> case test_result(['+' | Answer], Operands, 0) of
                  bad_list ->
                    {error, #{ input => Case
                             , output => Answer
                             , expected => "A list of the right length."}};
                  bad_operator ->
                    {error, #{ input => Case
                             , output => Answer
                             , expected => "A list with the right operators."}};
                  Solution ->
                    ok
                end
    catch
      _:Error ->
        {error, #{ input => Case
                 , output => Error
                 , expected => "Not an error, that's for sure."}}
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

answer(Operands, Solution) ->
  Combinations = get_operator_combinations(length(Operands) - 1),
  case lists:filter(fun(C) ->
                      Solution =:= test_result(['+' | C], Operands, 0)
                    end, Combinations) of
    [] ->
      [];
    [H | _] ->
      H
  end.

get_operator_combinations(1) ->
  [['+'], ['_']];
get_operator_combinations(N) ->
  C = get_operator_combinations(N - 1),
  [['+' | O] || O <- C] ++ [['-' | O] || O <- C].