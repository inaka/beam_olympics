-module(bo_permutation_sum).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Permusum: Write a function that receives an integer and"
                    " returns the sum of all the possible permutations of its"
                    " digits.">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"pos_integer()">>], output => <<"pos_integer()">>}.

-spec score() -> 150.
score() -> 150.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- cases()].

build_test({Number, Sum}) ->
  fun(Fun) ->
    try Fun(Number) of
      Sum -> ok;
      Bad -> {error, #{ input => Number
                      , output => Bad
                      , expected => Sum
                      }}
    catch
      _:Error ->
        {error, #{ input => Number
                 , output => Error
                 , stack => erlang:get_stacktrace()
                 , expected => <<"Not an error, that's for sure ;)">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
cases() ->
  [{0, 0}, {1, 1}, {12, 33}, {22, 22}, {122, 555}, {123, 1332}, {140, 1110},
   {1400, 16665}, {123456789, 201599999798400}].
