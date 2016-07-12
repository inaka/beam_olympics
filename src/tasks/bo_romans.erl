-module(bo_romans).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

-spec description() -> binary().
description() ->
  <<"Roman Numerals: Parse Roman Numerals as binaries and convert them into"
    " integers.\n"
    "Provided function is expected to fail on invalid input."
    >>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"binary()">>], output => <<"pos_integer()">>}.

-spec score() -> 150.
score() -> 150.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Input) || Input <- test_inputs()].

test_inputs() ->
  [ {<<"">>,              fail}
  , {<<"WAT">>,           fail}
  , {<<"I">>,             1}
  , {<<"II">>,            2}
  , {<<"III">>,           3}
  , {<<"IV">>,            4}
  , {<<"V">>,             5}
  , {<<"IX">>,            9}
  , {<<"X">>,             10}
  , {<<"XV">>,            15}
  , {<<"XXX">>,           30}
  , {<<"XL">>,            40}
  , {<<"L">>,             50}
  , {<<"XC">>,            90}
  , {<<"C">>,             100}
  , {<<"CC">>,            200}
  , {<<"D">>,             500}
  , {<<"M">>,             1000}
  , {<<"XIV">>,           14}
  , {<<"XIX">>,           19}
  , {<<"CMXLIX">>,        949}
  , {<<"MMMDCCCLXXVI">>,  3876}
  % , {<<"IIII">>,          fail}
  % , {<<"VV">>,            fail}
  % , {<<"IL">>,            fail}
  % , {<<"XM">>,            fail}
  ].

build_test({Roman, fail}) ->
  fun(Fun) ->
    try Fun(Roman) of
      Output -> {error, #{input => Roman, output => Output, expected => fail}}
    catch
      _:_ -> ok
    end
  end;
build_test({Roman, Decimal}) ->
  fun(Fun) ->
    try Fun(Roman) of
      Decimal -> ok;
      Output ->
        {error, #{input => Roman, output => Output, expected => Decimal}}
    catch
      _:Error ->
        {error, #{input => Roman, output => Error, expected => Decimal}}
    end
  end.
