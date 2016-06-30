-module(bo_first_task).

-behaviour(bo_task).

-export([describe/0, test/1]).

-spec describe() -> bo_task:task().
describe() ->
  #{ name => ?MODULE
   , desc => <<"Echo: Return whatever you receive">>
   , test => fun test/1
   }.

-spec test(fun()) -> bo_task:result().
test(Fun) when not is_function(Fun, 1) -> {error, invalid_input};
test(Fun) ->
  Results =
    lists:filtermap(
      fun(Something) ->
        case Fun(Something) of
          Something -> false;
          SomethingElse ->
            {true, #{ input => Something
                    , output => SomethingElse
                    , expected => Something
                    }}
        end
      end, [a, 1, 2.14, #{}]),
  case Results of
    [] -> ok;
    Results -> {failures, Results}
  end.
