-module(bo_task).

-type result() :: ok | {error, invalid | timeout} | {failures, [term()]}.

-type task() :: #{ name := module()
                 , desc := binary()
                 , time := pos_integer()
                 }.

-type test() :: fun((fun()) -> ok | {error, term()}).

-export_type([task/0, result/0, test/0]).

-callback description() -> binary().
-callback expected_arity() -> non_neg_integer().
-callback timeout() -> pos_integer().
-callback tests() -> [test()].

-export([describe/1, test/2]).

-spec describe(module()) -> bo_task:task().
describe(Task) ->
  #{ name => Task
   , desc => Task:description()
   , time => Task:timeout()
   }.

-spec test(module(), fun()) -> result().
test(Task, Fun) ->
  Arity = Task:expected_arity(),
  case is_function(Fun, Arity) of
    false -> {error, invalid};
    true ->
      Timeout = Task:timeout(),
      Tester = start_tester(Task, Fun),
      receive
        {Tester, Result} -> Result
      after Timeout ->
        true = exit(Tester, kill),
        {error, timeout}
      end
  end.

start_tester(Task, Fun) ->
  Caller = self(),
  proc_lib:spawn(
    fun() ->
      Results = do_test(Task, Fun),
      case Results of
        [] -> Caller ! {self(), ok};
        Results -> Caller ! {self(), {failures, Results}}
      end
    end).

do_test(Task, Fun) ->
  lists:filtermap(
    fun(Test) ->
      try Test(Fun) of
        ok -> false;
        {error, Msg} -> {true, Msg}
      catch
        _:Exception ->
          {true, #{error => Exception, stack => erlang:get_stacktrace()}}
      end
    end, Task:tests()).
