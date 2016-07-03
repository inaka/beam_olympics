-module(bo_task).

-type result() :: ok | {error, invalid | timeout} | {failures, [term()]}.

-type task() :: #{ name := module()
                 , desc := binary()
                 , spec := spec()
                 , score := pos_integer()
                 }.

-type test() :: fun((solution()) -> ok | {error, term()}).

-type spec() :: #{ input := [binary()]
                 , output := binary()
                 }.

-type solution() :: fun((_) -> _).

-export_type([task/0, result/0, test/0, spec/0, solution/0]).

-callback description() -> binary().
-callback spec() -> spec().
-callback timeout() -> timeout().
-callback tests() -> [test()].
-callback score() -> pos_integer().

-export([describe/1, test/3, score/1, tester/2]).

-spec describe(module()) -> bo_task:task().
describe(Task) ->
  #{ name => Task
   , desc => Task:description()
   , spec => Task:spec()
   , score => Task:score()
   }.

-spec score(module()) -> pos_integer().
score(Task) -> Task:score().

-spec test(module(), solution(), node()) -> result().
test(Task, Fun, Node) ->
  #{input := Params} = Task:spec(),
  case is_function(Fun, length(Params)) of
    false -> {error, invalid};
    true ->
      ok = ensure_code(?MODULE, Node),
      ok = ensure_code(Task, Node),
      case rpc:call(Node, ?MODULE, tester, [Task, Fun], Task:timeout()) of
        {badrpc, Error} -> {error, Error};
        Result -> Result
      end
  end.

-spec tester(module(), solution()) -> result().
tester(Task, Fun) ->
  case do_test(Task, Fun) of
    [] -> ok;
    Results -> {failures, Results}
  end.

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

ensure_code(Module, Node) ->
  {Module, Binary, Filename} = code:get_object_code(Module),
  {module, Module} =
    rpc:call(
      Node, code, load_binary, [Module, filename:basename(Filename), Binary]),
  ok.
