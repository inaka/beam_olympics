-module(bo_test_client).

-export([start/1, stop/1]).
-export([signup/2, task/2]).
-export([gen_call/2]).

-type task() :: #{ name := module()
                 , desc := binary()
                 , test := fun((fun()) -> term())
                 }.
-type player_name() :: binary().

-spec start(atom()) -> {ok, node()}.
start(NodeName) ->
  {ok, Node} = slave:start("127.0.0.1", NodeName),
  spawn(Node, code, add_paths, [code:get_path()]),
  ct:log("Nodes after starting ~p: ~p", [NodeName, nodes(connected)]),
  {ok, Node}.

-spec stop(node()) -> ok.
stop(Node) ->
  ok = slave:stop(Node),
  ct:log("Nodes after stopping ~p: ~p", [Node, nodes(connected)]),
  ok.

-spec signup(node(), player_name()) -> {ok, task()} | {error, term()}.
signup(Node, Player) ->
  call(Node, {signup, Player}).

-spec task(node(), player_name()) -> {ok, task()} | {error, term()}.
task(Node, Player) ->
  call(Node, {task, Player}).


call(Node, Msg) ->
  Caller = self(),
  Pid = spawn(Node, ?MODULE, gen_call, [Caller, Msg]),
  receive
    {Pid, Response} -> Response
  after 5000 ->
    throw({timeout, Node, Msg})
  end.

-spec gen_call(pid(), term()) -> {pid(), term()}.
gen_call(Caller, Msg) ->
  try gen_server:call({bo_server, 'bo_test@127.0.0.1'}, Msg) of
    Something ->
      Caller ! {self(), Something}
  catch
    _:Error ->
      Caller ! {self(), {error, {Error, erlang:get_stacktrace()}}}
  end.
