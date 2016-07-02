-module(bo_players).

-behaviour(sumo_doc).

-type name() :: binary().
-type action() :: skip | solve.

-type stats() ::
  #{ name := name()
   , done := non_neg_integer()
   , score := integer()
   }.

-type done_task() ::
  #{ task := module()
   , action := action()
   }.

-opaque player() ::
  #{ name := name()
   , node := node()
   , task := module() | undefined
   , done := [done_task()]
   , created_at => calendar:datetime()
   }.

-export_type(
  [ name/0
  , action/0
  , stats/0
  , player/0
  ]).

%% sumo_doc behaviour callbacks
-export(
  [ sumo_schema/0
  , sumo_sleep/1
  , sumo_wakeup/1
  ]).

-export(
  [ new/2
  , node/1
  , task/1
  , done/1
  , score/1
  , stats/1
  , finish/2
  , task/3
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(name, string, [id, unique])
    , sumo:new_field(node, custom, [not_null])
    , sumo:new_field(task, custom, [])
    , sumo:new_field(done, custom, [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    ]).

-spec sumo_sleep(player()) -> sumo:doc().
sumo_sleep(Player) -> Player.

-spec sumo_wakeup(sumo:doc()) -> player().
sumo_wakeup(Player) -> Player.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new(name(), node()) -> player().
new(Name, Node) ->
  #{ name => Name
   , node => Node
   , task => bo_tasks:first()
   , done => []
   , created_at => calendar:universal_time()
   }.

-spec node(player()) -> node().
node(#{node := Node}) -> Node.

-spec task(player()) -> module().
task(#{task := Task}) -> Task.

-spec done(player()) -> [module()].
done(#{done := Done}) -> [Task || #{task := Task} <- Done].

-spec score(player()) -> integer().
score(#{done := Done}) -> lists:sum(lists:map(fun do_score/1, Done)).

-spec stats(player()) -> stats().
stats(#{name := Name, done := Done} = Player) ->
  #{ name  => Name
   , done  => length(Done)
   , score => score(Player)
   }.

-spec finish(player(), action()) -> player().
finish(Player, Action) ->
  #{task := Task, done := Done} = Player,
  DoneTask = #{task => Task, action => Action},
  Player#{task := undefined, done := [DoneTask|Done]}.

-spec task(player(), action(), module()) -> player().
task(Player, Action, NextTask) ->
  #{task := Task, done := Done} = Player,
  DoneTask = #{task => Task, action => Action},
  Player#{task := NextTask, done := [DoneTask|Done]}.

do_score(#{task := Task, action := skip}) ->
  round(-0.5 * bo_task:score(Task));
do_score(#{task := Task, action := solve}) ->
  bo_task:score(Task).
