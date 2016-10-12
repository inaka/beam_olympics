-module(bo_players_repo).

-export([signup/2, fetch/1, advance/2, stats/0, test/2]).

-type stats() ::
  #{ tasks := pos_integer()
   , players := [bo_players:stats()]
   }.
-export_type([stats/0]).

%% @throws conflict
-spec signup(bo_players:name(), node()) -> bo_players:player().
signup(PlayerName, Node) ->
  Player = bo_players:new(PlayerName, Node),
  case sumo:fetch(players, PlayerName) of
    notfound -> sumo:persist(players, Player);
    OldPlayer ->
      error_logger:warning_msg(
        "~p already exists: ~p", [PlayerName, OldPlayer]),
      throw(conflict)
  end.

-spec fetch(bo_players:name()) -> bo_players:player() | notfound.
fetch(PlayerName) -> sumo:fetch(players, PlayerName).

-spec advance(bo_players:player(), bo_players:action()) -> bo_players:player().
advance(Player, Action) ->
  DoneTasks = [bo_players:task(Player) | bo_players:done(Player)],
  case bo_tasks:all() -- DoneTasks of
    [] -> sumo:persist(players, bo_players:finish(Player, Action));
    RemainingTasks ->
      NextTask =
        lists:nth(rand:uniform(length(RemainingTasks)), RemainingTasks),
      sumo:persist(players, bo_players:task(Player, Action, NextTask))
  end.

-spec stats() -> stats().
stats() ->
  PlayerStats =
    [bo_players:stats(Player) || Player <- sumo:find_all(players)],
  SortedStats =
    lists:sort(fun(#{score := S1}, #{score := S2}) -> S1 > S2 end, PlayerStats),
  #{ tasks => length(bo_tasks:all())
   , players => SortedStats
   }.

-spec test(bo_players:player(), bo_tasks:solution()) -> bo_task:result().
test(Player, Solution) ->
  Task = bo_players:task(Player),
  Node = bo_players:node(Player),
  bo_task:test(Task, Solution, Node).
