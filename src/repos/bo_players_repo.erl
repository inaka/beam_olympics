-module(bo_players_repo).

-export([signup/2, fetch/1]).

%% @throws conflict
-spec signup(bo_players:name(), node()) -> bo_players:player().
signup(PlayerName, Node) ->
  Player = bo_players:new(PlayerName, Node),
  case sumo:find(bo_players, PlayerName) of
    notfound -> sumo:persist(bo_players, Player);
    OldPlayer ->
      error_logger:warning_msg(
        "~p already exists: ~p", [PlayerName, OldPlayer]),
      throw(conflict)
  end.

-spec fetch(bo_players:name()) -> bo_players:player() | notfound.
fetch(PlayerName) -> sumo:find(bo_players, PlayerName).
