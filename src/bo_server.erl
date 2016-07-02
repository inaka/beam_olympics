-module(bo_server).

-behaviour(gen_server).

-export([ start_link/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type state() :: #{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(
    {local, ?MODULE}, ?MODULE, noargs, [{debug, [{trace, log}]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(noargs) -> {ok, state()}.
init(noargs) -> {ok, #{}}.

-spec handle_call
  ({signup, bo_players:name()}, {pid(), term()}, state()) ->
    {reply, {ok, bo_task:task()} | {error, conflict}, state()};
  ({task, bo_players:name()}, {pid(), term()}, state()) ->
    {reply, {ok, bo_task:task()}
          | {error, ended | forbidden | notfound}, state()};
  ({submit, bo_players:name(), term()}, {pid(), term()}, state()) ->
    {reply, {ok, bo_task:task()} | the_end
          | {error, invalid | timeout | ended | forbidden | notfound}
          | {failures, [term()]}, state()}.
handle_call({signup, PlayerName}, {From, _}, State) ->
  Node = node(From),
  try bo_players_repo:signup(PlayerName, Node) of
    Player -> {reply, task(Player), State}
  catch
    _:conflict -> {reply, {error, conflict}, State}
  end;
handle_call({task, PlayerName}, {From, _}, State) ->
  Node = node(From),
  case bo_players_repo:fetch(PlayerName) of
    notfound -> {reply, {error, notfound}, State};
    Player ->
      case bo_players:node(Player) of
        Node -> {reply, task(Player), State};
        NotNode ->
          error_logger:warning_msg(
            "~p trying to access from ~p but registered at ~p",
            [PlayerName, Node, NotNode]),
          {reply, {error, forbidden}, State}
      end
  end;
handle_call({submit, PlayerName, Solution}, {From, _}, State) ->
  Node = node(From),
  case bo_players_repo:fetch(PlayerName) of
    notfound -> {reply, {error, notfound}, State};
    Player ->
      case bo_players:node(Player) of
        Node ->
          {reply, test(Player, Solution), State};
        NotNode ->
          error_logger:warning_msg(
            "~p trying to access from ~p but registered at ~p",
            [PlayerName, Node, NotNode]),
          {reply, {error, forbidden}, State}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task(Player) ->
  case bo_players:task(Player) of
    undefined -> {error, ended};
    Task -> {ok, bo_task:describe(Task)}
  end.

test(Player, Solution) ->
  case bo_players:task(Player) of
    undefined -> {error, ended};
    Task ->
      case bo_task:test(Task, Solution) of
        ok -> next_task(Player);
        NOK -> NOK
      end
  end.

next_task(Player) ->
  NewPlayer = bo_players_repo:advance(Player),
  case bo_players:task(NewPlayer) of
    undefined -> the_end;
    Task -> {ok, bo_task:describe(Task)}
  end.
