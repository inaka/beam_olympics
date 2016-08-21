%%% @doc Main Application Module
-module(bo).
-author('elbrujohalcon@inaka.net').

-behaviour(application).

-export([ start/0
        , stop/0
        , start/2
        , stop/1
        , start_phase/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start / Stop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the Application
-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> application:ensure_all_started(beam_olympics).

%% @doc Stops the Application
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(beam_olympics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec start(application:start_type(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) -> bo_sup:start_link().

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.

%% @private
-spec start_phase(atom(), StartType::application:start_type(), []) ->
  ok | {error, _}.
start_phase(app_start_hook, _StartType, []) ->
  bo_hooks:execute(app_started, []);
start_phase(cxy_ctl_setup, _StartType, []) ->
  true = cxy_ctl:init([{bo, unlimited, 1000, 100000}]),
  ok;
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  % Create persistency schema
  sumo:create_schema().
