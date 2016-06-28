-module(bo_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

-spec init(noargs) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
  SupFlags = #{strategy => one_for_one},

  ChildSpecs =
    [#{ id    => bo_server
      , start => {bo_server, start_link, []}
      }
    ],

  {ok, {SupFlags, ChildSpecs}}.
