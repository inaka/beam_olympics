-module(bo_coverage_SUITE).

-export([ all/0
        , bo_server/1
        ]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> [bo_server].

-spec bo_server(config()) -> {comment, string()}.
bo_server(_Config) ->
  ct:comment("Build State"),
  {ok, State} = bo_server:init(noargs),

  ct:comment("handle_cast"),
  {noreply, State} = bo_server:handle_cast(msg, State),

  ct:comment("terminate"),
  ok = bo_server:terminate(reason, State),

  ct:comment("code_change"),
  {ok, State} = bo_server:code_change(oldvsn, State, extra),

  ct:comment("handle_info"),
  {noreply, State} = bo_server:handle_info(info, State),

  {comment, ""}.
