-module(bo_hooks).

-export([execute/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec execute(atom(), [any()]) -> ok.
execute(Hook, Args) ->
  Hooks = application:get_env(beam_olympics, hooks, #{}),
  execute_hook(maps:get(Hook, Hooks, undefined), Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute_hook({M, F}, A) ->
  ok = cxy_ctl:execute_task(bo, M, F, A);
execute_hook([H | T], A) ->
  ok = execute_hook(H, A),
  ok = execute_hook(T, A);
execute_hook(_, _A) ->
  ok.