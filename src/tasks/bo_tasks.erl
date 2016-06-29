-module(bo_tasks).

-export([first/0, all/0]).
-export([describe/1]).

-spec first() -> module().
first() -> application:get_env(beam_olympics, first_task, bo_first_task).

%% @todo dynamically compute DefaultList
-spec all() -> [module()].
all() ->
  DefaultList = [bo_first_task],
  application:get_env(beam_olympics, all_tasks, DefaultList).

-spec describe(module()) -> bo_task:task().
describe(Task) -> Task:describe().
