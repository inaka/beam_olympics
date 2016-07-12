-module(bo_tasks).

-export([first/0, all/0]).

-spec first() -> module().
first() -> application:get_env(beam_olympics, first_task, hd(all())).

%% @todo dynamically compute DefaultList
-spec all() -> [module()].
all() ->
  DefaultList =
    [ bo_first_task
    , bo_romans
    , bo_henchmen
    , bo_awesomeness
    , bo_missing_operations
    , bo_permutation_sum
    , bo_tuple_counter
    ],
  application:get_env(beam_olympics, all_tasks, DefaultList).
