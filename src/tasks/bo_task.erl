-module(bo_task).

-type result() :: ok | {errors, [term()]}.

-type task() :: #{ name := module()
                 , desc := binary()
                 , test := fun((fun()) -> result())
                 }.

-export_type([task/0]).

-callback describe() -> task().
