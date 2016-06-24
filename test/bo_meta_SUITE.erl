-module(bo_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([ktn_meta_SUITE]).

-export([init_per_suite/1]).

-spec init_per_suite(ktn_meta_SUITE:config()) -> ktn_meta_SUITE:config().
init_per_suite(Config) ->
  [ {application, beam_olympics}
    %% @todo remove once http://bugs.erlang.org/browse/ERL-173 is fixed
  , {xref_config, #{ dirs => dirs(Config)
                   , xref_defaults =>
                      [ {recurse, true}
                      , {builtins, true}
                      , {verbose, false}
                      ]
                   }}
  | Config
  ].

%% @todo remove once http://bugs.erlang.org/browse/ERL-173 is fixed
dirs(_Config) ->
  BaseDir = code:lib_dir(beam_olympics),
  [filename:join(BaseDir, "ebin"), filename:join(BaseDir, "test")].
