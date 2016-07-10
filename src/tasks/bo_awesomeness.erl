-module(bo_awesomeness).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

-spec description() -> binary().
description() ->
  <<"Awesomeness Factor: Measure the awesomeness of things.\n"
    "You should write a function (let's call it Aw) "
    "that measures how awesome any Erlang term is.\n"
    "The rules are:\n"
    "  - higher output means more awesomeness\n"
    "  - chuck_norris is the most awesome thing ever\n"
    "  - the awesomeness of a thing should not vary over time\n"
    "  - noone at inaka should known about this function. If an inako is\n"
    "    around, hide your code and act as if you were just checking facebook\n"
    "  - a non-empty list of things is awesomer than each of its items\n"
    "    (except if it includes chuck_norris)\n"
    "  - no horse is allowed on the surroundings of the curling stadium\n"
    "  - a non-empty tuple of things is less awesome than each of its items\n"
    "  - a non-empty map of things is as awesome as one of its values\n"
    "  - nothing can be faster than the speed of light, unless Chuck says so\n"
    "  - for any group of 100 different things, at least one of them must\n"
    "    have a different level of awesomeness than the rest [i.e. not all\n"
    "    can be equally awesome]\n"
    "  - your own awesomeness must be something to brag about on ICQ\n"
    "  - any string/binary with a lower-case vowel in it cant' have an\n"
    "    awesomeness larger than 10\n"
    "  - when one body exerts a force on a second body, the second body\n"
    "    simultaneously exerts a force equal in magnitude and opposite in\n"
    "    direction on the first body."
    >>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"term()">>], output => <<"number()">>}.

-spec score() -> 150.
score() -> 150.

-spec timeout() -> 30000.
timeout() -> 30000.

-spec tests() -> [bo_task:test()].
tests() ->
  [ fun chuck_norris/1
  , fun constant_awesomeness/1
  , fun lists/1
  , fun horse/1
  , fun tuples/1
  , fun maps/1
  , fun hundred/1
  , fun lowercase/1
  ].

chuck_norris(Fun) ->
  Inputs = random(5),
  Outputs = [Fun(I) || I <- [chuck_norris | Inputs]],
  Max = lists:max(Outputs),
  case Outputs of
    [Max | _] -> ok;
    [_ | _] -> {error, {<<"Chuck should be awesomer than">>, Inputs}}
  end.

constant_awesomeness(Fun) ->
  Inputs = random(5),
  try
    _ = [constant_awesomeness(Fun, I) || I <- Inputs],
    ok
  catch
    _:Input -> {error, {<<"awesomeness varies for">>, Input}}
  end.

constant_awesomeness(Fun, Input) ->
  case lists:usort([Fun(Input) || _ <- lists:seq(1, 10)]) of
    [_] -> ok;
    _ -> throw(Input)
  end.

lists(Fun) ->
  Inputs = random(list, 5),
  try
    _ = [lists(Fun, List) || List <- Inputs],
    ok
  catch
    _:List -> {error, {List, <<"isn't awesomer than its items">>}}
  end.

lists(Fun, List) ->
  AList = Fun(List),
  case lists:max([Fun(I) || I <- List]) of
    AMax when AMax >= AList -> throw(List);
    _ -> ok
  end.

horse(Fun) ->
  try Fun(horse) of
    _ -> ok
  catch
    _:_ ->
      erlang:display(
        <<"Really? I have to leave my horse away?, damn rules!">>),
      ok
  end.

tuples(Fun) ->
  Inputs = random(tuple, 5),
  try
    _ = [tuples(Fun, Tuple) || Tuple <- Inputs],
    ok
  catch
    _:Tuple -> {error, {Tuple, <<"is awesomer than its items">>}}
  end.

tuples(Fun, Tuple) ->
  ATuple = Fun(Tuple),
  case lists:min([Fun(I) || I <- tuple_to_list(Tuple)]) of
    AMin when AMin =< ATuple -> throw(Tuple);
    _ -> ok
  end.

maps(Fun) ->
  Inputs = random(map, 5),
  try
    _ = [maps(Fun, Map) || Map <- Inputs],
    ok
  catch
    _:Map -> {error, {Map, <<"is not as awesome as one of its values">>}}
  end.

maps(Fun, Map) ->
  AMap = Fun(Map),
  case lists:member(AMap, [Fun(I) || I <- maps:values(Map)]) of
    false -> throw(Map);
    true -> ok
  end.

hundred(Fun) ->
  Inputs = [random(100) || _ <- lists:seq(1, 5)],
  try
    _ = [hundreds(Fun, List) || List <- Inputs],
    ok
  catch
    _:List -> {error, {<<"all items on ">>, List, <<"are equally awesome">>}}
  end.

hundreds(Fun, List) ->
  case lists:usort(lists:map(Fun, List)) of
    [_] -> throw(List);
    _ -> ok
  end.

lowercase(Fun) ->
  Inputs = [<<"all">>, <<"the">>, <<"tiny">>, <<"good">>, <<"zulus">>],
  try
    _ = [lowercase(Fun, Word) || Word <- Inputs],
    ok
  catch
    _:Word -> {error, <<Word/binary, " is too awesome">>}
  end.

lowercase(Fun, Word) ->
  case Fun(Word) of
    AWord when AWord =< 10 -> ok;
    _ -> throw(Word)
  end.

random(N) -> [random_item() || _ <- lists:seq(1, N)].

random(T, N) when is_atom(T) -> [random_item(T) || _ <- lists:seq(1, N)];
random(Ts, N) -> random(random_type(Ts), N).

random_item() -> random_item(random_type()).

random_type() -> random_type(all()).
random_type(Types) -> lists:nth(rand:uniform(length(Types)), Types).

all() ->
  [number, atom, binary, reference, function, port, pid, tuple, map, list,
   simple_list, simple_tuple].

random_item(number) ->
  case rand:uniform(2) of
    1 -> trunc(rand:uniform());
    _ -> rand:uniform()
  end;
random_item(atom) ->
  list_to_atom(float_to_list(1.0 * random_item(number)));
random_item(binary) ->
  float_to_binary(1.0 * random_item(number));
random_item(reference) ->
  make_ref();
random_item(function) ->
  X = random_item(),
  fun() -> X end;
random_item(port) ->
  Ports = erlang:ports(),
  lists:nth(rand:uniform(length(Ports)), Ports);
random_item(pid) ->
  spawn_link(fun() -> timer:sleep(5000) end);
random_item(tuple) ->
  list_to_tuple(random_item(list));
random_item(map) ->
  Length = rand:uniform(5),
  Ks = random(atom, Length),
  Vs = random(all() -- [list, tuple, map], Length),
  maps:from_list(lists:zip(Ks, Vs));
random_item(list) ->
  random(all() -- [list, tuple, map], rand:uniform(10));
random_item(simple_list) ->
  random([atom, binary], rand:uniform(5));
random_item(simple_tuple) ->
  list_to_tuple(random_item(simple_list)).
