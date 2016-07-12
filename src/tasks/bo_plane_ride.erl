-module(bo_plane_ride).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() ->
  <<"Plane Ride: You are on a plane and you want to watch as many movies as you"
    " possibly can during your flight. For this, you need to write a function"
    " that receives a list of movies with their runtimes (in the form of"
    " {string(), integer()}) and a flight time and returns a playlist."
    " Keep in mind that the playlist should have as many movies as possibe and"
    " that the you won't tolerate more than 5 minutes of not watching any"
    " movies. If no such playlist exists, return an empty list.">>.

-spec spec() -> bo_task:spec().
spec() ->
  #{ input => [<<"[{string(), pos_integer()}]">>, <<"non_neg_integer()">>]
   , output => <<"[string()]">>
   }.

-spec score() -> 250.
score() -> 250.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- [0, 999999, 180, 187, 999, 2123, 1425]].

build_test(FlightTime) ->
  Args = [get_movie_list(), FlightTime],
  fun(Fun) ->
    try test_result(erlang:apply(Fun, Args), FlightTime) of
      {more_movies, Answer} ->
        {error, #{ input => Args
                 , output => Answer
                 , expected => "A longer playlist."}};
      {not_a_list, Answer} ->
        {error, #{ input => Args
                 , output => Answer
                 , expected => "A list would be a good start."}};
      {bad_format, Answer} ->
        {error, #{ input => Args
                 , output => Answer
                 , expected => "A list of movie names."}};
      {not_long_enough, Answer} ->
        {error, #{ input => Args
                 , output => Answer
                 , expected => "A playlist with a runtime closer to the flight"
                               " time."}};
      {too_long, Answer} ->
        {error, #{ input => Args
                 , output => Answer
                 , expected => "A shorter playlist, this one exceeds the flight"
                               " time."}};
      Ok ->
        Ok
    catch
      _:Error ->
        {error, #{ input => Args
                 , output => Error
                 , expected => "Not an error, that's for sure."}}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
test_result(Answer, FlightTime) when is_list(Answer) ->
  % First get the proper answer
  ProperAnswer = answer(FlightTime),
  % Make sure it's a valid playlist
  GoodFormat = valid_playlist(Answer),
  case {GoodFormat, ProperAnswer, Answer} of
    {false, _, _}  -> {bad_format, Answer};
    {true, [], []} -> ok;
    {true, [], _}  -> check_answer(Answer, FlightTime);
    {true, A1, A2} -> case length(A1) < length(A2) of
                        true  -> {more_movies, Answer};
                        false -> check_answer(Answer, FlightTime)
                      end
  end;
test_result(Answer, _FlightTime) ->
  {not_a_list, Answer}.

valid_playlist([]) ->
  true;
valid_playlist([H | T]) ->
  case lists:keyfind(H, 1, get_movie_list()) of
    false -> false;
    _     -> valid_playlist(T)
  end.

check_answer(Answer, FlightTime) ->
  case get_runtime(Answer) of
    Time when Time > FlightTime ->
      {too_long, Answer};
    Time when Time < FlightTime - 5 ->
      {not_long_enough, Answer};
    _ ->
      ok
  end.

answer(Time) ->
  answer(get_movie_list(), Time, []).

answer([H = {_, MovieTime} | T], Time, Acc) when MovieTime =< Time ->
  answer(T, Time - MovieTime, [H | Acc]);
answer(_Movies, Time, Acc) when Time =< 5 ->
  [N || {N, _} <- Acc];
answer(_Movies, _Time, []) ->
  [];
answer(Movies, Time, [{_, MovieTime} | Acc]) ->
  case [{MN, MT} ||
        {MN, MT} <- lists:reverse(Movies), MT - MovieTime =< Time] of
    [H = {_MovieName, NewMovieTime} | T] ->
      answer(lists:reverse(T), Time + MovieTime - NewMovieTime, Acc ++ [H]);
    _ ->
      []
  end.

get_runtime(Playlist) ->
  lists:sum([proplists:get_value(Movie, get_movie_list()) ||
             Movie <- Playlist]).

get_movie_list() ->
  [{"Blackway",                     90},
   {"The Witch",                    92},
   {"The Good Dinosaur",            93},
   {"Hello, My Name Is Doris",      95},
   {"Hardcore Henry",               96},
   {"Vacation",                     99},
   {"Fantastic Four",               100},
   {"Demolition",                   101},
   {"Eye in the Sky",               102},
   {"Cinderella",                   105},
   {"Ex Machina",                   108},
   {"Colonia",                      110},
   {"Pan",                          111},
   {"The Age of Adaline",           112},
   {"San Andreas",                  114},
   {"Ted 2",                        115},
   {"The Man from U.N.C.L.E.",      116},
   {"Ant-Man",                      117},
   {"Room",                         118},
   {"The Lobster",                  119},
   {"Mad Max: Fury Road",           120},
   {"Everest",                      121},
   {"The Intern",                   121},
   {"Jurassic World",               124},
   {"Trainwreck",                   125},
   {"Terminator Genisys",           126},
   {"Spotlight",                    128},
   {"The Big Short",                130},
   {"Legend",                       132},
   {"Creed",                        133},
   {"Love",                         135},
   {"Star Wars: The Force Awakens", 136},
   {"Furious 7",                    137},
   {"Avengers: Age of Ultron",      141},
   {"The Martian",                  144},
   {"Straight Outta Compton",       147},
   {"Spectre",                      148},
   {"The Revenant",                 156},
   {"The Hateful Eight",            187}].
