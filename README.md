# BeamOlympics



Let's find the fastest beamer!

## Introduction
**BeamOlympics** is an Erlang app to help you check your Erlang-Fu. You can also play with friends in a local network.

## Prerequisites
As stated in rebar.config, **BeamOlympics** server only compiles with Erlang/OTP19+.

## Installation
Before you can start playing, you have to install the **BeamOlympics** server.
To do so, clone this repo and use [reba3](https://github.com/erlang/rebar3) to generate a release for it.

```bash
$ git clone https://github.com/inaka/beam_olympics.git
$ rebar3 release
```

If needed, between those 2 steps you can change your node name in [vm.args](https://github.com/inaka/beam_olympics/blob/master/config/vm.args#L2) to your convenience.

## Start
To start the **BeamOlympics** server you generated in the previous step, you run…

```bash
$ _build/default/rel/beam_olympics/bin/beam_olympics start
```

That will boot up an Erlang node in your computer with the name specified by vm.args.

## Players
Inside the node started in the previous step, a [gen_server](http://erldocs.com/maint/stdlib/gen_server.html) will be running. That server will act as the point of contact for clients to play the game.
Every interaction will be accomplished through `gen_server` calls. For example, to check the overall game statistics, from a client node a caller can evaluate the following Elixir code:

```elixir
GenServer.call({:bo_server, server_node}, :stats)
```

where `server_node` is a variable bound to the server node name (or a function that returns it).

### General Rules
* To prevent fraud, users must always post solutions from the node in which they signed up. The node might crash/be closed, but when restarted it has to be called with the same name, otherwise submissions will be rejected.
* There is no *undo*, if you skipped a task you can't choose to submit a solution for it later.
* Each task has it's own associated score. Points are assigned this way:
    - Each time a task is correctly solved, `score` points are added to the user
    - Each time a task is skipped, `score / 2` points are deduced from the user
* After skipping or completing a task, the user will either receive a new task or just the atom `the_end` indicating that the game is over

### Commands
You can find the complete list of commands/calls with their possible answers below. Specs are written as functions although in reality the input is the call sent to the server and the output is the possible responses the server may return.

----
#### Signup
Using this call, the users register themselves in the game and receive their initial task.

##### Erlang Specs
```erlang
{signup, PlayerName :: binary()} -> {ok, Task} | {error, conflict}.
Task :: #{ name := module()
         , desc := binary()
         , spec := Spec
         , score := pos_integer()
         }.
Spec :: #{ input := [binary()]
         , output := binary()
         }.
```

##### Elixir Specs
```elixir
{:signup, player_name} :: {:ok, task} | {:error, :conflict}
  @type player_name :: String.t
  @type task :: %{
                  name: module,
                  desc: String.t,
                  spec: spec,
                  score: pos_integer
                 }
  @type spec :: %{
                  input: [String.t],
                  output: String.t
                 }
```

##### Details
* The server will return `conflict` if the player is already registered.
* The spec describes the function required to complete the task. `input` indicates the number and types of the parameters, `output` indicates the type of result it should produce.

##### Examples
```erlang
gen_server:call({bo_server, ServerNode}, {signup, <<"Player">>}).
```

```elixir
GenServer.call({:bo_server, server_node}, {:signup, "Player"})
```

----
#### Task
Retrieves the current task for the player.

##### Erlang Specs
```erlang
{task, PlayerName :: binary()} ->
    {ok, Task} | {error, ended | forbidden | notfound}
```

##### Elixir Specs
```elixir
{:task, player_name} :: {:ok, task} | {:error, :ended | :forbidden | :notfound}
```

##### Details
* The server will return `ended` if the game is over for the player and therefore there is no current task
* The server will return `forbidden` if the player is calling from a node that's not the one from which they signed up in the first place
* The server will return `notfound` if the player doesn't exist

##### Examples
```erlang
gen_server:call({bo_server, ServerNode}, {task, <<"Player">>}).
```

```elixir
GenServer.call({:bo_server, server_node}, {:task, "Player"})
```

----
#### Submit
Submits a solution for the current task.

##### Erlang Specs
```erlang
{submit, PlayerName :: binary(), Solution :: any()} ->
    {ok, Task}
  | the_end
  | {error, invalid | timeout | ended | forbidden | notfound}
  | {failures, [term(), ...]}
```

##### Elixir Specs
```elixir
{:submit, player_name, term} ::
    {:ok, task} | :the_end |
    {:error, :invalid | :timeout | :ended | :forbidden | :notfound} |
    {:failures, [term,...]}
```

##### Details
* The server will return `ended` if the game is over for the player and therefore there is no current task
* The server will return `forbidden` if the player is calling from a node that's not the one from which they signed up in the first place
* The server will return `notfound` if the player doesn't exist
* The server will return `timeout` if the evaluation of the tests took longer than expected
* The server will return `invalid` if the arity of the provided function doesn't match the arity expected by the task or if the input is not even a function
* The server will return `the_end` if the game is over for the player (i.e. there are no more tasks to complete/skip)
* The server will return `failures` with a list of failure descriptions if there are tests for which the provided function fails

##### Examples
```erlang
gen_server:call(
    {bo_server, ServerNode}, {submit, <<"Player">>, fun() -> something end}).
```

```elixir
GenServer.call(
    {:bo_server, server_node}, {:submit, "Player", fn() -> :something end})
```

----
#### Skip
Skips the current task.

##### Erlang Specs
```erlang
{skip, PlayerName :: binary()} ->
    {ok, Task} | the_end | {error, ended | forbidden | notfound}
```

##### Elixir Specs
```elixir
{:skip, player_name} ::
    {:ok, task} | :the_end | {:error, :ended | :forbidden | :notfound}
```

##### Details
* The server will return `ended` if the game is over for the player and therefore there is no current task
* The server will return `forbidden` if the player is calling from a node that's not the one from which they signed up in the first place
* The server will return `notfound` if the player doesn't exist
* The server will return `the_end` if the game is over for the player (i.e. there are no more tasks to complete/skip)

##### Examples
```erlang
gen_server:call({bo_server, ServerNode}, {skip, <<"Player">>}).
```

```elixir
GenServer.call({:bo_server, server_node}, {:skip, "Player"})
```

----
#### Score
Retrieves the current score for the player.

##### Erlang Specs
```erlang
{score, PlayerName :: binary()} -> {ok, integer()} | {error, forbidden | notfound}
```

##### Elixir Specs
```elixir
{:score, player_name} :: {:ok, integer} | {:error, :forbidden | :notfound}
```

##### Details
* The server will return `forbidden` if the player is calling from a node that's not the one from which they signed up in the first place
* The server will return `notfound` if the player doesn't exist

##### Examples
```erlang
gen_server:call({bo_server, ServerNode}, {score, <<"Player">>}).
```

```elixir
GenServer.call({:bo_server, server_node}, {:score, "Player"})
```

----
#### Stats
Retrieves the current game stats.

##### Erlang Specs
```erlang
stats -> #{ tasks := pos_integer()
          , players := [PlayerStats]
          }.
PlayerStats :: #{ name := binary()
                , done := non_neg_integer()
                , score := integer()
                }.
```

##### Elixir Specs
```elixir
:stats :: stats
  @type stats :: %{
                    tasks: pos_integer,
                    players: [player_stats]
                  }
  @type player_stats :: %{
                           name: String.t,
                           done: non_neg_integer,
                           score: integer
                         }
```

##### Details
* `tasks` will be the total number of tasks in the game
* `done` will be the number of tasks already completed/skipped by the player

##### Examples
```erlang
gen_server:call({bo_server, ServerNode}, stats).
```

```elixir
GenServer.call({:bo_server, server_node}, :stats)
```

---

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).
