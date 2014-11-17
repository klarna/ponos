                       _____   ____  _   _  ____   _____
                      |  __ \ / __ \| \ | |/ __ \ / ____|
                      | |__) | |  | |  \| | |  | | (___
                      |  ___/| |  | | . ` | |  | |\___ \
                      | |    | |__| | |\  | |__| |____) |
                      |_|     \____/|_| \_|\____/|_____/

## Description

Ponos is an Erlang application that exposes a flexible load generator
API. Ponos [[1](#ref1)] is named after the Greek god of hard labor and
toil
([http://en.wikipedia.org/wiki/Ponos](http://en.wikipedia.org/wiki/Ponos)).

Written by Jonathan Olsson <jonathan@klarna.com> with contributions from
Cons Åhs <cons@klarna.com>

## Quick Start Guide

    $> git clone https://github.com/klarna/ponos.git
    $> cd ponos
    $> make
    $> erl -pa ebin -s ponos
    1> Args = [ {name, unique_name_of_type_atom}
    1>        , {task, fun() -> ok end}
    1>        , {load_spec, ponos_load_specs:make_constant(10.0)}
    1>        ].
    2> ponos:add_load_generators([Args]).
    3> ponos:init_load_generators().
    4> application:stop(ponos).


## Introduction

Ponos is a simple yet powerful erlang application used to generate load
at configurable frequencies. It's designed to be lightweight, straight
forward to use and to require minimal configuration.

## Load Generators

There are only three required parts of a load generator:

* `Name`
    * A unique identifier (of type `atom()`) used to reference the load
      generator.
* `Task`
    * A callback function of arity 0; the work to be performed in
      accordance with `LoadSpec`.
* `LoadSpec`
    * The load specification defines the characteristic of the load. It
      is a function that maps time to intensity: `fun(T) -> I` where `T`
      is passed time in milliseconds and `I` is the intensity expressed
      as calls per second. The user may define its own specification,
      but ponos provides typical load patterns such as constant load,
      bursts, staircase, and sawtooth. See
      [`ponos_load_specs`](#ponos_load_specs) for a full list of load
      specifications.

### Adding and Starting Load Generators

    1> Name = foo.
    2> Task = fun() -> do_your_thing end.
    3> LoadSpec = ponos_load_specs:make_sawtooth(10, 20.0).
    4> Options = [{duration, 60*1000}, {auto_init, false}].
    5> Args = [{name,Name},{task,Task},{load_spec,LoadSpec},{options,Options}].
    6> [ok] = ponos:add_load_generators([Args]).
    7> ponos:init_load_generators([Name]).

`ponos:add_load_generators/1` takes a list of proplists where each
proplist denotes the arguments needed to add a new load generator to
ponos. The `options` argument is optional, and may contain all or none
of the available options.

Options:

* `auto_init` - Defaults to `false`. If set to `true`, the load
  generator will start generating load immediately. Otherwise
  `ponos:init_load_generators/0|1` have to be called to start generating
  load.
* `duration` - Defaults to `infinity`. By default the load generator
  will run until removed or paused. If set to a `pos_integer`, the load
  generator will generate load for that many milliseconds.
* `task_runner` - Defaults to `ponos_default_task_runner`. Must be a
  `module()` implementing the `ponos_task_runner_callbacks` behaviour.
* `task_runner_args` - Defaults to `[]`. May be of type any(). The value
  is passed to the `task_runner`'s `init` function.

For convenience, all operations on load generators permits referring to
a single load generator or a list of generators.

## <a name="ponos_load_specs"></a> ponos_load_specs

This module provides a set of predefined constructors for typical load
patterns. A load specification defines the characteristics of load and
is implemented as a function that maps time to intensity: `fun(T) -> I`
where `T` is passed time in milliseconds and `I` is the intensity
expressed as calls per second. The user may implement its own load
specifications.


### A Note About Sampling Intervals

A load generator is implemented as a gen_server receiving a tick every
millisecond. The `LoadSpec` - which is a function of time - thus gets
sampled at every tick to determine whether load should be sent or
not. Since we are theoretically limited to measure half of the sample
rate we are limited to output 500.0 calls per second per load
generator. Should the user need more than 500 cps for a specific task,
two - or more - generators are required. Note that this number may vary
depending on your CPU and/or OS and ponos is not guaranteed to output
500 cps for a single load generator.

## Task Runners

In essence, `ponos` gathers load generators under a supervisor and
makes sure they get a chance to execute their task at the requested
interval. Instead of executing the task itself, ponos hands over to a
task runner at certain points, e.g. when it is time to trigger a
task. For convenience, `ponos` provides two implementations of the
`ponos_task_runner_callbacks` interface:

* ponos_default_task_runner - takes no extra arguments and simply
  applies the provided `Task`.
* ponos_file_task_runner - applies the provided `Task` as well as logs
  the various events to file.

The `ponos_task_runner_callbacks` behaviour defines the following interface:

| ponos_task_runner_callbacks behaviour        |
| -------------------------------------------- |
| `call(Name, Task, State) -> ok`              |
| `init(Name, Args) -> {ok, State::any()}`     |
| `pause(Name, State) -> ok`                   |
| `start(Name, State) -> {ok, NewState::any()` |
| `teminate(Name, State) -> ok`                |

The API allows for modifying the state of the task runner at two
occasions: `init` and `start`. In all other instances, the task runner
may view the state but has no possibility to modify it.

Please refer to `ponos_default_task_runner.erl` and
`ponos_file_task_runner.` for further reference.

## Versioning

Ponos is versioned according to [semantic versioning](http://semver.org/).

## Notes

<a name="ref1"></a>[1] The author is aware of the Russian meaning of the
word *ponos*. I leave it to the Russian to fight this down with the
Greek. Needless to say, a suitable name is a suitable name.