%%%
%%%   Copyright (c) 2014, Klarna AB
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%

%%% @doc The ponos application exposes a flexible load generator API.
%%% @copyright 2014, Klarna AB
%%% @author Jonathan Olsson <jonathan@klarna.com>
%%% @end

%%%_* Module Declaration ===============================================
-module(ponos).

%%%_* Exports ==========================================================
%% API
-export([ add_load_generators/1
        , get_load_generators/0
        , init_load_generators/0
        , init_load_generators/1
        , is_running/1
        , pause_load_generators/0
        , pause_load_generators/1
        , remove_load_generators/0
        , remove_load_generators/1
        , start/0
        , top/0
        ]).

-export_type([ args/0
             , duration/0
             , intensity/0
             , load_spec/0
             , name/0
             , task/0
             ]).

%%%_* Code =============================================================
%%%_* Types ------------------------------------------------------------

-type load_generator_option() :: {auto_init, boolean()}  |
                                 {duration, duration()}  |
                                 {task_runner, module()} |
                                 {task_runner_args, any()}.
-type duration()      :: non_neg_integer() | infinity.
-type passed_time()   :: Milliseconds::integer().
-type intensity()     :: CallsPerSecond::float().
-type name()          :: atom().
-type load_spec()     :: fun((passed_time()) -> intensity()).
-type task()          :: fun().
-type arg()           :: {name,      name()}                     |
                         {task,      task()}                     |
                         {load_spec, load_spec()}                |
                         {options,   [load_generator_option()]}.
-type args() :: [arg()].


%%%_* External API -----------------------------------------------------
-spec add_load_generators([args()]) -> [ok | {error, {duplicated, name()}}].
%% @doc Add load generators to ponos. If a load generator with the same
%% name is added twice, the original one *won't* be overwritten.
%% @end
add_load_generators(Args) ->
  [add_load_generator(Arg) || Arg <- Args].

-spec add_load_generator(args()) -> ok | {error, {duplicated, name()}}.
%% @private
add_load_generator(Arg) ->
  ponos_serv:add_load_generator(Arg).

-spec get_load_generators() -> list().
%% @doc Return a readable representation of all registered load
%% generators.
%% @end
get_load_generators() ->
  ponos_serv:get_load_generators().

-spec init_load_generators() -> [ok | {error, {non_existing, name()}}].
%% @doc Same as {@link init_load_generators/1}, but for all registered
%% load generators.
%% @end
init_load_generators() ->
  init_load_generators(get_all_names()).

-spec init_load_generators([name()]) -> [ ok
                                        | {error, {non_existing, name()}}
                                        | {error, already_started}
                                        ].
%% @doc Initialize all generators present in `Names'. Already
%% initialized generators will be ignored.
%% @end
init_load_generators(Names) when is_list(Names) ->
  [init_load_generator(Name) || Name <- Names].

-spec init_load_generator(name()) -> ok
                                     | {error, {non_existing, name()}}
                                     | {error, already_started}.
%% @private
init_load_generator(Name) ->
  ponos_serv:init_load(Name).

-spec is_running(name()) -> boolean().
%% @doc Return `true' if the load generator is generating load, `false'
%% otherwise.
%% @end
is_running(Name) ->
  ponos_serv:is_running(Name).

-spec pause_load_generators() -> [ok | {error, {non_existing, name()}}].
%% @doc Same as {@link pause_load_generators/1}, but for all registered
%% load generators.
%% @end
pause_load_generators() ->
  pause_load_generators(get_all_names()).

-spec pause_load_generators([name()]) -> [ok | {error, {non_existing, name()}}].
%% @doc Stop generating load for all load generators present in
%% `Names'. Already paused load generators will be ignored.
%% @end
pause_load_generators(Names) when is_list(Names) ->
  [pause_load_generator(Name) || Name <- Names].

-spec pause_load_generator(name()) -> ok | {error, {non_existing, name()}}.
%% @private
pause_load_generator(Name) ->
  ponos_serv:pause(Name).

-spec remove_load_generators() -> [ok | {error, {non_existing, name()}}].
%% @doc Same as {@link remove_load_generators/1} but for all registered
%% load generators.
%% @end
remove_load_generators() ->
  remove_load_generators(get_all_names()).

-spec remove_load_generators([name()]) -> [ok | {error, {non_existing,name()}}].
%% @doc Stop generating load for load generators and remove them from
%% ponos.
%% @end
remove_load_generators(Names) ->
  [remove_load_generator(Name) || Name <- Names].

-spec remove_load_generator(name()) -> ok | {error, {non_existing, name()}}.
%% @private
remove_load_generator(Name) ->
  ponos_serv:remove_load_generator(Name).

-spec top() -> list().
%% @doc Return a fairly readable list of properties regarding the load
%% ponos is currently generating.
%% @end
top() ->
  ponos_serv:top().

%% @hidden
start() ->
  application:start(ponos).

%%%_* Internal ---------------------------------------------------------
-spec get_all_names() -> [name()].
%% @private
get_all_names() ->
  F = fun(LoadGen) -> proplists:get_value(name, LoadGen) end,
  lists:map(F, get_load_generators()).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
