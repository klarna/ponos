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

%%% @doc
%%% @copyright Klarna AB, 2014
%%% @end

%%% Module Declaration =================================================
-module(ponos_load_generator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Test Cases =======================================================
load_generator_basic_test_() ->
  Name     = foo,
  Task     = fun() -> ok end,
  LoadSpec = ponos_load_specs:make_constant(2.0),
  Duration = infinity,

  {setup,
   fun() -> basic_setup(Name, Task, LoadSpec, Duration) end,
   fun(LoadGen) -> basic_teardown(LoadGen) end,
   fun(LoadGen) ->
       [ ?_assertEqual(Name,     ponos_load_generator:get_name(LoadGen))
       , ?_assertEqual(Task,     ponos_load_generator:get_task(LoadGen))
       , ?_assertEqual(LoadSpec, ponos_load_generator:get_load_spec(LoadGen))
       , ?_assertEqual(Duration, ponos_load_generator:get_duration(LoadGen))
       ]
   end}.

load_generator_is_running_test_() ->
  {setup,
   fun() -> basic_setup() end,
   fun(LoadGen) -> basic_teardown(LoadGen) end,
   fun(LoadGen) ->
       [ ?_assertEqual(false, ponos_load_generator:is_running(LoadGen))
       , ?_assertEqual(true, begin
                                 ponos_load_generator:start(LoadGen),
                                 ponos_load_generator:is_running(LoadGen)
                             end)
       ]
   end}.

load_generator_pause_test_() ->
  {setup,
   fun() ->
       LoadGen = basic_setup(),
       ponos_load_generator:start(LoadGen),
       LoadGen
   end,
   fun(LoadGen) -> basic_teardown(LoadGen) end,
   fun(LoadGen) ->
       [ ?_assertEqual(true, ponos_load_generator:is_running(LoadGen))
       , ?_assertEqual(false, begin
                                ponos_load_generator:pause(LoadGen),
                                ponos_load_generator:is_running(LoadGen)
                              end)
       , ?_assertEqual(true,
                       begin
                         ponos_load_generator:start(LoadGen),
                         ponos_load_generator:is_running(LoadGen)
                       end)
       ]
   end}.

basic_setup() ->
  Name     = foo,
  Task     = fun() -> ok end,
  LoadSpec = ponos_load_specs:make_constant(2.0),
  Duration = infinity,
  basic_setup(Name, Task, LoadSpec, Duration).

basic_setup(Name, Task, LoadSpec, Duration) ->
  Args = [ {name, Name}
         , {task, Task}
         , {load_spec, LoadSpec}
         , {duration, Duration}
         , {task_runner, ponos_default_task_runner}
         , {task_runner_args, []}
         ],
  {ok, LoadGen} = ponos_load_generator:start_link(Args),
  LoadGen.

basic_teardown(LoadGen) ->
  unlink(LoadGen),
  ok = ponos_load_generator:stop(LoadGen).


%%%_* Emacs ============================================================
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
