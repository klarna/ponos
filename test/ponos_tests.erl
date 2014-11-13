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
%%%
%%% @copyright Klarna AB, 2014
%%% @end

%%% Module Declaration ========================================================
-module(ponos_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Test Cases ==============================================================
add_and_remove_load_gens_test_() ->
  {setup,
   fun setup_test_case/0,
   fun teardown_test_case/1,
   [ ?_assertEqual([ok], add_basic_load_generator(a, []))
   , ?_assertEqual(1, length(ponos:get_load_generators()))
   , ?_assertEqual([ok], ponos:remove_load_generators([a]))
   , ?_assertEqual(0, length(ponos:get_load_generators()))
   , ?_assertEqual(2, begin
                        LoadGens = [ mk_constant_load_gen(name1)
                                   , mk_constant_load_gen(name2)],
                        [ok, ok] = ponos:add_load_generators(LoadGens),
                        length(ponos:get_load_generators())
                      end)
   ]}.

add_single_load_gen_test_() ->
  LoadGen = mk_constant_load_gen(name1),
  {setup,
   fun setup_test_case/0,
   fun teardown_test_case/1,
   [ ?_assertEqual([ok], ponos:add_load_generators(LoadGen))
   , ?_assert(lists:keymember(name1, 1, ponos:top()))
   ]}.

auto_init_true_test_() ->
  {setup,
   fun setup_test_case/0,
   fun teardown_test_case/1,
   [ ?_assertEqual(false, begin
                            add_basic_load_generator(test1, []),
                            ponos:is_running(test1)
                          end)
   , ?_assertEqual(true, begin
                           add_basic_load_generator(test2, [{auto_init, true}]),
                           ponos:is_running(test2)
                         end)
   ]}.

operations_on_non_existing_load_gen_test_() ->
  Name     = foo,
  Expected = [{error, {non_existing, Name}}],

  {setup,
   fun setup_test_case/0,
   fun teardown_test_case/1,
   [ ?_assertEqual(Expected, ponos:remove_load_generators([Name]))
   ]}.

error_on_multiple_init_load_test_() ->
  {setup,
   fun() ->
       setup_test_case(),
       add_basic_load_generator(a, [{auto_init, false}])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual([ok], ponos:init_load_generators([a]))
       , ?_assertEqual( [{error, already_started}]
                      , ponos:init_load_generators([a]))
       ]
   end}.

init_all_test_() ->
  {setup,
   fun() ->
       setup_test_case(),
       add_basic_load_generator(a, [{auto_init, false}]),
       add_basic_load_generator(b, [{auto_init, false}])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual([ok, ok], ponos:init_load_generators())
       , ?_assertEqual(true, ponos:is_running(a))
       , ?_assertEqual(true, ponos:is_running(b))
       ]
   end}.

error_on_overwriting_test_() ->
  {setup,
   fun() ->
       setup_test_case(),
       add_basic_load_generator(a, [])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual( [{error, {duplicated, a}}]
                      , add_basic_load_generator(a, []))
       , ?_assertEqual( [ok, {error, {duplicated, name1}}]
                      , begin
                          LoadGens = [ mk_constant_load_gen(name1)
                                     , mk_constant_load_gen(name1)],
                          ponos:add_load_generators(LoadGens)
                        end)

       ]
   end}.

pause_load_generators_test_() ->
  {setup,
   fun() ->
       setup([name1, name2, name3, name4, name5], [{auto_init, false}]),
       ponos:init_load_generators([name1, name3])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(true, ponos:is_running(name1))
       , ?_assertEqual(true, ponos:is_running(name3))
       , ?_assertEqual(false, begin
                                [ok] = ponos:pause_load_generators([name1]),
                                ponos:is_running(name1)
                              end)
       , ?_assertEqual(false, ponos:is_running(name2))
       , ?_assertEqual([ok, ok], ponos:pause_load_generators([name2, name3]))
       , ?_assertEqual(false, ponos:is_running(name2))
       , ?_assertEqual(false, ponos:is_running(name3))
       , ?_assertEqual([false, false], begin
                                         [ok, ok, ok, ok, ok] =
                                           ponos:pause_load_generators(),
                                         [ ponos:is_running(name4)
                                         , ponos:is_running(name5)
                                         ]
                                       end)
       ]
   end}.

pause_single_load_generator_test_() ->
  {setup,
   fun() ->
       setup([name1, name2], [{auto_init, true}])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(true, ponos:is_running(name1))
       , ?_assertEqual([ok], ponos:pause_load_generators(name1))
       , ?_assertEqual(false, ponos:is_running(name1))
       , ?_assertEqual(true, ponos:is_running(name2))
       ]
   end}.

remove_multiple_generators_test_() ->
  {setup,
   fun() -> setup([name1, name2, name3], []) end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(3, length(ponos:get_load_generators()))
       , ?_assertEqual(1, begin
                            Removes = [name1, name2],
                            [ok,ok] = ponos:remove_load_generators(Removes),
                            length(ponos:get_load_generators())
                          end)
       , ?_assertEqual(0, begin
                            [ok] = ponos:remove_load_generators([name3]),
                            length(ponos:get_load_generators())
                          end)
       ]
   end}.

remove_all_generators_test_() ->
  {setup,
   fun() -> setup([name1, name2, name3], []) end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(3, length(ponos:get_load_generators()))
       , ?_assertEqual(0, begin
                            [ok,ok,ok] = ponos:remove_load_generators(),
                            length(ponos:get_load_generators())
                          end)
       ]
   end}.

remove_single_load_generator_test_() ->
  {setup,
   fun() -> setup([name1, name2], []) end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(2, length(ponos:get_load_generators()))
       , ?_assertEqual([ok], ponos:remove_load_generators(name2))
       , ?_assert(lists:keymember(name1, 1, ponos:top()))
       , ?_assertNot(lists:keymember(name2, 1, ponos:top()))
       ]
   end}.

init_multiple_test_() ->
  {setup,
   fun() ->
       setup([name1, name2, name3], [{auto_init, false}]),
       ponos:init_load_generators([name1, name2])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(true, ponos:is_running(name1))
       , ?_assertEqual(true, ponos:is_running(name2))
       , ?_assertEqual(false, ponos:is_running(name3))
       ]
   end}.

init_single_test_() ->
  {setup,
   fun() ->
       setup([name1, name2], [{auto_init, false}]),
       ponos:init_load_generators(name1)
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(true, ponos:is_running(name1))
       , ?_assertEqual(false, ponos:is_running(name2))
       ]
   end}.

top_empty_test_() ->
  {setup,
   fun setup_test_case/0,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(0.0, proplists:get_value(modeled_load, ponos:top()))
       , ?_assertEqual(0.0, proplists:get_value(current_load, ponos:top()))
       , ?_assertEqual(0, proplists:get_value(total_count, ponos:top()))
       , ?_assertEqual(0, proplists:get_value(running_tasks, ponos:top()))
       ]
   end}.

top_multiple_load_gens_test_() ->
  {setup,
   fun() ->
       setup([name1, name2], [{auto_init, false}]),
       ponos:init_load_generators([name1])
   end,
   fun teardown_test_case/1,
   fun(_) ->
       [ ?_assertEqual(1.0, proplists:get_value(modeled_load, ponos:top()))
       , ?_assertEqual(2.0, begin
                              ponos:init_load_generators([name2]),
                              proplists:get_value(modeled_load, ponos:top())
                            end)
       ]
   end}.


%%%_* Test Helpers ============================================================
add_basic_load_generator(Name, Options) ->
  LoadSpec = ponos_load_specs:make_constant(1.0),
  add_load_generator(Name, fun() -> ok end, LoadSpec, Options).

add_load_generator(Name, Task, LoadSpec, Options) ->
  LoadGen = [ {name, Name}
            , {task, Task}
            , {load_spec, LoadSpec}
            , {options, Options}
            ],
  ponos:add_load_generators([LoadGen]).

mk_constant_load_gen(Name) ->
  LoadSpec = ponos_load_specs:make_constant(10.0),
  Task     = fun() -> ok end,
  [{name, Name}, {task, Task}, {load_spec, LoadSpec}].

setup(Names, Options) ->
  setup_test_case(),
  lists:foreach(fun(Name) ->
                    add_basic_load_generator(Name, Options)
                end, Names).

setup_test_case() ->
  ok = application:start(ponos).

teardown_test_case(_) ->
  ok = application:stop(ponos).

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
