%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%%
%%% @copyright Klarna AB, 2014
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Module Declaration =================================================
-module(load_generator_tests).

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
       [ ?_assertEqual(Name,     load_generator:get_name(LoadGen))
       , ?_assertEqual(Task,     load_generator:get_task(LoadGen))
       , ?_assertEqual(LoadSpec, load_generator:get_load_spec(LoadGen))
       , ?_assertEqual(Duration, load_generator:get_duration(LoadGen))
       ]
   end}.

load_generator_is_running_test_() ->
  {setup,
   fun() -> basic_setup() end,
   fun(LoadGen) -> basic_teardown(LoadGen) end,
   fun(LoadGen) ->
       [ ?_assertEqual(false, load_generator:is_running(LoadGen))
       , ?_assertEqual(true, begin
                                 load_generator:start(LoadGen),
                                 load_generator:is_running(LoadGen)
                             end)
       ]
   end}.

load_generator_pause_test_() ->
  {setup,
   fun() ->
       LoadGen = basic_setup(),
       load_generator:start(LoadGen),
       LoadGen
   end,
   fun(LoadGen) -> basic_teardown(LoadGen) end,
   fun(LoadGen) ->
       [ ?_assertEqual(true, load_generator:is_running(LoadGen))
       , ?_assertEqual(false, begin
                                load_generator:pause(LoadGen),
                                load_generator:is_running(LoadGen)
                              end)
       , ?_assertEqual(true,
                       begin
                         load_generator:start(LoadGen),
                         load_generator:is_running(LoadGen)
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
  {ok, LoadGen} = load_generator:start_link(Args),
  LoadGen.

basic_teardown(LoadGen) ->
  unlink(LoadGen),
  ok = load_generator:stop(LoadGen).


%%%_* Emacs ============================================================
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
