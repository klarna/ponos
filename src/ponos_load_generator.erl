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
%%% @copyright 2014, Klarna AB
%%% @author Jonathan Olsson <jonathan@klarna.com>
%%% @end

%%%_* Module Declaration ===============================================
%% @private
-module(ponos_load_generator).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==========================================================
%% API
-export([ get_duration/1
        , get_load_spec/1
        , get_name/1
        , get_start/1
        , get_task/1
        , is_running/1
        , pause/1
        , start/1
        , start_link/1
        , stop/1
        , top/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% TODO: Should we terminate with a timer?
%% TODO: Some unit tests are missing
%%%_* Records and Definitions ==========================================
-record(state, {
          call_counter      :: integer(),
          duration          :: ponos:duration(),
          is_running        :: boolean(),
          load_spec         :: ponos:load_spec(),
          name              :: ponos:name(),
          next_trigger_time :: number(),
          intensities       :: list(erlang:now()),
          intensity         :: ponos:intensity(),
          running_tasks     :: integer(),
          start             :: erlang:timestamp(),
          task_runner       :: module(),
          task              :: ponos:task(),
          tick_counter      :: integer()
         }).

-export_type([ load_generator/0
             ]).

-define(PRUNE_INTENSITY_INTERVAL, 2000).

%%%_* Code =============================================================
%%%_* Types ------------------------------------------------------------
-type load_generator() :: pid().

%%%_* External API -----------------------------------------------------
get_duration(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_duration).

get_load_spec(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_load_spec).

get_name(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_name).

get_start(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_start).

get_task(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_task).

is_running(LoadGenerator) ->
  gen_server:call(LoadGenerator, is_running).

pause(LoadGenerator) ->
  gen_server:call(LoadGenerator, pause).

start(LoadGenerator) ->
  ok = gen_server:call(LoadGenerator, start),
  LoadGenerator.

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

stop(LoadGenerator) ->
  gen_server:call(LoadGenerator, stop).

top(LoadGenerator) ->
  gen_server:call(LoadGenerator, top).

%%%_* gen_server callbacks ---------------------------------------------
init(Args) ->
  process_flag(trap_exit, true),
  Name       = element(2, proplists:lookup(name, Args)),
  TaskRunner = element(2, proplists:lookup(task_runner, Args)),
  RunnerArgs = element(2, proplists:lookup(task_runner_args, Args)),

  {ok, State} = ponos_task_runner_callbacks:init(TaskRunner, Name, RunnerArgs),
  {ok, #state{
          call_counter      = 0,
          duration          = element(2, proplists:lookup(duration, Args)),
          load_spec         = element(2, proplists:lookup(load_spec, Args)),
          is_running        = false,
          name              = Name,
          next_trigger_time = 0,
          intensities       = [],
          running_tasks     = 0,
          start             = os:timestamp(),
          task_runner       = {TaskRunner, State},
          task              = element(2, proplists:lookup(task, Args)),
          tick_counter      = 0
         }}.

handle_call(get_duration, _From, State) ->
  {reply, state_get_duration(State), State};
handle_call(get_load_spec, _From, State) ->
  {reply, state_get_load_spec(State), State};
handle_call(get_name, _From, State) ->
  {reply, state_get_name(State), State};
handle_call(get_start, _From, State) ->
  {reply, state_get_start(State), State};
handle_call(get_task, _From, State) ->
  {reply, state_get_task(State), State};
handle_call(is_running, _From, State) ->
  {reply, state_get_is_running(State), State};
handle_call(pause, _From, State) ->
  {reply, ok, dispatch_pause(State)};
handle_call(stop, _From, State) ->
  {stop, {shutdown, removed}, ok, State};
handle_call(start, _From, State) ->
  NewState = dispatch_start(State),
  {reply, ok, NewState};
handle_call(top, _From, State) ->
  Top = dispatch_top(State),
  {reply, Top, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, state_dec_running_tasks(State)};
handle_info(tick, State) ->
  Duration = state_get_duration(State),
  Start = state_get_start(State),
  TimePassed = time_passed_in_ms(os:timestamp(), Start),
  case duration_is_not_exceeded(Duration, TimePassed) of
    true  ->
      tick(),
      {ok, NewState0} = dispatch_tick(TimePassed, State),
      {noreply, state_inc_tick_counter(NewState0)};
    false -> {stop, {shutdown, duration_exceeded}, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

terminate(shutdown, State) ->
  dispatch_terminate(shutdown, State);
terminate({shutdown, duration_exceeded} = Reason, State) ->
  dispatch_terminate(Reason, State);
terminate({shutdown, removed} = Reason, State) ->
  dispatch_terminate(Reason, State).

%% @private
tick() ->
  erlang:send_after(1, self(), tick).

code_change(_OldVsn, LoadGenerator, _Extra) ->
  {ok, LoadGenerator}.

%%%_* gen_server dispatch ----------------------------------------------
dispatch_pause(State) ->
  {TaskRunner, S} = state_get_task_runner(State),
  ponos_task_runner_callbacks:pause(TaskRunner, state_get_name(State), S),
  state_set_is_running(State, false).

dispatch_start(State) ->
  {TaskRunner, RunnerState} = state_get_task_runner(State),
  Name                      = state_get_name(State),
  {ok, RunnerState1} =
    ponos_task_runner_callbacks:start(TaskRunner, Name, RunnerState),
  NewState = init_load_generator(TaskRunner, RunnerState1, State),
  tick(),
  NewState.

dispatch_terminate(_Reason, State) ->
  {TaskRunner, S} = state_get_task_runner(State),
  Name            = state_get_name(State),
  ponos_task_runner_callbacks:terminate(TaskRunner, Name, S),
  ok.

dispatch_tick(TimePassed, State) ->
  case state_get_is_running(State) of
    true ->
      {ok, NewState} = maybe_trigger_task(TimePassed, State),
      {ok, update_intensity(TimePassed, maybe_prune_intensities(NewState))};
    false ->
      {ok, maybe_prune_intensities(State)}
  end.

dispatch_top(State) ->
  Intensities = state_get_intensities(State),
  ModeledLoad = calc_top_modeled_load(State),
  CurrentLoad = calc_current_load(Intensities),
  [ {current_load, CurrentLoad}
  , {total_count,  state_get_call_counter(State)}
  , {modeled_load, ModeledLoad}
  , {running_tasks, state_get_running_tasks(State)}
  ].

%%%_* Internal ---------------------------------------------------------
init_load_generator(TaskRunner, TaskRunnerState, State) ->
  Start      = os:timestamp(),
  TimePassed = time_passed_in_ms(Start, Start),
  LoadSpec   = state_get_load_spec(State),
  Intensity  = LoadSpec(TimePassed),
  State#state{
    start       = Start,
    intensity   = Intensity,
    is_running  = true,
    task_runner = {TaskRunner, TaskRunnerState},
    next_trigger_time = TimePassed + intensity_ms(Intensity)
   }.

maybe_prune_intensities(State) ->
  TickCounter = state_get_tick_counter(State),
  case (TickCounter rem ?PRUNE_INTENSITY_INTERVAL == 0) of
    true  -> do_prune_intensities(State);
    false -> State
  end.

do_prune_intensities(State) ->
  Intensities = state_get_intensities(State),
  Now = os:timestamp(),
  Fun = fun(E) ->
            time_passed_in_ms(Now, E) < ?PRUNE_INTENSITY_INTERVAL
        end,
  NewIntensities = lists:takewhile(Fun, Intensities),
  state_set_intensities(State, NewIntensities).

duration_is_not_exceeded(Duration, TimePassed) ->
  (Duration == infinity) or (Duration > TimePassed).

maybe_trigger_task(TimePassed, State) ->
  case get_next_action(TimePassed, State) of
    trigger_task ->
      {ok, trigger_task_and_update_counters(TimePassed, State)};
    skip ->
      {ok, State}
  end.

get_next_action(TimePassed, State) ->
  TriggerTime = state_get_next_trigger_time(State),
  Intensity   = state_get_intensity(State),
  case should_trigger_task(TimePassed, TriggerTime, Intensity) of
    true  -> trigger_task;
    false -> skip
  end.

should_trigger_task(_TimePased, _TriggerTime, Intensity) when Intensity == 0 ->
  false;
should_trigger_task(TimePassed, TriggerTime, _Intensity) ->
  TimePassed >= TriggerTime.

trigger_task_and_update_counters(_TimePassed, State) ->
  run_task(State),
  update_next_trigger_time(update_counters(State)).

run_task(State) ->
  spawn_link(fun() -> run_task(State, state_get_task(State)) end).

run_task(State, Task) ->
  {TaskRunner, RunnerState} = state_get_task_runner(State),
  Name                      = state_get_name(State),
  ponos_task_runner_callbacks:call(TaskRunner, Name, Task, RunnerState).

update_counters(State) ->
  State1 = state_inc_call_counter(State),
  State2 = state_inc_running_tasks(State1),
  state_add_intensity(State2, os:timestamp()).

update_next_trigger_time(State) ->
  Intensity   = state_get_intensity(State),
  Freq        = freq(Intensity),
  TriggerTime = state_get_next_trigger_time(State),
  state_set_next_trigger_time(State, TriggerTime + Freq).

update_intensity(TimePassed, State) ->
  LoadSpec  = state_get_load_spec(State),
  Intensity = LoadSpec(TimePassed),

  TriggerTime =
    case intensity_changed(Intensity, state_get_intensity(State)) of
      false ->
        state_get_next_trigger_time(State);
      true ->
        OldIntensity = state_get_intensity(State),
        new_trigger_time(OldIntensity, Intensity, State)
    end,
  state_set_next_trigger_time( state_set_intensity(State, Intensity)
                             , TriggerTime).

new_trigger_time(OldIntensity, Intensity, State) ->
  OldTrigger = state_get_next_trigger_time(State),
  do_new_trigger_time(OldIntensity, Intensity, OldTrigger).

do_new_trigger_time(OldIntensity, Intensity, OldTriggerTime) ->
  Freq    = freq(Intensity),
  OldFreq = freq(OldIntensity),
  case freq_increased(OldFreq, Freq) of
    false -> OldTriggerTime - (OldFreq - Freq);
    true  -> OldTriggerTime + abs(OldFreq - Freq)
  end.

freq_increased(OldFreq, Freq) ->
  Freq > OldFreq.

intensity_changed(Current, Previous) ->
  Current =/= Previous.

freq(Intensity) when Intensity == 0 ->
  0.0;
freq(Intensity) ->
  1 / intensity_ms(Intensity).

calc_current_load([]) -> 0.0;
calc_current_load(Intensities) ->
  Period = time_passed_in_ms(os:timestamp(), lists:last(Intensities)),
  do_calc_current_load(Intensities, Period).

do_calc_current_load(_Intensities, 0)     -> 0.0;
do_calc_current_load(Intensities, Period) ->
  _CurrentLoad = length(Intensities) / Period * 1000.0.

calc_top_modeled_load(State) ->
  Start = state_get_start(State),
  LoadSpec = state_get_load_spec(State),
  case Start of
    undefined ->
      0.0;
    Start ->
      LoadSpec(time_passed_in_ms(os:timestamp(), Start))
  end.

time_passed_in_ms(Now, Then) ->
  round(timer:now_diff(Now, Then) / 1000).

intensity_ms(Seconds) ->
  Seconds / 1000.

%% State accessors -----------------------------------------------------
state_get_call_counter(#state{call_counter = CallCounter})    -> CallCounter.
state_get_duration(#state{duration = Duration})               -> Duration.
state_get_intensities(#state{intensities = Intensities})      -> Intensities.
state_get_intensity(#state{intensity = Intensity})            -> Intensity.
state_get_is_running(#state{is_running = IsRunning})          -> IsRunning.
state_get_load_spec(#state{load_spec = LoadSpec})             -> LoadSpec.
state_get_name(#state{name = Name})                           -> Name.
state_get_next_trigger_time(#state{next_trigger_time = NTT})  -> NTT.
state_get_running_tasks(#state{running_tasks = RunningTasks}) -> RunningTasks.
state_get_start(#state{start = Start})                        -> Start.
state_get_task(#state{task = Task})                           -> Task.
state_get_task_runner(#state{task_runner = TaskRunner})       -> TaskRunner.
state_get_tick_counter(#state{tick_counter = TickCounter})    -> TickCounter.

state_add_intensity(State = #state{intensities = Intensities}, TimeStamp) ->
  State#state{intensities = [TimeStamp|Intensities]}.

state_set_intensities(State, Intensities) when is_list(Intensities) ->
  State#state{intensities = Intensities}.

state_inc_call_counter(State = #state{call_counter = CC}) ->
  State#state{call_counter = CC + 1}.

state_inc_running_tasks(State = #state{running_tasks = RTs}) ->
  State#state{running_tasks = RTs + 1}.

state_dec_running_tasks(State = #state{running_tasks = RTs}) ->
  State#state{running_tasks = RTs - 1}.

state_inc_tick_counter(State = #state{tick_counter = TC}) ->
  State#state{tick_counter = TC + 1}.

state_set_next_trigger_time(State, TriggerTime) ->
  State#state{next_trigger_time = TriggerTime}.

state_set_intensity(State, Intensity) when is_float(Intensity) ->
  State#state{intensity = Intensity}.

state_set_is_running(State, IsRunning) when is_boolean(IsRunning) ->
  State#state{is_running = IsRunning}.

%%%_* EUnit Tests ======================================================
-ifdef(TEST).

calc_current_load_test_() ->
  [ ?_assertEqual(0.0, calc_current_load([]))
  , ?_assertEqual(0.2, do_calc_current_load([1, 2], 10000))
  , ?_assertEqual(0.0, do_calc_current_load([1], 0))
  ].

duration_is_not_exceeded_test_() ->
  [ ?_assertEqual(true, duration_is_not_exceeded(infinity, 10))
  , ?_assertEqual(true, duration_is_not_exceeded(100, 99))
  , ?_assertEqual(false, duration_is_not_exceeded(100, 100))
  , ?_assertEqual(false, duration_is_not_exceeded(100, 101))
  ].

should_trigger_task_test_() ->
  [ ?_assertEqual(false, should_trigger_task(0, 0, 0))
  , ?_assertEqual(false, should_trigger_task(1, 2, 0))
  , ?_assertEqual(false, should_trigger_task(2, 1, 0))
  , ?_assertEqual(false, should_trigger_task(999, 1000, 1))
  , ?_assertEqual(true, should_trigger_task(1000, 1000, 1))
  , ?_assertEqual(true, should_trigger_task(1001, 1000, 1))
  ].

get_next_action_test_() ->
  OneCps = 1.0,
  ZeroCps = 0.0,
  State = mk_state(OneCps, 1000),
  [ ?_assertEqual(skip, get_next_action(999, State))
  , ?_assertEqual(trigger_task, get_next_action(1000, State))
  , ?_assertEqual(trigger_task, get_next_action(1001, State))
  , ?_assertEqual(skip, get_next_action(1000, mk_state(ZeroCps, 999)))
  , ?_assertEqual(skip, get_next_action(1000, mk_state(ZeroCps, 1000)))
  , ?_assertEqual(skip, get_next_action(1000, mk_state(ZeroCps, 1001)))
  ].

duration_test() ->
  process_flag(trap_exit, true),
  start_a_load_gen(),
  Reason = {shutdown, duration_exceeded},
  receive
    Exit -> ?assertMatch({'EXIT', _, Reason}, Exit)
  end.

start_a_load_gen() ->
  Args = [ {name, test_duration}
         , {task, fun() -> ok end}
         , {load_spec, ponos_load_specs:make_constant(1.0)}
         , {duration, 10}
         , {task_runner, ponos_default_task_runner}
         , {task_runner_args, []}
         ],
  {ok, LoadGen} = start_link(Args),
  start(LoadGen).

do_new_trigger_time_test_() ->
  [ ?_assertEqual(50.0, do_new_trigger_time(10.0, 20.0, 100))
  , ?_assertEqual(150.0, do_new_trigger_time(20.0, 10.0, 100))
  , ?_assertEqual(150.0, do_new_trigger_time(0.0, 20.0, 100))
  , ?_assertEqual(150.0, begin
                           LoadSpec = fun(_) -> 10.0 end,
                           S = mk_state(20.0, 100, LoadSpec),
                           get_next_trigger_time(update_intensity(100, S))
                         end)
  , ?_assertEqual(50.0, begin
                          LoadSpec = fun(_) -> 20.0 end,
                          S = mk_state(10.0, 100, LoadSpec),
                          get_next_trigger_time(update_intensity(100, S))
                        end)
  ].

get_next_trigger_time(#state{next_trigger_time = NTT}) ->
  NTT.

mk_state(Intensity, TriggerTime) ->
  #state{
     intensity = Intensity,
     next_trigger_time = TriggerTime
    }.

mk_state(Intensity, TriggerTime, LoadSpec) ->
  (mk_state(Intensity, TriggerTime))#state{load_spec = LoadSpec}.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
