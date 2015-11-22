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
-module(ponos_serv).
-behaviour(gen_server).

%%%_* Exports ==========================================================
%% API
-export([ add_load_generator/4
        , get_duration/1
        , get_load_generators/0
        , get_max_concurrent/1
        , get_start/1
        , init_load/1
        , is_running/1
        , pause/1
        , remove_load_generator/1
        , set_max_concurrent/2
        , start_link/0
        , top/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%_* Records and Definitions ==========================================

-define(SERVER,        ?MODULE).

-record(state, {
          load_generators = orddict:new()
         }).

%%%_* Code =============================================================
%%%_* External API -----------------------------------------------------
add_load_generator(Name, Task, LoadSpec, Options) when is_atom(Name),
                                                       is_function(Task),
                                                       is_function(LoadSpec),
                                                       is_list(Options) ->
  Options2 = mk_options(Options),
  case call({add_load_generator, Name, Task, LoadSpec, Options2}) of
    {error, {duplicated, Name}} = Reply ->
      Reply;
    ok ->
      case proplists:get_value(auto_init, Options2) of
        true  -> init_load(Name);
        false -> ok
      end
  end.

%% @private
mk_options(Options) ->
  lists:flatten([Options|default_options()]).

%% @private
default_options() ->
  [ {auto_init, false}
  , {duration, infinity}
  , {max_concurrent, 0}
  , {task_runner, ponos_default_task_runner}
  , {task_runner_args, []}
  ].

get_duration(Name) ->
  call({get_duration, Name}).

-spec get_load_generators() -> list().
get_load_generators() ->
  call(get_load_generators).

get_max_concurrent(Name) ->
  call({get_max_concurrent, Name}).

get_start(Name) ->
  call({get_start, Name}).

-spec init_load(ponos:name()) -> ok
                                 | {error, {non_existing, ponos:name()}}
                                 | {error, already_started}.
init_load(Name) ->
  call({init_load, Name}).

-spec is_running(ponos:name()) -> boolean().
is_running(Name) ->
  call({is_running, Name}).

pause(Name) ->
  call({pause, Name}).

remove_load_generator(Name) ->
  call({remove_load_generator, Name}).

set_max_concurrent(Name, MaxConcurrent) ->
  call({set_max_concurrent, Name, MaxConcurrent}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

top() ->
  call(top).

%% @private
call(Message) ->
  gen_server:call(?SERVER, Message).

%%%_* gen_server Callbacks ---------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({add_load_generator,Name,Task,LoadSpec,Options}, _From,State) ->
  server_add_load_generator(Name, Task, LoadSpec, Options, State);
handle_call({get_duration, Name}, _From, State) ->
  server_get_duration(Name, State);
handle_call(get_load_generators, _From, State) ->
  server_get_load_generators(State);
handle_call({get_max_concurrent, Name}, _From, State) ->
  server_get_max_concurrent(Name, State);
handle_call({get_start, Name}, _From, State) ->
  server_get_start(Name, State);
handle_call({init_load, Name}, _From, State) ->
  server_init_load(Name, State);
handle_call({is_running, Name}, _From, State) ->
  server_is_running(Name, State);
handle_call({pause, Name}, _From, State) ->
  server_pause(Name, State);
handle_call({remove_load_generator, Name}, _From, State) ->
  server_remove_load_generator(Name, State);
handle_call({set_max_concurrent, Name, MaxConcurrent}, _From, State) ->
  server_set_max_concurrent(Name, MaxConcurrent, State);
handle_call(top, _From, State) ->
  server_top(State).

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, {shutdown, removed} = R}, State) ->
  handle_load_generator_down(Ref, R, State);
handle_info({'DOWN',Ref,process,_Pid,{shutdown,duration_exceeded}=R},State) ->
  handle_load_generator_down(Ref, R, State);
handle_info(_Info, State) ->
  {noreply, State}.

handle_load_generator_down(Ref, _Reason, State) ->
  LoadGenName = monitor_to_load_gen_name(Ref, State),
  {noreply, erase_load_generator(LoadGenName, State)}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%%%_* gen_server dispatch ----------------------------------------------
server_add_load_generator(Name, Task, LoadSpec, Options, State) ->
  case load_generator_exists(Name, State) of
    true  ->
      {reply, {error, {duplicated, Name}}, State};
    false ->
      {LoadGen, Monitor} =
        start_and_monitor_load_generator(Name, Task, LoadSpec, Options),
      {reply, ok, store_load_generator(Monitor, LoadGen, State)}
  end.

start_and_monitor_load_generator(Name, Task, LoadSpec, Options) ->
  Duration      = proplists:get_value(duration, Options),
  TaskRunner    = proplists:get_value(task_runner, Options),
  Args          = proplists:get_value(task_runner_args, Options),
  MaxConcurrent = proplists:get_value(max_concurrent, Options),
  {ok, LoadGen} =
    ponos_load_generator_sup:start_child([ {name, Name}
                                         , {task, Task}
                                         , {load_spec, LoadSpec}
                                         , {duration, Duration}
                                         , {max_concurrent, MaxConcurrent}
                                         , {task_runner, TaskRunner}
                                         , {task_runner_args, Args}
                                         ]),
  {LoadGen, erlang:monitor(process, LoadGen)}.

server_get_duration(Name, State) ->
  LoadGen = fetch_load_generator(Name, State),
  {reply, ponos_load_generator:get_duration(LoadGen),State}.

server_get_load_generators(State) ->
  {reply, mk_load_generators_list(State), State}.

mk_load_generators_list(State) ->
  orddict:fold(fun collect_top_and_name_for/3, [], get_load_generators(State)).

collect_top_and_name_for(Name, {LoadGenPid, _Ref}, Acc) ->
  LoadGenerator = [{name, Name}|ponos_load_generator:top(LoadGenPid)],
  [LoadGenerator|Acc].

server_get_max_concurrent(Name, State) ->
  Fun = fun(LoadGen) ->
            {reply, ponos_load_generator:get_max_concurrent(LoadGen), State}
        end,
  execute_on_existing(Name, Fun, State).

server_get_start(Name, State) ->
  LoadGen = fetch_load_generator(Name, State),
  {reply, ponos_load_generator:get_start(LoadGen), State}.

server_init_load(Name, State) ->
  Fun = fun(_) -> maybe_start_load_generator(Name, State) end,
  execute_on_existing(Name, Fun, State).

maybe_start_load_generator(Name, State) ->
  {LoadGen, Ref} = fetch_load_generator_with_ref(Name, State),
  case ponos_load_generator:is_running(LoadGen) of
    false ->
      NewLoadGen = start_load_generator(LoadGen),
      {reply, ok, store_load_generator(Ref, NewLoadGen, State)};
    true ->
      {reply, {error, already_started}, State}
  end.

start_load_generator(LoadGen) ->
  _NewLoadGen = ponos_load_generator:start(LoadGen).

server_is_running(Name, State) ->
  Fun = fun(LoadGen) ->
            {reply, ponos_load_generator:is_running(LoadGen), State}
        end,
  execute_on_existing(Name, Fun, State).

server_pause(Name, State) ->
  Fun = fun(LoadGen) ->
            case ponos_load_generator:is_running(LoadGen) of
              true  -> {reply, ponos_load_generator:pause(LoadGen), State};
              false -> {reply, ok, State}
            end
        end,
  execute_on_existing(Name, Fun, State).

server_remove_load_generator(Name, State) ->
  Fun = fun(LoadGen) ->
            ok = shutdown_load_generator(LoadGen),
            {reply, ok, State}
        end,
  execute_on_existing(Name, Fun, State).

shutdown_load_generator(LoadGen) ->
  ponos_load_generator:stop(LoadGen).

server_set_max_concurrent(Name, MaxConcurrent, State) ->
  Fun = fun(LoadGen) ->
            R = ponos_load_generator:set_max_concurrent(LoadGen, MaxConcurrent),
            {reply, R, State}
        end,
  execute_on_existing(Name, Fun, State).


server_top(State) ->
  AllTop = top_for_all_running_load_gens(State),
  Reply  = [sum_top_results(AllTop, State)|orddict:to_list(AllTop)],
  {reply, lists:flatten(Reply), State}.

top_for_all_running_load_gens(State) ->
  Fun = fun(_Key, {LoadGenPid, _}) ->
            ponos_load_generator:top(LoadGenPid)
        end,
  orddict:map(Fun, get_load_generators(State)).

sum_top_results(AllTop, State) ->
  InitAcc = orddict:from_list([ {current_load, 0.0}
                              , {modeled_load, 0.0}
                              , {running_tasks, 0}
                              , {total_count, 0}
                             ]),
  Fun = fun(LoadGen, TopStats, Acc) ->
            Acc1 = update_current_load(TopStats, Acc),
            Acc2 = update_total_count(TopStats, Acc1),
            Acc3 = update_modeled_load(LoadGen, TopStats, State, Acc2),
            update_running_tasks(TopStats, Acc3)
        end,
  orddict:to_list(orddict:fold(Fun, InitAcc, AllTop)).

inc_key(Key, TopStats, Acc) ->
  {_, IncrementCurrent} = lists:keyfind(Key, 1, TopStats),
  orddict:update_counter(Key, IncrementCurrent, Acc).

update_current_load(TopStats, Acc) ->
  inc_key(current_load, TopStats, Acc).

update_total_count(TopStats, Acc) ->
  inc_key(total_count, TopStats, Acc).

update_running_tasks(TopStats, Acc) ->
  inc_key(running_tasks, TopStats, Acc).

update_modeled_load(LoadGen, TopStats, State, Acc) ->
  LoadGenPid = fetch_load_generator(LoadGen, State),
  case ponos_load_generator:is_running(LoadGenPid) of
    true ->
      {_, IncrementModeled} = lists:keyfind(modeled_load, 1, TopStats),
      orddict:update_counter(modeled_load, IncrementModeled, Acc);
    false ->
      Acc
  end.

execute_on_existing(Name, Fun, State) ->
  case load_generator_exists(Name, State) of
    true  -> Fun(fetch_load_generator(Name, State));
    false -> mk_non_existing_profile_reply(Name, State)
  end.

mk_non_existing_profile_reply(Name, State) ->
  {reply, {error, {non_existing, Name}}, State}.

%%%_* #state{} accessors -----------------------------------------------
erase_load_generator(Name, State = #state{load_generators = LoadGens}) ->
  State#state{load_generators = orddict:erase(Name, LoadGens)}.

fetch_load_generator(Name, State) ->
  {LoadGenPid, _Ref} = fetch_load_generator_with_ref(Name, State),
  LoadGenPid.

fetch_load_generator_with_ref(Name, #state{load_generators = LoadGens}) ->
  orddict:fetch(Name, LoadGens).

find_load_generator(Name, #state{load_generators = LoadGens}) ->
  case orddict:find(Name, LoadGens) of
    error -> error;
    {ok, {LoadGenPid, _Ref}} -> {ok, LoadGenPid}
  end.

get_load_generators(#state{load_generators=LoadGens}) ->
  LoadGens.

load_generator_exists(Name, State) ->
  find_load_generator(Name, State) =/= error.

monitor_to_load_gen_name(MonitorRef, #state{load_generators = LoadGens}) ->
  Fun = fun(_Key, {_, Ref}) -> Ref =:= MonitorRef;
           (_,_)            -> false
        end,
  hd(orddict:fetch_keys(orddict:filter(Fun, LoadGens))).

store_load_generator(Monitor, LoadGenPid, State = #state{}) ->
  LoadGens = State#state.load_generators,
  Name     = ponos_load_generator:get_name(LoadGenPid),
  LoadGen  = {LoadGenPid, Monitor},
  State#state{load_generators = orddict:store(Name, LoadGen, LoadGens)}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
