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
-module(ptop).

%%%_* Exports ==========================================================
-export([ start/0
        , stop/0
        ]).


%%%_* Records and Definitions ==========================================
-define(INTERVAL, 2000).

%%%_* Code =============================================================
%%%_* External API -----------------------------------------------------
start() ->
  start(ptop).

stop() ->
  stop(ptop).


%%%_* Internal API -----------------------------------------------------
start(Name) ->
  Self = self(),
  Pid = spawn(fun() -> init(Name, Self) end),
  Ref = erlang:monitor(process, Pid),
  receive
    {ack, Pid} ->
      erlang:demonitor(Ref, [flush]);
    {'DOWN', Ref, process, Pid, _} ->
      exit({already_started, Name})
  end.

stop(Name) ->
  case whereis(Name) of
    Pid when is_pid(Pid) ->
      Name ! {self(), stop},
      receive
        {stopped, R} -> {stopped, R}
      end;
    undefined ->
      not_started
  end.

init(Name, Parent) ->
  true = register(Name, self()),
  Parent ! {ack, self()},
  process_flag(trap_exit, true),
  tick(),
  loop(Name).

tick() ->
  erlang:start_timer(?INTERVAL, self(), tick).

loop(Name) ->
  receive
    {timeout, _, tick} ->
      pp(),
      tick(),
      loop(Name);
    {Stopper, stop} ->
      Stopper ! {stopped, Name}
  end.

-spec pp() -> ok.
%% @doc Pretty print current status of ponos. This is useful for getting
%% information on what load generators are added, their current load,
%% etc.
%% @end
pp() ->
  AllTop = ponos:top(),
  pp_procs(),
  pp_ports(),
  pp_header(),
  pp_load_generators(AllTop),
  pp_total(AllTop),
  pp_dividor("=").

%% @private
pp_procs() ->
  Args = [erlang:system_info(process_count), erlang:system_info(process_limit)],
  io:format("~n~nProcs/Limit: ~B/~B~n", Args).

%% @private
pp_ports() ->
  Args = [erlang:system_info(port_count), erlang:system_info(port_limit)],
  io:format("Ports/Limit: ~B/~B~n", Args).

%% @private
pp_header() ->
  pp_dividor("="),
  Fmt = "~-11.11s | ~-7.7s | ~-5.4s | ~-7.7s | ~-10s | ~-10s | ~-10s~n",
  Args = ["Name","Running","Load","Modeled","No Calls","Duration","Run Time"],
  io:format(Fmt, Args),
  pp_dividor("-").

%% @private
pp_dividor(Div) ->
  io:format("~-80.80." ++ Div ++ "s~n", [""]).

%% @private
pp_load_generators(AllTop) ->
  LoadGens = delete_aggregates(AllTop),
  Fun      = fun({Name, Top}) -> pp_load_generator(Name, Top) end,
  lists:foreach(Fun, LoadGens),
  pp_dividor("-").

%% @private
pp_load_generator(Name, Top) ->
  Duration  = ponos_serv:get_duration(Name),
  IsRunning = ponos_serv:is_running(Name),
  NoRunning = integer_to_list(proplists:get_value(running_tasks, Top)),
  IsRunningStr = case IsRunning of
                   true  -> "y(" ++ NoRunning ++ ")" ;
                   false -> "n(" ++ NoRunning ++ ")"
                 end,
  Name1     = lists:flatten(io_lib:write(Name, 11)),
  Fmt = "~-11.11s | ~-7.7s | ~-5.1f | ~-7.1f | ~-10B | ~-10w | ~s~n",
  io:format(Fmt, [ Name1
                 , IsRunningStr
                 , proplists:get_value(current_load, Top)
                 , proplists:get_value(modeled_load, Top)
                 , proplists:get_value(total_count, Top)
                 , Duration
                 , run_time(Name, IsRunning)
                 ]).

run_time(Name, _IsRunning = true) ->
  Start      = ponos_serv:get_start(Name),
  RunTimeSec = round(timer:now_diff(os:timestamp(), Start) / 1000 / 1000),
  Hours      = RunTimeSec div 60 div 60,
  Minutes    = RunTimeSec div 60 rem 60,
  Seconds    = RunTimeSec rem 60,
  io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hours, Minutes, Seconds]);
run_time(_, false) ->
  "00:00:00".


%% @private
pp_total(AllTop) ->
  CurrentLoad = proplists:get_value(current_load, AllTop),
  ModeledLoad = proplists:get_value(modeled_load, AllTop),
  TotalCount  = proplists:get_value(total_count, AllTop),
  Fmt = "~11s   ~-7s | ~-5.1f | ~-7.1f | ~-10B~n",
  Args = ["total:", "", CurrentLoad, ModeledLoad, TotalCount],
  io:format(Fmt, Args).

%% @private
delete_aggregates(AllTop) ->
  KeysToRemove = [current_load, modeled_load, total_count, running_tasks],
  lists:filter(fun({K, _}) -> not lists:member(K, KeysToRemove) end, AllTop).
%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
