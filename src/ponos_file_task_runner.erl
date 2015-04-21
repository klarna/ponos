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
-module(ponos_file_task_runner).
-behaviour(ponos_task_runner_callbacks).

-export([ call/3
        , concurrency_limit/2
        , init/2
        , pause/2
        , start/2
        , terminate/2
        ]).

call(Name, Task, IODevice) ->
  Result = Task(),
  log_call_after(Name, Result, IODevice).

concurrency_limit(Name, IODevice) ->
  log(Name, "hit concurrency limit", IODevice).

init(_Name, Path) ->
  AbsPath = filename:absname(Path),
  {ok, IODevice} = file:open(AbsPath, [append]),
  {ok, IODevice}.

pause(Name, IODevice) ->
  log(Name, "paused", IODevice).

start(Name, IODevice) ->
  log(Name, "started", IODevice),
  {ok, IODevice}.

terminate(Name, IODevice) ->
  log(Name, "terminated", IODevice),
  ok = file:close(IODevice).

log(Name, Msg, IODevice) ->
  NameStr = format("~p", [Name]),
  Msg1    = make_timestamp() ++ "\t" ++ NameStr ++ "\t" ++ Msg ++ "\n",
  ok      = file:write(IODevice, Msg1).

make_timestamp() ->
  Timestamp            = os:timestamp(),
  {{Y,M,D}, {H,Min,S}} = calendar:now_to_local_time(Timestamp),
  {_,_,MiS}            = Timestamp,
  Ms                   = round(MiS / 1000),
  format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w:~3.3.0w",
         [Y, M, D, H, Min, S, Ms]).

log_call_after(Name, Result, IODevice) ->
  Msg = "result: " ++ format("~p", [Result]),
  log(Name, Msg, IODevice).

format(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
