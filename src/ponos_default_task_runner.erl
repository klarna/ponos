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
-module(ponos_default_task_runner).
-behaviour(ponos_task_runner_callbacks).

-export([ call/3
        , concurrency_limit/2
        , init/2
        , pause/2
        , start/2
        , terminate/2
        ]).

call(_Name, Task, _State) ->
  Task(),
  ok.

concurrency_limit(_Name, _State) ->
  ok.

init(_Name, _Args) ->
  {ok, undefined}.

pause(_Name, _State) ->
  ok.

start(_Name, State) ->
  {ok, State}.

terminate(_Name, _State) ->
  ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
