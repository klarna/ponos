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
-module(ponos_task_runner_callbacks).

%%%_* Exports ==========================================================
%% API
-export([ call/4
        , concurrency_limit/3
        , init/3
        , pause/3
        , start/3
        , terminate/3
        ]).

%%%_* Behaviour ========================================================
-callback call(ponos:name(), ponos:task(), State::any()) -> ok.

-callback concurrency_limit(ponos:name(), State::any()) -> ok.

-callback init(ponos:name(), Args::any()) -> {ok, State::any()}.

-callback pause(ponos:name(), State::any()) -> ok.

-callback start(ponos:name(), State::any()) -> {ok, NewState::any()}.

-callback terminate(ponos:name(), State::any()) -> ok.

%%%_* Code =============================================================
%%%_* External API -----------------------------------------------------

%% @doc Executed by the load generator when a task should be triggered,
%% according to the load generator's `load_spec'.
call(CbMod, LoadGenName, Task, State) ->
  CbMod:call(LoadGenName, Task, State).

%% @doc Executed by the load generator when a task was prevented from
%% being triggered by the `max_concurrent' option.
%%
%% This only applies to load generators where the `max_concurrent'
%% option has been set to anything other than the default value of `0'.
%%
%% This will only execute once every time the concurrency limit is
%% reached.
concurrency_limit(CbMod, LoadGenName, State) ->
  CbMod:concurrency_limit(LoadGenName, State).

%% @doc Executed by the load generator during the init phase (i.e. in
%% the gen_server callback).
%%
%% The return value is used as `State' for the task runner.
init(CbMod, LoadGenName, Args) ->
  CbMod:init(LoadGenName, Args).

%% @doc Executed by the load generator when beeing paused.
pause(CbMod, LoadGenName, State) ->
  CbMod:pause(LoadGenName, State).

%% @doc Executed by the load generator when starting (through
%% `ponos:init_load_generators()' or by setting the option `{auto_init,
%% true}').
%%
%% Apart from {@link init/3}, this is the only function where altering
%% the state of the task runner is allowed.
start(CbMod, LoadGenName, State) ->
  CbMod:start(LoadGenName, State).

%% @doc Executed by the load generator when terminating.
terminate(CbMod, LoadGenName, State) ->
  CbMod:terminate(LoadGenName, State).



%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
