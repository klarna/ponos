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
        , init/3
        , pause/3
        , start/3
        , terminate/3
        ]).

%%%_* Behaviour ========================================================
-callback call(ponos:name(), ponos:task(), State::any()) -> ok.

-callback init(ponos:name(), Args::any()) -> {ok, State::any()}.

-callback pause(ponos:name(), State::any()) -> ok.

-callback start(ponos:name(), State::any()) -> {ok, NewState::any}.

-callback terminate(ponos:name(), State::any()) -> ok.

%%%_* Code =============================================================
%%%_* External API -----------------------------------------------------
call(CbMod, LoadGenName, Task, State) ->
  CbMod:call(LoadGenName, Task, State).

init(CbMod, LoadGenName, Args) ->
  CbMod:init(LoadGenName, Args).

pause(CbMod, LoadGenName, State) ->
  CbMod:pause(LoadGenName, State).

start(CbMod, LoadGenName, State) ->
  CbMod:start(LoadGenName, State).

terminate(CbMod, LoadGenName, State) ->
  CbMod:terminate(LoadGenName, State).



%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
