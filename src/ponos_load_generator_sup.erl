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
%%% @private
-module(ponos_load_generator_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
%% API
-export([ start_child/1
        , start_link/0
        ]).

%% supervisor callbacks
-export([ init/1
        ]).

%%%_* Code =============================================================
%%%_* External API -----------------------------------------------------
start_child(Args) ->
  supervisor:start_child(?MODULE, [Args]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%_* Supervisor Callbacks ---------------------------------------------
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestart      = 0,
  MaxTime         = 1,
  {ok, {{RestartStrategy, MaxRestart, MaxTime}, child_specs()}}.

child_specs() ->
  [ load_generator_sup_child_spec()
  ].

load_generator_sup_child_spec() ->
  {load_generator_sup,
   {ponos_load_generator, start_link, []},
   transient, 3600, worker, [ponos_load_generator]}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
