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

%%% @doc Provides a set of constructors for typical load functions.
%%%
%%% A load specification defines the characteristics of load and is
%%% implemented as a function that maps time to intensity: `fun(T) -> I'
%%% where `T' is passed time in milliseconds and `I' is the intensity
%%% expressed as calls per second.
%%%
%%% `ponos_load_specs' provides constructors for typical load functions.
%%%
%%% @copyright 2014, Klarna AB
%%% @author Jonathan Olsson <jonathan@klarna.com>
%%% @author Cons Ahs <cons@klarna.com>
%%% @end

%%%_* Module Declaration ===============================================
-module(ponos_load_specs).

%%%_* Exports ==========================================================
%% API
-export([ make_bursts/3
        , make_constant/1
        , make_from_matrix/1
        , make_from_gb_tree/1
        , make_sawtooth/2
        , make_staircase/2
        , make_staircase/3
        , make_inverse_staircase/3
        , make_inverse_staircase/4
        , make_triangle/2
        ]).

%%%_* Code =============================================================
%%%_* External API -----------------------------------------------------
-spec make_bursts( non_neg_integer()
                 , non_neg_integer()
                 , ponos:intensity()) -> ponos:load_spec().
%% @doc Create a LoadSpec that generates periodic bursts.
%%
%% <dl>
%%   <dt>Period</dt>
%%   <dd>Number of seconds without load.</dd>
%%   <dt>Duration</dt>
%%   <dd>Number of seconds to maintain load.</dd>
%%   <dt>Load</dt>
%%   <dd>The load in calls per second to generate during
%%       `Duration'.
%%   </dd>
%% </dl>
make_bursts(Period, Duration, Load) ->
  Length   = (Period + Duration) * 1000,
  PeriodMs = Period * 1000,
  fun(Time) ->
      case (Time rem Length) < PeriodMs of
        true  -> 0.0;
        false -> Load + 0.0
      end
  end.

-spec make_constant(ponos:intensity()) -> ponos:load_spec().
%% @doc Create a LoadSpec that generates constant load.
%%
%% <dl>
%%   <dt>CallsPerSecond</dt>
%%   <dd>The load in calls per second to generate.</dd>
%% </dl>
make_constant(CallsPerSecond) when CallsPerSecond > 0 ->
  fun(_Time) -> CallsPerSecond + 0.0 end.

-spec make_sawtooth(non_neg_integer(), ponos:intensity()) -> ponos:load_spec().
%% @doc Create a LoadSpec that generates load according to a sawtooth
%% function.
%%
%% <dl>
%%   <dt>Length</dt>
%%   <dd>The number of seconds to reach `MaxLoad'</dd>
%%   <dt>MaxLoad</dt>
%%   <dd>Calls per second at the function's peak.</dd>
%% </dl>
make_sawtooth(Length, MaxLoad) ->
  LengthMs = Length * 1000,
  fun(Time) ->
      RelTime    = Time rem LengthMs,
      Fraction   = RelTime / LengthMs,
      _Intensity = MaxLoad * Fraction + 0.0
  end.

-spec make_staircase(non_neg_integer(), ponos:intensity()) -> ponos:load_spec().
%% @doc Create a LoadSpec that generates load according to a staircase
%% function.
%%
%% The function will increase the load indefinitely.
%%
%% <dl>
%%   <dt>Length</dt>
%%   <dd>Number of seconds to maintain current load; load will be
%%       constant during this period.
%%   </dd>
%%   <dt>LoadIncrease</dt>
%%   <dd>Calls per second load should be increased every `Length'
%%       period.
%%   </dd>
%% </dl>
make_staircase(Length, LoadIncrease) ->
  LengthMs = Length * 1000,
  fun(Time) ->
      N          = Time div LengthMs,
      _Intensity = (N+1) * LoadIncrease + 0.0
  end.

-spec make_staircase(non_neg_integer()
                     , ponos:intensity()
                     , ponos:intensity()) -> ponos:load_spec().
%% @doc Create a loadspec that generates load according to a staircase
%% function, and stops when reaching the roof.
%%
%% The function will increase the load until the roof is reached.
%%
%% <dl>
%%   <dt>Length</dt>
%%   <dd>Number of seconds to maintain current load; load will be
%%       constant during this period.
%%   </dd>
%%   <dt>LoadIncrease</dt>
%%   <dd>Calls per second load should be increased every `Length'
%%       period.
%%   </dd>
%%   <dt>Roof</dt>
%%   <dd>Calls per second that is maximum after it's reached.
%%   </dd>
%% </dl>
make_staircase(Length, LoadIncrease, Roof) ->
  LengthMs = Length * 1000,
  StepsUntilRoof = round(Roof / LoadIncrease),
  fun(Time) ->
    N = Time div LengthMs,
    case ( N < StepsUntilRoof ) of
      true -> _Intensity = (N+1) * LoadIncrease + 0.0;
      false -> _Intensity = Roof + 0.0
    end
  end.

-spec make_inverse_staircase(non_neg_integer()
                             , ponos:intensity()
                             , ponos:intensity()) -> ponos:load_spec().
%% @doc Create a LoadSpec that generates load according to an inverse
%% staircase function.
%%
%% The function will decrese the load by `LoadDecrese' every `Length' second
%% starting from `From' load.
%%
%% <dl>
%%   <dt>Length</dt>
%%   <dd>Number of seconds to maintain current load. Load will be constant
%%       during this period.
%%   </dd>
%%   <dt>LoadDecrease</dt>
%%   <dd>The load to decrease every Length second until 0 is reached.</dd>
%%   <dt>From</dt>
%%   <dd>The load to start decreasing from.</dd>
%% </dl>
make_inverse_staircase(Length, LoadDecrease, From) ->
  make_inverse_staircase(Length, LoadDecrease, From, 0.0).

-spec make_inverse_staircase(non_neg_integer()
                             , ponos:intensity()
                             , ponos:intensity()
                             , ponos:intensity()) -> ponos:load_spec().
%% @doc Create a LoadSpec that generates load according to an inverse
%% staircase function.
%%
%% The function will decrease the load by `LoadDecrease' every `Length' second
%% starting from `From' load until `To' is reached.
%%
%% <dl>
%%   <dt>Length</dt>
%%   <dd>Number of seconds to maintain current load. Load will be constant
%%       during this period.
%%   </dd>
%%   <dt>LoadDecrease</dt>
%%   <dd>The load to decrease every `Length' second until `To' is reached.</dd>
%%   <dt>From</dt>
%%   <dd>The load to start decreasing from.</dd>
%%   <dt>To</dt>
%%   <dd>The load to decrease to, and then deliver, for eternity (or
%%       when stopped).
%%   </dd>
%% </dl>
make_inverse_staircase(Length, LoadDecrease, From, To) when From >= To ->
  LengthMs = Length * 1000,
  StepsUntilFloor = round((From - To) / LoadDecrease),
  fun(Time) ->
    N = Time div LengthMs,
    case ( N < StepsUntilFloor ) of
      true -> _Intensity = From - (N * LoadDecrease + 0.0);
      false -> _Intensity = To + 0.0
    end
  end.

-spec make_triangle(non_neg_integer(), ponos:intensity()) -> ponos:load_spec().
%% @doc Create a `load_spec()' that generates load according to a
%% triangle function.
%%
%% <dl>
%%   <dt>Length</dt>
%%   <dd>The base of the triangle.</dd>
%%   <dt>MaxLoad</dt>
%%   <dd>Calls per second at the function's peak. This occurrs at 50% of
%%       `Length'
%%   </dd>
%% </dl>
make_triangle(Length, MaxLoad) ->
  LengthMs = Length * 1000,
  fun(Time) ->
     Fraction = (Time rem LengthMs / LengthMs * 2) - 1,
     _Intensity = MaxLoad - abs(MaxLoad * Fraction) + 0.0
  end.

%% @doc Create a `load_spec()' that generates load according to a
%% gb_tree
%%
%% <dl>
%%   <dt>Tree</dt>
%%   <dd>The gb_tree to create output from, should be covered each second
%%   with a specified load, otherwise 0.0 will be returned.
%%   </dd>
%% </dl>
make_from_gb_tree(Tree) ->
  fun(Time) ->
    case gb_trees:lookup(Time div 1000, Tree) of
      {value, Value} -> Value;
      none           -> 0.0
    end
  end.

-spec make_from_matrix(list()) -> ponos:load_spec().
%% @doc Create a `load_spec()' that generates load according to a
%% Matrix in the format [{0,2.0},{1,5.0},...]
%%
%% <dl>
%%   <dt>Matrix</dt>
%%   <dd>Matrix in form of a list of tuples containing {Second, Load} formatted
%%       as {non_neg_integer(), ponos:intensity()}.</dd>
%% </dl>
make_from_matrix(Matrix) ->
  make_from_gb_tree(gb_trees:from_orddict(interpolate(Matrix, []))).

%%%_* Internal API -----------------------------------------------------
interpolate([], Acc) ->
  lists:ukeysort(1, lists:flatten(Acc));
interpolate([{FromTime, FromLoad} = H1, {ToTime, ToLoad} = H2|T], Acc) ->
  case ToTime - FromTime > 1 of
    true ->
      Expanded = interpolate_load(FromTime, ToTime, FromLoad, ToLoad),
      interpolate([H2|T], [Expanded|Acc]);
    false ->
      interpolate([H2|T], [H1|Acc])
  end;
interpolate([H|T], Acc) ->
  interpolate(T, [H|Acc]).

interpolate_load(FromTime, ToTime, FromLoad, ToLoad) ->
  LoadFun =
    fun(CurrentSec, Acc) ->
      Part = CurrentSec - FromTime,
      Period = ToTime - FromTime,
      [{CurrentSec, FromLoad + (Part / Period) * (ToLoad - FromLoad)}|Acc]
    end,
  lists:foldl(LoadFun, [], lists:seq(FromTime, ToTime)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
