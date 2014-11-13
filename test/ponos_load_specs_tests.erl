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
%%%
%%% @copyright Klarna AB, 2014
%%% @end

%%% Module Declaration ========================================================
-module(ponos_load_specs_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Test Cases ==============================================================
make_bursts_test_() ->
  LoadSpec = ponos_load_specs:make_bursts(1, 1, 2),
  [ ?_assertEqual(0.0, LoadSpec(0))
  , ?_assertEqual(0.0, LoadSpec(999))
  , ?_assertEqual(2.0, LoadSpec(1000))
  , ?_assertEqual(2.0, LoadSpec(1999))
  , ?_assertEqual(0.0, LoadSpec(2000))
  , ?_assertEqual(2.0, LoadSpec(3000))
  ].

make_constant_load_spec_test() ->
  ?assertEqual(5.0, (ponos_load_specs:make_constant(5))(irrelevant)).

make_sawtooth_test_() ->
  LoadSpec = ponos_load_specs:make_sawtooth(10, 10),
  [ ?_assertEqual(0.0, LoadSpec(0))
  , ?_assertEqual(1.0, LoadSpec(1000))
  , ?_assertEqual(2.5, LoadSpec(2500))
  , ?_assertEqual(0.0, LoadSpec(10000))
  ].

make_staircase_test_() ->
  LoadSpec = ponos_load_specs:make_staircase(2, 3),
  [ ?_assertEqual(3.0, LoadSpec(0))
  , ?_assertEqual(3.0, LoadSpec(1))
  , ?_assertEqual(3.0, LoadSpec(1999))
  , ?_assertEqual(6.0, LoadSpec(2000))
  , ?_assertEqual(9.0, LoadSpec(5999))
  ].

make_staircase_roof_test_() ->
  LoadSpec = ponos_load_specs:make_staircase(1,1,2),
  [ ?_assertEqual(1.0, LoadSpec(0))
  , ?_assertEqual(1.0, LoadSpec(1))
  , ?_assertEqual(1.0, LoadSpec(999))
  , ?_assertEqual(2.0, LoadSpec(1000))
  , ?_assertEqual(2.0, LoadSpec(1999))
  , ?_assertEqual(2.0, LoadSpec(2000))
  , ?_assertEqual(2.0, LoadSpec(999999))
  ].

make_inverse_staircase_3_test_() ->
  LoadSpec = ponos_load_specs:make_inverse_staircase(1,1,2),
  [ ?_assertEqual(2.0, LoadSpec(0))
  , ?_assertEqual(2.0, LoadSpec(1))
  , ?_assertEqual(2.0, LoadSpec(999))
  , ?_assertEqual(1.0, LoadSpec(1000))
  , ?_assertEqual(1.0, LoadSpec(1001))
  , ?_assertEqual(1.0, LoadSpec(1999))
  , ?_assertEqual(0.0, LoadSpec(2000))
  , ?_assertEqual(0.0, LoadSpec(99999))
  ].

make_inverse_staircase_error_test_() ->
  [ ?_assertError(function_clause,
                 ponos_load_specs:make_inverse_staircase(1,1,2,3))
  ].

make_inverse_staircase_4_test_() ->
  LoadSpec = ponos_load_specs:make_inverse_staircase(1,1,2,1),
  [ ?_assertEqual(2.0, LoadSpec(0))
  , ?_assertEqual(2.0, LoadSpec(1))
  , ?_assertEqual(2.0, LoadSpec(999))
  , ?_assertEqual(1.0, LoadSpec(1000))
  , ?_assertEqual(1.0, LoadSpec(1001))
  , ?_assertEqual(1.0, LoadSpec(1999))
  , ?_assertEqual(1.0, LoadSpec(2000))
  , ?_assertEqual(1.0, LoadSpec(99999))
  ].

make_triangle_test_() ->
  LoadSpec = ponos_load_specs:make_triangle(10, 10),
  [ ?_assertEqual(0.0, LoadSpec(0))
  , ?_assertEqual(1.0, LoadSpec(500))
  , ?_assertEqual(1.5, LoadSpec(750))
  , ?_assertEqual(10.0, LoadSpec(5000))
  , ?_assertEqual(7.5, LoadSpec(6250))
  , ?_assertEqual(1.0, LoadSpec(9500))
  , ?_assertEqual(10.0, LoadSpec(15000))
  ].

make_from_tree_test_() ->
  Matrix = [{0,5.0},{1,6.0},{2,8.0},{3,20.0},{4,2.0}],
  Tree = gb_trees:from_orddict(Matrix),
  LoadSpec = ponos_load_specs:make_from_gb_tree(Tree),
  [ ?_assertEqual(5.0, LoadSpec(0))
  , ?_assertEqual(5.0, LoadSpec(500))
  , ?_assertEqual(6.0, LoadSpec(1000))
  , ?_assertEqual(8.0, LoadSpec(2000))
  , ?_assertEqual(20.0, LoadSpec(3000))
  , ?_assertEqual(2.0, LoadSpec(4000))
  , ?_assertEqual(2.0, LoadSpec(4999))
  , ?_assertEqual(0.0, LoadSpec(5000))
  , ?_assertEqual(0.0, LoadSpec(250000))
  ].

make_from_matrix_test_() ->
  Matrix = [{0,0.0},{5,10.0},{10,5.0}],
  LoadSpec = ponos_load_specs:make_from_matrix(Matrix),
  [ ?_assertEqual(0.0, LoadSpec(0))
  , ?_assertEqual(2.0, LoadSpec(1000))
  , ?_assertEqual(4.0, LoadSpec(2000))
  , ?_assertEqual(6.0, LoadSpec(3000))
  , ?_assertEqual(8.0, LoadSpec(4000))
  , ?_assertEqual(10.0, LoadSpec(5000))
  , ?_assertEqual(9.0, LoadSpec(6000))
  , ?_assertEqual(8.0, LoadSpec(7000))
  , ?_assertEqual(7.0, LoadSpec(8000))
  , ?_assertEqual(6.0, LoadSpec(9000))
  , ?_assertEqual(5.0, LoadSpec(10000))
  , ?_assertEqual(0.0, LoadSpec(11000))
  ].

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
