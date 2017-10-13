-module(liste_tests).
-include_lib("eunit/include/eunit.hrl").

-define(L_EMPTY, {}).
-define(L1, {1, {}}).
-define(L2, {2, {3, {}}}).
-define(L3, {4, {5, {6, {}}}}).

%% ---------------------------------------------
%% Tests
%% ---------------------------------------------

equal_test() ->
  ?assert(liste:equal(?L_EMPTY, ?L_EMPTY)),
  ?assert(liste:equal(?L1, ?L1)),
  L_A = {1, {}}, L_B = L_A,
  ?assert(liste:equal(L_A, L_B)),
  ?assert(liste:equal(L_B, L_A)),
  ?assertNot(liste:equal({}, ?L1)),
  ?assertNot(liste:equal(?L1, {})),
  ?assertNot(liste:equal(?L2, ?L1)).

isEmpty_test() ->
  ?assert(liste:isEmpty(?L_EMPTY)),
  ?assertNot(liste:isEmpty(?L1)).

isList_test() ->
  ?assert(liste:isList(?L_EMPTY)),
  ?assert(liste:isList(?L1)),
  ?assert(liste:isList(?L2)),
  ?assert(liste:isList(?L3)),
  ?assertError(_, liste:isList({1, 2, 3})),
  ?assertError(_, liste:isList(a)),
  ?assertError(_, liste:isList(1)).

laenge_test() ->
  ?assertEqual(0, liste:laenge(?L_EMPTY)),
  ?assertEqual(1, liste:laenge(?L1)),
  ?assertEqual(2, liste:laenge(?L2)),
  ?assertEqual(3, liste:laenge(?L3)),
  ?assertError(_, liste:laenge(error)).

insert_test() ->
  ?assertEqual(1, liste:laenge(liste:insert(?L_EMPTY, 1, 1))),
  L2_i = liste:insert(?L2, 1, 1), % should result in {1, {2, {3, {}}}}
  Expected = {1, {2, {3, {}}}},
  ?assert(liste:equal(L2_i, Expected)),
  L3_i = liste:insert(?L3, 4, 7),
  Expected_2 = {4, {5, {6, {7, {}}}}},
  ?assert(liste:equal(L3_i, Expected_2)).

find_test() ->
  Pos1 = liste:find(?L1, 1),
  ?assertEqual(1, Pos1),
  Pos2 = liste:find(?L1, 3),
  ?assertEqual(0, Pos2), % not found case
  Pos3 = liste:find(?L3, 6),
  ?assertEqual(3, Pos3).

retrieve_test() ->
  El1 = liste:retrieve(?L1, 1),
  ?assertEqual(1, El1),
  El2 = liste:retrieve(?L2, 2),
  ?assertEqual(3, El2),
  ?assertThrow(not_found, liste:retrieve(?L3, 1)). % not found case

concat_test() ->
  L_Concat = liste:concat(?L1, ?L2),
  ?assert(liste:equal(L_Concat, {1, {2, {3, {}}}})),
  L_Concat_2 = liste:concat(?L_EMPTY, ?L_EMPTY),
  ?assert(liste:equal(L_Concat_2, ?L_EMPTY)).

diffListe_test() ->
  L_Diff = liste:diffListe(?L1, ?L1),
  ?assert(liste:equal(L_Diff, ?L_EMPTY)),
  L_Diff_2 = liste:diffListe(?L3, {5, {6, {}}}),
  ?assert(liste:equal(L_Diff_2, {4, {}})).
