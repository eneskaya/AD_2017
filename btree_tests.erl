-module(btree_tests).
-include_lib("eunit/include/eunit.hrl").

isBT_test() ->
  %% Korreter BT
  B = {},
  %% Korrekter BT
  B1 = {12, 3, {}, {23, 2, {19, 1, {}, {}}, {}}},
  %% Korrekter BT
  B2 = {12, 3, {6, 1, {}, {}}, {23, 2, {19, 1, {}, {}}, {}}},
  ?assert(btree:isBT(B)),
  ?assert(btree:isBT(B1)),
  ?assert(btree:isBT(B2)),
  %% Kein korrekter BT (Höhe für Knoten 23 falsch!)
  B3 = {12, 3, {6, 1, {}, {}}, {23, 4, {19, 1, {}, {}}, {}}},
  ?assertNot(btree:isBT(B3)).
