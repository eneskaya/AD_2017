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

insertion_test() ->
  B = btree:initBT(),
  B1 = btree:insertBT(B, 23),
  ?assert(btree:isBT(B1)),
  B2 = btree:insertBT(B1, 19),
  ?assert(btree:isBT(B2)),
  B3 = btree:insertBT(B2, 25),
  ?assert(btree:isBT(B3)),
  %% Doppelter insert wird ignoriert (Bäume sind immer noch gleich)
  B4 = btree:insertBT(B3, 25),
  ?assert(btree:equalBT(B4, B3)).

printBT_test() ->
  B = btree:initBT(),
  B1 = btree:insertBT(B, 6),
  B2 = btree:insertBT(B1, 2),
  B3 = btree:insertBT(B2, 8),
  B4 = btree:insertBT(B3, 1),
  B5 = btree:insertBT(B4, 4),
  B6 = btree:insertBT(B5, 7),
  B7 = btree:insertBT(B6, 9),
  B8 = btree:insertBT(B7, 0),
  B9 = btree:insertBT(B8, 3),
  B10 = btree:insertBT(B9, 5),
  btree:printBT("printBT.dot", B10).
