-module(avltree_tests).
-include_lib("eunit/include/eunit.hrl").

-import(avl_utils, [getAVLTree/1]).

isBT_test() ->
  %% Korreter AVL Tree
  B = {},
  %% Inkorrekter AVL Tree, imbalance
  B1 = {12, 3, {}, {23, 2, {19, 1, {}, {}}, {}}},
  %% Korrekter AVL Tree
  B2 = {12, 3, {6, 1, {}, {}}, {23, 2, {19, 1, {}, {}}, {}}},
  ?assert(avltree:isBT(B)),
  ?assertNot(avltree:isBT(B1)),
  ?assert(avltree:isBT(B2)),
  %% Kein korrekter AVL Tree (Höhe für Knoten 23 falsch!)
  B3 = {12, 3, {6, 1, {}, {}}, {23, 4, {19, 1, {}, {}}, {}}},
  ?assertNot(avltree:isBT(B3)).

insertion_test() ->
  B = avltree:initBT(),
  B1 = avltree:insertBT(B, 23),
  ?assert(avltree:isBT(B1)),
  B2 = avltree:insertBT(B1, 19),
  ?assert(avltree:isBT(B2)),
  B3 = avltree:insertBT(B2, 25),
  ?assert(avltree:isBT(B3)),
  %% Doppelter insert wird ignoriert (Bäume sind immer noch gleich)
  B4 = avltree:insertBT(B3, 25),
  ?assert(avltree:equalBT(B4, B3)).

insertionBig_test() ->
  Big = avl_utils:getAVLTree(100000),
  ?assert(avltree:isBT(Big)).

printBT_test() ->
  B = avltree:initBT(),
  B1 = avltree:insertBT(B, 6),
  B2 = avltree:insertBT(B1, 2),
  B3 = avltree:insertBT(B2, 8),
  B4 = avltree:insertBT(B3, 1),
  B5 = avltree:insertBT(B4, 4),
  B6 = avltree:insertBT(B5, 7),
  B7 = avltree:insertBT(B6, 9),
  B8 = avltree:insertBT(B7, 0),
  B9 = avltree:insertBT(B8, 3),
  B10 = avltree:insertBT(B9, 5),
  ?assertEqual(avltree:printBT("printBT.dot", B10), ok).

delete_test() ->
  List = util:randomliste(100),
  AVL = avl_utils:insertRek(avltree:initBT(), List),
  