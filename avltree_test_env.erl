-module(avltree_test_env).
-export([getBigAVLTree/1, print/0]).

-import(avltree, [initBT/0, insertBT/2, printBT/2]).
-import(util, [randomliste/1]).

getBigAVLTree(Size) ->
    AVLTree = avltree:initBT(),
    RandomList = util:randomliste(Size),
    insertRek(AVLTree, RandomList).

insertRek(AVLTree, []) -> AVLTree;
insertRek(AVLTree, [H|T]) -> insertRek(avltree:insertBT(AVLTree, H), T).

print() ->
    BigAVLTree = getBigAVLTree(1000),
    avltree:printBT("VeryBig.dot", BigAVLTree).