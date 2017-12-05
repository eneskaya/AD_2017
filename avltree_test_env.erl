-module(avltree_test_env).
-export([getAVLTree/1, print/0]).

-import(avltree, [initBT/0, insertBT/2, printBT/2]).
-import(util, [randomliste/1]).

getAVLTree(Size) ->
    AVLTree = avltree:initBT(),
    RandomList = util:randomliste(Size),
    insertRek(AVLTree, RandomList).

insertRek(AVLTree, []) -> AVLTree;
insertRek(AVLTree, [H|T]) -> insertRek(avltree:insertBT(AVLTree, H), T).

print() ->
    BigAVLTree = getAVLTree(1000),
    avltree:printBT("VeryBig.dot", BigAVLTree).