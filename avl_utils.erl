-module(avl_utils).
-export([getAVLTree/1, print/0, printCounters/0, resetCounters/0]).

-import(avltree, [initBT/0, insertBT/2, printBT/2]).
-import(util, [randomliste/1]).

-define(LEFTROTATE, leftrotate).
-define(RIGHTROTATE, rightrotate).
-define(DDLEFTROTATE, ddleftrotate).
-define(DDRIGHTROTATE, ddrightrotate).

getAVLTree(Size) ->
    AVLTree = avltree:initBT(),
    RandomList = util:randomliste(Size),
    insertRek(AVLTree, RandomList).

insertRek(AVLTree, []) -> AVLTree;
insertRek(AVLTree, [H|T]) -> insertRek(avltree:insertBT(AVLTree, H), T).

print() ->
    BigAVLTree = getAVLTree(1000),
    avltree:printBT("VeryBig.dot", BigAVLTree).

printCounters() ->
    Leftrotates = util:getglobalvar(?LEFTROTATE),
    Rightrotates = util:getglobalvar(?RIGHTROTATE),
    DDLeftrotates = util:getglobalvar(?DDLEFTROTATE),
    DDRightrotates = util:getglobalvar(?DDRIGHTROTATE),
    S = "LinksRot: \t\t~p ~nRechtsRot: \t\t~p ~n",
    SD = "DoppelLinksRot: \t~p ~nDoppelRechtsRot: \t~p ~n",
    io:fwrite(S, [ Leftrotates, Rightrotates ]),
    io:fwrite(SD, [ DDLeftrotates, DDRightrotates ]).


resetCounters() ->
    util:globalvarreset(?LEFTROTATE),
    util:globalvarreset(?RIGHTROTATE),
    util:globalvarreset(?DDLEFTROTATE),
    util:globalvarreset(?DDRIGHTROTATE).