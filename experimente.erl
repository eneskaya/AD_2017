-module(experimente).
-export([start/0, startBT/0]).

-import(avl_utils, [insertRek/2, resetCounters/0, 
                    getCountersTuple/0, printCounters/0]).

getMilliseconds() ->
    {Megasecs, Secs, Microsecs} = erlang:timestamp(),
    (Megasecs * 1000000) + Secs + (Microsecs / 1000000).

startWithCount(Count) ->
    io:fwrite("~n ----------- ~n"),
    List = util:randomliste(Count),
    B = avltree:initBT(),
    avl_utils:resetCounters(),
    Start = getMilliseconds(),
    avl_utils:insertRek(B, List),
    Stop = getMilliseconds(),
    Delta = Stop - Start,
    io:fwrite("Laufzeit für Einfügen von ~p Elementen: ~ps ~n", [ Count, Delta ]),
    avl_utils:printCounters(),
    avl_utils:resetCounters().

start() ->
    io:fwrite("Starte Test für AVL-Baum: ~n"),
    startWithCount(1000),
    startWithCount(10000),
    startWithCount(100000),
    startWithCount(1000000),
    startWithCount(10000000).

insertRekBTree(B, []) -> B;
insertRekBTree(B, [H|T]) -> insertRek(btree:insertBT(B, H), T).

startBTWithCount(Count) ->
    io:fwrite("~n ----------- ~n"),
    List = util:randomliste(Count),
    B = btree:initBT(),
    Start = getMilliseconds(),
    insertRekBTree(B, List),
    Stop = getMilliseconds(),
    Delta = Stop - Start,
    io:fwrite("Laufzeit für Einfügen von ~p Elementen: ~ps ~n", [ Count, Delta ]).

startBT() ->
    startBTWithCount(1000),
    startBTWithCount(10000),
    startBTWithCount(100000),
    startBTWithCount(1000000),
    startBTWithCount(10000000).

klcTest(Count) ->
    List = util:randomliste(Count),
    B = avltree:initBT(),
    