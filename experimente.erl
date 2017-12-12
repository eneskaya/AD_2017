-module(experimente).
-export([start/0, startBT/0]).

-import(math, [pow/2]).
-import(avl_utils, [insertRek/2, resetCounters/0, 
                    getCountersTuple/0, printCounters/0]).

getMilliseconds() ->
    {Megasecs, Secs, Microsecs} = erlang:timestamp(),
    (Megasecs * 1000000) + Secs + (Microsecs / 1000000).

startWithCount(Count) ->
    StartzeitStr = lists:concat(["Startzeit: ", util:timeMilliSecond(), "----------- ~n"]),
    io:fwrite(StartzeitStr),
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
    startWithCount(trunc(math:pow(2,10))),
    startWithCount(trunc(math:pow(2,12))),
    startWithCount(trunc(math:pow(2,14))),
    startWithCount(trunc(math:pow(2,16))),
    startWithCount(trunc(math:pow(2,18))),
    startWithCount(trunc(math:pow(2,20))),
    startWithCount(trunc(math:pow(2,22))),
    startWithCount(trunc(math:pow(2,24))),
    startWithCount(trunc(math:pow(2,26))),
    startWithCount(trunc(math:pow(2,28))),
    startWithCount(trunc(math:pow(2,30))),
    startWithCount(trunc(math:pow(2,32))).


insertRekBTree(B, []) -> B;
insertRekBTree(B, [H|T]) -> insertRek(btree:insertBT(B, H), T).

startBTWithCount(Count) ->
    StartzeitStr = lists:concat(["Startzeit: ", util:timeMilliSecond(), "----------- ~n"]),
    io:fwrite(StartzeitStr),
    List = util:randomliste(Count),
    B = btree:initBT(),
    Start = getMilliseconds(),
    insertRekBTree(B, List),
    Stop = getMilliseconds(),
    Delta = Stop - Start,
    io:fwrite("Laufzeit für Einfügen von ~p Elementen: ~ps ~n", [ Count, Delta ]).

startBT() ->
    startBTWithCount(trunc(math:pow(2,10))),
    startBTWithCount(trunc(math:pow(2,12))),
    startBTWithCount(trunc(math:pow(2,14))),
    startBTWithCount(trunc(math:pow(2,16))),
    startBTWithCount(trunc(math:pow(2,18))),
    startBTWithCount(trunc(math:pow(2,20))),
    startBTWithCount(trunc(math:pow(2,22))),
    startBTWithCount(trunc(math:pow(2,24))),
    startBTWithCount(trunc(math:pow(2,26))),
    startBTWithCount(trunc(math:pow(2,28))),
    startBTWithCount(trunc(math:pow(2,30))),
    startBTWithCount(trunc(math:pow(2,32))).

% klcTest(Count) ->
%     List = util:randomliste(Count),
%     B = avltree:initBT(),
    