-module(experimente).
-export([start/0]).

-import(avl_utils, [insertRek/2, resetCounters/0, 
                    getCountersTuple/0, printCounters/0]).

-define(LEFTROTATE, leftrotate).
-define(RIGHTROTATE, rightrotate).
-define(DDLEFTROTATE, ddleftrotate).
-define(DDRIGHTROTATE, ddrightrotate).

getMilliseconds() ->
    {Megasecs, Secs, Microsecs} = erlang:timestamp(),
    (Megasecs * 1000000) + Secs + (Microsecs / 1000000).

startWithCount(Count) ->
    io:fwrite("~n ----------- ~n"),
    List = util:randomliste(Count),
    B = avltree:initBT(),
    avl_utils:resetCounters(),
    Start = getMilliseconds(),
    Big = avl_utils:insertRek(B, List),
    Stop = getMilliseconds(),
    Delta = Stop - Start,
    io:fwrite("Laufzeit für Einfügen von ~p Elementen: ~p ~n", [ Count, Delta ]),
    avl_utils:printCounters(),
    avl_utils:resetCounters().

start() ->
    startWithCount(1000),
    startWithCount(10000),
    startWithCount(100000),
    startWithCount(1000000),
    startWithCount(10000000),
    startWithCount(100000000).
