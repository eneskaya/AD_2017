-module(schlange_tests).
-include_lib("eunit/include/eunit.hrl").

createQ_test() ->
  Q = schlange:createQ(),
  ?assert(schlange:isEmptyQ(Q)).

enqueue_test() ->
  Q = schlange:createQ(),
  Q1 = schlange:enqueue(Q, 1),
  Q2 = schlange:enqueue(Q1, 2),
  Front = schlange:front(Q2),
  ?assert(not schlange:isEmptyQ(Q2)),
  ?assertEqual(1, Front).

equalQ_test() ->
  Q = schlange:createQ(),
  ?assert(schlange:equalQ(Q, Q)),
  Q1 = schlange:enqueue(Q, 1),
  Q2 = schlange:enqueue(Q1, 2),
  Q3 = schlange:enqueue(Q1, 3),
  ?assertNot(schlange:equalQ(Q1, Q2)),
  ?assertNot(schlange:equalQ(Q2, Q3)).

front_test() ->
  Q = schlange:createQ(),
  Q1 = schlange:enqueue(Q, 1),
  Q2 = schlange:enqueue(Q1, 2),
  Q3 = schlange:dequeue(Q2),
  Front = schlange:front(Q3),
  ?assertEqual(2, Front).
