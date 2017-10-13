-module(schlange_tests).
-include_lib("eunit/include/eunit.hrl").

createQ_test() ->
  Q = schlange:createQ(),
  ?assert(schlange:isEmptyQ(Q)).
