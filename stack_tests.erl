-module(stack_tests).
-include_lib("eunit/include/eunit.hrl").

createS_test() ->
  S = stack:createS(),
  ?assert(stack:isEmptyS(S)).

push_test() ->
  S = stack:createS(),
  S1 = stack:push(S, 1),
  Top = stack:top(S1),
  ?assertEqual(1, Top).

pop_test() ->
  S = stack:createS(),
  S1 = stack:push(S, 1),
  S2 = stack:pop(S1),
  ?assert(stack:equalS(S, S2)).

equalS_test() ->
  S = stack:createS(),
  ?assert(stack:equalS(S, S)),
  S1 = stack:push(S, 1),
  S2 = stack:push(S, 2),
  ?assertNot(stack:equalS(S1, S2)).

reverseS_test() ->
  S = stack:createS(),
  S1 = stack:push(S, 1),
  S2 = stack:push(S1, 2),
  S_Reverse = stack:reverseS(S2),
  S3 = stack:push(S, 2),
  S4 = stack:push(S3, 1),
  ?assert(stack:equalS(S4, S_Reverse)).
