-module(stack_tests).
-include_lib("eunit/include/eunit.hrl").

createS_test() ->
  S = stack:createS(),
  ?assert(stack:isEmpty(S)).
