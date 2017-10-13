%% HAW Hamburg, AD 2017
%% @author Enes Kaya
-module (stack).
-export ([createS/0, push/2, pop/1, isEmptyS/1,
          top/1, equalS/2, reverseS/1]).

createS() -> liste:create().

push(S, Element) -> liste:insert(S, 1, Element).

pop(S) -> liste:delete(S, 1).

top(S) -> liste:retrieve(S, 1).

isEmptyS(S) -> liste:isEmpty(S).

equalS(S1, S2) -> liste:equal(S1, S2).

reverseS(S) -> reverseS(S, createS()).

reverseS(S, N) ->
  EmptyS = isEmptyS(S),
  if
    EmptyS ->
      N;
    not EmptyS ->
      reverseS(pop(S), push(N, top(S)))
  end.
