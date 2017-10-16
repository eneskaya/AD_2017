-module (schlange).
-export ([createQ/0, enqueue/2, dequeue/1, isEmptyQ/1,
          equalQ/2, front/1]).

createQ() ->
  InStack = stack:createS(),
  OutStack = stack:createS(),
  {InStack, OutStack}.

enqueue({In, Out}, Element) ->
  NewIn = stack:push(In, Element),
  {NewIn, Out}.

dequeue({{}, Out}) -> {stack:createS(), stack:pop(Out)};
dequeue({In, {}}) -> {stack:createS(), stack:pop(stack:reverseS(In))};
dequeue({{}, {}}) -> createQ().

isEmptyQ({In, Out}) ->
  InIsEmpty = stack:isEmptyS(In),
  OutIsEmpty = stack:isEmptyS(Out),
  InIsEmpty and OutIsEmpty.

equalQ({In1, Out1}, {In2, Out2}) ->
  InStacksAreEqual = stack:equalS(In1, In2),
  OutStacksAreEqual = stack:equalS(Out1, Out2),
  InStacksAreEqual and OutStacksAreEqual.

front({{}, Out}) -> stack:top(Out);
front({In, {}}) -> stack:top(stack:reverseS(In));
front({{}, {}}) -> throw(queue_empty).
