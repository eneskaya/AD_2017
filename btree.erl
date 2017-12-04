-module (btree).
-export ([
          initBT/0, isBT/1, insertBT/2, 
          isEmptyBT/1, equalBT/2, printBT/2]).

% Beispiel B-Baum:
%
%               16
%              /   \
%            14     18
%           /  \      \
%          9   15     23
%                    /
%                   22
%
%
% Tuple Repräsentation:
%
% {16, 3,
%   {14, 2,
%     {9, 1
%       {},
%       {}
%     },
%     {15, 1,
%       {},
%       {}
%     }
%   },
%   {18, 2,
%     {},
%     {23, 2,
%       {22, 4,
%         {},
%         {}
%       }
%     }
%   }
% }

initBT() -> {}.


% ---------- isBT ----------

%% Leerer Baum ist ein Baum.
isBT({}) -> true;
%% Ein Baum nur mit Wurzel und Höhe == 1 ist ein Baum.
isBT({_, H, {}, {}}) -> H == 1;
%% Alles andere muss überprüft werden.
isBT({Value, Height, Left, Right}) ->
  {L_V, L_H} = getValueAndHeight(Left),
  {R_V, R_H} = getValueAndHeight(Right),
  ValueCorrect = middle(Value, L_V, R_V),
  if
    %% Linker Knoten leerer Baum
    L_H == 0 ->
      HeightCorrect = Height == R_H + 1;
    %% Rechter Knoten leerer Baum
    R_H == 0 ->
      HeightCorrect = Height == L_H + 1;
    true -> HeightCorrect = (max(L_H, R_H) + 1) == Height
  end,
  HeightCorrect and ValueCorrect and isBT(Left) and isBT(Right).

getValueAndHeight({}) -> {nil, 0};
getValueAndHeight({Value, Height, _, _}) -> {Value, Height}.

%% Value ist größer als LeftValue aber kleiner als RightValue
middle(Value, nil, RightValue) -> Value < RightValue;
middle(Value, LeftValue, nil) -> Value > LeftValue;
middle(Value, LeftValue, RightValue) ->
  (LeftValue < Value) and (RightValue > Value).

% ---------- insertBT ----------

%% Einfügen in leeren Baum
insertBT({}, N) -> {N, 1, {}, {}};
insertBT({E, H, Left, Right}, N) ->
  if
    %% Füge Rechts hinzu:
    N > E ->
      NewRightTree = insertBT(Right, N),
      {_, Height} = getValueAndHeight(NewRightTree),
      if
        H == Height ->
          NewHeight = Height + 1;
        true -> NewHeight = H
      end,
      {E, NewHeight, Left, NewRightTree};
    %% Füge Links hinzu:
    N < E ->
      NewLeftTree = insertBT(Left, N),
      {_, Height} = getValueAndHeight(NewLeftTree),
      if
        H == Height ->
          NewHeight = Height + 1;
        true -> NewHeight = H
      end,
      {E, NewHeight, NewLeftTree, Right};
    %% Ignorieren
    true -> {E, H, Left, Right}
  end.

% ---------- isEmptyBT ----------

isEmptyBT({}) -> true;
isEmptyBT(B) -> isBT(B) and false.

% ---------- equalBT ----------

equalBT({}, {}) -> true;
equalBT({E, H, L1, R1}, {E, H, L2, R2}) -> equalBT(L1, L2) and equalBT(R1, R2);
equalBT(_, _) -> false.

% ---------- printBT ----------

printBT(Filename, {}) ->
  writeHead(Filename),
  writeFoot(Filename);
printBT(Filename, BTree) ->
  IsBT = isBT(BTree),
  if
    IsBT -> 
      writeHead(Filename),
      startPrint(Filename, BTree),
      writeFoot(Filename);
    true -> nil
  end.

startPrint(_, {}) ->
  io:fwrite("Matched startPrint(_, {})~n");

startPrint(_, {_, _, {}, {}}) ->
  io:fwrite("Matched startPrint(_, {_, _, {}, {}})~n");

startPrint(Filename, {X, _, {}, Right}) ->
  io:fwrite("Matched startPrint(Filename, {X, _, {}, Right})~n"),
  { RightNodeValue, RightNodeHeight, _, _ } = Right,
  writeLine(Filename, {X, RightNodeValue}, RightNodeHeight),
  startPrint(Filename, Right);

startPrint(Filename, {X, _, Left, {}}) ->
  io:fwrite("Matched startPrint(Filename, {X, _, Left, {}})~n"),
  { LeftNodeValue, LeftNodeHeight, _, _ } = Left,
  writeLine(Filename, {X, LeftNodeValue}, LeftNodeHeight),
  startPrint(Filename, Left);

startPrint(Filename, {X, _, Left, Right}) ->
  { LeftNodeValue, LeftNodeHeight, _, _ } = Left,
  { RightNodeValue, RightNodeHeight, _, _ } = Right,
  writeLine(Filename, {X, LeftNodeValue}, LeftNodeHeight),
  writeLine(Filename, {X, RightNodeValue}, RightNodeHeight),
  startPrint(Filename, Left),
  startPrint(Filename, Right).

writeHead(Filename) -> util:logging(Filename, "digraph avltree \n{\n").
writeFoot(Filename) -> util:logging(Filename, "}\n").

%% Siehe String Interpolation
%% https://rosettacode.org/wiki/String_interpolation_(included)#Erlang
writeLine(Filename, {N1, N2}, Label) ->
  StringN1 = integer_to_list(N1),
  StringN2 = integer_to_list(N2),
  StringLabel = integer_to_list(Label),
  S = "~s -> ~s [label = ~s];~n",
  Formatted = lists:flatten(io_lib:format(S, [StringN1, StringN2, StringLabel])),
  util:logging(Filename, Formatted).