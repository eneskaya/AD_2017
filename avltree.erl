-module (avltree).
-export ([initBT/0, isBT/1, insertBT/2, isEmptyBT/1, equalBT/2, printBT/2]).

% ---------- initBT ----------

initBT() -> {}.

% ---------- isBT ----------

%% Leerer Baum ist ein AVL Baum.
isBT({}) -> true;
%% Ein Baum nur mit Wurzel und Höhe == 1 ist ein AVL Baum.
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

% ---------- Rotationen ----------

% linksRotation() -> nil.

% rechtsRotation() -> nil.
% doppeltLinksRotation() -> nil. %% Rechts-Links-Rotation
% doppeltRechtsRotation() -> nil. %% Links-Rechts-Rotation

% ---------- insertBT ----------

%% Einfügen in leeren Baum
insertBT({}, N) ->
    % io:fwrite("---------- Inserting ~p in {} ---------- ~n", [N]),
    Type = util:type_is(N),
  if
    Type /= integer -> {};
    true -> {N, 1, {}, {}}
  end;

insertBT({E, H, Left, Right}, N) ->
  Type = util:type_is(N),
  if
    %% Prüfe zu aller erst ob einzufügender Wert ein Integer ist.
    %% Falls nicht, gib den Baum so zurück wie er reinkommt.
    Type /= integer -> {E, H, Left, Right};
    true ->
      % io:fwrite("---------- Inserting ~p in ~w ----------", [N, {E, H, Left, Right}]),
      if
      %% Füge Rechts hinzu:
      N > E ->
        NewRightTree = insertBT(Right, N),
        {_, Height} = getValueAndHeight(NewRightTree),
        if
          H == Height -> NewHeight = Height + 1;
          true -> NewHeight = H
        end,
        io:fwrite("---------- HEIGHT: ~p, balanceIsValid: ~p ~n", [NewHeight, balanceIsValid(Left, NewRightTree)]),
        {E, NewHeight, Left, NewRightTree};
      %% Füge Links hinzu:
      N < E ->
        NewLeftTree = insertBT(Left, N),
        {_, Height} = getValueAndHeight(NewLeftTree),
        if
          H == Height -> NewHeight = Height + 1;
          true -> NewHeight = H
        end,
        io:fwrite("---------- HEIGHT: ~p, balanceIsValid: ~p ~n", [NewHeight, balanceIsValid(NewLeftTree, Right)]),
        {E, NewHeight, NewLeftTree, Right};
      %% Alle anderen Fälle werden ignoriert
      true -> {E, H, Left, Right}
    end
  end.
  
% ---------- Hilfs-Funktionen ----------

getValueAndHeight({}) -> {nil, 0};
getValueAndHeight({Value, Height, _, _}) -> {Value, Height}.

%% Value ist größer als LeftValue aber kleiner als RightValue
middle(Value, nil, RightValue) -> Value < RightValue;
middle(Value, LeftValue, nil) -> Value > LeftValue;
middle(Value, LeftValue, RightValue) ->
  (LeftValue < Value) and (RightValue > Value).

%% Gibt den Balance Faktor für einen (Teil-) Baum zurück.
balanceFaktor({}, {}) -> 0;
balanceFaktor({_, HL, _, _}, {}) -> HL - 0;
balanceFaktor({}, {_, HR, _, _}) -> 0 - HR;
balanceFaktor({_, HL, _, _}, {_, HR, _, _}) -> HR - HL.

balanceIsValid(L, R) ->
  Faktor = abs(balanceFaktor(L, R)),
  Faktor =< 1.

% ---------- isEmptyBT ----------

isEmptyBT({}) -> true;
isEmptyBT(_) -> false.

% ---------- equalBT ----------

equalBT({}, {}) -> true;
equalBT({E, H, L1, R1}, {E, H, L2, R2}) -> equalBT(L1, L2) and equalBT(R1, R2);
equalBT(_, _) -> false.

% ---------- printBT ----------

startPrint(_, {}) -> nil;

startPrint(_, {_, _, {}, {}}) -> nil;

startPrint(Filename, {X, _, {}, Right}) ->
  { RightNodeValue, RightNodeHeight, _, _ } = Right,
  writeLine(Filename, {X, RightNodeValue}, RightNodeHeight),
  startPrint(Filename, Right);

startPrint(Filename, {X, _, Left, {}}) ->
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

writeLine(Filename, {N1, N2}, Label) ->
  StringN1 = util:to_String(N1),
  StringN2 = util:to_String(N2),
  StringLabel = util:to_String(Label),
  S = "~s -> ~s [label = ~s];~n",
  Formatted = lists:flatten(io_lib:format(S, [StringN1, StringN2, StringLabel])),
  util:logging(Filename, Formatted).

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
