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
  balanceIsValid({Value, Height, Left, Right}) and HeightCorrect and ValueCorrect and isBT(Left) and isBT(Right).

% ---------- Rotationen ----------

linksRotation(Node) -> io:fwrite("linksRotation ~n", []), Node.

rechtsRotation(Node) -> io:fwrite("rechtsRotation ~n", []), Node.

%% Rechts-Links-Rotation
doppeltLinksRotation(Node) -> io:fwrite("doppeltLinksRotation ~n", []), Node.

%% Links-Rechts-Rotation
doppeltRechtsRotation(Node) -> io:fwrite("doppeltRechtsRotation ~n", []), Node. 

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
          H == Height ->
            NewHeight = Height + 1,
            %% Falls die Höhe sich geändert hat, wird überprüft, ob eine Rebalancierung
            %% notwendig ist und ggf. durch Anwendung von Rotationen ausgeführt.
            checkAndRebalance({E, NewHeight, Left, NewRightTree});
          true -> {E, H, Left, NewRightTree}
        end;
      %% Füge Links hinzu:
      N < E ->
        NewLeftTree = insertBT(Left, N),
        {_, Height} = getValueAndHeight(NewLeftTree),
        if
          H == Height -> 
            NewHeight = Height + 1,
            %% s.o.
            checkAndRebalance({E, NewHeight, NewLeftTree, Right});
          true -> {E, H, NewLeftTree, Right}
        end;
      %% Alle anderen Fälle werden ignoriert
      true -> {E, H, Left, Right}
    end
  end.

%% Überprüft die Notwendigkeit einer Rebalancierung und führt diese ggf. aus.

checkAndRebalance({ E, H, L, R }) ->
  B_Ober = balanceFaktor({ E, H, L, R }),
  if
    B_Ober == -2 ->
      %% Rebalancierung im linken Teilbaum notwendig
      B_Unter = balanceFaktor(L),
      if
        B_Unter == -1 -> rechtsRotation({ E, H, L, R });
        B_Unter == 1 -> doppeltRechtsRotation({ E, H, L, R })
      end;
    B_Ober == 2 ->
      %% Rebalancierung im rechten Teilbaum notwendig
      B_Unter = balanceFaktor(R),
      if
        B_Unter == -1 -> doppeltLinksRotation({ E, H, L, R });
        B_Unter == 1 -> linksRotation({ E, H, L, R })
      end;
    true -> { E, H, L, R }
  end.

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

% ---------- Hilfs-Funktionen ----------

getValueAndHeight({}) -> {nil, 0};
getValueAndHeight({Value, Height, _, _}) -> {Value, Height}.

%% Value ist größer als LeftValue aber kleiner als RightValue
middle(Value, nil, RightValue) -> Value < RightValue;
middle(Value, LeftValue, nil) -> Value > LeftValue;
middle(Value, LeftValue, RightValue) ->
  (LeftValue < Value) and (RightValue > Value).

%% Gibt den Balance Faktor für einen (Teil-) Baum zurück.

balanceFaktor({}) -> 0;
balanceFaktor({ _, _, {}, {}}) -> 0;
balanceFaktor({ _, _, {_, HL, _, _}, {}}) -> 0 - HL;
balanceFaktor({ _, _, {}, {_, HR, _, _}}) -> HR - 0;
balanceFaktor({ _, _, {_, HL, _, _}, {_, HR, _, _}}) -> HR - HL.

balanceIsValid(Node) -> (abs(balanceFaktor(Node)) =< 1).