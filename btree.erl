-module (btree).
-export ([initBT/0, isBT/1, insertBT/2, isEmptyBT/1, equalBT/2]).

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
    true -> HeightCorrect = (maxBT(L_H, R_H) + 1) == Height
  end,
  HeightCorrect and ValueCorrect and isBT(Left) and isBT(Right).

getValueAndHeight({}) -> {nil, 0};
getValueAndHeight({Value, Height, _, _}) -> {Value, Height}.

%% Value ist größer als LeftValue aber kleiner als RightValue
middle(Value, nil, RightValue) -> Value < RightValue;
middle(Value, LeftValue, nil) -> Value > LeftValue;
middle(Value, LeftValue, RightValue) ->
  (LeftValue < Value) and (RightValue > Value).

maxBT(A, A) -> A;
maxBT(A, B) when A > B -> A;
maxBT(A, B) when A < B -> B.

% ---------- insertBT ----------

%% Einfügen in leeren Baum
insertBT({}, N) -> {N, 1, {}, {}};
%% Nur ein Wurzelknoten
%%
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
isEmptyBT(_) -> false.

% ---------- equalBT ----------

equalBT({}, {}) -> true;
equalBT({E, H, L1, R1}, {E, H, L2, R2}) -> equalBT(L1, L2) and equalBT(R1, R2);
equalBT(_, _) -> false.
