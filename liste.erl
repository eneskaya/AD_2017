%% HAW Hamburg, AD 2017
%% @author Enes Kaya
-module (liste).
-export ([create/0, isEmpty/1, isList/1, equal/2, laenge/1, insert/3,
          delete/2, find/2, retrieve/2, concat/2, diffListe/2, get_tail_from/2]).

% Structure of List with N Elements
%
%   {Element1,                    Pos. 1
%     {Element2,                  Pos. 2
%       {Element3,                Pos. 3
%         ElementN, {}            Pos. N
%       }
%     }
%   }

%% ---------------------------------------------
%% Public API Methods
%% ---------------------------------------------

%% Constructor
%%
create() -> {}.

%% Return true, if the list L is empty. Else return false.
%%
isEmpty({}) -> true;
isEmpty({_, _})  -> false.

%% Return true, if the argument L is a list.
%%
isList({}) -> true;
isList({H, T}) -> tupleMatch({H, T}) and isList(T).

tupleMatch({NotTuple, Tuple}) ->
  not is_tuple(NotTuple) and is_tuple(Tuple).

%% Return true, if
%% - L1 and L2 are both lists
%% - L1 and L2 contain same elements at same position.
%%
%% Else return false.
%%
equal({}, {}) -> true;
equal({}, _) -> false;
equal(_, {}) -> false;
equal({Head1, Tail1}, {Head2, Tail2}) when Head1 == Head2 -> equal(Tail1, Tail2);
equal({Head1, Tail1}, {Head2, Tail2}) when Head1 /= Head2 -> false.

%% Returns the length of the list L, meaning the
%% count of elements it contains.
%%
laenge({}) -> 0;
laenge({_, Tail}) -> 1 + laenge(Tail).

%% Returns a new list with the Element inserted
%% in the list L at the given Position.
%%
insert(List, 1, Element) -> {Element, List};
insert(List, Position, Element) ->
  {Head, Tail} = List,
  NewList = {Head, insert(Tail, Position - 1, Element)},
  NewList.

%% Returns a new list with the Element deleted at
%% given Position in the list L.
%%

%% TODO Fehlerbehandlung, falls nicht vorhandene Elemente/Positionen
%% gelöscht werden sollen einfach ignorieren

delete({}, _) -> {};
delete({_, {}}, _) -> {};
delete({Head, Tail}, 1) -> Tail;
delete(List, Position) ->
  {Head, Tail} = List,
  {Head, delete(Tail, Position - 1)}.

%% Returns the position of Element in the list L.
%%
find({}, _) -> 0;
find({Head, Tail}, Element) ->
  Length = laenge({Head, Tail}),
  find({Head, Tail}, Element, Length, 1).

find({}, _, _, _) -> 0;
find({Head, Tail}, Element, Length, Counter) ->
  % io:format("Head: ~w, Element: ~w, Length: ~w, Counter: ~w\n", [Head, Element, Length, Counter]),
  if
    Head == Element ->
      Counter;
    Counter >= Length ->
      0;
    Head /= Element ->
      IncrementedCounter = Counter + 1,
      find(Tail, Element, Length, IncrementedCounter)
  end.

%% Returns the element at the given Position in the list L.
%%
retrieve({}, _) -> throw(not_found);
retrieve({Head, _}, 1) -> Head;
retrieve({Head, Tail}, Position) -> retrieve(Tail, Position - 1).

%% Returns a list containing elements of the lists L1 and L2 in
%% their respective order.
%%
concat({}, L) -> L;
concat({H1, T1}, L) -> {H1, concat(T1, L)}.

%% Returns a list cotaining all elements of L1 without elements of L2.
%%

% 1 Go through L1
% For each El of L1 check if is in L2.
% If yes, delete of L1
diffListe(L1, L2) -> {}.

eoCount(L) -> {0,0}.

%% ---------------------------------------------
%% Private Methods
%% ---------------------------------------------

get_tail_from({_, Tail}, 1) -> Tail;
get_tail_from({_, Tail}, From) -> get_tail_from(Tail, From - 1).

%% ---------------------------------------------

print_list_elements({ Element, {} }) -> io:format("~w\n", [Element]);
print_list_elements({ Element, Tail }) ->
  io:format("~w\n", [Element]),
  print_list_elements(Tail).
