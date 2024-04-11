/* -------------------------------------------------------------------------------- */
/*
Question 1.

This clause computes the set difference between two lists. The set difference
consists of the elements that are in the first list but not in the second list.

The clause consists of three parameters.
- Parameter 1 represents the list S1.
- Parameter 2 represents the list S2.
- Parameter 3 represents the set difference of S1 and S2.
*/
setDifference([], _, []).
setDifference([Head|Tail], S2, [Head|ResultTail]) :-
    \+ member(Head, S2),
    setDifference(Tail, S2, ResultTail).
setDifference([Head|Tail], S2, Result) :-
    member(Head, S2),
    setDifference(Tail, S2, Result).

/* -------------------------------------------------------------------------------- */
/*
Question 2.

This clause swaps each pair of elements.

The clause consists of two parameters.
- Parameter 1 is the input list to swap
- Parameter 2 is the output list with each pair of elements swapped
*/
swap([], []).
swap([X, Y | Tail], [Y, X | SwappedTail]) :- swap(Tail, SwappedTail).
swap([X], [X]).

/* -------------------------------------------------------------------------------- */
/*
Question 3.
*/
/*
This clause takes a nested list and flattens it so that it becomes an unnested
list. This clase consists of two parameters:
    Param 1: The nested list that we want to flatten.
    Param 2: The output which is the flattened list.
*/
flattenList([], []).
flattenList([Head | Tail], FlatList) :-
    is_list(Head),
    flattenList(Head, FlatHead),
    flattenList(Tail, FlatFail),
    append(FlatHead, FlatFail, FlatList).
flattenList([Head | Tail], [Head | FlatTail]) :-
    \+ is_list(Head),
    flattenList(Tail, FlatTail).

/*
This clause evaluates to true or false when a certain condition is met. This
clause consists of three parameters:
    E: This is an atom in the list we want to filter.
    Param 2: The operation we want to perform to filter each element. Could
        be either equal, greaterThan, or lessThan.
    N: The number we want to compare to.
*/
filterCondition(E, equal, N) :- E =:= N.
filterCondition(E, greaterThan, N) :- E > N.
filterCondition(E, lessThan, N) :- E < N.

/*
This clause goes through an un-nested list and filters it according to a
filter condition. This clause contains 4 parameters:
    Param 1: This is the un-nested list in which we want to filter.
    OP: This is the operation we want to perform to filter the list. It
        can be either equal, greaterThan, or lessThan.
    N: This is the value we want to compare against.
    Param 4: This is the result of filtering the list from param 1.
*/
filterList([], _, _, []).
filterList([H|T], OP, N, [H|R]) :-
    filterCondition(H, OP, N),
    filterList(T, OP, N, R).
filterList([H|T], OP, N, R) :-
    \+ filterCondition(H, OP, N),
    filterList(T, OP, N, R).

/*
This clause filters a possibly nested list and returns a flat list
with its elements filtered. This clause consists of 4 parameters:
    L: This is a possibly nested list in which we want to filter.
    OP: This is the operation we want to perform to filter the elements
        in L. OP can be either equal, greaterThan, or lessThan.
    N: This is the number in which we want to compare against when
        performing the operations.
    R: This is a flat list containing the filtered elements from L.
*/
filter(L, OP, N, L1) :-
    flattenList(L, FlatList),
    filterList(FlatList, OP, N, L1).

/* -------------------------------------------------------------------------------- */
/*
Question 4.
*/
/*
This clause first creates an intermediate data structure storing the elements in L without
duplicates. The countEach helper clause will then be invoked on this intermediate data
structure. This clause has two parameters:
    L: This is the list that gets passed by the user. It's the list we want to perform out
        computations on.
    R: This is an unsorted list of pairs. Each pair contains the atom and the number of
        occurrences.
*/
countOccurrences(L, R) :-
    sort(L, UniqueAtoms),  % This is to remove duplicates. Sorting will be done properly later.
    countEach(UniqueAtoms, L, R).

/*
This clause counts the number of occurrences of each element. This clause has three parameters:
    Param 1: This is a non-nested list containing each unique atom to consider.
    L: This is the original list the user passed in.
    Param 3: This is an unsorted list of pairs. Each pair contains the atom and the number
        of occurrences.
*/
countEach([], _, []).
countEach([H|T], L, [[H, Count]|R]) :-
    countAtom(H, L, Count),
    countEach(T, L, R).

/*
This clause counts the number of occurrences of an atom. It filters the list of elements and
then computes the length of the filtered list. this clause contains three parameters:
    Atom: We want to keep elements in the list that are equal to Atom.
    List: This is the list we want to filter.
    Count: This is a number representing the number of occurrences of Atom.
*/
countAtom(Atom, List, Count) :-
    include(==(Atom), List, Filtered),  % Filters List with only elements matching Atom.
    length(Filtered, Count).  % Counts the num of elements by finding length of filtered list.

/*
This clause defines the predicate used to sort the final result containing pairs. This
predicate will be invoked in the predsort/3 function.
*/
comparePairs(Order, [Atom1, Num1], [Atom2, Num2]) :-
    compare(NumOrder, Num1, Num2),
    (NumOrder == '=' ->
        compare(Order, Atom1, Atom2);
        Order = NumOrder).

/*
This clause sorts the final result. This clause contains two parameters:
    L: The unsorted list we want to sort.
    S: The final result. This list is sorted.
*/
sortPairs(L, S) :-
    predsort(comparePairs, L, S).

/*
This clause counts the number of occurrences of each element in a flat list. This clause
consists of 2 parameters.
    L: This is a flat list that we want to count the number of occurrences of.
    N: this is a sorted list containing a list of pairs. In each pair the first element
        is an atom. The second element is a number representing the number of
        occurrences of that element.
*/
countAll(L, N) :-
    countOccurrences(L, X),
    sortPairs(X, N).

/* -------------------------------------------------------------------------------- */
/*
Question 5.

This clause substitutes values in a list. This predicate contains 3 parameters:
    Param 1: This is a possibly nested list of atoms.
    S: This is a list of pairs in the form [[x1,e1],[x2,e2]...]
    Param 3: This is the final result list with elements substituted.
*/
sub([], _, []).
sub([H|T], S, [H1|T1]) :-
    atom(H),
    (member([H, E], S) ->
        H1 = E;
        H1 = H
    ),
    sub(T, S, T1).
sub([H|T], S, [H1|T1]) :-
    is_list(H),
    sub(H, S, H1),
    sub(T, S, T1).

/* -------------------------------------------------------------------------------- */
/*
Question 6.
*/
/*
This predicate collects all nodes in the graph and puts them in the Nodes variable.
*/
allNodes(Nodes) :-
    findall(Node, node(Node), Nodes).

/*
This predicate generates all possible subsets from a list of nodes. This predicate
contains 2 parameters:
    Param 1: This is a list of nodes.
    Param 2: This is the power set of the list of nodes.
*/
subsets([], []).
subsets([H|T], [H|R]) :-
    subsets(T, R).
subsets([_|T], R) :-
    subsets(T, R).

/*
This predicate verifies if a list of nodes is a clique. In order for this predicate
to return true each node in the list must be connected to every other node in the
list. This predicate has 1 parameter which is the list of nodes.
*/
isClique([]).
isClique([H|T]) :-
    connectToAll(H, T),
    isClique(T).

/*
This predicate checks if a singular node is connected to every other node in a list.
This predicate has 2 parameters:
    param 1: The node we want to check.
    param 2: The list of nodes we want to check whether node is connected to.
*/
connectToAll(_, []).
connectToAll(Node, [H|T]) :-
    edge(Node, H); edge(H, Node),
    connectToAll(Node, T).

/*
This predicate finds all cliques within a graph. The final result is stored in the
variable L.
*/
clique(L) :-
    allNodes(Nodes),
    subsets(Nodes, L),
    isClique(L).

/* -------------------------------------------------------------------------------- */
/*
Question 7.
*/
use_module(library(lists)).

/*
This is the main predicate that parses the input list according to the specifications.
This predicate has two parameters:
    Term: The list represented string that we want to parse.
    Result: The final result of parsing the list represented string.
*/
convert(Term, Result) :-
    convertHelper(Term, [], Result).

/*
This is the main helper function for the convert predicate. This function handles
all of the parsing and calls any helper functions whenever needed. This predicate
has 3 parameters:
    Param 1: This is the term that we want to parse.
    Param 2: This is an accumulator variable that stores the final result.
    Param 3. This is the final result of parsing the list represented string.
*/
convertHelper([], Acc, Result) :-
    /*
        The reason I reverse the accumulator variable is because elements are
        appended from the front.
    */
    reverse(Acc, Result).
convertHelper(['q'|T], Acc, Result) :-
    hasQ(T),
    processQuote(T, QContent, Rest),
    reverse(['q'|QContent], QContentRev),
    append(QContentRev, Acc, NewAcc),
    convertHelper(Rest, NewAcc, Result).
convertHelper(['q'|T], Acc, Result) :-
    \+ hasQ(T),
    convertHelper(T, ['q'|Acc], Result).
convertHelper([e|T], Acc, Result) :-
    convertHelper(T, Acc, Result).
convertHelper([H|T], Acc, Result) :-
    H \= e,
    H \= 'q',
    convertHelper(T, [w|Acc], Result).

/*
This function checks whether a term contains q. This is used mainly in the
helper function above on whether it is processing a terms between two quotes.
*/
hasQ(['q'|T]).
hasQ(['q']).
hasQ([H|T]) :-
    H \= 'q',
    hasQ(T).

/*
This function returns elements in a term between two quotes.
*/
processQuote([], [], []).
processQuote(['q'|T], ['q'], T).
processQuote([H|T], [H|QContent], Rest) :-
    processQuote(T, QContent, Rest).

/* -------------------------------------------------------------------------------- */