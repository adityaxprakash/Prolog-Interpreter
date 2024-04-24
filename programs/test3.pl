/* List operations */

/*Merge sort algorithm*/
min(X, X, X).
min(X, Y, Z) :- X > Y, Z = Y.
min(X, Y, Z) :- Y > X, Z = X.

mergesort([], []).
mergesort([A], [A]).
mergesort([A,B|R], S) :-
    split([A,B|R], L1, L2),
    mergesort(L1, S1),
    mergesort(L2, S2),
    merge(S1, S2, S).

split([], [], []).
split([A], [A], []).
split([A,B|R], [A|Ra], [B|Rb]) :- split(R, Ra, Rb).

merge(A, [], A).
merge([], B, B).
merge([A|Ra], [B|Rb], [A|M]) :-  min(A, B, A), merge(Ra, [B|Rb], M).
merge([A|Ra], [B|Rb], [B|M]) :-  B < A, merge([A|Ra], Rb, M).

/* Some test cases

?- mergesort([5,34,3,10,2,10,8,9],X).      
X = [2, 3, 5, 8, 9, 10, 10, 34].


?- mergesort([2,3,1],[1,2,3]).
true.

?-  mergesort([5,4,1,3,3],[1,3,4,5]).  
false.

*/

/* Kth element of a list */
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).

/* Some test cases

?- element_at(X,[1,2,3,5,3|_],3).             
X = 3.

?- element_at(4,X,7).
X = [_, _, _, _, _, _, 4|_].

?- element_at(7,[1,7,2,3],2).
true.

?- element_at(2,[1,2,3],3).
false.

*/