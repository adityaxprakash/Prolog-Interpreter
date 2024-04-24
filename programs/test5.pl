/* Assignment 1 of the course */

mem(_,[]) :- fail. 
mem(X,[X|_]).
mem(X,[_|R]) :- mem(X,R).


/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 
del(_, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- !, del(X, R, Z).
del(X, [Y|R], [Y|Z]) :- del(X, R, Z).
  
/*  remdups(L, L1) remove duplicates from a list L to get L1 */
remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

/* Assuming no duplicates in S1, S2, here is an implementation of union of S1, S2 */
unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */
append( [ ], L, L):-!.
append( [X|R], L, [X|Z]) :- append(R, L, Z).

/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */
mapcons(_, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

/* powerI( S, P1): Here is an implementation of powerset of S */
powerI([ ], [ [ ] ]):-!.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).


/*Intersection of two sets*/
interI([],_,[]):-!.
interI([X|S1],S2,S3):- mem(X,S2),interI(S1,S2,R), unionI([X],R,S3_),eqset(S3_,S3),!.
interI([X|S1],S2,S3):- \+ mem(X,S2), interI(S1,S2,S3_),eqset(S3_,S3),!.

/*Difference of two sets*/
diffI([],_,[]):-!.
diffI(X,[],X):-!.
diffI([X|S1],S2,S3):- mem(X,S2),diffI(S1,S2,S3_),eqset(S3_,S3),!.
diffI([X|S1],S2,S3):- \+ mem(X,S2), diffI(S1,S2,L3), unionI([X],L3,S3_),eqset(S3_,S3), !.


eqset([],[]).
eqset(S,S).
eqset([X|S1],S2):- mem(X,S2), del(X,S2,S2_),eqset(S1,S2_),!.


/* Some Test cases 

?- interI([1,2,3,4],[4,3,2],X). 
X = [2, 3, 4].

?- unionI([3,2,1,4],[5,6,1],X).
X = [3, 2, 1, 4, 5, 6].


?-  powerI([1,3],X).
X = [[1, 3], [1], [3], []].


?- diffI([1,2,3],[1,2],X).
X = [3].

?- interI([1,2,3],[2,3],[1,2,3]).
false.


*/