/* Simple Predicate Logic */

male(john).
male(harry).
male(bob).

female(jane).
female(sue).
female(ann).

parent(john, jane).
parent(jane, sue).
parent(harry, sue).
parent(sue, ann).

father(X, Y):-parent(X,Y), male(X).
mother(X, Y):-parent(X,Y), female(X).

spouse(X,Y):-parent(X,Z), parent(Y,Z), X\=Y.

grandparent(X, Y):-parent(X, Z), parent(Z, Y).

/* Some test cases 

?- spouse(jane,X).
X = harry.

?- mother(X, ann).
X = sue.

?- grandparent(X,Y).
X = john, Y = sue.

?- parent(sue, john).
false.

?- father(sue,ann).
false.


*/
