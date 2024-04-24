/* Use of cut in prolog */

p(1).
p(2) :- !.
p(3).

diffp(1).
diffp(2).
diffp(3).

/* Some test cases

?- p(X).
X = 1.

?- p(X), X=3.
false.


?- diffp(X), X is 1+2.
X = 3.

*/