/* Arithmetic operations */

factorial(0,1).
factorial(N,F):- N>0, N1 is N-1, factorial(N1,F1), F is N*F1.

comb(N,0,1).
comb(N,N,1).
comb(N,K,C):- N>0, K>0, N>K, N1 is N-1, K1 is K-1, comb(N1,K,C1), comb(N1,K1,C2), C is C1+C2.



/* Some test cases

?- factorial(5, X).
X = 120.

?- comb(7,3,X).
X = 35.

?- comb(9,10,X).
false.

?- comb(6,3,20).
true.

?- comb(6,3,14).
false.

*/