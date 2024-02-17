:- initialization(main).

/* Question 2.1 */
% predicate that checks if X unifies with 0.
equals_zero(X)
  :- X == '0'.

% predicate that checks that all digits are unique.
all_different(X)
  :- sort(X, S),
     length(X, L1),
     length(S, L2),
     L1 == L2.

/* converts the digits of a four-digit number into two independent numbers,
 * such that a number N, where digits of N are 0123, is split into two
 * numbers p and q such that p = 01 and q = 23, using base 10. */
get_nums(X, R)
  :- nth0(0, X, A),
     nth0(1, X, B),
     nth0(2, X, C),
     nth0(3, X, D),
     atom_number(A, X0),
     atom_number(B, X1),
     atom_number(C, X2),
     atom_number(D, X3),
     V1 is X0*10 + X1,
     V2 is X2*10 + X3,
     R = [V1, V2].

/* Checks that P is a par, wherein a par is:
 * A number N, where N is any four digit number, makes up two 2-digit numbers
 * p and q. For example N = 9876, thus p = 98 and q = 76, N is a PAR iff all 
 * digits are unique and p mod q = 0. */
par(P)
  :- number_chars(P, P1),
     length(P1, 4),
     include(equals_zero, P1, P2),
     length(P2, 0),
     all_different(P1),
     get_nums(P1, L),
     nth0(0, L, L1),
     nth0(1, L, L2),
     0 is L1 mod L2.

/* Question 2.2 */
% generates a list of all pars.
pars(PARS)
  :- findall(X, between(1000, 9999, X), L),
     include(par, L, PARS).

/* Question 2.3 */
% finds the missing number from a party.
missing_num(L, N)
  :- number_chars(123456789, P),
     subtract(P, L, R),
     nth0(0, R, N).

/* checks that X and Y are a party, wherein a party is:
 * Two pars, X and Y, such that the digits of X and Y 
 * use all but one element in the list [1,2,3,4,5,6,7,8,9],
 * are all unique and where A and B are both divisible by
 * the missing digit, n, such that X mod n = 0 and Y mod n = 0. */
party(X, Y)
  :- number_chars(X, X1),
     number_chars(Y, Y1),
     all_different(X1),
     all_different(Y1),
     append(X1, Y1, Z),
     all_different(Z),
     missing_num(Z, N),
     atom_number(N, N1),
     0 is X mod N1,
     0 is Y mod N1.

/* Question 2.4 */
% predicate returns a list of all elements after a given element.
el_after(H, [H|T], T).
el_after(X, [_|T], R) 
  :- el_after(X, T, R).

% predicate returns a list of all elements before a given element.
el_before(X, L, R)
  :- reverse(L, L1),
     el_after(X, L1, W),
     reverse(W, R).

% predicate returns list of all elements before and after a given element.
surround(X, L, R)
  :- el_before(X, L, A),
     el_after(X, L, B),
     append(A, B, R).

/* returns all combinations of pairs of elements such that, for an element n,
 * all pairs including n and elements before and n and elements after are 
 * returned */
combination(0, _, []).
combination(N, L, [H|T])
  :- N > 0,
     surround(H, L, R),
     N1 is N-1,
     combination(N1, R, T).

% predicate checks if a pair is a party
is_party([X, Y])
  :- party(X, Y),
     party(Y, X).

/* predicate generates all partys possible by:
 * generating all pars,
 * generating a list of all possible combinations of said pars,
 * reversing the list and thus sorting by first element of each pair,
 * filtering those pairs that are partys. */
partys(PARTYS)
  :- pars(P),
     bagof(B, combination(2, P, B), R),
     reverse(R, W),
     include(is_party, W, PARTYS).

main
  :- partys(R), write(R).