:- initialization(main).

/* Question 1.1 */
% all of these atoms are animals.
animal(E)
  :- member(E, [aardvark, antelope, coyote, dingo, donkey, elephant, horse, jaguar, kangaroo]).

% all of these atoms are vegetables.
vegetable(E)
  :- member(E, [artichoke, cabbage, carrot, celery, leek, lettuce, marrow, onion, potato]).

% all of these atoms are minerals.
mineral(E)
  :- member(E, [anatase, basalt, cobalt, copper, galena, nickel, sodium, silver, zircon]).

/* Question 1.2 */
% converts an atom into a list of characters.
spell( E, X )
  :- atom_chars(E, X).

/* Question 1.3 */
% checks that an atom is the correct length.
correctLength(N, X)
  :- spell(X, Y),
     length(Y, N).

% checks that N1 unifies with N2.
checkLetters(N1, N2, X)
  :- spell(X, Y),
     nth1(N1, Y, L1),
     nth1(N2, Y, L2),
     L1 = L2.

% solves the niner puzzle.
main
  :- A = [aardvark, antelope, coyote, dingo, donkey, elephant, horse, jaguar, kangaroo],
     V = [artichoke, cabbage, carrot, celery, leek, lettuce, marrow, onion, potato],
     M = [anatase, basalt, cobalt, copper, galena, nickel, sodium, silver, zircon],
     include(correctLength(6), A, A1),
     include(correctLength(6), V, V1),
     include(correctLength(7), M, M1),
     include(checkLetters(2, 4), A1, A2),
     include(checkLetters(2, 6), V1, V2),
     include(checkLetters(1, 3), M1, M2),
     nth1(1, A2, A3),
     spell(A3, A4),
     nth1(1, V2, V3),
     spell(V3, V4),
     nth1(1, M2, M3),
     spell(M3, M4),
     nth1(6, M4, L1),
     nth1(3, A4, L2),
     nth1(2, M4, L3),
     nth1(1, A4, L4),
     nth1(2, V4, L5),
     nth1(1, V4, L6),
     nth1(1, M4, L7),
     nth1(5, A4, L8),
     nth1(7, M4, L9),
     S = [L1, L2, L3, L4, L5, L6, L7, L8, L9],
     atomic_list_concat(S, S1),
     write(S1).