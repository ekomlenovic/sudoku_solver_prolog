grille2(1, 1, [[1, 1, 0]]) :- !.
grille2(X, Y, [[X,Y,0]|L]) :- Y > 1 ,! , Ym1 is Y - 1, grille2(X, Ym1, L).
grille2(X, Y, [[X, Y, 0]|L]) :- Xm1 is X - 1, grille2(Xm1, 9, L).

grille(L) :- grille2(9, 9, L).

init_grille(B) :- grille(A), valeurs_fixees(V), init_grille2(A, B, V).

init_grille2([], [], _):- !.
init_grille2([[X,Y,_]|A], [[X,Y,V2]|B], V) :- member([X, Y, V2], V), ! ,init_grille2(A, B, V).	
init_grille2([[X,Y,_]|A], [[X,Y,0]|B], V) :- init_grille2(A ,B ,V).

carre([X, Y] , N) :- N is (3*((X-1)//3))+((Y-1)//3+1).

consistants([_, _, V], [_, _, V1]) :- V \== V1, !.
consistants([X, Y, _], [A, B, _]) :- X \== A, Y \== B, carre([X, Y], N1), carre([A, B], N2), N1 \== N2.

teste(_, []).
teste([X, Y, V],[[A, B, V1]|L]) :- consistants([X, Y, V], [A, B, V1]), teste([X, Y, V], L), !.

resoudre(S) :- init_grille(D), valeurs_fixees(Ltaboue), fixe_valeur(D,S,Ltaboue), !.

fixe_valeur([], [], _).
fixe_valeur([[X, Y, V]|L], [[X, Y, V]|R], Ltaboue):- V \== 0, !, fixe_valeur(L, R, Ltaboue).
fixe_valeur([[X, Y, _]|L], [[X, Y, V2]|R], Ltaboue):- member(V2, [1,2,3,4,5,6,7,8,9]), teste([X, Y, V2], Ltaboue), fixe_valeur(L, R, [[X, Y, V2]|Ltaboue]).

liste_valeur_possible([1,2,3,4,5,6,7,8,9]).

init_domaine(D):- init_grille(L), init_domaine2(L, L2), init_filtre(L2, D), !.

init_domaine2([], []).
init_domaine2([[X, Y, V]|L], [[[X, Y, V], [V]]|L2]):- V \== 0, !, init_domaine2(L, L2).
init_domaine2([X|L], [[X, D]|L2]):- liste_valeur_possible(D), init_domaine2(L, L2).

init_filtre([], []).
init_filtre([[[X, Y, V], [V]]|L], [[[X, Y, V], [V]]|R]):- V \==0 , !, filtre([X, Y, V], L, L2), init_filtre(L2, R).
init_filtre([X|L], [X|R]):- init_filtre(L, R).

consistants2([X1, Y1],[X2, Y2]):- X1 \== X2, Y1 \== Y2, carre([X1, Y1], N1), carre([X2, Y2], N2), N1 \== N2.

enleve(_, [], []).
enleve(X, [X|L], L) :- !.
enleve(X, [Y|L], [Y|L1]) :- enleve(X, L, L1).

filtre(_, [], []).
filtre([X1, Y1, V1], [[[X2, Y2, V2], D]|L], [[[X2, Y2, V2], D]|L2]):- consistants2([X1, Y1], [X2, Y2]), !, filtre([X1, Y1, V1], L, L2).
filtre([X1, Y1, V1], [[[X2, Y2, V2], D]|L], [[[X2, Y2, V2], D2]|L2]):- enleve(V1, D, D2), D2 \== [], filtre([X1,Y1,V1],L,L2).

fixe_valeur2([], []).
fixe_valeur2([[[X, Y, V], D]|L], [[X, Y, V2]|R]):- V =:= 0, !, member(V2, D), filtre([X, Y, V2], L, L2), fixe_valeur2(L2, R).
fixe_valeur2([[[X, Y, V], _]|L], [[X, Y, V]|R]):- fixe_valeur2(L, R).

resoudre_avec_filtrage(Sol):- init_domaine(Ei), fixe_valeur2(Ei, Sol), !.


valeurs_fixees([[1,1,4],[1,3,6],[1,7,3],[1,9,1],[2,3,3],[2,7,4],[3,1,5],[3,4,4],
[3,6,3],[3,9,8],[4,2,1],[4,4,7],[4,6,9],[4,8,2],[5,5,3],[6,2,5],
[6,4,1],[6,6,4],[6,8,8],[7,1,1],[7,4,3],[7,6,2],[7,9,7],[8,3,9],
[8,7,1],[9,1,8],[9,3,4],[9,7,2],[9,9,9]]).	


ecrit(L):-
	append(L1,LR1,L),length(L1,9),append(L2,LR2,LR1),length(L2,9),
	append(L3,LR3,LR2),length(L3,9),append(L4,LR4,LR3),length(L4,9),
	append(L5,LR5,LR4),length(L5,9),append(L6,LR6,LR5),length(L6,9),
	append(L7,LR7,LR6),length(L7,9),append(L8,L9,LR7),length(L8,9),
	write('-------------'),nl,
	affiche_une(L1),affiche_une(L2),affiche_une(L3),write('-------------'),nl,
	affiche_une(L4),affiche_une(L5),affiche_une(L6),write('-------------'),nl,
	affiche_une(L7),affiche_une(L8),affiche_une(L9),write('-------------'),nl.

affiche_une([A,Z,E,R,T,Y,U,I,O]):-
	write('|'),ecrit_un(O),ecrit_un(I),ecrit_un(U),
	write('|'),ecrit_un(Y),ecrit_un(T),ecrit_un(R),
	write('|'),ecrit_un(E),ecrit_un(Z),ecrit_un(A),write('|'),nl.
ecrit_un([_,_,V]):-write(V).