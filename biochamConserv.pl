% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2006-2010, INRIA, Projet Contraintes
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
%
% GNU prolog file biochamConserv.pl
% by Sylvain Soliman

:-dynamic(k_conservation/1).  % mass conservation stoichiometric lists

% remove 'fake' molecules that were eliminated by an invariant
remove_invariants(ML,L,ML2):-
   findall(M,k_macro([M],_),L),
   length(L,N),
   format_debug(3,
      "~w molecules eliminated by conservation laws:~n~w~n",[N,L]),
   delete_rec(ML,L,ML2).

% add an invaraint
conservation(A):-
   get_molecules(A,AA),
   normalize(AA,LA),
   assertset(k_conservation(LA)),
   (
        have_gui
                ->
                        format("[GUI] conservationLaws Add: ~w~n",[LA])
                ;
                        true
   ),
   check_conserv(LA),
  (
        have_gui
                ->
                        format("[GUI] conservationLaws Stop ~n",[])
                ;
                        true
   ).

% TODO attention 2 molecules in common -> crossed macros :(
normalize(L1, [MM*M| L2]):-
   sort(L1),
   member(MM*M, L1),
   \+((k_conservation(L), member(_*M, L))),
   select(MM*M, L1, L2), !.

normalize(_, _):-
  
        (
                have_gui
                        ->
                                format("[GUI] conservationLaws Check: Error: All the molecules in this conservation law already appear in other laws~n",[])
                        ;
   write_line_col('Error'),
                                 format("all the molecules in this conservation law already appear in other laws~n",[])
               ),
   fail.

% remove one
delete_conservation(A):-
(
                have_gui
                        ->
                                format("[GUI] conservationLaws Delete: ~w is being deleted.~n",[A])
                        ;
                                true
               ),
   get_molecules(A,AA),
   sort(AA),
   k_conservation([MM*M|L]),
   select(MM*M,AA,L),
   retractall(k_conservation([MM*M|L])).

% remove all
delete_conservations:-
(
                have_gui
                        ->
                                format("[GUI] conservationLaws DeleteAll:~n",[])
                        ;
                                true
               ),
   retractall(k_conservation(_)).

% list invariants
list_conservations:-
   writeall(user_output,k_conservation(A),conservation(A)).

% create macros for invariants
% first clean old such macros
do_conservation:-
   retractall(k_macro([_],_)),
   fail.

% then treat each law
do_conservation:-
   k_conservation(A),
   conserve(A),
   fail.

do_conservation.

% treat one conservation law
conserve(L):-
   get_total_molecules(L,CC),  % sum of initial # mol
   L=[MM*H|T],
   mult_volumes(T,TTT),
   build_sum(TTT,TT),     % get the term for the sum of the other molecules
   format_debug(
      3,
      "In ~w consider ~w as macro to maintain total equal ~w~n",
      [L, H, CC]
   ),
   (
      (
         H = '::'(_,Loc),
         k_volume(Loc,Vol)
      )
   ->
      simplify((CC-TT)/(MM*Vol), HH),
      format_debug(3, "~w = ~w~n", [[H], HH]),
      (
         have_gui
      ->
         format("[GUI] dontAddMacro ~w~n",[[H],HH])
      ;
         true
      ),
      macro([H], HH)
   ;
      simplify((CC-TT)/MM, HH),
      format_debug(3, "~w = ~w~n", [[H], HH]),
      (
         have_gui
      ->
         format("[GUI] dontAddMacro ~w~n",[[H],HH])
      ;
         true
      ),
      macro([H], HH)
   ).

% sum initial # mol
get_total_molecules(L,C):-
   findall(C,(
      member(MM*M,L),
      initconc(M,CC),
      (
         (
            M = '::'(_,Loc),
            k_volume(Loc,Vol)
         )
      ->
         C = MM*CC*Vol
      ;
         C = MM*CC
      )
   ),CL), % get all initial # mol.
   build_sum(CL,CC),  % sum over the list
   eval(CC,[],C).

% check conservation
check_conservations:-
   k_conservation(A),
   write(A),write(' '),
        (
                have_gui
                        ->
                                format("[GUI] conservationLaws Check: ~w~n",[A])
                        ;
                                true
              ),        
   check_conserv(A),
   fail.

check_conservations.

check_conserv(A):-
   (
      check_conserv_rules(A)
   ->
     
        (
                have_gui
                        ->
                                format("[GUI] conservationLaws Check: ~w : Checked from rules~n",[A])
                        ;
      format("checked from rules~n",[])
               )
   ;
      (
         check_conserv_num(A)
      ->
        
        (
                have_gui
                        ->
                                format("[GUI] conservationLaws Check: ~w : Checked from kinetics~n",[A])
                        ;
         format("checked from kinetics~n",[])
               )
      ;
        
        (
                have_gui
                        ->
                                format("[GUI] conservationLaws Check: ~w : Trusted but not checked.~n",[A])
                        ;
         format("trusted but not checked!~n",[])
               )
      )
   ).

check_conserv_rules(L):-
   findall((KL,KR),(
      rule(_,KL,KR,_,_,_,_),
      append(KL,KR,KK),
      member(_*M,L),
      member((_,M),KK)),LL),
   check_conserv_rules(LL,L).
   
check_conserv_rules([],_).
check_conserv_rules([(KL,KR)|LL],L):-
   count(L,KL,N),
   count(L,KR,N),
   check_conserv_rules(LL,L).
 
% count occurrences of a list of [M] in a list of pairs with stoichiometry
count([],_,0).
count([MM*M|L],KL,N):-
   (
      member((N1,M),KL)
   ->
      true
   ;
      (N1=0)
   ),
   count(L,KL,N2),
   N is MM*N1+N2.

% check from kinetics
check_conserv_num(L):-
   apply_kinetics(K,_ML),
   compile_conserv(K,L,ExprList),
   flatten_sum(ExprList,S),
   elim_sum(S,SS),
   format_debug(3,"remaining term(s) for ~w: ~w~n",[L,SS]),
   (SS=[]).

% get kinetics from list of [M]
compile_conserv(_,[],[]).
compile_conserv(K,[MM*M|L],[E|EL]):-
   compile_all(K,[M],[E1]),
   (
      (
         M = '::'(_,Loc),
         k_volume(Loc,Vol)
      )
   ->
      (
         E1 = E3/Vol
      ->
         E=MM*E3
      ;
         (
            E1 = -(E2/Vol)
         ->
            E = -(MM*E2)
         ;
            simplify(MM*E1*Vol,E)
         )
      )
   ;
      simplify(MM*E1, E)
   ),
   compile_conserv(K,L,EL).

% make a list (sum) of lists (products) from a term
flatten_sum([],[]).
flatten_sum([H|T],S):-
   flatten_sum_term(H,S1),
   flatten_sum(T,S2),
   append(S1,S2,S).

flatten_sum_term(X+Y,L):-
   !,
   flatten_sum_term(X,L1),
   flatten_sum_term(Y,L2),
   append(L1,L2,L).
   
flatten_sum_term(X-Y,L):-
   !,
   flatten_sum_term(X,L1),
   flatten_sum_term(-Y,L2),
   append(L1,L2,L).
   
flatten_sum_term(-X,L):-
   !,
   flatten_sum_term(X,L1),
   neg_sum_term(L1,L).
   
flatten_sum_term(X*Y,L):-
   !,
   flatten_sum_term(X,L1),
   flatten_sum_term(Y,L2),
   mult_sum_term(L1,L2,L).
   
flatten_sum_term(A,[[A]]).

neg_sum_term([],[]).
neg_sum_term([-H|T],[H|TT]):-
   !,neg_sum_term(T,TT).
neg_sum_term([H|T],[-H|TT]):-
   !,neg_sum_term(T,TT).

mult_sum_term([],_,[]).
mult_sum_term([H|T],T2,L):-
   mult_sum_term_rec(H,T2,L1),
   mult_sum_term(T,T2,L2),
   append(L1,L2,L).

mult_sum_term_rec(_,[],[]).
mult_sum_term_rec(-A,[-H|T],[AH|L]):-
   !,
   append(A,H,AH),
   msort(AH),
   mult_sum_term_rec(A,T,L).
mult_sum_term_rec(-A,[H|T],[-AH|L]):-
   !,
   append(A,H,AH),
   msort(AH),
   mult_sum_term_rec(A,T,L).
mult_sum_term_rec(A,[-H|T],[-AH|L]):-
   !,
   append(A,H,AH),
   msort(AH),
   mult_sum_term_rec(A,T,L).
mult_sum_term_rec(A,[H|T],[AH|L]):-
   !,
   append(A,H,AH),
   msort(AH),
   mult_sum_term_rec(A,T,L).

% try to rmove pairs of opposites
elim_sum([],[]).
elim_sum([-H|T],L):-
   !,
   (
      select(H,T,L1)
   ->
      elim_sum(L1,L)
   ;
      elim_sum(T,L2),
      (L=[-H|L2])
   ).

elim_sum([[0|_]|T], L):-
   !, elim_sum(T, L).

elim_sum([H|T],L):-
   !,
   (
      select(-H,T,L1)
   ->
      elim_sum(L1,L)
   ;
      elim_sum(T,L2),
      (L=[H|L2])
   ).

% get a list of matching molecules, with the [] around them
get_molecules({A,B},L):-!,
   get_molecules(A,L1),
   get_molecules({B},L2),
   append(L1,L2,L),
   sort(L).

get_molecules({A},L):-!,
   get_molecules(A,L).

get_molecules([], []):-!.
get_molecules([MM*M|T], [MM*M|TT]):-
   !, get_molecules(T, TT).
get_molecules([M|T], [1*M|TT]):-
   !, get_molecules(T, TT).


get_molecules(A,L):-
	findall(1*B,find_molecules(A,B),L).

build_sum([],0).
build_sum([A],A).
build_sum([H1,H2|T],S+H1):-
   build_sum([H2|T],S).

mult_volumes([],[]).
mult_volumes([MM*H|T],[HH|TT]):-
   (
      (
         H = '::'(_,Loc),
         k_volume(Loc,Vol)
      )
   ->
      HH = MM*Vol*[H]
   ;
      HH=MM*[H]
   ),
   mult_volumes(T,TT).
