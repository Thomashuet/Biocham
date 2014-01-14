%setmemberchk(+E, +S) succeeds if E is an element of the set S. Can only succeed once.
setmemberchk(X, {X, _}) :- !.

setmemberchk(X, {_, T}) :- setmemberchk(X, {T}).

setmemberchk(X, {X}).

%setmemberchk(+E, +S) succeeds if E is an element of the set S. Can only succeed once.
setmember(X, {X, _}).

setmember(X, {_, T}) :- !, setmember(X, {T}).

setmember(X, {X}).


%map_list(+LIn, -LOut, +Pred, +PredArgs) is a classic map on list,
%where LOut is the list of ElemOut obtained by calling Pred(ElemIn, ElemOut, PredArgs)
%for every ElemIn in LIn.
map_list([], [], _Pred, _PredArgs).

map_list([HIn|TIn], [HOut|TOut], Pred, PredArgs) :-
   F =.. [Pred, HIn, HOut | PredArgs],
   map_list(TIn, TOut, Pred, PredArgs),
   call(F).

%same calling convention as map_list,
%except if Pred fails, the output list is still built,
%by skipping on the element causing the failure.
fmap_list([], [], _Pred, _PredArgs).

fmap_list([HIn|TIn], Ret, Pred, PredArgs) :-
   F =.. [Pred, HIn, HOut | PredArgs],
   fmap_list(TIn, TOut, Pred, PredArgs),
   (call(F) -> Ret = [HOut|TOut] ; Ret = TOut).


%same as map_list, on sets.
map_set({}, {}, _Pred, _PredArgs).

map_set({HIn, TIn}, {HOut, TOut}, Pred, PredArgs) :-
   !,
   map_set({HIn}, {HOut}, Pred, PredArgs),
   map_set({TIn}, {TOut}, Pred, PredArgs).

map_set({A}, {A2}, Pred, PredArgs) :-
   F =.. [Pred, A, A2 | PredArgs],
   call(F).


%same convention as map_list, except the predicate has no "returning argument".
iter_list([], _Pred, _PredArgs).

iter_list([H|T], Pred, PredArgs) :-
   F =.. [Pred, H | PredArgs],
   call(F),
   iter_list(T, Pred, PredArgs).


%same as map_list, but here Pred is a predicate that rewrites a term.
%The intended use is to be able to rewrite a tree with leaves of type a
%in a tree with leaves of type b.
%This is done in a greedy way: Pred checks whether it should and can rewrite
%the current term, and if it fails, the rewriting is tried on every branch.
rewrite_term_rulep(T, TRet, Pred, PredArgs) :-
   (
      F =.. [Pred, T, Ret | PredArgs],
      call(F)
   ->
      TRet = Ret
   ;
      T =.. [TF | TArgs],
      map_list(TArgs, TRetArgs, rewrite_term_rulep, [Pred, PredArgs]),
      TRet =.. [TF | TRetArgs]
   ).


%marks the location of molecules in a BIOCHAM rule,
%so that they can be identified easily when using rewrite_term_rulep.
mark_molecules(K for R, K2 for R2) :-
   !,
   mark_molecules_kinetic(K, K2),
   mark_molecules(R, R2).

mark_molecules(T, TRet) :-
   T =.. [TF | TArgs],
   (
      memberchk(TF, [*])
   -> %recurse in right argument only
      [Arg1, Arg2] = TArgs,
      mark_molecules(Arg2, Arg2R),
      TRet =.. [TF, Arg1, Arg2R]
   ;
      memberchk(_, TArgs),
      \+ memberchk(TF, [~, -, #]) 
   -> 
      map_list(TArgs, TRetArgs, mark_molecules, []),
      TRet =.. [TF | TRetArgs]
   ;
      TRet = molecule_here(T)
   ).


mark_molecules_kinetic([A], [molecule_here(A)]) :- !.

mark_molecules_kinetic(T, TRet) :-
   T =.. [TF | TArgs],
   map_list(TArgs, TRetArgs, mark_molecules_kinetic, []),
   TRet =.. [TF | TRetArgs].
	       

%substitutes any molecule of SSL by TS.
substitute_molecule(molecule_here(E), El, SSL, TS):-
   parse_molecule(E, NE),
   (     
      setmemberchk(NE, SSL)
   ->
      El = TS
   ;
      El = E
   ).


merge_species_rule(R, RR, NSS, TS) :-
   mark_molecules(R, Rm),
   rewrite_term_rulep(Rm, RR, substitute_molecule, [NSS, TS]).


%simplify_rule(Rule, Simplified_rule)
%removes '_' from Rule, and rewrites Rule so that
%there are no duplicate molecules in solutions
%i.e. no A + A => ...

simplify_rule(:(N,R), :(N,RR)) :- !, simplify_rule(R, RR).

simplify_rule(K for R, K for RR) :- !, simplify_rule(R, RR).

simplify_rule(R => P, RR => PP) :-
   !,
   simplify_rule(R, RR),
   simplify_solution(P, PP).			 
   
simplify_rule(R <=> P, RR <=> PP) :-
   !,
   simplify_rule(R, RR),
   simplify_solution(P, PP).

simplify_rule(R, RR) :-
   R =.. [RF, G, [C]],
   memberchk(RF, [=, <=]),
   !,
   simplify_solution(G, GG),
   simplify_rule(C, CC),
   (CC = '_' -> RR = GG ; RR =.. [RF, GG, [CC]]).   

simplify_rule(R, RR) :-
   simplify_solution(R, RR).
   

simplify_solution(Solution, Resulting_solution) :-
   simplify_solution_to_list(Solution, NOSM_list, [], 0, _ReturnedRank), 
   %NOSM is (Normalized molecule, Order from left to right, Stoichiometry, Molecule)
   sort(NOSM_list, Sorted_NOSM_list),
   simplify_solution_merge(Sorted_NOSM_list, Merged_NOSM_list),
   findall(Y, (member(X, Merged_NOSM_list),
               X = (N, A, B, C),
               N \= '_',
               Y = (A, B, C)),
           Filtered_OSM_list),
   sort(Filtered_OSM_list, Sorted_OSM_list),
   findall(X, (member((A, B, C), Sorted_OSM_list),
               X = (B, C)),
           Stoichiometry_list),
   expand_solution(Stoichiometry_list, Resulting_solution).


%simplify_solution_to_list returns the list of leaves of the solution in a left-to-right order.
%if we traversed the tree in a left-to-right order while accumulating the elements in a list,
%we would have to reverse the list at the end.
%To prevent that horrible event, we do a right-to-left traversal and give decreasing ranks.
simplify_solution_to_list(A + B, R, Acc, Rank, RRank) :- !,
   simplify_solution_to_list(B, RB, Acc, Rank, RRankB),
   simplify_solution_to_list(A, R , RB , RRankB, RRank).


simplify_solution_to_list(C * A, R, Acc, Rank, RRank) :- !,
   RRank is Rank - 1,
   parse_molecule(A, NA),
   R = [(NA, Rank, C, A) | Acc].


simplify_solution_to_list(A, R, Acc, Rank, RRank) :-
   simplify_solution_to_list(1 * A, R, Acc, Rank, RRank).


simplify_solution_merge([], []).

simplify_solution_merge([A], [A]).

simplify_solution_merge([(N, O1, S1, M1), (N, _O2, S2, _M2) | L], [(N, O1, S, M1) | R]) :- !,
   S is S1 + S2,
   simplify_solution_merge(L, R).

simplify_solution_merge([A, B | L], [A | R]) :- !,
	simplify_solution_merge([B | L], R).




	      
merge_species(SSpecies, TSpecies):-
   map_set(SSpecies, NSSpecies, parse_molecule, []),
   findall(A, rule(A, _B, _C, _D, _E, _F, _G), Rules),
   map_list(Rules, RRules, merge_species_rule, [NSSpecies, TSpecies]),
   map_list(RRules, SRules, simplify_rule, []),
   clear_rules,

   asserta(silence_mode),
   iter_list(SRules, add_rule, []),
   retract(silence_mode).


smerge({SObject, SObjects}, TSpecies) :- !,
   find_molecules(SObject, SSpecies),
   merge_species({SSpecies}, TSpecies),
   smerge({SObjects}, SSpecies).

smerge({SObject}, TSpecies) :- !,
   find_molecules(SObject, SSpecies),
   merge_species({SSpecies}, TSpecies).

smerge({}, _) :- !.

smerge(SObject, TSpecies) :- smerge({SObject}, TSpecies).
 

delete_species(SSpecies) :-
   merge_species(SSpecies, '_').


sdelete({SObject, SObjects}) :- !,
   sdelete({SObject}),
   sdelete({SObjects}).

sdelete({SObject}) :- !,
   find_molecules(SObject, SSpecies),
   delete_species({SSpecies}).

sdelete({}) :- !.

sdelete(SObject) :-
   sdelete({SObject}).
 

rdelete(Rules) :-
   delete_rules(Rules).


rmerge(PatternsRule) :-
   PatternsRule = {_}, !,
   %get all reactions matching the pattern
   findall(Actants, 
             (setmember(P, PatternsRule),
              pattern_rule(P,X,LA,Y,RA),
              rule(_,LeftSol,RightSol,_R,LB,RB,_RL),
              match_list2(X,LA,LB),
              match_list2(Y,RA,RB),
              Actants = (LeftSol, RightSol)
             ),
           Reactions0),
   sort(Reactions0, Reactions), % a reaction may match several patterns

   findall(SReactant,
             (member((L, _), Reactions),
              member((_, Reactant), L),
              SReactant = (1, Reactant)
             ),
           Reactants0),
   sort(Reactants0, Reactants),

   findall(SProduct,
             (member((_, R), Reactions),
              member((_, Product), R),
              SProduct = (1, Product)
             ),
           Products0),
   sort(Products0, Products),

   expand_solution(Reactants, LeftSolution),
   expand_solution(Products, RightSolution),

   asserta(silence_mode),
   delete_rules(PatternsRule),
   add_rule(LeftSolution => RightSolution),
   retract(silence_mode),
 
   length(Reactions, NReactions),
   (
	   have_gui
		->
		   	format("[GUI] reduction Merged ~d reactions.~n", [NReactions])
	        ;
			format("Merged ~d reactions~N", [NReactions])
   ). 


rmerge(PatternRule) :- rmerge({PatternRule}).

print_all_rules:-
   rule(A, B, C, D, E, F, G),  
 (
	   have_gui
		->
		   	format("[GUI] reduction ##~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N ~N",
	  [A, A, B, B, C, C, D, D, E, E, F, F, G, G])
	        ;
			format("##~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N#~w~N~k~N ~N ~N",
	  [A, A, B, B, C, C, D, D, E, E, F, F, G, G])
   ),   
   fail
   ;
   true.	   


map_db(P, G, R) :-
   retract(P),
   G,
   assertz(R),
   fail
   ;
   true.

