% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2003-2010, INRIA, Projet Contraintes
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
% GNU prolog file biochamLearn.pl
% by Nathalie Chabrier-Rivier, Francois Fages

:-dynamic(interactive/0).
:-dynamic(learning_mode/0).
:-dynamic(verbose_mode/0).
:-dynamic(silence_mode/0).
:-dynamic(reduit_model_mode/0).
:-dynamic(faire_le_check/0).
:-dynamic(th_biais_pattern_rule/1).  % cest le biais utiliser pour ajouter des regles dans theory_revision
:-dynamic(score_mode/0).
:-dynamic(theory_revision_mode/0).
:-dynamic(lrule/7). % possible rules (for adding)
:-dynamic(drule/3). % possible rules (for deletion)
% drule(R,LqT,LqF) R=rule(OR,LL,LR,Rn,LB,RB,V),
% LqT=list of neg. specs made true by the deletion of R
% LqF=list of neg. specs remaining false even when R is deleted
% all specs appearing in these lists had R in their 'why'
:-dynamic(badQuery/1). %badQuery(Q) Q is a neg. spec, not verified and
% needing more than only one deletion
:-dynamic(good_rule/7).
:-dynamic(rule_learned/2). % rule_learned(R,'add') ou rule_learned(R,'del') garde en m�moire les r�gles ajout�es ou enlev�es automatiquement pour ne pas boucler.(R est une regle biocahm (A+B=>C+D)
:-dynamic(forgotten_spec/1). % spec que lon arrive pas � rendre vraie, mise en attente de ???

:- dynamic(nb_deep_max/1).

nb_deep_max(6).

new_molecule_for_learning({M,L}):-
	new_molecule_for_learning(M),
	new_molecule_for_learning({L}).
new_molecule_for_learning({M}):-
	new_molecule_for_learning(M).
new_molecule_for_learning(Mol):-
	add_rules({'_'=>Mol, Mol=>'_'}).


before_learn_anything:-
        g_assign(search_depth,0),
	retractall(badQuery),
	retractall(good_rule),
	retractall(lrule(_,_,_,_,_,_,_)),
	retractall(drule(_,_,_)),
	(
	  (faire_le_check;(rules_added; (spec(_,_,V),var(V))))
	->
	  (
	    %nusmv_time(_),
	    % format("All the specifications are being tested before the learning process is started~n",[]),
	    assertz(learning_mode),
	    check,
	    retractall(faire_le_check),
	    %nusmv_time(T1),
	    % format("Time: ~2f s~n",[T1]),
	    retract(learning_mode)
	  )
	;
	  (
	    true
	  )
	).

learn_one_addition(R):- learn_one_rule(R). %FF

learn_one_rule(R):- % be carefull R can be  a set of rules (R={R1,R2,...})
   before_learn_anything,
   (
      spec(Q,'negative',false)     % FIXME choisir si on fait �a pour learn_one_rule ou pour learn_one_deletion. Sinon, on va tourner en rond !
   ->
      (
         !,
         (
            have_gui
         ->
            format("[GUI] checkCTL Some negative formulae are not satisfied (ex:~w), delete some rules before learning a rule to be added~n",[Q])
         ;
            format("Some negative formulae are not satisfied (ex:~w), delete some rules before learning a rule to be added~n",[Q])
         ),
         fail
      )
   ;
      true
   ),
   nusmv_time(_),
   assertset(learning_mode),
   g_assign(nbLrule,0),
   g_assign(nbGrule,0),
   add_rules(R),  % in learning_mode adds (several) 'lrules'
   retract(lrule(_,LL,LR,Rt,LB,RB,V)),
   g_read(nbLrule,Nb),
   NbL is Nb+1,
   g_assign(nbLrule,NbL),
   retractall(learning_mode),
   check_and_assert_rule(Rt,LL,LR,Rt,LB,RB,V),  % TODO no orule added, pb ?
   test_all('positive',false),
   (
      (\+(spec(_,'positive',false));last_query(_,_,true))
   ->
      test_all('negative',true), 
      (
         (\+(spec(_,'negative',true)) ; last_query(_,_,true))
      ->
         test_all('undefined')
      ;
         true
      )
   ;
      true
   ),
   retract(rule(Rt,LL,LR,Rt,LB,RB,V)),% precisely the one we just added
   last_query(_,_,true),
   assertset(good_rule(Rt,LL,LR,Rt,LB,RB,V)),
   g_read(nbGrule,NbG),
   NbGr is NbG +1,
   g_assign(nbGrule,NbGr),
   fail.

learn_one_rule(_):-
   g_read(nbLrule,NbL),
   g_read(nbGrule,NbG),
   nusmv_time(T),
   (
      have_gui
   ->
      format("[GUI] checkCTL Time: ~2f s~n",[T]),
      format("[GUI] checkCTL Rules tested: ~w~n",[NbL]),
      format("[GUI] checkCTL Possible rules to add: ~w~n",[NbG])

   ;
      format("Time: ~2f s~nRules tested: ~w~nPossible rules to add: ~w~n",[T,NbL,NbG])
   ),
   retract(good_rule(Rt,_,_,Rt,_,_,_)),
   write(Rt),
   (
      have_gui
   ->
      format("[GUI] checkCTL ~w~n",[Rt])
   ;
      true
   ),
   nl,
   fail.

learn_one_rule(_):-
	retractall(learning_mode).


learn_one_deletion(R):-
	(
	  reduit_model_mode
	->
	  true
	;
	  (
	    before_learn_anything,
	    nusmv_time(_)
	  )
	),
	g_assign(nbLrule,0),
	g_assign(nbGrule,0),
	assertset(learning_mode),
	( reduit_model_mode -> assertz(silence_mode) ;true),
	delete_rules(R), % in learning_mode deletes (several) 'drules'
	( reduit_model_mode -> retract(silence_mode) ;true),
	retractall(learning_mode),
	
	retract(lrule(OR,LL,LR,Rt,LB,RB,V)),
	format_debug(1,"Removing ~w~n",[Rt]),
	g_read(nbLrule,Nb),
	NbL is Nb+1,
	g_assign(nbLrule,NbL),
	retract(rule(OR,LL,LR,Rt,LB,RB,V)),	% no clean_orule, pb?
	assertset(mols_added),    % to make the deletion noticed
	assertset(declare_added),
 	assertset(rules_added),
	test_all('negative',false),
	(
	  (\+(spec(_,'negative',false));last_query(_,_,true))
	->
	  (
	    test_all('positive',true),
	    (
	      (\+(spec(_,'positive',true));last_query(_,_,true))
	    ->
	      test_all('undefined')
	    ;
	      true
	    )
	  )
	;
	  true
	),
	assertz(rule(OR,LL,LR,Rt,LB,RB,V)),% precisely the one we just deleted
	format_debug(1,"Putting back ~w~n",[Rt]),
	last_query(_,_,true),
	format_debug(1,"~w is a possible rule~n",[Rt]),
	(
	  reduit_model_mode
	->
	  (
	    delete_rule_b(Rt),
	    assertb(rule_learned(Rt,'del'))
	  )
	;
	  (
	    assertset(good_rule(OR,LL,LR,Rt,LB,RB,V)),
	    g_read(nbGrule,NbG),
	    NbGr is NbG +1,
	    g_assign(nbGrule,NbGr),
	    fail
	  )
	).

learn_one_deletion(Biais):-
   g_read(nbLrule,NbL),
   g_read(nbGrule,NbG),
   (
      reduit_model_mode
   ->
      (
         have_gui
      ->
         format("[GUI] checkCTL After reduction, ~w rules remain corresponding to the bias ~w~n",[NbL,Biais])
      ;
         format("After reduction, ~w rules remain corresponding to the bias ~w~n",[NbL,Biais])
      )
   ;
      (
         nusmv_time(T),
         (
            have_gui
         ->
            format("[GUI] checkCTL Time: ~2f s~n",[T]),
            format("[GUI] checkCTL Rules tested: ~w~n",[NbL]),
            format("[GUI] checkCTL Possible rules to delete: ~w ~n",[NbG])
         ;
            format("Time: ~2f s~n",[T]),
            format("Rules tested: ~w~n",[NbL]),
            format(" Possible rules to delete: ~w~n",[NbG])
         ),
         retract(good_rule(_,_,_,Rt,_,_,_)),
         (
            have_gui
         ->
            format("[GUI] checkCTL ~w ~n",[Rt])
         ;
            true
         ),
         write(Rt),nl,
         fail
      )
   ).

learn_one_deletion(_):-
	assertset(mols_added),	% sinon la remise de la derni�re regle ne sera pas prise en compte.
	assertset(declare_added),
 	assertset(rules_added),
	retractall(learning_mode),!.



learn_one_deletion:- % using the output of 'why' and no bias
   before_learn_anything,
   nusmv_time(_),
   g_assign(nbLrule,0),
   g_assign(nbGrule,0),
   assertset(learning_mode),
   spec(Q,'negative',false),
   pathway(Q,Path),
   (
      Path=[]
   ->
      (
         have_gui
      ->
         format("[GUI] checkCTL Maybe change the initial state to make this query ~w true~n",[Q])
      ;
         format("Maybe change the initial state to make this query ~w true~n",[Q])
      ),
      fail
   ;
      assertset(badQuery(Q))
   ),
   member(Rn,Path),
   g_read(nbLrule,_nb_rule),
   _nb_rulep is _nb_rule +1,
   g_assign(nbLrule,_nb_rulep),
   retract_rule(Rn),	% instead of 'delete_rules' (doesn't clean orules)
   nusmv(Q),
   assertz(Rn),
   assertset(rules_added),
   last_query(Q,_,A),
   (
      A=true
   ->
      (
         retractall(badQuery(Q)),
         (
            retract(drule(Rn,LqT,LqF))
         ->
            assertz(drule(Rn,[Q|LqT],LqF))
         ;
            assertz(drule(Rn,[Q],[]))
         )
      )
   ;
      (
         retract(drule(Rn,LqT,LqF))
      ->
         assertz(drule(Rn,LqT,[Q|LqF]))
      ;
         assertz(drule(Rn,[],[Q]))
      )
   ),

	fail.


learn_one_deletion:-
   badQuery(Q),
   (
      have_gui
   ->
      format("[GUI] checkCTL This query ~w requires more than one deletion to become true, try theory_revision~n",[Q])
   ;
      format("This query ~w requires more than one deletion to become true, try theory_revision~n",[Q])
   ),
   fail.


learn_one_deletion:-
	\+(badQuery(_));
	drule(Rn,_,[]),
	Rn=rule(OR,LL,LR,Rt,LB,RB,V),
	g_read(nbLrule,Nb),
	NbL is Nb+1,
	g_assign(nbLrule,NbL),
	retractall(rule(OR,LL,LR,Rt,LB,RB,V)),		% on a pas fait clean_orule,
				% pour pas que le mod�le soit tout casser apr�s, pb ?
	assertset(mols_added),	% sinon la d�l�tion ne sera pas prise en compte.
	assertset(declare_added),
 	assertset(rules_added),
	test_all('negative',false),
	(
	  (\+(spec(_,'negative',false));last_query(_,_,true))
	->
	  (
	    test_all('positive',true),
	    ((\+(spec(_,'positive',true));last_query(_,_,true))-> test_all('undefined');true)
	  )
	;
	  true
	),
	assertset(Rn),
	last_query(_,_,true),
	assertset(good_rule(OR,LL,LR,Rt,LB,RB,V)),
	g_read(nbGrule,NbG),
	NbGr is NbG +1,
	g_assign(nbGrule,NbGr),
	fail.

	
learn_one_deletion:-
   g_read(nbLrule,NbL),
   g_read(nbGrule,NbG),
   nusmv_time(T),
   (
      have_gui
   ->
      format("[GUI] checkCTL Time: ~2f s. Rules tested: ~w . Possible rules to delete: ~w .~n",[T,NbL,NbG])
   ;
      format("Time: ~2f s~nRules tested: ~w~nPossible rules to delete: ~w~n",[T,NbL,NbG])
   ),
   %       (NbG=0 -> write("Try theory revision algorithm\n"); true), 
   retract(good_rule(_,_,_,Rt,_,_,_)),
   write(Rt),
   (
      have_gui
   ->
      format("[GUI] checkCTL ~w ~n",[Rt])
   ;
      true
   ),nl,
   fail.

learn_one_deletion:-
	assertset(mols_added),	% sinon la remise de la derni�re regle ne sera pas prise en compte.
	assertset(declare_added),
 	assertset(rules_added),
	retractall(learning_mode),!.
	
% deletes a rule, without clean_orule,
retract_rule_num(N,Rn):-
	rule(N,Rn),
	retract(Rn),
	assertset(mols_added),    % take the deletion into account
	assertset(declare_added),
 	assertset(rules_added).
% deletes a rule, without clean_orule,
retract_rule(Rn):-
	retract(Rn),
	assertset(mols_added),    % take the deletion into account
	assertset(declare_added),
 	assertset(rules_added).

% deletes a list of rules, without clean_orule,
retract_rules([]).
retract_rules([R|LR]):-
	retract_rule(R),
	retract_rules(LR).

% asserts a rule, without add_orule
assert_rule(Rn):-
	assertset(Rn),
	assertset(mols_added),    % take the deletion into account
	assertset(declare_added),
 	assertset(rules_added).

% asserts a list of rules, without add_orule
assert_rules([]).
assert_rules([R|LR]):-
	assert_rule(R),
	assert_rules(LR).


write_spec_neg_false:-
   spec(Q,'negative',false),
   write(Q),
   (
      have_gui
   ->
      format("[GUI] checkCTL ~w~n",[Q])
   ;
      true
   ),
   nl,
   fail.
write_spec_neg_false:-!.
write_spec_pos_false:-
   spec(Q,'positive',false),
   write(Q),
   (
      have_gui
   ->
      format("[GUI] checkCTL ~w~n",[Q])
   ;
      true
   ),
   nl,
   fail.
write_spec_pos_false:-!.
write_spec_undef_false:-
   spec(Q,'undefined',false),
   write(Q),
   (
      have_gui
   ->
      format("[GUI] checkCTL ~w~n",[Q])
   ;
      true
   ),
   nl,
   fail.

write_spec_undef_false:-!.
write_spec_false:-
   (
      have_gui
   ->
      format("[GUI] checkCTL Positive formulae not satisfied: ~n",[])
   ;
      write('Positive formulae not satisfied:\n')
   ),
   write_spec_pos_false,nl,
   (
      have_gui
   ->
      format("[GUI] checkCTL Negative formulae not satisfied: ~n",[])
   ;
      write('Negative formulae not satisfied:\n')
   ),
   write_spec_neg_false,nl,
   (
      have_gui
   ->
      format("[GUI] checkCTL Undefined formulae not satisfied: ~n",[])
   ;
      write('Undefined formulae not satisfied:\n')
   ),
   write_spec_undef_false.





forget_spec(C,A):-
	retract(spec(Q,C,A)),
	format_debug(1,"Forgetting specification ~w~n",[Q]),
	assertset(forgotten_spec(spec(Q,C,A))),
	fail.
forget_spec(_,_):-!.
remember_spec(C,A):-
	retract(forgotten_spec(spec(Q,C,A))),
	add_spec(Q),
	fail.
remember_spec(_,_):-!.
	


% attention effet de bord sur learning_mode
add_rule_b(R):-
   g_read(search_depth,D),
   D1 is D+1,
   (
      (
         retractall(learning_mode),
         write(D1),
         g_assign(search_depth,D1),
         (
            have_gui
         ->
            format("[GUI] checkCTL ~w ~n",[D1])
         ;
            true
         ),
         write(': adding '),
         (
            have_gui
         ->
            format("[GUI] checkCTL : adding  ~n",[])
         ;
            true
         ),
         add_rules(R),
         assertset(faire_le_check),
         assertset(learning_mode)
      )
   ;
      (
         retractall(learning_mode),
         write(D1),
         g_assign(search_depth,D),
         (
            have_gui
         ->
            format("[GUI] checkCTL : backtracking on previous add -> deleting  ~n",[])
         ;
            write(': backtracking on previous add -> deleting ')
         ),
         delete_rules(R),
         assertset(faire_le_check),
         assertset(learning_mode),
         fail
      )
   ).


% attention effet de bord sur learning_mode
delete_rule_b(R):-
   g_read(search_depth,D),
   D1 is D+1,
   (
      (
         g_assign(search_depth,D1),
         write(D1),
         (
            have_gui
         ->
            format("[GUI] checkCTL ~w ~n",[D1])
         ;
            true
         ),
         retractall(learning_mode),
         (
            have_gui
         ->
            format("[GUI] checkCTL : deleting ~n",[])
         ;
            write(': deleting ')
         ),
         flush_output,
         (
            R=..[:,_,Rname]
         ->
            true
         ;
            Rname=R
         ),
         (Rname=for(_,Rr) -> true; Rr=Rname),
         delete_rules(Rr),
         assertset(faire_le_check),
         assertset(learning_mode)
      )
   ;
      (
         retractall(learning_mode),
         write(D1),
         (
            have_gui
         ->
            format("[GUI] checkCTL ~w ~n",[D1])
         ;
            true
         ),
         g_assign(search_depth,D),
         (
            have_gui
         ->
            format("[GUI] checkCTL : backtracking on previous deletion -> adding  ~n",[])
         ;
            write(': backtracking on previous deletion -> adding ')
         ),
         flush_output,
         add_rules(R),
         assertset(faire_le_check),
         assertset(learning_mode),
         fail
      )
   ).

delete_rules_b([]).
delete_rules_b([R|L]):-	
	delete_rule_b(R),
	assertb(rule_learned(R,'del')),
	delete_rules_b(L).
	

retract_specs_maj_b([]).
retract_specs_maj_b([(Q,_)|LQ]):-
	((
	  retract(spec(Q,_,_)),
	  assertset(forgotten_spec(spec(Q,_,_))),
	  g_read(nb_spec_treated,N),
	  Np is N-1,
	  g_assign(nb_spec_treated,Np)
	)
	;
	(
	  add_spec(Q),
	  retract(forgotten_spec(spec(Q,_,_))),
	  g_read(nb_spec_treated,Nn),
	  Nm is Nn+1,
	  g_assign(nb_spec_treated,Nm),
	  fail
	)
	),
	retract_specs_maj_b(LQ).

	
%% Mol est transform�e
%elementary_interaction_pattern_for(Mol))
%de_complexation: $A + Mol <=> $A-Mol  where $A in all  and $A diff Mol
%de_phosporylation: $A  <=[$C]=> Mol where $A in all and Mol  in all and $C in all and Mol more_phos_than $A  and $A diff Mol
%synthesis: _=[$G]=>$A where $A in all_simple and $G in all
%degradation: $A =[$D]=>_ where $A in all and $D in all



%% Mol est cataliseur
%elementary_interaction_pattern_of(Mol))


%elementary_interaction_pattern(Mol))


%interne pour debugger
th:-
	assertset(verbose_mode),
	revise_model,
	retractall(verbose_mode),
	!.



%theory revision qui prend en compte les spec dans l'ordre d'arriv�e.
% une spec ECTL -> ajout d'une regle en respectant les specs d�j� prise en compte.
% une spec ACTL -> retrait d'une regle ou plusieurs r�gles (max 3) et retraiter les spec ECTL ou undefined qui deviennent fausses
% une spec undefined -> essais de retrait d'une regle ou plusieurs r�gles (max 3) ou d'en ajouter une  tout en respectant les specs d�j� prises en compte.


revise_model_interactive(R):-
	assertset(interactive),
	revise_model(R),
	retractall(interactive).

revise_model_interactive:-
	assertset(interactive),
	revise_model,
	retractall(interactive).

revise_model(R):-
	assertset(th_biais_pattern_rule(R)),
	revise_model.
revise_model:-
	(g_read(debug,9) -> assertset(verbose_mode) ;true),
	(
	  th_biais_pattern_rule(_)
	->
	  true
	;
	  assertset(th_biais_pattern_rule(elementary_interaction_rules))
	),
	retractall(rule_learned(_,_)),
	g_assign(nb_spec_treated,0),
	g_assign(max_spec_treated,0),
%	forget_spec(_,_),
	forget_spec('positive',_),
	forget_spec('undefined',_),
	forget_spec('negative',_), % pour trier les specs.
	findall((Q,C),forgotten_spec(spec(Q,C,_)),Lqueries),
	nusmv_time(_),
	th_treat_liste_specs(Lqueries),
	format("~nSuccess~n",[]),flush_output,

   (
      interactive
   ->
      (
         %delete_flag_rule_deleted_and_added, % utilisation risque effet de bord (ne pas prendre en compte les r�gles qui apparissent dans deletion and addition)
         (
            have_gui
         ->
            format("[GUI] checkCTL Modifications found:  Deletion(s): ~n",[]),
            writeall_rule(rule_learned(R,'del'),R)
         ;
            format("~nModifications found:~n  Deletion(s):~n",[])
         ),
         (
            have_gui
         ->
            format("[GUI] checkCTL Addition(s):~n",[]),
            writeall_rule(rule_learned(R,'add'),R)
         ;
            format("  Addition(s):~n",[])
         ),
         format("     Do you want an other solution ? [y,n]",[]),flush_output,
         read_atom(Atom),
         (
            Atom='n'
         ->
            true
         ;
            fail
         )
      )
   ;
      true
   ),

   th_fin,!,(
      have_gui
   ->
      format("[GUI] checkCTL FINISH REVISING~n",[])
   ;
      true
   ).

revise_model:-
   (
      have_gui
   ->
      format("[GUI] checkCTL No solution found~n",[])
   ;
      format("~nNo solution found~n",[])
   ),flush_output,
   th_file(TH_FILE),
   format_debug(1,"The best model is in the temporary file: ~w ~n",[TH_FILE]),
   th_fin,
   !,
   (
      have_gui
   ->
      format("[GUI] checkCTL FINISH REVISING~n",[])
   ;
      true
   ).

th_fin:-
   nusmv_time(T),
   (
      have_gui
   ->
      format("[GUI] checkCTL  Time: ~2f s~n",[T])
   ;
      format("Time: ~2f s~n",[T])
   ),
   g_read(nb_spec_treated,N),
   %	g_read(max_spec_treated,M),
   %	format("~w queries treated (max is ~w)~n",[N,M]),
   (
      have_gui
   ->
      format("[GUI] checkCTL ~w properties treated~n",[N])
   ;
      format("~w properties treated~n",[N])
   ),
   remember_spec(_,_),
   %nusmv_time(_),
   %format("All the specifications are being tested ~n",[]),
   (
      have_gui
   ->
      format("[GUI] checkCTL All the specifications are being tested ~n",[])
   ;
      true
   ),
   assertz(learning_mode),
   check,
   %nusmv_time(T1),
   %format("Time: ~2f s~n",[T1]),
   (spec(_,_,false) -> write_spec_false; true), %write('All the specifications are true\n')),
   delete_flag_rule_deleted_and_added,
   (
      have_gui
   ->
      format("[GUI] checkCTL Modifications found:  Deletion(s):~n",[]),
      writeall_rule(rule_learned(R,'del'),R)
   ;
      format("Modifications found:~n  Deletion(s):~n",[])
   ),
   (
      have_gui
   ->
      format("[GUI] checkCTL Addition(s):~n",[]),
      writeall_rule(rule_learned(R,'add'),R)
   ;
      format("  Addition(s):~n",[])
   ),
   writeall(rule_learned(R,'add'),R),
   retractall(learning_mode),
   retractall(th_biais_pattern_rule(_)),
   !,

	(
	  once(rule_learned(_,'add'))
	->
	  (
	    %format("The following rules are not necessary and have not been added.~n",[]),
	    setof(Rule,rule_learned(Rule,'add'),List_rules_added),
	    list_to_set(List_rules_added,Rules_added),
	    reduce_model(Rules_added)
	  )
	;
	  (
	    true
	  )
	),
	(g_read(debug,9) -> retractall(verbose_mode) ;true).

	
th_treat_liste_specs([]):-
	(verbose_mode ->(	write('\n***   fin  ***'),nl);true).

th_treat_liste_specs([(Q,'positive')|Lqueries]):-% learn_one_rule
	(verbose_mode ->(	format("~n the positive query ~w is treating~n",[Q]));true),
	th_test_rule(Q,'positive',Ap),
	(
	  (
	    Ap=true,
	    (verbose_mode ->( format("        it is already true~n",[]));true)
	  )
	    
	->
	  true
	;
	  (	    
	    th_try_add_rule(Q,'positive')
	  )
	),	
	th_treat_liste_specs_fin(Q,'positive',Lqueries).



th_treat_liste_specs([(Q,'negative')|Lqueries]):- %learn_one deletion,
	% enlever une regle ou plusieurs et si besoin remettre des Q positive ou undefined � traiter. fitness(le moins de Q � retraiter)
	(verbose_mode ->(format("~n the negative query ~w is treating~n",[Q]),flush_output);true),

	th_test_rule(Q,'negative',Ap),
	(
	  Ap=true
	->
	  (
	    (verbose_mode ->(format("        it is already true~n",[]),flush_output);true),
	    th_treat_liste_specs_fin(Q,'negative',Lqueries)
	  )
	;
	  (
	    (verbose_mode ->(format("         it is false.~n",[]),flush_output);true),

       pathway(Q,Path),
       (
          Path=[]
       ->
          (
             (
                have_gui
             ->
                format("[GUI] checkCTL You might need to change the initial state to make the query ~w true. It is forgotten.~n",[Q])
             ;
                format("You might need to change the initial state to make the query ~w true. It is forgotten.~n",[Q])
             ),flush_output,
             th_treat_liste_specs(Lqueries) 
          )
       ;
          (
             (verbose_mode ->(	format("       on va essayer d'enlever des regles~n",[]),flush_output);true),  % a virer
             g_assign(nb_Q_a_retraite,0),
             th_treat_negative_query(Q,Path,LR,Lrequeries),
             g_read(nb_Q_a_retraite,Nretraite),
             (verbose_mode ->(format("les regles ~w sont enlev�es pour rendre ~w true, ~w specs ~w sont retrait�es.~n",[LR,Q,Nretraite,Lrequeries]),flush_output);true),
             append(Lrequeries,Lqueries,Lqueries_bis),

		th_treat_liste_specs_fin(Q,'negative',Lqueries_bis)
	      )
	    )
	  )
	).



th_treat_liste_specs([(Q,'undefined')|Lqueries]):- %learn_one_rule; learn_one_deletion,
	(verbose_mode ->(format("~n the undefined query ~w is treating~n",[Q]));true),
	th_test_rule(Q,'negative',Ap), % pour que le chemin soit calculer si false
	(
	  Ap=true
	->
	  (
	     (verbose_mode ->(  format("        it is already true~n",[]));true)
	  )
	;
	  (
	    pathway(Q,Path), %FIXME peut-�tre changer l'ordre essayer d'ajouter puis essayer d'enlever
	    (
	      (
                  nb_deep_max(K),
		  g_assign(nb_deep,K),  % max de regle a enlever pour rendre Q v
		  th_make_q_true(Q,Path,LR,LRn),
		  th_score(LRn,0,[])
	      )
	    ->
	      (% en enlevant des r�gles Q devient vraie sans rien casser
		  delete_rules_b(LR)
	      )
	    ;
	      ( % on essaye d'ajouter une r�gle pour que Q devienne vraie sans rien casser
		  th_try_add_rule(Q,'undefined')
	      )
	    )
	  )
	),
	th_treat_liste_specs_fin(Q,'undefined',Lqueries).

th_treat_liste_specs([(Q,_)|_]):-
	(verbose_mode ->(format("BACKTRACK (echec sur ~w)~n",[Q]));true),
	fail.


% backtrack
% mise � jour du nb de spec trait�es correctement.
th_maj_nb:-
	(
	  (
	    g_read(nb_spec_treated,N),
	    Np is N+1,
	    g_assign(nb_spec_treated,Np),
	    test_best_result
	  )
	;
	  (
	    g_read(nb_spec_treated,Nn),
	    Nm is Nn-1,
	    g_assign(nb_spec_treated,Nm),
	    fail
	  )
	).

th_test_rule(Q,C,Ap):-
	assertset(learning_mode),
	nusmv(Q),
	last_query(Q,_,Ap),
	!,
	 (
	   
	   (C='negative',Ap=false)
	 ->
	   (
	     why,
	     findall(R,(last_path(NR),rule(NR,R)),Path1),
              %FF plutot commencer par les dernieres regles du why
            reverse(Path1,Path),
	     assertset(pathway(Q,Path))
	   )
	 ;
	   retractall(pathway(Q,_))
	 ),
	retractall(learning_mode),
	!.

th_add_rule_to_test:-
	assertset(learning_mode),
	retractall(lrule(_,_,_,_,_,_,_)),
	(verbose_mode -> true ;(assertz(silence_mode))),
	th_biais_pattern_rule(Rbiais),
	add_rules(Rbiais),
	(verbose_mode -> true ;(retract(silence_mode))),
	retractall(learning_mode),
	!.

	
test_best_result:-
   g_read(nb_spec_treated,N),
   g_read(max_spec_treated,M),
   (
      verbose_mode
   ->
      (
         (
            have_gui
         ->
            format("[GUI] checkCTL ~w queries are treated, the max was ~w~n",[N,M])
         ;
            format("~w queries are treated, the max was ~w~n",[N,M])
         ),
         flush_output
      )
   ;
      true
   ),
   (
      N>M
   ->
      (
         assertset(theory_revision_mode),
         th_file(TH_file),
         export_biocham(TH_file,'no'), 
         retractall(theory_revision_mode),
         g_assign(max_spec_treated,N)
      )
   ;
      true
   ),
   !.


% backtrack.
th_treat_liste_specs_fin(Q,C,Lqueries):-
	(
      verbose_mode
   ->
      (
         
      (
                have_gui
                        ->
                                format("[GUI] checkCTL on est en fin de traitement pour ~w~n",[Q])
                        ;
                                format("on est en fin de traitement pour ~w~n",[Q])
                ),
         flush_output
      )
   ;
      true
   ), % a virer
	(
	  (
        retractall(forgotten_spec(spec(Q,C,_))),
        add_spec(Q)
     )
	;
	  (
        delete_spec(Q),
        assertset(forgotten_spec(spec(Q,C,_))),
        format_debug(1,"Forget specification ~q~n",[Q]),
        fail
     )
	),
	th_maj_nb,
	th_treat_liste_specs(Lqueries).


% backtrack.
th_treat_negative_query(Q,Path,LR_best,Lrequeries_best):-
	findall(
	      (Score-(LR,Lrequeries)),
	      (
                  nb_deep_max(K),
		  g_assign(nb_deep,K), % max nb rules a enlever.
		  th_make_q_true(Q,Path,LR,LRn),
		  th_score(LRn,Score,Lrequeries)
	      ),
	      LSRQ       
	     ),
	keysort(LSRQ), % j'espere que c'est du min au max.
	!,
	member((Score_best-(LR_best,Lrequeries_best)), LSRQ),
	g_assign(nb_Q_a_retraite,Score_best),
	retract_specs_maj_b(Lrequeries_best),                 % il serait peu-�tre mieux d'essayer de modifier la regle que de l'enlever, ou d'essayer d'jouter une seule regle qui v�rifie toute les requetes mise � retraiter. 
	delete_rules_b(LR_best).

% backtract 
th_make_q_true(Q,Path,LR,LRn):-
	!,
	g_read(nb_deep,D),
	D>=0,
	Dp is D -1,
	g_assign(nb_deep,Dp),
	member(Rn,Path),
	Rn=rule(OR,_,_,R,_,_,V),
	(V==0 -> RV=R ; RV=for(V,R)),
	(
	  OR=..[:,Name,_]
	->
	  RVn=..[:,Name,RV]
	;
	  RVn=RV
	),
%	\+(rule_learned(R,_)),   % FIXME 
	retract_rule(Rn),	% instead of 'delete_rules' (doesn't clean orules)
	th_test_rule(Q,'negative',A),
	last_query(Q,_,A),
	(
	  A=true
	->
	  (
	    assert_rule(Rn),
	    LR =[RVn],
	    LRn =[Rn]

	  )
	;
	  (
	    pathway(Q,Pathp),
	    (th_make_q_true(Q,Pathp,LRp,LRnp); (assert_rule(Rn),fail)),
	    assert_rule(Rn),
	    LR=[RVn|LRp],
	    LRn=[Rn|LRnp]
	  )
	).

% ne backtrack pas
th_score(LRn,Score,Lrequeries):-
	g_assign(nb_Q_a_retraite,0),
	g_assign(q_a_retraite,[]),
	assertset(score_mode),
	retract_rules(LRn),
	assertset(learning_mode),
	test_all,
	retractall(learning_mode),
	g_read(nb_Q_a_retraite,Score),
	g_read(q_a_retraite,Lrequeries),
	assert_rules(LRn),
	retractall(score_mode),
	!.

th_try_add_rule(Q,C):-
	th_add_rule_to_test,
	
	lrule(_,LL,LR,R,LB,RB,V),
%	\+(rule_learned(R,'del')),  % FIXME normalement diminue juste le nombre de r�gle � tester
	(
	  assert_rule(rule(R,LL,LR,R,LB,RB,V))
	;
	  retract_rule(rule(R,LL,LR,R,LB,RB,V))
	),
	th_test_rule(Q,C,true),
	test_all('negative'),	% on ne teste pas les positive qui ne devrait pas avoir changer, et toutes les requ�tes qui sont ajout�es sont true.
	last_query(_,_,true),
	test_all('undefined'),
	last_query(_,_,true),
	add_rule_b(R),
	assertb(rule_learned(R,'add')).	% A v�rifier





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% attention peut cr�er des effets de bords si on fait backtracker revise_model
delete_flag_rule_deleted_and_added:-
	rule_learned(R,'del'),
	rule_learned(R,'add'),
	retract_flag(R),
	fail.
delete_flag_rule_deleted_and_added:-!.

retract_flag(R):-
	!,
	retract(rule_learned(R,'del')),
	retract(rule_learned(R,'add')),
	!.



%%%%%%%%%%%%%%%%%%%
% reduce model


reduce_model:-
   reduce_model((?) => (?)),!.
reduce_model(Biais):-
   (
      \+(spec(_,_,_))
   ->
      (
         have_gui
      ->
         format("[GUI] checkCTL There is no spec, all rules can be deleted. ~n",[])
      ;
         format("There is no spec, all rules can be deleted ~n",[])
      ),
      fail
   ;
      true
   ),
   before_learn_anything,
   assertset(reduit_model_mode),
   retractall(rule_learned(_,_)),
   assertset(rules_added), % juste pour passer le premier tour !
   reduce_model_body(Biais),
   assertset(mols_added),
   assertset(declare_added),
   assertset(rules_added),
   retractall(learning_mode),
   retractall(reduit_model_mode),
   !.


reduce_model_body(Biais):-
%	format("rentre dans reduit_model(~w)~n",[Biais]),
	(
	  (spec(_,_,false) ; \+(rules_added))
	->
	  (
	    reduce_model_body_end
	  )
	;
	  (
	    learn_one_deletion(Biais),
	    reduce_model_body(Biais)
	  )
	).


reduce_model_body_end:-
   %nusmv_time(T),
   %format("Time: ~2f s~n",[T]),
   (
      have_gui
   ->
      format("[GUI] CheckCTL Deletion(s): ~n",[]),
      writeall_rule(rule_learned(R,'del'),R)
   ;
      format("Deletion(s):~n",[])
   ),
   nl,nl,
   !.
%,

%	format("~n ~n ****    New_prososition   ****~n",[]),
%	fail.  % pour calculer tous les models possibles.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% affiche les specs vrai de type Ei(EF(M)), Ei(EF(!(M))), Ei(EG(M)), Ei(EG(!(M))),Ai(oscil(M)), Ai(AG(!(M) -> checkpoint(M2,M))) et  Ai(AG(M -> checkpoint(M2,!(M)))) 
genCTL:-
        g_assign(file,user_output),
 	(
                have_gui
                        ->
                                format("[GUI] genCTL START~n",[])
                        ;
                                true
        ),
        genCTL2.

add_genCTL:-
        g_assign(file,add_spec),
        genCTL2,
        (
                have_gui
                        ->
                                format("[GUI] add_spec FINISHED~n",[])
                        ;
                                true
          ).

genCTL(F):-
   \+(atom(F)),!,
  
        (
                have_gui
                        ->
                                format("[GUI] CheckCTL Error: Filename must be an atom, or be enclosed in simple quotes. ~n",[])
                        ;
   write_line_col('Error'),
                                 write('Filename must be an atom, or be enclosed in simple quotes\n')
          ).
   
        
genCTL(F):-
   (
      sub_atom(F,_,3,0,'.bc')
   ->
      G=F
   ;
      atom_concat(F,'.bc',G)
   ),
   open(G, write, S),
   write(S,'% BIOCHAM generated file of specifications'),
        nl(S),nl(S),        
   g_assign(file,S),
   genCTL2.

genCTL2:-
   nusmv_time(_),
   search_all_molecules,
   assertz(learning_mode),
   test_and_insert_all_ctl, % nusmv(Q),assertz(ctl_learned(Q,R)) Q dans pattern et R dans true or false
   nusmv_time(T),
   (
      have_gui
   ->
      format("[GUI] CheckCTL Time: ~2f s~n",[T])
   ;
      format("Time: ~2f s~n",[T])
   ),
   retract(learning_mode),
   !.

assert_learned(Q,R):-
   (
      R=true 
   ->
      g_read(file,F),
      (
         F=add_spec
      ->
         add_spec(Q)
      ;
 	(
                have_gui
                        ->
                                format("[GUI] genCTL ~w~n",[Q])
                        ;
                                write(F,add_spec(Q)),nl(F),flush_output(F)
          )
      )
   ; 
      true
   ).

test_and_insert_all_ctl:-
	
	molecule(M,B),

	Q_reach=('Ei'(reachable(M))),
   format_debug(3,"trying(~w)~n",[Q_reach]),
	nusmv(Q_reach),
	last_query(_,_,R),
	assert_learned(Q_reach,R),
	
	Q_non_reach=('Ei'(reachable('!'(M)))),
   format_debug(3,"trying(~w)~n",[Q_non_reach]),
	nusmv(Q_non_reach),
	last_query(_,_,R),
	assert_learned(Q_non_reach,R),

	Q_oscil=('Ai'(oscil(M))), %'AG'('&'('->'(M,'EF'('!'(M))),'->'('!'(M),'EF'(M)))))), 
   format_debug(3,"trying(~w)~n",[Q_oscil]),
   nusmv(Q_oscil),
   last_query(_,_,R1),
   assert_learned(Q_oscil,R1),

	
	(
	  \+(init_absent(B))
	->
	  (
	      Q_steady_present='Ei'(steady(M)),
         format_debug(3,"trying(~w)~n",[Q_steady_present]),
         nusmv(Q_steady_present),
         last_query(_,_,R2),
         assert_learned(Q_steady_present,R2)
	   )
	;
	   true
	),
	(
	  \+(init_present(B))
	->
	   (
	      Q_steady_absent='Ei'(steady('!'(M))),
         format_debug(3,"trying(~w)~n",[Q_steady_absent]),
         nusmv(Q_steady_absent ),
         last_query(_,_,R3),
         assert_learned(Q_steady_absent,R3)
	  )
	;
	  true
	),
	
	molecule(M2,_),
	M2\=M,
	
	Q_checkpoint=('Ai'('AG'('->'('!'(M),checkpoint(M2,M))))),
   format_debug(3,"trying(~w)~n",[Q_checkpoint]),
	nusmv(Q_checkpoint),
	last_query(_,_,R4),
	assert_learned(Q_checkpoint,R4),


	Q_checkpoint_non_M=('Ai'('AG'('->'(M,checkpoint(M2,!(M)))))),
   format_debug(3,"trying(~w)~n",[Q_checkpoint_non_M]),
	nusmv(Q_checkpoint_non_M),
	last_query(_,_,R5),
	assert_learned(Q_checkpoint_non_M,R5),
	
	fail. 

test_and_insert_all_ctl:- !.
	
