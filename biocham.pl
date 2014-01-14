% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2003-2010, INRIA Paris-Rocquencourt, EPI Contraintes
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
% GNU prolog file biocham.pl
% by Nathalie Chabrier-Rivier, Francois Fages and Sylvain Soliman


% The rules are stored with the predicate rule(rule as written, leftlist,
% rightlist, expanded rule, left pattern, right pattern, ratelaw)
% The initial state is stored with the predicate initconc(molecule,
% concentration/parameter)

:- initialization(pre_biocham).

:- dynamic(portray/1).

portray(X):-
   float(X),
   get_print_stream(S),
   format(S, "~9g",[X]).


%     flags
:-dynamic(nusmv_running/0).
:-dynamic(rules_added/0).  % lorsque la base de regles a �t� modifi� en ajout ou en retrait.
:-dynamic(mols_added/0).   %  true when new objects since search_molecules.
:-dynamic(declare_added/0).   %  true when new declarations since search_all_molecules.
:-dynamic(initconc_added/0). % true when new initial condition (present or absent) since search_all_molecules.
:-dynamic(new_file/0).     % remind user of saving his modifications
:-dynamic(no_instantiation/0).     % est utilise dans parse_pattern (donc aussi parse_object et parse_molecule) pour is_object_pattern(M,N)
:-dynamic(not_pattern/0).     % est utilise dans parse_molecule (donc aussi parse_object) pour is_object(M,N)
:-dynamic(no_board_effect/0). % utilis�e dans add_rules(elementary_interaction_rules)... afin de ne pas prendre en compte les nouvelles mol�cules ajouter lors de cet ajout de r�gles (que all correspondent toujours aux m�mes mol�cules.
%:-dynamic(init_present/1).   % replaced by initconc
%:-dynamic(init_absent/1).    % same
:-dynamic(gene_list/1). % to count gene compounds in the initial state
:-dynamic(gene_done/1). % to avoid doing it too much
:-dynamic(rule/7).
:-dynamic(initconc/2).  % initial concentration -> present/absent
:-dynamic(orule/1).     % original rules (pattern rules)
:-dynamic(matching/1).  % rules matching current pattern
:-dynamic(molecule/2).
:-dynamic(question_mark/2). % var added to remplace ? in pattern by rename_question_marks
:-dynamic(check_mol/2).
:-dynamic(state/1).
:-dynamic(shown/2).
:-dynamic(hide/2).
:-dynamic(phospho/2).   % declaration of possible phospho sites
:-dynamic(locvar/2).    % local pattern variables var(A) in P

:-dynamic(constrvar/2).    % Constraints on phospho variables
:-dynamic(constrnotvar/2). % 'not in' constraints on phospho variables
:-dynamic(constrdiff/2).      % 'diff' constraints of difference between 2 molecules
:-dynamic(constrnotmodif/2).  % 'not phos_form_of' constraints 
:-dynamic(constrmodif/2).  % 'phos_form_of' constraints
:-dynamic(constrnotmorephos/2).  % 'not more_phos_than' constraints 
:-dynamic(constrmorephos/2).  % 'more_phos_than' constraints
:-dynamic(constrnotsubmol/2).  % 'not sub_mol' constraints 
:-dynamic(constrsubmol/2).  % 'sub_mol' constraints
:-dynamic(constrnocommon/2).  % 'has_no_simple_mol_in_common' constraints 
:-dynamic(constrcommon/2).  % 'has_simple_mol_in_common' constraints 

:-dynamic(th_file/1). % stores the name of the temp file of the "best model found" for theory_revision
:-dynamic(smv_file/1).  % stores the name of the temp file for NuSMV
:-dynamic(smv_file_out/1).  % stores the name of the temp file of the output of NuSMV
:-dynamic(smv_file_out_err/1).  % stores the name of the temp file of redirection of the output of NuSMV
:-dynamic(plot_file/1). % stores the name of the temp files for plotting
:-dynamic(plot_added/1).% true if there is anything to plot (arg is mol list)
:-dynamic(plot_pipe/2). % stores the stream of 'gnuplot', and window #
:-dynamic(plot_color/2).% stores the colors set by the user
:-dynamic(data_file/1).
:-dynamic(read_tok/1).  % used for token parsing

:-dynamic(have_gui/0).  % a GUI is running

% Parsing
%%%%%%%%%

%%% be CAREFUL: if defining multi-chars operators, and their first char is an
%%% operator, please adapt unopify/3

:- op(999,xfx,'where').
:- op(999,xfy,'for').
:- op(1001,xfx,':').
:- op(900,xfx,'=>'). %<1000 to be parenthesized by [ ]
:- op(900,xfx,'<=>').
:- op(700,xfx,'<=').

:- op(900,fx,'declare').
:- op(800,xfy,'and').
:- op(700,xfx,'in').
:- op(700,xfx,'diff').
:- op(700,xfx,'phos_form').
:- op(700,xfx,'more_phos_than').
:- op(700,xfx,'sub_mol').
:- op(700,xfx,'has_simple_mol_in_common').
:- op(700,xfx,'has_no_simple_mol_in_common').
:- op(600,xf,'not').

%%Operators for conditional functions
%:- op(780,xfx,'if').
% :- op(750,fx,'otherwise').
:- op(890,fx,'if').
:- op(870,xfx,'then').
:- op(850,xfx,'else').

%% Operators for constraints
%:- op(700,xfx,'eq').
%:- op(700,xfx,'lt').
%:- op(700,xfx,'gt').
%:- op(700,xfx,'geq').
%:- op(700,xfx,'leq').

%%% Operators for expressions, also used in stoichiometry
%%% THUS one should write in the prolog files 2*(a-b)!!!

:- op(500,xfx,'::').
:- op(490,yfx,'-').
:- op(400,yfx,'~').

:- op(50,fx,'$').  % variables in rules
:- op(50,fx,'@').  % CONTROL VAR notation
:- op(50,fx,'#').  % GENE notation



% reading from the top level, instanciating variables to their atom name
biocham_read(T):-
   readq(user_input,T).   % accept variables in function position
   %read_term(T,[variables(VL),variable_names(V)]),
   %name_vars(V),
   %name_anonymous(VL).

% now used in biochamSbml
name_vars([]).
name_vars([X=X|L]):-name_vars(L).

%name_anonymous([]).
%name_anonymous([X|L]):-
%  (var(X) -> X='_' ; true),
%  name_anonymous(L).

%% Pretty print for checking parentheses
%%pretty(A):-show(A),nl.
%pretty(A):- write_term(A,[ignore_ops(true)]).
% 
%showpar(A):-var(A),!,write(A).
%showpar(A):-A=..[F],!,write(F).
%showpar(A):-A=..[F|L],write(F),write('('),show_list(L),write(')').
%  
%show_list([A]):-!,showpar(A).
%show_list([A|L]):-showpar(A),show_list(L).
  
% Reading from the bc file

bc_read(BC):-
	(at_end_of_stream(BC)
	->
	    true
	;
       bc_read(BC,T),
       (
          catch(treat(T),E,write_error(E)),!
       ;
          true
       ),
	    bc_read(BC)).

bc_read(BC,T):-
   readq(BC,T).
   %read_term(bc,T,[variables(VL),variable_names(V)]),
   %name_vars(V),
   %name_anonymous(VL).

% unique variables represent Genes or States (like aligned chromosomes)

treat(end_of_file):-!.

treat(declare(D)):-
   !,treat_phospho_def(D).

treat(RD):-
   is_rule(RD),!,
   retractall(question_mark(_,_)),
   rename_question_marks(RD,RDD),
   treat_rule_with_def(RDD,RD).

treat(A):-
   format("~n~p.~n",[A]),!,
	call(A).


%%%Asserts a rule - the rate law, stoichiometry everything

assert_rule(R,[],[],_,_):-
   !,
   (
      have_gui
   ->
      format("[GUI] warnings Warning: Ignoring dummy rule ~w ~n",[R])
   ;
      write_line_col('Warning'),
      format("Ignoring dummy rule ~w ~n",[R])
   ).

assert_rule(R,LL,LR,OR,V):-
	check_rule(R,LL,LR),
	pattern_list(LL,LB), 
	pattern_list(LR,RB),!,
   (
      equiv(R,R1,R2)
   ->  
      (
         (
            V = (V1,V2)
         ;
            (V = 0), % if dummy kinetics, no warning
            (V1 = 0),
            (V2 = 0)
         )
      ->
         check_and_assert_rule(OR,LL,LR,R1,LB,RB,V1),
         check_and_assert_rule(OR,LR,LL,R2,RB,LB,V2)
      ;
        (
                have_gui
        ->
                format("[GUI] warnings Warning: Insufficient Rate Law information - will affect kinetic calculations! You might want to re-check rate information for this rule: ~w~n",[R])
        ;
         write_line_col('Warning'),
         write('Insufficient Rate Law information - will affect kinetic calculations!'),nl,
         write('You might want to re-check rate information for this rule:\n'),
         write(R),nl
      )
       
      )
   ;
      check_and_assert_rule(OR,LL,LR,R,LB,RB,V)
   ).

check_and_assert_rule(_,LL,LR,_,_,_,_):-
      learning_mode,
      (rule(_,LL,LR,_,_,_,_);lrule(_,LL,LR,_,_,_,_)), % rule with same meaning
      !.


check_and_assert_rule(OR1,LL,LR,R1,_,_,_):-
   rule(OR2,LL,LR,R2,_,_,_),!,   % rule with same meaning
   (
      (OR1 = OR2)                % if coming from same o_rule
   ->                            % ignore silently
      true
   ;
        (
                have_gui
        ->
                format("[GUI] warnings Warning: A rule with same reactants and products has been found: ~w and ~w . Ignoring the duplicate entry.~n",[R1,R2])
        ;
      write_line_col('Warning'),
      write('A rule with same reactants and products has been found:\n'),
      write(R1),nl,
      write(R2),nl,
      write('Ignoring the duplicate entry\n')
        )
     
   ).

check_and_assert_rule(OR1,LL,LR,R1,LB,RB,V):-
   check_kinetics(LL,V),
   (
      learning_mode
    ->
      assertset(lrule(OR1,LL,LR,R1,LB,RB,V))
    ;
      (
        assertset(rule(OR1,LL,LR,R1,LB,RB,V)),
        (
                have_gui
        ->
                format("[GUI] add_rule ~w;~w;~w~n",[OR1,V,R1])
        ;
                true % write(OR1),nl
        ),
        assertset(mols_added),
        assertset(declare_added),
        assertset(rules_added)
      )
    ).

assertset(A):-
	(A -> true; assertz(A)).


% Parsing molecules

% TODO a better check

parse_name(A) :-
	atom(A),
   A \= (?),
%   A \= '_',  %% pourquoi on ne l'autorise pas pour l'enlever ( molA-_-molC deviendrait molA-molC)?
%    A \= 'A',
%    A \= 'E',
%    A \= 'U',
%    A \= 'AF',
%    A \= 'EF',
%    A \= 'AG',
%    A \= 'EG',
%    A \= 'AX',
%    A \= 'EX',
%    A \= 'xor',
   !.

parse_object(A,B) :-
   parse_abstract(A,B),!.
   
parse_object(A,AA):-
   parse_molecule(A,AA),
   !,
   (
      AA = B::_
   ->
      true        % FIXME: check location declaration?
   ;
      AA = B
   ),
   (
      B = C~D,!   % get the molecule in 'phosphorylated form'
   ;
      (
         C=B,
         D={}     % empty if necessary
      )
   ),
   (
      check_declare(C,D),!
   ;
     
        (
        have_gui
                ->
                        format("[GUI] errors Error: Trying to add a rule which violates the declaration of possible phosphorylation sites of ~w~n",[C])
                ;
      write_line_col('Error'),
                        write('trying to add a rule which violates the declaration of possible\n'),
      write('   phosphorylation sites of '),
      write(C),
                        nl
        ),
      fail
   ).

parse_abstract(@(A),@(A)):-
   fail. %parse_name(A),!.
   
parse_molecule(A::B,AA::C):-
   parse_molecule(A,AA),
   AA \= _::_,
   (
      parse_name(B)
   ->
      C=B
   ;
      parse_pattern(B,C),
      parse_name(C)
   ),!.

parse_molecule(A-B,M):- 
   parse_molecule(A,C),
	parse_molecule(B,D),
   merge_sort_complex(C,D,M),!.  % associativity, commutativity

parse_molecule(A~{B},E):- 
	parse_molecule(A,C),!,
   (
      C = '_'
   ->
      E = '_'
   ;
      (
         C = C1~{C2}
      ->
         (
            goal_to_list(C2,LC),
            goal_to_list(B,LB),
            append(LC,LB,LLB),
            sort(LLB),
            list_to_goal(LLB,D),
            E=C1~{D}
         )
      ;
         (
            goal_to_list(B,LB),
	         sort(LB),
	         list_to_goal(LB,D),
            E=C~{D}
         )
      )
   ).

parse_molecule(A,A):-
   A= #(B),
   parse_name(B),!.

parse_molecule(A,B) :-
	(
      not_pattern
   ->
      fail
   ;
	   parse_pattern(A,B)
   ),!.

parse_molecule(A,A):-
   parse_name(A),!.

parse_molecule(A,_):-
        (
         have_gui
                 ->
                         format("[GUI] checkLTL Error: parsing molecule ~w .~n",[A])
                 ;
                           true
        ),
   throw('Error: parsing molecule '(A)).  % Error???

%%% Parse CTL queries and normalize molecules inside


parse_query('Ei'(A),'Ei'(B)):-
   parse_ctl(A,B).
parse_query('Ai'(A),'Ai'(B)):-
   parse_ctl(A,B).

parse_ctl(A,B):-
   functor(A,O,1),
   memberchk(O,['!','EF','EG','EX','AF','AG','AX',reachable,stable,steady,oscil]),!, % FF
   A=..[O,C],
   parse_ctl(C,D),
   B=..[O,D].
parse_ctl(A,B):-
   functor(A,O,1),
   memberchk(O,['E','A']),!,
   A=..[O,C],
   parse_path(C,D),
   B=..[O,D].
parse_ctl(A,B):-
   functor(A,O,2),
   memberchk(O,['&','|','->','<->', checkpoint, loop]),!, % FF
   A=..[O,C,D],
   parse_ctl(C,E),
   parse_ctl(D,F),
   B=..[O,E,F].
parse_ctl(A,B):-
   parse_object(A,B).

parse_path('U'(A,B),'U'(C,D)):- !,
   parse_ctl(A,C),
   parse_ctl(B,D).
parse_path('W'(A,B),'W'(C,D)):- !,
   parse_ctl(A,C),
   parse_ctl(B,D).
parse_path('F'(A),'F'(B)):- !,
   parse_ctl(A,B).
parse_path('G'(A),'G'(B)):- !,
   parse_ctl(A,B).
parse_path('X'(A),'X'(B)):- !,
   parse_ctl(A,B).

%%% check if a molecule's phosphorylation sites declaration exists
%%% and if so, if the given set of sites is included

check_declare(M,S):-
   g_read(no_declare_check,C),   % can be de-activated, ex: in parse_molset
   (
      C=1
   ->
      true
   ;
      (
         phospho(M,L)
      ->
         member(S,L)
      ;
         true
      )
   ).

%goal_to_list(A,[A]):- var(A),!.
goal_to_list((A,B),L):-!,
   parse_name(A), % only 'names' in phosphorylations.
	goal_to_list(B,LB),
   L = [A|LB].
goal_to_list(A,[A]):- parse_name(A).   % only 'names' in phosphorylations.

list_to_goal([A],A):-!.
list_to_goal([A|L],(A,B)):-
	list_to_goal(L,B).

merge_sort_complex('_',C,C) :-!.
merge_sort_complex(C,'_',C) :-!.

merge_sort_complex(A-B,C-D,M) :-
   !,
   (B@<D ->
      (M = N-D,
         merge_sort_complex(A-B,C,N));
      (M = N-B,
         merge_sort_complex(A,C-D,N))).

merge_sort_complex(A-B,C,M) :-
   !,
   (B@=<C ->
      (M = A-B-C );
      (M = N-B,
         merge_sort_complex(A,C,N))).

merge_sort_complex(A,C-D,M) :-
   !,
   (D@=<A ->
      (M = C-D-A);
      (M = N-D,
         merge_sort_complex(A,C,N))).

merge_sort_complex(A,C,M) :-
   !,
   (A@=<C -> M=A-C;M=C-A).
   
% Parsing solutions

parse_solution('_',[],'_'):-!.
parse_solution(A+B,L,C):-
	parse_solution(A,LA,AA),
	parse_solution(B,LB,BB),
   !,
	(
      (AA = '_')
   ->
      (C = BB),
      (L = LB)
   ;
      (
         (BB = '_')
      ->
         (C = AA),
         (L = LA)
      ;
         merge_sort_solution(LA,LB,L),
         expand_solution(L,C)
      )
   ).

parse_solution(V*A,C,D):-
	!,
   parse_object(A,B),!,
   (
      float(V),
      TV is truncate(V),
      TV =:= V
   ->
      VV = TV
   ;
      VV = V
   ),
   (
      B = '_'
   ->
      C = []
   ;
      C=[(VV,B)]
   ),
   (
      VV=1
   ->
      D=B
   ;
      D=VV*B
   ).
parse_solution(A,C,B) :-
   parse_solution(1*A,C,B).

% merge two sorted molecule lists

merge_sort_solution([],L,L).
merge_sort_solution(L,[],L).

merge_sort_solution([(S1,M1)|T1],[(S2,M2)|T2],[(S2,M2)|L]):-
   M1 @> M2,!,
   merge_sort_solution([(S1,M1)|T1],T2,L).
   
merge_sort_solution([(S1,M1)|T1],[(S2,M1)|T2],[(S3,M1)|L]):-
   !,S3 is S1+S2,
   merge_sort_solution(T1,T2,L).
   
merge_sort_solution([(S1,M1)|T1],[(S2,M2)|T2],[(S1,M1)|L]):-
   merge_sort_solution(T1,[(S2,M2)|T2],L).
   
% get a solution from a list of (stoichiometry,molecule)

expand_solution(List, Solution) :-
   reverse(List, RList),
	expand_solution0(RList, Solution).

expand_solution0([],'_').
expand_solution0([(S,M)],E):-
   (
      S=1
   ->
      E=M
   ;
      E=S*M
   ).
expand_solution0([(S1,M1),(S2,M2)|L],S+E):-
   (
      S1=1
   ->
      E=M1
   ;
      E=S1*M1
   ),
   expand_solution0([(S2,M2)|L],S).

% Parsing rules

% These two case come from expansion of object patterns equal to '_'
% FIXME: this is syntactically incorrect
parse_rule(A=[$(V)]=>B,LL,LR,R):-
   parse_object($(V),VV),(VV='_'),
   !,parse_rule(A=>B,LL,LR,R).

parse_rule(A<=[$(V)]=>B,LL,LR,R):-
   parse_object($(V),VV),(VV='_'),
   !,parse_rule(A<=>B,LL,LR,R).


parse_rule(A=[R]=>B,LL,LR,AA =[ RR ]=> BB):-
   parse_solution(A,LA,AA),
	parse_solution(B,LB,BB),
   (
      R = (_=>_), parse_rule(R,L1,L2,RR)
   ->
      true
   ;
      parse_object(R,RR),L1=[(1,RR)], L2=L1
   ),
	merge_sort_solution(L1,LA,LL),
	merge_sort_solution(L2,LB,LR),!.
parse_rule(A<=[R]=>B,LL,LR,AA<=[M]=>BB):-
   parse_solution(A,LA,AA),
   parse_solution(B,LB,BB),
   parse_object(R,M),
   merge_sort_solution([(1,M)],LA,LL),
   merge_sort_solution([(1,M)],LB,LR),!.
parse_rule(A<=>B,LA,LB,AA <=> BB):-
	parse_solution(A,LA,AA),
	parse_solution(B,LB,BB),!.
parse_rule(A=>B,LA,LB,AA=>BB):-
   parse_solution(A,LA,AA),
   parse_solution(B,LB,BB),!.

is_rule((_:R)):-
   is_rule(R).

is_rule(R where _) :-
   is_rule(R).

is_rule(_ for R):-
   is_rule(R).

is_rule(_ => _).

is_rule(_ <=> _).

is_rule(_ =[ _ ]=> _).

is_rule(_ <=[ _ ]=> _).


% true if
% M is an object pattern,
% and Mnormal  the normalized form of M (variable's names dont change)
is_object_pattern(M,Mnormal):-
	assertz(no_instantiation),
	parse_object(M,Mnormal),
	!,
	retractall(no_instantiation),
	\+(var(Mnormal)).
is_object_pattern(M):-
	is_object_pattern(M,Mnormal),
	!,
	\+(var(Mnormal)).

% true if
% M is an object and not an object_pattern (no variable and no ?)
% and Mnormal  the normalized form of M 
is_object(M,Mnormal):-
	assertz(not_pattern),
	parse_object(M,Mnormal),
	!,
	retractall(not_pattern),
	\+(var(Mnormal)).
is_object(M):-
	is_object(M,Mnormal),
	!,
	\+(var(Mnormal)).


%%%Rate law parsing - handles conditional functions and normal functions

% Mass Action Law
parse_rate_law('MA'(K),LL,_,MA):-
   !,make_prod(LL,P),
   simplify(K*P,MA).
% Michaelis Menten
% on the first reactant that is not a product
% parse_rate_law('MM'(V,K),LL,LR,(V*[M])/(K+[M])):-
%   !,
%   remove_common(LL,LR,[(SM,M)|UL],_UR),
%   (
%      (SM > 1 ; UL \= [])
% FF: Michaelis Menten on the product of the reactants
parse_rate_law('MM'(V,K),LL,_,VP/KP):-
   !,make_prod(LL,P),
   simplify(V*P,VP),
   simplify(K+P,KP),
   (
      (P \= [_]^1)
   ->
     
        (
                have_gui
        ->
                format("[GUI] warnings Warning: Michaelis-Menten kinetics shortcut used for a reaction with more than one reactant.~n",[])
        ;
      write_line_col('Warning'),
      write('Michaelis-Menten kinetics shortcut used for a reaction with more than one reactant.\n')
        )
   ;
      true
   ).

% parse_rate_law('H'(V,K,N),LL,LR,(V*([M]^N))/(K^N+[M]^N)):-
%   !,
%   remove_common(LL,LR,[(SM,M)|UL],_UR),
%   (
%      (SM > 1 ; UL \= [])
parse_rate_law('H'(V,K,N),LL,_,VPN/KPN):-
   !,make_prod(LL,P),
   simplify(V*(P^N),VPN),
   simplify(K^N+P^N,KPN),
   (
      (P \= [_]^1)
   ->
      
        (
                have_gui
        ->
                format("[GUI] warnings Warning: Hill kinetics shortcut used for a reaction with more than one reactant.~n",[])
        ;
      write_line_col('Warning'),
      write('Hill kinetics shortcut used for a reaction with more than one reactant.\n')
        )
   ;
      true
   ).

% parse_rate_law('HN'(V,K,N),LL,LR,(V*(K^N))/(K^N+[M]^N)):-
%   !,
%   remove_common(LL,LR,[(SM,M)|UL],_UR),
%   (
%      (SM > 1 ; UL \= [])
%   ->
%     
%        (
%                have_gui
%        ->
%                format("[GUI] warnings Warning: Hill Negative kinetics shortcut used for a reaction with more than one reactant.~n",[])
%        ;
%      write_line_col('Warning'),
%      write('Hill Negative kinetics shortcut used for a reaction with more than one reactant.\n')
%        )
%   ;
%      true
%   ).
parse_rate_law(N,_,_,N) :-
   number(N),!.
parse_rate_law(N*X,LL,LR,N*XX)  :- 
   number(N),!,
   parse_rate_law(X,LL,LR,XX).
parse_rate_law(X^Y,LL,LR,XX^YY) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(X*Y,LL,LR,XX*YY) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(X/Y,LL,LR,XX/YY) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(X+Y,LL,LR,XX+YY) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(X-Y,LL,LR,XX-YY) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(min(X,Y),LL,LR,min(XX,YY)) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(max(X,Y),LL,LR,max(XX,YY)) :- 
   !,parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law(-Y,LL,LR,-YY) :- 
   !,parse_rate_law(Y,LL,LR,YY).
parse_rate_law([X],_,_,[XX]) :- 
   !,parse_object(X,XX).
parse_rate_law(log(X),LL,LR,log(XX)) :- 
   !,parse_rate_law(X,LL,LR,XX).
parse_rate_law(exp(X),LL,LR,exp(XX)) :- 
   !,parse_rate_law(X,LL,LR,XX).
parse_rate_law(sin(X),LL,LR,sin(XX)) :- 
   !,parse_rate_law(X,LL,LR,XX).
parse_rate_law(cos(X),LL,LR,cos(XX)) :- 
   !,parse_rate_law(X,LL,LR,XX).
parse_rate_law(abs(X),LL,LR,abs(XX)) :- 
   !,parse_rate_law(X,LL,LR,XX).
parse_rate_law(frac(X), LL, LR, frac(XX)) :-
   !,
   parse_rate_law(X,LL,LR,XX).

% Accept parameters or macros and random function
parse_rate_law(P,_,_,P) :-
%   k_parameter(P,_),!.    % only if already defined
   !.

parse_rate_law_constr(X < Y,LL,LR,XX < YY) :-
   parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law_constr(X > Y,LL,LR,XX > YY) :-
   parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law_constr(X = Y,LL,LR,XX = YY) :-
   parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law_constr(X =< Y,LL,LR,XX =< YY) :-
   parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).
parse_rate_law_constr(X >= Y,LL,LR,XX >= YY) :-
   parse_rate_law(X,LL,LR,XX),
   parse_rate_law(Y,LL,LR,YY).

parse_conditions(X,LL,LR,XX) :-
   parse_rate_law_constr(X,LL,LR,XX).
parse_conditions(X and E,LL,LR,XX and EE) :-
   parse_rate_law_constr(X,LL,LR,XX),
   parse_conditions(E,LL,LR,EE).


parse_cond_syntax(if C then E else F,LL,LR,if CC then EE else FF) :-
   !,
   parse_rate_law(E,LL,LR,EE),
   parse_conditions(C,LL,LR,CC),
   parse_cond_syntax(F,LL,LR,FF).
parse_cond_syntax(if C then E,LL,LR,if CC then EE) :-
   !,
   parse_rate_law(E,LL,LR,EE),
   parse_conditions(C,LL,LR,CC).
parse_cond_syntax(E,LL,LR,EE) :-
   parse_rate_law(E,LL,LR,EE).


parse_the_rate_law((V1,V2),LL,LR,(VV1,VV2)) :-
   !,
   parse_cond_syntax(V1,LL,LR,VV1),
   parse_cond_syntax(V2,LR,LL,VV2).
parse_the_rate_law(V,LL,LR,VV) :-
   parse_cond_syntax(V,LL,LR,VV).

make_prod([],1).
make_prod([(C,M)],[M]^C):-!.
make_prod([(C,H)|T],[H]^C*P):-
   make_prod(T,P).

% Remove (stoechiometrically) catalyzers
remove_common([],KR,[],KR).
remove_common([(SL,M)|KL],KR,UKL,UKR):-
   (
      select((SR,M),KR,KR1)
   ->
      (
         SL>SR
      ->
         SL2 is SL-SR,
         UKL=[(SL2,M)|KL1],
         remove_common(KL,KR1,KL1,UKR)
      ;
         (
            SL<SR
         ->
            SR2 is SR-SL,
            UKR=[(SR2,M)|KR2],
            remove_common(KL,KR1,UKL,KR2)
         ;
            remove_common(KL,KR1,UKL,UKR)
         )
      )
   ;
      UKL=[(SL,M)|KL1],
      remove_common(KL,KR,KL1,UKR)
   ).

treat_rule_with_def((_:R), OR):-
   !,treat_rule_with_def(R,OR).

treat_rule_with_def(V for R where D,OR):-
   !,
   format_debug(2,"treating ~w where ~w~n",[R,D]),
   retractall(constrvar(_,_)), %% pourquoi pas retractall(constrnotvar(_,_)) virer dans remove_neg ?
   retractall(constrdiff(_,_)),
   retractall(constrnotmodif(_,_)),
   retractall(constrmodif(_,_)),
   retractall(constrnotmorephos(_,_)),
   retractall(constrmorephos(_,_)),
   retractall(constrnotsubmol(_,_)),
   retractall(constrsubmol(_,_)),
   retractall(constrnotcommon(_,_)),
   retractall(constrcommon(_,_)),
   search_all_molecules, % afin de prendre en compte les "declare" et les "present" "absent"
   format_debug(2,"init done~n",[]),
   find_vars_to_instantiate(R,LV),
   format_debug(2,"vars to be instantiated are: ~w~n",[LV]),
   find_constr(D),
   format_debug(2,"constr done~n",[]),
   get_cv(LC),
   format_debug(2,"vars to be constrained are: ~w~n",[LC]),!,
   setof(LLV,(instantiate_cv(LC),instantiate(D),find_values(LV,LLV)),I),
   format_debug(2,"found values: ~w~n",[I]),
   assert_rule_with_def(LV,I,R,OR,V),
   (
     learning_mode
     ->
     true
     ;
     assertz(orule(OR))
   ).

treat_rule_with_def(V for R,OR) :-
   !,
   format_debug(2,"treating ~w~n",[R]),
   retractall(constrvar(_,_)),
   format_debug(2,"init done~n",[]),
   find_vars_to_instantiate(R,LV),
   format_debug(2,"vars to be instantiated are: ~w~n",[LV]),
   get_cv(LC),
   format_debug(2,"vars to be constrained are: ~w~n",[LC]),
   setof(LLV,(instantiate_cv(LC),find_values(LV,LLV)),I),
   format_debug(2,"found values: ~w~n",[I]),
   assert_rule_with_def(LV,I,R,OR,V),
   (
     learning_mode
     ->
     true
     ;
     assertset(orule(OR))
   ).

treat_rule_with_def(R,OR):-
   (R \= (_ for _)),
% FF 
%   treat_rule_with_def(0 for R, OR).
   (equiv(R)
    ->
    treat_rule_with_def(('MA'(1),'MA'(1)) for R, OR)
    ;
    treat_rule_with_def(('MA'(1)) for R, OR)
   ).

% TODO expand_pattern

% Parsing patterns

% FIXME Error message repeated if incorrect rule has several instances
parse_pattern($(A),VA):-
   !,
   (
      no_instantiation
   ->
      VA=($(A))
   ;	
      (
         locvar($(A),VA)
      ->
         true
      ;
         (
           
            (
                have_gui
                        ->
                                format("[GUI] errors Error: Variable ~w is used but not defined.~n",[A])
                        ;
            write_line_col('Error'),
            write('variable '),write($(A)),
                                write(' used but not defined\n')
            ),
            fail
         )
      )
   ).

% TODO check if used at all
parse_pattern(A~ (?) ,B~ VA):-
   parse_molecule(A,B),
   (
      no_instantiation
   ->
      VA=(?)
   ;
      locvar((?)-_ ,VA),!,
      retract(locvar((?)-_ ,VA))
   ).

parse_pattern(A~ $(V),D):-
   !,
   (
      no_instantiation
   ->
      (
         parse_molecule(A,B),
         D=B~ $(V)
      )
   ;
      locvar($(V),VV),
      (
         VV == {}
      ->
         parse_object(A,D)
      ;
         parse_object(A~ VV,D)
      )
   ).

% parsing sets

parse_parts({P},L) :-
   goal_to_list(P,L1),
   sort(L1),
   setof(LL,X^(sublist(X,L1),list_to_set(X,LL)),L).

list_to_set([],{}).
list_to_set([H|T],{G}):-
   list_to_goal([H|T],G).



merge_sort([],L,L).
merge_sort(L,[],L).


merge_sort([M|T1],[M|T2],[M|L]):-!,
   merge_sort(T1,T2,L).
merge_sort([M1|T1],[M2|T2],[M2|L]):-
   M1 @> M2,!,
   merge_sort([M1|T1],T2,L).
   
merge_sort([M1|T1],[M2|T2],[M1|L]):-!,
   merge_sort(T1,[M2|T2],L).
   

%set_object_pattern_to_list(S,L):-
% S unnormalized set of object patterns
% L sorted list of the normalized versions of the objects of S
set_object_pattern_to_list({},[]):-!.

set_object_pattern_to_list({A,S},L):-
	set_object_pattern_to_list({A},LA),
	set_object_pattern_to_list({S},LS),
	merge_sort(LA,LS,L).

set_object_pattern_to_list({A},[An]):-
	is_object_pattern(A,An).
		

assert_list_diff(A,L):-
	member(M1,L),
	is_object_pattern(M1,M),
	(
      (
         constrdiff(A,M)
      ;
         constrdiff(M,A)
      )
   ->
      true
   ;
	   assertz(constrdiff(A,M))
	),
	fail.
assert_list_diff(_,_):-!.
	


parse_molset(('_',B),L) :-
   parse_molset(B,L1),
   L = ['_'|L1],!.
   
parse_molset('_',['_']) :-
   !.
   
parse_molset((A,B),L) :-
   !,
   parse_molset(A,L1),
   parse_molset(B,L2),
   append(L1,L2,L).
   
parse_molset(A~(?),L) :-
   !,parse_molecule(A,B),
   (
      phospho(B,L1)
   ->
      (
         map_phospho(B,L1,L)
      )
   ;
      (
        
         (
          have_gui
                ->
                        format("[GUI] errors Error: Using ~? for the molecule ~w but no declaration of its possible sutes.~n",[A])
                ;
         write_line_col('Error'),
         write('using ~? for the molecule '),
         write(A),
                        write('\n   but no declaration of its possible sites.\n')
         ),
         fail
      )
   ).

parse_molset(A-B,L):-
   !,
   parse_molset(A,L1),
   parse_molset(B,L2),
   complex_product(L1,L2,L).

parse_molset(A,[MA]) :-
   g_assign(no_declare_check,1),
   (
      parse_object(A,MA)
   ->
      g_assign(no_declare_check,0)
   ;
      g_assign(no_declare_check,0),
      fail
   ).
 
map_phospho(_,[],[]).
map_phospho(M,[{}|T],[M|PT]):-
   !,map_phospho(M,T,PT).
map_phospho(M,[H|T],[M~H|PT]):-
   map_phospho(M,T,PT).

complex_product([],_,[]).
complex_product([H|T],L,C):-
   complex_rec(H,L,HL),
   complex_product(T,L,C1),
   append(HL,C1,C).

complex_rec(_,[],[]).
complex_rec(A,[H|T],[A-H|L]):-
   complex_rec(A,T,L).

parse_phosphoset(({A},B),L) :-
   goal_to_list(A,L1),
   sort(L1,L2),
   list_to_goal(L2,MA),
   parse_phosphoset(B,L3),
   L = [{MA}|L3],!.
   
parse_phosphoset({A},L) :-
   goal_to_list(A,L1),
   sort(L1,L2),
   list_to_goal(L2,MA),
   L = [{MA}],!.
   
parse_phosphoset(({},B),L) :-
   parse_phosphoset(B,L2),
   L = [{}|L2],!.
   
parse_phosphoset({},[{}]) :-
   !.
   
% parsing phosphorylation sites declarations

treat_phospho_def(M~P):-
   parse_molecule(M,N),
   (
      P = parts_of(Q)
   ->
      parse_parts(Q,L)
   ;
      (
         (
            P = {Q},parse_phosphoset(Q,L),
            !
         )
      ;
         (
           
            (
                have_gui
                        ->
                                format("[GUI] errors Cannot parse the set of phosphorylation sites.~n",[P])
                        ;
            write_line_col('Error'),
            write('cannot parse the set of phosphorylation sites '(P)),
                                nl
            ),
            fail
         )
      )
   ),
   (
      phospho(N,_)
   ->
      (
        
        (
                have_gui
                        ->
                                format("[GUI] warnings Phosphorylation sites of ~w already defined. Ignoring...~n",[M])
                        ;
         write_line_col('Warning'),
         write('phosphorylation sites of '),write(M),
         write(' already defined\n   ignoring...\n')
      )
      )
   ;
      (
         (
            member({},L)
         ->
            true
         ;
            (
              
                (
                have_gui
                        ->
                                format("[GUI] warnings Phosphorylation sites of ~w do not include {}. The molecule ~w will be refused in non-phosphorylated form~n",[M,M])
                        ;
               write_line_col('Warning'),
               write('phosphorylation sites of '),
               write(M),
               write(' do not include {}\n   the molecule '),
               write(M),
               write(' will be refused in non-phosphorylated form.\n')
            )
            )
         ),
         assertz(phospho(N,L)),
         assertset(declare_added)
      )
   ),
(
                have_gui
        ->
                format("[GUI] declare ~w;~w~n",[M, L])
        ;
                true
   ).

%%% Parsing pattern variables

% treated in find_constr
treat_local_patvar(_ not in _) :- !.
treat_local_patvar(_ diff _) :-    !.
treat_local_patvar(_ phos_form _):-!.
treat_local_patvar(_ not phos_form _):-!. 
treat_local_patvar(_ more_phos_than _):-!.
treat_local_patvar(_ not more_phos_than _):-!. 
treat_local_patvar(_ sub_mol  _):-!.
treat_local_patvar(_ not sub_mol _):-!.
treat_local_patvar(_ has_simple_mol_in_common _):-!.
treat_local_patvar(_ has_no_simple_mol_in_common  _):-!.

%%% 'in' constraints only reduce an existing domain (intersection semantics)

treat_local_patvar($(V) in all) :-
   format_debug(3,"values for ~w are all molecules already parsed~n",[$(V)]),
   (
      locvar($(V),_)
   ->  
      % if we already have a value, we're done
      format_debug(4,
      "~w in all, but a value already exists, ignoring...~n",[$(V)])
   ;
      (
         (
            Mnorm='_'
         ;
            molecule(Mnorm,_)
         ),
         authorizedval($(V),Mnorm),
         assertb(locvar($(V),Mnorm))
      )
   ).

% TODO genes? no process (@)
treat_local_patvar($(V) in all_simple) :-
   format_debug(3,"values for ~w are all simple molecules already parsed~n",[$(V)]),
   (
      locvar($(V),Mnorm),  % if we already have a value, check it
      format_debug(4,
      "~w in all_simple, but a value already exists, checking...~n",[$(V)]),
      !
   ;
      true                 % else take any that passes the check
   ),
   (
      Mnorm='_'
   ;
      molecule(Mnorm,Mnorm)
   ),
   authorizedval($(V),Mnorm),
   assertb(locvar($(V),Mnorm)).


treat_local_patvar($(V) in Q) :-
   (
      Q = parts_of(P)
   ->
      parse_parts(P,L)
   ;
      (
         Q = {P},
         (
            parse_phosphoset(P,L);
            parse_molset(P,L)
         )
      )
   ),
   format_debug(3,"possible values for ~w: ~w~n",[$(V),L]),
   (
      locvar($(V),M),   % if we already have a value, check it
      format_debug(4,
      "~w in ~w, but a value already exists, checking...~n",[$(V),L]),
      !
   ;
      true              % else take any that passes the check
   ),
   member(M,L),
   authorizedval($(V),M),
   assertb(locvar($(V),M)).

%%% generate a value from positive constraints
get_value_from_positive_constraints($(Var),Val):-
   constrsubmol($(Var),B),
	format_debug(4,"found positive constraint ~w sub_mol ~w~n",[$(Var),B]),
   get_cur_value(B,Val2),
	submolgen(Val2,Val),
   format_debug(4,"trying: ~w~n",[Val]).

get_value_from_positive_constraints($(Var),Val):-
   constrmodif($(Var),B),
	format_debug(4,"found positive constraint ~w phos_form ~w~n",[$(Var),B]),
	get_cur_value(B,Val2),
   get_molecule(Val2,M),
	morephosgen(M,Val),
   format_debug(4,"trying: ~w~n",[Val]).

get_value_from_positive_constraints($(Var),Val):-
   constrmorephos($(Var),B),
	format_debug(4,"found positive constraint ~w more_phos ~w~n",[$(Var),B]),
	get_cur_value(B,Val2),
	morephosgen(Val2,Val),
   format_debug(4,"trying: ~w~n",[Val]).

   
authorizedval($(V),Val):-
	format_debug(7,"is ~w forbidden for ~w?~n",[Val,$(V)]),
   \+(forbiddenval($(V),Val)),
	format_debug(7,"no~n",[]),
   format_debug(7,"is ~w satisfactory for ~w?~n",[Val,$(V)]),
	obligatory_form_of_val($(V),Val),
   format_debug(7,"yes~n",[]). 

%%% Utility function

get_cur_value(Obj,Val):-
   (
     Obj = $(Var)
   ->
     locvar($(Var),Val)
   ;
     parse_object(Obj,V),
     V=Val
   ).

%%% forbiddenval($(V),Val) is true if Val is not a possible value for V

% variables and var/value
forbiddenval($(V),Val):-
   (
      constrdiff($(V),B)
   ;
      constrdiff(B,$(V))
   ),
   get_cur_value(B,Val).

% variable must differ by more than phospho
forbiddenval($(V),Val):-
	(
      constrnotmodif($(V),B)
   ;
      constrnotmodif(B,$(V))
   ),
	get_cur_value(B,Val2),
   phosphorilated_form(Val,Val2).


% variable must not be a phosphorylation of... (not symmetrical)
forbiddenval($(V),Val):-
	constrnotmorephos($(V),B),
	get_cur_value(B,Val2),
	more_phosphorilated_than(Val,Val2).


% variable must not be a submolecule of pattern
forbiddenval($(V),Val):-
	constrnotsubmol($(V),B),
	get_cur_value(B,Val2),
	submolecule(Val2,Val).

% mustn't have a common submol
forbiddenval($(V),Val):-
	(
      constrnocommon($(V),B)
   ;
      constrnocommon(B,$(V))
   ),
	get_cur_value(B,Val2),
	common_compound(Val,Val2).


% obligatory_form_of_val($(V),val)
% is true if val fulfills all constraints on the 'form' of $V
%
% always: cut and fail if not satisfactory

% phosphorylation
obligatory_form_of_val($(V),Val):-
	(
      constrmodif($(V),B)
   ;
      constrmodif(B,$(V))
   ),
	get_cur_value(B,Val2),
	\+(phosphorilated_form(Val,Val2)),
	!,fail.

	
% more phosphorylation
obligatory_form_of_val($(V),Val):-
	constrmorephos($(V),B),
	get_cur_value(B,Val2),
	\+(more_phosphorilated_than(Val,Val2)),
	!,fail.
	
% submolecule
obligatory_form_of_val($(V),Val):-
	constrsubmol($(V),B),
	get_cur_value(B,Val2),
	\+(submolecule(Val2,Val)),
	!,fail.
	
% must have common submol
obligatory_form_of_val($(V),Val):-
	(
      constrcommon($(V),B)
   ;
      constrcommon(B,$(V))
   ),
	get_cur_value(B,Val2),
	\+(common_compound(Val,Val2)),
	!,fail.
	
% suceeds if everything before wasn't already 'cut and failed'
obligatory_form_of_val(_,_):-!.


more_phosphorilated_than(M,N):-
	g_assign(non_sym,true),
	phosphorilated_form(M,N,1),
   g_assign(non_sym,false). 

% M and N (normalized) differ only by some phosphorylation
phosphorilated_form(M,N):-phosphorilated_form(M,N,0),!. 

phosphorilated_form(M,M,_):-!.
phosphorilated_form(M~_,M,_):-!.
phosphorilated_form(M~S,M~Sp,_):-
	(g_read(non_sym,true) ->subset(Sp,S);true), !.
phosphorilated_form(M-N,Mp-Np,_):-
	(g_read(non_sym,true) ->T=1;T=0),
	phosphorilated_form(M,Mp,T), phosphorilated_form(N,Np,T),!.
phosphorilated_form(A,B,T):-T\=1,!, phosphorilated_form(B,A,1),!.

% N and M (normalized) are such that the 2nd is a sub-molecule of the 1st

submolecule(M~_,M):-!.
submolecule(M~S,M~Sp):- subset(Sp,S),!.
submolecule(M-C,N):-(submolecule(M,N);submolecule(C,N)),!.
submolecule(M-N,Mp-Np):-submolecule(M,Mp), submolecule(N,Np),!.
submolecule(A,A).

% same but creates all sub_molecules
submolgen(A,_):-
   format_debug(5,"looking for sub mols of ~w~n",[A]),fail.

submolgen(A-B,V):-
   submolgen(A,V);
   submolgen(B,V).

submolgen(A~S1,A~S2):-
   !,subsetgen(S1,S2).

submolgen(A,A).

% creates all forms more phosphorylated
morephosgen(A~S,A~Sp):-
   !,
   phospho(A,H),
   member(Sp,H),
   subset(S,Sp).

morephosgen(A,A~Sp):-
   phospho(A,H),
   member(Sp,H).
   
% N and M (normalized) have a common submol
common_compound(M,N):-commoncompound(M,N);commoncompound(N,M),!.
commoncompound(M,M):-!.
commoncompound(M~_,M):-!.
commoncompound(M~_,M~_):-!.
commoncompound(M-C,N):-(commoncompound(M,N);commoncompound(C,N)),!.
commoncompound(M-N,Mp-Np):-commoncompound(M,Mp); commoncompound(N,Np),!.



% subset({S},{Sp}) on supose S et Sp tri� {S} include in {Sp} et les �l�ments sont s�parer par des virgules.

subset(S,Sp):-
	set_object_pattern_to_list(S,L),
	set_object_pattern_to_list(Sp,Lp),!,
	sublist(L,Lp),
	!.

% same in generation
subsetgen(S,Sp):-
   set_object_pattern_to_list(S,L),
   sublist(L,Lp),
   list_to_set(Lp,Sp).


find_vars_to_instantiate($(A)~ $(V), [$(A),$(V)]) :-
   (
      question_mark($(V),$A~(?))          % if it's a ? do nothing
   ;
      assertz(question_mark($(V),$(A)))   % else record variable dependance
   ),!.                                   % for a later check (when A ground)

find_vars_to_instantiate(A~ $(V), LV) :-
   !,get_molecule(A,B),
   (
      phospho(B,P)
   ->
      (
         constrvar($(V),L)
      -> 
	      (
            retractall(constrvar($(V),_)),
            assertz(constrvar($(V),[P|L])),
            LV=[]
         )
      ;
	      (
            assertz(constrvar($(V),[P])),
            LV=[$(V)]
         )
      )
   ;
      LV=[$(V)]
   ).

find_vars_to_instantiate($(V),[$(V)]) :-
   !.

find_vars_to_instantiate(T,L):-
   T=..[_|Args],
   find_vars_to_instantiate_rec(Args,[],L).

find_vars_to_instantiate_rec([],L,L).
find_vars_to_instantiate_rec([H|T],L1,L2):-
   find_vars_to_instantiate(H,L3),
   append(L1,L3,L4),
   sort(L4),
   find_vars_to_instantiate_rec(T,L4,L2).
   
rename_question_marks(R where D,RR where D) :-  % for optimization, do NOT
   !,rename_question_marks(R,RR).   % anonimize the ? in the def.

rename_question_marks(A~(?), A~($(New))):-
   new_atom('$$',New),
   assertz(question_mark($(New),A~(?))),!.

% FIXME: forbidden? or in all semantics?
rename_question_marks(?, $(New)):-  % A finir  coomentaire a virer
   new_atom('$$',New),
   
        (
                have_gui
        ->
                format("[GUI] warnings Attention ce ? risque d'engendrer une erreur~n",[])
        ;
                format("Attention ce ? risque d'engendrer une erreur~n",[])
        ),
   assertz(question_mark($(New),?)),
   !.

rename_question_marks(A,B) :-
   A=..[F|Args],
   rename_question_marks_rec(Args,L),
   B=..[F|L].

rename_question_marks_rec([],[]).
rename_question_marks_rec([H|T],[HH|TT]):-
   rename_question_marks(H,HH),
   rename_question_marks_rec(T,TT).

get_molecule(A~_,B):-
   !,get_molecule(A,B).

get_molecule(A,B):-
   !,parse_molecule(A,B).

find_constr(D1 and D):-
   find_constr(D1),
   find_constr(D).

find_constr($(V) in P) :-
   (
      (
         P = parts_of(Q),
         parse_parts(Q,L)
      )
   ;
      (
         P = {Q},
         parse_phosphoset(Q,L)
      )
   ),
   !,
   (
      constrvar($(V),C)
   -> 
      (
         retractall(constrvar($(V),_)),
         assertz(constrvar($(V),[L|C]))
      )
   ;
      assertz(constrvar($(V),[L]))
   ).

find_constr($(V) in P) :-
   (
      constrvar($(V),_)
   ->
      
        (
                have_gui
        ->
                format("[GUI] warnings Variable ~w used once as a phosphorylation sites and once as molecule.~n",[V])
        ;
      write_line_col('Warning'),
      write('Variable '),write($(V)),
      write(' used once as a phosphorylation sites and once as molecule.\n')
        )
   ;
      (
         (
            P=all;P=all_simple
         )
      ->
         true
      ;
         (
            P = {Q},
            find_vars_to_instantiate(Q,_) % add default constraints for vars 
         )                                % appearing in Q
      )
   ).

find_constr(A not in $(V)) :-
   parse_name(A),
   assertz(constrnotvar($(V),A)).

find_constr($(A) not in {P}) :-
	find_vars_to_instantiate(P,_),   % add default constraints for vars of P
	set_object_pattern_to_list({P},L),
	assert_list_diff($(A),L).

find_constr($(A) diff B) :-
	assert_list_diff($(A),[B]).


find_constr($(A) phos_form B):-
	is_object_pattern(B,Bn),
	(
      constrmodif(Bn,$(A))
   ->
      true
   ;
      assertset(constrmodif($(A),Bn))
   ).

find_constr($(A) not phos_form B):-
	is_object_pattern(B,Bn),
	(
      constrnotmodif(Bn,$(A))
   ->
      true
   ;
	   assertset(constrnotmodif($(A),Bn))
   ).


find_constr($(A) more_phos_than B):-
	is_object_pattern(B,Bn),
   assertset(constrmorephos($(A),Bn)).

find_constr($(A) not more_phos_than B):-
	is_object_pattern(B,Bn),
   assertset(constrnotmorephos($(A),Bn)).

find_constr($(A) has_simple_mol_in_common B):-
	is_object_pattern(B,Bn),
	(
      constrcommon(Bn,$(A))
   ->
      true
   ;
	   assertset(constrcommon($(A),Bn))
   ).

find_constr($(A) has_no_simple_mol_in_common B):-
	is_object_pattern(B,Bn),
	(
      constrnocommon(Bn,$(A))
   ->
      true
   ;
	   assertset(constrnocommon($(A),Bn))
   ).



find_constr($(A) sub_mol B):-
	is_object_pattern(B,Bn),
   assertset(constrsubmol($(A),Bn)).

find_constr($(A) not sub_mol B):-
	is_object_pattern(B,Bn),
   assertset(constrnotsubmol($(A),Bn)).

% treating local definitions

instantiate(D1 and D):-
   instantiate(D1),
   instantiate(D).

instantiate(($(V) in P)):-
   treat_local_patvar($(V) in P).

% treated in find_constr
instantiate(_ not in _).
instantiate(_ diff  _).
instantiate(_ phos_form _).
instantiate(_ not phos_form _). 
instantiate(_ more_phos_than _).
instantiate(_ not more_phos_than _). 
instantiate(_ sub_mol  _).
instantiate(_ not sub_mol _).
instantiate(_ has_simple_mol_in_common _).
instantiate(_ has_no_simple_mol_in_common  _).


% handle variables with a default definition

get_cv(L) :-
   findall(X-Y,(constrvar(X,P),merge_pat0(X,P,Z),remove_neg(X,Z,Y)),L).

% FIXME complains way too much
% (a~$P => b where $P in {{p1},{p1,p2}} and p2 not in $P)
%
% complain about singleton variables which are not '?' (i.e. $$xxx)
merge_pat0(X,P,Z):-
   (
      (
         P = [_],
         X = $(_),
         \+(question_mark(X,_))
      )
   -> 
      (
        
        (
                have_gui
        ->
                format("[GUI] warnings Named variable ~w used but appearing only once.~n",[X])
        ;
         write_line_col('Warning'),
         write('named variable '),write(X),
         write(' used but appearing only once.\n')
      )
      )
   ;
      true
   ),
   merge_pat(P,Z).

% intersect the allowed domains
merge_pat([A],A).
merge_pat([H|T],P):-
   merge_pat(T,P1),
   intersect(H,P1,P).

intersect([],_,[]).
intersect([H|T],L,M) :-
   (member(H,L) -> M=[H|N];M=N),
      intersect(T,L,N).

% remove the sets containing a forbidden phosphorylation
remove_neg(X,Z,Y) :-
   constrnotvar(X,P),!,
   remove_rec(P,Z,Z1),
   retract(constrnotvar(X,P)),
   remove_neg(X,Z1,Y).

remove_neg(_,Z,Z):-!.

remove_rec(_,[],[]):-!.

remove_rec(P,[{H}|T],L) :-
   remove_rec(P,T,L1),
   goal_to_list(H,HL),
   (member(P,HL) -> (L=L1);
      L=[{H}|L1]),!.

remove_rec(P,[{}|T],[{}|L]) :-
   remove_rec(P,T,L).

% instantiate constrained variables (w.r.t. their constraints)
instantiate_cv([]).
instantiate_cv([H-L|T]):-
   member(V,L),
   (locvar(H,_) -> 
      (
        (
                have_gui
        ->
                format("[GUI] warnings Variable ~w is already defined. Ignoring...~n",[H])
        ;
                write_line_col('Warning'),
         write('variable '),write(H),
                write(' already defined\n   ignoring...\n')
        )
      );
      assertb(locvar(H,V))),
   instantiate_cv(T).

assert_rule_with_def([],[[]],R,OR,V) :-
   !,
   format_debug(7, "asserting rule ~w as ~w for ~w with no def~n", [OR, V, R]),
   parse_rule(R,LL,LR,RR),
   format_debug(7, "LL:~w LR:~w~n", [LL, LR]),
   parse_the_rate_law(V,LL,LR,W),
   format_debug(7, "W:~w~n", [W]),
   assert_rule(RR,LL,LR,OR,W).


assert_rule_with_def(LV,[H|T],R,OR,V):-
   assert_values(LV,H),
   format_debug(2,"values asserted~n",[]),
   parse_rule(R,LL,LR,RR),    % has to be done now because otherwise variables make impossible the parsing
   parse_the_rate_law(V,LL,LR,W),   % rate law from sorted reactants ! Pb for MM and H
   format_debug(2,"rule parsed~n",[]),
   assert_rule(RR,LL,LR,OR,W),!,
   format_debug(2,"rule asserted~n",[]),
   retractall(locvar(_,_)),
   assert_rule_with_def(LV,T,R,OR,V).

assert_rule_with_def(LV,[_H|T],R,OR,V):-  % In case parse/assert failed
    assert_rule_with_def(LV,T,R,OR,V).
 
assert_rule_with_def(_LV,[],_R,_OR,_):-
   format_debug(3,"done~n",[]),
   retractall(locvar(_,_)).

assert_values([],[]).
assert_values([H|T],[V|L]):-
	assertb(locvar(H,V)),
	assert_values(T,L).

find_values([],[]).
find_values([H|T],[V|L]):-
   (
      locvar(H,V)
   ->
      (
         question_mark(H,$(A)),!,   % if H is another variable phospho
         locvar($(A),Va),           % check it!
         get_molecule(Va,Vb),
         (
            V\='{}'
         ->
            Vb\='_'                 % _ cannot be phosphorylated
         ;
            true
         ),
         check_declare(Vb,V)        % check if coherent with declarations
      ;
         true
      ),
      find_values(T,L)
   ;
      (
         question_mark(H,$(A)~(?))
      -> 
         (
            locvar($(A),Va),
            get_molecule(Va,Vb),
            phospho(Vb,P),
            member(V,P),
            assertb(locvar(H,V)),   % optim
            find_values(T,L)
         )
      ;
         (
            format_debug(4,"~w has no definition yet, trying to create one~n",
               [H]),
            findall(X,(
               get_value_from_positive_constraints(H,X),
               format_debug(4,"trying to generate value ~w for ~w~n",[X,H]),
               authorizedval(H,X)),P),
            format_debug(4,"Got ~w in ~w~n",[H,P]),
            (
               P = []
            ->
               
               (
                have_gui
                        ->
                                format("[GUI] errors Variable ~w used but not definied yet.~n",[H])
                        ;
               write_line_col('Error'),
                                write('variable '),write(H), write(' used but not defined (yet?).\n')
               ),
              
               fail
            ;
               member(V,P)
            )
         )
      )
   ).

% Top level
%%%%%%%%%%%

header:-
   write('BIOCHAM '),
   biocham_version(V),
   write(V),nl,
   write('Copyright (C) 2003-2011 INRIA, EPI Contraintes, Paris-Rocquencourt, France,'),nl,
   write('license GNU GPL 2, http://contraintes.inria.fr/biocham/'),nl.

:- dynamic(slave/0).

pre_biocham:-
   format_debug(1,"biocham top level start-up~n",[]),
   (
      environ('TMPDIR',TMP)
   ;
      environ('TEMP',TMP)
   ;
      environ('TMP',TMP)
   ;
      TMP='/tmp'
   ),!,
   os_version(OS),
   (
      atom_concat('CYGWIN',_,OS)
   ->
      (
         atom_concat('/cygdrive/',T1,TMP)
      ->
         % cygpath -w
         sub_atom(T1, 0, 1, _, Drive),
         sub_atom(T1, 1, _, 0, Path),
         atom_codes(Path, PL),
         replace_slash(PL, PL2),
         append(PL2, [92], PL3),
         atom_codes(Path2, PL3),
         atom_concat(Drive,':', D1),
         atom_concat(D1, Path2, Tmp)
      ;
         Tmp2=''
      )
   ;
      atom_concat(TMP, '/', Tmp)
   ),
   (
      catch(
         (
            file_property(Tmp, type(directory)),
            file_property(Tmp, permission(write))
         ),
         _,
         fail
      )
   ->
      Tmp2 = Tmp
   ;
      % Nothing works, trying local dir...
      Tmp2=''
   ),
	randomize,
   random(1,1000,N),
   number_atom(N,A),
   format_to_atom(B,'~w.gplot_~w',[Tmp2, A]),
   atom_concat(B,'.tmp.csv',Datafile),
   assertz(data_file(Datafile)),
   atom_concat(B,'.tmp.plot',Plotfile),
   assertz(plot_file(Plotfile)),
   format_to_atom(C,'~w.bc_~w.tmp.smv',[Tmp2, A]),
   assertz(smv_file(C)),
   format_to_atom(E,'~w.nusmv_out_err_~w.',[Tmp2, A]),
   assertz(smv_file_out_err(E)),
   format_to_atom(D,'~w.th_~w.tmp.bc',[Tmp2, A]),
   assertz(th_file(D)),
   dot_init_hook(Tmp2, A), 
   kinetics_init_hook,
   (
      environ('_', Executable),
      file_property(Executable, real_file_name(RealExec)),
      (
         atom_concat(ExecDir, '/biocham', RealExec)
      ;
         atom_concat(ExecDir, '/biocham.exe', RealExec)
      ;
         atom_concat(ExecDir, '/biocham', RealExec)
      )
   ->
      true
   ;
      file_property('.', real_file_name(ExecDir))
   ),
   g_assign('biocham_path', ExecDir),
   g_assign(prompt, 'biocham: '),
   argument_list(ArgList),    % Do it only now, in case some commands
   treat_args(ArgList),!,     % need to be executed
   (
      slave
   ->
      true
   ;
      header,
      biocham
   ).

% 47 = / 92 = \
%/

replace_slash([],[]).
replace_slash([47|L],[92|LL]):-
   !,replace_slash(L,LL).
replace_slash([X|L],[X|LL]):-
   replace_slash(L,LL).

:- dynamic(disable_prolog/0).

:- dynamic(png/0).

treat_args([]).
treat_args(['--debug',N|L]):-
   number_atom(NN,N),
   g_assign(debug,NN),
   treat_args(L).

treat_args(['--old_parser'|L]):-
   g_assign(g_old_parser,1),
   treat_args(L).

% Second arg is biocham running directory
treat_args(['--have_gui', Path|L]):-
   assertz(have_gui),
   file_property(Path, real_file_name(FullPath)),
   g_assign(biocham_path,FullPath),
   treat_args(L).

treat_args(['--version'|_]):-
   header,
   quit.

treat_args(['--lib' | L]):-
   g_assign(biocham_lib, 1),
   open_output_atom_stream(DummyStream),
   set_output(DummyStream),
   treat_args(L).

% treat_args(['--client', Goal]):-
%    client(Goal).

treat_args(['--png' | L]):-
   assertz(png),
   treat_args(L).

treat_args(['--slave' | L]):-
   assertz(slave),
   treat_args(L).

treat_args(['--disable-prolog' | L]):-
   assertz(disable_prolog),
   treat_args(L).

treat_args(['--error-position' | L]):-
   assertz(error_position),
   treat_args(L).

treat_args(['--prompt', Prompt | L]):-
   g_assign(prompt, Prompt),
   treat_args(L).
   
%treat_args(['--dump-commands' | L]):-
%   dump_commands,
%   treat_args(L).

treat_args([F|L]):-
   \+(sub_atom(F,0,1,_,'-')),
   add_biocham(F),
   treat_args(L).

treat_args(_):-
   write('Usage: biocham [BIOCHAM_FILE]...\n\n'),
   write(
   'BIOCHAM is a programming environment for modeling biochemical systems,\n'),
   write('making simulations, querying and constraining models in temporal logic.\n\n'),
   write('Report bugs to <biocham@inria.fr>\n'),
   quit.

%state_filename('state').
%
%save_state :-
%   state_filename(StateFilename),
%   open(StateFilename, write, StateFile),
%   \+ (
%      member(F/N, [rule/7, k_parameter/2, value/2, k_value/3, initconc/2, orule/1, molecule/2, k_macro/2, k_new_macro/1, trace_loaded/0, cmaes_init/0, cmaes_std/0, hasht/2,data/1]),
%      functor(P, F, N),
%      portray_clause(StateFile, retractall(P)),
%      call(P),
%      \+ (
%         portray_clause(StateFile, assertz(P))
%      )
%   ),
%   \+ (
%      member(X, [bound_penalty,bounds,cmaes,datatab,debug,debugcode,done,end,fish,formula,fss,fssbounds,fval,ilist_g_init,interval0,k,land,landsp1,lgvars,lmols,macrosl,macroslwithdef,maxevals,maxsimul,means,nbcurr,nbto,nbtotal,nbtrace,nf,nh,nk,nn,ns,ntime,obj0,objectives,paramlist,params0,plist_g_init,p_t_list,replace,rob,seed,size,spec0,specs,std,stopfitness,sumdist,sums,tdom,timesim0,timesimul,trace_times,varlist0,varlists,v_obj]),
%      g_read(X, V),
%      \+ (
%         portray_clause(StateFile, g_assign(X, V))
%      )
%   ),
%   close(StateFile).
%
%restore_state :-
%   state_filename(StateFilename),
%   open(StateFilename, read, StateFile),
%   (
%      repeat,
%      read(StateFile, Goal),
%      \+ (
%         Goal \= end_of_file,
%         call(Goal)
%      )
%   ->
%      close(StateFile)
%   ).
%
%client(GoalAtom) :-
%   restore_state,
%   read_from_atom(GoalAtom, Goal),
%   (
%      catch(
%         call(Goal),
%         E,
%         (
%            format(user_error, 'Uncaught exception: ~q.\n', [E]),
%            nl(user_error),
%            fail
%         )
%      )
%   ->
%      save_state,
%      halt(0)
%   ;
%      halt(1)
%   ).

biocham:-
   handle_ctrl_c,
   '$sys_var_inc'(10),
   repeat,
   '$get_current_B'(B),
   '$sys_var_write'(11, B),
% write('Attention a l utilisation de controle C !'),nl,
%        (nusmv_running -> nusmv_close; true),
   biocham2.

% if lib mode, do nothing
biocham2 :-
   g_read(biocham_lib, 1),
   !.

biocham2:-
   %g_assign(allsolutions,false),
   %g_assign(success,false),
   %g_assign(absent,false),
   g_assign(line,0),
   g_read(prompt, Prompt),
   write(Prompt),%(write('biocham: '); g_read(success,false),nl,write('No'),nl,fail),
   (
      have_gui
   ->
      nl
   ;
      true
   ),
   flush_output,
   (
      catch(biocham_top,C,(write_error(C),nl))
   ;
      nl
   ),
	fail.

biocham2:-biocham2.	

biocham_top:-
	biocham_read(A),
	(A=end_of_file -> quit; true),
      %'$get_current_B'(B),
	call(A).%,
   %'$get_current_B'(B1),
   %nl,
   %(g_read(success,false) -> write('Yes '), nl, g_assign(success,true) ; true),
	%write(A),
   %(g_read(allsolutions,true) -> nl, fail ; (B1>B -> biocham_prompt,!; nl, fail)).

%biocham_prompt:-
%	write(' ? '),
%	next_char(C),
%	(C='h' -> write(' ; for next solution, a for all solutions, h for help'), biocham_prompt;
%	    (char_code(C,59) -> nl, fail ;%(C=';' -> nl, fail ;
%		(C='a' -> g_assign(allsolutions,true), nl, fail ; nl))).

%next_char(C):-
%	get_key(T),
%	char_code(C,T).

% backquote for calling Prolog from Biocham
prolog(A) :-
   (
      disable_prolog
   ->
      write('Disabled command: prolog.'),
      nl
   ;
      atom_concat(A,'.',B),
      read_term_from_atom(B,G,[variable_names(L)]),
      write('? '), write(G),nl,
      call(G),nl,
      write('success '),write(L),
      ( get_code(59) -> get_code(_),fail; !)
   ).

% Rules
%%%%%%%

load_biocham(F):-
	clear_biocham,
   add_biocham(F),!.

clear_biocham:-
   clear_rules,
   clear_initial_state,
   clear_spec, % FF
   retractall(mols_added),
   retractall(rules_added),
   show_molecules('?'),
   delete_events.

add_biocham(FF):-
   write_to_atom(F, FF),
   statistics(user_time, _),
   working_directory(OldDir),
   format_debug(5, "cwd: ~w~n", [OldDir]),
   decompose_file_name(F, Dir, FFF, Ext),
   change_directory(Dir),
   format_debug(5, "cd: ~w~n", [Dir]),
   (
      Ext = ''
   ->
      EExt = '.bc'
   ;
      EExt = Ext
   ),
   atom_concat(FFF, EExt, G),
   format_debug(5, "opening ~w~n", [G]),
   catch(
      open(G,read,BC),
      Error,
      (
         % restore working directory
         change_directory(OldDir),
         format_debug(5, "cd: ~w~n", [OldDir]),
         % errors caught at top level
         throw(Error)
      )
   ),
   % removed alias because several streams might be open at the same time...
   format_debug(5, "done opening ~w~n", [FF]),
   (
      have_gui
   ->
      format("[GUI] file ~w~n",[FF])
   ;
      true
   ),
   g_assign(warning_cnt,0),
   g_read(current_bc_stream, CurrentBCStream),
   g_read(current_bc_file, CurrentBCFile),
   g_assign(current_bc_stream, BC),
   g_assign(current_bc_file, G),
   bc_read(BC),
   close(BC),
   g_assign(current_bc_stream, CurrentBCStream),
   g_assign(current_bc_file, CurrentBCFile),
   % restore working directory
   change_directory(OldDir),
   format_debug(5, "cd: ~w~n", [OldDir]),
   statistics(user_time,[_,T]),
   format_debug(3,"Time: ~w~n",[T]),
   assertset(mols_added),
   assertset(declare_added),
   assertset(rules_added),
   g_read(warning_cnt,W),
   (
      W>0
   ->
      (
         have_gui
      ->
         format("[GUI] warnings There were ~w warning(s)/errors due to loading/adding to a model...~n",[W])
      ;
         format("** There were ~w warning(s)/error(s) **~n",[W])
      )
   ;
      true
   ).


clear_rules:-
	retractall(rule(_,_,_,_,_,_,_)),
	retractall(orule(_)),
	retractall(phospho(_,_)),
	assertset(mols_added),
	assertset(declare_added),
	assertset(rules_added),
	(
         have_gui
      ->
         format("[GUI] clearRules~n",[])
      ;
	 true
      ).

add_rule(A):-
        add_rules(A).

add_rules({R,L}):-!,add_rules(R),add_rules({L}).
add_rules({R}):-!,add_rules(R).

% reaction_shortcuts
add_rules('elementary_interaction_rules'):-!,
	search_all_molecules,
	assertz(no_board_effect),

	add_rules({'synthesis','degradation','de_complexation','de_phosphorylation'}),

	retract(no_board_effect),!.
add_rules('other_elementary_interaction_rules'):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({'synthesis','degradation','de_complexation', 're_complexation_dephosphorylation','re_complexation_phosphorylation'}),


         add_rules({'de_complexation', 'de_phosphorylation'}),


	retract(no_board_effect),!.
add_rules('elementary_interaction_rules_more'):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({'elementary_interaction_rules','re_complexation_dephosphorylation','re_complexation_phosphorylation'}),
	retract(no_board_effect),!.
add_rules('more_elementary_interaction_rules'):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({'elementary_interaction_rules','re_complexation_dephosphorylation_cat','re_complexation_phosphorylation_cat'}),
	retract(no_board_effect),!.
add_rules('complexation'):-!,
%	add_rules($A + $B => $A-$B  where $A in all and $B in all and $A diff $B).
	add_rules('$'('A') + '$'('B') => '$'('A')-'$'('B') where '$'('A') in all and  '$'('B') in all and '$'('B') diff '$'('A') and '$'('B') diff '_' and '$'('A') diff '_').
add_rules('decomplexation'):-!,
%	add_rules($A-$B => $A+$B  where $A in all and $B in all and $A diff $B).
	add_rules('$'('A')-'$'('B') => '$'('A') + '$'('B') where '$'('A') in all and  '$'('B') in all and '$'('B') diff '$'('A') and '$'('B') diff '_' and '$'('A') diff '_').
add_rules('de_complexation'):-!,
   add_rules({decomplexation,complexation}).

add_rules('re_complexation'):-!,
	add_rules('$'('A') + '$'('B') <=> '$'('A')-'$'('B') where '$'('A') in all and  '$'('B') in all and '$'('B') diff '$'('A') and '$'('B') diff '_' and '$'('A') diff '_').

add_rules('phosphorylation'):-!,
% $A  =[$C]=> $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $B diff $A
%	add_rules('$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A')).
	add_rules({'$'('A')  =>  '$'('B') where '$'('A') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A'),
	'$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A') and '$'('C') diff '_'}).
add_rules('dephosphorylation'):-!,
% $A  =[$C]=> $B where $B in all and $A  in all and $C in all and $A more_phos_than $B  and $A diff $B
	add_rules({
   '$'('A')  =>  '$'('B') where '$'('B') in all and '$'('A') in all and
      '$'('A') more_phos_than '$'('B') and '$'('A') diff '$'('B'),
   '$'('A')  =['$'('C')]=>  '$'('B') where '$'('C') in all and  '$'('B') in
      all and '$'('A') in all and '$'('A') more_phos_than '$'('B') and '$'('A')
      diff '$'('B') and '$'('C') diff '_'}).
add_rules('de_phosphorylation'):-!,
   add_rules({dephosphorylation,phosphorylation}).

add_rules('re_phosphorylation'):-!,
		add_rules({'$'('A') <=>  '$'('B') where '$'('A') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A'),
		'$'('A') <=['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A') and '$'('C') diff '_'}).

add_rules('synthesis'):-!,
	%_=[$G]=>$A where $A in all_simple and $G in all and $A diff $G
%		add_rules('_' =['$'('G')]=>  '$'('A') where '$'('A') in all_simple and '$'('G') in all and '$'('G') diff '$'('A') and '$'('A') diff '_').
		add_rules({'_' =>  '$'('A') where '$'('A') in all_simple and '$'('A') diff '_',
		'_' =['$'('G')]=>  '$'('A') where '$'('A') in all_simple and '$'('G') in all and '$'('G') diff '$'('A') and '$'('A') diff '_' and '$'('G') diff '_'}).
add_rules('degradation'):-!,
	%$A =[$D]=>_ where $A in all and $D in all and $A diff $D
%	add_rules('$'('A') =['$'('D')]=> '_' where '$'('A') in all and '$'('D') in all and '$'('D') diff '$'('A') and  '$'('A') diff '_').
	add_rules({'$'('A') => '_' where '$'('A') in all and '$'('A') diff '_',
	'$'('A') =['$'('D')]=> '_' where '$'('A') in all and '$'('D') in all and '$'('D') diff '$'('A') and  '$'('A') diff '_' and  '$'('D') diff '_'}).

add_rules('decomplexation_dephosphorylation'):-!,
	%$A-$C => $C+ $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B')-'$'('C') =>  '$'('A') + '$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).
	
add_rules('decomplexation_phosphorylation'):-!,
	%$A-$C => $C+ $B where $A in all and $B  in all and $C in all and $A more_phos_than $B and $A diff $B
	add_rules('$'('A')-'$'('C') =>  '$'('B') + '$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('complexation_dephosphorylation'):-!,
	%$A-$C => $C+ $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B') + '$'('C') =>  '$'('A')-'$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('complexation_de_phosphorylation'):-!,
	%$A-$C => $C+ $B where $A in all and $B  in all and $C in all and $B phos_form $A and $A diff $B
	add_rules('$'('B') + '$'('C') =>  '$'('A')-'$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('decomplexation_de_phosphorylation'):-!,
	%$A + $C => $C-$B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B')-'$'('C') =>  '$'('A') + '$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')). % and '$'('B') diff '$'('A')).


add_rules('re_complexation_de_phosphorylation'):-!,
	%$A-$C <=> $C+ $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B')-'$'('C') <=>  '$'('A') + '$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')). % and '$'('B') diff '$'('A')).



add_rules('complexation_phosphorylation'):-!,
	%$A-$C => $C+ $B where $A in all and $B  in all and $C in all and $A more_phos_than $B and $A diff $B
	add_rules('$'('A') + '$'('C') =>  '$'('B')-'$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('re_complexation_dephosphorylation'):-!,
	%$A-$C <=> $C+ $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B') + '$'('C') <=>  '$'('A')-'$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).
	
add_rules('re_complexation_phosphorylation'):-!,
	%$A-$C <=> $C+ $B where $A in all and $B  in all and $C in all and $A more_phos_than $B and $A diff $B
	add_rules('$'('A') + '$'('C') <=>  '$'('B')-'$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).








add_rules('decomplexation_dephosphorylation_cat'):-!,
	%$A-$C =[$D]=> $C+ $B where D in all and $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B')-'$'('C') =['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in all and  '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).
	
add_rules('decomplexation_phosphorylation_cat'):-!,
	%$A-$C =[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $A more_phos_than $B and $A diff $B
	add_rules('$'('A')-'$'('C') =['$'('D')]=>  '$'('B') + '$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('complexation_dephosphorylation_cat'):-!,
	%$A-$C =[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B') + '$'('C') =['$'('D')]=>  '$'('A')-'$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('complexation_de_phosphorylation_cat'):-!,
	%$A-$C =[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $B phos_form $A and $A diff $B
	add_rules('$'('B') + '$'('C') =['$'('D')]=>  '$'('A')-'$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')). % and '$'('B') diff '$'('A')).

add_rules('decomplexation_de_phosphorylation_cat'):-!,
	%$A + $C =[$D]=> $C-$B where $D in all and  $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B')-'$'('C') =['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')). % and '$'('B') diff '$'('A')).


add_rules('re_complexation_de_phosphorylation_cat'):-!,
	%$A-$C <=[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B')-'$'('C') <=['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')). % and '$'('B') diff '$'('A')).



add_rules('complexation_phosphorylation_cat'):-!,
	%$A-$C =[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $A more_phos_than $B and $A diff $B
	add_rules('$'('A') + '$'('C') =['$'('D')]=>  '$'('B')-'$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A')).

add_rules('re_complexation_dephosphorylation_cat'):-!,
	%$A-$C <=[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $B more_phos_than $A and $A diff $B
	add_rules('$'('B') + '$'('C') <=['$'('D')]=>  '$'('A')-'$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A')).
	
add_rules('re_complexation_phosphorylation_cat'):-!,
	%$A-$C <=[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $A more_phos_than $B and $A diff $B
	add_rules('$'('A') + '$'('C') <=['$'('D')]=>  '$'('B')-'$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A')).



% reaction_shortcuts for a molecule
add_rules(elementary_interaction_rules(New)):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({de_complexation(New),de_phosphorylation(New),synthesis(New),degradation(New)}),
	retract(no_board_effect),!.
add_rules(other_elementary_interaction_rules(New)):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({de_complexation(New), re_complexation_de_phosphorylation(New),synthesis(New),degradation(New)}),
	retract(no_board_effect),!.
add_rules(elementary_interaction_rules_more(New)):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({elementary_interaction_rules(New),re_complexation_de_phosphorylation(New)}),
	retract(no_board_effect),!.
add_rules(more_elementary_interaction_rules(New)):-!,
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({elementary_interaction_rules(New),re_complexation_de_phosphorylation_cat(New)}),
	retract(no_board_effect),!.
add_rules(complexation(New)):-!,
%	add_rules($A + $B => $A-$B  where $A in all and $B in all and $A diff $B).
	add_rules('$'('A') + '$'('B') => '$'('A')-'$'('B') where '$'('A') in {New} and  '$'('B') in all and '$'('B') diff '$'('A') and '$'('B') diff '_' and '$'('A') diff '_').
add_rules(decomplexation(New)):-!,
%	add_rules($A-$B => $A+$B  where $A in all and $B in all and $A diff $B).
	add_rules('$'('A')-'$'('B') => '$'('A') + '$'('B') where '$'('A') in {New} and  '$'('B') in all and '$'('B') diff '$'('A') and '$'('B') diff '_' and '$'('A') diff '_').
add_rules(de_complexation(New)):-!,
   add_rules({decomplexation(New),complexation(New)}).
add_rules(re_complexation(New)):-!,
	add_rules('$'('A')-'$'('B') <=> '$'('A') + '$'('B') where '$'('A') in {New} and  '$'('B') in all and '$'('B') diff '$'('A') and '$'('B') diff '_' and '$'('A') diff '_').

add_rules(phosphorylation(New)):-!,
% $A  =[$C]=> $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $B diff $A
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in {New} and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A'),
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in {New} and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A'),
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in all and  '$'('B') in {New} and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A')
		  }),
	retract(no_board_effect),!.
add_rules(dephosphorylation(New)):-!,
% $A  =[$C]=> $B where $B in all and $A  in all and $C in all and $A more_phos_than $B  and $A diff $B
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('C') in {New} and  '$'('B') in all and '$'('A') in all and '$'('A') more_phos_than '$'('B') and '$'('A') diff '$'('B'),
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('C') in all and  '$'('B') in {New} and '$'('A') in all and '$'('A') more_phos_than '$'('B') and '$'('A') diff '$'('B'),
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('C') in all and  '$'('B') in all and '$'('A') in {New}  and '$'('A') more_phos_than '$'('B') and '$'('A') diff '$'('B')
		  }),
	retract(no_board_effect),!.
add_rules(de_phosphorylation(New)):-!,
%	$A  =[$C]=> $B where $A in all and $B  in all and $C in all and $B phos_form $A  and $A diff $B
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in {New} and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A')   and '$'('B') diff '$'('A'), %FIXME
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in {New} and  '$'('B') in all and '$'('B') phos_form '$'('A')  and '$'('B') diff '$'('A'), %FIXME
		  '$'('A')  =['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in all and  '$'('B') in {New} and '$'('B') phos_form '$'('A')  and '$'('B') diff '$'('A') %FIXME
		  }),
	retract(no_board_effect),!.
add_rules(re_phosphorylation(New)):-!,
% $A  =[$C]=> $B where $A in all and $B  in all and $C in all and $B more_phos_than $A and $B diff $A
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({
		  '$'('A')  <=['$'('C')]=>  '$'('B') where '$'('A') in {New} and '$'('C') in all and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A'),
		  '$'('A')  <=['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in {New} and  '$'('B') in all and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A'),
		  '$'('A')  <=['$'('C')]=>  '$'('B') where '$'('A') in all and '$'('C') in all and  '$'('B') in {New} and '$'('B') more_phos_than '$'('A') and '$'('B') diff '$'('A')
		  }),
	retract(no_board_effect),!.

add_rules(synthesis(New)):-!,
	%_=[$G]=>$A where $A in all_simple and $G in all and $A diff $G
	search_all_molecules,
	assertz(no_board_effect),
		add_rules({'_' =['$'('G')]=>  '$'('A') where '$'('A') in {New} and '$'('G') in all and '$'('G') diff '$'('A') and '$'('A') diff '_',
			  '_' =['$'('G')]=>  '$'('A') where '$'('A') in all_simple and '$'('G') in {New} and '$'('G') diff '$'('A') and '$'('A') diff '_'}),
	retract(no_board_effect),!.
add_rules(degradation(New)):-!,
	%$A =[$D]=>_ where $A in all and $D in all and $A diff $D
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({'$'('A') =['$'('D')]=> '_' where '$'('A') in {New} and '$'('D') in all and '$'('D') diff '$'('A') and  '$'('A') diff '_',
		 '$'('A') =['$'('D')]=> '_' where '$'('A') in all and '$'('D') in {New} and '$'('D') diff '$'('A') and  '$'('A') diff '_'}),
	retract(no_board_effect),!.
 



add_rules(re_complexation_de_phosphorylation(New)):-!,
	%$A-$C <=> $C+ $B where $A in all and $B  in all and $C in all and $B phos_form $A and $A diff $B
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({'$'('B')-'$'('C') <=>  '$'('A') + '$'('C') where '$'('A') in {New} and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A') and '$'('B') diff '$'('A'),
		 '$'('B')-'$'('C') <=>  '$'('A') + '$'('C') where '$'('A') in all and '$'('C') in {New} and  '$'('B') in all and '$'('B') phos_form '$'('A') and '$'('B') diff '$'('A'),
		 '$'('B')-'$'('C') <=>  '$'('A') + '$'('C') where '$'('A') in all and '$'('C') in all and  '$'('B') in {New} and '$'('B') phos_form '$'('A') and '$'('B') diff '$'('A')}),
	retract(no_board_effect),!.



add_rules(re_complexation_de_phosphorylation_cat(New)):-!,
	%$A-$C <=[$D]=> $C+ $B where $D in all and  $A in all and $B  in all and $C in all and $B phos_form $A and $A diff $B
	search_all_molecules,
	assertz(no_board_effect),
	add_rules({
		  '$'('B')-'$'('C') <=['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in {New} and '$'('A') in all and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A'), % and '$'('B') diff '$'('A'),
		  '$'('B')-'$'('C') <=['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in all and '$'('A') in {New} and '$'('C') in all and  '$'('B') in all and '$'('B') phos_form '$'('A'), % and '$'('B') diff '$'('A'),
		  '$'('B')-'$'('C') <=['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in {New} and  '$'('B') in all and '$'('B') phos_form '$'('A'), % and '$'('B') diff '$'('A'),
		  '$'('B')-'$'('C') <=['$'('D')]=>  '$'('A') + '$'('C') where '$'('D') in all and '$'('A') in all and '$'('C') in all and  '$'('B') in {New} and '$'('B') phos_form '$'('A') % and '$'('B') diff '$'('A')
		 }),
	retract(no_board_effect),!.


%the real add_rule
add_rules(R):-
	retractall(question_mark(_,_)),
	rename_question_marks(R,RR),
	treat_rule_with_def(RR,R),
	(silence_mode -> true ;(write(R),nl)),
	assertset(mols_added),
	assertset(declare_added), % pas r�ellement mais comme lors du prochain search_molecules toutes les molecule/2 seront enlev�es, les mols que d�clar�e ne seraient plus prises en compte
	assertset(rules_added),
	!.


add_rules(A):-!,
   write_error('Error: add rule failed '(A)),
   fail.                           

delete_rules({A,L}):-!,delete_rules(A), delete_rules({L}).
delete_rules({A}):-!,delete_rules(A).
delete_rules(_ for A):-
   !,delete_rules(A).
delete_rules(A):-
	pattern_rule(A,X,LA,Y,RA),
	rule(_,LL,LR,ER,LB,RB,V),
	match_list2(X,LA,LB),
	match_list2(Y,RA,RB),
	(
  	   have_gui
	->
    	   format("[GUI] delete_rule ~w~n",[ER])
        ;
	   silence_mode
	->
           true
        ;
           write(ER),nl
   	),
	rule(OR,LL,LR,ER,LB,RB,V),
	(
	  learning_mode 
	->
	  assertset(lrule(OR,LL,LR,ER,LB,RB,V))
	;
	  (
	    retractall(rule(OR,LL,LR,ER,LB,RB,V)),
	    clean_orule(OR),
	    assertset(mols_added),
	    assertset(declare_added),
	    assertset(rules_added)
	  )
	),
	fail.
delete_rules(_).

clean_orule(R):-
   retract(rule(R,LL,LR,ER,LB,RB,V)),
   assertz(rule(ER,LL,LR,ER,LB,RB,V)),
   assertset(orule(ER)), % sinon list_rules(ER) n'affiche rien.
   fail.

clean_orule(R):-
   retractall(orule(R)).

% Listing rules and molecules
expand_rules:-
    g_assign(num_R,0),
    writeall_num(
      (rule(_,_,_,R,_,_,RL),
       (
         (RL=0)
       ->
         (A=R)
       ;
         (A=for(RL,R))
       )
      ),A).

expand_rules_clean(S):-
   writeall(S,
      (
         rule(_,_,_,R,_,_,RL),
         (
            (RL=0)
         ->
            (A=R)
         ;
            (A=for(RL,R))
         )
      ),A
   ).


writeall_num(P,A):-
   call(P),
   g_read(num_R,Old),
   New is Old +1,
   g_assign(num_R,New),
   %g_inc(num_R, New),
   write(New),write(' '),
   write_term(A,[quoted(false)]),write(.),
   nl,
   fail.
writeall_num(_,_).

pathway([]).
pathway([A|L]):-
	write(A), write(' '),
	rule(A),
	pathway(L).

rule(N):-
   integer(N),!,
   g_assign(num_R,0),
   rule_num(N).

rule(Name):-
   orule((Name:R)),
   write((Name:R)),nl,
   fail.

rule(_).

% Rn is the rule number N
rule(N,Rn):-
	g_assign(num_R,0),
	rule(OR,LL,LR,A,LB,RB,V),
	g_read(num_R,Old),
	New is Old +1,
	g_assign(num_R,New),
	N = New,
	Rn=rule(OR,LL,LR,A,LB,RB,V),
	!.

rule_num(N):-
   call(rule(_,_,_,A,_,_,_)),
	g_read(num_R,Old),
	New is Old +1,
	g_assign(num_R,New),
	N = New,!,
   %g_inc(num_R, N),!,
   %write(N), write(' '),
   write_term(A,[quoted(false)]),write(.),
   nl.

list_rules:-
	write_rules(user_output).

write_rules(F):-
    findall(X,rule(X,_,_,_,_,_,_),L),
    %sort(L),
    remove_dup(L,M),
    writeall(F,member(R,M),R).

% Remove duplicates in a sorted list
remove_dup([],[]).
remove_dup([A],[A]).
remove_dup([A,A|L],M):-
   !,
   remove_dup([A|L],M).
remove_dup([A,B|L],[A|M]):-
   remove_dup([B|L],M).

writeall(A):-
   writeall(A,A).

writeall(P,A) :-
   writeall(user_output,P,A).

writeall(F,P,A):-
	call(P),
	write_term(F,A,[quoted(false), portrayed(true)]),write(F,'.'),
	nl(F),
	fail.
writeall(_,_,_).

%Anthony, writeall_fss
writeall_fss(P,A) :-
   writeall_fss(user_output,P,A).

writeall_fss(F,P,A):-
        call(P),        
        format(F,"[GUI] checkLTL ~w ~n",[A]),
        fail.
writeall_fss(_,_,_).

%Dragana, writeall_cmaes
writeall_cmaes(P,A) :-
   writeall_cmaes(user_output,P,A).

writeall_cmaes(F,P,A):-
        call(P),        
        format(F,"[GUI] checkLTL ~w ~n",[A]),
        fail.
writeall_cmaes(_,_,_).

%Dragana, writeall_multiConditions
writeall_multiCond(P,A) :-
   writeall_multiCond(user_output,P,A).

writeall_multiCond(F,P,A):-
        call(P),
        format(F,"[GUI] checkLTL ~w ~n",[A]),
        fail.
writeall_multiCond(_,_,_).

%Dragana, writeall_pInvariants
writeall_pInvariants(P,A) :-
   writeall_pInvariants(user_output,P,A).

writeall_pInvariants(F,P,A):-
        call(P),        
        format(F,"[GUI] conservationLaws P-Invariants: ~w ~n",[A]),
        fail.
writeall_pInvariants(_,_,_).

%Dragana, writeall_rule
writeall_rule(P,A) :-
   writeall_rule(user_output,P,A).

writeall_rule(F,P,A):-
        call(P),        
        format(F,"[GUI] checkCTL ~w ~n",[A]),
        fail.
writeall_rule(_,_,_).

%Dragana, writeall_trace
writeall_trace(P,A) :-
   writeall_trace(user_output,P,A).

writeall_trace(F,P,A):-
        call(P),        
        format(F,"[GUI] checkLTL ~w ~n",[A]),
	format(F,"[GUI] addParam ~w ~n",[A]),
        fail.
writeall_trace(_,_,_).


list_molecules:-
   search_molecules,
   (
      have_gui
   ->
      writeall(
         (
            molecule(A,_),
            format_to_atom(Atom,"[GUI] molecule ~w",[A])
         ),
         Atom
      )
   ;
      writeall(molecule(A,_),A)
   ).

search_molecules:-
   molecule(_,_),
   \+(mols_added),!.

search_molecules:-
	retractall(molecule(_,_)),
	assertset(declare_added),assertset(initconc_added),
	rule(_,LL,LR,_,_,_,_),
	add_molecules(LL),
	add_molecules(LR),
	fail.
search_molecules:-
   retractall(mols_added).


list_all_molecules:-
	search_all_molecules,
	writeall(molecule(A,_),A).

search_all_molecules:-
	no_board_effect, % pour ne pas prendre en compte les nouvelles mol�cules ajout�esdans add_rules(elementary_interaction_pattern)...
	!.
	
search_all_molecules:-
	molecule(_,_),
	\+(declare_added),!.  % �vite qu'on recalcule toutes les mols
% la non prise en compte de mols_added est intentionnel car la seule fois o� elle est ajoutee sans que declare_added soit ajout� est dans search_all_molecules

search_all_molecules:-
	search_molecules,
	phospho(A,LP),  
	assertset(mols_added), % afin que les molecules prises en compte en plus ici, ne le soient pas par un simple seach_molecules.
	member(P,LP),
	(P={} -> (add_molecules([(_,A)]));
	    (add_molecules([(_,A~(P))]))),
	fail.

search_all_molecules:-
	search_initconc_molecules,
	retractall(declare_added).

% ajoute les molecules d�finies dans l'�tat initial 
search_initconc_molecules:-
	(initconc_added ->
	    (
	      initconc(A,_),
	      assertset(mols_added), % afin que les molecules prises en compte en plus ici, ne le soient pas par un simple seach_molecules.
	      add_molecules([(_,A)])
	    );true
	),fail.
search_initconc_molecules:-
	retractall(initconc_added),!.


add_molecules([]).
add_molecules([(_,A)|L]):-
	pattern_molecule(A,B),
	assertset(molecule(A,B)),
	add_molecules(L).

% Checks lower and upper cases in molecules
% then checks for production and degradation rules

check_molecules:-
	search_molecules,
	retractall(check_mol(_,_)),
	molecule(M,_),
	write_term_to_atom(A,M,[quoted(false)]),
	upper(A,B),
	(
      check_mol(B,C)
	->
      (
         A=C
      ->
         true
      ;
         (
            have_gui
         ->
            write_line_col('[GUI] checkMolecule'),
            format("[GUI] warnings Molecules ~w and ~w differ by uppeer/lower cases.~n",[C,A])
         ;
            true
         ),
         write_line_col('Warning'),
         write('molecules '),write(C),write(' and '),write(A),
         write(' differ by upper/lower cases'),nl,
         assertz(check_mol(B,A))
      )
	;
      assertz(check_mol(B,A))
   ),
   (
      rule(_,LL,RL,_,_,_,_),
      memberchk((_,M),RL),
      \+(memberchk((_,M),LL))
   ->
      true
   ;
      (
         have_gui
      ->
         write_line_col('[GUI] checkMolecule')
      ;
         true
      ),
		write_line_col('Warning'),
      format("molecule ~w has no production rule.~n",[M])
   ),
   (
      rule(_,LL2,RL2,_,_,_,_),
      memberchk((_,M),LL2),
      \+(memberchk((_,M),RL2))
   ->
      true
   ;
      (
         have_gui
      ->
         write_line_col('[GUI] checkMolecule')
      ;
         true
      ),
		write_line_col('Warning'),
      format("molecule ~w has no degradation rule.~n",[M])
   ),
	fail.

check_molecules.

upper(A,B):-
	atom_chars(A,LA),
	map_upper(LA,LB),
	atom_chars(B,LB).

map_upper([],[]).
map_upper([C|L],[D|R]):-
	lower_upper(C,D),
	map_upper(L,R).

% Makes gene forms absent if not explicitely present

complete_init :-
   molecule(A,_),
   uniq(A),                   % GENE or CTRL_VAR
   \+(A= @(_)),               % only GENE forms
   \+(init_present(A)),
   initconc_inplace(A,0),
   fail.

complete_init.

% checks the unicity of gene forms
% (warning if all absent, or two or more present)

check_init :-
   retractall(gene_list(_)),
   retractall(gene_done(_)),
   nusmv_search_molecules,
   molecule(A,_),             % get present before absent
   get_gene(A,G),
   \+(gene_done(G)),
   (init_present(A) ->
      (gene_list(G) ->
         (
        (
                have_gui
        ->
                format("[GUI] warnings Gene ~w is present in more than one compound of the initial state.~n",[G])
        ;
                write_line_col('Warning'),write('gene '),write(G),
                write(' is present in more than one compound of the initial state\n')
        ),
          assertz(gene_done(G)));
         assertz(gene_list(G)));
   (gene_list(G) -> true;
      (
        (
                have_gui
        ->
                format("[GUI] warnings No compound of gene ~w is present in the initial state.~n",[G])
        ;
                write_line_col('Warning'),write('no compound of gene '),write(G),
                write(' is present in the initial state\n')
        ),
       assertz(gene_done(G))))),
   fail.
       
check_init.
   
% check if rules don't use more than one gene

check_rule(R,LL,LR) :-
   get_gene_list(LL,LGL),
   (LGL = [G1,G2|_] ->
      (
        (
                have_gui
        ->
                format("[GUI] warnings Two genes (# ~w and # ~w) are presenet in the left side of rule.~n",[G1,G2])
        ;
                write_line_col('Warning'),
       write('two genes (#'),write(G1),write(' and #'),write(G2),
       write(') are present in the left side of rule'),nl,
                write('   '),write(R),nl
        )
      );true),
   get_gene_list(LR,RGL),   
   (RGL = [G3,G4|_] ->
      (
        (
                have_gui
        ->
                format("[GUI] warnings Two genes (# ~w and # ~w) are presenet in the right side of rule.~n",[G3,G4])
        ;
                write_line_col('Warning'),
       write('two genes (#'),write(G3),write(' and #'),write(G4),
       write(') are present in the right side of rule'),nl,
                write('   '),write(R),nl
        )
       );true),
   (sublist(LGL,RGL) ->
      true;
      (
        (
                have_gui
        ->
                format("[GUI] warnings There are genes in left side of rule ~w which are not in the right side.~n",[R])
        ;
                write_line_col('Warning'),
       write('there are genes in left side of rule '),write(R),nl,
                write('   which are not in the right side.'),nl
        )
      )),
   (sublist(RGL,LGL) ->
      true;
      (
(
                have_gui
        ->
                format("[GUI] warnings There are genes in right side of rule ~w which are not in the left side.~n",[R])
        ;
                write_line_col('Warning'),
       write('there are genes in right side of rule '),write(R),nl,
                write('   which are not in the left side.'),nl
        )
      )).

get_gene_list([],[]).

get_gene_list([(_,H)|T],LG) :-
   get_gene_list(T,LGG),
   (get_gene(H,G) -> LG = [G|LGG];LG = LGG).
   
% Searching molecules with a pattern
				% the pattern is first normalized with sets for complexes and phosphorylation sites.

list_molecules({A,B}):-!,
   list_molecules(A),
   list_molecules({B}).

list_molecules({A}):-!,
   list_molecules(A).

list_molecules(A):-
	find_molecules(A,B),
	write(B),nl,
	fail.

list_molecules(_).

find_molecules(A,B):-
   \+(sub_term(A,(?))),
   !,
   parse_object(A,B).

find_molecules(A,B):-
	search_molecules,
	pattern_molecule(A,P),
	molecule(B,Q),
	match_molecule(P,Q).

% ATTENTION, pas optimise(calcul search_mol meme si y a pas de ? dans un pattern), mais ne donne pas d'effet de bord (comme des mol�cules non utilis�es mais d�clar�es qui apparaitrait quand on ferait un simple list_mol apr�s.
list_all_molecules(A):-
	search_all_molecules,
	retractall(mols_added),  % pour courcicuiter le search_molecules
	list_molecules(A),
	assertset(mols_added).

%%% ne sert pas encore pour le ?
set_molecules(A,S):-
	setof(B, find_molecules(A,B),L),
	list_to_set(L,S).

sub_term(A::B,T):-
   sub_term(A,T),!;
   sub_term(B,T).
sub_term(A-B,T):-
   sub_term(A,T),!;
   sub_term(B,T).
sub_term(A~B,T):-
   sub_term(A,T),!;
   B == T.
sub_term(T,T).
      
pattern_molecule(A,A):-
	(
      atom(A)
   ;
      (
         (
            A= #(B)
         ;
            A= @(B)
         ),
         atom(B)
      )
   ),!.
pattern_molecule(A::B,loc(AA,B)):-
   pattern_molecule(A,AA),
   AA \= loc(_,_).

pattern_molecule(A-B,complex(X,L)):-
	pattern_complex(A-B,L,X).
pattern_molecule(A~(?),B):-
	pattern_molecule(A,P),!,
   (
      P = phospho(Q,_,L)
   ->
      B = phospho(Q,(?),L)
   ;
      B = phospho(P,(?),[])
   ).
pattern_molecule(A~{B},C):-
	pattern_molecule(A,P),
	goal_to_list(B,L,X),
   (
      P = phospho(Q,X,M)
   ->
      append(L,M,N),
      C = phospho(Q,X,N)
   ;
      C = phospho(P,X,L)
   ).

pattern_complex(?,[],?):-!.
pattern_complex(A,[A],_):-
   (atom(A);((A= #(B);A= @(B)),atom(B))),!.
pattern_complex(A,[B],_):-
	(A=(_~_);A=_~(?)),!,
	pattern_molecule(A,B).
pattern_complex(A-(?),L,?):-
	pattern_complex(A,L,_),!.
pattern_complex(A-B,L,X):-
	pattern_complex(A,LA,X),
	pattern_complex(B,LB,X),
	append(LA,LB,L).

% extracts ?
goal_to_list(A,[A],_):- var(A),!.   % FIXME ??? used when ???
goal_to_list((A,B),L,X):- !,
	goal_to_list(A,LA,X),
	goal_to_list(B,LB,X),
	append(LB,LA,L).
goal_to_list(?,[],?):-!.
goal_to_list(A,[A],_).


% matching
match_molecule(? , _):-!.
match_molecule(A,A):-!.
match_molecule(loc(A,L),loc(B,M)):-
   (
      L\=(?)
   ->
      M=L
   ;
      true
   ),
   match_molecule(A,B),!.
match_molecule(loc(A,(?)),B):-
   match_molecule(A,B),!.
match_molecule(complex(_,[A]),B):-
	match_molecule(A,B),!.
match_molecule(complex(X,A),complex(_,B)):-
	match_list(X,A,B),!.
match_molecule(phospho(M,_,[]),B):-
	match_molecule(M,B),!.
match_molecule(phospho(M,X,A),phospho(N,_,B)):-
	match_molecule(M,N),
	match_list(X,A,B),!.


selectall([],R,R).
selectall([A|L],B,R):-
	match_select(A,B,C),!,
	selectall(L,C,R).

% with stoichio
selectall2([],R,R).
selectall2([(S,A)|L],B,R):-
	match_select2((S,A),B,C),!,
	selectall2(L,C,R).

match_select(A,[B|L],L):-
	match_molecule(A,B),!.

match_select(A,[B|L],[B|R]):-
	match_select(A,L,R).

% with stoichio
match_select2((SA,A),[(SB,B)|LB],L):-
	match_molecule(A,B),
   SA=<SB,!,
   S is SB-SA,
   (
      S=0
   ->
      L=LB
   ;
      L=[(S,B)|LB]
   ).

match_select2(A,[(S,B)|L],[(S,B)|R]):-
	match_select2(A,L,R).


% Searching rules with a pattern ?

expand_rules(A):-
   pattern_rule(A,X,LA,Y,RA),
	rule(_,_,_,R,LB,RB,RL),
	match_list2(X,LA,LB),
	match_list2(Y,RA,RB),
	(
      (RL=0)
   ->
      write(R)
   ;
      write(for(RL,R))
   ),
	nl,
	fail.

expand_rules(_).

list_rules(A):-
	pattern_rule(A,X,LA,Y,RA),!,
	orule(OR),
   retractall(matching(_)),
   g_assign(full_match,'yes'),
   (
      list_rules(OR,X,LA,Y,RA)   % will always fail when completed
   ;
      g_read(full_match,FM),
      (
         FM == 'yes'
      ->
         write(OR),nl
      ;
         writeall(matching(P),P)
      ),
      fail
   ).

list_rules(_).

list_rules(OR,X,LA,Y,RA):-
   rule(OR,_,_,R,LB,RB,_),
   (
     match_list2(X,LA,LB),
     match_list2(Y,RA,RB)
   ->
      assertz(matching(R))
   ;
      g_assign(full_match,'no')
   ),
	fail.

match_head(A,B):-
	(equiv(A) -> equiv(B) ; true).

equiv((_:R)):-equiv(R).
equiv(R where _):- equiv(R).
equiv(_<=>_).
equiv(_<=_=>_).


equiv((N:R),(N:R1),(N:R2)):-
   equiv(R,R1,R2).
equiv(A<=>B,A=>B,B=>A).
equiv(A<=C=>B,A=C=>B,B=C=>A).
equiv((R where D),(R1 where D),(R2 where D)):-
   equiv(R,R1,R2).

% without stoichio
match_list(X,LA,LL):-
	(
      var(X)
   ->
      selectall(LA,LL,[])
   ;
      selectall(LA,LL,_)
   ).

% with stoichio
match_list2(X,LA,LL):-
	(
      var(X)
   ->
      selectall2(LA,LL,[])
   ;
      selectall2(LA,LL,_)
   ).

pattern_list([],[]).
pattern_list([(S,A)|L],[(S,B)|R]):-
	pattern_molecule(A,B),
	pattern_list(L,R).

% TODO matching where...
% FIXME =[?]=> n'est pas +? => +?

%pattern_rule(R,X,LL,Y,LR)
% R est le pattern de r�gle
% LL est la liste des patterns des mol�cules de la partie gauche de la r�gle R.
% X vaut ? si la partie gauche de la r�gle comporte un ?
% LR et Y c'est la m�me chose pour la partie droite de la r�gle R.
pattern_rule((_:R),X,LL,Y,LR):-
	pattern_rule(R,X,LL,Y,LR),!.
pattern_rule(_ for R,X,LL,Y,LR):-
	pattern_rule(R,X,LL,Y,LR),!.
pattern_rule(A=[R]=>B,X,LL,Y,LR):-
	pattern_solution(A,LA,X),
	pattern_solution(B,LB,Y),
	(
      pattern_molecule(R,M)
	->
	   (
         M=(?)
      ->
         X=M,
         Y=M,
         L1=[],
         L2=[]
      ;
         L1=[(1,M)],
         L2=L1
      )
	;
	   pattern_rule(R,X,L1,Y,L2)),
	append(L1,LA,LL),
	append(L2,LB,LR),!.
pattern_rule(A<=[R]=>B,X,LL,Y,LR):-
	pattern_rule(A=[R]=>B,X,LL,Y,LR),
   !.
pattern_rule(A=>B,X,LA,Y,LB):-
	pattern_solution(A,LA,X),
	pattern_solution(B,LB,Y),!.
pattern_rule(A<=>B,X,LA,Y,LB):-
	pattern_rule(A=>B,X,LA,Y,LB),!.

pattern_rule(A,_,_,_,_):-
   (
      have_gui
   ->
      format("[GUI] errors Error parsing rule pattern ~w.~n",[A])
   ;
      true
   ),
   throw('Error: parsing rule pattern '(A)). %% throw??

%pattern_solution(A,LA,X),
%A est une solution biocham (mol1+mol2+mol3...)
%LA est la liste des pattern des mol�cules de la solution A ( [pattern_mol1,pattern_mol2,...])
% X vaut ? si un ? apparait dans la solution (mol1 +?+...).
pattern_solution('_',[],_):-!.
pattern_solution('?',[],'?'):-!.
pattern_solution(A+B,L,X):-
	pattern_solution(A,LA,X),
	pattern_solution(B,LB,X),
   merge_sort_solution(LA,LB,L),!.
pattern_solution(S*A,[(S,B)],_):-
	pattern_molecule(A,B).
pattern_solution(A,[(1,B)],_):-
	pattern_molecule(A,B).


% Initial state
%%%%%%%%%%%%%%%

list_initial_state:- show_initial_state.

show_initial_state:-
   (
      have_gui
   ->
      (
         initconc(M,C),
         format("[GUI] initv ~w,~w~n",[M,C]),
         fail
      ;
         !
      )
   ;
      true
   ),
	initial_state(user_output).

initial_state(F):-
   write_present(F),
   nl(F),
   write_parameterized(F),
   nl(F),
   write_absent(F).
%	findall(M,init_present(M),L),
%   ((L == []) -> true;
%      list_to_goal(L,G),format(F,"present({~w}).~n",[G])),
%	findall(N,init_absent(N),P),
%   ((P == []) -> true;
%      list_to_goal(P,H),format(F,"absent({~w}).~n",[H])),
%   fail.
%
%initial_state(_).

write_present(F):-
   writeall(F,(initconc(M,C),number(C),C>0,format_to_atom(A,"~p",[C])),present(M,A)).

write_parameterized(F):-
   writeall(F,(initconc(M,K),k_parameter(K,_)),present(M,K)).

write_absent(F):-
   writeall(F,(initconc(M,C),number(C),C=:=0),absent(M)).

list_declarations :-
   write_declare(user_output).

write_declare(F):-
   phospho(M,P),
   list_to_goal(P,G),
   format(F,"declare ~w~~{~w}.~n",[M,G]),
   fail.

write_declare(F):-
   nl(F).

clear_initial_state:-
	retractall(initconc(_,_)),
	retractall(k_parameter(_,_)),
	retractall(k_unit(_,_)),
	retractall(k_macro(_,_)),
   (
      have_gui
   ->
      write('[GUI] clear_initial_state\n')
   ;
      true
   ),
   assertset(rules_added), assertset(initconc_added), assertset(declare_added).

present({A,_}):-
	find_molecules(A,B),    % to sort the order of phosphorylation sites
	initconc_inplace(B,1),  % default concentration is set to 1 arbitrarily
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[B,1])
   ;
      true
   ),
	fail.
present({_,L}):-
   !,present({L}).
present({A}):-
   find_molecules(A,B),    % to sort the order of phosphorylation sites
	initconc_inplace(B,1),  % default concentration is set to 1 arbitrarily
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[B,1])
   ;
      true
   ),
   assertset(rules_added),assertset(initconc_added),assertset(declare_added),
   fail.
present({_}):-
   !.

present(A):-
   present({A}).

present(A,C):-
   find_molecules(A,B),
   initconc_inplace(B,C),
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[B,C])
   ;
      true
   ),
   assertset(initconc_added),assertset(declare_added).
      
absent({A,_}):-
	find_molecules(A,B),    % same as above
	initconc_inplace(B,0),
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[B,0])
   ;
      true
   ),
	fail.
absent({_,L}):-
   !,absent({L}).

absent({A}):-
   find_molecules(A,B),    % to sort the order of phosphorylation sites
	initconc_inplace(B,0),
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[B,0])
   ;
      true
   ),
   assertset(rules_added), assertset(initconc_added),assertset(declare_added),
   fail.
absent({_}):-
   !.
absent(A):-
   absent({A}).

undefined({A,_}):-
	find_molecules(A,B),
	retractall(initconc(B,_)),
   fail.
undefined({_,L}):-
	!,undefined({L}).
undefined({A}):-
   find_molecules(A,B),
	retractall(initconc(B,_)),
   assertset(rules_added),assertset(initconc_added),assertset(declare_added),
   fail.
undefined({_}):-
   !.
undefined(A):-
   (
      have_gui
   ->
      format("[GUI] undefined ~w~n",[A])
   ;
      true
   ),
   undefined({A}).

initconc_inplace(I,C):-
   (
      initconc(I,_)
   ->
      initconc_inplace(I,C,[])
   ;
      assertz(initconc(I,C))
   ).

initconc_inplace(P,V,L):-
   retract(initconc(Q,W)),!,
   (
      (P=Q)
   ->
      initconc_list([(Q,V)|L])
   ;
      initconc_inplace(P,V,[(Q,W)|L])
   ).

initconc_list([]).
initconc_list([(P,V)|L]):-
   asserta(initconc(P,V)),
   initconc_list(L).

make_present_not_absent:-
	search_molecules,
	make_present,
	assertset(rules_added).

make_present:-
	molecule(A,_),
	\+(initconc(A,_)),
	assertz(initconc(A,1)),
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[A,1])
   ;
      true
   ),
	fail.
make_present.

make_absent_not_present:-
	search_molecules,
	make_absent,
	assertset(rules_added).

make_absent:-
	molecule(A,_),
	\+(init_present(A)),
	assertset(initconc(A,0)),
   (
      have_gui
   ->
      format("[GUI] initv ~w,~w~n",[A,0])
   ;
      true
   ),
	fail.
make_absent.

init_present(A) :-
   initconc(A,C),
   (
      number(C)
   ->
      C > 0
   ;
      k_parameter(C,V),
      V > 0
   ).

init_absent(A) :-
   initconc(A,C),
   (
      number(C)
   ->
      C =:= 0
   ;
      k_parameter(C,V),
      V =:= 0
   ).

declare(D):- treat_phospho_def(D).

% Simulator
%%%%%%%%%%%

boolean_simulation:-  % TODO: a real interruptable infinite loop 
	boolean_simulation(30).

boolean_simulation(M):-
   retractall(state(_)),
   g_assign(depth,0),
   get_showable(L),  % calls search_molecules
   fill_plot_file(L),
   data_file(Data),
   open(Data, write, _, [alias(datafile)]),
   write(datafile,'#'),
   init_data_file(L),
   init_simulator(L),
   catch(random_simulator(M,L),X,write(X)),
   close(datafile),
   retractall(plot_added(_)),
   assertz(plot_added(L)),
   retractall(kplot_changed).

% Fill in legend in the data file
init_data_file([]):-
   nl(datafile),!.

init_data_file([H|T]):-
   write(datafile,H),
   write(datafile,'\t'),
   init_data_file(T).

random_simulator(_,LM):-
	findall((A,B,C),
      (
         rule(A,BB,CC,_,_,_,_),
         forget_stoichiometry(BB,B),
         forget_stoichiometry(CC,C),
         applicable(B)
      ), L1),
	(L1=[] -> L=[((loop:_),[],[])] ; L=L1),
	g_read(depth,D), D1 is D+1, 
        g_assign(depth,D1),
   length(L,N),
	N1 is N+1,
	random(1,N1,I),
	nth(I,L,(R,LL,LR)),
	random_apply(LL,LR),
	write(D1),
	(R=(RN : _) -> write(' '),write(RN) ; true),
	write(': '), write_state(LM),
	fail.

random_simulator(M,L):-
	g_read(depth,D),
   ((var(M); D<M) ->
	   random_simulator(M,L);
      true).


boolean_enumeration:-
   retractall(state(_)),
   g_assign(depth,0),
   (
      have_gui
   ->
      format("[GUI] warnings All molecules not explicitely present are considered absent.~n",[])
   ;
      write('All molecules not explicitely present are considered absent.'),nl
   ),
   check_init,nl,
   init_simulator,
   catch(loop_simulator,_,true).

init_simulator:-
	init_present(A),
	assertset(state(A)),
	fail.
init_simulator:-
	write('0: '), write_state.

init_simulator(_):-
	init_present(A),
	assertset(state(A)),
	fail.
init_simulator(L):-
	write('0: '), write_state(L).

loop_simulator:-
	rule(A,LL,LR,_,_,_,_),
	forget_stoichiometry(LL,LLL),
	forget_stoichiometry(LR,LLR),
   applicable(LLL),
	apply(LLL,LLR),
	g_read(depth,D), D1 is D+1, g_assignb(depth,D1),
	write(D1),
	((A=(N:_))-> write(' '),write(N); true),
	write(': '),
	write_state,
   get_key(Key),
	(Key = 60 -> nl,fail;      % reads <
      (Key = 113 -> nl,throw(_);  % reads q
         loop_simulator)).

write_state:-
	state(A),
	showable(A),
	write(A),write(' '),
	fail.
write_state:- nl.

% write state w.r.t. a list of molecules
write_state(L) :-
   length(L,N),
   (
      N > 50
   ->
     
        (
                have_gui
                        ->
                                format("[GUI] errors Too many molecules to show (~w). Use ShowMolecules and HideMolecules options. ~n",[N])
                        ;
      write_line_col('Error'),
      write('too many molecules to show ('),write(N),
                                write(')\nuse "show_molecules" and "hide_molecules"\n')
        ),
      close(datafile),fail
   ;
      write_state(L,0)
   ).

write_state([],_) :-
   nl,nl(datafile).

write_state([A|L],N) :-
   write('\t'),
   (state(A) -> (write(A),M is N+1);
      M=N),
   number_atom(M,MM),
   write(datafile,MM),
   write(datafile,'\t'),
   NN is N+2,
   write_state(L,NN).

%%% Gnuplot related

fill_plot_file(L):-
   plot_file(Plot),
   open(Plot,write, Gnuplot),
   write(Gnuplot,
   'set style data steps\nset format y ""\nset yrange [0:*]\nset ytics nomirror 1\nset xrange [0:*]\nset xtics nomirror\nset mxtics 5\nset key outside Left reverse\n'),
   write_plots(L,1,Gnuplot),
   close(Gnuplot).

png_filename(Figure_count, Filename) :-
   format_to_atom(Filename, 'figure~d.png', [Figure_count]).

last_png_file(Filename) :-
   g_read(figure_count, Figure_count),
   png_filename(Figure_count, Filename).

output_png :-
   last_png_file(Filename),
   format('<png:~a>~n', [Filename]).

flush_plot :-
   (
      png
   ->
      output_png
   ;
      true
   ).

fresh_png_file(Filename) :-
   g_inc(figure_count, _, Figure_count),
   png_filename(Figure_count, Filename).

:- dynamic(png_width/1).

:- dynamic(png_height/1).

png_width(1024).

png_height(768).

set_png_size(X, Y) :-
  retract(png_width(_)),
  retract(png_height(_)),
  asserta(png_width(X)),
  asserta(png_height(Y)).

plot_png(S) :-
   png_width(X),
   png_height(Y),
   format(S, 'set term png size ~p,~p', [X, Y]),   nl(S),
   fresh_png_file(Filename),
   format(S, 'set output "~a"~n', [Filename]).

execute_plot(S, N) :-
   format_debug(5,"execute plot~n",[]),
   (
      have_gui
   ->
      write_error('[GUI] No gnuplot with GUI'),
      fail
   ;
      (
         plot_pipe(S, N)
      ->
         write(S,'reset\n'),
         (
            png
         ->
            plot_png(S)
         ;
            true
         )
      ;
         (
            find_in_path(gnuplot,Gnuplot),!
         ;
            (
               find_in_path(pgnuplot,Gnuplot),!
            ;
               write_error('gnuplot not found in PATH'),
               fail
            )
         ),
         (
            \+ windows,
            format_to_atom(Testaqua,
               "echo \"set term\" | ~w | grep -q aqua", [Gnuplot]),
            system(Testaqua, 0)
         ->
            Gnuterm='aqua'
         ;
            Gnuterm='x11'
         ),
         g_assign(plot_term,Gnuterm),
         format_to_atom(QGnuplotQ, '"~a"', [Gnuplot]),
         popen(QGnuplotQ, write, S),
         (
            png
         ->
            plot_png(S)
         ;  
            windows
         ->
            true
         ;
            format_debug(5, "set term~n", []),
            write(S,'set term '),
            write(S,Gnuterm),
            write(S,' 1\n')  % output to window number 1
         ),
         N = 1,
         assertz(plot_pipe(S, N))
      ),
      flush_output(S)
   ),
   format_debug(5,"plot stream ~w~n",[S]).

plot_load(Plot) :-
   format_debug(5,"plot_load~n",[]),
   (
      have_gui
   ->
      (
         kplot_changed
      ->
         format("[GUI] plot ~w~n",[Plot])
      ;
         format("[GUI] booleanPlot ~w~n",[Plot])
      )
   ;
      execute_plot(S, _),
      format(S,"load \"~w\"~n",[Plot]),
      flush_output(S),
      flush_plot
   ).
   

plot:-
   format_debug(5,"plot~n",[]),
   plot_file(Plot),
   format_debug(5,"plot file: ~w~n",[Plot]),
   (
      (plot_added(_),!)
   ;
      (
         kplot_changed,
         numerical_simulation(-1)
      )
   ),
   plot_load(Plot).


export_plot(F):-
   \+(atom(F)),!,
  
        (
                have_gui
                        ->
                                format("[GUI] errors Filename must be an atom, or be enclosed in simple quotes. ~n",[])
                        ;
   write_line_col('Error'),
                                write('Filename must be an atom, or be enclosed in simple quotes\n')
        ).

export_plot(F):-
   plot_added(L),!,
   retract(data_file(D)),
   retract(plot_file(P)),
   atom_concat(F,'.csv',D3),
   atom_concat(F,'.plot',P3),
   biocham_cp(D, D3),
   assertz(plot_file(P3)),
   assertz(data_file(D3)),
   check_init,nl,
   fill_plot_file(L),
   retractall(data_file(_)),
   retractall(plot_file(_)),
   assertz(data_file(D)),
   assertz(plot_file(P)).

export_plot(F) :-
   export_kinetic_plot(F).
   
% copy a file without 'cp'
biocham_cp(A, B) :-
   open(A, read, Sin),
   open(B, write, Sout),
   repeat,
      get_char(Sin, C),
      put_char(Sout, C),
      at_end_of_stream(Sin),
   !,
   close(Sin),
   close(Sout).

% write lines in the gnuplot file
write_plots([],_,G):-nl(G).

write_plots([H|T],N,G) :-
   (N > 1 -> write(G,',');write(G,'plot')),
   write(G,' "'),
   data_file(Data),
   write(G,Data),
   write(G,'" using '),
   write(G,N),
   write(G,' title "'),
   write(G,H),
   write(G,'"'),
   (
      plot_color(H,C)
   ->
      format(G," with steps lt ~w",[C])
   ;
      true
   ),
   M is N+1,
   write_plots(T,M,G).

test_plot:-
   execute_plot(P, N),
   (
      windows
   ->
      true
   ;
      png
   ->
      print(P, 'test'),
      nl(P)
   ;
      g_read(plot_term, Gnuterm),
      format(P,"set term ~w 9~ntest~nset term ~w ~w~n",[Gnuterm, N, Gnuterm])
   ),
   flush_plot.

set_color(M,C):-
   retractall(plot_color(M,_)),
   assertz(plot_color(M,C)).

keep_plot:-
   have_gui,!,
   write('[GUI] kplot\n').

keep_plot:-
   retract(plot_pipe(P,N)),
   M is ((N mod 15) + 1),              % 16 available but 0 for 'test'
   assertz(plot_pipe(P,M)),
   g_read(plot_term,Gnuterm),
   (
      windows
   ->
      true
   ;
      format(P,"set term ~w ~w~n",[Gnuterm, M])
   ).
   
%%%

applicable([]).
applicable([A|L]):-
	state(A),
	applicable(L).

apply(LL,LR):-
	add_stateb(LR),
	choose(LL,LR).

add_stateb([]).
add_stateb([A|L]):-
	assertb(state(A)),
	add_stateb(L).

add_state([]).
add_state([A|L]):-
	assertset(state(A)),
	add_state(L).

choose([],_).
choose([A|L],LR):-
	(member(A,LR) -> true; (               % if on the right, no consumption
      uniq(A) -> retractall(state(A)) ; ( % if GENE/CTRL VAR -> consume
      retractall(state(A));assertz(state(A))))), % possible consumption
	choose(L,LR).

random_apply(LL,LR):-
	add_state(LR),
	random_choose(LL,LR).

random_choose([],_).
random_choose([A|L],LR):-
	(member(A,LR) -> true; (random(0,2,X), ((X=0;uniq(A)) -> retractall(state(A));true))), % possible consumption
	random_choose(L,LR).
	

% Backtrackable assert
assertb(A):-
	(A -> true ; (assertz(A); retractall(A),fail)).

% Shown molecules

show_molecules({A,B}):-!,
   show_molecules(A),
   show_molecules({B}).
show_molecules({A}):-!,
   show_molecules(A).

show_molecules(A):-
   % suppressed because not coherent: 'show' could 'hide' molecules
   % retractall(shown(?,_)),
	pattern_molecule(A,B),
	assertset(shown(B,A)),
	hide(P,_),
	match_molecule(B,P),
	retractall(hide(P,_)),
	fail.
show_molecules(_).

hide_molecules({A,B}):-!,
   hide_molecules(A),
   hide_molecules({B}).
hide_molecules({A}):-!,
   hide_molecules(A).

hide_molecules(A):-
	pattern_molecule(A,B),
	assertset(hide(B,A)),
	shown(P,_),
	match_molecule(B,P),
	retractall(shown(P,_)),
	fail.
hide_molecules(_).

show_hide:-
	write('Show:'),nl,
	shown(_,A),
	write(A),nl,
	fail.
show_hide:-
	write('Hide:'),nl,
	hide(_,A),
	write(A),nl,
	fail.
show_hide:-
   (
      kplot_macros
   ->
      write('\nMacros will be plotted\n')
   ;
      write('\nMacros will not be plotted\n')
   ),
   (
      have_gui
   ->
      write('[GUI] show\n'),
      nusmv_search_molecules,
      findall(A,molecule(A,_),ML),
      show_hide_list(ML),
      write('[GUI] hide\n')
   ;
      true
   ).

show_hide_list([]).
show_hide_list([M|ML]):-
   (
      showable(M)
   ->
      write('show ')
   ;
      write('hide ')
   ),
   write(M),
   nl,
   show_hide_list(ML).

shown(?,?).

% showable(A):-
% 	pattern_molecule(A,B),
% 	shown(P,_),
% 	match_molecule(P,B),!,
% 	\+ hidden(B).
% 
% hidden(B):-
% 	hide(P,_),
% 	match_molecule(P,B),!.

showable(A):-
   format_debug(3,"is ~w showable?~n",[A]),
   pattern_molecule(A,B),
   shown(P,_),
   match_molecule(P,B),
   format_debug(3,"found shown pattern ~w~n",[P]),
   findall(Q,(hide(Q,_),match_molecule(Q,B)),L),
   format_debug(3,"found hidden pattern list ~w~n",[L]),
   match_rec(P,L),
   format_debug(3,"ok~n",[]).

match_rec(_,[]).

match_rec(P,[Q|L]):-
   match_molecule(Q,P),
   match_rec(P,L).

% make a list of all the showable molecules
get_showable(L) :-
   %nusmv_search_molecules,
   
        (
                have_gui
        ->
                format("[GUI] warnings All molecules not explicitely present are considered absent.~n",[])
        ;
                write('All molecules not explicitely present are considered absent.\n')
        ),
   check_init,nl,
   findall(X,molecule(X,_),ML),
   get_showable(ML,L).

get_showable([],[]).
get_showable([A|ML],L) :-
   (showable(A) ->
      L = [A|LL];
      L = LL),
   get_showable(ML,LL).
	
% biocham stop
quit:-
   have_gui,
   write('[GUI] quit\n'),
   retract(have_gui),
   fail.
   
quit:-
	nusmv_close,
   data_file(Data),
   plot_file(Plot),
   th_file(Th),
   unlink(Th),
   unlink(Data),
   unlink(Plot),
	dot_quit_hook,
   (
      plot_pipe(P,_)
   ->
      close(P, [force(true)])
   ;
      true
   ),
   halt.



list_flag:-
	listing(rules_added),
	listing(fichier),
	listing(nusmv_running),
	listing(new_file).





% cleaning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear_flag:-
	retractall(rules_added),
	retractall(new_file).


clear_running:-
	retractall(nusmv_running).

clear_file:-
	clear_rules,   % and declarations!
	clear_initial_state.

clear_all_flag:-
	clear_file,
	clear_running, % NuSMV still running?!?
	clear_flag.



clear:-
	clear_file,
	clear_flag. % order is important, because the first adds flag


% Save to a BIOCHAM file
%%%%%%%%%%%%%%%%%%%%%%%%

export_biocham(F):- export_biocham(F,no). %FF
expand_biocham(F):- export_biocham(F,yes).

export_biocham(F,_):-
   \+(atom(F)),!,
  
(
                have_gui
                        ->
                                format("[GUI] errors Filename must be an atom, or be enclosed in simple quotes. ~n",[])
                        ;
   write_line_col('Error'),
                                write('Filename must be an atom, or be enclosed in simple quotes\n')
               ).

   

%Dragana, export_biocham changed to save model's specifications in every case,not depending of theory_revision_mode and changed to save events.
export_biocham(F,Expand):-
	(
      sub_atom(F,_,3,0,'.bc')
   ->
      G=F
   ;
      atom_concat(F,'.bc',G)
   ),
	format_debug(5,"output to ~w~n",[G]),
   open(G, write, S),
	format_debug(5,"aka stream ~w~n",[S]),
   write(S,'% BIOCHAM model file'),nl(S),nl(S),
	format_debug(5,"comments done...~n",[]),
	(
      k_volume(_,_)
   ->
      write(S,'% locations\n'),
      list_volumes(S), nl(S) 
   ;
      true
   ),
   list_model(S,Expand).

list_model:-
   current_output(S),
   list_model(S,'no').
list_model(S,Expand):-
   write(S,'% declarations\n'),
   write_declare(S),
   nl(S),
	format_debug(5,"decl done...~n",[]),
  write(S,'% events\n'),nl(S),
  writeall(S,tevent(T,A,B,C),time_event(T,A,B,C)),nl(S),
         format_debug(5,"time events done...~n",[]),
  writeall(S,event3(A,B,C),event(A,B,C)),nl(S),
         format_debug(5,"events done...~n",[]),
   write(S,'% LTL specification'),nl(S),
   writeall(S,t_spec(A),add_ltl(A)),nl(S),
	format_debug(5,"LTL spec done...~n",[]),
   write(S,'% Parameters'),nl(S),
	list_parameters(S), nl(S),nl(S), nl(S),
	format_debug(5,"parameters done...~n",[]),
   write(S,'% Macros'),nl(S),
	list_macros(S), nl(S),nl(S), nl(S),
	format_debug(5,"macros done...~n",[]),
   write(S,'% Conservation laws'),nl(S),
   writeall(S,k_conservation(A),conservation(A)),nl(S),
	format_debug(5,"Conserv done...~n",[]),
	write(S,'% Reaction rules'),nl(S),
	(
      (Expand='yes')
   ->
      expand_rules_clean(S)
   ;
      write_rules(S)
   ), nl(S),
	format_debug(5,"rules done...~n",[]),
   write(S,'% Initial state'),nl(S),
	initial_state(S), nl(S),nl(S), nl(S),
	format_debug(5,"init state done...~n",[]),
	write(S,'% Spec'),nl(S),                                          
	writeall(S,spec(Q,_,_),add_spec(Q)), nl(S),nl(S), nl(S),
	format_debug(5,"spec done...~n",[]),
	close(S),
	(rules_added -> assertset(new_file); true).


readq(Stream,T) :-
 	g_read(g_old_parser, 0), !,
	read_biosyntax(Stream,T).

% Reading a term (even with variables in function position)
% quoting all variables
%%%%%%%%%%%%%%%%%%%%%%%
readq(Stream,T) :-
   %retractall(read_tok(_)),
   assertz(read_tok([])),
   g_assign(line,0),
   catch(my_read_chars(Stream),S,
      catch(read_from_chars(S,T),error(syntax_error(_),read_from_chars/2),
         (syntax_error_info(_,_,C,E),  % get incriminated char number
            nth(C,S,Char),
           
            (
                have_gui
                        ->
                                format("[GUI] errors Syntax error ~w at '~w' in ~S ~n",[E,Char,S])
                        ;
            write_line_col('Error'),
            write('syntax error "'),write(E),write('"\n'),
            format("at '~w' in~n",[Char]),
                                format("~S~n",[S])
           ),
            T=fail
         ))).

my_read_chars(Stream) :-
   repeat,
   retract(read_tok(A1)),
   read_token(Stream,A),
   (
      (Stream == user_input)
   ->
      true
   ;
      last_read_start_line_column(Line,Col),
      g_assign(line,Line),
      g_assign(col,Col)
   ),
   ((A = punct(full_stop)) ->
      (format_to_chars(A2,"~S.",[A1]),throw(A2));
      ((A = punct(end_of_file)) ->
         throw(A1);
         ((A = var(V)) ->
            (format_to_chars(A3,"~S ~q",[A1,V]),
               assertz(read_tok(A3)),fail);
            ((A = punct(P)) ->
               (atom_codes(P,[124]) -> % '|'
                  (format_to_chars(A3,"~S ~q",[A1,P]),
                     assertz(read_tok(A3)),fail);
                  (format_to_chars(A3,"~S~a",[A1,P]),
                     assertz(read_tok(A3)),fail));
               (number(A) ->
                  (format_to_chars(A3,"~S ~q",[A1,A]),
                     assertz(read_tok(A3)),fail);
                  (atom_codes(A,[44]) -> % ','
                     (format_to_chars(A3,"~S,",[A1]),
                        assertz(read_tok(A3)),fail);
                     (current_op(_,_,A) ->
                        (format_to_chars(A3,"~S ~q ",[A1,A]),
                           assertz(read_tok(A3)),fail);
                        (atom_chars(A,L),
                           unopify(L,A2,' ',Stream),   % see below
                           append(A1,[' '|A2],A3),
                           assertz(read_tok(A3)),fail)))))))).

% puts spaces around operators and quotes around atoms/vars
unopify([],[X],X,_).

unopify([('<'), ('='), ('>') | T], L, X, S) :-
   !,
   (atom_codes(X,[39]) ->
      (L = ['''','<','=','>',' '|L2]);
      L = ['<','=','>',' '|L2]),
   unopify(T,L2,' ',S).

unopify([('='), ('>') | T], L, X, S) :-
   !,
   (atom_codes(X,[39]) ->
      (L = ['''','=','>',' '|L2]);
      L = ['=','>',' '|L2]),
   unopify(T,L2,' ',S).

unopify(['?','.'],L,X,S):-
   !,
   unopify(['?'],L,X,S),
   unget_char(S,'.').

unopify([H|T],L,X,S) :-
   ((current_op(_,_,H), \+(atom_codes(H,[47]))) -> % '/' in file names!!!
      (atom_codes(X,[39]) ->
         (L=['''',' ',H,' '|L2], X2 = ' ');
         (L=[' ',H,' '|L2], X2 = ' '))
      ;
      (atom_codes(X,[39]) ->  % X = ''''
         (L=[H|L2], X2 = '''');
         (L=['''',H|L2], X2 = ''''))),
   unopify(T,L2,X2,S).

% Pretty print common errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_error(_):-
   g_read(warning_cnt,W),
   WW is W+1,
   g_assign(warning_cnt,WW),
   fail.

write_error(error(existence_error(source_sink,F),open/4)) :-
   !,
(
                have_gui
                        ->
                                format("[GUI] errors Cannot open file ~w. ~n",[F])
                        ;
                                write('Error: cannot open file '),write(F),nl
               ).

write_error(error(existence_error(procedure,_),_)) :-
   !,
        (
                have_gui
                        ->
                                format("[GUI] errors Unknown command. ~n",[])
                        ;
                                write('Error: unknown command'),nl
               ).

write_error(error(syntax_error(_),_)) :-
   !,
(
                have_gui
                        ->
                                format("[GUI] errors Syntax error.~n",[])
                        ;
                                write('Error: syntax error'),nl
               ).

write_error(error(E,G)) :-
   !,
(
                have_gui
        ->
                format("[GUI] errors ~w in ~w.~n",[E,G])
        ;
                write('Error: '),write(E),write(' in '),write(G),nl
        ).

write_error(E) :-
   (
                have_gui
        ->
                format("[GUI] errors ~w.~n",[E])
        ;
                write(E),nl
        ).

errormsg(Type, FormatString, Args) :-
   format_to_atom(Message, FormatString, Args),
   (
      have_gui
   ->
      format("[GUI] ~ws ~w.~n",[Type, Message])
   ;
      atom_chars(Type, [H | T]),
      lower_upper(H, HH),
      atom_chars(TType, [HH | T]),
      write_line_col(TType),
      write(Message),
      nl
   ).

write_line_col(E) :-
   g_read(warning_cnt,W),
   WW is W+1,
   g_assign(warning_cnt,WW),
   g_read(current_bc_stream, S),
   (
      (S \== 0)
   ->
      (
         g_read(current_bc_file, F),
         line_count(S, Line),
         format("[File: ~w, Line: ~w] ~w: ",[F, Line, E])
      )
   ;
      (
         write(E),
         write(': ')
      )
   ).

% Look if some component of the compound is 'unique'
% (for instance a Gene)
%%%%%%%%%%%%%%%%%%%%%%%
uniq(@(A)) :-
	!,atom(A).  % CTRL var

uniq(A) :-
   get_gene(A,_).

get_gene(A-B,G):-
	!,(get_gene(A,G);get_gene(B,G)).

get_gene(A~{_},G):-
	!,get_gene(A,G).

get_gene(#(A),A):-
   atom(A).

% Debug messages
%%%%%%%%%%%%%%%%

format_debug(Level,S,A):-
   g_read(debug,N),
   (
      (N >= Level)
   -> 
      (
         (
            (Level > 0)
         ->
            write(user_error, '** Debug ** ')
         ;
            true
         ),
         format(user_error, S,A), flush_output(user_error)
      )
   ;
      true
   ).

% Dump commands
%%%%%%%%%%%%%%%

dump_commands :-
   \+ (
      comm_type(Command, Args),
      \+ (
         command_atom(Command, Args, Atom),
         format('~a;', [Atom])
      )
   ),
   write('\n').

command_atom(Command, Args, Atom) :-
   (
      Args = []
   ->
      Atom = Command
   ;
      Args = [Head | Tail]
   ->
      atom_chars(Command, Command_chars),
      atom_chars(Head, Head_chars),
      append(Command_chars, ['(', '<' | Head_chars], Beginning_chars),
      append(Beginning_chars, Tail_chars, Chars),
      command_atom_args(Tail, Tail_chars),
      atom_chars(Atom, Chars)
   ).

command_atom_args([], ['>', ')']).

command_atom_args([Head | Tail], Chars) :-
   atom_chars(Head, Head_chars),
   append(['>', ',', ' ', '<' | Head_chars], Tail_chars, Chars),
   command_atom_args(Tail, Tail_chars).

   
% Export to other formats
%%%%%%%%%%%%%%%%%%%%%%%%%


:- include(biochamVars).
:- include(biochamDot).
:- include(biochamProlog).
:- include(biochamSbml).
:- include(biochamKinetics).
:- include(biochamOde).
:- include(biochamReduce).
:- include(biochamMorphisms).
% :- include(biochamLotos).
:- include(biochamLearn).
:- include(biochamNuSMV). %FF caution this file changes the priority of -> !!!! should be included last
