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
% GNU prolog file biochamLotos.pl by Nathalie Sznajder
% version du 22/07/02
 
% Exports BIOCHAM to LOTOS 

% TODO: use GENE and CONTROL_VAR

%writes the rules into a lotos file F.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_lotos(F):-
   \+(atom(F)),!,
  
        (
                have_gui
        ->
                format("[GUI] errors Filename must be an atom, or be enclosed in simple quotes.~n",[])
        ;
   write_line_col('Error'),
                write('Filename must be an atom, or be enclosed in simple quotes\n')
        ).

export_lotos(F):-
        g_assign(numero_regle, 0),
        list_rules,
        search_molecules,
	(sub_atom(F,_,_,0,'.lot') -> G=F ; atom_concat(F,'.lot',G)),
        open(G, write, _, [alias(lotos)]),
        write_term(lotos, 'specification Biochimie [ETAT, ', []),
        write_list_rules(lotos),
        write_term(lotos, '] : noexit\n', []),
        nl(lotos),
        write_term(lotos,'library BOOLEAN endlib\n', []),
        nl(lotos),
        write_types(lotos),
        write_term(lotos, '(****************************************************)\n\n', []),
        write_lotos(lotos),
        write_obs_state(lotos),
        write_term(lotos, '\tendproc\n', []),
        write_term(lotos, '\nendspec\n', []),
        close(lotos).

write_molecule(Stream, A):-
	(A=B-C -> (write_molecule(Stream, B), write_term(Stream, 'Z_T', []),write_molecule(Stream, C));
   %(A=B~{C}->(write_molecule(Stream, B), write_term(Stream, 'Z_E', []), write_phosphoryl(Stream, C), write_term(Stream, 'Z_A', []));
	(
      A='~'(B,{C})
   ->
      (
         write_molecule(Stream, B),
         write_term(Stream, 'Z_E', []),
         write_phosphoryl(Stream, C),
         write_term(Stream, 'Z_A', [])
      )
   ;
	   write_term(Stream, A, [])
   )).

write_phosphoryl(Stream, A):-
	(A=(B,C) -> (write_term(Stream, B, []), write_term(Stream, 'Z_V', []), write_phosphoryl(Stream,C));
	    (write_term(Stream, A, []))).

%writes R1, ..., Rn
write_list_rules(Alias) :-
        g_assign(numero_regle, 0),
        rule(_, _, _, _, _, _, _),
        %pour incrementer numero_regle, car g_inc ne passe pas... pourquoi?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        g_read(numero_regle, Old),
        New is Old+1,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        g_assign(numero_regle, New),
        (New==1 -> true ; write_term(Alias, ', ', [])),
        write_term(Alias, 'R', []),
        write_term(Alias, New, []),
        fail.

write_list_rules(_).
%       stream_line_column(Alias, Line, Column),
%       Result is dec(Column),
%       set_stream_line_column(Alias, Line, Result).

%writes the types declaration for all the molecules.
write_types(Alias) :- 
        molecule(A,_), 
        write_type_decl(Alias, A),  
        nl(Alias),
        nl(Alias),
        fail.

write_types(_). 

%writes the type declaration of a molecule Mol.
write_type_decl(Alias, Mol):- 
        write_term(Alias, 'type ABSTRACTION_', []),
        write_molecule(Alias, Mol),
        write_term(Alias, ' is BOOLEAN', []),
        nl(Alias),
        write_term(Alias, '\t sorts TYPE_', []),
        write_molecule(Alias, Mol),
        nl(Alias),
        write_term(Alias, '\t opns', []),
        write_term(Alias, '\n\t\tSANS_', []),
        write_molecule(Alias, Mol),
	write_term(Alias, ' (*! constructor *)', []), 
        write_term(Alias, ', AVEC_', []),
        write_molecule(Alias, Mol),
	write_term(Alias, '(*! constructor *)', []),
        write_term(Alias, ' : -> TYPE_', []),
        write_molecule(Alias, Mol),
        nl(Alias),
        write_term(Alias, '\t\tPRESENT : TYPE_', []),
        write_molecule(Alias, Mol),
        write_term(Alias, ' -> BOOL', []),
        nl(Alias),
        write_term(Alias, '\teqns', []),
        nl(Alias),
        write_term(Alias, '\t\tofsort BOOL', []),
        nl(Alias),
        write_term(Alias, '\t\t\tPRESENT (SANS_', []),
        write_molecule(Alias, Mol),
        write_term(Alias, ') = false;\n\t\t\t', []),
        write_term(Alias, 'PRESENT(AVEC_', []),
        write_molecule(Alias, Mol),
        write_term(Alias, ') = true;', []),
        nl(Alias),
        write_term(Alias, 'endtype', []).


%behaviour part.
%%%%%%%%%%%%%%%% 
write_lotos(Alias):- 
        write_term(Alias, 'behaviour', []),
        nl(Alias),
        nl(Alias),
        write_choice(Alias),
        write_term(Alias, '\t\tReaction [ETAT,', []),
        write_list_rules(Alias),
        write_term(Alias, '] (', []),
        write_initial_state(Alias),
        write_term(Alias, ')', []),
        nl(Alias),
        nl(Alias),
        write_term(Alias, 'where\n\n', []),
        write_term(Alias, '\tprocess Reaction [ETAT, ', []),
        write_list_rules(Alias),
        write_term(Alias, '] (', []),
        write_liste_types(Alias),
        write_term(Alias, ') : noexit :=', []),
        nl(Alias),
        write_rules_decl(Alias).


write_choice(Alias):-
   molecule(A,_),
   (
      initconc(A,_)
   ->
      true
   ;
      write_term(Alias, '\tchoice ', []),
      write_molecule(Alias, A),
      write_term(Alias, ':TYPE_', []),
      write_molecule(Alias, A),
      write_term(Alias, ' []\n', [])
   ),
   fail.

write_choice(_).

%writes the initial state : for each molecule A 'AVEC_A' if it is present,
%'SANS_A' if it is absent, and A if it is not specified.
write_initial_state(Alias) :-
        g_assign(numero_mol, 0),

        molecule(A,_),
        g_read(numero_mol, Old),
        New is Old+1,
        g_assign(numero_mol, New),
        (New==1 ->true;  write_term(Alias, ', ', [])),
    
        ((init_present(A), 
        write_term(Alias, 'AVEC_', []),
        write_molecule(Alias, A));
        (\+init_present(A),
        ((init_absent(A),
        write_term(Alias, 'SANS_', []),
        write_molecule(Alias, A));
        (\+init_absent(A),
        write_molecule(Alias, A))))),
        fail.

write_initial_state(_).

%writes the list of molecules followed by their types.
write_liste_types(Alias) :-
        g_assign(numero_mol, 0),
        molecule(A,_),
        g_read(numero_mol, Old),
        New is Old+1,
        g_assign(numero_mol, New),
        (New == 1 -> true; write_term(Alias, ',', [])),
        write_molecule(Alias, A),
        write_term(Alias, ':TYPE_', []),
        write_molecule(Alias, A),
        fail.


write_liste_types(_).

%writes all the rules declarations.
write_rules_decl(Alias) :-
        g_assign(numero_courant, 0),
        rule(_, L1, L2, _, _, _, _),
        forget_stoichiometry(L1,LL),
        forget_stoichiometry(L2,LR),
        nl(Alias),
        write_rule_decl(Alias, LL, LR),
        fail.

write_rules_decl(_).

%for one rule rule(Q, L, R), writes the corresponding declaration.
write_rule_decl(Alias, L, R):-
        g_read(numero_courant, Old),
        New is Old+1,
        g_assign(numero_courant, New),
        write_term(Alias, '\t\t(* reaction R', []),
        write_term(Alias,New, []),
        write_term(Alias, ' *)', []),
        nl(Alias),
        write_left(Alias, L),
        nl(Alias),
        write_term(Alias, '\t\t    Reaction [ETAT, ', []), 
        g_assign(numero_regle, 0),
        write_list_rules(Alias),
        write_term(Alias, '] (', []),
        write_right(Alias, R),
        write_term(Alias, ')', []),
        nl(Alias),
        write_term(Alias, '\t\t[] \n', []).

%writes the data about the left member of the rule.
write_left(Alias, L) :-
	g_assign(listMolSeen, []),
	unique(L),
	g_read(listMolSeen, P),
        write_term( Alias,'\t\t[', []),
        write_left_first(Alias, P),
        !,
        write_term( Alias,'] ->', []),
        nl(Alias),
        write_term(Alias, '\t\t  R', []),
        g_read(numero_courant, Numero),
        write_term(Alias, Numero, []),
	g_read(listMolSeen, List),
	write_left_second(Alias, List).


%writes the molecules of the left member as PRESENT(A).
write_left_first(_, []).      


write_left_first(Alias, [A|L]) :-
	write_term(Alias, 'PRESENT (', []),
	write_molecule(Alias, A),
	(length(L, 0)->write_term(Alias, ')', []);
	    (write_term(Alias, ') and ', []),
	    write_left_first(Alias, L))).

write_left_second(Alias, []):-
        write_term(Alias, ';', []).

write_left_second(Alias, [A|L]):-
        write_term(Alias, ' ?', []),
        write_molecule(Alias, A),
        write_term(Alias, ':TYPE_', []),
        write_molecule(Alias, A),
        write_left_second(Alias, L).
        
%writes the data about the right part of the rule.
write_right(Alias, R):-
        g_assign(numero_molecule, 0),
        molecule(A,_),
        g_read(numero_molecule, Old),
        New is Old+1,
        g_assign(numero_molecule, New),

        (New == 1 -> true ; write_term(Alias, ',', [])),
        ((member(A, R),
        write_term(Alias, 'AVEC_', []),
        write_molecule(Alias, A));
        (\+member(A,R),
        write_molecule(Alias, A))),
        fail.

write_right(_,_).
        
        
        
write_obs_state(Alias):-
        write_term(Alias, '\t\t(*observation de l\'etat*)\n', []),
        write_term(Alias, '\t\tETAT', []),
        write_molecules(Alias),
        write_term(Alias, ';\n', []),
        write_term(Alias, '\t\t\tReaction [ETAT, ', []),
        g_assign(numero_regle, 0),
        write_list_rules(Alias),
        write_term(Alias, '] (', []),
        write_list_molecules(Alias),
        write_term(Alias, ')\n', []).

write_molecules(Alias):-
        molecule(A,_),
        write_term(Alias, ' !', []),
        write_molecule(Alias, A),
        fail.

write_molecules(_).

write_list_molecules(Alias):-
        g_assign(numero_mol, 0),
        molecule(A,_),
        g_read(numero_mol, Old),
        New is Old+1,
        g_assign(numero_mol, New),
        (New==1 -> true; write_term(Alias, ',', [])),
        write_molecule(Alias, A),
        fail.

write_list_molecules(_).

%assigns listMolSeen to a list where each term appears only once.
unique([]).

unique([A|L]):-
	g_read(listMolSeen, P),
	(member(A, P)->(NewList = P);
	    (NewList = [A|P])),
	g_assign(listMolSeen, NewList),
	unique(L).


  %:-include('biochamPrism.pl').
