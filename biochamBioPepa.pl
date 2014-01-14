% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2004-2013, INRIA, Projet Contraintes
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
% GNU prolog file importBioPepa.pl by Sylvain Soliman

% Very partial BioPepa export. Only structural (stoichiometry matrix)

:- dynamic(biopepa_rule/3).
:- dynamic(biopepa_molecule/1).

export_biopepa(F):-
   \+(atom(F)),
   !,
   (
      have_gui
   ->
      write('[GUI] errors Export BioPepa: ')
   ;
      write_line_col('Error')
   ),
   write('Filename must be an atom, or be enclosed in simple quotes\n').


export_biopepa(F):-
   (
      sub_atom(F,_,1,_,'.')
   ->
      (G=F)
   ;
      atom_concat(F,'.biopepa',G)
   ),
   open(G, write, _, [alias(biopepa)]),
   biopepa_fake_rates,
   biopepa_species,
   biopepa_initial_state,
   close(biopepa).


biopepa_fake_rates :-
   retractall(biopepa_rule(_, _, _)),
   g_assign(biopepa_number, 0),
   (
      rule(R, LL, LR, ER, _, _, _),
      g_read(biopepa_number, Nb),
      NNb is Nb+1,
      g_assign(biopepa_number, NNb),
      (
         R = (Name:RR)
      ->
         (
            % already expanded -> name is unigue
            RR = ER
         ->
            N = Name
         ;
            format_to_atom(N, "~w~w", [Name, Nb])
         )
      ;
         format_to_atom(N, "r~w", [Nb])
      ),
      remove_common(LL, LR, LL1, LR1),
      format(biopepa, "kineticLawOf ~w : 1;~n", [N]),
      assertz(biopepa_rule(N, LL1, LR1)),
      fail
   ;
      nl(biopepa)
   ).


biopepa_species :-
   retractall(biopepa_molecule(_)),
   search_molecules,
   molecule(M, _),
   sbml_convert_molecule_name(M, MM),
   format_debug(5, "exporting for ~w~n", [M]),
   assertz(biopepa_molecule(MM)),
   biopepa_rules(M, MM),
   fail.

biopepa_species :-
   nl(biopepa).


biopepa_rules(M, MM) :-
   format_to_atom(InitNext, "~w = ", [MM]),
   g_assign(biopepa_next, InitNext),
   biopepa_rule(R, LL, LR),
   format_debug(8, "   exporting for rule ~w~n", [R]),
   g_read(biopepa_next, Next),
   (
      member((S, M), LL)
   ->
      format(biopepa, "~w(~w,~w) << ~w ", [Next, R, S, MM]),
      g_assign(biopepa_next, '+ ')
   ;
      member((S, M), LR)
   ->
      format(biopepa, "~w(~w,~w) >> ~w ", [Next, R, S, MM]),
      g_assign(biopepa_next, '+ ')
   ;
      true
   ),
   fail.

biopepa_rules(_, _) :-
   (
      g_read(biopepa_next, '+ ')
   ->
      write(biopepa, ';\n')
   ;
      true
   ).


biopepa_initial_state :-
   findall(M, biopepa_molecule(M), L),
   biopepa_initial_state(L).


biopepa_initial_state([]).

biopepa_initial_state([M]) :-
   write(biopepa, M),
   write(biopepa, '[0]\n').

biopepa_initial_state([M1, M2 | L]) :-
   write(biopepa, M1),
   write(biopepa, '[0] <*> '),
   biopepa_initial_state([M2 | L]).
