% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2003-2013, INRIA, Projet Contraintes
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
% GNU prolog file biochamMatlab.pl by Sylvain Soliman
 
load_matlab(F):-
   clear_biocham,
   add_matlab(F).

add_matlab(F):-
   \+(atom(F)),
   !,
   write_line_col('Error'),
   write('Filename must be an atom, or be enclosed in simple quotes\n'),
   fail.

add_matlab(F):-
   (
      sub_atom(F, _, _, 0, '.m')
   ->
      G=F
   ;
      atom_concat(F, '.m', G)
   ),
   retractall(ode_rate(_, _)),
   retractall(ode_species(_)),
   open(G, read, _, [alias(ode)]),
   format_debug(8, "parsing Matlab file...", []),
   catch(parse_matlab, E, write('Error in parsing Matlab file '(E))),
   close(ode),
   !,
   g_read(debug, Debug),
   (
      Debug >= 8
   ->
      listing(ode_rate/2)
   ;
      true
   ),
   % same end as add_ode
   recover_zombie_molecules,
   format_debug(8, "~nadding kinetics...", []),
   add_kinetics,
   format_debug(8, "~ngenerating biocham rules...", []),
   generate_biocham_rules,
   format_debug(8, "~nadding biocham rules...", []),
   add_biocham_rules,
   format_debug(8, "done.~N", []).


parse_matlab :-
   repeat,
   (
      skip_matlab_blank(C),
      get_matlab_line(C, L)
   ->
      parse_matlab_line(L),
      fail
   ;
      !
   ).

skip_matlab_blank(C):-
   get_char(ode, B),
   (
      memberchk(B, [' ', '\n', '\t', '\r'])
   ->
      skip_matlab_blank(C)
   ;
      C = B
   ).

get_matlab_line(end_of_file, _):-
   !,
   fail.

get_matlab_line(C, []) :-
   % only care about lines starting with dydt...
   C \= 'd',
   !,
   get_line('%', _).

get_matlab_line('d', L) :-
   get_matlab_line_aux('d', L).

get_matlab_line('%', []) :-
   !,
   get_line('%', _).

get_matlab_line_aux(end_of_file, []):-
   !.

get_matlab_line_aux(';', ['.']):-
   !.

get_matlab_line_aux(C, [C | L]):-
   get_char(ode, D),
   get_matlab_line_aux(D, L).


parse_matlab_line([]).

parse_matlab_line(['d', 'y', 'd', 't', '=' | _]) :-
   !.

parse_matlab_line(['d', 'y', 'd', 't', '(' | L]) :-
   parse_matlab_int(L, I, [')', '=' | L2], 0),
   parse_matlab_expression(L2, E),
   format_debug(8, "read expression ~p for ~p~n", [E, I]),
   add_ode_term(E, I).


parse_matlab_expression(L, E) :-
   read_from_chars(L, E).


get_ode_rate(S, R) :-
   (
      ode_species(S)
   ->
      ode_rate(R, S)
   ;
      R = 0
   ).


add_ode_term(E, I) :-
   !,
   substitute_indices(E, EE),
   number_atom(I, A),
   atom_concat('y__', A, S),
   (
      ode_species(S)
   ->
      retract(ode_rate(_, S)),
      assertz(ode_rate(EE, S))
   ;
      assertz(ode_species(S)),
      assertz(ode_rate(EE, S))
   ).

add_ode_term(X, I) :-
   format_to_atom(
      A,
      "Warning: Unrecognized expression ~p in the right side of ~p~n",
      [X, I]
   ),
   write_error(A).

parse_matlab_int([D | L1], I, L2, I0) :-
   D @>= '0',
   D @=< '9',
   !,
   number_atom(N, D),
   I1 is I0 * 10 + N,
   parse_matlab_int(L1, I, L2, I1).

parse_matlab_int(L, I, L, I).


substitute_indices(X, Y) :-
   X =.. [F, I],
   number(I),
   \+(current_op(_, _, F)),
   !,
   number_atom(I, A),
   (
      F = dydt
   ->
      atom_concat('y__', A, YY),
      get_ode_rate(YY, Y)
   ;
      atom_concat(F, '__', FF),
      atom_concat(FF, A, Y)
   ).

substitute_indices(X, Y) :-
   compound(X),
   !,
   X =.. [H | T1],
   substitute_indices_rec(T1, T2),
   Y =.. [H | T2].

substitute_indices(X, X).


substitute_indices_rec([], []).

substitute_indices_rec([H | T], [HH | TT]) :-
   substitute_indices(H, HH),
   substitute_indices_rec(T, TT).
