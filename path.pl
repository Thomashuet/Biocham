% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2005-2010, INRIA, Projet Contraintes
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
% GNU prolog file path.pl by Sylvain Soliman

%%% Path finding utilities

windows :-
   os_version(OS),
   sub_atom(OS, _, _, _, 'Windows').

safe_file_permission(F, P) :-
   catch(file_permission(F, P), _, fail).

% looks for F in path, returns full path P if found
find_in_path(F,P):-
   environ('PATH',PATH),
   split_path(PATH,PL),
   format_debug(6, "splitted path: ~p~n", [PL]),
   (
     windows
   ->
     atom_concat(F, '.exe', FE),
     atom_concat('\\', FE, FF)
   ;
     atom_concat('/',F,FF)
   ),
   g_read(biocham_path,BP),
   (
      BP \= 0,
      file_property(BP, type(directory))
   ->
      Path = BP
   ;
      Path = '.'
   ),
   find_in_path(FF, [Path | PL], P),
   format_debug(6,"found executable: ~w~n", [P]).

% looks for file F (starting with /) in path list, returns full path P if
% found
find_in_path(F,[H|T],P):-
   atom_concat(H,F,A),
   format_debug(6, "Trying ~w~n", [A]),
   (
      safe_file_permission(A,execute)
   ->
      P = A
   ;
      find_in_path(F,T,P)
   ).

% split a : separated path in an atom list
split_path(A,[H|T]):-
   (
     windows
   ->
     Sep = (';')
   ;
     Sep = (':')
   ),
   sub_atom(A,B,1,_,Sep),!,
   sub_atom(A,0,B,_,H),
   BB is B+1,        
   sub_atom(A,BB,_,0,AA),
   split_path(AA,T).
                     
split_path(A,[A]).

current_directory:- 
   working_directory(X),
   write(X),nl.



