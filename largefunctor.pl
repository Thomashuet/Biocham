% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2004-2010, INRIA, Projet Contraintes
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
% GNU prolog file largefunctor.pl by François Fages
% for representing terms of arity >= max_arity 255
% as f(... , f(... , f(...)))

% functor
large_functor(T,F,N):-
	nonvar(T),
	!,
	functor(T,F,K),
	arg(K,T,L),
	((nonvar(L),functor(L,F,_))
	->
		 large_functor(L,_,M),
		 N is K+M
	;
		 N=K).

large_functor(T,F,N):-
	N>=255,
	!,
	K is N-254,
	functor(T,F,255),
	arg(255,T,L),
	large_functor(L,F,K).

large_functor(T,F,N):-
	functor(T,F,N).

% arg
large_arg(I,T,A):-
	I<255,
	!,
	arg(I,T,A).

large_arg(I,T,A):-
	arg(255,T,L),
	J is I-254,
	large_arg(J,L,A).

% :..
large_dec(S,L):-
	var(S),
	!,
	L=[F|LA],
	length(LA,N),
	large_functor(S,F,N),
	args_from_list(S,LA,1).

large_dec(S,[F|L]):-
	large_functor(S,F,_),
	args_to_list(S,L,1).

args_from_list(_,[],_):-
	!.
args_from_list(S,L,255):-
	!,
	arg(255,S,T),
	args_from_list(T,L,1).
args_from_list(S,[A|L],I):-
	!,
	large_arg(I,S,A),
	I1 is I+1,
	args_from_list(S,L,I1).
	
args_to_list(S,L,255):-
	!,
	arg(255,S,T),
	args_to_list(T,L,1).
args_to_list(S,[],I):-
	functor(S,_,J),
	I>J,
	!.
args_to_list(S,[A|L],I):-
	arg(I,S,A),
	I1 is I+1,
	args_to_list(S,L,I1).

% map F on arg
large_map(S,T,I,J,F):-
	((I>J)
	->
		 true
	;
		 ((I>=255)
		 ->
			  arg(255,S,LS),
			  arg(255,T,LT),
			  I1 is I-254,
			  J1 is J-254,
			  large_map(LS,LT,I1,J1,F)
		 ;
			  arg(I,S,A),
			  arg(I,T,B),
			  G=..[F,A,B],
			  call(G),
			  I1 is I+1,
			  large_map(S,T,I1,J,F))).

	

