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
% GNU prolog file formal.pl by Sylvain Soliman

%%% Symbolic work on equations

% Global variables used:
%   formal_matrix
%   formal_m
%   formal_n
%   formal_imax
%   formal_index
%   formal_scaling
%   formal_sum
%   formal_tmp
%   formal_vector

% Simplify

simplify(E,F):-
   E =.. [O,A,B],
   number(A),
   number(B),!,
   (
      O = (^)
   ->
      G =.. ['**',A,B],
      FF is G
   ;
      FF is E
   ),
   (
      FF >= 0
   ->
      F=FF
   ;
      F = -(-FF)
   ).

simplify(E+F,G):-
   !,simplify(E,EE),
   simplify(F,FF),
   (
      has_same_main(EE,FF,Main,EEE,FFF)
   ->
      simplify((EEE+FFF)*Main,G)
   ;
      (
         has_val(EE,0)
      ->
         G = FF
      ; 
         (
            EE = -(EEE)
         ->
            (
               FF = -(FFF)
            ->
               G = -(EEE+FFF)
            ;
               G = FF - EEE
            )
         ;
            (
               FF = -(FFF)
            ->
               G = EE - FFF
            ;
               (
                  has_val(FF,0)
               ->
                  G = EE
               ;
                  G = EE + FF
               )
            )
         )
      )
   ).

simplify(E-F,G):-
   !,simplify(E,EE),
   simplify(F,FF),
   (
      has_same_main(EE,FF,Main,EEE,FFF)
   ->
      simplify((EEE-FFF)*Main,G)
   ;
      (
         has_val(EE,0)
      ->
         (
            FF = -(FFF)
         ->
            G = FFF
         ;
            (
               has_val(FF,0)
            ->
               G = 0
            ;
               G = -(FF)
            )
         )
      ; 
         (
            FF = -(FFF)
         ->
            G = EE + FFF
         ;
            (
               has_val(FF,0)
            ->
               G = EE
            ;
               G = EE - FF
            )
         )
      )
   ).

simplify(-(E),F):-
   !,simplify(E,EE),
   (
      EE = -(F)
   ->
      true
   ;
      (
         EE = E1 - E2
      ->
         F = E2 - E1
      ;
         (
            has_val(EE,0)
         ->
            F = 0
         ;
            F = -(EE)
         )
      )
   ).

simplify(log(E),G):-
   !,simplify(E,EE),
   (
      EE = exp(G)
   ->
      true
   ;
      G = log(EE)
   ).

simplify(exp(E),G):-
   !,simplify(E,EE),
   (
      EE = log(G)
   ->
      true
   ;
      G = exp(EE)
   ).

simplify(sin(E),sin(EE)):-
   !,simplify(E,EE).

simplify(cos(E),cos(EE)):-
   !,simplify(E,EE).

simplify(abs(E),abs(EE)):-
   !,simplify(E,EE).

simplify(frac(E), frac(EE)):-
   !,
   simplify(E,EE).

simplify(min(E,F),min(EE,FF)):-
   !,simplify(E,EE),
   simplify(F,FF).

simplify(max(E,F),max(EE,FF)):-
   !,simplify(E,EE),
   simplify(F,FF).

simplify(E^F,G):-
   !,simplify(E,EE),
   simplify(F,FF),
   (
      has_val(FF,1)
   ->
      G = EE
   ;
      G = EE^FF
   ).
   
simplify(E*F,G):-
   !,simplify(E,EE),
   simplify(F,FF),
   (
      has_val(EE,1)
   ->
      (G = FF)
   ;
      (
         has_val(FF,1)
      ->
         G = EE
      ;
         (
            (has_val(EE,0) ; has_val(FF,0))
         ->
            G = 0
         ;
            (
               EE = -(EEE)
            ->
               (
                  FF = -(FFF)
               ->
                  (
                     has_val(EEE,1)
                  ->
                     G = FFF
                  ;
                     G = EEE*FFF
                  )
               ;
                  (
                     has_val(EEE,1)
                  ->
                     G = -(FF)
                  ;
                     G = -(EEE*FF)
                  )
               )
            ;
               (
                  FF = -(FFF)
               ->
                  G = -(EE*FFF)
               ;
                  G = EE*FF
               )
            )
         )
      )
   ).

simplify(E/F,G):-
   !,simplify(F,FF),
   (
      has_val(FF,0)
   ->
      throw(error(evaluation_error(zero_divisor), simplify/2))
   ;
      true
   ),
   (
      (FF = ONE/FFF , has_val(ONE,1))
   ->
      simplify(E*FFF,G)
   ;
      (
         simplify(E,EE),
         (
            has_val(EE,1)
         ->
            G = 1/FF
         ;
            (
               has_val(FF,1)
            ->
               G = EE
            ;
               (
                  has_val(EE,0)
               ->
                  G = 0
               ;
                  (
                     EE = -(EEE)
                  ->
                     (
                        FF = -(FFF)
                     ->
                        G = EEE/FFF
                     ;
                        G = -(EEE/FF)
                     )
                  ;
                     (
                        FF = -(FFF)
                     ->
                        G = -(EE/FFF)
                     ;
                        G = EE/FF
                     )
                  )
               )
            )
         )
      )
   ).

% simplify(if C then D else E, F):-
simplify(if(then(C,else(D,E))), F):-
   !,simplify(D,DD),
   simplify(E,EE),
   (
      EE = DD
   ->
      F = EE
   ;
      %F = (if C then DD else EE)
      F = (if(then(C,else(DD,EE))))
   ).

simplify(A,A).

% check if X is evaluable, and has value V

has_val(X,V):-
   catch(X =:= V,_,fail).

% get a common factor
has_same_main(N*T,M*T,T,N,M).
has_same_main(-(N*T),M*T,T,-(N),M).
has_same_main(N*T,-(M*T),T,N,-(M)).
has_same_main(-(N*T),-(M*T),T,-(N),-(M)).
has_same_main(T,M*T,T,1,M).
has_same_main(-(T),M*T,T,-1,M).
has_same_main(T,-(M*T),T,1,-(M)).
has_same_main(N*T,T,T,N,1).
has_same_main(N*T,-(T),T,N,-1).
has_same_main(-(N*T),T,T,-(N),1).

% try to avoid unnecessary parentheses

prettify(A+(B+C),D+E):-
   !,prettify(A+B,D),
   prettify(C,E).

% Not for BIOCHAM precedences
% prettify(A+(B-C),D-E):-
%    !,prettify(A+B,D),
%    prettify(C,E).

prettify((A+B)-C,D+E):-
   !,prettify(A,D),
   prettify(B-C,E).

prettify(A-(B+C),D-E):-
   !,prettify(A-B,D),
   prettify(C,E).

prettify(A-(B-C),D+E):-
   !,prettify(A-B,D),
   prettify(C,E).

prettify(A*(B*C),D*E):-
   !,prettify(A*B,D),
   prettify(C,E).

prettify(A*(B/C),D/E):-
   !,prettify(A*B,D),
   prettify(C,E).

prettify(A,A):-
   atomic(A),!.

prettify(A,B):-
   A=..[F|LA],
   prettify_rec(LA,LB),
   B=..[F|LB].

prettify_rec([HA|TA],[HB|TB]):-
   prettify(HA,HB),
   prettify_rec(TA,TB).

prettify_rec([],[]).

% differentiation Exp, X, dExp/dX

differentiate(U+V,X,DU+DV):-
   !,differentiate(U,X,DU),
   differentiate(V,X,DV).
differentiate(U-V,X,DU-DV):-
   !,differentiate(U,X,DU),
   differentiate(V,X,DV).
differentiate(U*V,X,DU*V+U*DV):-
   !,differentiate(U,X,DU),
   differentiate(V,X,DV).
differentiate(U/V,X,DU/V-U*DV/V^2):-
   !,differentiate(U,X,DU),
   differentiate(V,X,DV).
differentiate(U^N,X,N*(U^N1)*DU):-
   integer(N),!,
   N1 is N-1,
   differentiate(U,X,DU).
differentiate(-U,X,-DU):-
   !,differentiate(U,X,DU).
differentiate(log(U),X,DU/U):-
   !,differentiate(U,X,DU).
differentiate(exp(U),X,DU*exp(U)):-
   !,differentiate(U,X,DU).
differentiate(cos(U),X,-sin(DU)):-
   !,differentiate(U,X,DU).
differentiate(sin(U),X,cos(DU)):-
   !,differentiate(U,X,DU).
differentiate(frac(U), X, DU):-
   !,differentiate(U, X, DU).
differentiate(abs(U),X,R):-
   !,differentiate(if(then(U<0,else(-U,U))),X,R).
differentiate(min(U,V),X,R):-
   !,differentiate(if(then(U<V,else(U,V))),X,R).
differentiate(max(U,V),X,R):-
   !,differentiate(if(then(U>V,else(U,V))),X,R).
differentiate(random,_,0):-!.
differentiate(X,X,1):-!.

% differentiate(if C then D else E,X,if C then DD else DE):-
differentiate(if(then(C,else(D,E))),X,if(then(C,else(DD,DE)))):-
   !,differentiate(D,X,DD),
   differentiate(E,X,DE).

differentiate(_,_,0).

%%% Linear algebra

:-dynamic(line_matrix/1).

% set up the global array for storing a matrix

init_matrix([H|T]):-
   length([H|T],M),
   length(H,N),
   g_assign(formal_matrix,g_array(M,g_array(N))),
   MM is M-1,
   g_assign(formal_m,MM),
   NN is N-1,
   g_assign(formal_n,NN),
   fill_matrix([H|T],0).

fill_matrix([],_).
fill_matrix([H|T],I):-
   fill_matrix_rec(H,I,0),
   II is I+1,
   fill_matrix(T,II).

fill_matrix_rec([],_,_).
fill_matrix_rec([H|T],I,J):-
   g_assign(formal_matrix(I,J),H),
   JJ is J+1,
   fill_matrix_rec(T,I,JJ).

% get a matrix from the global array

get_matrix(A):-
   retractall(line_matrix(_)),
   g_read(formal_m,M),
   catch(
      (
         for(I,0,M),
            g_read(formal_matrix(I),g_array(L)),
            assertz(line_matrix(L)),
         fail
      ;
         findall(L,line_matrix(L),A)
      ),_,fail).

% pretty print

write_matrix([]).
write_matrix([H|T]):-
   write_tab(H),
   write_matrix(T).

write_tab([]):-nl.
write_tab([H|T]):-
   write(H),write('\t'),
   write_tab(T).

% decompose in place the matrix in the global array into two matrices,
% lower diagonal and upper diagonal (the diagonal of the lower one is set to
% unity

ludcmp_matrix(A,B):-
   init_matrix(A),
   ludcmp_matrix,
   get_matrix(B).

ludcmp_matrix:-
   % Value if pivot element is zero (singular)
   TINY=1.0E-100,

   g_read(formal_m,M),
   g_read(formal_n,N),
   (
      M \= N
   ->
      throw(error('Matrix must be square for LU decomposition'))
   ;
      true
   ),
   NN is N+1,
   g_assign(formal_index,g_array(NN)),
   g_assign(formal_scaling,g_array(NN)),
   % fill implicit scaling info
   (
      for(I,0,N),
         g_assign(formal_tmp,0),
         (
            for(J,0,N),
               g_read(formal_matrix(I,J),T2),
               TMP is abs(T2),
               g_read(formal_tmp,T1),
               (
                  TMP > T1
               ->
                  g_assign(formal_tmp,TMP)
               ;
                  true
               ),
            fail
         ;
            true
         ),
         g_read(formal_tmp,TMP),
         (
            TMP =:= 0
         ->
            throw(error(singular_matrix))
         ;
            true
         ),
         T3 is 1/TMP,
         g_assign(formal_scaling(I),T3),
      fail
   ;
      true
   ),         
   (
      for(J,0,N),
         (
            g_assign(formal_tmp,0),
            for(I,0,N),
            %format("computing (~w,~w)~n",[I,J]),
               (
                  % I=<J
                  I<J
               ->
                  matrix_sum(I,I,J,S),
                  g_assign(formal_matrix(I,J),S)
               ;
                  matrix_sum(J,I,J,S),
                  g_assign(formal_matrix(I,J),S),
                  g_read(formal_scaling(I),VV),
                  TMP is abs(S)*VV,
                  g_read(formal_tmp,T1),
                  (
                     TMP > T1
                  ->
                     g_assign(formal_tmp,TMP),
                     g_assign(formal_imax,I)
                  ;
                     true
                  )
               ),
            fail
         ;
            true
         ),
         g_read(formal_imax,IMAX),
         %format("for ~w max line is ~w~n",[J,IMAX]),
         (
            J \= IMAX
         ->
            % we need to interchange rows
            (
               for(II,0,N),
                  g_read(formal_matrix(IMAX,II),DUM),
                  g_read(formal_matrix(J,II),TMP),
                  g_assign(formal_matrix(IMAX,II),TMP),
                  g_assign(formal_matrix(J,II),DUM),
               fail
            ;
               true
            ),
            g_read(formal_scaling(J),VV),
            g_assign(formal_scaling(IMAX),VV)
         ;
            true
         ),
         g_assign(formal_index(J),IMAX),
         g_read(formal_matrix(J,J),AJJ),
         (
            AJJ =:= 0
         ->
            % don't keep a zero here, we will divide by it!
            AAJJ = TINY,
            g_assign(formal_matrix(J,J),AAJJ)
         ;
            AAJJ = AJJ
         ), 
         (
            J < N
         ->
            JJ is J+1,
            (
               for(II,JJ,N),
                  g_read(formal_matrix(II,J),AIJ),
                  AAIJ is AIJ/AAJJ,
                  g_assign(formal_matrix(II,J),AAIJ),
               fail
            ;
               true
            )
         ;
            true
         ),        
      fail
   ;
      true
   ).

% S is M(I,J) - the sum for K between 0 and N-1 of M(I,K)M(K,J)

matrix_sum(N,I,J,S):-
   %format("summing for ~w,~w up to ~w~n",[I,J,N]),
   g_read(formal_matrix(I,J),A),
   matrix_sum_rec(0,N,I,J,A,S).


matrix_sum_rec(N,N,_,_,S,S):-!.
matrix_sum_rec(K,N,I,J,SS,S):-
   g_read(formal_matrix(I,K),A),
   g_read(formal_matrix(K,J),B),
   SSS is SS - A*B,
   KK is K+1,
   matrix_sum_rec(KK,N,I,J,SSS,S).

% compute A.B

matrix_prod(A,B,C):-
   transpose(B,BB),
   matrix_prod_rec(A,BB,C).

matrix_prod_rec([],_,[]).
matrix_prod_rec([L|A],B,[H|T]):-
   matrix_prod_sub(L,B,H),
   matrix_prod_rec(A,B,T).

matrix_prod_sub(_,[],[]).
matrix_prod_sub(L,[C|CC],[P|PP]):-
   scalar_prod(L,C,P),
   matrix_prod_sub(L,CC,PP).

scalar_prod([],[],0).
scalar_prod([X|A],[Y|B],S):-
   scalar_prod(A,B,SS),
   S is SS + X*Y.

% apply A to vector B

matrix_apply([],_,[]).
matrix_apply([A|M],B,[S|C]):-
   scalar_prod(A,B,S),
   matrix_apply(M,B,C).
   
% A+B

matrix_sum([],[],[]).
matrix_sum([LA|A],[LB|B],[LC|C]):-
   line_sum(LA,LB,LC),
   matrix_sum(A,B,C).

line_sum([],[],[]).
line_sum([A|LA],[B|LB],[C|LC]):-
   C is A+B,
   line_sum(LA,LB,LC).

% A-B

matrix_sub([],[],[]).
matrix_sub([LA|A],[LB|B],[LC|C]):-
   line_sub(LA,LB,LC),
   matrix_sub(A,B,C).

line_sub([],[],[]).
line_sub([A|LA],[B|LB],[C|LC]):-
   C is A-B,
   line_sub(LA,LB,LC).

% Create a diagonal matrix M of size N with value V

fill_diag_matrix(N,V,M):-
   g_assign(formal_matrix,g_array(N,g_array(N))),
   NN is N-1,
   g_assign(formal_n,NN),
   g_assign(formal_m,NN),
   (
      for(I,0,NN),
         g_assign(formal_matrix(I,I),V),
      fail
   ;
      true
   ),
   get_matrix(M).

fill_diag_matrix(N,V):-
   g_assign(formal_matrix,g_array(N,g_array(N))),
   NN is N-1,
   g_assign(formal_n,NN),
   g_assign(formal_m,NN),
   (
      for(I,0,NN),
         g_assign(formal_matrix(I,I),V),
      fail
   ;
      true
   ).

% substract from the current formal_matric

sub_matrix([H|T]):-
   sub_matrix([H|T],0).

sub_matrix([],_).
sub_matrix([H|T],I):-
   sub_matrix_rec(H,I,0),
   II is I+1,
   sub_matrix(T,II).

sub_matrix_rec([],_,_).
sub_matrix_rec([H|T],I,J):-
   g_read(formal_matrix(I,J),IJ),
   A is IJ-H,
   g_assign(formal_matrix(I,J),A),
   JJ is J+1,
   sub_matrix_rec(T,I,JJ).

% Transpose A (uses formal_matrix!)

transpose(A,B):-
   transpose_matrix(A),
   get_matrix(B).

transpose_matrix([H|T]):-
   length([H|T],N),
   length(H,M),
   g_assign(formal_matrix,g_array(M,g_array(N))),
   MM is M-1,
   g_assign(formal_m,MM),
   NN is N-1,
   g_assign(formal_n,NN),
   fill_matrix_transposed([H|T],0).

fill_matrix_transposed([],_).
fill_matrix_transposed([H|T],I):-
   fill_matrix_transposed_rec(H,I,0),
   II is I+1,
   fill_matrix_transposed(T,II).

fill_matrix_transposed_rec([],_,_).
fill_matrix_transposed_rec([H|T],I,J):-
   g_assign(formal_matrix(J,I),H),
   JJ is J+1,
   fill_matrix_transposed_rec(T,I,JJ).

% use LU decomposition to solve Ax=b by fwd/backwd substitution
% supposing A has already been decomposed using the above predicate

lusubst(B,X):-
   g_read(formal_n,N),
   g_assign(formal_vector,g_array(B)),
   g_assign(formal_sum,0),
   (
      for(I,0,N),
         g_read(formal_index(I),IP),
         g_read(formal_vector(IP),Y),
         g_assign(formal_sum,Y),
         g_read(formal_vector(I),TMP),
         g_assign(formal_vector(IP),TMP),
         II is I-1,
         (
            for(J,0,II),
               g_read(formal_sum,S),
               g_read(formal_matrix(I,J),A),
               g_read(formal_vector(J),YY),
               SS is S - A*YY,
               g_assign(formal_sum,SS),
               %format("sum at (~w,~w) ~w -> ~w~n",[I,J,S,SS]),
            fail
         ;
            g_read(formal_sum,S),
            g_assign(formal_vector(I),S)
         ),
      fail
   ;
      true
   ),
   %(
      %for(I,0,N),
         %g_read(formal_vector(I),Y),
         %format("y(~w) = ~w~n",[I,Y]),
      %fail
   %;
      %true
   %),
   (
      g_read(formal_vector(N),Y),
      g_read(formal_matrix(N,N),BB),
      YY is Y/BB,
      g_assign(formal_vector(N),YY),
      %format("x(~w) = ~w~n",[N,YY]),
      fail
   ;
      true
   ),
   (
      for(I,1,N),
         g_assign(formal_sum,0),
         II is N-I,
         III is II+1,
         (
            for(J,III,N),
               g_read(formal_sum,S),
               g_read(formal_matrix(II,J),A),
               g_read(formal_vector(J),Y),
               SS is S + A*Y,
               g_assign(formal_sum,SS),
               %format("sum at (~w,~w) ~w -> ~w~n",[II,J,S,SS]),
            fail
         ;
            g_read(formal_vector(II),Y),
            g_read(formal_sum,S),
            g_read(formal_matrix(II,II),BB),
            YY is (Y - S)/BB,
            g_assign(formal_vector(II),YY)
         ),
      fail
   ;
      true
   ),
   g_read(formal_vector,g_array(X)).
         
