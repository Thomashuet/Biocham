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
% GNU prolog file biochamPinvar.pl
% by Sylvain Soliman

:- dynamic(pinvar_done/2). % avoid useless symmetrical rules
:- dynamic(class/2).       % store parallel classes

% find the set of minimal P-invariants
find_all_pinvar :-
   fd_max_integer(MaxInt),
   find_pinvar(MaxInt).

search_conservations:- find_pinvar.
search_conservations(I):- find_pinvar(I).
find_pinvar :- find_pinvar(4).
find_pinvar(ForcedMax) :-
   statistics(cpu_time,_),
   findall((UKL,UKR),
      (
         rule(_,KL,KR,_,_,_,_),        % get the reactions as input/output
         remove_common(KL,KR,UKL,UKR)  % get the real stoechiometric coeff
      ),L),
   retractall(pinvar_done(_,_)),
   get_constraints(L,[],[],Vars,Varlist,1,MaxDomain1), % add constraints X.M=0
   MaxDomain is min(MaxDomain1, ForcedMax),
   fd_domain(Varlist,0,MaxDomain),     % add a large upper bound for minimal PI
   format_debug(1,"~w~n",[MaxDomain]),
   format_debug(3,"~w~n~w~n",[Vars,Varlist]),
   build_sum(Varlist,Sum),!,
   Sum #> 0,                           % avoid the trivial solution
   g_assign(base,[]),
   init_classes(Vars),
   find_classes,
   zero_classes(Vars,Varlist),         % zero all but one in each class
   repeat,
   (
      get_base_constr(Varlist),
      fd_labelingff(Varlist)     % does NOT guarantee a base e.g.
                                 % add_rule(2*Y + Z => 3*X). for labeling not
                                 % ff
   ->
      g_read(base,B),
      make_base(B,Varlist,BB),
      g_assign(base,BB),
      fail
   ;
      expand_base(Vars),
      g_read(base,B),
      writeall((member(VL,B),to_mols(VL,Vars,Mol),sort(Mol)), Mol),
(
                have_gui
                        ->
                                writeall_pInvariants((member(VL,B),to_mols(VL,Vars,Mol),sort(Mol)), Mol)
                        ;
                                true
               )
   ),!,
   % get singletons too
   nusmv_search_molecules,
   writeall((molecule(M,_),\+(member(M,Vars))),[M]),
        (
                have_gui
                        ->
                                writeall_pInvariants((molecule(M,_),\+(member(M,Vars))),[M])
                        ;
                                true
               ),
   statistics(system_time,[Time,_]),
   format_debug(3,"Time: ~w ms~n",[Time]).

%% find possible parallel places (molecules)

% set everything to 0 and next to 1 
init_classes(V):-
   g_assign(next_class,1),
   retractall(class(_,_)),
   init_class_rec(V).

init_class_rec([]).
init_class_rec([H|T]):-
   assertz(class(H,0)),
   init_class_rec(T).

% find classes for each rule, left side, then right side
find_classes:-
   pinvar_done(L1,L2),
   find_classes(L1),
   find_classes(L2),
   fail.

% once over, regrou classes
find_classes:-
   g_read(next_class,N),
   do_class(0,N).

do_class(N,N):-
   !.
do_class(M,N):-
   findall(Mol,retract(class(Mol,M)),L),
   assertz(class(L,M)),
   MM is M+1,
   do_class(MM,N).

% find in a list all molecules with same stoechiometry
find_classes([]).
find_classes([(S,M)|L]):-
   find_s_classes(S,L,L1,L2),
   set_class([M|L1]),   % potentially same class
   find_classes(L2).    % other ones remain to be checked

find_s_classes(_,[],[],[]).
find_s_classes(S,[(S,M)|L],[M|L1],L2):-
   !,find_s_classes(S,L,L1,L2).
find_s_classes(S,[(SS,M)|L],L1,[(SS,M)|L2]):-
   find_s_classes(S,L,L1,L2).

% remove from classes of members of L all non member of L
set_class(L):-
   set_class(L,L).

set_class([],_).
set_class([M|C],L):-
   class(M,CM),
   findall(Mol,(class(Mol,CM),\+(memberchk(Mol,L))),ML),
   change_class(ML),
   (
      ML=[]
   ->
      true
   ;
      g_read(next_class,NC),
      NNC is NC+1,
      g_assign(next_class,NNC)
   ),
   set_class(C,L).

change_class([]).
change_class([H|T]):-
   retractall(class(H,_)),
   g_read(next_class,N),
   assertz(class(H,N)),
   change_class(T).

% put constraint taht all non first members are at 0
zero_classes(Vars,Varlist):-
   findall(C,class(C,_),L),
   zero_classes(L,Vars,Varlist).

zero_classes([],_,_).
zero_classes([C|CL],Vars,Varlist):-
   C = [_|T],
   zero_rec(T,Vars,Varlist),
   zero_classes(CL,Vars,Varlist).

zero_rec([],_,_).
zero_rec([M|L],Vars,Varlist):-
   nth(N,Vars,M),
   nth(N,Varlist,V),
   V=0,
   zero_rec(L,Vars,Varlist).

expand_base(Vars):-
   g_read(next_class,N),
   expand_base(0,N,Vars).

expand_base(N,N,_) :- !.
expand_base(M,N,Vars):-
   class(L,M),
   length(L,Len),
   (
      Len > 1
   ->
      g_read(base,B),
      add_to_base(L,B,Vars)
   ;
      true
   ),
   MM is M+1,
   expand_base(MM,N,Vars).

add_to_base(_,[],_).
add_to_base(C,[B|L],Vars):-
   C = [H|T],
   nth(N,Vars,H),
   nth(N,B,ValH),
   (
      ValH = 0
   ->
      true
   ;
      add_to_base(T,B,Vars,H,ValH)
   ),
   add_to_base(C,L,Vars).

add_to_base([],_,_,_,_).
add_to_base([M|L],B,Vars,H,ValH):-
   add_to_base(M,B,Vars,H,ValH,V),
   g_read(base,Base),
   g_assign(base,[V|Base]),
   add_to_base(L,B,Vars,H,ValH).

add_to_base(_,[],[],_,_,[]).
add_to_base(M,[0|B],[M|Vars],H,ValH,[ValH|V]):-
   !,add_to_base(M,B,Vars,H,ValH,V).
add_to_base(M,[ValH|B],[H|Vars],H,ValH,[0|V]):-
   !,add_to_base(M,B,Vars,H,ValH,V).
add_to_base(M,[VB|B],[_|Vars],H,ValH,[VB|V]):-
   !,add_to_base(M,B,Vars,H,ValH,V).

% From a list of rules get involved molecules, associate a variable list
% and the product of highest coefficients (for upper bound)

get_constraints([],Vars,Varlist,Vars,Varlist,MaxDomain,MaxDomain).

get_constraints([(KL,KR)|L],Vars,Varlist,NVars,NVarlist,MaxDomain,NMaxDomain):-
   pinvar_done(KR,KL),!,
   get_constraints(L,Vars,Varlist,NVars,NVarlist,MaxDomain,NMaxDomain).

get_constraints([(KL,KR)|L],Vars,Varlist,NVars,NVarlist,MaxDomain,NMaxDomain):-
   assertz(pinvar_done(KL,KR)),
   make_sum(KL,SL,Vars,Varlist,Vars1,Varlist1,Max1),
   make_sum(KR,SR,Vars1,Varlist1,Vars2,Varlist2,Max2),
   MaxDomain3 is MaxDomain*max(Max1,Max2),   % set upper bound
   (
      MaxDomain3 < 0 % overflow :(
   ->
      fd_max_integer(N),
      MaxDomain2=N
   ;
      MaxDomain2=MaxDomain3
   ),
   SL #= SR,   % add constraint X.M = O
   format_debug(3,"~w~n",[SL #= SR]),
   format_debug(3,"Max:~w = ~w*max(~w,~w)~n~w~n",
      [MaxDomain2,MaxDomain,Max1,Max2,SL #= SR]),
   get_constraints(L,Vars2,Varlist2,NVars,NVarlist,MaxDomain2,NMaxDomain).

% get molecule and variable list with maximum coeff
make_sum([],0,V,VL,V,VL,0).
make_sum([(S,M)],VV,Vars,Varlist,Vars1,Varlist1,S):-
   (
      S=1
   ->
      VV=V
   ;
      VV=S*V
   ),
   (
      nth(N,Vars,M)
   ->
      nth(N,Varlist,V),
      Vars1=Vars,
      Varlist1=Varlist
   ;
      fd_max_integer(MaxInt),
      fd_domain(V,0,MaxInt),
      Vars1=[M|Vars],
      Varlist1=[V|Varlist]
   ),!.
make_sum([(S,M)|L],VV+Sum,Vars,Varlist,Vars1,Varlist1,Max):-
   (
      S=1
   ->
      VV=V
   ;
      VV=S*V
   ),
   (
      nth(N,Vars,M)
   ->
      nth(N,Varlist,V),
      Vars2=Vars,
      Varlist2=Varlist
   ;
      fd_max_integer(MaxInt),
      fd_domain(V,0,MaxInt),
      Vars2=[M|Vars],
      Varlist2=[V|Varlist]
   ),
   make_sum(L,Sum,Vars2,Varlist2,Vars1,Varlist1,Max1),
   Max is Max1+S,!.

% replace variables by the corresponding molecule

to_mols([],[],[]).
to_mols([0|T],[_|Vars],Mols):-
   !,to_mols(T,Vars,Mols).
to_mols([1|T],[V|Vars],[V|Mols]):-
   !,to_mols(T,Vars,Mols).
to_mols([N|T],[V|Vars],[N*V|Mols]):-
   to_mols(T,Vars,Mols).

% Add constraints that no base vector should be overlapped

get_base_constr(Varlist):-
   g_read(base,B),
   format_debug(3,"Base: ~w~n",[B]),
   get_base_constr_rec(B,Varlist),
   format_debug(3,"got constr...~n",[]).

get_base_constr_rec([],_).
get_base_constr_rec([B|BL],Varlist):-
   build_prod(B,Varlist,_Prod,Supp,_NotSupp),
   % stupid fd_solver explodes if the product can be over maxint :(
   /* fd_max_integer(MaxInt),
   fd_vector_max(MaxVect),
   length(Supp,N),
   MaxDom is floor(MaxInt**(1/N)),
   (
      MaxDom > MaxVect
   ->
      fd_domain(Supp,0,MaxDom),
      format_debug(3,"~w~n",[Prod #= 0]),
      Prod #= 0
   ;
      fd_domain(Supp,0,MaxVect),
      format_debug(3,"~w~n",[Prod #=# 0]),
      Prod #=# 0         % Linux segmentation violation :(
   ),*/
   fd_atleast(1,Supp,0),   % useless :(
   %%% build_sum(NotSupp,Sum),!,  % [redundant] Support of a new vector cannot
   %%% Sum #> 0,                  % be included in that of base vect
   %%% format_debug(3,"~w~n",[Sum #> 0]),  % Linux segm violation :(
   %%% format_debug(3,"member(0,~w)~n",[Supp]),
   %%% member(0,Supp),           % too much branching :(
   get_base_constr_rec(BL,Varlist).

build_prod([],[],1,[],[]).
build_prod([0|B],[V|Varlist],Prod,S,[V|NS]):-
   !,build_prod(B,Varlist,Prod,S,NS).
build_prod([_|B],[V|Varlist],Prod*V,[V|S],NS):-
   build_prod(B,Varlist,Prod,S,NS).


% remove in first args vectors subsumed by second arg, then add it
make_base([],V,[V]).
make_base([H|T],V,B):-
   (
      bigger_support(H,V)
   ->
      format_debug(3,"removing a vector from the base~n",[]),
      make_base(T,V,B)
   ;
      make_base(T,V,B1),
      B = [H|B1]
   ).

bigger_support([],[]).
bigger_support([_|V],[0|B]):-
   !,bigger_support(V,B).
bigger_support([N|V],[_|B]):-
   !,N>0,
   bigger_support(V,B).
