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
% GNU prolog file ctl.pl by Fran�ois Fages

%%% A CTL model checker for state transition systems in Prolog
% Can be used on systems generated from BIOCHAM by export_prolog
% currently restricted to flat (non imbricated) CTL formulae without X
% plus some ad hoc CTL* treatment of oscillation properties

% assumes the following facts and clauses :
% dimension(N).
% state(name1,...,nameN). where nameN=state(...) recursively for arity>=255
% initial(state(v1,...,vN)).
% transition(state(v1,...,vN),state(w1,...,wN)).
% ctl_spec(formula).

% the values of a variable in a state can be
% 0 absent
% 1 present
% a absent or present as wished (for variables in Ei initial state)
% c absent or present unknown (for variables in Ai initial state)

% evaluates CTL queries by computing a frontier of forward and backward states 
% where a state S is given with its instance SI obtained by replacing variables by constant a
% fw(S,SI)
% bw(S,SI)

:- include(largefunctor).

:- dynamic(initial/1). % initial state
:- dynamic(fw/2). % forward reachable states
:- dynamic(bw/2). % backward reachable states
:- dynamic(fm/1). % forward mask
:- dynamic(bm/1). % backward mask

%%% TODO
% boolean constraints on states
% imbrication ? normal forms ?
% algo for CTL* ?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checking of a complete CTL specification given with facts ctl(F)

check_all:-
    ctl(F),
    (ctl_check(F)
     ->
     write(F),write(' is true.'),nl,fail
     ;
     write(F),write(' is false.'),nl
     ).

ctl_init:-
    retractall(fm(_)),
    retractall(bm(_)),
    retractall(fw(_,_)),
    retractall(bw(_,_)).

% Forward analysis : compute the reachable states from S

forward_reachable:-
    initial(S),
    forward_reachable(S).

forward_reachable(S):-
    ctl_init,
    state_instance(S,SI),
    asserta(fw(S,SI)),
    asserta(fm(_)),
    fwEU.
forward_reachable(_):-
    retractall(fw(stop,stop)),
    fw(_,S),
    write(S),nl,
    fail.

% Backward analysis : compute the predecessor states of S

backward_reachable:-
    initial(S),
    backward_reachable(S).

backward_reachable(S):-
    ctl_init,
    state_instance(S,SI),
    asserta(bw(S,SI)),
    asserta(bm(_)),
    bwEU.
backward_reachable(_):-
    retractall(bw(stop,stop)),
    bw(_,S),
    write(S),nl,
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% replaces the variables in a state by constant a

state_instance(S,SI):-
	dimension(N),
	large_functor(SI,state,N),
	large_map(S,SI,1,N,var_a).

var_a(X,a):-
	var(X),
	!.
var_a(X,X).

state_instance_c(S,SI):-
	dimension(N),
	large_functor(SI,state,N),
	large_map(S,SI,1,N,var_c).

var_c(X,c):-
	var(X),
	!.
var_c(X,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CTL model-checking with Ei (for some initial state) and Ai (for all)

ctl_check('Ei'(F)):- !,
    ctl_init,
    initial(S),
    state_instance(S,SI),
    asserta(fw(S,SI)),
    ctl_form(F).

ctl_check('Ai'(F)):- !,
    ctl_init,
    initial(S),
    state_instance_c(S,SI), % Skolem constant c (could be 0 for monotonic transition systems)
    asserta(fw(SI,SI)),
    ctl_form(F).

ctl_check(F):-
        (
                have_gui
        ->
                format("[GUI] warnings No prefix for some initial state assumed Ei(~w).~n",[F])
        ;
                write('Warning : no prefix, "for some initial state" assumed '),write('Ei'(F)),nl
        ),
   
    ctl_check('Ei'(F)).

% copy operation on bw -> bw2

copy_bw:-
	retractall(bw2(_,_)),
	bw(X,Y),
	assertz(bw2(X,Y)),
	fail.
copy_bw:-
        retractall(bw(_,_)).

% CTL* formulas
% Oscillation property approximated in CTL as AG(!P->EF(P) & P->EF(!P)
% expressed here in CTL* as EG(F(!P)&F(P))
% by a forward EG computation with mask F(alternation on P) computed backward

%ctl_form('AG'('&'('->'(F,'EF'('!'(F))),'->'('!'(F),'EF'(F))))):- !,
%    ctl_form(oscil(F)).

ctl_form(oscil(F)):- !,
   ctl_form(loop(F,'!'(F))).

%ctl_form(oscil(F)):- !, 
%    ctl_state(F,S),
%    ctl_state('!'(F),T),
%    forall(transition(S,T),add_checkbw(S)), % alternation F -> !F
%    forall(transition(T,S),add_checkbw(T)), % alternation !F -> F
% FAUX !!!! prendre le et des F( alternation) pas le ou
%    fw(A,B),
%    retractall(fw(_,_)), % hack the initial state not to use it as mask in bwEU
%    asserta(bm(_)),
%    (bwEU;true), % complete set of states leading to one alternation of F
%    retractall(bw(stop,stop)),
%    asserta((fm(X):-bw(X,_),!)), % backward states used as mask
%    retractall(fw(_,_)),
%    asserta(fw(A,B)), % restores the initial state
%    fwEG.

% Loop property between two states X and Y
% approximated in CTL as AG(X->EF(Y) & Y-> EF(X))

ctl_form(loop(F,G)):- !,
    %ctl_form('AG'('&'('->'(F,'EF'(G)),'->'(G,'EF'(F))))).
    ctl_state(F,S),
    ctl_state(G,T),
    fw(A,B),
    retractall(fw(_,_)), % hack the initial state not to use it as mask in bwEU
    add_checkbw(S),
    asserta(bm(_)),
    (bwEU;true), % complete set of states leading to F
    retractall(bw(stop,stop)),
    copy_bw, 
    add_checkbw(T),
    asserta(bm(_)),
    (bwEU;true), % complete set of states leading to G
    retractall(bw(stop,stop)),
    asserta((fm(X):-bw(X,_),bw2(X,_),!)), % conjunction of backward states used as mask
    retractall(fw(_,_)),
    asserta(fw(A,B)), % restores the initial state
    fwEG.% approximation as can stay on a path which may lead to F and G without going there

ctl_form(reachable(F)):- !,
    ctl_form('EF'(F)).

ctl_form(stable(F)):- !,
    ctl_form('AG'(F)).

ctl_form(steady(F)):- !,
    ctl_form('EG'(F)).

ctl_form('AG'('->'('!'(Y),checkpoint(X,Y)))):- !,
        (
                have_gui
        ->
                format("[GUI] warnings Formula AG(!Y -> checkpoint(X,Y)) treated as checkpoint(X,Y).~n",[])
        ;
                write('Warning : formula AG(!Y -> checkpoint(X,Y)) treated as checkpoint(X,Y)'),nl
        ),
    
    ctl_form(checkpoint(X,Y)).

ctl_form(checkpoint(X,Y)):- !,
    \+ ctl_form('E'('U'('!'(X),Y))).

ctl_form('EF'(F)):- !,
    ctl_form('E'('U'(true,F))).
%    ctl_state(F,S),
%    state_instance(S,SI),
%    asserta(bw(S,SI)),
%    fwbwEF. % better than fwEF

ctl_form('EG'(F)):- !,
%    ctl_form('E'('W'('!'(F),false))).
    ctl_state(F,S),
    asserta(fm(S)),
    fwEG.

ctl_form('E'('U'(F,G))):- !,
     ctl_state(G,T),
     state_instance(T,TI),
    (ctl_state(F,S)  % not false
     ->
     asserta(fm(S)), % forward mask
     asserta(bm(S)), % backward mask as forward mask !
     asserta(bw(T,TI)), 
     fwbwEU
     ;
     fw(TI,_)). % G true in the initial state

ctl_form('E'('W'(F,G))):- !,
    ctl_state(F,S), % failure if false
    asserta(fm(S)), % forward mask
    (ctl_state(G,T)
     ->
     state_instance(T,TI),
     asserta(bw(T,TI)),
     S=T,            % backward mask as the conjunction of fw bw masks
     asserta(bm(S)),
     fwEW            % should better use fwbwEW.
     ;
     fwEG).

ctl_form('AF'(F)):- !,
    \+ ctl_form('EG'('!'(F))).

ctl_form('AG'(F)):- !,
    \+ ctl_form('EF'('!'(F))).

% 'X' and imbricated formulas not implemented yet
ctl_form(F):-
    ctl_state(F,S),
    fw(S,_). % success if true in initial state


% Boolean formula over states

ctl_state('&'(A,B),S):- !,
    ctl_state(A,S),
    ctl_state(B,S).

ctl_state('!'(A),S):- !,
    ctl_state(A,T),
    xor_state(T,S).

ctl_state(true,S):- !,
    dimension(N), 
    large_functor(S,state,N).

% ctl_state(false,_):- !, fail.
ctl_state(false,false):- !. 

ctl_state(I,S):- integer(I), !,
    dimension(N), 
    large_functor(S,state,N),
    large_arg(I,S,1).

ctl_state(F,_):-
    (
                have_gui
        ->
                format("[GUI] warnings Formula not implemented ~w.~n",[F])
        ;
                 write('Warning : formula not implemented '),write(F),nl
    ),
   
    fail.

% exchange 0 and 1 leaving variables in states

xor_state(S,T):-
	large_functor(S,F,N),
	large_functor(T,F,N),
	large_map(S,T,1,N,ctl_xor).

ctl_xor(V,U):-
    var(V),var(U),!,
    V=U.
ctl_xor(0,1).
ctl_xor(1,0).
ctl_xor(a,c). % not used
ctl_xor(c,a).

% forall X satisfying P do Q for side effects

forall(P,Q):- P, Q, fail.
forall(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Computation of fw and bw frontiers

% add R to forward states
add_fw(R):-
    state_instance(R,RI),
    add_fw(R,RI).

add_fw(R,RI):-
    elim_fw(R),
    asserta(fw(R,RI)).  % new states in front

% add R to forward states checking not already in
add_checkfw(R):-
    state_instance(R,RI),
    (fw(RI,_)
     ->
     true
     ;
     add_fw(R,RI)).

% eliminate the forward states subsumed by R
elim_fw(R):-
    fw(S,R), % then R subsumes S
    retractall(fw(S,_)),
    fail.
elim_fw(_).

% add R to backward states
add_bw(R):-
    state_instance(R,RI),
    add_bw(R,RI).

add_bw(R,RI):-
    elim_bw(R),
    asserta(bw(R,RI)).  % new states in front

% add R to backward states checking not already in
add_checkbw(R):-
    state_instance(R,RI),
    (bw(RI,_)
     ->
     true
     ;
     add_bw(R,RI)).

% eliminate the backward states subsumed by R
elim_bw(R):-
    bw(S,R), % then R subsumes S
    retractall(bw(S,_)),
    fail.
elim_bw(_).

% is R in backward states ?
is_bw(R):-
    state_instance(R,RI),
    bw(RI,_).

% is R in forward states ?
is_fw(R):-
    state_instance(R,RI),
    fw(RI,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Forward saturation with fm mask until reaching a bw state

fwEU:-
    fw(S,_),
    fm(S),   % initial failure if S does not unify with the mask
    fwEUa.

fwEUa:-
    fw(S,_),
    bw(S,_). % initial success if S unifies with a backward state
fwEUa:-
    fwEUb.

fwEUb:-fwEUc.
fwEUb:-
    fw(S,_),
    !,
    ((S=stop) -> fail ; fwEUb).

% assert does not impact the current goals
% hence the complication with first and stop markers...
fwEUc:-
    asserta(fw(first,first)),
    fw(S,_),
    ((S=stop)
     ->
     !, fail
     ;
     ((S=first)
      ->
      retractall(fw(first,_)),
      asserta(fw(stop,stop)),
      fail
      ;
      transition(S,R),
      fm(R),
      (bw(R,_)  % success if R unifies with a backward state
       ->
       !,
       asserta(fw(stop,stop)),
       asserta(bw(stop,stop))
       ;
       add_checkfw(R),
       fail
       )
      )
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Backward saturation with bm mask until reaching a fw state
% simply exchange fw with bw in fwEU code plus transition(R,S)

bwEU:-
    bw(S,_),
    bm(S),   % initial failure if S does not unify with the mask
    bwEUa.

bwEUa:-
    bw(S,_),
    fw(S,_). % initial success if S unifies with a forward state
bwEUa:-
    bwEUb.

bwEUb:-bwEUc.
bwEUb:-
    bw(S,_),
    !,
    ((S=stop) -> fail ; bwEUb).

bwEUc:-
    asserta(bw(first,first)),
    bw(S,_),
    ((S=stop)
     ->
     !, fail
     ;
     ((S=first)
      ->
      retractall(bw(first,_)),
      asserta(bw(stop,stop)),
      fail
      ;
      transition(R,S),
      bm(R),
      (fw(R,_)  % success if R unifies with a backward state
       ->
       !,
       asserta(fw(stop,stop)),
       asserta(bw(stop,stop))
       ;
       add_checkbw(R),
       fail
       )
      )
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Forward-backward saturation with fm and bm masks
% until reaching a common state

fwbwEU:-
    fw(S,_),
    fm(S),
    bw(T,_),
    bm(T),
    fwbwEUa.

fwbwEUa:-
    fw(S,_),
    bw(S,_). % initial success if S unifies with a backward state
fwbwEUa:-
    fwbwEUb.

fwbwEUb:-fwEUc.
fwbwEUb:-bwEUc.
fwbwEUb:-
    fw(S,_),
    bw(R,_),
    !,
    (S=stop
     ->
     (R=stop
      ->
      fail
      ;
      bwEUb)
     ;
     (R=stop
      ->
      fwEUb
      ;
      fwbwEUb)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fwEG computes a fw set with fm mask until looping in the set

fwEG:-
    fw(S,_),
    fm(S), % initial failure if the initial state does not satisfy the mask
    fwEGb.

fwEGb:-fwEGc.
fwEGb:-
    fw(S,_),
    !,
    ((S=stop) -> fail ; fwEGb).

fwEGc:-
    asserta(fw(first,first)),
    fw(S,_),
    ((S=stop)
     ->
     !, fail % no loop
     ;
     ((S=first)
      ->
      retractall(fw(first,_)),
      asserta(fw(stop,stop)),
      fail
      ;
      transition(S,R),
      fm(R),  % R needs satisfy the formula
      (fw(R,_)  % success if R creates a loop
       ->
       !, true
       ;
       add_checkfw(R), 
       fail
       )
      )
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fwEW computes a fw set satisfying fm until looping or reaching bw
% (simply replace EG by EW in previous code and add success check on bw)
% fwbwEW would be more efficient

fwEW:-
    fw(S,_),
    fm(S), % initial failure if the initial state does not satisfy the mask
    fwEWb.

fwEWb:-fwEWc.
fwEWb:-
    fw(S,_),
    !,
    ((S=stop) -> fail ; fwEWb).

fwEWc:-
    asserta(fw(first,first)),
    fw(S,_),
    ((S=stop)
     ->
     !, fail % no loop
     ;
     ((S=first)
      ->
      retractall(fw(first,_)),
      asserta(fw(stop,stop)),
      fail
      ;
      transition(S,R),
      fm(R),  % R needs satisfy the formula
      ((fw(R,_);bw(R,_))  % success if R creates a loop or reaches bw
       ->
       !, true
       ;
       add_checkfw(R), 
       fail
       )
      )
     ).

    



