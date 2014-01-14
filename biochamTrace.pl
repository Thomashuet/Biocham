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
% GNU prolog file biochamTrace.pl
% by Sylvain Soliman
% modified by Francois Fages for oscil(F,N,V)

% store true formulae for a set of param values
:- dynamic(trace_state/2).
% store values of maxima for period search
:- dynamic(t_same_val/1).
% store LTL specs *without* duration (i.e. only LTL formula)
:- dynamic(t_spec/1).

ensure_trace:-
   (
      k_value(_,_,_)
   ;
      format_debug(0,"Generating trace, please wait...\n",[]),
        (
                have_gui
        ->
                format("[GUI] checkLTL Generating trace, please wait...~n",[])
        ;
                true
        ),
      numerical_simulation
   ).
   
check_ltl(E):- trace_check(E).

trace_check(E):-
   % first get a trace
   ensure_trace,!,
   trace_normalize(E,EE),!,
   trace_tag(EE),!,
   k_value(0,_,L),
   format_debug(0,"~p is ",[E]),
   (
      memberchk(EE,L)
   ->
      format_debug(0,"true.\n",[]),
      (
         have_gui
      ->
         format("[GUI] checkLTL ~p is true.\n",[E])
      ;
         true
      )

   ;
      format_debug(0,"false.\n",[]),
      (
         have_gui
      ->
         format("[GUI] checkLTL ~p is false.\n",[E])
      ;
         true
      ),
      fail
   ).

% Shortcut for oscillations
trace_normalize(oscil(X,N),O) :-
   !,
   %make_oscil(X,N,OO),
   %trace_normalize(OO,O).
   parse_object(X,XX),
   compute_derivatives(XX),
   O = oscil(XX,N).

trace_normalize(oscil(X,N,W),O) :-
   !,
   parse_object(X,XX),
   compute_derivatives(XX),
   O = oscil(XX,N,W).

trace_normalize(period(X,P),period(XX,P)) :-
   !,
   parse_object(X,XX),
   compute_derivatives(XX).

trace_normalize(phase_shift(X,Y,P),phase_shift(XX,YY,P)) :-
   !,
   parse_object(X,XX),
   compute_derivatives(XX),
   parse_object(Y,YY),
   compute_derivatives(YY).

% Shortcut for crossing
trace_normalize(cross(X,Y,N),O) :-
   !,make_cross(X,Y,N,OO),
   trace_normalize(OO,O).

% Time is a mcro that should be evaluated in a special way...
trace_normalize('Time',['_Time']):-!.

trace_normalize(E,E):-
   atomic(E),!.

% normalize conditions to catch what we can, unknown stuff will be caught by
% eval_condition

trace_normalize([M],[MM]):-
   !,parse_object(M,MM).

trace_normalize(d([M])/dt,d(MM)):-
   !,
   parse_object(M,MM),
   compute_derivatives(MM).

trace_normalize(A < B,AA < BB):-
   !,
   trace_normalize(A,AA),
   trace_normalize(B,BB).

trace_normalize(A > B,AA > BB):-
   !,
   trace_normalize(A,AA),
   trace_normalize(B,BB).

trace_normalize(A =< B,AA =< BB):-
   !,
   trace_normalize(A,AA),
   trace_normalize(B,BB).

trace_normalize(A >= B,AA >= BB):-
   !,
   trace_normalize(A,AA),
   trace_normalize(B,BB).

trace_normalize(A = B,AA = BB):-
   !,
   trace_normalize(A,AA),
   trace_normalize(B,BB).

trace_normalize(E,F):-
   E=..[H|T],!,
   trace_normalize_rec(T,TT),
   F=..[H|TT].

trace_normalize_rec([],[]).
trace_normalize_rec([H|T],[HH|TT]):-
   trace_normalize(H,HH),
   trace_normalize_rec(T,TT).

% forcing a new simulation with the given time  objective
trace_check(E,N) :-
   numerical_simulation(N),
   trace_check(E).

% Check if not already done
trace_tag(E):-
   k_value(_,_,L),
   memberchk(E,L),!.

trace_tag('&'(E,F)):-
   !,
   trace_tag(E),
   trace_tag(F),
   (
      k_value(T,V,L),
      memberchk(E,L),
      memberchk(F,L),
      retract(k_value(T,V,L)),
      assertz(k_value(T,V,[('&'(E,F))|L])),
      fail
   ;
      true
   ).

trace_tag('|'(E,F)):-
   !,
   trace_tag(E),
   (
      k_value(T,V,L),
      (
         memberchk(E,L)
      ;
         trace_tag(F),
         k_value(T,V,LL),
         memberchk(F,LL)
      ),
      retract(k_value(T,V,LLL)),
      assertz(k_value(T,V,['|'(E,F)|LLL])),
      fail
   ;
      true
   ).

trace_tag('xor'(E,F)):-
   !,
   trace_tag(E),
   trace_tag(F),
   (
      k_value(T,V,L),
      (
         memberchk(E,L)
      ->
         \+(memberchk(F,L))
      ;
         memberchk(F,L)
      ),
      retract(k_value(T,V,L)),
      assertz(k_value(T,V,['xor'(E,F)|L])),
      fail
   ;
      true
   ).

trace_tag('->'(E,F)):-
   !,
   trace_tag(E),
   trace_tag(F),
   (
      k_value(T,V,L),
      (
         memberchk(E,L)
      ->
         memberchk(F,L)
      ;
         true
      ),
      retract(k_value(T,V,L)),
      assertz(k_value(T,V,['->'(E,F)|L])),
      fail
   ;
      true
   ).

trace_tag('<->'(E, F)):-
   !,
   trace_tag(E),
   trace_tag(F),
   (
      k_value(T,V,L),
      (
         memberchk(E,L)
      ->
         memberchk(F,L)
      ;
         \+(memberchk(F,L))
      ),
      retract(k_value(T,V,L)),
      assertz(k_value(T,V,['<->'(E,F)|L])),
      fail
   ;
      true
   ).

trace_tag('!'(E)):-
   !,
   trace_tag(E),
   (
      k_value(T,V,L),
      \+(memberchk(E,L)),
      retract(k_value(T,V,L)),
      assertz(k_value(T,V,['!'(E)|L])),
      fail
   ;
      true
   ).

trace_tag('F'(E)):-
   !,
   trace_tag(E),
   g_read(trace_times,TL),
   trace_finally(TL,E).
   

trace_tag('G'(E)):-
   !,
   trace_tag(E),
   g_read(trace_times,TL),
   trace_globally(TL,E).

trace_tag('U'(E,F)):-
   !,
   trace_tag(E),
   trace_tag(F),
   g_read(trace_times,TL),
   trace_until(TL,E,F).

trace_tag('X'(E)):-
   !,
   trace_tag(E),
   g_read(trace_times,TL),
   trace_next(TL,E).

% Rolle theorem based equality
% could also be done in trace_translate (see commented code below)
% probably more efficient there, but more clear here :)
% TODO? use that for =< <-> < | = ?

trace_tag(A = B):-
   !,
   trace_tag('|'('&'((A >= B),'X'(A =< B)),'&'((A =< B),'X'(A >= B)))),
   findall(T,(k_value(T,_,L),
				  member('|'('&'((A >= B),'X'(A =< B)),'&'((A =< B),'X'(A >= B))),L)),
      EQL),
   mark_all(EQL,A = B).

% Optimized version

trace_tag(oscil(X, N)) :-
   k_value(0,_,L),
   member(oscil(X, M), L),!,
   (
      (M>=N)
   ->
      retract(k_value(0, V, L)),
      assertz(k_value(0, V, [oscil(X, N) | L]))
   ;
      true
   ).

trace_tag(oscil(X,N)):-
   !,
   trace_tops(X,EQL),
   length(EQL,M),
   retract(k_value(0,V,LL)),
   (
      (M >= N) 
   ->
      %(K is M - N + 1),
      %nth(K,EQL,TT),
      %g_read(trace_times,TL),
      %TTL=[TT|_],
      %suffix(TTL,TL),
      %mark_all(TTL,oscil(X,N))
      assertz(k_value(0,V,[oscil(X,N),oscil(X,M)|LL])) % keep N for memcheck
   ;
      assertz(k_value(0,V,[oscil(X,M)|LL]))
   ).

trace_tag(oscil(X, N, V)) :-
   k_value(0,_,L),
   member(oscil(X, M, W), L),!,
   (
      (M>=N, W>=V)
   ->
      retract(k_value(0, VV, L)),
      assertz(k_value(0, VV, [oscil(X, N, V) | L]))
   ;
      true
   ).

trace_tag(oscil(X,N,W)):- %FF mininimum max value for the oscillations
   !,
   trace_top_values(X,EQL),
   length(EQL,M,W),
   retract(k_value(0,V,LL)),
   (
      (M >= N) 
   ->
      assertz(k_value(0,V,[oscil(X,N,W),oscil(X,M,W)|LL]))
   ;
      assertz(k_value(0,V,[oscil(X,M,W)|LL]))
   ).

trace_tag(period(X, P)) :-
   k_value(0, _, L),
   member(period(X, Q), L),!,
   (
      almost_equal(P, Q, 2)
   ->
      retract(k_value(0, V, L)),
      assertz(k_value(0, V, [period(X, P) | L]))
   ;
      true
   ).

trace_tag(period(X,P)):-
   !,
   %trace_tops(X,L),
   lazy_trace_tops(X,3,L),
   length(L,M),
   format_debug(5,"tops: ~p~n",[L]),
   retract(k_value(0,V,LL)),
   (
      reverse(L,[T2,T1,T0|_]),!,
      P2 is T2-T1,
      P1 is T1-T0,
      almost_equal(P2,P,2),
      almost_equal(P1,P,2),
      assertz(k_value(0,V,[period(X,P),oscil(X,M)|LL]))
   ;
      assertz(k_value(0,V,[oscil(X,M)|LL]))
   ).
   
trace_tag(phase_shift(X,Y,P)):-
   !,
   lazy_trace_tops(X,4,LX),!,
   reverse(LX,XL),
   format_debug(1,"tops ~p: ~p~n",[X,XL]),
   lazy_trace_tops(Y,3,LY),!,
   reverse(LY,YL),
   format_debug(1,"tops ~p: ~p~n",[Y,YL]),
   (
      (XL = [XH|XT]),
      (YL = [Y1,Y2,Y3]),
      XX is XH+P,
      (
         almost_equal(XX,Y1,4)
      ->
         (XL = [X1,X2,X3|_])
      ;
         (XT = [X1,X2,X3])
      ),
      E1 is Y1-X1,
      E2 is Y2-X2,
      E3 is Y3-X3,
      almost_equal(E1,P,4),
      almost_equal(E2,P,4),
      almost_equal(E3,P,4),
      retract(k_value(0,V,LL)),
      assertz(k_value(0,V,[phase_shift(X,Y,P)|LL]))
   ;
      true
   ).
   
trace_tag(E):-
   k_value(T,V,L),
   (
      test_conditions(E,[(['_Time'],T)|V])
   ->
      retract(k_value(T,V,L)),
      assertz(k_value(T,V,[E|L]))
   ;
      true
   ),
   fail.

trace_tag(_).

length([],0,_).
length([(_,V)|L],N,W):-
   length(L,LN,W),
   (
      (V >= W)
   ->
      (N is LN+1) 
   ;
      N is LN
   ).

% remove all tagging except derivatives information
trace_clean:-
   g_read(trace_times,TL),
   trace_clean(TL).

trace_clean([]).
trace_clean([T|TL]):-
   retract(k_value(T,V,E)),
   trace_clean2(E,EE),
   assertz(k_value(T,V,EE)),
   trace_clean(TL).

trace_clean2([],[]).
trace_clean2([(d(M)<0)|L1],[(d(M)<0)|L2]):-
   !,trace_clean2(L1,L2).
trace_clean2([(d(M)>0)|L1],[(d(M)>0)|L2]):-
   !,trace_clean2(L1,L2).
trace_clean2([(d(M)=0)|L1],[(d(M)=0)|L2]):-
   !,trace_clean2(L1,L2).
trace_clean2(['X'(d(M)<0)|L1],['X'(d(M)<0)|L2]):-
   !,trace_clean2(L1,L2).
trace_clean2(['X'(d(M)>0)|L1],['X'(d(M)>0)|L2]):-
   !,trace_clean2(L1,L2).
trace_clean2(['X'(d(M)=0)|L1],['X'(d(M)=0)|L2]):-
   !,trace_clean2(L1,L2).
trace_clean2([_|L1],L2):-
   trace_clean2(L1,L2).
   
% Get points where the derivative goes from + to -
trace_tops(X,OL):-
   trace_tag('&'(d(X)>0,'X'(d(X)<0))),
   findall(T,(
      k_value(T,_,L),
      memberchk('&'(d(X)>0,'X'(d(X)<0)),L)),OL),
   sort(OL).

trace_top_values(X,OL):-
   trace_tag('&'(d(X)>0,'X'(d(X)<0))),
	findall((T,V),(
						k_value(T,LV,L),
						memberchk('&'(d(X)>0,'X'(d(X)<0)),L),
						member(([X],V),LV)),OL),
										  % FIXME should check t_same_val...
   sort(OL).

trace_finally([],_).

trace_finally([H|T],E):-
   k_value(H,_,L),
   (
      memberchk(E,L)
   ->
      mark_all([H|T],'F'(E))
   ;
      trace_finally(T,E)
   ).

% G implemented as:
% next(E) is TRUE whatever E for the last step
% start from last, and while E true G(E) true...

trace_globally([],_).

trace_globally([H|T],E):-
   k_value(H,_,L),
   (
      memberchk(E,L)
   ->
      retract(k_value(H,V,L)),
      assertz(k_value(H,V,['G'(E)|L])),
      trace_globally(T,E)
   ;
      true
   ).

% X implemented as:
% next is FALSE for the last step
% for the others, just check...

trace_next([],_).
trace_next([_],_).
trace_next([H,N|T],E):-
   k_value(H,_,L),
   (
      memberchk(E,L)
   ->
      (
         retract(k_value(N,V,M)),
         assertz(k_value(N,V,['X'(E)|M]))
      )
   ;
      true
   ),
   trace_next([N|T],E).

% Find backwards a state such that F is true
% then mark all preceding states while E (or F) is true

trace_until([],_,_).

trace_until([H|T],E,F):-
   k_value(H,_,L),
   (
      memberchk(F,L)
   ->
      (
         retract(k_value(H,V,L)),
         assertz(k_value(H,V,['U'(E,F)|L])),
         trace_until2(T,E,F)
      )
   ;
      trace_until(T,E,F)
   ).
     
trace_until2([],_,_).

trace_until2([H|T],E,F):-
   k_value(H,_,L),
   (
      (
         memberchk(E,L),!
      ;
         memberchk(F,L)
      )
   ->
      (
         retract(k_value(H,V,L)),
         assertz(k_value(H,V,['U'(E,F)|L])),
         trace_until2(T,E,F)
      )
   ;
      trace_until(T,E,F)
   ).
                        

   
mark_all([],_).

mark_all([H|T],E):-
   retract(k_value(H,V,L)),
   assertz(k_value(H,V,[E|L])),
   mark_all(T,E).

%make_oscil(X,N,'&'('F'((d([X])/dt > 0), 'F'(d([X])/dt < 0)))):-
%   N =< 1,!.

%make_oscil(X,N,'&'('F'((d([X])/dt > 0), 'F'((d([X])/dt < 0) & O)))):-
%   M is N-1,
%   make_oscil(X,M,O).

make_cross(X,Y,N,'X'('F'([X] = [Y]))):-
   N =< 1,!.

make_cross(X,Y,N,'X'('F'('&'([X] = [Y],O)))):-
   M is N-1,
   make_cross(X,Y,M,O).


% recursively check all lists in second argument for containing first arg

memberchk_all(_,[]).

memberchk_all(E,[L|LL]):-
   memberchk(E,L),
   memberchk_all(E,LL).

% compute a molecule's derivative at all times

compute_derivatives(M):-
   g_read(trace_times,TL),
   compute_derivatives(M,TL).

compute_derivatives(_,[]).

% derivative is supposed null at first instant

compute_derivatives(MM,[0]):-
   retract(k_value(0,V,E)),
   assertz(k_value(0,[(d(MM),0)|V],E)).

compute_derivatives(MM,[T,TPrec|TL]):-
   retract(k_value(T,V,E)),
   (
      member((d(MM),_),V)  % stop as soon as we have something already
                           % computed
   ->
      (
         format_debug(4,"Found already computed d(~w) at ~p~n",[MM,T]),
         (
            have_gui
         ->
            format("[GUI] checkLTL Found already computed d(~w) at ~p~n",[MM,T])
         ;
            true
         ),
         assertz(k_value(T,V,E))
      )
   ;
      retract(k_value(TPrec,VPrec,EPrec)),
      member(([MM],MPrec),VPrec),
      member(([MM],M),V),
      D is (M-MPrec)/(T-TPrec),
      (                 % store info about d(MM)'s sign for T and TPrec
         (D>0)          % to improve oscil and period calculations
      ->
         (EE=(d(MM)>0))
      ;  
         (
            (D<0)
         ->
            (EE=(d(MM)<0))
         ;
            (EE=(d(MM)=0))
         )
      ),
      assertz(k_value(T,[(d(MM),D)|V],[EE|E])),
      assertz(k_value(TPrec,VPrec,['X'(EE)|EPrec])),
      compute_derivatives(MM,[TPrec|TL])
   ).

% Check equality at N%

almost_equal(A,B,N):-
   abs((B-A)/A) < (N/100).

% Lazy version of trace_tops: only look for at most N tops and don't store
% info about d(M)

lazy_trace_tops(M,N,L):-
   g_read(trace_times,TL),
   retractall(t_same_val(_)),
   lazy_trace_tops(M,N,TL,L1),
   reverse(L1,L).

lazy_trace_tops(_,_,[],[]):-!.
lazy_trace_tops(_,_,[0],[]):-!.
lazy_trace_tops(_,_,[_,0],[]):-!.
lazy_trace_tops(_,0,_,[]):-!.
lazy_trace_tops(M,N,[T2,T1,T0|TL],L):-
   k_value(T2,V2,_),
   k_value(T1,V1,_),
   k_value(T0,V0,_),
   member(([M],M2),V2),
   member(([M],M1),V1),
   member(([M],M0),V0),
   (D1 is (M2-M1)),
   (
      (D1<0)
   ->
      (D0 is (M1-M0)),
      (
         (D0>0)
      ->
         (
            t_same_val(SV)
         ->
            (
               almost_equal(SV,M1,4)
            ->
               (NN is (N-1)),
               L = [T1|LL]
            ;
               (NN=N),
               (LL=L)
            )
         ;
            assertz(t_same_val(M1)),
            (NN is (N-1)),
            L = [T1|LL]
         ),
         lazy_trace_tops(M,NN,TL,LL)
      ;
         lazy_trace_tops(M,N,[T1,T0|TL],L)
      )
   ;
      lazy_trace_tops(M,N,[T1,T0|TL],L)
   ).

%%% Using trace_check to find parameter values

% get value of parameters in ParamList between (Vmin,Vmax) in IntervalList
% with MaxIter maximum iterations for *each* parameter
% s.t. Spec is true for a trace of length N

learn_parameters(ParamList,IntervalList,MaxIter,N):-
	search_parameters(ParamList,IntervalList,MaxIter,N).
learn_parameters(ParamList,IntervalList,MaxIter,Spec,N):-
	search_parameters(ParamList,IntervalList,MaxIter,Spec,N).

search_parameters(ParamList,IntervalList,MaxIter,N):-
   build_spec(Spec),
   search_parameters(ParamList,IntervalList,MaxIter,Spec,N).

search_parameters(ParamList,IntervalList,MaxIter,Spec,N):-
   g_assign(search_mode,first),
   g_assign(search_count,0),
   trace_get(ParamList,IntervalList,MaxIter,Spec,N).

search_all_parameters(ParamList,IntervalList,MaxIter,N):-
   build_spec(Spec),
   search_all_parameters(ParamList,IntervalList,MaxIter,Spec,N).

search_all_parameters(ParamList,IntervalList,MaxIter,Spec,N):-
   g_assign(search_mode,all),
   g_assign(search_count,0),
   trace_get(ParamList,IntervalList,MaxIter,Spec,N).

search_random_parameters(ParamList,IntervalList,MaxIter,N):-
   build_spec(Spec),
   search_random_parameters(ParamList,IntervalList,MaxIter,Spec,N).

search_random_parameters(ParamList,IntervalList,MaxIter,Spec,N):-
   g_assign(search_mode,random(0)),
   trace_get(ParamList,IntervalList,MaxIter,Spec,N).
 
search_random_all_parameters(Min,Max,MaxIter,N):-
   build_spec(Spec),
   search_random_all_parameters(Min,Max,MaxIter,Spec,N).

search_random_all_parameters(Min,Max,MaxIter,Spec,N):-
   g_assign(search_mode,random(0)),
	findall(P,k_parameter(P,_),ParamList),
	findall((Min,Max),k_parameter(_,_),IntervalList),
   trace_get(ParamList,IntervalList,MaxIter,Spec,N).

% get spec from existing LTL specs

build_spec(Spec):-
   findall(S,t_spec(S),L),
   build_conjunction(L,Spec).

build_conjunction([S],S):-!.
build_conjunction([S|SL],'&'(S,Spec)):-
   build_conjunction(SL,Spec).

% first tries the min value of parameters
% then scan the other values by increments or random search according to Mode
trace_get(ParamList,IntervalList,MaxIter,Spec,N):-
   g_assign(trace_found,0),
   g_read(search_mode,Mode),
   get_incr_list(IntervalList,MaxIter,IncrList),
   length(ParamList, NParams),
   TotalIter is MaxIter ** NParams,
   get_set_init_values(ParamList,InitValues,IntervalList),
   g_read(debug,Debug),
   g_assign(debug,-1),
   % store gui status but hide it so that trace_check does not output too much
   % data
   (
      have_gui
   ->
      HadGui = 'true',
      retract(have_gui)
   ;
      HadGui = 'false'
   ),
   estimate_search_time(ParamList,MaxIter,Spec,N),
   statistics(cpu_time,[T1,_]),
   repeat,
   (
      trace_check(Spec,N),
      g_assign(trace_found,1)
   ;
      g_assign(trace_found,0),
      \+ (
         (
            g_read(search_mode,random(I))
         ->
            (
               I1 is I+1,
               I1=<MaxIter,
               g_assign(search_mode,random(I1)),
               random_list(ParamList,IntervalList)
            )
         ;
            g_read(search_count, I),
            I =< TotalIter,
            I1 is I + 1,
            g_assign(search_count, I1),
            incr_list(ParamList,IncrList,IntervalList)
         )
      )
   ),
   % restore GUI status
   (
      HadGui = 'true'
   ->
      assertz(have_gui)
   ;
      true
   ),
   g_read(trace_found,Success),
   (
      (Mode=all, Success=1)
   ->
      (
         (
            have_gui
         ->
            writeall_trace((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A)),
            writeall((nth(NP,ParamList,P),k_parameter(P,V),nth(NP,InitValues,VV),((V=\=VV) -> format_to_atom(A,"[GUI] param ~w,~p",[P,V]);format_to_atom(A,"parameter(~w, ~p)",[P,V]))),A)
         ;
            true
         ),
         writeall((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A)),
         nl,
         fail
      )
   ;
      (
         !,
         statistics(cpu_time,[T2,_]),
         T is (T2-T1)/1000,
         g_assign(debug,Debug),
         (
            (Success = 0)
         ->
            (
               (
                  (Mode=all)
               ->
                  (
                     have_gui
                  ->
                     format("[GUI] checkLTL No more values found. ~n",[])
                ;
                   write('No more values found.\n')
                ),
                (
                   have_gui
                ->
                   format("[GUI] checkLTL Finished searching. ~n ~n",[])
                ;
                   true
                )
             ;
                (
                   have_gui
                ->
                   format("[GUI] checkLTL No value found. ~n",[])
                ;
                   write('No value found.\n')
                )
             ),
             (
                have_gui
             ->
                format("[GUI] checkLTL Finished searching. ~n ~n",[])
             ;
                true
             ),
             set_init_values(ParamList,InitValues),
             fail
          )
       ;
          (
             (
                have_gui
             ->
                format("[GUI] checkLTL First values found that make ~p true:~n  ",[Spec])
             ;
                format("First values found that make ~p true:~n",[Spec])
             ),
             (
                have_gui
             ->
                writeall_trace((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A))
             ;
                writeall((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A))
             )
          )
       ),
       format_debug(0,"Search time: ~2f s~n",[T]),
       (
          have_gui
       ->
          format("[GUI] checkLTL Search time: ~2f s~n",[T])
       ;
          true
       ),
       (
          have_gui
       ->
          format("[GUI] checkLTL Finished searching.~n ~n",[])
       ;
          true
       )
    )
 ).

% Search by corners/center dichotomy
learn_parameter(ParamList,IntervalList,Spec,N):-
   trace_get(ParamList,IntervalList,Spec,N).

% learn_parameters(ParamList,IntervalList,Spec,N):-
%    trace_get(ParamList,IntervalList,Spec,N).

trace_get(ParamList,IntervalList,Spec,N):-
   g_assign(trace_found,0),
   retractall(trace_state(_,_)),
   get_set_init_values(ParamList,InitValues,IntervalList),
   g_read(debug,Debug),
   g_assign(debug,-1),
   MaxSplit=5, % TODO Doc + parametrize MaxSplit
   MaxIter is 2 ** MaxSplit,
   estimate_search_time(ParamList,MaxIter,Spec,N),
   statistics(cpu_time,[T1,_]),
   (
      trace_get2(ParamList,IntervalList,Spec,N,MaxSplit,InitValues)
   ;
      
        (
                        have_gui
                ->
                        format("[GUI] checkLTL  No value found.~n",[])
   ;
                        write('No value found.\n')
                ),
      set_init_values(ParamList,InitValues)
   ),!,
   statistics(cpu_time,[T2,_]),
   T is (T2-T1)/1000,
   g_assign(debug,Debug),
                (
                        have_gui
                ->
                        format("[GUI] checkLTL Search time: ~2f s~n",[T])
                ;
                        true
                ),
   format_debug(0,"Search time: ~2f s~n",[T]).

trace_get2(ParamList,IntervalList,Spec,N,MaxSplit,InitValues):-
   get_point_list(IntervalList,PList),
   add_center_point(IntervalList,[],PList,PointList),
   %format_debug(-2,"PointList ~w~n",[PointList]),
   trace_get_sub(ParamList,PointList,Spec,N),
   g_read(trace_found,Found),
   (
      \+(Found=0)
   ->
      set_init_values(ParamList,Found),
      numerical_simulation(N),
     
      (
         have_gui
      ->
                        format("[GUI] checkLTL Found parameters that make ~p true: ~n",[Spec])
      ;
                         format("Found parameters that make ~p true:~n",[Spec])
                ),
      (
         have_gui
      ->
         writeall_trace((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A))
      ;
         true
      ),
         writeall((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A))
   ;
      (
         same_trace_state(PointList)   % same failure at all corners+center
      ->
         fail                          % no need to go further
      ;
         (MaxSplit > 1),
         split_intervals(IntervalList,NewList),
         NewMax is MaxSplit-1,
         trace_get_rec(ParamList,NewList,Spec,N,NewMax,InitValues)
      )
   ).

% Try trace get on a list of points
trace_get_sub(_,[],_,_).
trace_get_sub(ParamList,[H|T],Spec,N):-
   (
      trace_state(H,_)
   ->
      true
   ;
      set_init_values(ParamList,H),
      %format_debug(-2,"Checking for ~w~n",[H]),
      (
         trace_check(Spec,N)
      ->
         %format_debug(-2,"Yes !!~n",[]),
         g_assign(trace_found,H)
      ;
         true %format_debug(-2,"No~n",[])
      ),
      k_value(0,_,S),
      assertz(trace_state(H,S))%,
      %format_debug(-2,"trace_state: ~w~n",[S])
   ),
   trace_get_sub(ParamList,T,Spec,N).

% Check if all points in the given list have the same trace_state

same_trace_state([]).
same_trace_state([_]).
same_trace_state([A,B|L]):-
   trace_state(A,S),
   trace_state(B,S),
   same_trace_state([B|L]).

% cut intervals in half, return a list of list of intervals

split_intervals([],[[]]).
split_intervals([(Min,Max)|L],NL):-
   split_intervals(L,IL),
   Med is (Min+Max)/2,
   add_point_list((Min,Med),IL,L1),
   add_point_list((Med,Max),IL,L2),
   append(L1,L2,NL).

% Disjunctive search in a list of intervals.

trace_get_rec(_,[],_,_,_,_):-
   fail.

trace_get_rec(ParamList,[H|T],Spec,N,MaxSplit,InitValues):-
   (
      trace_get2(ParamList,H,Spec,N,MaxSplit,InitValues)
   ->
      true
   ;
      trace_get_rec(ParamList,T,Spec,N,MaxSplit,InitValues)
   ).

% Get corners of intervals
get_point_list([],[[]]).

get_point_list([(Min,Max)|T],PL):-
   get_point_list(T,L),
   add_point_list(Min,L,L1),
   add_point_list(Max,L,L2),
   append(L1,L2,PL).

% make a list of n+1 tuples from a list n-tuples by adding a value for each

add_point_list(_,[],[]).
add_point_list(P,[H|T],[[P|H]|TT]):-
   add_point_list(P,T,TT).
   
% get center of intervals
add_center_point([],P1,PList,[P2|PList]):-
   reverse(P1,P2).
add_center_point([(Min,Max)|T],P,PList,PointList):-
   Med is (Min+Max)/2,
   add_center_point(T,[Med|P],PList,PointList).




% Compute a list of increments from a list of pairs (Vmin,Vmax) and a number
% of iterations

get_incr_list([],_MaxIter,[]).
get_incr_list([(Vmin,Vmax)|IntervalList],MaxIter,[Incr|IncrList]):-
   Incr is (Vmax - Vmin)/MaxIter,
   get_incr_list(IntervalList,MaxIter,IncrList).

% Save initial parameter values in a list and initialize them at their minimum

get_set_init_values([],[],[]).
get_set_init_values([K|ParamList],[Kinit|InitValues],[(Kmin,_Kmax)|IntervalList]):-
   (
      retract(k_parameter(K,Kinit))
   ->
      assertz(k_parameter(K,Kmin))
   ;
     
        (
                have_gui
        ->
                format("[GUI] checkLTL Error: ~w is not a known parameter.~n",[K])
        ;
      write_line_col('Error'),
      format("~w is not a known parameter.~n",[K])
        ),
      fail
   ),
   get_set_init_values(ParamList,InitValues,IntervalList). 

% increment (lexicographically) a list of parameters w.r.t. a list of
% increments (for each parameter) and a list of pairs (Vmin,Vmax)

incr_list([_|ParamList],[_|IncrList],[_|IntervalList]):-
   incr_list(ParamList,IncrList,IntervalList),
   !.

incr_list([K|_],[Incr|_],[(Kmin,Kmax)|_]):-
   retract(k_parameter(K,Kval)),
   KK is Kval + Incr,
   (
      (KK =< Kmax)
   ->
      assertz(k_parameter(K,KK))
   ;
      assertz(k_parameter(K,Kmin)),
      fail
   ).

% randomly changes a list of parameters w.r.t. a list of pairs
% (Vmin,Vmax) for each parameter

random_list([K|ParamList],[(Kmin,Kmax)|IntervalList]):-
   retract(k_parameter(K,_)),
	Kmin=<Kmax,
	random(R),
	V is Kmin+R*(Kmax-Kmin),
	assertz(k_parameter(K,V)),
	random_list(ParamList,IntervalList).
random_list([],[]).

% Reset values w.r.t. a given list

set_init_values([],[]).
set_init_values([K|ParamList],[V|InitValues]):-
   retract(k_parameter(K,_)),
   assertz(k_parameter(K,V)),
   set_init_values(ParamList,InitValues).

% Estimate search time by an interpolation from 1 iteration
estimate_search_time(ParamList,MaxIter,Spec,N):-
   length(ParamList,PL),
   statistics(cpu_time,[T1,_]),
   (trace_check(Spec,N);true),
   statistics(cpu_time,[T2,_]),
   % Be pessimistic: should be /1000 but add 25%
   (
      g_read(search_mode,random(_))
   ->
      (
         T is ((T2-T1) * MaxIter)/800
      )
   ;
      T is ((T2 - T1) * (MaxIter ** PL))/800
   ),
   format_debug(-1,"Estimated maximum search time: ~2f s~n",[T]),
   (
      have_gui
   ->
      format("[GUI] checkLTL Estimated maximum search time: ~2f s~n",[T])
   ;
      true
   ),
   (
      have_gui
   ->
      format("[GUI] checkLTL In progress..... Please wait........................~n",[])
   ;
      true
   ).

% get min/max in a trace

get_max_from_trace(M):-
   ensure_trace,!,
   parse_object(M,MM),
   format_debug(3,"object: ~w~n",[MM]),
   findall(X,(k_value(_,V,_),member(([MM],X),V)),L),
   format_debug(5,"maxlist: ~p~n",[L]),
   max_list(L,MMAX),
   format_debug(3,"max: ~p~n",[MMAX]),
   nth(N,L,MMAX),
   g_read(trace_times,TL),
   length(TL,NN),
   NNN is NN - N + 1,
   nth(NNN,TL,T),
  
   (
         have_gui
         ->
                format("[GUI] plotData Value max of ~w is ~p at time ~p~n",[M,MMAX,T])
          ;
                format("Value max of ~w is ~p at time ~p~n",[M,MMAX,T])
        ).
   
get_min_from_trace(M):-
   ensure_trace,!,
   parse_object(M,MM),
   format_debug(3,"object: ~w~n",[MM]),
   findall(X,(k_value(_,V,_),member(([MM],X),V)),L),
   format_debug(5,"minlist: ~p~n",[L]),
   min_list(L,MMAX),
   format_debug(3,"min: ~p~n",[MMAX]),
   nth(N,L,MMAX),
   g_read(trace_times,TL),
   length(TL,NN),
   NNN is NN - N + 1,
   nth(NNN,TL,T),
   
   (
         have_gui
         ->
           format("[GUI] plotData Value min of ~w is ~p at time ~p~n",[M,MMAX,T])
          ;
          format("Value min of ~w is ~p at time ~p~n",[M,MMAX,T])
        ).
   
get_period_from_trace(M):-
   !,
   statistics(cpu_time,[Ti0,_]),
   parse_object(M,MM),
   lazy_trace_tops(MM,3,L),!,
   format_debug(5,"tops: ~p~n",[L]),
   (Ti1=Ti0),
   %compute_derivatives(MM),
   %statistics(cpu_time,[Ti1,_]),
   %TT0 is (Ti1-Ti0)/1000,
   %format_debug(1,"Derivatives time: ~2f s~n",[TT0]),
   %trace_tops(MM,L)
   statistics(cpu_time,[Ti2,_]),
   TT1 is (Ti2-Ti1)/1000,
   format_debug(1,"Peek id time: ~2f s~n",[TT1]),
   (
      reverse(L,[T2,T1,T0|_]),
      P2 is T2-T1,
      P1 is T1-T0,
      !,
      (
         almost_equal(P2,P1,4)
      ->
         (P is (P2+P1)/2,
         
        (
         have_gui
         ->
                format("[GUI] plotData Period of ~w is ~p~n",[M,P])
      ;
                format("Period of ~w is ~p~n",[M,P])
      )
)
   ;
         
        (
         have_gui
         ->
                format("[GUI] plotData Period of ~w oscillations not constant (last two periods: ~p ~p)~n",[M,P1,P2])
          ;
                format("Period of ~w oscillations not constant (last two periods: ~p ~p)~n", [M,P1,P2])
        )
      )
   ;
      
        (
         have_gui
         ->
                format("[GUI] checkLTL Get period from trace: ~w does not show sustained oscillations~n",[M])
          ;
      format("~w does not show sustained oscillations~n",[M])
        )
   ).

% Easter egg

dtc(A):-
   atom_length(A,N),
   (
      (N < 20)
   ->
      (M is 10 - N//2)
   ;
      (M = 0)
   ),
   make_spaces(M,S),
   format("~n~S~w~n",[S,A]),
   (
         have_gui
         ->
           format("[GUI] checkLTL ~n~S~w~n",[S,A])
          ;
         true
        ),
   format("~s~n~s~n",[
      [32,32,32,124,32,32,32,32,32,32,32,32,32,32,32,32,32,124],
      [32,32,47,32,32,32,32,32,32,92,32,47,32,32,32,32,32,32,92]]),
        (
         have_gui
         ->
           format("[GUI] checkLTL ~s~n~s~n",[
      [32,32,32,124,32,32,32,32,32,32,32,32,32,32,32,32,32,124],
      [32,32,47,32,32,32,32,32,32,92,32,47,32,32,32,32,32,32,92]])
          ;
         true
        ),
   format("~s~n~s~n",[
      [32,124,32,32,32,32,32,32,32,32,124,32,32,32,32,32,32,32,32,124],
      [32,32,92,32,32,32,32,32,32,32,42,60,45,45,45,45,32,32,47]]),
(
         have_gui
         ->
           format("[GUI] checkLTL ~s~n~s~n",[
      [32,124,32,32,32,32,32,32,32,32,124,32,32,32,32,32,32,32,32,124],
      [32,32,92,32,32,32,32,32,32,32,42,60,45,45,45,45,32,32,47]])
          ;
         true
        ),
   format("~s~n~n",[
      [32,32,32,124,39,45,45,45,45,39,124,39,45,45,45,45,39,124]]),
(
         have_gui
         ->
           format("[GUI] checkLTL ~s~n~n",[
      [32,32,32,124,39,45,45,45,45,39,124,39,45,45,45,45,39,124]])
          ;
         true
        ).

      
% Set initial state from state at time T
set_init_from_trace(T):-
   ensure_trace,!,
   g_read(trace_times,TL),
   get_time_from_trace(TL,T,TT),
   format_debug(3,"time found: ~p~n",[TT]),
   k_value(TT,V,_),
   set_init_from_values(V),
   show_initial_state.%,
   %list_parameters.

%% Get closet time point in the current trace
% trivial
get_time_from_trace([],_,0).
get_time_from_trace([T],_,T):-!.
% already above most advanced time point
get_time_from_trace([TT|_],T,TT):-
   TT=<T,!.
% smaller than T2 -> continue
get_time_from_trace([_,T2|TL],T,TT):-
   T<T2,!,
   get_time_from_trace([T2|TL],T,TT).
% just between T1 and T2 -> get closest point
get_time_from_trace([T1,T2|_],T,TT):-
   D1 is T1-T,
   D2 is T-T2,
   (
      (D1 < D2)
   ->
      (TT = T1)
   ;
      (TT = T2)
   ).

set_init_from_values([]).
set_init_from_values([([MM],I)|V]):-
   !,
   set_init(MM,I),
   set_init_from_values(V).
set_init_from_values([(_,_)|V]):-
   set_init_from_values(V).

set_init(M,V):-
   initconc(M,C),
   (
      number(C)
   ->
      present(M,V)
   ;
      parameter(C,V)
   ).

% LTL specs
add_ltl((A,B)):-
   !,add_ltl(A),
   add_ltl(B).

add_ltl(A):-
(
      have_gui
   ->
      format("[GUI] add_ltl ~w~n",[A])
   ;
      true
   ),
   assertset(t_spec(A)).

delete_ltl((A,B)):-
   !,delete_ltl(A),
   delete_ltl(B).
delete_ltl(A):-
   retractall(t_spec(A)).

list_ltl:-
   writeall(t_spec(A),A).

clear_ltl:-
   retractall(t_spec(_)).

check_ltl_spec(T):-
   numerical_simulation(T),!,
   check_ltl_spec.

check_ltl_spec:-
   t_spec(S),
   nl,
   trace_check(S),
   fail.

check_ltl_spec.
