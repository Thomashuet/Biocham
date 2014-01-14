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
% GNU prolog file biochamKinetics.pl
% by Sylvain Soliman, Francois Fages, Shrivaths Rajagopalan, Chi-Chia Huang

% FF only plotted macros plus plotted parameters are now saved in the trace


:-dynamic(kplot_changed/0).
:-dynamic(k_parameter/2).
:-dynamic(k_macro/2).
:-dynamic(k_volume/2).
:-dynamic(kplot_macros/0).
:-dynamic(kplot_macro/1).  % which macros to show
:-dynamic(k_new_macro/1).  % which macros NOT to show
:-dynamic(kplot_parameters/0).
:-dynamic(kplot_parameter/1).  % which parameters to show
:-dynamic(k_new_parameter/1).  % which parameters NOT to show
:-dynamic(value/2).        % current value
:-dynamic(k_value/3).      % used in biochamTrace.pl k_value(t,[([x],f(x)),...],[formulas true in x])
%:-dynamic(k_time/1).       % used in biochamTrace.pl
:-dynamic(k_neg_conc/0).   % negative concentration warning
:-dynamic(k_fail/0).       % failure warning

:-dynamic(event3/3).       % store trigger events (event3 since otherwise conflict with event predicate)
:-dynamic(event_true/2).   % store their state
:-dynamic(event/2).        % store initial param value
:-dynamic(event_change/5). % store event change interpolated position

:-dynamic(tevent/4).   % store time events
% CCH :-dynamic(k_maximum/2). 

% :-include(formal).

% default step size, for RK (not Rosenbrock!)
step_size:- step_size(0.01).
step_size(V):-
   (
      V>0
   ->
      g_assign(step_size,V)
   ;
     
        (
                have_gui
        ->
                format("[GUI] errors Step_size must be greater than 0.~n",[])
        ;
      write('step_size must be greater than 0\n')
        )
   ).

% (relative or absolute) error, used for step change in different methods
step_doubling:- g_assign(step_doubling_error,0.0001).

step_doubling(V):-
   (
      V>0
   ->
       g_assign(step_doubling_error,V)
   ;
      
        (
                have_gui
        ->
                format("[GUI] errors Step_doubling error must be greater than 0.~n",[])
        ;
      write('step_doubling error must be greater than 0\n')
        )
   ).

init_stoch :-
   g_assign(conversion_factor,100), % before was 10000
   g_assign(critical_threshold,20). /* CHH,
   g_assign(drawing_period,10),
   g_assign(mean_order,10),
   g_assign(filtering,1) */

no_step_doubling:-
   g_read(k_method,M),
   (
      M=rk
   ->
      g_assign(step_doubling_error,0)
   ;
      
        (
                have_gui
        ->
                format("[GUI] errors No_step_doubling is only available for Runge-Kutta method.~n",[])
        ;
      write('no_step_doubling is only available for Runge-Kutta method\n')
        )
   ).

% used to initialize stuff
kinetics_init_hook:-
   g_assign(not_zero_error,0.0001),
   g_assign(k_method,stiff),
   step_size,
   step_doubling,
   init_stoch,
   SAFE = 0.9, GROW = 1.5, PGROW = -0.25,
   SHRINK = 0.5, PSHRINK is -1.0/3.0, ERRCON = 0.1296,
   %MAXTRY = 40,
   GAM is 1.0/2.0, A21=2.0, A31 is 48.0/25.0, A32 is 6.0/25.0, C21 = -8.0,
   C31 is 372.0/25.0, C32 is 12.0/5.0, C41 is -112.0/125.0,
   C42 is -54.0/125.0, C43 is -2.0/5.0,
   B1 is 19.0/9.0, B2 is 1.0/2.0, B3 is 25.0/108.0, B4 is 125.0/108.0,
   E1 is 17.0/54.0, E2 is 7.0/36.0, E3 is 0.0, E4 is 125.0/108.0,
   % C1X, C2X, C3X, C4X,
   % A2X is 1.0, A3X is 3.0/5.0,
   g_assign(k_rbk_safe,SAFE),
   g_assign(k_rbk_grow,GROW),
   g_assign(k_rbk_pgrow,PGROW),
   g_assign(k_rbk_shrink,SHRINK),
   g_assign(k_rbk_pshrink,PSHRINK),
   g_assign(k_rbk_errcon,ERRCON),
   g_assign(k_rbk_gam,GAM),
   g_assign(k_rbk_a21,A21),
   g_assign(k_rbk_a31,A31),
   g_assign(k_rbk_a32,A32),
   g_assign(k_rbk_c21,C21),
   g_assign(k_rbk_c31,C31),
   g_assign(k_rbk_c32,C32),
   g_assign(k_rbk_c41,C41),
   g_assign(k_rbk_c42,C42),
   g_assign(k_rbk_c43,C43),
   g_assign(k_rbk_b1,B1),
   g_assign(k_rbk_b2,B2),
   g_assign(k_rbk_b3,B3),
   g_assign(k_rbk_b4,B4),
   g_assign(k_rbk_e1,E1),
   g_assign(k_rbk_e2,E2),
   g_assign(k_rbk_e3,E3),
   g_assign(k_rbk_e4,E4).

% displays/stores the prefered simulation method
numerical_method:-
   g_read(k_method,M),
   format("Numerical simulation method: ~w~n",[M]),
   (
      ((M=tl, format("Tau liping method~n",[]));(M=ssa, format("Gillespie algorithm~n",[])))
   ->
      conversion_factor,
      critical_reaction_threshold/*, CCH
      g_read(filtering,F),
      (
         F = 1
      ->
         filtering
      ;
         no_filtering
      ) */
   ;
      g_read(step_doubling_error,E),
      (
         M=rk
      ->
         g_read(step_size,S),
         (E > 0
         ->
            format("Runge-Kutta method with adaptive step size~n",[]),
            format("Initial step size: ~p~n",[S]),
            format("Maximum step error: ~p~n",[E])
         ;  
            format("Runge-Kutta method with fixed step size: ~p~n",[S])
         )
      ;
         %M = stiff
         format("Rosenbrock's implicit method for stiff systems",[]),
         format("Initial step size: time horizon/100~n",[]),
         format("Maximum step error: ~p~n",[E]),
         format("Maximum step size: time horizon/20~n",[])
      )
   ).

numerical_method(M):-
   (
      (M=stiff;M=rk;M=tl;M=ssa)
   ->
      g_assign(k_method,M),
      g_read(step_doubling_error,E),
      (
         (M=stiff,E=:=0)
      ->
         step_doubling
      ;
         true
      ),
      numerical_method
   ;
      
        (
                have_gui
        ->
                format("[GUI] errors Unknown simulation method: ~w~nUse rk, stiff, tl, or ssa~n",[M])
        ;
      format("Unknown simulation method: ~w~nUse rk, stiff, tl, or ssa~n",[M])
        )
   ).

process_reactant_coeff(V,if C then E else F,if C then -V*(E) else FL) :-
   !,process_reactant_coeff(V,F,FL).
process_reactant_coeff(V,if C then E,if C then -V*(E)):-!.
process_reactant_coeff(V,E,-V*(E)).

handle_reactants_cond([],_,[]).
handle_reactants_cond([(V,R1)|RL],RTL,[(R1,F)|RLL]) :-
   !,
   process_reactant_coeff(V,RTL,FF),
   (
      (
         R1 = '::'(_,Loc),
         k_volume(Loc,Vol)
      )
   ->
      F=FF/Vol
   ;
      F=FF
   ),
   handle_reactants_cond(RL,RTL,RLL).

process_product_coeff(V,if C then E else F,if C then V*(E) else FL) :-
   !,process_product_coeff(V,F,FL).
process_product_coeff(V,if C then E,if C then V*(E)):-!.
process_product_coeff(V,E,V*(E)).

handle_products_cond([],_,[]).
handle_products_cond([(V,R1)|RL],RTL,[(R1,F)|RLL]) :-
   !,
   process_product_coeff(V,RTL,FF),
   (
      (
         R1 = '::'(_,Loc),
         k_volume(Loc,Vol)
      )
   ->
      F=FF/Vol
   ;
      F=FF
   ),
   handle_products_cond(RL,RTL,RLL).

% SInfo is list of elements of the kind (Rule,Reactant List,Product list,Rate
% law) 

get_kinetics([],[]).
get_kinetics([(_,RL,PL,RT)|SInfo],[K|KInfo]) :-
   (
      (RT=0)
   ->
      (K=0)
   ;
      dot_intersect(RL,PL,_,URL,UPL),  % remove common
      handle_reactants_cond(URL,RT,RLL),     
      handle_products_cond(UPL,RT,PLL),
      append(RLL,PLL,K)
   ),
   get_kinetics(SInfo,KInfo).

apply_kinetics(K,ML) :-
   apply_kinetics(K,ML,_).

apply_kinetics(K,ML,L) :- 
     findall((R,KL,KR,RL),rule(_,KL,KR,R,_,_,RL),L),
     get_kinetics(L,K),
     nusmv_search_molecules,
     findall(A,molecule(A,_),ML).


%%% Compilation of the concentration ODE for every molecule

% get info for One molecule in One kinetics
compile_one_exp(_,[],0).
compile_one_exp(A,[(A,X)|T],Y) :-
   !,
   compile_one_exp(A,T,E),
   (
      (E=0)
   ->
      (Y=X)
   ;
      Y=X+E
   ).
compile_one_exp(A,[_|T],(E)) :- compile_one_exp(A,T,E).

% get info for One molecule in a list of kinetics
compile_info(_,[],0).
compile_info(A,[K|KL],Z) :-
   !,
   (
      member((A,_),K) 
   ->
      compile_one_exp(A,K,X),
      compile_info(A,KL,Y),
      (
         (Y=0)
      ->
         (Z=X)
      ;
         Z=X+Y
      )
   ;
      compile_info(A,KL,Z)
   ).

% get info from list of Kinetics, List of molecules, return List of expr
compile_all(_,[],[]):-!.
compile_all(K,[M|L],[EE|Rest]) :-
   compile_info(M,K,E),!,
   simplify(E,E1),
   prettify(E1,E2),
   simplify(E2,E3),
   prettify(E3,EE),
   compile_all(K,L,Rest).


%%% Prepare the initial concentration list

find_all_else([],_,[]):-!.
find_all_else([M|ML],L,[([M],C)|XS]) :- member((M,C),L),find_all_else(ML,L,XS),!.
find_all_else([M|ML],L,[([M],0)|XS]) :- 
   (
      initconc(M,K)
   ->
        (
                have_gui
        ->
                format("[GUI] warnings Unknown parameter ~w, assuming 0 as initial value for ~w.~n",[K,M])
        ;
      write_line_col('Warning'),
      write('unknown parameter: '),
      write(K),
      write(', assuming 0 as initial value for '),
      write(M),nl
        )
     
   ;
      true
   ),
   find_all_else(ML,L,XS),!.

get_all_initial_values(ML,X):-
    findall((X,V),(
      initconc(X,C),
      (
         number(C)
      ->
         V=C
      ;
         k_parameter(C,V)
      )),L),
    find_all_else(ML,L,X).

show_kinetics_list([],[]):-nl.
show_kinetics_list([(M,C)|CL],[E|EL]) :- 
%   format("~p~c~w~c~p~n",[C,9,M,9,E]),
	(
                have_gui
        ->
                format("[GUI] showKinetics ~p,d~w/dt=~p~n",[C,M,E])
        ;
   		format("~p~cd~w/dt=~p~n",[C,9,M,E])
        ),
   %write(C),write('\t'),write(M),write('\t'),write(E),nl,
   show_kinetics_list(CL,EL).

%% Show all kinetic data 

list_ODE:- show_kinetics.
list_ODE(M):- show_kinetics(M).

list_kinetics:- show_kinetics.
list_kinetics(M):- show_kinetics(M).

show_kinetics:- 
   apply_kinetics(K,ML),
   compile_all(K,ML,ExprList),     
   get_all_initial_values(ML,ConcList),
%   write('Init '),write('\t'),write('Mol.'),write('\t'),write('ODE'),nl,
	(
                have_gui
        ->
                format("[GUI] showKinetics Start~n",[])
        ;
   		write('Init '),write('\t'),write('ODE'),nl
        ),
   show_kinetics_list(ConcList,ExprList),
   check_params(ExprList),
	(
                have_gui
        ->
                format("[GUI] showKinetics Finish~n",[])
        ;
   		true
        ).

% same for only one molecule

show_kinetics(A):-
   find_molecules(A,B),
   apply_kinetics(K,_ML),
   compile_all(K,[B],ExprList),
   get_all_initial_values([B],ConcList),
   write('Init '),write('\t'),write('Mol.'),write('\t'),write('ODE'),nl,
   show_kinetics_list(ConcList,ExprList).
   
%%% Kinetic simulator using Runge-Kutta 4th Order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_trace(Expression, Call, Value) :-
   format_debug(4, 'Evaluating ~q...\n', [Expression]),
   catch(
     Call,
     error(evaluation_error(zero_divisor), _),
     throw(division_by_zero(Expression))
   ),
   format_debug(4, '~q = ~q\n', [Expression, Value]).

eval(E, VL, Z) :-
   eval_trace(E, subeval(E, VL, Z), Z).

subeval(X+Y,VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is N1+N2.
subeval(X-Y,VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is N1-N2.
subeval(-Y,VL,Z)  :- !,eval(0-Y,VL,Z).
subeval(X*Y,VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is N1*N2.
% TODO catch division by zero
subeval(X/Y,VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is N1/N2.
subeval(X^Y,VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is N1**N2.
subeval(min(X,Y),VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is min(N1,N2).
subeval(max(X,Y),VL,Z) :- !,eval(X,VL,N1),eval(Y,VL,N2), Z is max(N1,N2).
subeval(log(X),VL,Z) :- !,eval(X,VL,N1), Z is log(N1).
subeval(exp(X),VL,Z) :- !,eval(X,VL,N1), Z is exp(N1).
subeval(sin(X),VL,Z) :- !,eval(X,VL,N1), Z is sin(N1).
subeval(cos(X),VL,Z) :- !,eval(X,VL,N1), Z is cos(N1).
subeval(abs(X),VL,Z) :- !,eval(X,VL,N1), Z is abs(N1).
subeval(random,_,Z) :- !, random(Z).
subeval(frac(X), VL, Z) :-
   !,
   eval(X, VL, N1),
   Z is float_fractional_part(N1).
subeval(D,_,R) :- number(D),!,D=R.
subeval(P,_,V) :- k_parameter(P,X),!,X=V.
subeval(P,VL,V) :- 
   k_macro(P,VV),
   !,
   eval_conditional_func(VV,VL,V).

subeval('Time',_,T) :- % fragile !!! not sure about rk
   g_read(x,X),
   g_read(k_method,M),
   (
      ((M=stiff);(X=0))
   ->
      g_read(hdid,H)
   ;
      g_read(h,H)
   ),
   T is X+H,!.

subeval(sq_wave(V0, D0, V1, D1, V2, D2), _, V):-
   !,
   eval('Time', [], T),
   (
      T < D0
   ->
      k_parameter(V0, V)
   ;
      (
         ((D1 + D2) * float_fractional_part((T - D0)/ (D1 + D2)) < D1)
      ->
         k_parameter(V1, V)
      ;
         k_parameter(V2, V)
      )
   ).

subeval(sq_wave(V1, D1, V2, D2), _, V):-
   !,
   eval(sq_wave(0, 0, V1, D1, V2, D2), [], V).

subeval(X,VL,Z) :- member((X,Z),VL),!.

subeval(A,_,_):-
   
        (
                have_gui
        ->
                format("[GUI] errors Unknown parameter or molecule ~w.~n",[A]),
                format("[GUI] add_rule Error: unknown parameter or molecule: ~w~n",[A])
        ;
   		write_line_col('Error'),
   		write('unknown parameter or molecule: '),
                write(A),nl
        ),
	(
           have_gui
        		->
                		format("[GUI] checkLTL Error: unknown parameter or molecule: ~w~n",[A]),			       
                		format("[GUI] checkLTL Aborting evaluation...~n",[])
			;
				true
        ),
        throw('Aborting evaluation...').
   
   
   
eval_condition(Y =< N,VL) :-
   (
      eval_condition(Y < N,VL)
   ->
      true
   ;
      eval_condition(Y = N,VL)
   ).
eval_condition(Y >= N,VL) :-
   (
      eval_condition(Y > N,VL)
   ->
      true
   ;
      eval_condition(Y = N,VL)
   ).
eval_condition(X < Y,VL):-
   eval(X,VL,XX),
   eval(Y,VL,YY),
   (XX < YY).
eval_condition(X > Y,VL):-
   eval(X,VL,XX),
   eval(Y,VL,YY),
   (XX > YY).
eval_condition(X = Y,VL):-
   eval(X,VL,XX),
   eval(Y,VL,YY),
   (XX =:= YY).
eval_condition(true,_).

test_conditions(X,VL) :- eval_condition(X,VL),!.
test_conditions(X and E,VL) :- test_conditions(X,VL),!,test_conditions(E,VL).

eval_case_function(if C then E else F,VL,Z) :-
   !,
   (
      test_conditions(C,VL)
   ->
      eval(E,VL,Z)
   ;
      eval_case_function(F,VL,Z)
   ).
eval_case_function(if C then E,VL,Z) :-
   !,
   (
      test_conditions(C,VL)
   ->
      eval(E,VL,Z)
   ;
      Z is 0
   ).
eval_case_function(E,VL,Z) :- eval(E,VL,Z).

eval_conditional_func(E, VL, Z) :-
   eval_trace(E, subeval_conditional_func(E, VL, Z), Z).

% FF what about -X ? use in symbolic expressions for rk steps
subeval_conditional_func([],_,0):-!.
subeval_conditional_func(X+Y,VL,Z) :- !,eval_conditional_func(X,VL,N1), eval_conditional_func(Y,VL,N2), Z is N1+N2.
subeval_conditional_func(X-Y,VL,Z) :- !,eval_conditional_func(X,VL,N1), eval_conditional_func(Y,VL,N2), Z is N1-N2.
subeval_conditional_func(X*Y,VL,Z) :- !,eval_conditional_func(X,VL,N1), eval_conditional_func(Y,VL,N2), Z is N1*N2.
subeval_conditional_func(X/Y,VL,Z) :- !,eval_conditional_func(X,VL,N1), eval_conditional_func(Y,VL,N2), Z is N1/N2.
subeval_conditional_func(F,VL,Z) :- eval_case_function(F,VL,Z).

eval_ind(Fn,Xn,Yn,T) :- append([(t,Xn)],Yn,Z),eval_conditional_func(Fn,Z,T).


add_two_list([],[],[]):-!.
add_two_list([(V,X)|L],[(V,Y)|M],[(V,Z)|N]) :- Z is X+Y,!,add_two_list(L,M,N),!.

mult_list([],_,[]):-!.
mult_list([(V,X)|L],D,[(V,Y)|M]) :- Y is X * D,!, mult_list(L,D,M),!.


eval_func_list(_,_,[],[],[]):-!.
eval_func_list(Xn,Yn,[(Var,_)|L],[F|FL],[(Var,Val)|M]) :-
		eval_ind(F,Xn,Yn,Val),!,
		eval_func_list(Xn,Yn,L,FL,M).

apply_first_step(Xn,Yn,H,FL,K1) :- eval_func_list(Xn,Yn,Yn,FL,K),mult_list(K,H,K1).

apply_second_step(Xn,Yn,H,FL,K1,K2) :- mult_list(K1,1/2,P),add_two_list(P,Yn,D),eval_func_list(Xn+H/2,D,D,FL,K),mult_list(K,H,K2).

apply_third_step(Xn,Yn,H,FL,K2,K3)  :- mult_list(K2,1/2,P),add_two_list(P,Yn,D),eval_func_list(Xn+H/2,D,D,FL,K),mult_list(K,H,K3).

apply_fourth_step(Xn,Yn,H,FL,K3,K4) :- add_two_list(K3,Yn,D),eval_func_list(Xn+H,D,D,FL,K),mult_list(K,H,K4).

apply_final_step(Yn,K1,K2,K3,K4,Y) :-   add_two_list(K2,K3,D),
	                                mult_list(D,2,S),
					add_two_list(S,K1,S1),
					add_two_list(S1,K4,S2),
					mult_list(S2,1/6,S3),
					add_two_list(Yn,S3,Y).

% returns the curve point OL at time Xn+H from derivative FL
runge_kutta(Xn,Yn,H,FL,OL) :-                
		apply_first_step(Xn,Yn,H,FL,K1),
		apply_second_step(Xn,Yn,H,FL,K1,K2),
		apply_third_step(Xn,Yn,H,FL,K2,K3),
		apply_fourth_step(Xn,Yn,H,FL,K3,K4),
		apply_final_step(Yn,K1,K2,K3,K4,OL).

% why 3 significant digits??? c'etait pour que ce soit plus lisible pour Laurence, mais ce n'est pas ass�s pr�cis, alors j'en ai mis 9

print_number_tabulation(F,X):-
	float(X),!,
	format(F,"~p~c",[X,9]).  %9 significant  digits.	% char 9 is a tabulation
print_number_tabulation(F,X):-
	integer(X),!,
	format(F,"~d~c",[X,9]).


% pretty_print(F,[]) :- nl(F).
pretty_print(_,[]).
pretty_print(F,[(M,X)|L]) :-
	print_number_tabulation(F,X),
   (
      (X < -1.0e-10),\+(k_neg_conc)
   ->
      assertz(k_neg_conc),
      format_debug(0,"Warning: found negative concentration (~p) for ~w~n",[X,M])
   ;
      true
   ),
	pretty_print(F,L).

% FF to optimize by keeping only the points where the sign of one derivative changes
trace_step(File,Xm,V):-
   print_number_tabulation(File,Xm),
   pretty_print(File,V),
   print_parameters(File),
   print_macros(File,V),
   assertz(k_value(Xm,V,[])),
   %assertset(k_time(Xm)),
   g_read(k_time_units,TU),
   TTU is TU+1,
   g_assign(k_time_units,TTU),
   g_assign(k_time,g_array_extend(TU,Xm)).

runge_kutta_without_step_doubling(File,Fn,InitX,InitY,N) :-
   init_event,
   (
      InitX == 0
   ->
      g_read(step_size,H),
      g_assign(h,H),
      g_assign(x,InitX),
      g_assign(fx,InitY),
      print_number_tabulation(File,InitX),
      pretty_print(File,InitY),
      print_parameters(File),
      print_macros(File,InitY),
      assertz(k_value(InitX,InitY,[])),
      %assertset(k_time(InitX)),
      g_assign(k_time,g_array([InitX])),
      g_assign(k_time_units,2),
      handle_events(0,0,[],InitY,_,_)
   ;
      g_read(h,H)
   ),

   repeat,
   g_read(x,M),
   g_read(fx,U),

   init_time_event,
   (  g_read(time_event_time,M)
   -> 
      handle_time_events,
      trace_step(File,M,U), %FF to remove if causes problems with QFLTL(R) constraints
      init_time_event
   ;
      true),
   g_read(time_event_time,T), % well the ODE may have changed then
   (  M<T,T<M+H
   -> 
      H2 is T-M,
      Xm is T
   ;  
      (  M+H>InitX+N
      ->
         H2 is InitX+N-M,
         Xm is InitX+N
      ;
         H2 is H,
         Xm is M+H
      )
   ),

   runge_kutta(Xm,U,H2,Fn,V),   %integration step with new ODE system 
   handle_events(M,Xm,U,V,XX,VV),
   format_debug(4,"rk integration step at ~p~n",[XX,VV]),
   trace_step(File,XX,VV),
   g_assign(x,XX),
   g_assign(fx,VV),
   Xm>=InitX+N,
   !,
   end_event.
   
runge_kutta_with_step_doubling(File,Fn,InitX,InitY,N) :-
   init_event,
   HMAX is (N-InitX)/20,
   g_assign(hmax,HMAX),
   (  
      InitX == 0
   -> 
      g_read(step_size,H0),
      g_assign(h,H0),
      g_assign(x,InitX),
      g_assign(fx,InitY),
      print_number_tabulation(File,InitX),
      pretty_print(File,InitY),
      print_parameters(File),
      print_macros(File,InitY),
      assertz(k_value(InitX,InitY,[])),
      g_assign(k_time,g_array([InitX])),
      g_assign(k_time_units,2),
      handle_events(0,0,[],InitY,_,_)
   ;  
      true
   ),

	repeat,

	g_read(h,H),
	g_read(x,M),
	g_read(fx,U),

        init_time_event,
        (  g_read(time_event_time,M)
        -> 
           handle_time_events,
           trace_step(File,M,U), %FF to remove if causes problems with QFLTL(R) constraints
           init_time_event
        ;  
           true),
        g_read(time_event_time,T), % well the ODE may have changed then
        (  M<T,T<M+H
        -> 
           H2 is T-M,
           Xm is T
        ;  
           (  M+H>InitX+N
           -> 
              H2 is InitX+N-M,
              Xm is InitX+N
           ;  
              H2 is H,
              Xm is M+H
           )
        ),

	runge_kutta(Xm,U,H2,Fn,Vn),
	g_assign(fxn,Vn),
	check_error(Fn),

	g_read(fxn,V),
	g_read(h,Hn),
	Xn is M+Hn, % we have Xn =< Xm

        handle_events(M,Xn,U,V,XX,VV), % we have XX =< Xn

        format_debug(4,"rk integration step at ~p~n",[XX,VV]),
        trace_step(File,XX,VV),
        % FF g_assign(k_time,g_array_extend(TU,Xm)),
	g_assign(x,XX),
	g_assign(fx,VV),

        (  Xn==XX
        ->
           H3 is 2*Hn
        ;
           g_read(step_size,H3) %FF prudence mais pas suffisant le mal est fait VV est faux
        ),
	g_assign(h,H3),

	XX>=InitX+N, 
	!,
        end_event.

check_error(Fn):-
	g_read(x,M),
	g_read(fx,U),

	repeat, % in order to reuse computation steps at H/2
	g_read(h,H),
	g_read(fxn,V),
	format_debug(4,"checking error at ~p with step size ~p~n",[M,H]),
        H2 is H/2,
	Xi is M+H2,
	runge_kutta(Xi,U,H2,Fn,V2),
	Xj is Xi+H2,
	runge_kutta(Xj,V2,H2,Fn,V3),

	(
      no_error(V,V3)
    ->
	
      !,
	   format_debug(4,"ok~n",[]),
      g_assign(fxn,V3)
   ;
	   g_assign(h,H2),
	   g_assign(fxn,V2),
	   fail
   ).

no_error([],[]).
no_error([(V,A)|L],[(_,B)|M]):-
	g_read(step_doubling_error,E),
   g_read(not_zero_error,Z),
   (
      ((A+B) > Z)
   ->
      Err is abs(((A-B)*2)/(A+B)) % for a relative error
   ;
      Err is abs(A-B)
   ),
   (
      Err < E,
      no_error(L,M)
   ;
      format_debug(4,"error check failed for ~p with ~p~n",[V,Err]),
      fail
   ).

% Main ODE simulator 
% DataFile: file name for writing the trace of values
% Fn: list of expressions (multivariable function)
%% InitX: initial values of "time"
% InitY: list of initial values
% N: number of iterations
% H: step size

ode_solver(_,_,_,_,N):-   % Allow negative values to avoid simulating...
   N < 0, !.

ode_solver(DataFile,Fn,InitX,InitY,N) :-
   (
      (InitX==0)
   ->
      open(DataFile,write,File),
      write(File,'#Time\t'),
      kinit_data_file(InitY,File),
      retractall(k_value(_,_,_)),
      retractall(k_time(_))   % FF old code ????
   ;
      open(DataFile,append,File)
   ),
   g_read(k_method,M),
   statistics(cpu_time,_),
   (
      M=stiff
   ->
      rosenbrock(File,Fn,InitX,InitY,N)
   ;
	   g_read(step_doubling_error,E),
	   (
         E=0
      -> 
	      runge_kutta_without_step_doubling(File,Fn,InitX,InitY,N)
      ;
         runge_kutta_with_step_doubling(File,Fn,InitX,InitY,N)
      )
   ),
   statistics(cpu_time,[_,T]),
   TT is T/1000,
   format_debug(0,"Simulation time: ~ps~n",[TT]),
   % store time info for trace querying
   %findall(Time,k_time(Time),TimeL),
   g_read(k_time,g_array(TimeL)),
   reverse(TimeL,TL), % get a list of time points in reverse order
   g_assign(trace_times,TL),
   retractall(k_neg_conc),
   retractall(k_fail),
   close(File).
   
% Fill in legend in the data file
kinit_data_file([],F):-
   kinit_data_file_parameters(F),
   kinit_data_file_macros(F),
   nl(F),!.

kinit_data_file([(M,_)|T],F):-
   write(F,M),
   write(F,'\t'),
   kinit_data_file(T,F).

kinit_data_file_macros(F):-
   kplot_macro(M), 
   write(F,M),
   write(F,'\t'),
   fail.

kinit_data_file_macros(_).

kinit_data_file_parameters(F):-
   kplot_parameter(M),
   write(F,M),
   write(F,'\t'),
   fail.

kinit_data_file_parameters(_).

kget_showable([],[],N,N).
kget_showable([A|ML],L,N,NN) :-
   (
      showable(A)
   ->
      L = [(A,N)|LL]
   ;
      L = LL
   ),
   M is N+1,
   kget_showable(ML,LL,M,NN).

kwrite_plots([],N,G):-
   (
      (kplot_macros;kplot_parameters)
   ->
      g_assign(kplot_nb,N),
      kplot_parameters(G),
      kplot_macros(G)
   ;  
      nl(G)
   ).
kwrite_plots([(H,N)|T],M,G) :-
   (
      (g_read(first_plot,1))
   ->
      write(G,'plot'),
      g_assign(first_plot,2)
   ;
      write(G,',')
   ),
   write(G,' "'),
   data_file(Data),
   write(G,Data),
   write(G,'" using 1:'),
   write(G,N),
   write(G,' title "'),
   write(G,H),
   write(G,'"'),
   (
      plot_color(H,C)
   ->
      format(G," with lines lt ~w",[C])
   ;
      true
   ),
   kwrite_plots(T,M,G).
   
kplot_parameters(G):-
   format_debug(4,"plotting parameters...~n",[]),
   k_parameter(H,_),
   atomic(H),
   format_debug(4,"plotting ~w~n",[H]),
   data_file(Data),
   \+(k_new_parameter(H)),     % do NOT plot new parameters
   (
      kplot_parameter('_all_')
   ->
      true
   ;
      kplot_parameter(H)
   ),
   g_read(kplot_nb,N),
   M is N+1,
   g_assign(kplot_nb,M),
   format_debug(4,"writing ~w to file~n",[H]),
   (
      (g_read(first_plot,1))
   ->
      write(G,'plot'),
      g_assign(first_plot,2)
   ;
      write(G,',')
   ),
   write(G,' "'),
   write(G,Data),
   write(G,'" using 1:'),
   write(G,N),
   write(G,' title "'),
   write(G,H),
   write(G,'"'),
   (
      plot_color(H,C)
   ->
      format(G," with lines lt ~w",[C])
   ;
      true
   ),
   fail.

kplot_parameters(_).
   
kplot_macros(G):-
   format_debug(4,"plotting macros...~n",[]),
   k_macro(H,_),
   atomic(H),
   format_debug(4,"plotting ~w~n",[H]),
   data_file(Data),
   \+(k_new_macro(H)),     % do NOT plot new macros
   (
      kplot_macro('_all_')
   ->
      true
   ;
      kplot_macro(H)
   ),
   g_read(kplot_nb,N),
   M is N+1,
   g_assign(kplot_nb,M),
   format_debug(4,"writing ~w to file~n",[H]),
   (
      (g_read(first_plot,1))
   ->
      write(G,'plot')
   ;
      write(G,',')
   ),
   write(G,' "'),
   write(G,Data),
   write(G,'" using 1:'),
   write(G,N),
   write(G,' title "'),
   write(G,H),
   write(G,'"'),
   (
      plot_color(H,C)
   ->
      format(G," with lines lt ~w",[C])
   ;
      true
   ),
   fail.

kplot_macros(G):-
   nl(G).

show_macros:-
   assertset(kplot_macro('_all_')),
   assertset(kplot_macros).

show_macros({H,T}):-
   !,assertset(kplot_macro(H)),
   show_macros({T}).
   
show_macros({}).
show_macros({H}):-
   assertset(kplot_macro(H)),
   assertset(kplot_macros).

hide_macros:-
   retractall(kplot_macro(_)),
   retractall(kplot_macros).

show_parameters:-
   assertset(kplot_parameter('_all_')),
   assertset(kplot_parameters).

show_parameters({H,T}):-
   !,assertset(kplot_parameter(H)),
   show_parameters({T}).
   
show_parameters({}).
show_parameters({H}):-
   assertset(kplot_parameter(H)),
   assertset(kplot_parameters).

hide_parameters:-
   retractall(kplot_parameter(_)),
   retractall(kplot_parameters).

kfill_plot_file(L) :-
   plot_file(Plot),
   open(Plot,write,Gnuplot),        
   write(Gnuplot,'set style data lines\nset format y "%g"\n'),
   g_read(kplot_range,Range),
   format(Gnuplot,"set xrange [~w:~w]~nset yrange [~w:~w]~n",Range),
   write(Gnuplot,'set ytics nomirror autofreq\nset xtics nomirror\nset mxtics default\nset key outside Left reverse\n'),
   kget_showable(L,L2,2,N),
%   (
%     (L2 = [])
%   ->
%      (
%        
%      (
%            have_gui
%                 ->
%%%%%%                        format("[GUI] plot Error: You need to plot at least one molecule.~n",[]),
%%                        format("[GUI] errors You need to plot at least one molecule.~n",[])
%                  ;
%                        write('Error: '),
%                        write('You need to plot at least one molecule.\n')
%        )
%      )
%   ;
%      (
%         % removed cause breaks invariants
%         % sort_by_init(L2,[],L3),
%         kwrite_plots(L2,N,Gnuplot,1)
%      )
%   ),
   g_assign(first_plot,1),
   kwrite_plots(L2,N,Gnuplot),
   close(Gnuplot).

% phase space plots
plot(M1,M2):-
   parse_object(M1,X1),
   parse_object(M2,X2),
   k_value(_,_,_),
   plot_file(Plot),
   (
      have_gui
   ->
      format("[GUI] pplot ~w ~w ~w~n",[X1,X2,Plot])
   ;
      data_file(Data),
      assertz(plot_added(phase)),
      apply_kinetics(_,ML),
      remove_invariants(ML,IL,ML2),
      append(ML2,IL,ML3),
      %g_read(kplot_range,Range),
      open(Plot,write,Gnuplot),
      write(Gnuplot,'set style data lines\nunset key\n'),
      format(Gnuplot,"set xrange [*:*]~nset yrange [*:*]~n",[]),
      format(Gnuplot,"set title \"~w vs ~w\"~n",[M1,M2]),
      format(Gnuplot,"set xlabel \"~w\"~nset ylabel \"~w\"~n",[M2,M1]),
      nth(N1,ML3,X1),
      NN1 is N1+1,
      nth(N2,ML3,X2),
      NN2 is N2+1,
      format(Gnuplot,"plot \"~w\" using ~w:~w~n",[Data,NN2,NN1]),
      close(Gnuplot),
      plot,
      retract(plot_added(phase))
   ),
   assertz(kplot_changed),!.

plot(M1,M2,M3):-
   parse_object(M1,X1),
   parse_object(M2,X2),
   parse_object(M3,X3),
   k_value(_,_,_),
   plot_file(Plot),
   (
      have_gui
   ->
      format("[GUI] ppplot ~w ~w ~w ~w~n",[X1,X2,X3,Plot])
   ;
      data_file(Data),
      assertz(plot_added(phase)),
      apply_kinetics(_,ML),
      remove_invariants(ML,IL,ML2),
      append(ML2,IL,ML3),
      %g_read(kplot_range,Range),
      open(Plot,write,Gnuplot),
      % set nokey -> unset key for GNUPLOT >= 4.0
      write(Gnuplot,'set style data lines\nset nokey\n'),
      format(Gnuplot,"set xrange [*:*]~nset yrange [*:*]~n",[]),
      format(Gnuplot,"set title \"~w vs ~w vs ~w\"~n",[M3,M2,M1]),
      format(Gnuplot,"set xlabel \"~w\"~nset ylabel \"~w\"~nset zlabel \"~w\"~n",
      [M1,M2,M3]),
      nth(N1,ML3,X1),
      NN1 is N1+1,
      nth(N2,ML3,X2),
      NN2 is N2+1,
      nth(N3,ML3,X3),
      NN3 is N3+1,
      format(Gnuplot,"splot \"~w\" using ~w:~w:~w~n",[Data,NN1,NN2,NN3]),
      close(Gnuplot),
      plot,
      retract(plot_added(phase))
   ),
   assertz(kplot_changed),!.

% insertion sort
sort_by_init([],L,L).
sort_by_init([H|T],L,S):-
   insert(H,L,LL),
   sort_by_init(T,LL,S).

insert((X,N),[],[(X,N)]).
insert((X,NX),[(Y,NY)|T],[(Y,NY)|TT]):-
   get_init(X,X0),
   get_init(Y,Y0),
   X0 < Y0,!,
   insert((X,NX),T,TT).

insert((X,NX),[(Y,NY)|T],[(X,NX),(Y,NY)|T]).

get_init(X,Y):-
   initconc(X,C),!,
   (
      number(C),
      Y=C
   ;
      k_parameter(C,Y)
   ).

get_init(_,0).


%FF should allow simulating and plotting 0 molecules only macros or parameters

numerical_simulation:-numerical_simulation(0,20).

numerical_simulation(N):-numerical_simulation(0,N).

numerical_simulation(I,N):-
   apply_kinetics(K,ML,L),
   do_conservation,
   remove_invariants(ML,IL,ML2),
   (
      is_zero_list(K)
   ->
      
        (
         have_gui
                 ->
                         format("[GUI] errors All kinetic laws are null...You must have a set of rules first. ~n",[])
                 ;
      write_line_col('Error'),
      write('All kinetic laws are null...\n'),
      write('you must load a set of rules (with load_biocham(file_bc)'),
                         write(' or\nwith add_biocham(file_bc) or with add_rule(R))\n')
        ),
      fail
   ;
      true
   ),
   compile_all(K,ML2,ExpList),
   get_all_initial_values(ML2,ConcList),
   check_macros(ConcList),
   data_file(Data),
   g_read(k_method,M),
   (       
      (M=tl;M=ssa)
   ->
      stochastic_solver(Data,L,I,ConcList,N)
   ;
      ode_solver(Data,ExpList,I,ConcList,N)
   ),
   (
      (N>0)
   ->
      retractall(k_new_macro(_)),   % all macros should be plottable
      g_assign(kplot_range,[0,N,'*','*'])   % default xrange/yrange
   ;
      true
   ),
   append(ML2,IL,ML3),
   kfill_plot_file(ML3),
   assertset(kplot_changed),
   retractall(trace_loaded),
   retractall(plot_added(_)).

continue(N):-
   g_read(trace_times,[T|_]),
   M is T+N,
   trace_clean,
   numerical_simulation(T,M).

is_zero_list([]).
is_zero_list([0|L]):-
   is_zero_list(L).

set_xmin(X):-
   (
      g_read(kplot_range,[XMin,XMax,YMin,YMax])
   ->
      true
   ;
      ([XMin,XMax,YMin,YMax]=['*','*','*','*'])
   ),
   g_assign(kplot_range,[X,XMax,YMin,YMax]).

set_xmax(X):-
   (
      g_read(kplot_range,[XMin,XMax,YMin,YMax])
   ->
      true
   ;
      ([XMin,XMax,YMin,YMax]=['*','*','*','*'])
   ),
   g_assign(kplot_range,[XMin,X,YMin,YMax]).

set_ymin(Y):-
   (
      g_read(kplot_range,[XMin,XMax,YMin,YMax])
   ->
      true
   ;
      ([XMin,XMax,YMin,YMax]=['*','*','*','*'])
   ),
   g_assign(kplot_range,[XMin,XMax,Y,YMax]).

set_ymax(Y):-
   (
      g_read(kplot_range,[XMin,XMax,YMin,YMax])
   ->
      true
   ;
      ([XMin,XMax,YMin,YMax]=['*','*','*','*'])
   ),
   g_assign(kplot_range,[XMin,XMax,YMin,Y]).


fit_xmin:-
   set_xmin('*').

fit_xmax:-
   set_xmax('*').

fit_x:-
   fit_xmin,fit_xmax.

fit_ymin:-
   set_ymin('*').

fit_ymax:-
   set_ymax('*').

fit_y:-
   fit_ymin,fit_ymax.


export_kinetic_plot(F):-
   kplot_changed,
   % retract(kplot_changed),
   apply_kinetics(_,L),
   retract(data_file(D)),
   retract(plot_file(P)),
   atom_concat(F,'.csv',D3),
   atom_concat(F,'.plot',P3),
   biocham_cp(D, D3),
   assertz(plot_file(P3)),
   assertz(data_file(D3)),
   do_conservation,
   remove_invariants(L,IL,L2),
   append(L2,IL,L3),
   kfill_plot_file(L3),
   retractall(data_file(_)),
   retractall(plot_file(_)),
   assertz(data_file(D)),
   assertz(plot_file(P)).


% Forget stoichiometric information from a list of pairs (stoi,mol)
forget_stoichiometry([],[]).

forget_stoichiometry([(_,M)|L1],[M|L2]):-
   forget_stoichiometry(L1,L2).

%%% Parameter management

%% Dragana,deleting parameter,macro and declaration.

%Dragana,delete a parameter P
delete_parameter(P):-
   retract(k_parameter(P,_)),
   (  
      have_gui
   -> 
      format("[GUI] deleteParameter ~w~n",[P])
   ;  
      format("Parameter ~w deleted.~n",[P])
   ).

%Dragana,delete a macro M
delete_macro(M):-
   retract(k_macro(M,_)),
   (  
      have_gui
   -> 
      format("[GUI] deleteMacro ~w~n",[M])
   ;  
      format("Macro ~w deleted.~n",[M])
   ).

%Dragana,delete declaration D
delete_declaration(D):-
   retract(phospho(D,_)),
   (  
      have_gui
   -> 
      format("[GUI] deleteDeclaration ~w~n",[D])
   ;  
      format("Declaration ~w deleted.~n",[D])
   ).


% set a parameter P to value V

parameter('Time',_):-
   !,
  
   (
        have_gui
        ->
                format("[GUI] errors Impossible to redefine the value of Time.~n",[])
        ;
   write_line_col('Error'),
                 write('impossible to redefine the value of "Time".\n')
   ).



parameter(P,V):-
   (
      number(V)
   ->
      (
         (
            k_parameter(P,_)
         ->
            % retractall(k_parameter(P,_)),
            parameter_inplace(P,V,[])
         ;
            assertz(k_parameter(P,V))
         ),
         (
            have_gui
         ->
            format("[GUI] param ~w,~p~n",[P,V])
         ;
            true
         )
      )
   ;
      (
         
        (
                have_gui
                        ->
                                format("[GUI] errors Parameter value must be a real number.~n",[])
                        ;
         write_line_col('Error'),
         write('parameter value must be a real number.\n')
      )
      )
   ).

% change parameter value, keeping the order of asserted k_parameter clauses.
parameter_inplace(P,V,L):-
   retract(k_parameter(Q,W)),!,
   (
      (P=Q)
   ->
      parameter_list([(Q,V)|L])
   ;
      parameter_inplace(P,V,[(Q,W)|L])
   ).

parameter_list([]).
parameter_list([(P,V)|L]):-
   asserta(k_parameter(P,V)),
   parameter_list(L).

parameter(P):-
   (
      k_parameter(P,V)
   ->
      format("~w = ~p~n",[P,V])
   ;
      
      (
         have_gui
      ->
                format("[GUI] errors Uknown parameter ~w.~n",[P])
   ;
      format("unknown parameter: ~w~n",[P])
        )
   ).

% set a macro M to value V

macro('Time',_):-
   !,
  
   (
        have_gui
                ->
                        format("[GUI] errors Impossible to redefine the value of Time.~n",[])
                ;
   write_line_col('Error'),
                         write('impossible to redefine the value of "Time".\n')
   ).

macro(M,V):-
   retractall(k_macro(M,_)),
   assertz(k_new_macro(M)),   % do NOT plot until new simu
(
           have_gui
         ->
             format("[GUI] macro ~w,~p~n",[M,V])
            ;
            true
        ),
   parse_rate_law(V,[],[],VV),
   assertz(k_macro(M,VV)).

macro(M):-
   (
      k_macro(M,V)
   ->
      format("~w = ~p~n",[M,V])
   ;
     
        (
                have_gui
        ->
                format("[GUI] errors Uknown macro ~w.~n",[M])
        ;
      format("unknown macro: ~w~n",[M])
        )
   ).


check_macros(ML):-
   k_macro(H,_),
   (
      eval2(H,ML,_)
   ->
      fail
   ;
        (
                have_gui
        ->
                format("[GUI] errors Looping macro definition or undefinied macro/parameter.~n",[])
        ;
                true
        ),
      throw(error('looping macro definition or undefined macro/parameter',H))
   ).

check_macros(_).
   
% FIXME misses nested macro use of a parameter...
check_params(L):-
   k_parameter(K,_),
   format_debug(6,"checking parameter ~w~n",[K]),
   (
      subtermrec(K,L)
   ->
      format_debug(6,"~w appears in the ODEs~n",[K]),
      true
   ;
      (
         k_macro(M,V),
         subterm(K,V),
         subtermrec(M,L)
      ->
         format_debug(6,"~w appears in a macro which appears in the ODEs~n",[K]),
         true
      ;
         (
            initconc(_,K)
         ->
            format_debug(6,"~w appears in the initial state~n",[K]),
            true
         ;
            (
               (event3(C,_,_);tevent(_,C,_,_);tevent(C,_,_,_)),
               subterm(K,C)
            ->
               format_debug(6,"~w appears in event condition~n",[C]),
               true
            ;  
               (  
                  have_gui
               -> 
                  format("[GUI] warnings The parameter ~w is defined but never used.~n",[K])
               ;  
                  write_line_col('Warning'),
                  format("the parameter ~w is defined but never used~n",[K])
               )
            )
         )
      )
   ),
   fail.

check_params(_).

subterm(A, A) :-
   !.

subterm(A,B):-
   compound(B),
   B =.. [_|T],
   subtermrec(A,T).

subtermrec(A,[H|_]):-
   subterm(A,H),!.
subtermrec(A,[_|T]):-
   subtermrec(A,T).


% Volumes

volume(Loc,Vol):-
   parse_rate_law(Vol,[],[],V),
   retractall(k_volume(Loc,_)),
   assertz(k_volume(Loc,V)),
(
           have_gui
         ->
             format("[GUI] volume ~w,~p~n",[Loc,Vol])
            ;
            true
        ).


volume(P):-
   (
      k_volume(P,V)
   ->   
      format("~w has volume ~p~n",[P,V])
   ; 
      (
         have_gui
      ->
         format("[GUI] warnings Uknown location: ~w. The volume will be assumed as default (equal to 1).~n",[P])
      ;
         format("unknown location: ~w~nvolume will be assumed equal to 1~n",[P])
      )
   ).

list_volumes:-
   list_volumes(user_output).

list_volumes(F):-
   writeall(F,k_volume(P,V),volume(P,V)),
   nl(F).

% FIXME?: if an error occurs during eval, the macro will not be re-asserted
eval2(A,VL,V):-
   k_macro(A,M),!,
   retractall(k_macro(A,M)),
   (
      eval2(M,VL,V)
   ->
      assertz(k_macro(A,M))
   ;
      (assertz(k_macro(A,M)),
       fail)
   ).

eval2(A,VL,V):-
   eval_conditional_func(A,VL,V).

% listing and exporting functions

list_parameters:-
   list_parameters(user_output).

list_parameters(F):-
   writeall(F,(k_parameter(P,V),format_to_atom(A,"~p",[V])),parameter(P,A)),
   nl(F).

list_macros:-
   list_macros(user_output).

list_macros(F):-
   writeall(F,(k_macro(M,V),atomic(M)),macro(M,V)),
   nl(F).

export_param(F):-
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
   write(S,'% BIOCHAM file'),nl(S),nl(S),
	format_debug(5,"comments done...~n",[]),
	list_parameters(S), nl(S),nl(S), nl(S),
	format_debug(5,"parameters done...~n",[]),
	close(S),
	(rules_added -> assertset(new_file); true).

export_init(F):-
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
   write(S,'% BIOCHAM file'),nl(S),nl(S),
	format_debug(5,"comments done...~n",[]),
   write(S,'% Initial state'),nl(S),
	initial_state(S), nl(S),nl(S), nl(S),
	format_debug(5,"init state done...~n",[]),
   write(S,'% Macros'),nl(S),
	list_macros(S), nl(S),nl(S), nl(S),
	format_debug(5,"macros done...~n",[]),
   write(S,'% Parameters'),nl(S),
	list_parameters(S), nl(S),nl(S), nl(S),
	format_debug(5,"parameters done...~n",[]),
	close(S),
	(rules_added -> assertset(new_file); true).

print_parameters(F):-
   kplot_parameter('_all_'),
   !,
   (
      k_parameter(K, _),
      print_parameter(K, F),
      fail
   ;
      true
   ).

print_parameters(F):-
   kplot_parameter(M),
   print_parameter(M, F),
   fail.

print_parameters(_).

print_parameter(K, F) :-
   k_parameter(K, V),
   format(F, "~p~c", [V, 9]).

print_macros(F, VL):-
   kplot_macro('_all_'),
   !,
   (
      k_macro(M, _),
      print_macro(M, F, VL),
      fail
   ;
      nl(F)
   ).

print_macros(F,VL):-
   kplot_macro(M),
   print_macro(M, F, VL),
   fail.

print_macros(F,_):-
   nl(F).

print_macro(M, F, VL) :-
   eval(M, VL, V),
   format(F, "~p~c", [V, 9]).

%%% Rosenbrock method (with Shampine parameters)
%%% cf. Numerical Recipes in C++ chap 16

% File: strem to feed data to
% DYDT: list of expressions giving dY/dT
% InitX: start time
% InitY: list of pairs (Yi,Yi(0))
% N: time up to which we have to go to

rosenbrock(File,DYDT,InitX,InitY,N):-
   % formal differenciation
   differentiate_rec(DYDT,InitY,Jacob),

   % setup values
   % h: step size
   % x: time
   % hdid: step size done
   % value(_,V) V is list of var, concentration
   % hmax: max step size, to avoid not so nice graphs...
   HMAX is (N-InitX)/20,
   % g_read(step_size,H),
   g_assign(hmax,HMAX),
   g_assign(x,InitX),
   g_assign(fx,InitY), %FF
   init_event,
   (
      (InitX==0)
   ->
      H is HMAX/5,
      g_assign(h,H),
      retractall(value(_,_)),
      assertz(value(0,InitY)),
      assertz(k_value(0,InitY,[])),
      %assertset(k_time(0)),
      g_assign(k_time,g_array([0])),
      g_assign(k_time_units,2),
      % Fill file for time 0
      print_number_tabulation(File,0),
      pretty_print(File,InitY),
      print_parameters(File),
      print_macros(File,InitY),
      handle_events(0,0,[],InitY,_,_)
   ;
      g_read(h,H)
   ),
   repeat,
   retract(value(_,Y)),
   g_read(x,X),
   format_debug(5,"entering step for ~p~n",[X]),
   rosenbrock_step(DYDT,Jacob,Y,X,YY),
   g_read(hdid,HH), % 0 initially
   XX is X+HH,
   handle_events(X,XX,Y,YY,XXX,YYY),
   init_time_event,
   g_read(time_event_time,TE),
   (  TE=<XXX
   -> 
      g_assign(x,TE),
      g_assign(fx, YYY), %FF should be a linear approximation between Y and YYY using ponderate_vect
      handle_time_events
      % trace_step(File,TE,YYY) %FF to remove if causes problems with QFLTL(R) constraints
   ;
      true),
   (XX==XXX
   ->
      true
   ;
      HDID is XXX-X,
      g_assign(hdid,HDID) %FF prudence but not sure of the impact
   ),
   trace_step(File,XXX,YYY),
   assertz(value(sylvain,YYY)), % FF ???
   g_assign(x,XXX),
   g_assign(fx,YYY), %FF
   XXX >= N,!,
   end_event,
   g_assign(hdid,0).

rosenbrock_step(DYDT,Jacob,Y,_X,Y4):-
   g_read(k_rbk_safe,SAFE),
   g_read(k_rbk_grow,GROW),
   g_read(k_rbk_pgrow,PGROW),
   g_read(k_rbk_shrink,SHRINK),
   g_read(k_rbk_pshrink,PSHRINK),
   g_read(k_rbk_errcon,ERRCON),
   g_read(k_rbk_gam,GAM),
   g_read(k_rbk_a21,A21),
   g_read(k_rbk_a31,A31),
   g_read(k_rbk_a32,A32),
   g_read(k_rbk_c21,C21),
   g_read(k_rbk_c31,C31),
   g_read(k_rbk_c32,C32),
   g_read(k_rbk_c41,C41),
   g_read(k_rbk_c42,C42),
   g_read(k_rbk_c43,C43),
   g_read(k_rbk_b1,B1),
   g_read(k_rbk_b2,B2),
   g_read(k_rbk_b3,B3),
   g_read(k_rbk_b4,B4),
   g_read(k_rbk_e1,E1),
   g_read(k_rbk_e2,E2),
   g_read(k_rbk_e3,E3),
   g_read(k_rbk_e4,E4),
   
   repeat,
      g_read(h,H),
      g_assign(hdid,H),
      length(DYDT,N),
      
      % evaluate Jacobian at current time point
      eval_matrix(Jacob,Y,DFDY),
      format_debug(5,"Jacobian evaluated~n",[]),
      format_debug(9,"Jacob = ~w     Y = ~w~n",[Jacob, Y]),
      format_debug(9,"DFDY = ~w~n",[DFDY]),
      
      % fill the left hand side of AX=B
      GGAM is 1.0/(GAM*H),
      fill_diag_matrix(N,GGAM),
      %matrix_sub(AA,DFDY,A),
      %init_matrix(A),
      sub_matrix(DFDY),

      % LU decompose A
      ludcmp_matrix,
      %get_matrix(AAAAAA),
      %write_matrix(AAAAAA),nl,
      %g_read(formal_index,g_array(INDX)),
      %write(INDX),nl,
      format_debug(5,"Left side evaluated~n",[]),
      
      % fill the right hand side, G1
      eval_line(DYDT,Y,G1),
      format_debug(5,"Right side evaluated~n",[]),
      format_debug(9,"DYDT = ~w     Y = ~w~n",[DYDT, Y]),
      format_debug(9,"G1 = ~w~n",[G1]),

      % compute by backsubstitution
      lusubst(G1,G1X),
      format_debug(5,"G1=~p~n",[G1X]),

      % get the new values at time + A2X * H
      inc_val_list(Y,A21,G1X,Y1),
      format_debug(9,"Y1 = ~w~n",[Y1]),
      eval_line(DYDT,Y1,DYDT1),
      format_debug(5,"DYDT at +A2X = ~p~n",[DYDT1]),

      % fill rhs again for G2
      C21H is C21/H,
      inc_vector(DYDT1,C21H,G1X,G2),
      format_debug(5,"Right side evaluated~n",[]),

      % compute by backsubstitution
      lusubst(G2,G2X),
      format_debug(5,"G2=~p~n",[G2X]),

      % get the new values at time + (A2X+A3X) * H
      inc_val_list(Y,A31,G1X,Y21),
      inc_val_list(Y21,A32,G2X,Y2),
      eval_line(DYDT,Y2,DYDT2),
      format_debug(5,"DYDT at +A2X+A3X = ~p~n",[DYDT2]),

      % fill rhs again for G3
      C31H is C31/H,
      C32H is C32/H,
      inc_vector(DYDT2,C31H,G1X,G31),
      inc_vector(G31,C32H,G2X,G3),
      format_debug(5,"Right side evaluated~n",[]),

      % compute by backsubstitution
      lusubst(G3,G3X),
      format_debug(5,"G3=~p~n",[G3X]),

      % for G4 no need to recompute Y and DYDT
      C41H is C41/H,
      C42H is C42/H,
      C43H is C43/H,
      inc_vector(DYDT2,C41H,G1X,G41),
      inc_vector(G41,C42H,G2X,G42),
      inc_vector(G42,C43H,G3X,G4),
      format_debug(5,"Right side evaluated~n",[]),

      % compute by backsubstitution
      lusubst(G4,G4X),
      format_debug(5,"G4=~p~n",[G4X]),

      % now get 4th order estimate for Y
      inc_val_list(Y,B1,G1X,Y41),
      inc_val_list(Y41,B2,G2X,Y42),
      inc_val_list(Y42,B3,G3X,Y43),
      inc_val_list(Y43,B4,G4X,Y4),
      format_debug(5,"Y 4th order evaluated~n",[]),
      
      % Estimate error
      g_assign(error,g_array(N)),
      g_read(error,g_array(Err0)),
      inc_vector(Err0,E1,G1X,Err1),
      inc_vector(Err1,E2,G2X,Err2),
      inc_vector(Err2,E3,G3X,Err3),
      inc_vector(Err3,E4,G4X,Err),
      scale_error(Err,Y,ErrList),
      max_list(ErrList,Erro),
      format_debug(5,"Error computed~n",[]),
      format_debug(9,"Erro = ~w~n",[Erro]),

      % check error
      g_read(step_doubling_error,ErrMax),
      Error is Erro/ErrMax,
      format_debug(9,"Error = ~w~n",[Error]),
      
      (
         (
            Error > 1.0
         ;
            \+(Error > log(0))   % catch the case where Error=nan
         )
      ->
         % failure: reduce the step
         % TODO if not yet at maxtry
         (
            H < ErrMax/1000
         ->
            (
               k_fail
            ->
               true
            ;
               assertz(k_fail),
               format_debug(0,"Warning: failure with minimal step, ",[]),
               format_debug(0,"error will not be guaranteed~n",[])
            )
         ;
            H1 is SAFE*H*(Error**PSHRINK),
            H2 is SHRINK*H,
            max_list([H1,H2],HNext),
            g_assign(h,HNext),
            format_debug(4,"error ~p, candidates ~p ~p~n",[Erro,H1,H2]),
            format_debug(4,"step size ~p failed, trying ~p...~n",[H,HNext]),
            fail
         )
      ;
         % success, try increasing the step size
         (
            Error > ERRCON*ErrMax
         ->
            HNext is SAFE*H*(Error**PGROW)
         ;
            HNext is H*GROW
         ),
         format_debug(5,"step size ~p ok, trying ~p...~n",[H,HNext]),
         g_read(hmax,HMAX),
         (
            HNext < HMAX
         ->
            g_assign(h,HNext)
         ;
            true
         )
      ),
         
      !,
   true.

% Events handling

init_event :-
   retractall(event_true(_,_)),
   retractall(event(_,_)),
   init_time_event,
   init_event2.

% compute next time event time

init_time_event:-
   g_assign(time_event_time,1000000), % maximum simulation time
   g_read(x,M),
   g_read(fx,VL),
   tevent(TE,_,_,_),
   eval(TE,VL,R),
   M=<R,
   format_debug(3,"There is an event for time ~p~n",[R]),
   g_read(time_event_time,T),
   R<T,
   g_assign(time_event_time,R),
   fail.
init_time_event.


% store initial values...

init_event2 :-
   (event3(_,L,_);tevent(_,_,L,_)),
   init_event3(L),
   fail.
init_event2.
   
init_event3([]).
init_event3([P|L]) :-
   k_parameter(P,V),!,
   assertz(event(P,V)),
   init_event3(L).

% Handling time events programmed for current time x
handle_time_events:-
   g_read(x,T),
   g_read(fx,VL),
   format_debug(3,"handling time ~p events in state ~p~n",[T,VL]),
   findall((P,V), (tevent(E,C,P,V),eval(E,VL,T),test_conditions(C,VL)), L),
   do_new_values_list(L,VL).


% Currently obsolete:
% handling iteratively all time events between current time and next time M1 excluded
% the time expression may depend on the concentration values

handle_time_events_before(M1):-
   g_read(x,M),
   g_read(fx,VL),
   format_debug(3,"handling time events in time [~p,~p[ in state ~p~n",[M,M1,VL]),
   repeat,
   (  tevent(E,C,P,V),
      % eval(E,[],T) for raising an error if the event time depends on concentrations
      eval(E,VL,T), 
      M=<T,T<M1
   -> 
      test_conditions(C,VL),
      do_new_values_list(P,V),
      fail
   ;
      !).


% handling events

handle_events(OldT,T,OldVL,VL,_,_):-
   event3(Condition,Param,NewValue),
   (
      test_conditions(Condition,VL)
   ->
      (
         event_true(Condition,NewValue)
      ->
         true
      ;
         format_debug(3,"~p became true between time ~p and ~p~n~p~n~p~n",[Condition,OldT,T,OldVL,VL]),
         event_change(Condition,OldT,T,OldVL,VL, Param, NewValue)
      )
   ;
      retractall(event_true(Condition,NewValue))
   ),
   fail.

handle_events(_OldT,T,_OldVL,VL,NewX,NewY):-
   findall((TL, V, C, P, NV), event_change(TL, V, C, P, NV), L),
   retractall(event_change(_, _, _, _, _)),
   (
      (L=[])
   ->
      (NewX=T),
      (NewY=VL)
   ;
      get_min_time(L, NewX, NewY, Conds, Param, NewValue),
      assertz(k_macro('Time', NewX)), % masking the evaluation of Time by a temporary macro !
      do_new_values(Param, NewValue, NewY),
      recompute_truth(Conds, NewY),
      retractall(k_macro('Time', _)),
      format_debug(5,"change(s) approx at ~p~n",[NewX])
   ).


do_new_values_list([],_VL).
do_new_values_list([(P,V)|L],VL):-
   do_new_values(P,V,VL),
   do_new_values_list(L,VL).

do_new_values([], [], _).
do_new_values([Param|PL], [NewValue|NVL], VL) :-
   eval_conditional_func(NewValue,VL,V),
   retract(k_parameter(Param,_)),
   assertz(k_parameter(Param,V)),
   format_debug(1,"parameter ~p gets value ~p~n",[Param,V]),
   do_new_values(PL, NVL, VL).

% recompute truth only of events that just fired
% actually check after the do_new if indeed True
% since firing the event might have it back to False...
recompute_truth(Conds, NewY) :-
   member(Condition, Conds),
   event3(Condition, _, NewValue),
   (
      test_conditions(Condition, NewY)
   ->
      assertz(event_true(Condition, NewValue))
   ;
      true
   ),
   fail.

recompute_truth(_Conds, _NewY).

% Find dichotomically where C became true
% use Rosenbrock specific HMAX

event_change(C, T1, T2, _V1, V2, Param, NewValue):-
   g_read(step_doubling_error, ErrMax),
   (
      ErrMax=0
   ;
      g_read(hmax, HMAX),
      T2-T1 < min(HMAX/1000, ErrMax)
   ),
   !,
   format_debug(4,"~p taken true at time ~p~n~p~n",[C,T2,V2]),
   %FF despite the good job of dichotomic search
   %   always overapproximating the time introduces a bias
   %   which is visible in ball.bc and event-based stochastic simulations
   assertz(event_change(T2, V2, C, Param, NewValue)).

event_change(C,T1,T2,V1,V2, Param, NewValue):-
   T3 is (T1+T2)/2,
   middle_vect(V1,V2,V3),
   assertz(k_macro('Time',T3)),
   (
      test_conditions(C,V3)
   ->
      retract(k_macro('Time',_)),
      event_change(C,T1,T3,V1,V3, Param, NewValue)
   ;
      retract(k_macro('Time',_)),
      event_change(C,T3,T2,V3,V2, Param, NewValue)
   ).

% get the highest time where all conditions changed

get_min_time([(T, VL, C, P, NV)], T, VL, [C], P, NV):-!.
get_min_time([(T, VL, C, P, NV) | L], T2, VL2, C2, P2, NV2):-
   get_min_time(L, T1, VL1, C1, P1, NV1),
   (
      T =:= T1
   ->
      (T2=T1),
      (VL2=VL1),
      (C2=[C | C1]),
      append(P, P1, P2),
      append(NV, NV1, NV2)
   ;
      T < T1
   ->
      (T2=T),
      (VL2=VL),
      (C2=[C]),
      (P2=P),
      (NV2=NV)
   ;
      (T2=T1),
      (VL2=VL1),
      (C2=C1),
      (P2=P1),
      (NV2=NV1)
   ).

% compute the average of two (key,value) lists

middle_vect([],[],[]).
middle_vect([(X,V1)|T1],[(X,V2)|T2],[(X,V3)|T3]):-
   V3 is (V1+V2)/2,
   middle_vect(T1,T2,T3).

ponderate_vect(L1,A,L2,B,L3):-
   mult_list(L1,A,LL1),
   mult_list(L2,B,LL2),
   add_two_list(LL1,LL2,L3).


% put init values back
 
end_event :-
  retract(event(P,V)),
  retract(k_parameter(P,_)),
  assertz(k_parameter(P,V)),
  fail.

end_event.

add_event(C, P, V) :-
   list(P),!,
   assertz(event3(C, P, V)),
   (
      have_gui
   ->
      format("[GUI] add_event ~w;~w;~w~n",[C, P, V])
   ;
      true
   ).

add_event(C,P,V):-
   assertz(event3(C,[P],[V])),
   (
      have_gui
   ->
      format("[GUI] add_event ~w;~w;~w~n",[C, [P], [V]])
   ;
      true
   ).

event(C,P,V):- add_event(C,P,V).

time_event(T,C,P,V):- add_time_event(T,C,P,V).

add_time_event(T, C, P, V) :- 
   list(P),!,
   assertz(tevent(T, C, P, V)),
   (
      have_gui
   ->
      format("[GUI] add_time_event ~w;~w;~w;~w~n",[T, C, P, V])
   ;
      true
   ).

add_time_event(T,C,P,V):-
   assertz(tevent(T,C,[P],[V])),
   (
      have_gui
   ->
      format("[GUI] add_time_event ~w;~w;~w~n",[T,C, [P], [V]])
   ;
      true
   ).

list_events:-
   list_events(user_output).

list_events(F):-
   writeall(F,tevent(T,C,P,V),time_event(T,C,P,V)),
   writeall(F,event3(C,P,V),event(C,P,V)).

delete_events:-
   retractall(event3(_,_,_)),
   retractall(tevent(_,_,_,_)),
   (
      have_gui
   ->
      format("[GUI] delete_all_events~n",[])
   ;
      true
   ).

delete_event(C,P,V):-
   list(P),!,
   (
      retract(event3(C,P,V))
   ->
      (
         have_gui
      ->
         format("[GUI] delete_event ~w;~w;~w~n",[C, P, V])
      ;
      true
      )
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The corresponding event [~w,~w,~w] was not found.~n",[C,P,V])
   ;
      write_line_col('Warning'),
      format("the corresponding event was not found~n",[])
        )
   ).

delete_event(C,P,V):-
   delete_event(C, [P], [V]).

delete_time_event(T,C,P,V):-
   list(P),!,
   (
      retract(tevent(T,C,P,V))
   ->
      (
         have_gui
      ->
         format("[GUI] delete_event ~w;~w;~w;~w~n",[T,C, P, V])
      ;
      true
      )
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The corresponding time event [~w,~w,~w,~w] was not found.~n",[T,C,P,V])
   ;
      write_line_col('Warning'),
      format("the corresponding time event was not found~n",[])
        )
   ).

delete_time_event(T,C,P,V):-
   delete_time_event(T,C, [P], [V]).

% Compute the Jacobian from a list of DFDY and a list of Y (actually pairs
% (Y,Y0)

differentiate_rec([],_,[]).

differentiate_rec([D|DYDT],LVar,[J|Jacob]):-
   unmacro(D,DD),
   diff_rec(DD,LVar,J),
   differentiate_rec(DYDT,LVar,Jacob).

diff_rec(_,[],[]).

diff_rec(E,[(X,_)|L],[DDE|DEDL]):-
   differentiate(E,X,DE),
   simplify(DE,DDE),
   diff_rec(E,L,DEDL).

% replace macros and parameters by their values

unmacro(if C then D else E,if C then DD else EE):-
   !,unmacro(D,DD),
   unmacro(E,EE).

unmacro(X+Y,XX+YY) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(X-Y,XX-YY) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(X*Y,XX*YY) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(X/Y,XX/YY) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(X^Y,XX^YY) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(min(X,Y),min(XX,YY)) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(max(X,Y),max(XX,YY)) :-
   !,unmacro(X,XX),
   unmacro(Y,YY).
unmacro(-X,-XX) :-
   !,unmacro(X,XX).
unmacro(log(X),log(XX)) :-
   !,unmacro(X,XX).
unmacro(exp(X),exp(XX)) :-
   !,unmacro(X,XX).
unmacro(sin(X),sin(XX)) :-
   !,unmacro(X,XX).
unmacro(cos(X),cos(XX)) :-
   !,unmacro(X,XX).
unmacro(abs(X),abs(XX)) :-
   !,unmacro(X,XX).
unmacro(random,random):-
   !.
unmacro(frac(X), frac(XX)) :-
   !,
   unmacro(X,XX).
unmacro(X,XXX) :-
   k_macro(X,XX),!,
   unmacro(XX,XXX).
unmacro(X,XX) :-
   k_parameter(X,XX),!.
unmacro(X,X).

%% evaluate in a matrix

eval_matrix([],_,[]).
eval_matrix([H|T],Y,[EH|ET]):-
   eval_line(H,Y,EH),
   eval_matrix(T,Y,ET).

eval_line([],_,[]).
eval_line([H|T],Y,[EH|ET]):-
   eval_conditional_func(H,Y,EH),!,
   eval_line(T,Y,ET).

%% increment a (name,value) list by a vector V times a constant C
inc_val_list([],_C,_V,[]).
inc_val_list([(X,Val)|L],C,[V|VV],[(X,VVal)|LL]):-
   VVal is Val + C*V,
   inc_val_list(L,C,VV,LL).

%% same for a simple vector
inc_vector([],_C,_V,[]).
inc_vector([Val|L],C,[V|VV],[VVal|LL]):-
   VVal is Val + C*V,
   inc_vector(L,C,VV,LL).

%% scale error vector with absolute value of variable (in Name,Value list)
%% if that |value| is > 1
scale_error([],[],[]).

scale_error([E|Err],[(_,V)|L],[SE|SErr]):-
   (
      abs(V) < 1
   ->
      SE is abs(E)
   ;
      SE is abs(E/V)
   ),
   scale_error(Err,L,SErr).

%%% Simple check to avoid nonsensical rules
check_kinetics(L,V) :-
   format_debug(7, "checking kinetics for ~w with ~w~n",[V, L]),
   unmacro(V,V1),
   format_debug(7, "checking kinetics for ~w~n",[V1]),
   (
      check_reactant_kinetics(L, V1)
   ->
      check_kinetics0(L,V1)
   ;
      true
   ),
   check_negative_kinetics(V, V1),
   format_debug(7, "checking kinetics done~n",[]).


% check_negative_kinetics(+Original, +Expression)
%
% simple syntactic check for substraction indicating that the Expression might
% become negative for some point in the phase space
check_negative_kinetics(Original, Expression) :-
   (
      check_negative_kinetics_aux(Expression)
   ->
      true
   ;
      errormsg(
         warning,
         "The kinetic expression ~w might become negative or undefined~nin some points of the phase space~n",
         [Original]
      )
   ).

check_negative_kinetics_aux('Time').

check_negative_kinetics_aux(abs(_)).

check_negative_kinetics_aux(random(_)).

check_negative_kinetics_aux(exp(_)).

check_negative_kinetics_aux([_]).

% we might miss a warning here if Expr < 1
check_negative_kinetics_aux(log(_)).

check_negative_kinetics_aux(frac(X)) :-
   check_negative_kinetics_aux(X).

check_negative_kinetics_aux(X + Y) :-
   check_negative_kinetics_aux(X),
   check_negative_kinetics_aux(Y).

check_negative_kinetics_aux(X * Y) :-
   check_negative_kinetics_aux(X),
   check_negative_kinetics_aux(Y).

check_negative_kinetics_aux(X / Y) :-
   check_negative_kinetics_aux(X),
   check_negative_kinetics_aux(Y).

check_negative_kinetics_aux(min(X, Y)) :-
   check_negative_kinetics_aux(X),
   check_negative_kinetics_aux(Y).

check_negative_kinetics_aux(sq_wave(X, _, Y, _, Z, _)) :-
   check_negative_kinetics_aux(X),
   check_negative_kinetics_aux(Y),
   check_negative_kinetics_aux(Z).

check_negative_kinetics_aux(sq_wave(X, _, Y, _)) :-
   check_negative_kinetics_aux(X),
   check_negative_kinetics_aux(Y).

% OK if at least one is non-negative
check_negative_kinetics_aux(max(X, Y)) :-
   (
      check_negative_kinetics_aux(X)
   ;
      check_negative_kinetics_aux(Y)
   ).

check_negative_kinetics_aux(X ^ _) :-
   check_negative_kinetics_aux(X).

% check_negative_kinetics_aux(X) :-
%    number(X).
% 
% check_negative_kinetics_aux(X) :-
%    k_parameter(X, _).

check_negative_kinetics_aux(X) :-
   atomic(X).

check_negative_kinetics_aux(X) :-
   format_debug(3, "negative here: ~w~n", [X]),
   !,
   fail.

% this might be too nice...
% check_negative_kinetics_aux(X - Y) :-
%    check_negative_kinetics_aux(X),
%    \+ check_negative_kinetics_aux(Y).
% 
% check_negative_kinetics_aux(- Y) :-
%    \+ check_negative_kinetics_aux(Y).


% check_reactant_kinetics(+ListOfReactants, +Kinetics)
%
% checks that all reactants appear in the kinetics and vice-versa

check_reactant_kinetics(L, V) :-
   (
      check_kinetics_in_reactants(V, L),
      !
   ;
      true
   ),
   check_reactants_in_kinetics(L, V).


% check_reactant_in_kinetics(+ListOfReactants, +Kinetics)
%
% checks that all reactants appear in the kinetics

check_reactants_in_kinetics([], _).

check_reactants_in_kinetics([(_, M) | L], V) :-
   format_debug(8, "reactant ~w in kinetic exression ~w?~n", [M, V]),
   (
      subterm([M], V)
   ->
      true
   ;
      errormsg(
         warning,
         "The reactant ~w does not appear in the kinetics~n",
         [M]
      ),
      fail
   ),
   check_reactants_in_kinetics(L, V).


% check_kinetics_in_reactants(+Kinetics, +ListOfReactant)
%
% checks that all compounds of the kinetics are reactants

check_kinetics_in_reactants([M], L) :-
   !,
   format_debug(8, "compound ~w of kinetics in reactant list ~w?~n", [M, L]),
   (
      memberchk((_, M), L)
   ->
      true
   ;
      errormsg(
         warning,
         "The compound ~w appears in the kinetics but is not a reactant~n",
         [M]
      ),
      fail
   ).


check_kinetics_in_reactants(C, L) :-
   compound(C),
   !,
   C =.. [_ | Args],
   check_kinetics_in_reactants_rec(Args, L).

check_kinetics_in_reactants(_, _).


check_kinetics_in_reactants_rec([], _).

check_kinetics_in_reactants_rec([H | T], L) :-
   check_kinetics_in_reactants(H, L),
   check_kinetics_in_reactants_rec(T, L).


check_kinetics0([],_).

check_kinetics0([(_,M)|L],V):-
   replace([([M],0)],V,V1),
   format_debug(8, "kinetics become ~w when ~w=0~n", [V1, M]),
   % TODO better warning for the div by zero case...
   catch(simplify(V1,V2), error(evaluation_error(zero_divisor), _), V2=1),
   format_debug(8, "kinetics become ~w after simplify~n", [V2]),
   (
      has_val(V2,0)
   ->
      check_kinetics0(L,V)
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The reaction ~w seems to be able to proceed even when [~w]=0 ....~n",[V2,M])
        ;
      write_line_col('Warning'),
      write('this reaction seems to be able to proceed\n'),
                format("even when [~w] = 0 ...~n",[M])
        ),
      format_debug(3,"V = ~p~n",[V2])
   ).

% replace in T first members of pair-list L by rigth member gives TT
replace(L,A,B):-
   member((A,B),L),!.

replace(_,T,T):-
   atomic(T),!.

replace(A,T,TT):-
   T=..[F|L],
   replace_rec(A,L,LL),
   (
      (F=(^))
   ->
      (G=(**))
   ;
      (G=F)
   ),
   TT=..[G|LL].

replace_rec(_,[],[]).
replace_rec(A,[H|T],[HH|TT]):-
   replace(A,H,HH),
   replace_rec(A,T,TT).


critical_reaction_threshold:-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      g_read(critical_threshold,N),
      format("Critical reaction threshold: ~w~n",[N])
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The critical reaction threshold is not available for numerical method ~w.~n",[M])
        ;
      format("The critical reaction threshold is not available for numerical method ~w.~n",[M])
        )
   ).

critical_reaction_threshold(N):-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      (
         N > 0,
         integer(N)
      ->
         g_assign(critical_threshold,N),
         critical_reaction_threshold
      ;
         
        (
                have_gui
        ->
                format("[GUI] warnings The threshold needs to be a integer greater than 0.~n",[])
        ;
         format("The threshold needs to be a integer greater than 0.~n",[])
      )
      )
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The critical reaction threshold is not available for numerical method ~w.~n",[M])
        ;
      format("The critical reaction threshold is not available for numerical method ~w.~n",[M])
        )
   ).

conversion_factor:-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      g_read(conversion_factor,N),
      format("Conversion rate: ~w~n",[N])
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The conversion rate is only for tau-leaping or ssa.~n",[])
        ;
      format("The conversion rate is only for tau-leaping or ssa.~n",[])
        )
   ).

conversion_factor(N):-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      (
         N > 0,
         integer(N)
      ->
         g_assign(conversion_factor,N),
         conversion_factor
      ;
         
        (
                have_gui
        ->
                format("[GUI] warnings The rate needs to be a integer greater than 0.~n",[])
        ;
         format("The rate needs to be a integer greater than 0.~n",[])
      )
      )
   ;
      
        (
                have_gui
        ->
                format("[GUI] warnings The conversion rate is only for tau-leaping or ssa.~n",[])
   ;
      format("The conversion rate is only for tau-leaping or ssa.~n",[])
        )
   ).

/* CHH
filtering:-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      g_assign(filtering,1),
      g_read(drawing_period,N1),
      g_read(mean_order,N2),
      format("The simulation result will be selected among at most ~w results,",[N1]),
      format("and averged among ~w neighbors.~n",[N2])
   ;
      format("This command is only for tau-leaping or ssa.~n",[])
   ).
filtering(N1,N2):-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      (
         N1 > 2, N2 > 0,
         integer(N1),
         integer(N2)
      ->
         g_assign(drawing_period,N1),
         g_assign(mean_order,N2),
         filtering
      ;
         format("The period needs to be a integer greater than 2, ",[]),
         format("and the dimension needs to be a positive integer.~n",[])
      )
   ;
      format("This command is only for tau-leaping.~n",[])
   ).
no_filtering:-
   g_read(k_method,M),
   (
      (M = tl;M = ssa)
   ->
      g_assign(filtering,0),
      format("Keep all simulation results.~n",[])
   ;
      format("This command is only for tau-leaping.~n",[])
   ).
*/

convert_mole_to_number([],[]).
convert_mole_to_number([(Var,Val1)|UL],[(Var,Val2)|VL]):-
   g_read(conversion_factor,Rate),
   Val2 is Val1*Rate,
   convert_mole_to_number(UL,VL).

compute_alpha([],_,Alpha,[]):- !,Alpha = 0.0.
compute_alpha([(_,RL,_,_)|LL],U,Alpha,[Val|AL]):-
   eval_conditional_func(RL,U,Val),
   compute_alpha(LL,U,A,AL),
   Alpha is A + Val.
 
next_reaction([],[],_,_,_):-!,fail.
next_reaction([(_,_,_,V)|LL],[Val|AL],Bound,CA,EV):-
   NCA is CA + Val,
   (
      NCA >= Bound
   ->
      EV = V
   ;
      next_reaction(LL,AL,Bound,NCA,EV)
   ).
next_reaction([],_,_,_):-!,fail.
next_reaction([(_,Aj,Vj)|LL],Bound,CA,V):-
   NCA is CA + Aj,
   (
      NCA >= Bound
   ->
      V = Vj
   ;
      next_reaction(LL,Bound,NCA,V)
   ).

next_reaction1([],_,_,_,_):-!,fail.
next_reaction1([(Rj,Aj,Vj)|LL],Bound,CA,R,V):-
   NCA is CA + Aj,
   (
      NCA >= Bound
   ->
        V = Vj,
        R = Rj
   ;
      next_reaction1(LL,Bound,NCA,R,V)
   ).

undate_macros([],[],[],_):-!.
undate_macros([((Var,OVal))|OVectorL],[((Var,EVal))|EVectorL],[((Var,RVal))|RVectorL],Times):-
   !,
   RVal is OVal + EVal * Times,
   undate_macros(OVectorL,EVectorL,RVectorL,Times).

undate_macros([],[],[]):-!.
undate_macros([((Var,OVal))|OVectorL],[((Var,EVal))|EVectorL],[((Var,RVal))|RVectorL]):-
   !,
   RVal is OVal + EVal,
   undate_macros(OVectorL,EVectorL,RVectorL).

gillespie(U, Tau, L, V):-
   g_read(conversion_factor,Rate),
   Ratio is 1.0/Rate,
   mult_list(U,Ratio,TempU),
   compute_alpha(L,TempU,Alpha,AL),
   (
      Alpha =:= 0
   ->
      Tau=0,
      V=U
   ;
      repeat,
         random(R1),
         R1 =\= 0,
      !,
      random(R2),
      Tau is (1/Alpha)*log(1/R1),
      Bound is R2*Alpha,
      next_reaction(L,AL,Bound,0,EVector),
      undate_macros(U,EVector,V),
      !
   ).
 
reset_vector([],[]).
reset_vector([(Var,_)|YL],[(Var,0.0)|VL]):-
   reset_vector(YL,VL).

set_vector([],[],_).
set_vector([(Var,_)|YL],[(Var,N)|VL],N):-
   set_vector(YL,VL,N).

del_effect([],VL,VL).
del_effect(_,[],[]).
del_effect((Num,Macro),[(Var,Val)|YL],[(Var,NVal)|VL]):-
   (
      [Macro] == Var
   ->
      NVal is Val - Num,
      VL = YL
   ;
      NVal = Val,
      del_effect((Num,Macro),YL,VL)
   ).
del_effect([(Num,Macro)|KL],YL,VL):-
      del_effect((Num,Macro),YL,VL1),
      del_effect(KL,VL1,VL).

add_effect([],VL,VL).
add_effect(_,[],[]).
add_effect((Num,Macro),[(Var,Val)|YL],[(Var,NVal)|VL]):-
   (
      [Macro] == Var
   ->
      NVal is Val + Num,
      VL = YL
   ;
      NVal = Val,
      add_effect((Num,Macro),YL,VL)
   ),
   !.
add_effect([(Num,Macro)|KL],YL,VL):-
   add_effect((Num,Macro),YL,VL1),
   add_effect(KL,VL1,VL).

get_effect_vector([],_,[]).
get_effect_vector(L,Y,NewL):-
   L = [(R,KL,KR,RL)|LL],
   reset_vector(Y,V1),
   (
      KL = []
   ->
      V2 = V1
   ;
      del_effect(KL,V1,V2)
   ),
   (
      KR = []
   ->
      V3 = V2
   ;
      add_effect(KR,V2,V3)
   ),
   unmacro(RL,RLD),
   diff_rec(RLD,Y,DRLD),
   NewL = [(R,RL,DRLD,V3)|NewLL],
   get_effect_vector(LL,Y,NewLL).

/* CHH
average2([],[],[]).
average2([(Var,Fx1)|S1L],[(_,Fx2)|S2L],[(Var,Fxa)|AvgL]):-
   Fxa is (Fx1 + Fx2)/2,
   average2(S1L,S2L,AvgL).

condition_add_two_list([],[],[],[],F):- F = 0,!.
condition_add_two_list([(V,X)|L],[(V,Y)|M],[(V,Z)|N],[(V,B)|O],F) :- 
   g_read(conversion_factor,Rate),
   LowBound is Rate*0.02,
   (
      (B =< LowBound ; (abs((X-B)/B) < 0.1))
   ->
      Z is X+Y,
      condition_add_two_list(L,M,N,O,F)
   ;
      F = 1
   ),!.

sum_steplist([(X,Y)|SteplistL],(SumX,SumY)):-
   (
      SteplistL = []
   ->
      SumX = X,
      SumY = Y
   ;
      sum_steplist(SteplistL,(_SumX,_SumY)),
      SumX is X + _SumX,
      add_two_list(Y,_SumY,SumY)
   ).
sum_steplist([(X,Y)|SteplistL],(SumX,SumY),B,F,N):-
   (
      (SteplistL = [])
   ->
      SumX = X,
      SumY = Y,
      N = 1,
      F = 0
   ;
      sum_steplist(SteplistL,(_SumX,_SumY),B,_F,_N),
      (
         (_F = 1)
      ->
         SumX = _SumX,
         SumY = _SumY,
         N = _N,
         F = 1
      ;
         condition_add_two_list(Y,_SumY,TmpY,B,F),
         (
            (F = 1)
         ->
            SumX = _SumX,
            SumY = _SumY,
            N = _N
         ;         
            SumX is X + _SumX,
            SumY = TmpY,
            add_two_list(Y,_SumY,SumY),
            N is _N + 1
         )
      )
   ).

line_equation(_,[],[],_,[],[],[]).
line_equation(DeltaX,[(_,SY)|SYL],[(_,EY)|EYL],AvgX,[(_,AvgY)|AvgYL],[S|SlopL],[C|CL]):-
   S is (EY - SY)/DeltaX,
   C is AvgY - AvgX*S,
   line_equation(DeltaX,SYL,EYL,AvgX,AvgYL,SlopL,CL).

next_draw(Steplist,M,U,Xm,V,WX,WY):-
   g_read(drawing_period,Period),
   Steplist = [(Xtn,Ytn),(Xtn_1,Ytn_1)|SteplistL],
   SX is (Xtn + Xtn_1)/2,
   EX is (M + Xm)/2,
   average2(Ytn,Ytn_1,SY),
   average2(U,V,EY),
   DeltaX is EX - SX,
   sum_steplist(SteplistL,(SumX,SumY)),
   AvgX is SumX/(Period-2),
   mult_list(SumY,1/(Period-2),AvgY),
   line_equation(DeltaX,SY,EY,AvgX,AvgY,Slop,C),
   select_draw(SteplistL,Slop,C,WX,WY,_).

line_diff(_,[],[],[],0).
line_diff(X,[(_,Y)|YL],[S|SL],[C|CL],D):-
   line_diff(X,YL,SL,CL,_D),
   D is _D + abs(Y - S*X - C).

select_draw([],_,_,_,_,MinD):- fd_max_integer(MinD).
select_draw([(X,Y)|SteplistL],Slop,C,WX,WY,MinD):-
   select_draw(SteplistL,Slop,C,_WX,_WY,_MinD),
   line_diff(X,Y,Slop,C,D),
   (
      D < _MinD
   ->
        MinD = D,
        WX = X,
        WY = Y
   ;
        MinD = _MinD,
        WX = _WX,
        WY = _WY
   ).

undate_list([(X,Y)|SteplistL],Xm,V,WX,Newlist):-
   (
      X = WX
   ->
      append([(X,Y)|SteplistL],[(Xm,V)],Newlist)
   ;
      undate_list(SteplistL,Xm,V,WX,Newlist)
   ).

update_maximum([]).
update_maximum([(Var,Val)|YL]):-
   (
      k_maximum(Var,Max)
   ->
      true
   ;
      Max = 0,
      assertz(k_maximum(Var,0))
   ),
   (
      (Val > Max)
   ->
      retract(k_maximum(Var,_)),
      assertz(k_maximum(Var,Val))
   ;
      true
   ),
   %format("Max: ~w ~w~n",[Var,Max]),
   update_maximum(YL).
*/

tauleap_simulation(File,L,InitX,InitY,N) :-
   (
      InitX == 0
   ->
      g_assign(x,InitX),
      g_assign(fx,InitY),
      print_number_tabulation(File,InitX),
      pretty_print(File,InitY),
      print_parameters(File),
      print_macros(File,InitY),
      assertz(k_value(InitX,InitY,[])),
      %assertset(k_time(InitX)),
      g_assign(k_time,g_array([InitX])),
      g_assign(k_time_units,2),
      g_assign(steplist,[(InitX,InitY)]),
      g_assign(mavglist,[(InitX,InitY)]),
      %set_seed(0)
      randomize
   ;
      true
   ),
   %format("Test~n",[]),
   g_read(conversion_factor,Rate),
   /* CHH g_read(filtering,FilteringFlag),
   g_read(drawing_period,Period),
   g_read(mean_order,Order), */
   get_effect_vector(L,InitY,NewL),
   %format("L ~w~n",[L]),
   % CHH update_maximum(InitY),
   repeat,
   g_read(x,M),
   g_read(fx,U),
   tauleap(M,U,Tau,NewL,V),
   % CHH update_maximum(V),
   H is Tau/Rate,
   Xm is M + H,
   g_assign(h,H),
   ( /* CHH
      FilteringFlag = 1
   ->
      g_read(steplist,Steplist),  
      length(Steplist,Con),
      (
         ((Con < Period) , (Xm < InitX+N))
      ->
         append(Steplist,[(Xm,V)],Newlist)
      ;
         next_draw(Steplist,M,U,Xm,V,WX,WY),
         %%Average part
         g_read(mavglist,Mavglist),  
         append(Mavglist,[(WX,WY)],Tmplist),
         length(Tmplist,MCon),        
         (
           MCon =< Order
         ->
           Nmavglist = Tmplist
         ;
           Tmplist = [(_,_)|Nmavglist]
         ),
         %length(Nmavglist,NMCon),
         %sum_steplist(Nmavglist,(SumX,SumY)),
         sum_steplist(Nmavglist,(SumX,SumY),WY,_,NMCon),
         %format("NMCon: ~w~n",[NMCon]),
         AvgX is SumX/NMCon,
         mult_list(SumY,1/NMCon,AvgY),
         trace_step(File,AvgX,AvgY),
         g_assign(mavglist,Nmavglist),
         undate_list(Steplist,Xm,V,WX,Newlist)
     ),
     g_assign(steplist,Newlist)
   ; */
     trace_step(File,Xm,V)
   ),
   g_assign(x,Xm),
   g_assign(fx,V),
   (
      Tau =:= 0
   ->
      
        (
                have_gui
        ->
                format("[GUI] warnings No more possible reaction at time ~p~n",[Xm])
        ;
      format("Warning: no more possible reaction at time ~p~n",[Xm])
        )
   ;
      Xm >= InitX+N
   ),
   !.

ssa_simulation(File,L,InitX,InitY,N) :-
   (
      InitX == 0
   ->
      g_assign(x,InitX),
      g_assign(fx,InitY),
      print_number_tabulation(File,InitX),
      pretty_print(File,InitY),
      print_parameters(File),
      print_macros(File,InitY),
      assertz(k_value(InitX,InitY,[])),
      %assertset(k_time(InitX)),
      g_assign(k_time,g_array([InitX])),
      g_assign(k_time_units,2),
      g_assign(steplist,[(InitX,InitY)]),
      g_assign(mavglist,[(InitX,InitY)]),
      %set_seed(0)
      randomize
   ;
      true
   ),
   %format("Test~n",[]),
   g_read(conversion_factor,Rate),
   /* CHH g_read(filtering,FilteringFlag),
   g_read(drawing_period,Period),
   g_read(mean_order,Order), */
   get_effect_vector(L,InitY,NewL),
   %format("L ~w~n",[L]),
   % CHH update_maximum(InitY),
   repeat,
   g_read(x,M),
   g_read(fx,U),
   gillespie(U,Tau,NewL,V),
   % CHH update_maximum(V),
   H is Tau/Rate,
   Xm is M + H,
   g_assign(h,H),
   (
      /* CHH
      FilteringFlag = 1
   ->
      g_read(steplist,Steplist),  
      length(Steplist,Con),
      (
         ((Con < Period) , (Xm < InitX+N))
      ->
         append(Steplist,[(Xm,V)],Newlist)
      ;
         next_draw(Steplist,M,U,Xm,V,WX,WY),
         %%Average part
         g_read(mavglist,Mavglist),  
         append(Mavglist,[(WX,WY)],Tmplist),
         length(Tmplist,MCon),        
         (
           MCon < Order
         ->
           Nmavglist = Tmplist
         ;
           Tmplist = [(_,_)|Nmavglist]
         ),
         length(Nmavglist,NMCon),
         sum_steplist(Nmavglist,(SumX,SumY)),
         AvgX is SumX/NMCon,
         mult_list(SumY,1/NMCon,AvgY),
         trace_step(File,AvgX,AvgY),
         g_assign(mavglist,Nmavglist),
         undate_list(Steplist,Xm,V,WX,Newlist)
     ),
     g_assign(steplist,Newlist)
   ; */
     trace_step(File,Xm,V)
   ),
   g_assign(x,Xm),
   g_assign(fx,V),
   (
      Tau =:= 0
   ->
      
        (
                have_gui
        ->
                format("[GUI] warnings No more possible reaction at time ~p~n",[Xm])
        ;
      format("Warning: no more possible reaction at time ~p~n",[Xm])
        )
   ;
      Xm >= InitX+N
   ),
   !.

stochastic_solver(_,_,_,_,N):-   % Allow negative values to avoid simulating...
   N < 0, !.

stochastic_solver(DataFile,L,InitX,InitY,N) :-
   convert_mole_to_number(InitY,NewY),
   write('Initial state:\n'),
   writeall(member(([Molecule], Number), NewY), present(Molecule, Number)),
   nl,
   (
      (InitX==0)
   ->
      open(DataFile,write,File),
      write(File,'#Time\t'),
      kinit_data_file(NewY,File),
      retractall(k_value(_,_,_)),
      retractall(k_time(_))   % FF old code ????
   ;
      open(DataFile,append,File)
   ),
   g_read(k_method,M),
   statistics(cpu_time,_),
   (
      (M = tl)
   ->
      tauleap_simulation(File,L,InitX,NewY,N)
   ;
	   ssa_simulation(File,L,InitX,NewY,N)
   ),
   statistics(cpu_time,[_,T]),
   TT is T/1000,
   format_debug(0,"Simulation time: ~ps~n",[TT]),
   % store time info for trace querying
   %findall(Time,k_time(Time),TimeL),
   g_read(k_time,g_array(TimeL)),
   reverse(TimeL,TL), % get a list of time points in reverse order
   g_assign(trace_times,TL),
   retractall(k_neg_conc),
   retractall(k_fail),
   close(File).

sum_over_critical([],Sum,Sum):-!.   
sum_over_critical([(_,Val,_)|List],It,Sum):-
   NIt is Val + It,
   sum_over_critical(List,NIt,Sum).

tauleap(Xm,U,H,L,V):-
   g_read(conversion_factor,Rate),
   Ratio is 1.0/Rate,
   mult_list(U,Ratio,TempU),
   compute_alpha(L,TempU,Alpha,AL),
   (
      Alpha =:= 0
   ->
      H=0,
      V=U
   ;
      (
         critical_reaction_list(L,U,AL,CL,NCL),
         (
            NCL = []
         ->
            fd_max_integer(Tau1)
         ;
            select_tau(L,NCL,TempU,Alpha,Tau1)
         ),
         (
            Tau1 < 10/Alpha
         ->
            g_assign(ssax,Xm),
            g_assign(ssafx,U),
            g_assign(ssacounter,0),
            repeat,
            g_read(ssax,GM),
            g_read(ssafx,GU),
            g_read(ssacounter,Con),
            NCon is Con+1,
            g_assign(ssacounter,NCon),
            gillespie(GU,GH,L,GNU),
            GX is GM + GH,
            g_assign(ssax,GX),
            g_assign(ssafx,GNU),
            NCon >= 100,
            !,
            H is GX - Xm,
            V = GNU
         ;
            sum_over_critical(CL,0.0,AlphaC),
            repeat,
            random(R1),
            R1 =\= 0,
            !,
            (
               AlphaC =:= 0
            ->
               fd_max_integer(Tau2)
            ;
               Tau2 is (1/AlphaC)*log(1/R1)
            ),
            g_assign(tau1,Tau1),
            repeat,
            g_read(tau1,RTau1),
            (
               Tau2 >= RTau1
            ->
               %format("No critical reaction~n",[]),
               Tau is RTau1,
               NU = U
            ;
               Tau is Tau2,
               random(R2),
               Bound is R2*AlphaC,
               next_reaction(CL,Bound,0,EV),
               undate_macros(U,EV,NU)
            ),
            each_reaction(NCL,Tau,NU,VP),
            negative_test(VP,Flag),
            NTau1 is RTau1 / 2,
            g_assign(tau1,NTau1),
            Flag = 0,
            !,
            H = Tau,
            V = VP
         ),!
      )
   ).

negative_test([],0):-!.
negative_test([(_,Val)|UL],Flag):-
   (
      Val < 0
   ->
      Flag = 1
   ;
      negative_test(UL,Flag)
   ).


critical_reaction_list([],_,[],[],[]):-!.
critical_reaction_list([(R,_,_,EVector)|LL],U,[Aj|ALL],CL,NCL):-
   critical_reaction(EVector,U,Flag),
   (
      Flag = 1,
      Aj > 0
   ->
      CL = [(R,Aj,EVector)|CLL],
      NCL = NCLL
   ;
      CL = CLL,
      NCL = [(R,Aj,EVector)|NCLL]
   ),
   critical_reaction_list(LL,U,ALL,CLL,NCLL).

critical_reaction([],[],0):-!.
critical_reaction([(Var,Val1)|EVector],[(Var,Val2)|UL],Flag):-
   g_read(critical_threshold,N),
   (
      Val1 < 0,
      Val2 < N
   ->
      Flag = 1
   ;
      critical_reaction(EVector,UL,Flag)
   ).

each_reaction([],_,V1,V2):- V2 = V1,!.
each_reaction([(_,Aj,EVector)|LL],Tau,V1,V2):-
   AT is Aj * Tau,
   podev(AT,N),
   (
      N =:= 0
   ->
      Vtmp = V1
   ;
      undate_macros(V1,EVector,Vtmp,N)
   ),
   each_reaction(LL,Tau,Vtmp,V2).

select_tau(L,NCL,U,Alpha,Tau):-
   protau_list(L,NCL,U,Alpha,TauL),
   min_list(TauL,Tau).

inner_product([],[],Cur,Cur):- !.
inner_product([V1|DAL],[(_,V2)|Vjp],Cur,Val):-
   g_read(conversion_factor,Rate),
   NCur is Cur + V1 * V2 / Rate,
   inner_product(DAL,Vjp,NCur,Val).

muJ_sigmaJ(_,[],_,0.0,0.0).
muJ_sigmaJ(DAL,[(_,Ajp,Vjp)|LpL],U,MuJ,SigmaJ):-
   inner_product(DAL,Vjp,0,FJjp),
   muJ_sigmaJ(DAL,LpL,U,NMuJ,NSigmaJ),
   MuJ is NMuJ + FJjp*Ajp,
   SigmaJ is NSigmaJ + FJjp*FJjp*Ajp.
    
protau_list([],_,_,_,[]):-!.
protau_list([(_,_,DRLDj,_)|LL],Lp,U,Alpha,[Tau|TauL]):-
   %write(protau_list([(_,_,DRLDj,_)|LL],Lp,U,Alpha,[Tau|TauL])),
   eval_line(DRLDj,U,DAL),
   muJ_sigmaJ(DAL,Lp,U,MuJ,SigmaJ),
   (
      MuJ =:= 0
   ->
      fd_max_integer(Left) 
   ;
      Left is 0.01*Alpha/abs(MuJ)
   ),
   (
      SigmaJ =:= 0
   ->
      fd_max_integer(Right)
   ;
      Right is 0.0001*Alpha*Alpha/SigmaJ
   ),
   (
      Left < Right
   ->
      Tau = Left
   ;
      Tau = Right
   ),
   protau_list(LL,Lp,U,Alpha,TauL).

%%% Gamma function
%%% cf. Numerical Recipes in C chap 6
gammaln(X,Y):-
   Tmp1 is X + 5.5,
   Tmp2 is Tmp1 - (X + 0.5) * log(Tmp1),
   Ser1 = 1.0000000001990015,
   Ser2 is Ser1 + 76.18009172947146/(X+1),
   Ser3 is Ser2 - 86.50532032941677/(X+2),
   Ser4 is Ser3 + 24.01409824083097/(X+3),
   Ser5 is Ser4 - 1.231739572450155/(X+4),
   Ser6 is Ser5 + 0.1208650973866179e-2/(X+5),
   Ser7 is Ser6 - 0.5395239384953e-5/(X+6),
   Y is log(2.5066282746310005 * Ser7 / X) -Tmp2.
%%% Poisson Deviates
%%% cf. Numerical Recipes in C chap 7
podev(AT,N):-
   (
      AT < 12.0
   ->
      G is exp(-AT),
      g_assign(podev_em,-1),
      g_assign(podev_t,1.0),
      repeat,
      g_read(podev_em, EM),
      g_read(podev_t, T),
      random(R),
      NEM is EM + 1,
      NT is T * R,
      g_assign(podev_t,NT),
      g_assign(podev_em,NEM),
      NT =< G,
      !,
      N = NEM
   ;
      SQ is sqrt(2*AT),
      Alxm is log(AT),
      NAT is AT + 1,
      gammaln(NAT,GNAT),
      G is AT * Alxm - GNAT,
      repeat,
      podev_em(AT,SQ,Y,EM),
      FEM is floor(EM),
      NEM is FEM + 1,
      gammaln(NEM,GNEM),
      T is 0.9*(1 + Y*Y)*exp(FEM*Alxm - GNEM - G),
      random(R2),
      R2 < T,
      !,
      N = FEM
   ).
podev_em(AT,SQ,Y,EM):-
   repeat,
   random(R),
   ARC is 3.141592654*R,
   Y is sin(ARC)/cos(ARC),
   EM is SQ*Y +AT,
   EM >= 0.0,
   !.
