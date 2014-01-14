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
% GNU prolog file biochamAnalyze.pl
% by Aurelien Rizk

:- dynamic(data/1).
:- dynamic(hasht/2).
:- dynamic(result/4).
:- dynamic(vertex/1).
:- dynamic(vals_rect/2).
:- dynamic(trace_loaded/0).
:- dynamic(ppl_initialized/0).
:- dynamic(cmaes_init/0).
:- dynamic(cmaes_std/0).
:- dynamic(fss_init/0).
:- dynamic(fss_maxevals/0).
:- dynamic(fss_fish/0).
:- dynamic(fss_std/0).
:- dynamic(fss_debugcode/0).

%morris
:- foreign(call_morris(+integer,+integer,+integer, +float)).
:- foreign(store_params_morris(+integer, +integer, +integer, +float)).



%Anthony,fss parameters: 
%% dimensions
%% fun
%% maxevals_b
%% pl_stopfitness_b 
%% cseed
%% std 
%% number_fish
:- foreign(call_main_fss(+integer, +integer,+integer, +integer, +term, +integer, +float, +integer, +integer)).
:- foreign(getFssCost(-float)).
%cmaes
:- foreign(call_main_cmaes(+integer,+integer,+integer,+term,+integer,+float)).
:- foreign(getFCost(-float)).
%ppl calls
:- foreign(empty_tab_constraint(+integer)).
:- foreign(empty_tab_vars(+integer)).
:- foreign(empty_tab_dom(+integer)).
:- foreign(init_ppl(+integer)).
:- foreign(end_ppl(+integer)).
:- foreign(free_tab_dom(+integer)).
:- foreign(dist_var_domain(+integer,+integer,+float,-float)).
:- foreign(d_disp_ppl(+integer,-string)).
:- foreign(var_coeff_assign(+integer,+integer)).
:- foreign(var_coeff_store(+integer,+integer)).
:- foreign(var_get_key(+string,-integer)).


:- foreign(var_norm_init(+integer)).
:- foreign(var_norm_store(+integer,+float)).
:- foreign(freedomkey(+integer)).
:- foreign(d_union_dom(+integer,+integer,-integer)).
:- foreign(d_inter_dom(+integer,+integer,-integer)).
:- foreign(d_build_dom(+integer,+float,+integer,-integer)).
:- foreign(d_build_dom_empty(-integer)).
:- foreign(d_build_dom_universe(-integer)).
:- foreign(send_objective(+integer,+float)).
:- foreign(disp_obj(+integer)).
:- foreign(clean_obj(+integer)).

measure_memory(Term) :-
   g_inc(memory_sum),
   (
      compound(Term)
   ->
      Term =.. [_F | Args],
      \+ (
         member(Arg, Args),
         \+ (
            measure_memory(Arg)
         )
      )
   ;
      true
   ).

measure_memory :-
   g_assign(memory_sum, 0),
   \+ (
      current_predicate(Name / Arity),
      functor(Head, Name, Arity),
      predicate_property(Head, dynamic),
      print(Name / Arity),
      nl,
      clause(Head, Body),
      \+ (
         measure_memory(Head),
         measure_memory(Body)
      )
   ),
   \+ (
      member(X, ['absent', 'allsolutions', 'base', 'bg_reactions_count', 'bg_rs_arcs', 'bg_species_count', 'bg_sr_arcs', 'biocham_lib', 'biocham_path', 'bound_penalty', 'bounds', 'cmaes', 'col', 'compteur_sortie_err', 'conversion_factor', 'critical_threshold', 'current_bc_file', 'current_bc_stream', 'current_dim_expr', 'current_dim_term', 'current_dir', 'datatab', 'debug', 'debugcode', 'depth', 'dists', 'done', 'dot_mod_dash', 'drawing_period', 'end', 'error', 'file', 'filtering', 'first_plot', 'fish', 'formal_imax', 'formal_index', 'formal_m', 'formal_matrix', 'formal_n', 'formal_scaling', 'formal_sum', 'formal_tmp', 'formal_vector', 'formula', 'fss', 'fssbounds', 'full_match', 'fval', 'fx', 'fxn', 'g_biosyntax_initialized', 'g_old_parser', 'grn_fix', 'g_syntax_only', 'h', 'hdid', 'hmax', 'interval0', 'k', 'k_method', 'kplot_nb', 'kplot_range', 'k_rbk_a21', 'k_rbk_a31', 'k_rbk_a32', 'k_rbk_b1', 'k_rbk_b2', 'k_rbk_b3', 'k_rbk_b4', 'k_rbk_c21', 'k_rbk_c31', 'k_rbk_c32', 'k_rbk_c41', 'k_rbk_c42', 'k_rbk_c43', 'k_rbk_e1', 'k_rbk_e2', 'k_rbk_e3', 'k_rbk_e4', 'k_rbk_errcon', 'k_rbk_gam', 'k_rbk_grow', 'k_rbk_pgrow', 'k_rbk_pshrink', 'k_rbk_safe', 'k_rbk_shrink', 'k_time', 'k_time_units', 'land', 'landsp1', 'lgvars', 'line', 'listMolSeen', 'lmols', 'macrosl', 'macroslwithdef', 'mavglist', 'maxevals', 'maxsimul', 'max_spec_treated', 'mean_order', 'means', 'nbcurr', 'nb_deep', 'nbGrule', 'nbLrule', 'nb_Q_a_retraite', 'nb_reload', 'nb_spec_treated', 'nbto', 'nbtotal', 'nbtrace', 'next_class', 'nf', 'nh', 'nk', 'nn', 'no_declare_check', 'no_dim_output', 'non_sym', 'not_zero_error', 'ns', 'ntime', 'numero_courant', 'numero_mol', 'numero_molecule', 'numero_regle', 'num_R', 'nusmv_cur_rule', 'nusmv_ms', 'nusmv_s', 'nusmv_state', 'obj0', 'objectives', 'ode_tex_mode', 'paramlist', 'params0', 'plot_term', 'podev_em', 'podev_t', 'prolog', 'prompt', 'p_t_list', 'q_a_retraite', 'reduction_total', 'replace', 'rob', 'sbml_current_reac', 'sbml_indent', 'sbml_number', 'search_count', 'search_depth', 'search_mode', 'seed', 'sepi_dist', 'size', 'spec0', 'specs', 'ssacounter', 'ssafx', 'ssax', 'state_list', 'std', 'step_doubling_error', 'steplist', 'step_size', 'stopfitness', 'success', 'sumdist', 'sums', 'tau1', 'tdom', 'time_event_time', 'timesim0', 'timesimul', 'trace_found', 'trace_times', 'varlist0', 'varlists', 'varmol', 'v_obj', 'warning_cnt', 'x', 'xml_indent']),
      print(X),
      nl,
      g_read(X, V),
      \+ (
         measure_memory(V)
      )
   ).

output_memory :-
   measure_memory,
   g_read(memory_sum, Sum),
   format('Memory: ~d~n', [Sum]).

%%%%%%trace import

read_float(S,N):-
	read_number(S,N1),
	peek_char(S,C),
	((C='e';C='E')
	->
	 (get_char(S,_),
	 read_number(S,N2))
	;N2 is 0
	),
	Z is 10**(-N2),
	N is N1/Z.


import_line(S,L):-
	peek_char(S,C),
	(C='\t'),!,
	get_char(S,_),
	import_line(S,L).

import_line(S,L):-
	peek_char(S,C),
	(C=' '),!,
	get_char(S,_),
	import_line(S,L).

import_line(S,[]):-
	peek_char(S,C),
	(C='\n'),!,
	get_char(S,_).
import_line(S,[]):-
	at_end_of_stream(S),!.
	
import_line(S,[N|L]):-
	read_float(S,N),
	import_line(S,L).



import_fline(S,L):-
	peek_char(S,C),
	(C='\t'),!,
	get_char(S,_),
	import_fline(S,L).

import_fline(S,L):-
	peek_char(S,C),
	(C=' '),!,
	get_char(S,_),
	import_fline(S,L).

import_fline(S,L):-
	peek_char(S,C),
	write_to_codes(Z,C),
	(Z=[35]),!,
	get_char(S,_),
	import_fline(S,L).

import_fline(S,[]):-
	peek_char(S,C),
	(C='\n'),!,
	get_char(S,_).
	
import_fline(S,[N|L]):-
	read_mol(S,N),
	import_fline(S,L).


read_mol(S,[]):-
	peek_char(S,C),
	((C='');C='\t'),!,
	get_char(S,_).



read_mol(S,[D]):-
	peek_char(S,C),
	(C=']'),!,
	get_code(S,D).


read_mol(S,[H|T]):-
	!,get_code(S,H),
	read_mol(S,T).



load_trace(FF):-
	statistics(cpu_time,[T1,_]),
	
	g_assign(datatab,0),
	retractall(data(_)),
	retractall(hasht(_,_)),
	write_to_atom(F,FF),
	(
	 sub_atom(F,_,1,_,'.')
	->
	 (G=F)
	;
	 atom_concat(F,'.csv',G)
	),
	open(G,read,S),
	import_fline(S,Ld),
	%format("Ld : ~w  ~n",[Ld]),%%
	repeat,
	(
        at_end_of_stream(S)
	->
	 true
	;
	 import_line(S,L),
	 (\+(length(L,0))
	 ->
	  assertz(data(L))
	 ),
	 fail
	),
	!,
	close(S),
	findall(X,data(X),R),
	length(R,Nu),
	g_assign(size,Nu),
	format_debug(10,"data : ~w ~n ~w ~n  ~w ~n", [Ld,R,Nu]),
	list_mols(Ld), %assign lmols
	length(Ld,Nbl),
	NN is Nu+1,
	Nb is 3*Nbl+1,
	format_debug(4,"I:~w J:~w ~n",[Nb,NN]),
	g_assign(datatab,g_array(Nb,g_array(NN))),
	Ld=[Ti|Ldd],
	parse_mols(Ldd,Ldd2),
	%format("Ldd :~w ~n Ldd2 ~w ~n",[Ldd, Ldd2]),
	LL2=[Ti|Ldd2],
	check_time(Ti),
	format_data(LL2,R,LL2,1),
	compute_all_derivatives,
	compute_all_derivatives2,
	g_assign(tdom,g_array(NN)),
	retractall(data(_)),
	assertz(trace_loaded),
	statistics(cpu_time,[T2,_]),
	T is (T2-T1),
	format("time elapsed : ~w ms ~n",[T]),
	(
     	 have_gui
   	->
    	  format("[GUI] loadTrace FINISHED ~n")
  	 ;
		true
  	).


load_mem:-
	g_assign(datatab,0),
	retractall(hasht(_,_)),
	build_hash,
	build_lmols,
	
	build_hasht_lmols_macros,
	g_read(lmols,Lmols),
	length(Lmols,Nbmols),
	
	g_read(trace_times,TL),
	length(TL,Trace_length),
	g_assign(ntime,Trace_length),
	g_assign(size,Trace_length),
	
	TT is Trace_length+1,
	Nb is 3*(Nbmols+1)+1,

	g_assign(datatab,g_array(Nb,g_array(TT))),
	
	(
	 k_value(T,LV,_),
	 %format("T: ~w  ~nLV: ~w ~n",[T,LV]),
	 g_read(ntime,N),
	 add_data_time([('Time',T)|LV],N),
	 g_dec(ntime),
	 fail
	;
	 true
	 ),
	load_datatab_macros,	%do only if formula contains macros ?
	compute_all_derivatives,
	compute_all_derivatives2,
	g_assign(tdom,g_array(TT)),
	assertz(trace_loaded).





load_datatab_macros:-
	g_read(macroslwithdef,LM),
	%format("macros :~w ~n",[LM]),
	datatab_macros(LM).

datatab_macros([]).

datatab_macros([H|T]):-
	g_read(size,N),
	g_assign(end,0), %pour probleme avec le cas fail das eval_c et le repeat dans trace_domain_sup
	add_macro_tab(H,N),
	datatab_macros(T).


add_macro_tab((H,Def),N):-
	format_debug(2,"H : ~w N ~w ~n",[H,N]),
	g_assign(nn,N),
	repeat,
	g_read(end,E),
	g_read(nn,NN),
	(((NN=0);(E==1))
	->
	 true
	;
	 format_debug(2,"DH : ~w ~n",[Def]),
	 eval_c(Def,V,NN),
	 hasht(H,NH),
	 g_assign(datatab(NH,NN),V),
	 g_dec(nn),
	 fail
	).


build_hasht_lmols_macros:-
	getmacros,
	addmacros_hasht,
	add_macros_lmols.


addmacros_hasht:-
	findall(X,hasht(X,_),LL),
	length(LL,Nd),
	g_read(macrosl,LM),
	N is Nd+1,
	add_macros_aux(LM,N).

add_macros_aux([],_).
add_macros_aux([H|T],N):-
	assertz(hasht(H,N)),
	NN is N+1,
	add_macros_aux(T,NN).


add_macros_lmols:-
	g_read(macrosl,LM),
	g_read(lmols,L),
	append(L,LM,LLM),
	g_assign(lmols,LLM).

getmacros:-
	g_assign(macrosl,[]),
	g_assign(macroslwithdef,[]),
	getmacros_aux.

	
getmacros_aux:-
	k_macro(M,Def),
	%format("MAcro : ~w ~n",[M]),
	g_read(macrosl,LM),
	g_read(macroslwithdef,LM2),
	
	write_to_codes(MM,M),
	g_assign(macrosl,[MM|LM]),
	g_assign(macroslwithdef,[(MM,Def)|LM2]),
	fail.

getmacros_aux.


build_lmols:-
	k_value(0,LV,_),
	build_lmols_aux(LV,Lmols),
	g_assign(lmols,Lmols).

build_lmols_aux([],[]).
build_lmols_aux([(M,_)|T],[MM|TT]):-
	write_to_codes(MM,M),
	build_lmols_aux(T,TT).
	


testl:-
	  g_read(trace_times,TL),
	  length(TL,NN),
	  findall(T,k_value(T,_,_),R),
	  length(R,NN2),
	  format("NN: ~w ~n NN2 : ~w ~n",[NN,NN2]).


add_data_time(LV,N):-
	add_data_aux(LV,N,1).


add_data_aux([],_,_).
add_data_aux([(_,V)|T],Ntime,Nmol):-
	g_assign(datatab(Nmol,Ntime),V),
	NN is Nmol+1,
	add_data_aux(T,Ntime,NN).


build_hash:-
	k_value(0,LV,_),
	build_hash_aux([('Time',0)|LV],1).


build_hash_aux([],_).

build_hash_aux([(H,_)|T],N):-
	write_to_codes(C,H),
	assertz(hasht(C,N)),
	NN is N+1,
	build_hash_aux(T,NN).






	
check_time([84,105,109,101]):-!.
check_time(Ti):-
   atom_codes(T,Ti),
   (
      have_gui
   ->
      format("[GUI] errors First column is ~w  but should be Time ~n",[T])
   ;
      format("Error : First column is ~w  but should be Time ~n",[T])
   ).

%ajouter erreur si pas de molecules (liste vide ou ï¿½ un elt)
list_mols([],[]).
list_mols([string(A)|T],[A|Tm]):-
	list_mols(T,Tm).
	

parse_mols([],[]).
parse_mols([M|Lmol],[LL|LLmol]):-
	memberchk(91,M),!,
	atom_codes(M2,M),
	sub_atom(M2,1,_,1,M3),
	parse_object(M3,M4),
	atom_concat('[',M4,M4b),
	atom_concat(M4b,']',M5),
	write_to_codes(LL,M5),
	parse_mols(Lmol,LLmol).

parse_mols([M|Lmol],[LL|LLmol]):-
	atom_codes(M2,M),
	parse_object(M2,M3),
	write_to_codes(LL,M3),
	parse_mols(Lmol,LLmol).



list_mols([_|T]):-
	g_assign(lmols,T).	
format_data([],_,_,_).
format_data([M|Lmol],Lval,Lmoli,K):-
	assertz(hasht(M,K)),
	format_col(M,Lmoli,Lval,R),
	reverse(R,R2),
	fill_data(R2,K,1),
	KK is K+1,
	format_data(Lmol,Lval,Lmoli,KK).

fill_data([],_,_).
fill_data([H|L],Nm,Ni):-
	format_debug(7,"Assign : I:~w J ~w ~n:",[Nm,Ni]),
	g_assign(datatab(Nm,Ni),H),
	NN is Ni +1,
	fill_data(L,Nm,NN).
	
disp_all:-
	nl,
	findall(X,hasht(X,_),LL),
	g_read(size,S),
	disp_all_mol(LL),
	nl,
	disp_allt(LL,S).

disp_all_mol([]).
disp_all_mol([H|T]):-
   atom_codes(HH,H),
   format("~w ",[HH]),
   (
      have_gui
   ->
      format("[GUI] checkLTL  ~w ",[HH])
   ;
      true
   ),
   disp_all_mol(T).

disp_allt(_,1).
disp_allt(LL,S):-
	disp_all(LL,S),
	S2 is S-1,
	disp_allt(LL,S2).

disp_all([],_):-nl.
disp_all([H|T],S):-
	display_trace(H,S),
	disp_all(T,S).

display_trace(M,S):-
   hasht(M,N),
   g_read(datatab(N,S),D),!,
   format("~3f",[D]),
   format("    ",[]),
   (
      have_gui
   ->
      format("[GUI] checkLTL trace display: ~3f",[D])
   ;
      true
   ),
   format("    ",[]),
   (
      have_gui
   ->
      format("[GUI] checkLTL       ",[])
   ;
      true
   ).

format_col(M,Lmol,Lval,R):-
	nth(N,Lmol,M),
	format_col(N,Lval,R).
format_col(_,[],[]).
format_col(N,[H|L],[Hr|R]):-
	nth(N,H,Hr),
	format_col(N,L,R).
	

%fill each time point with empty list(no formulae, no domain)
ensure_domains:-
	g_read(tdom(0),X),
	(X=[]
	 ;
	 g_read(size,S),
	 fill_domains(S)
	).

ensure_data:-
   (
    trace_loaded
   ;
    ensure_trace,!,
   %% data_file(File),
   %write(loadtrace),nl,
    %%load_trace(File)
   % write(load_data),nl,
    load_mem
   ).

ensure_ppl:-
   (
    ppl_initialized
   ;
    init_ppl(0),
    assertz(ppl_initialized)
        ).

clean_ppl:-
	free_tab_dom(1),
	empty_tab_vars(1),
	empty_tab_dom(1).
	

%%%%%%%%%trace_analyze
domains(E):-trace_analyze(E).
solve(E):-trace_analyze(E).

trace_analyze(E):-
   ensure_ppl,
   ensure_data,!,
   statistics(cpu_time,[T1,_]),
   var_norm_init(1),
   format_debug(3,"trace_analyze :E  ~w ~n",[E]),
   g_assign(lgvars,[]),
   f_normalize(E,EE),
   format_debug(3,"trace_analyze :EE  ~w ~n",[EE]),
   ensure_domains,!,
   trace_domain(EE),!,
   g_read(tdom(1),L),
   member((EE,D),L),
   format_debug(4,"liste en 0: ~w ~n",[L]),
   disp_dom(E,D),
   statistics(cpu_time,[T2,_]),
   T is (T2-T1),
   format("time elapsed : ~w ms ~n",[T]),
   clean_ppl,
   empty_domains,!.


empty_domains:-
	g_read(size,S),
	fill_domains(S),!.

%%%%display validity domain
disp_dom(_,D):-
	d_disp_ppl(D,Output),
	(
      	have_gui
   		->
			format("[GUI] traceAnalyze start~n",[]),
      			format("[GUI] traceAnalyze ~w~n",[Output])
   		;
      			format("~w",[Output])
   	).



% Check if not already done
trace_domain(E):-
	g_assign(end,0), %pour probleme avec le cas fail das eval_c et le repeat dans trace_domain_sup
	g_read(tdom(1),L),
	memberchk((E,_),L),!.

%%%%%%%%%%%%%%% (E & F)
trace_domain('&'(E,F)):-
	!,
	format_debug(4,"and E:~w ~n & F:~w ~n ~n",[E,F]),
	trace_domain(E),!,
	trace_domain(F),!,
	g_read(size,S),
	trace_domain_and(S,E,F).

%%%%%%%%%%%%%%% (E | F)
trace_domain('|'(E,F)):-
	!,
	format_debug(4,"or E:~w ~n | F:~w ~n ~n",[E,F]),
	trace_domain(E),!,
	trace_domain(F),!,
	g_read(size,S),
	trace_domain_or(S,E,F).

%%%%%%%%%%%%%%% ('F' E)
trace_domain('F'(E)):-
	!,
	format_debug(4,"finally :F ~w ~n ~n",[E]),
	trace_domain(E),!,
	g_read(size,La),
	g_read(tdom(La),L),
	member((E,De),L),
	g_assign(tdom(La),[('F'(E),De)|L]),
	format_debug(5,"trace_domain :F ~w ~w  ~n",[La,De]),
	trace_domain_finally(La,E).


%%%%%%%%%%%%%%% ('G' E)
trace_domain('G'(E)):-
	!,
	format_debug(4,"globally :G ~w ~n ~n",[E]),
	trace_domain(E),!,
	g_read(size,La),
	g_read(tdom(La),L),
	member((E,De),L),
	g_assign(tdom(La),[('G'(E),De)|L]),
	format_debug(5,"trace_domain :G ~w ~w  ~n",[La,De]),
	trace_domain_globally(La,E).

%%%%%%%%%%%%%%% ( E 'U' F)
trace_domain('U'(E,F)):-
	!,
	format_debug(4,"until E:~w ~n 'U' & F:~w ~n ~n",[E,F]),
	trace_domain(E),!,
	trace_domain(F),!,
	g_read(size,La),
	g_read(tdom(La),L),
	member((F,De),L),
	g_assign(tdom(La),[('U'(E,F),De)|L]),
	trace_domain_until(La,E,F).


%%%%%%%%%%%%%%% ('X' E)
trace_domain('X'(E)):-
	!,
	format_debug(4,"next : X ~w ~n ~n",[E]),
	trace_domain(E),!,
	g_read(size,La),
	g_read(tdom(La),L),
	member((E,De),L),
	format_debug(5,"t: ~w ~n f: ~w ~n ",[La,('X'(E))]),
	g_assign(tdom(La),[('X'(E),De)|L]),
	trace_domain_next(La,E).

%%%%%%%%%%%%%%SUP%%%%%%%%%%%%%%%%%%%
trace_domain(Expr1 >= Expr2):-
	contains_var(Expr1 >= Expr2,N),
	g_read(size,S),
 	trace_domain_sup(S,Expr1,Expr2,N).

trace_domain(Expr1 =< Expr2):-
	contains_var(Expr1 =< Expr2,N),
	g_read(size,S),
 	trace_domain_inf(S,Expr1,Expr2,N).


trace_domain(Expr1 > Expr2):-
	contains_var(Expr1 >= Expr2,N),
	g_read(size,S),
 	trace_domain_supo(S,Expr1,Expr2,N).

trace_domain(Expr1 < Expr2):-
	contains_var(Expr1 =< Expr2,N),
	g_read(size,S),
 	trace_domain_info(S,Expr1,Expr2,N).


trace_domain(curve_fit(Mol,Ltimes,Lvars)):-
	!,
	trace_domain_curve_fit(Mol,Ltimes,Lvars,Dom),
	g_read(tdom(1),L),
	fill_curve(Mol,Ltimes,Lvars,Dom),
	g_assign(tdom(1),[(curve_fit(Mol,Ltimes,Lvars),Dom)|L]).



trace_domain(curve_fit_error(Mol,Lvals,Ltimes,Lvars)):-
	!,
%	store_varfit(Lvars),
	trace_domain_curve_fit_err(Mol,Lvals,Ltimes,Lvars,Dom),
	g_read(tdom(1),L),
	fill_curve_err(Mol,Lvals,Ltimes,Lvars,Dom),
	g_assign(tdom(1),[(curve_fit_error(Mol,Lvals,Ltimes,Lvars),Dom)|L]).

fill_curve_err(Mol,Lvals,Ltimes,Lvars,Dom):-
	g_read(size,S),
	fill_curve_err_aux(Mol,Lvals,Ltimes,Lvars,Dom,S).


fill_curve_err_aux(Mol,Lvals,Ltimes,Lvars,Dom,NS):-
	g_assign(nf,NS),
	repeat,
	g_read(nf,S),
	((S==1)
	->
	 true
	;
	 g_read(tdom(S),L),
	 g_assign(tdom(S),[(curve_fit_error(Mol,Lvals,Ltimes,Lvars),Dom)|L]),
	 g_dec(nf),
	 fail
	).


trace_domain_curve_fit_err(_,[],[],[],D_universe):-
	d_build_dom_universe(D_universe).
	

trace_domain_curve_fit_err(M,[Val|Lvals],[T|Ltimes],[V|Lvars],Dres):-
	eval_mol(M,T,X),
	trace_domain_curve_fit_err(M,Lvals,Ltimes,Lvars,Dom),
	empty_tab_constraint(1),
	build_constraint(V,+),
	XX is X*1.0,
	ZZ is -abs(Val-XX),
	d_build_dom(1,ZZ,3,DD),
	d_inter_dom(Dom,DD,Dres).





trace_domain_curve_fit(_,[],[],D_universe):-
	d_build_dom_universe(D_universe).
	

trace_domain_curve_fit(M,[T|Ltimes],[V|Lvars],Dres):-
	eval_mol(M,T,X),
	trace_domain_curve_fit(M,Ltimes,Lvars,Dom),
	empty_tab_constraint(1),
	build_constraint(V,+),
	XX is -X*1.0,
	d_build_dom(1,XX,5,DD),
	d_inter_dom(Dom,DD,Dres).


fill_curve(Mol,Ltimes,Lvars,Dom):-
	g_read(size,S),
	fill_curve_aux(Mol,Ltimes,Lvars,Dom,S).


fill_curve_aux(Mol,Ltimes,Lvars,Dom,NS):-
	g_assign(nf,NS),
	repeat,
	g_read(nf,S),
	((S==1)
	->
	 true
	;

	 g_read(tdom(S),L),
	 g_assign(tdom(S),[(curve_fit(Mol,Ltimes,Lvars),Dom)|L]),
	 g_dec(nf),
	 fail
	).



store_varfit([]).
store_varfit([V|T]):-
	store_coeffs(V,+),
	store_varfit(T).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%
trace_domain_and(HH,E,F):-
	g_assign(nf,HH),
	repeat,
	g_read(end,End),
	g_read(nf,H),
	(((H==0);(End==1))
	->
	 true
	;
	g_read(tdom(H),L),
	member((E,De),L),
	member((F,Df),L),
	d_inter_dom(De,Df,Dr),
	format_debug(5,"and : E,De : ~w ~w ~n F,Df : ~w ~w ~n",[E,De,F,Df]),
	format_debug(5,"and, t :~w Dr:~w ~n",[H,Dr]),
	g_assign(tdom(H),[('&'(E,F),Dr)|L]),
	 g_dec(nf),
	 fail
	).



	 
%%%%%%

trace_domain_or(HH,E,F):-
	g_assign(nf,HH),
	repeat,
	g_read(nf,H),
	g_read(end,End),
	(((H==0);(End==1))
	->
	 true
	;
	g_read(tdom(H),L),
	member((E,De),L),
	member((F,Df),L),
	d_union_dom(De,Df,Dr),
	format_debug(5,"or : E,De : ~w ~w ~n F,Df : ~w ~w ~n",[E,De,F,Df]),
	format_debug(5,"or, t :~w Dr:~w ~n",[H,Dr]),
	g_assign(tdom(H),[('|'(E,F),Dr)|L]),
	 g_dec(nf),
	 fail
	).


%%%%%%
trace_domain_next(HH,E):-
	g_assign(nf,HH),
	repeat,
	g_read(end,End),
	g_read(nf,H),
	(((H==1);(End==1))
	->
	 true
	;
	
	N is H-1,
	g_read(tdom(H),L),
	member((E,De),L),
	format_debug(5,"next : E,De : ~w  ~n",[E]),
	format_debug(5,"next, t :~w Dr:~w ~n",[N,De]),
	g_read(tdom(N),Lc),
	g_assign(tdom(N),[('X'(E),De)|Lc]),
	 g_dec(nf),
	 fail
	).


%%%%%%
trace_domain_finally(H,E):-
	g_assign(nf,H),
	repeat,
	g_read(end,End),
	g_read(nf,NF),
	(((NF==1);(End==1))
	->
	 true
	;
	N is NF-1,
	g_read(tdom(NF),L),
	member(('F'(E),Df),L),
	g_read(tdom(N),Ln),
	member((E,De),Ln),
	d_union_dom(De,Df,Dr),
	%length(Dr,T),
	%format_debug(4,"taille domaine en ~w : ~w ~n",[N,T]),
	format_debug(5,"finally : E,De : ~w ~w ~n F(E),Df : ~w ~n",[E,De,Df]),
	format_debug(5,"finally, t :~w Dr:~w ~n",[N,Dr]),
	g_assign(tdom(N),[('F'(E),Dr)|Ln]),
	 g_dec(nf),
	 fail
	).




%%%%%%
trace_domain_globally(HH,E):-
	g_assign(nf,HH),
	repeat,
	g_read(nf,H),
	g_read(end,End),
	(((H==1);(End==1))
	->
	 true
	;
	N is H-1,
	g_read(tdom(H),L),
	member(('G'(E),Df),L),
	g_read(tdom(N),Ln),
	member((E,De),Ln),
	d_inter_dom(De,Df,Dr),
%	length(Dr,T),
%	format_debug(4,"taille domaine en ~w : ~w ~n",[N,T]),
	format_debug(5,"globally : E,De : ~w ~w ~n G(E),Df :  ~w ~n",[E,De,Df]),
	format_debug(5,"globally, t :~w Dr:~w ~n",[N,Dr]),
	g_assign(tdom(N),[('G'(E),Dr)|Ln]),
	 g_dec(nf),
	 fail
	).

%%%%%%
trace_domain_until(HH,E,F):-
	g_assign(nf,HH),
	repeat,
	g_read(end,End),
	g_read(nf,H),
	(((H==1);(End==1))
	->
	 true
	;
	N is H-1,
	g_read(tdom(H),L),
	format_debug(5,"until : L,E,F : ~w ~w ~w ~n",[L, E, F]),
	member(('U'(E,F),Dfo),L),
	format_debug(5,"until : Dfo : ~w ~n",[Dfo]),
	g_read(tdom(N),Ln),
	member((E,De),Ln),
	member((F,Df),Ln),
	format_debug(5,"until : E,De : ~w ~w ~n G(E),Df : ~w ~w ~n",[E,De,F,Df]),
	d_inter_dom(De,Dfo,Dr),
	d_union_dom(Dr,Df,Dr2),
	format_debug(5,"until, t :~w Dr:~w ~n",[N,Dr2]),
	g_assign(tdom(N),[('U'(E,F),Dr2)|Ln]),
	 g_dec(nf),
	 fail
	).

contains_var(P1 =< P2,NN):-!,
 	contains_var(P1,N1),
 	contains_var(P2,N2),
	NN is N1+N2.

contains_var(P1 >= P2,NN):-!,
 	contains_var(P1,N1),
 	contains_var(P2,N2),
	NN is N1+N2.


contains_var(P1 + P2,NN):-!,
 	contains_var(P1,N1),
 	contains_var(P2,N2),
	NN is N1+N2.

contains_var(P1 - P2,NN):-!,
 	contains_var(P1,N1),
 	contains_var(P2,N2),
	NN is N1+N2.

contains_var(P,1):-
 	 is_cvar(P,_,_),!.
contains_var(_,0).



eval_c(X+Y,Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is XX+YY.
eval_c(X-Y,Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is XX-YY.
eval_c(-Y,Z,N):-!,eval_c(0-Y,Z,N).
eval_c(X*Y,Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is XX*YY.
eval_c(d([X])/dt,Z,N):-!,eval_c(d([X]),Z,N). %rewrite derivatives to avoid conflict with division
eval_c(X/Y,Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is XX/YY.
eval_c(X^Y,Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is XX**YY.
eval_c(log(X),Z,N):-!,eval_c(X,XX,N),Z is log(XX).
eval_c(exp(X),Z,N):-!,eval_c(X,XX,N),Z is exp(XX).
eval_c(sin(X),Z,N):-!,eval_c(X,XX,N),Z is sin(XX).
eval_c(cos(X),Z,N):-!,eval_c(X,XX,N),Z is cos(XX).
eval_c(abs(X),Z,N):-!,eval_c(X,XX,N),Z is abs(XX).
eval_c(min(X,Y),Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is min(XX,YY).
eval_c(max(X,Y),Z,N):-!,eval_c(X,XX,N),eval_c(Y,YY,N), Z is max(XX,YY).
eval_c(frac(X), Z, N) :-
   !,
   eval_c(X, XX, N),
   Z is float_fractional_part(XX).
eval_c(X,X,_):-number(X),!.
eval_c(X,0,_):-is_cvar(X,_,_),!.

eval_c(sq_wave(V1,D1,V2,D2),V,N):-
	!,
	hasht([84,105,109,101],Nt),%get index of time
	g_read(datatab(Nt,N),T),%get current time value
	(((D1+D2)*float_fractional_part(T/(D1+D2))<D1)
	->
	 k_parameter(V1,V)
	;
	 k_parameter(V2,V)
	).

eval_c(M,MM,N):-
	k_macro(M,VV),!,
	write_to_codes(X,M),
	hasht(X,D),
	g_read(datatab(D,0),T),
	((T=0)
	->
	 eval_c(VV,MM,N)
	;
	 g_read(datatab(D,N),MM)
	).



eval_c(M,MM,N):-
   !,
   format_debug(7,"eval M:~w  at : ~w ~n",[M,N]),
   write_to_codes(X,M),
   !,
   (
      (hasht(X,D)) %evaluate molecules and Time
   ->
      (
         g_array_size(datatab,TD),
         g_array_size(datatab(2),TD2),	 
         format_debug(6,"eval_c read : I : ~w J: ~w  TD : ~w  TD2 :~w ~n ",[D,N,TD,TD2]),
         g_read(datatab(D,N),MM),
         format_debug(6,"N: ~w D: ~w ~n",[N,MM])
      )
   ;
      (
         (k_parameter(M,_)) %evaluate parameters
      ->
         k_parameter(M,MM) 
      ;
         (
            (is_var(M))
         ->
            (
               have_gui
            ->
               format("[GUI] errors More than one variable in a condition",[])
            ;
               format("Error : more than one variable in a condition",[])
            )
         ;
            (
               have_gui
            ->
               format("[GUI] checkLTL Unknown molecule : ~w ~n",[M]),
               format("[GUI] errors Unknown molecule : ~w ~n",[M])
            ;
               format("Unknown molecule : ~w ~n",[M])
            )
         ),
       g_assign(end,1),fail
      )
   ).
%%%%%%
is_var(P):-
   k_parameter(P,_),!,
   fail.

is_var(P):-
	k_macro(P,_),!,
	fail.

is_var(P):-
	write_to_codes([84,105,109,101],P),!,fail.

is_var(P):-
	atom(P).



is_cvar(P,1,P):-
	is_var(P),!.


is_cvar(-P,-1,P):-
	is_var(P),!.

is_cvar(C*P,C,P):-
	is_var(P),!.

is_cvar(P*C,C,P):-
	is_var(P),!.

is_cvar(P/C,CC,P):-
	%format("C ~w ~n",[C]),
	is_var(P),CC is 1/C,!.



%%%%%%%%expr > param
trace_domain_sup(H,Expr1,Expr2,N):-
	g_assign(ns,H),
	repeat,
	g_read(ns,NS),
	g_read(end,E),
	(((NS==0);(E==1))
	->
	 true
	;

	empty_tab_constraint(1),
	g_read(size,S),
	K is S+1-NS,
	eval_c(Expr1 - Expr2,Z,K),
	g_read(tdom(NS),L),
	build_constraint(Expr1-Expr2,+),
	ZZ is Z*1.0,
	d_build_dom(N,ZZ,3,DD),
	g_assign(tdom(NS),[(Expr1 '>=' Expr2,DD)|L]),
	 g_dec(ns),
	fail
	).


trace_domain_supo(NS,Expr1,Expr2,N):-
	g_assign(ns,NS),
	repeat,

	g_read(end,E),
	g_read(ns,H),
	(((H==0);(E==1))
	->
	 true
	;
	empty_tab_constraint(1),
	g_read(size,S),
	K is S+1-H,
	eval_c(Expr1 - Expr2,Z,K),
	g_read(tdom(H),L),
	 build_constraint(Expr1-Expr2,+),
	ZZ is Z*1.0,
	d_build_dom(N,ZZ,4,DD),
	g_assign(tdom(H),[(Expr1 '>' Expr2,DD)|L]),
	 g_dec(ns),
	 fail
	).

	 
%%%%%%%%expr > param
trace_domain_inf(NS,Expr1,Expr2,N):-
	g_assign(ns,NS),
	repeat,
	g_read(end,E),
	g_read(ns,H),
	(((H==0);(E==1))
	->
	 true
	;
	empty_tab_constraint(1),
	g_read(size,S),
	K is S+1-H,
	eval_c(Expr1-Expr2,Z,K),
	g_read(tdom(H),L),
	build_constraint(Expr1-Expr2,+),
	ZZ is Z*1.0,
	d_build_dom(N,ZZ,2,DD),
	g_assign(tdom(H),[(Expr1 '=<' Expr2,DD)|L]),
	 g_dec(ns),
	 fail
	).



trace_domain_info(NS,Expr1,Expr2,N):-
	g_assign(ns,NS),
	repeat,
	g_read(end,E),
	g_read(ns,H),
	(((H==0);(E==1))
	->
	 true
	;
	empty_tab_constraint(1),
	g_read(size,S),
	K is S+1-H,
	eval_c(Expr1-Expr2,Z,K),
	g_read(tdom(H),L),
	build_constraint(Expr1-Expr2,+),
	ZZ is Z*1.0,
	d_build_dom(N,ZZ,1,DD),
	g_assign(tdom(H),[(Expr1 '<' Expr2,DD)|L]),
	 g_dec(ns),
	 fail
	).




%%%%%%%Normalization negation
f_normalize('!'('!'(E)),E):-!.
f_normalize('!'('&'(E,F)),'|'(EE,FF)):-
	!,
	f_normalize('!'(E),EE),
	f_normalize('!'(F),FF).
	
f_normalize('!'('|'(E,F)),'&'(EE,FF)):-
	!,
	f_normalize('!'(E),EE),
	f_normalize('!'(F),FF).

f_normalize('!'('F'(E)),'G'(EE)):-
	!,
	f_normalize('!'(E),EE).

f_normalize(!('G'(E)),'F'(EE)):-
	!,
	f_normalize('!'(E),EE).

f_normalize('!'('U'(E,F)),'|'('G'(FF),('U'((FF),'&'(FF,EE))))):-
	!,
	f_normalize('!'(E),EE),
	f_normalize('!'(F),FF).

f_normalize(('!'('X'(E))),'X'(EE)):-
	!,
	f_normalize('!'(E),EE).
f_normalize('!'('->'(E,F)),'&'(EE,FF)):-
	!,
	f_normalize(E,EE),
	f_normalize('!'(F),FF).

f_normalize('!'(M>=P),AA):-!,f_normalize((M<P),AA).
f_normalize('!'(M=<P),AA):-!,f_normalize((M>P),AA).
f_normalize('!'(M>P),AA):-!,f_normalize((M=<P),AA).
f_normalize('!'(M<P),AA):-!,f_normalize((M>=P),AA).


f_normalize('!'(M=P),'|'(EE,FF)):-
	!,
	f_normalize((M<P),EE),
	f_normalize((M>P),FF).




f_normalize('&'(E,F),'&'(EE,FF)):-
	!,
	f_normalize(E,EE),
	f_normalize(F,FF).
f_normalize('|'(E,F),'|'(EE,FF)):-
	!,
	f_normalize(E,EE),
	f_normalize(F,FF).
f_normalize('F'(E),'F'(EE)):-
	!,
	f_normalize(E,EE).
f_normalize('G'(E),'G'(EE)):-
	!,
	f_normalize(E,EE).
f_normalize('U'(E,F),'U'(EE,FF)):-
	!,
	f_normalize(E,EE),
	f_normalize(F,FF).
f_normalize('X'(E),'X'(EE)):-
	!,
	f_normalize(E,EE).
f_normalize('->'(E,F),'|'(EE, FF)):-
	!,
	f_normalize('!'(E),EE),
	f_normalize(F,FF).
f_normalize('<->'(E,F),'&'(AA,BB)):-
	!,
	f_normalize(E,EE),
	f_normalize(F,FF),
	f_normalize('->'(EE,FF),AA),
	f_normalize('->'(FF,EE),BB).

f_normalize(curve_fit_error(Mol,Lvals,Ltimes,Lvars),curve_fit_error(Mol,Lvals,Ltimes,Lvars)):-!,
	store_varfit(Lvars).
f_normalize(curve_fit(Mol,Ltimes,Lvars),curve_fit(Mol,Ltimes,Lvars)):-!,
	store_varfit(Lvars).

f_normalize(M=<P,MM=<PP):-!,store_coeffs(M-P,+),f_normalize(M,MM),f_normalize(P,PP).
f_normalize(M>=P,MM>=PP):-!,store_coeffs(M-P,+),f_normalize(M,MM),f_normalize(P,PP).

f_normalize(M>P,MM>PP):-!,store_coeffs(M-P,+),f_normalize(M,MM),f_normalize(P,PP).
f_normalize(M<P,MM<PP):-!,store_coeffs(M-P,+),f_normalize(M,MM),f_normalize(P,PP).
f_normalize(M=P,'&'(AA,BB)):-store_coeffs(M-P,+),
	f_normalize(M,MM),f_normalize(P,PP),
	AA=(MM=<PP),
	BB=(MM>=PP).




f_normalize([M],[MM]):-
	!,parse_object(M,MM).%,check_mol(MM).
% has to be done before reaching the normal / normalization
f_normalize(d([M])/dt,d([MM])/dt):- 
   !,
   parse_object(M,MM).
f_normalize(A + B , AA + BB):-!,f_normalize(A,AA),f_normalize(B,BB).
f_normalize(A - B , AA - BB):-!,f_normalize(A,AA),f_normalize(B,BB).
f_normalize(- B , - BB):-!,f_normalize(B,BB).
f_normalize(A * B , AA * BB):-!,f_normalize(A,AA),f_normalize(B,BB).
f_normalize(A / B , AA / BB):-!,f_normalize(A,AA),f_normalize(B,BB).
f_normalize(A ^ B , AA ^ BB):-!,f_normalize(A,AA),f_normalize(B,BB).
f_normalize(log(B) , log(BB)):-!,f_normalize(B,BB).




f_normalize(Param,Param).


get_amplitude(D,A):-
	l_amplitude(D,L),
	max_list(L,A).
l_amplitude([],[]).
l_amplitude([H|D],[A|L]):-
	amplitude(H,A),
	l_amplitude(D,L).
	

amplitude([[(_,Vmin,_)],[(_,Vmax,_)]],A):-
	A is Vmax -Vmin.
	
e_norm(A >= B, A-B >= 0).
e_norm(A =< B, A-B =< 0).
build_constraint(A+B,S):-
	!,
	build_constraint(A,S),
	build_constraint(B,S).

build_constraint(A-B,S):-
	!,
	build_constraint(A,S),
	op(S,OpS),
	build_constraint(B,OpS).

build_constraint(-A,S):-
	!,
	op(S,OpS),
	build_constraint(A,OpS).

build_constraint(A,+):-
	is_cvar(A,C,Va),!,
	g_read(lgvars,L),
	(
	 (member((Va,NA),L))
	->
	 %format(" KA : ~w C : ~w ~n",[NA,C]),
	 var_coeff_assign(NA,C)
	 ;
	 var_get_key(Va,KA),
	 %format("A: ~w KA : ~w C : ~w ~n",[A,KA,C]),
	 var_coeff_assign(KA,C),
	 g_assign(lgvars,[(Va,KA)|L])
	 ).


build_constraint(A,-):-
	is_cvar(A,CC,Va),!,
	C is -CC,
	g_read(lgvars,L),
	(
	 (member((Va,NA),L))
	->
	 %format(" _KA : ~w C : ~w ~n",[NA,C]),
	 var_coeff_assign(NA,C)
	 ;
	 var_get_key(Va,KA),
	 %format("_A: ~w KA : ~w C : ~w ~n",[A,KA,C]),
	 var_coeff_assign(KA,C),
	 g_assign(lgvars,[(Va,KA)|L])
	 ).





build_constraint(_,_).

op(-,+).
op(+,-).

store_coeffs(A+B,S):-
	!,
	store_coeffs(A,S),
	store_coeffs(B,S).

store_coeffs(A-B,S):-
	!,
	store_coeffs(A,S),
	op(S,OpS),
	store_coeffs(B,OpS).

store_coeffs(-A,S):-
	!,
	op(S,OpS),
	store_coeffs(A,OpS).

store_coeffs(A,+):-
	is_cvar(A,C,Va),!,
	g_read(lgvars,L),
	(
	 (member((Va,NA),L))
	->
%	 format(" NA : ~w C : ~w ~n",[NA,C]),
	 var_coeff_store(NA,C)
	 ;
	 var_get_key(Va,KA),
%	 format("A: ~w KA : ~w C : ~w ~n",[A,KA,C]),
	 var_coeff_store(KA,C),
	 g_assign(lgvars,[(Va,KA)|L])
	 ).


store_coeffs(A,-):-
	is_cvar(A,CC,Va),!,
	C is -CC,
	g_read(lgvars,L),
	(
	 (member((Va,NA),L))
	->
%	 format(" NA : ~w C : ~w ~n",[NA,C]),
	 var_coeff_store(NA,C)
	 ;
	 var_get_key(Va,KA),
%	 format("A: ~w KA : ~w C : ~w ~n",[A,KA,C]),
	 var_coeff_store(KA,C),
	 g_assign(lgvars,[(Va,KA)|L])
	 ).



store_coeffs(_,_).



	
%%%%%%%%
fill_domains(NH):-
	g_assign(nh,NH),
	repeat,
	g_read(nh,H),
	((H==0)
	->
	 true
	;
	g_assign(tdom(H),[]),
	 g_dec(nh),
	 fail
	).

	
%%%%single
list_remove_dupl([],[]).
list_remove_dupl([H|T],Lr):-
	memberchk(H,T),!,
	list_remove_dupl(T,Lr).
list_remove_dupl([H|T],[H|Lr]):-
	list_remove_dupl(T,Lr).


%%%%%compute derivatives
compute_all_derivatives:-
	g_read(lmols,L),
	findall(X,hasht(X,_),LL),
	length(LL,Nd),
	NN is Nd+1,
	compute_all_derivatives(L,NN).
compute_all_derivatives([],_).
compute_all_derivatives([H|T],N):-
 	compute_data_derivatives(H,N),
	NN is N+1,
	compute_all_derivatives(T,NN).

compute_data_derivatives(M,N):-
	hasht(M,Nm),
	hasht([84,105,109,101],Nt),
	append([100,40],M,L1),
	append(L1,[41],L2),
	assertz(hasht(L2,N)),
	g_read(size,S),
	compute_data_derivatives(N,Nm,Nt,S,S),!.



compute_data_derivatives(Nd,Nm,Nt,K,S):-
	g_assign(nk,K),
	repeat,
	g_read(nk,NK),
	((NK==1)
	->
	 g_assign(datatab(Nd,S),0.0)
	 ;	 
	 I is S-NK+1,
	 g_read(datatab(Nt,I),T),
	 g_read(datatab(Nm,I),D),
	 II is I+1,
	 g_read(datatab(Nt,II),TPrec),
	 g_read(datatab(Nm,II),DPrec),
	 Dt is (D-DPrec)/(T-TPrec),
	 g_assign(datatab(Nd,I),Dt),
	 g_dec(nk),
	 fail
	 ).


compute_all_derivatives2:-
	g_read(lmols,L),
	findall(X,hasht(X,_),LL),
	length(LL,Nd),
	NN is Nd+1,
	compute_all_derivatives2(L,NN).
compute_all_derivatives2([],_).
compute_all_derivatives2([H|T],N):-
	compute_data_derivatives2(H,N),
	NN is N+1,
	compute_all_derivatives2(T,NN).

compute_data_derivatives2(M,N):-
	hasht([84,105,109,101],Nt),
	append([100,40],M,L1),
	append(L1,[41],L2),
	hasht(L2,Nm),
	delete(M,91,M2),
	delete(M2,93,M3),
	append([91,100,50],M3,M4),
	append(M4,[93],M5),
	assertz(hasht(M5,N)),
	g_read(size,S),
	compute_data_derivatives2(N,Nm,Nt,S,S),!.



compute_data_derivatives2(Nd,Nm,Nt,K,S):-
	g_assign(nk,K),
	repeat,
	g_read(nk,NK),
	((NK==1)
	->
	 g_assign(datatab(Nd,S),0.0)
	 ;	 
	 I is S-NK+1,
	 g_read(datatab(Nt,I),T),
	 g_read(datatab(Nm,I),D),
	 II is I+1,
	 g_read(datatab(Nt,II),TPrec),
	 g_read(datatab(Nm,II),DPrec),
	 Dt is (D-DPrec)/(T-TPrec),
	 g_assign(datatab(Nd,I),Dt),
	 g_dec(nk),
	 fail
	 ).


%%%%%%%%%distance (point, domain)%%%%%%%

dist_ppl(Fval,Dom,D):-
	%write(dist),nl,
        %format("Fval : ~w ~nDom ~w ~nDD ~w  ~n",[Fval,Dom,DD]),
	dist_ppl_aux(Fval,Dom,DD),
	%write(dist2),nl,	
	D is sqrt(DD).


dist_ppl_aux([],_,0).
dist_ppl_aux([(V,ValV)|T],Dom,R):-
	g_read(lgvars,LG),
	(
	 (member((V,NA),LG))
	->
	 (
	  dist_ppl_aux(T,Dom,RR),
	  ValVV is 1.0*ValV,
	  dist_var_domain(Dom,NA,ValVV,D),
	  R is RR + D**2
	 )
	;
	 (
	  have_gui
	 ->
	  format("[GUI] checkLTL Unrecognized variable : ~w ~n",[V]),
	  format("[GUI] errors Unrecognized variable : ~w ~n",[V])
	 ;
	  format("Unrecognized variable : ~w ~n",[V])
	 )
	).



varlist_aux([],[],[]).
varlist_aux([V|Vlist],[Val|Vallist],[(V,Val)|List]):-
	varlist_aux(Vlist,Vallist,List).


merge([],[],[]).
merge([P|Plist],[Incr|IncrList],[(P,Incr)|MList]):-
	merge(Plist,IncrList,MList).

	
condition([],[]).

condition([(Min,Max)|Interval],[P|Params]):-
	k_parameter(P,Pval),
	(
	 (Pval>Min,Pval<Max)
	->
	 condition(Interval,Params)
	;
	 fail
	).



get_set_init_values2([],[],[]).
get_set_init_values2([K|ParamList],[Kinit|InitValues],[(_,_)|IntervalList]):-
   (
      retract(k_parameter(K,Kinit))
   ->
      assertz(k_parameter(K,Kinit))
   ;
        (
         have_gui
                 ->
                        format("[GUI] checkLTL Error: ~w is not a known parameter.~n",[K]),
                        format("[GUI] errors ~w is not a known parameter.~n",[K])
                 ;
      			write_line_col('Error'),
                        format("~w is not a known parameter.~n",[K])
        ),
      fail
   ),
   get_set_init_values2(ParamList,InitValues,IntervalList).



eval_mol(M,T,V):-
	write_to_codes(X,M),
	(
	 (k_macro(M,_))
	->
	 XXX=X
	;
	 (
	  append([91],X,XX),
	  append(XX,[93],XXX)
	  )
	),
	((hasht(XXX,Nm))
	 ->
	  true
	 ;
	  (
	   have_gui
	  ->
	   format("[GUI] checkLTL Unknown molecule ~w ~n",[M])
	  ;
	   format(" Unknown molecule ~w ~n",[M])
	  ),
	  fail
	  ),
	hasht([84,105,109,101],Nt),
	find_nth(T,It),
	Itnext is It -1,
	g_read(datatab(Nm,It),Mv),
	g_read(datatab(Nm,Itnext),Mvnext),
	g_read(datatab(Nt,It),Tv),
	g_read(datatab(Nt,Itnext),Tvnext),
	V1 is (Mvnext-Mv)/(Tvnext-Tv),
	V is Mv+V1*(T-Tv).



find_nth(T,It):-
       g_read(size,S),
       hasht([84,105,109,101],Nt),
       find_aux(S,T,Nt,It).


find_aux(1,_,_,_):-
       !,
       
        (
         have_gui
                 ->
                         format("[GUI] checkLTL Error : requested time in curve_fit outside simulated trace ~n",[])
                 ;
                         write('Error : requested time in curve_fit outside simulated trace'),nl
        )
	,fail.

find_aux(S,T,Nt,It):-
       Snext is S-1,
       g_read(datatab(Nt,S),Tv),
       g_read(datatab(Nt,Snext),Tvnext),
       (
       ((Tv =< T),(T<Tvnext))
       ->
       It is S
       ;
       find_aux(Snext,T,Nt,It)
       ).

landscape_log(ParamList,IntervalList,Spec,VarList,ObjList,Grid,N,File):-
	g_assign(land,log),
	landscape(ParamList,IntervalList,Spec,VarList,ObjList,Grid,N,File),	
	g_assign(land,norm).

%landscape(_,_,_,_,_,_,_,File):-
%   \+(atom(File)),!,
%   write_line_col('Error'),
%   write('Filename must be an atom, or be enclosed in simple quotes\n').


landscape(ParamList,IntervalList,Spec,VarList,ObjList,Grid,N,File):-
   atom_concat(File,'.csv',Land_data),
   atom_concat(File,'.plot',Land_plot),

   open(Land_data,write,St), % fichier landscape a modifier
   get_set_init_values(ParamList,InitValues,IntervalList),
   g_read(land,C),
   (
      (C==log)
   ->
      get_incr_list_log(IntervalList,Grid,IncrList)
   ;
      get_incr_list_norm(IntervalList,Grid,IncrList)
   ),
   objectivelist(VarList,ObjList,Fval),
   (
      (ParamList=[P1,P2])
   ->
      g_assign(landsp1,P1)
   ;
      (
         have_gui
      ->
         format("[GUI] checkLTL landscape: There can only be two dimensions~n",[])
      ;
         format("There can only be two dimensions~n",[])
      )
   ),
   g_read(debug,Debug),
   g_assign(nbtotal,Grid),
   g_assign(nbcurr,0),
   g_assign(debug,-1),
   repeat,
   (  
      numerical_simulation(N),
      % format("F : ~w , SP : ~w ~n",[F,SP]),
      trace_analyze6(Spec),
      g_read(tdom(1),L),
      f_normalize(Spec,FF2),
      memberchk((FF2,Dom),L),
      empty_domains,
      dist_ppl(Fval,Dom,DD),
       D is 1/(1+DD),
      ParamList=[X,Y],
      k_parameter(X,VX),
      k_parameter(Y,VY),
      format(St,"~f ~f  ~3f ~n",[VX,VY,D]),
      clean_ppl,
            %	 format("Pa ~w ~nIncr ~w ~nInt ~w~n",[ParamList,IncrList,IntervalList]),
      \+(
         (
            (C==log)
         ->
            incr_list_log(ParamList,IncrList,IntervalList,St)
         ;
            incr_list_norm(ParamList,IncrList,IntervalList,St)
         )
      )
      
   ),!,
   g_assign(debug,Debug),
   set_init_values(ParamList,InitValues),

   open(Land_plot,write,Gnuplot),
   write(Gnuplot,'set pm3d\nset pm3d map\n'),
   write(Gnuplot,'set pm3d corners2color c2\n'),

   (
      (C==log)
   ->
      write(Gnuplot,'set logscale x\n set logscale y\n')	 
   ;
      true
   ),
   IntervalList=[(Xmin,Xmax),(Ymin,Ymax)],

   format(Gnuplot,"set xrange [~g:~g]~nset yrange [~g:~g]~nset cbrange [*:*]~nset zrange [*:*]~n",[Xmin,Xmax,Ymin,Ymax]),
   format(Gnuplot,"set xlabel \"~w\"~nset ylabel \"~w\"~n",[P1,P2]),
   (
         have_gui
                 ->
			  atom_concat(File,'.png',Land_image),
                          format(Gnuplot,"set terminal png~nset output \"~w\"~n",[Land_image])
                 ;
                         true
    ),
   format(Gnuplot,"splot \"~w\" using 1:2:3 title \"Satisfaction degree\"~n",[Land_data]),


   flush_output(St),
   flush_output(Gnuplot),

   close(Gnuplot),
%   plot_land(Land_plot),
   close(St),
   (
         have_gui
                 ->
                         format("[GUI] landscape FINISHED ~n",[])
                 ;
                         true
    ).



satisfaction_degree_plot_log(P,I,Spec,VarList,ObjList,Grid,N,File):-
	g_assign(land,log),
	satisfaction_degree_plot(P,I,Spec,VarList,ObjList,Grid,N,File),
	g_assign(land,norm).


satisfaction_degree_plot(P,I,Spec,VarList,ObjList,Grid,N,File):-
   atom_concat(File,'.csv',Land_data),
   atom_concat(File,'.plot',Land_plot),

   open(Land_data,write,St), % fichier landscape a modifier
   get_set_init_values([P],InitValues,I),
   g_read(land,C),
   (
      (C==log)
   ->
      get_incr_list_log(I,Grid,IncrList)
   ;
      get_incr_list_norm(I,Grid,IncrList)
   ),
   objectivelist(VarList,ObjList,Fval),
   
   g_assign(landsp1,0),
   
   g_read(debug,Debug),
   g_assign(nbtotal,Grid),
   g_assign(nbcurr,0),
   g_assign(debug,-1),
   repeat,
   (  
      numerical_simulation(N),
      trace_analyze6(Spec),
      g_read(tdom(1),L),
      f_normalize(Spec,FF2),
      memberchk((FF2,Dom),L),
      empty_domains,
      dist_ppl(Fval,Dom,DD),
       D is 1/(1+DD),
      k_parameter(P,VP),
      format(St,"~f  ~3f ~n",[VP,D]),
      %format("~f  ~3f ~n",[VP,D]),
      clean_ppl,
       \+(
         (
            (C==log)
         ->
            incr_list_log([P],IncrList,I,St)
         ;
            incr_list_norm([P],IncrList,I,St)
         )
      )
      
   ),!,
   g_assign(debug,Debug),
   set_init_values([P],InitValues),

   open(Land_plot,write,Gnuplot),
   write(Gnuplot,'set style data lines\n'),

   (
      (C==log)
   ->
      write(Gnuplot,'set logscale x\n')	 
   ;
      true
   ),
   I=[(Xmin,Xmax)],

   format(Gnuplot,"set xrange [~g:~g]~nset yrange [*:*]~n",[Xmin,Xmax]),
   format(Gnuplot,"set xlabel \"~w\"~nset ylabel \"Satisfaction degree\"~n",[P]),
   (
         have_gui
                 ->
			  atom_concat(File,'.png',Land_image),
                          format(Gnuplot,"set terminal png~nset output \"~w\"~n",[Land_image])
                 ;
                         true
    ),
   format(Gnuplot,"plot \"~w\" using 1:2 lt -1 lw 2 title \"Specification satisfaction degree\"~n",[Land_data]),


   flush_output(St),
   flush_output(Gnuplot),

   close(Gnuplot),
%   plot_land(Land_plot),
   close(St),
   (
         have_gui
                 ->
                         format("[GUI] landscape FINISHED ~n",[])
                 ;
                         true
    ).









objectivelist([],[],[]).
objectivelist([V|TV],[O|TO],[(V,O)|T]):-
	objectivelist(TV,TO,T).

objective(LTL_objective):-
	g_assign(v_obj,[]),
	get_objective(LTL_objective).

get_objective('&'(E,F)):-
 	!,
 	get_objective(E),
 	get_objective(F).

get_objective(Expr = O):-
 	!,
	g_read(v_obj,L),
	g_assign(v_obj,[(Expr,O)|L]).

%Anthony,fss
fss_params(Maxsimul,Maxevals,Stopfitness,Std,Fish,Debug):-
	fss_params(Maxsimul,Stopfitness),
	fss_init_maxevals(Maxevals),
	fss_init_fish(Fish),
	fss_init_debug(Debug),
	fss_init_std(Std).


%Anthony,fss
fss_params(Maxsimul,Stopfitness):-
	g_assign(maxsimul,Maxsimul),
	g_assign(stopfitness,Stopfitness),
	assertz(fss_init).

%Anthony,fss
fss_read_params(Maxsimul,Stopfitness):-
	(
	 fss_init
	->
	 g_read(maxsimul,Maxsimul),
	 g_read(stopfitness,Stopfitness)
	 ;
	 Maxsimul=50,
	 Stopfitness=0.01
	 ).


%Anthony,fss
fss_init_debug(Debugcode):-
	g_assign(debugcode,Debugcode),
	assertz(fss_debugcode).

%Anthony,fss
fss_read_debug(Debugcode):-
	(
	 fss_debugcode
	->
	 g_read(debugcode,Debugcode)
	 ;
	 Debugcode=0
	 ).
	 
%Anthony,fss
fss_init_std(Std):-
	g_assign(std,Std),
	assertz(fss_std).

%Anthony,fss
fss_read_std(Std):-
	(
	 fss_std
	->
	 g_read(std,Std)
	 ;
	 Std=0.1
	 ).
	 
%Anthony,fss
fss_init_maxevals(Maxevals):-
	g_assign(maxevals,Maxevals),
	assertz(fss_maxevals).

%Anthony,fss
fss_read_maxevals(Maxevals):-
	(
	 fss_maxevals
	->
	 g_read(maxevals,Maxevals)
	 ;
	 Maxevals=1000
	 ).
	 
%Anthony,fss
fss_init_fish(Fish):-
	g_assign(fish,Fish),
	assertz(fss_fish).

%Anthony,fss
fss_read_fish(Fish):-
	(
	 fss_fish
	->
	 g_read(fish,Fish)
	 ;
	 Fish=10
	 ).
	
%used by cmaes, robustness and landscape
%does not clean domains
% to enable satisfaction degree computation
trace_analyze6(E):-
	ensure_data,
	ensure_ppl,
	empty_tab_vars(1),
	g_assign(lgvars,[]),
	f_normalize(E,EE),
 	ensure_domains,!,
  	trace_domain(EE),!.

cmaes_params(Maxsimul,Stopfitness,Std):-
	cmaes_params(Maxsimul,Stopfitness),
	cmaes_init_std(Std).


cmaes_params(Maxsimul,Stopfitness):-
	g_assign(maxsimul,Maxsimul),
	g_assign(stopfitness,Stopfitness),
	assertz(cmaes_init).

cmaes_read_params(Maxsimul,Stopfitness):-
	(
	 cmaes_init
	->
	 g_read(maxsimul,Maxsimul),
	 g_read(stopfitness,Stopfitness)
	 ;
	 Maxsimul=300,
	 Stopfitness=0.01
	 ).


cmaes_seed(Seed):-
	g_assign(seed,Seed).

read_seed(Seed):-
   g_read(seed,Seed).

cmaes_init_std(Std):-
	g_assign(std,Std),
	assertz(cmaes_std).

read_std(Std):-
	(
	 cmaes_std
	->
	 g_read(std,Std)
	 ;
	 Std=0.1
	 ).



satisfaction_degree(Spec,VarList,ObjList,N):-
	statistics(cpu_time,[T1,_]),
	g_assign(timesimul,N),
	objectivelist(VarList,ObjList,V_OList),
	g_assign(fval,V_OList),
	g_assign(formula,Spec),
	distance4(VD),%VD is violation degree
	SD is 1/(1+VD),	
	(
         have_gui
                 ->
                         format("[GUI] satisfactionDegree The satisfaction degree is ~g . ~n",[SD])
                 ;
                         format("Satisfaction degree ~g ~n",[SD])
    	),
	statistics(cpu_time,[T2,_]),
 	T is (T2-T1),
 	TT is T/1000,
 	format("Time elapsed ~2f s",[TT]).


%Anthony - fss
search_parameters_fss_log(ParamList,IntervalList,Spec,VarList,ObjList,N):-
	g_assign(fss,log),
	search_parameters_fss(ParamList,IntervalList,Spec,VarList,ObjList,N),
	g_assign(fss,norm).


%Anthony - fss
search_parameters_fss(ParamList,IntervalList,Spec,VarList,ObjList,N):-
	check_valid_fss_params(ParamList,IntervalList,VarList,ObjList),
	statistics(cpu_time,[T1,_]),
	g_read(debug,Debug),
	var_norm_init(1),
	%initialize_ppl, %already checked in trace_analyze(6)
	g_assign(debug,-1),
	length(ParamList,Dim),
	g_assign(fssbounds,g_array(Dim)),
	g_assign(interval0,IntervalList),
	fss_bounds(IntervalList,0),
	get_set_init_values2(ParamList,InitValues,_),
	objectivelist(VarList,ObjList,V_OList),
	set_init_fss(ParamList,Plist),
	g_assign(timesimul,N),
	g_assign(fval,V_OList),

	g_assign(formula,Spec),
	g_assign(p_t_list,Plist),
	disp_initial(ParamList,N),
	fss_read_params(Maxsimul,Stopfitness),
	fss_read_maxevals(Maxevals),
	read_seed(Seed),
	fss_read_std(Std),
	fss_read_fish(Fish),
	fss_read_debug(Debugcode),
	call_main_fss(Dim,0,Maxsimul,Maxevals,Stopfitness,Seed,Std,Fish,Debugcode),	
	(
	 have_gui
	->
	 getFssCost(FF),
	 format("[GUI] checkLTL Final Cost: ~w~n",[FF]),
	 format("[GUI] checkLTL Found Parameters :  ~n",[]),
	 writeall_fss((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A))
	;
	 format("Found Parameters :~n",[]),
	 writeall((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A))
	),
	set_init_values(ParamList,InitValues),
	g_assign(debug,Debug),
	statistics(cpu_time,[T2,_]),
	T is (T2-T1),
	TT is T/1000,
	(
	 have_gui
	->
	 format("[GUI] checkLTL Time elapsed ~2f s ~n",[TT])
	;
	 format("Time elapsed ~2f s",[TT])
	).

%Anthony - fss
fss_restart(_):-
	g_read(p_t_list,Plist),
	fss_restart_aux(Plist,0,Plistnew),
	g_assign(p_t_list,Plistnew).


%Anthony - fss
fss_restart_aux([],_,[]).
fss_restart_aux([(P,_)|Plist],N,[(P,Pval2)|Plist2]):-
	%write(retart_aux),nl,
	g_read(fssbounds(N),(Min,Max)),
	%write(bounds_read),nl,
	(
	 (Min=:=0)
	->
	 Min3=0.00001
	 ;
%	 Min3 is 1.2*Min
	 Min3 is Min
	),
%	Max3 is 0.8*Max,
	Max3 is Max,
	random(Min3,Max3,Pval2),
	NN is N+1,
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,Pval2)),
	
	fss_restart_aux(Plist,NN,Plist2).



%Anthony - fss
fss_bounds([],_).
fss_bounds([(Min,Max)|T],N):-
	g_assign(fssbounds(N),(Min,Max)),
	NN is N + 1,
	fss_bounds(T,NN).

%Anthony - fss
set_init_fss([],[]).
set_init_fss([P|Plist],[(P,Pval)|Ilist]):-
	k_parameter(P,Pval),
	set_init_fss(Plist,Ilist).
		
%Anthony - fss
set_param_fss([],[],[]).
set_param_fss([V|Vlist],[(P,T)|Plist],[(Min,Max)|Ilist]):-
	V2 is V*T,
	((V2<Min)
	->
	 (V3 is Min,Err is (V2-Min)**4)
	;
	 ((V2>Max)
	 ->
	  (V3 is Max,Err is (V2-Max)**4)
	 ;
	  (Err=0, V3 is V2)
	 )
	),
	g_read(bound_penalty,Pen),
	Pen2 is Pen +Err,
	g_assign(bound_penalty,Pen2),
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,V3)),
	set_param_fss(Vlist,Plist,Ilist).


%Anthony - fss
set_param_log_fss([],[],[]).
set_param_log_fss([V|Vlist],[(P,T)|Plist],[(Min,Max)|Ilist]):-
	V2 is (T*exp((V-1)*log(10))),
	((V2<Min)
	->
	 (V3 is Min,Err is (V2-Min)**4)
	;
	 
	 ((V2>Max)
	 ->
	  (V3 is Max,Err is (V2-Max)**4)
	 ;
	  (Err=0, V3 is V2)
	 )
	),
	
	g_read(bound_penalty,Pen),
	Pen2 is Pen +Err,

	g_assign(bound_penalty,Pen2),
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,V3)),
	set_param_log_fss(Vlist,Plist,Ilist).


%Anthony - fss
simul_fss(Vlist):-
	g_read(p_t_list,Plist),
	g_read(timesimul,N),
	g_read(interval0,Ilist),
	set_param_fss(Vlist,Plist,Ilist),
	numerical_simulation(N),
	(
     		have_gui
   		->
      			format("[GUI] fssPlotTrue ~n",[]),
			plot,
			format("[GUI] fssPlotFalse ~n",[])

   		;
      			plot
   	).
	



search_parameters_cmaes_log(ParamList,IntervalList,Spec,VarList,ObjList,N):-
	g_assign(cmaes,log),
	search_parameters_cmaes(ParamList,IntervalList,Spec,VarList,ObjList,N),
	g_assign(cmaes,norm).

%call_main_cmaes(Dim, Fun, Maxsimul, Stopfitness, Seed, Std) :-
%   save_state,
%   g_read(debug, Debug),
%   print_to_atom(DebugAtom, Debug),
%   print_to_atom(DimAtom, Dim),
%   print_to_atom(FunAtom, Fun),
%   print_to_atom(MaxsimulAtom, Maxsimul),
%   print_to_atom(StopfitnessAtom, Stopfitness),
%   print_to_atom(SeedAtom, Seed),
%   print_to_atom(StdAtom, Std),
%   format_debug(1,"call cmaes_biocham ~a ~a ~a ~a ~a ~a ~a~n",[DebugAtom, DimAtom, FunAtom, MaxsimulAtom, StopfitnessAtom, SeedAtom, StdAtom]),
%   spawn('cmaes_biocham', [DebugAtom, DimAtom, FunAtom, MaxsimulAtom, StopfitnessAtom, SeedAtom, StdAtom]),
%   restore_state.

:- foreign(slave_call_c(+string)).

slave_call(Goal) :-
   write_canonical_to_atom(Atom, Goal),
   slave_call_c(Atom).

client_call(Atom) :-
   read_term_from_atom(Atom, Goal, [end_of_term(eof)]),
   call(Goal).

save_state :-
   \+ (
      member(F/N, [rule/7, k_parameter/2, value/2, k_value/3, initconc/2, orule/1, molecule/2, k_macro/2, k_new_macro/1, trace_loaded/0, cmaes_init/0, cmaes_std/0, hasht/2,data/1]),
      functor(P, F, N),
      slave_call(retractall(P)),
      call(P),
      \+ (
         slave_call(assertz(P))
      )
   ),
   \+ (
      member(X, [bound_penalty,bounds,cmaes,datatab,debug,debugcode,done,end,fish,formula,fss,fssbounds,fval,ilist_g_init,interval0,k,land,landsp1,lgvars,lmols,macrosl,macroslwithdef,maxevals,maxsimul,means,nbcurr,nbto,nbtotal,nbtrace,nf,nh,nk,nn,ns,ntime,obj0,objectives,paramlist,params0,plist_g_init,p_t_list,replace,rob,seed,size,spec0,specs,std,stopfitness,sumdist,sums,tdom,timesim0,timesimul,trace_times,varlist0,varlists,v_obj]),
      g_read(X, V),
      \+ (
         slave_call(g_assign(X, V))
      )
   ).

search_parameters_cmaes(ParamList,IntervalList,Spec,VarList,ObjList,N):-
	check_valid_cmaes_params(ParamList,IntervalList,VarList,ObjList),
	statistics(cpu_time,[T1,_]),
	var_norm_init(1),
	%initialize_ppl, %already checked in trace_analyze(6)
	length(ParamList,Dim),
	g_assign(bounds,g_array(Dim)),
	g_assign(interval0,IntervalList),
	cmaes_bounds(IntervalList,0),
	get_set_init_values2(ParamList,InitValues,_),
	objectivelist(VarList,ObjList,V_OList),
	set_init_cmaes(ParamList,Plist),
	g_assign(timesimul,N),
	g_assign(fval,V_OList),

	g_assign(formula,Spec),
	g_assign(p_t_list,Plist),
	disp_initial(ParamList,N),
	cmaes_read_params(Maxsimul,Stopfitness),
	read_seed(Seed),
	read_std(Std),
        save_state,
	call_main_cmaes(Dim,0,Maxsimul,Stopfitness,Seed,Std),
	(
	 have_gui
           ->
	 getFCost(FF),
	 format("[GUI] checkLTL Final Cost: ~w~n",[FF]),
	 format("[GUI] checkLTL Found Parameters :  ~n",[]),
	 writeall_cmaes((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A))
	;
	 format("Found Parameters :~n",[]),
	 writeall((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A))
	),
	set_init_values(ParamList,InitValues),
	statistics(cpu_time,[T2,_]),
	T is (T2-T1),
	TT is T/1000,
	(
	 have_gui
	->
	 format("[GUI] checkLTL Time elapsed ~2f s ~n",[TT])
	;
	 format("Time elapsed ~2f s",[TT])
	).

cmaes_restart(_):-
	g_read(p_t_list,Plist),
	cmaes_restart_aux(Plist,0,Plistnew),
	g_assign(p_t_list,Plistnew).


cmaes_restart_aux([],_,[]).
cmaes_restart_aux([(P,_)|Plist],N,[(P,Pval2)|Plist2]):-
	%write(retart_aux),nl,
	g_read(bounds(N),(Min,Max)),
	%write(bounds_read),nl,
	(
	 (Min=:=0)
	->
	 Min3=0.00001
	 ;
	 Min3 is 1.2*Min
	),
	Max3 is 0.8*Max,
	random(Min3,Max3,Pval2),
	NN is N+1,
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,Pval2)),
	
	cmaes_restart_aux(Plist,NN,Plist2).



cmaes_bounds([],_).
cmaes_bounds([(Min,Max)|T],N):-
	NN is N + 1,
	g_assign(bounds(N),(Min,Max)),
	cmaes_bounds(T,NN).


bounds(N,Min,Max):-
	g_read(p_t_list,Plist),
	%format("N : ~w Plist ~w ~n",[N,Plist]),
	NN is N+1,
	nth(NN,Plist,(_,Pref)),
	%format("Pref~w ~n",[Pref]),
	g_read(bounds(N),(Min1,Max1)),
	Min is Min1*1.0/Pref,
	Max is Max1*1.0/Pref.

fssbounds(N,Min,Max):-
	g_read(p_t_list,Plist),
	%format("N : ~w Plist ~w ~n",[N,Plist]),
	NN is N+1,
	nth(NN,Plist,(_,Pref)),
	%format("Pref~w ~n",[Pref]),
	g_read(fssbounds(N),(Min1,Max1)),
	Min is Min1*1.0/Pref,
	Max is Max1*1.0/Pref.




set_init_cmaes([],[]).
set_init_cmaes([P|Plist],[(P,Pval)|Ilist]):-
	k_parameter(P,Pval),
	set_init_cmaes(Plist,Ilist).

disp_initial(Plist,N):-
   numerical_simulation(N),
   distance4(D),
   (
      have_gui
   ->
      format("[GUI] checkLTL Initial Parameters : ~n",[]),
      writeall_cmaes((member(P,Plist),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A)),
      format("[GUI] checkLTL Initial Cost : ~4f ~n",[D]),
      format("[GUI] checkLTL In progress..... Please wait........................~n",[])
   ;
%      plot,
      format("Initial Parameters :~n",[]),
      writeall((member(P,Plist),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A)),
      format("~nInitial Cost : ~4f ~n",[D])
   ),nl.


disp_initial_aux([]).

disp_initial_aux([(P,Pval)|Ptlist]):-
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,Pval)),
	disp_initial_aux(Ptlist).


bounds(B):-
	g_read(bounds,B).


make_ptlist([],[],[]).
make_ptlist([P|Plist],[(V,_)|Ilist],[(P,V)|Ptlist]):-
	make_ptlist(Plist,Ilist,Ptlist).


distance4(D):-
	g_read(timesimul,N),
	g_read(fval,Fval),
	g_read(formula,F),
	numerical_simulation(N),
	trace_analyze6(F),
	g_read(tdom(1),L),
	f_normalize(F,FF2),
	memberchk((FF2,Dom),L),
	empty_domains,!,
	dist_ppl(Fval,Dom,D),
	clean_ppl.

satisfaction_degree(D):-
	distance4(DD),
	D is 1/(1+DD).

distance_cmaes(VPlist,D):-
        \+ \+ (
	g_read(p_t_list,Plist),
	g_read(cmaes,C),
	g_read(interval0,Ilist),
	((C==log)
	->
	 set_param_log_cmaes(VPlist,Plist,Ilist)
	 ;
	 set_param_cmaes(VPlist,Plist,Ilist)
	),
	distance4(D),
        g_assign(distance_result, D)
        ),
        g_read(distance_result, D).

set_param_cmaes([],[],[]).
set_param_cmaes([V|Vlist],[(P,T)|Plist],[(Min,Max)|Ilist]):-
	V2 is V*T,
	((V2<Min)
	->
	 (V3 is Min,Err is (V2-Min)**4)
	;
	 ((V2>Max)
	 ->
	  (V3 is Max,Err is (V2-Max)**4)
	 ;
	  (Err=0, V3 is V2)
	 )
	),
	g_read(bound_penalty,Pen),
	Pen2 is Pen +Err,
	g_assign(bound_penalty,Pen2),
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,V3)),
	set_param_cmaes(Vlist,Plist,Ilist).




set_param_log_cmaes([],[],[]).
set_param_log_cmaes([V|Vlist],[(P,T)|Plist],[(Min,Max)|Ilist]):-
	V2 is (T*exp((V-1)*log(10))),
	((V2<Min)
	->
	 (V3 is Min,Err is (V2-Min)**4)
	;
	 
	 ((V2>Max)
	 ->
	  (V3 is Max,Err is (V2-Max)**4)
	 ;
	  (Err=0, V3 is V2)
	 )
	),
	
	g_read(bound_penalty,Pen),
	Pen2 is Pen +Err,

	g_assign(bound_penalty,Pen2),
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,V3)),
	set_param_log_cmaes(Vlist,Plist,Ilist).


simul_cmaes(Vlist):-
	g_read(p_t_list,Plist),
	g_read(timesimul,N),
	g_read(interval0,Ilist),
	set_param_cmaes(Vlist,Plist,Ilist),
	numerical_simulation(N).
%	(
%     		have_gui
%   		->
%      			format("[GUI] cmaesPlotTrue ~n",[]),
%			plot,
%			format("[GUI] cmaesPlotFalse ~n",[])
%
%   		;
%      			plot
%   	).

get_incr_list_norm([],_MaxIter,[]).
get_incr_list_norm([(Vmin,Vmax)|IntervalList],MaxIter,[Incr|IncrList]):-
	Incr is (Vmax - Vmin)/MaxIter,
	get_incr_list_norm(IntervalList,MaxIter,IncrList).



incr_list_log([_|ParamList],[_|IncrList],[_|IntervalList],St):-
   incr_list_log(ParamList,IncrList,IntervalList,St),
   !.

incr_list_log([K|_],[Incr|_],[(Kmin,Kmax)|_],St):-
   retract(k_parameter(K,Kval)),
   KK is Kval * Incr,
   (
      (KK =< (Kmax*1.1))
   ->(g_read(landsp1,P1),
      ((P1==K)
      ->
       format(St,"~n",[])
      ;
       true
      ),
     assertz(k_parameter(K,KK))
     )
     ;
    assertz(k_parameter(K,Kmin)),
    g_inc(nbcurr),
%    g_read(nbcurr,Cu), %%affichage avancement du calcul ( a modifier)
%    g_read(nbtotal,Tot),
%    Perc is (Cu/Tot)*100,
 %   format("~0f ",[Perc]),flush_output,
      fail
   ).




incr_list_norm([_|ParamList],[_|IncrList],[_|IntervalList],St):-
   incr_list_norm(ParamList,IncrList,IntervalList,St),
   !.

incr_list_norm([K|_],[Incr|_],[(Kmin,Kmax)|_],St):-
   retract(k_parameter(K,Kval)),
   KK is Kval * Incr,
   (
      (KK =< (Kmax*1.1))
   ->(g_read(landsp1,P1),
      ((P1==K)
      ->
       format(St,"~n",[])
      ;
       true
      ),
     assertz(k_parameter(K,KK))
     )
     ;
    assertz(k_parameter(K,Kmin)),
    g_inc(nbcurr),
%    g_read(nbcurr,Cu), %%affichage avancement du calcul ( a modifier)
%    g_read(nbtotal,Tot),
%    Perc is (Cu/Tot)*100,
 %   format("~0f ",[Perc]),flush_output,
      fail
   ).

robustness_log(ParamList,CvarList,Spec,VarList,ObjList,Nb,N):-
	g_assign(rob,log),
	robustness(ParamList,CvarList,Spec,VarList,ObjList,Nb,N),
	g_assign(rob,norm).	

robustness(ParamList,CvarList,Spec,VarList,ObjList,Nb,N):-
	g_read(debug,Debug),
	g_assign(debug,-1),
	g_read(rob,C),
	((C==log)
	->
	 set_init_log_rob(ParamList,CvarList,MeanVarList)
	 ;
	 set_init_rob(ParamList,CvarList,MeanVarList)
	 ),
	objectivelist(VarList,ObjList,Fval),
	get_set_init_values2(ParamList,InitValues,_),
	g_assign(timesimul,N),
	g_assign(fval,Fval),
	g_assign(formula,Spec),

   robustness_b(ParamList,MeanVarList,Spec,Fval,N,Nb,Rob),
   set_init_values(ParamList,InitValues),
   satisfaction_degree(Dinit),
   RSD is Rob/Dinit,
   (
      have_gui
   ->
      format("[GUI] robustness Satisfaction degree : ~g~n",[Dinit]),
      format("[GUI] robustness Robustness : ~g~n",[Rob]),
      format("[GUI] robustness Relative Robustness : ~g ~n",[RSD])
   ;
      format("Satisfaction degree : ~g ~nRobustness : ~g ~nRelative Robustness : ~g ~n",[Dinit,Rob,RSD])
   ),      
   g_assign(debug,Debug).





set_init_rob([],[],[]).

set_init_rob([P|Plist],[CVar|CList],[(M,V)|Mvlist]):-
	k_parameter(P,Pval),
	M is Pval,
	V is CVar*Pval,
	set_init_rob(Plist,CList,Mvlist).


set_init_log_rob([],[],[]).

set_init_log_rob([P|Plist],[CVar|Lsigma],[(M,V)|Mvlist]):-
	k_parameter(P,Pval),
	M is log(Pval)-0.5*log(1+CVar**2),
	V is sqrt(log(1+CVar**2)),
	set_init_log_rob(Plist,Lsigma,Mvlist).



robustness_b(ParamList,MeanVarList,Spec,Fval,N,Nb,Rob):-


	g_assign(timesimul,N),
	g_assign(fval,Fval),
	g_assign(formula,Spec),
	statistics(cpu_time,[T1,_]),

	g_assign(nbto,Nb),
	g_assign(k,1),
	
	Nbp is Nb+1,
	g_assign(dists,g_array(Nbp)),
	g_assign(means,g_array(Nbp)),	
	g_assign(sums,g_array(Nbp)),
	g_assign(sums(0),0),
	robustness_vd2_aux(ParamList,MeanVarList),
	!,

   g_read(means(Nb),Rob),
   statistics(cpu_time,[T2,_]),
   T is (T2-T1),
   TT is T/1000,
   format_debug(0,"Time elapsed : ~2f s ~n",[TT]),
   (
      have_gui
   ->
      format("[GUI] checkLTL Time elapsed : ~2f s ~n",[TT])
   ;
      true
   ).


robustness_vd2_aux(ParamList,MeanVarList):-
	g_read(rob,C),
	((C==log)
	->
	 set_param_log(ParamList,MeanVarList)	 
	 ;
	 set_param(ParamList,MeanVarList)	 
	 ),
	g_read(k,K),
	g_read(nbto,Nb),
	(K>Nb),
	!.

robustness_vd2_aux(_,_):-
	g_read(k,K),
	satisfaction_degree(VD),
	g_assign(dists(K),VD),
	Kb is K-1,
	g_read(sums(Kb),Sumb),
	Sum is Sumb+VD,
	g_assign(sums(K),Sum),
	Mean is Sum/K,
	g_assign(means(K),Mean),	
	g_inc(k),
	fail.

robustness_vd2_aux(ParamList,MeanVarList):-
	robustness_vd2_aux(ParamList,MeanVarList).



set_param_log([],[]).
set_param_log([P|Plist],[(M,E)|MVlist]):-
	random_gaussian(M,E,Z),
	ZZ is exp(Z),
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,ZZ)),
	set_param_log(Plist,MVlist).



set_param([],[]).
set_param([P|Plist],[(M,E)|MVlist]):-
	random_gaussian(M,E,Z),
	retract(k_parameter(P,_)),
	assertz(k_parameter(P,Z)),
	set_param(Plist,MVlist).


random_gaussian(M,E,ZZZ):-
	random(X),random(Y),
	Z is sqrt(-2*log(X))*sin(3.1415926535897931*2*Y),
	ZZ is M+Z*E,
	((ZZ<0)
	-> ZZZ is 1.0e-10
	; ZZZ is ZZ
	).

seed(N):-
	set_seed(N), %gprolog seed
	cmaes_seed(N). %cmaes seed
	


get_incr_list_log([],_MaxIter,[]).
get_incr_list_log([(Vmin,Vmax)|IntervalList],MaxIter,[Incr|IncrList]):-
   Incr is (Vmax / Vmin)**(1/MaxIter),
   get_incr_list_log(IntervalList,MaxIter,IncrList).



plot_land(Plot):-
   plot_load(Plot).

%%%%multiple conditions%
first_fss_search_condition(ParamList,IntervalList,Spec,VarList,ObjList,N):-
	check_valid_fss_params(ParamList,IntervalList,VarList,ObjList),
	g_assign(nbtrace,1),
	g_assign(replace,g_array(10)),
	g_assign(objectives,g_array(10)),
	g_assign(specs,g_array(10)),
	g_assign(varlists,g_array(10)),
	g_assign(params0,ParamList),
	g_assign(interval0,IntervalList),
	g_assign(spec0,Spec),
	g_assign(varlist0,VarList),
	g_assign(obj0,ObjList),
	g_assign(timesim0,N),
	g_assign(timesimul,N),	
	g_assign(paramlist,ParamList).

first_search_condition(ParamList,IntervalList,Spec,VarList,ObjList,N):-
	check_valid_cmaes_params(ParamList,IntervalList,VarList,ObjList),
	g_assign(nbtrace,1),
	g_assign(replace,g_array(10)),
	g_assign(objectives,g_array(10)),
	g_assign(specs,g_array(10)),
	g_assign(varlists,g_array(10)),
	g_assign(params0,ParamList),
	g_assign(interval0,IntervalList),
	g_assign(spec0,Spec),
	g_assign(varlist0,VarList),
	g_assign(obj0,ObjList),
	g_assign(timesim0,N),
	g_assign(timesimul,N),	
	g_assign(paramlist,ParamList).


add_search_condition(Spec,VarList,O,R):-
	check_variable_names(VarList),
	g_read(nbtrace,Nb),
	g_assign(replace(Nb),R),
	g_assign(objectives(Nb),O),
	g_assign(varlists(Nb),VarList),
	g_assign(specs(Nb),Spec),
	g_inc(nbtrace).





cmaes_multi_conditions:-
	statistics(cpu_time,[T1,_]),
	g_read(params0,ParamList),
	g_read(interval0,IntervalList),

	var_norm_init(1),	
	length(ParamList,Dim),
	g_assign(bounds,g_array(Dim)),
	cmaes_bounds(IntervalList,0),
	get_set_init_values2(ParamList,InitValues,_),
	set_init_cmaes(ParamList,Plist),
	g_assign(p_t_list,Plist),
	cmaes_read_params(Maxsimul,Stopfitness),
 	read_seed(Seed),


	disp_initial_multi(ParamList),
	read_std(Std),
        save_state,
 	call_main_cmaes(Dim,1,Maxsimul,Stopfitness,Seed,Std),%call cmaesmulti
	(
               have_gui
            ->
		format("[GUI] checkLTL Found Parameters :~n",[]),
	       	writeall_multiCond((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A))
            ;
               	format("Found Parameters :~n",[]),
	       	writeall((member(P,ParamList),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A))
        ),

	set_init_values(ParamList,InitValues),
	statistics(cpu_time,[T2,_]),
	T is (T2-T1),
	TT is T/1000,
	(
               have_gui
            ->
		format("[GUI] checkLTL Time elapsed ~2f s",[TT])
            ;
               	format("Time elapsed ~2f s",[TT])
        ).

%Anthony - fss
distance_fss(VPlist,D):-
	g_read(p_t_list,Plist),
	g_read(fss,F),
	g_read(interval0,Ilist),
	((F==log)
	->
	 set_param_log_fss(VPlist,Plist,Ilist)
	 ;
	 set_param_fss(VPlist,Plist,Ilist)
	),
	distance4(D).

%Anthony
distance_multi_fss(VPlist,D):-
	g_read(p_t_list,Plist),
	g_read(fss,F),
	g_read(interval0,Ilist),
	((F==log)
	->
	 set_param_log_fss(VPlist,Plist,Ilist)
	;
	 (
	  set_param_fss(VPlist,Plist,Ilist))
	),
	distance_multiFSS(D).

distance_multiFSS(D):-
	%1ere dist
	g_read(obj0,Obj0),
	g_read(spec0,Spec0),
	g_read(timesim0,N),
	g_read(varlist0,VarList),

	objectivelist(VarList,Obj0,Fval),
	%list_parameters,
	numerical_simulation(N),
	trace_analyze6(Spec0),
	g_read(tdom(1),L),
	f_normalize(Spec0,S0b),
	memberchk((S0b,Dom),L),
	empty_domains,!,
	dist_ppl(Fval,Dom,D0),
	clean_ppl,
	g_assign(done,1),
	g_assign(sumdist,0),
	multi_aux,
	g_read(sumdist,D0b),
	D is D0+D0b.


distance_cmaes_multi(VPlist,D):-
   \+ \+ (
	g_read(p_t_list,Plist),
	g_read(cmaes,C),
	g_read(interval0,Ilist),
        length(VPlist, VPlist_length),
        length(Plist, Plist_length),
        (
           VPlist_length = Plist_length
        ->
           true
        ;
           throw(dimension_mismatch(values(VPlist_length), parameters(Plist_length)))
        ),
	((C==log)
	->
	 set_param_log_cmaes(VPlist,Plist,Ilist)
	;
	 (
	  set_param_cmaes(VPlist,Plist,Ilist))
	),
	distance_multi(D),
        g_assign(distance_result, D)
         ),
   g_read(distance_result, D).


distance_multi(D):-
	%1ere dist
	g_read(obj0,Obj0),
	g_read(spec0,Spec0),
	g_read(timesim0,N),
	g_read(varlist0,VarList),
	objectivelist(VarList,Obj0,Fval),
	%list_parameters,
	numerical_simulation(N),
	trace_analyze6(Spec0),
	g_read(tdom(1),L),
	f_normalize(Spec0,S0b),
	memberchk((S0b,Dom),L),
	empty_domains,!,
	dist_ppl(Fval,Dom,D0),
	clean_ppl,
	g_assign(done,1),
	g_assign(sumdist,0),
	multi_aux,
	g_read(sumdist,D0b),
	D is D0+D0b.


multi_aux:-
	g_read(done,DDone),
	g_read(nbtrace,Nb),
	%format("nb : ~w done : ~w  ~n",[Nb,DDone]),
	(DDone=:=Nb),!.
	

multi_aux:-
	g_read(done,DD),
	g_read(objectives(DD),Obji),
	g_read(replace(DD),Lr),

	g_read(specs(DD),Speci),
	g_read(varlists(DD),Varlisti),
	makelist(Lr,Lrr),
	get_set_init_values2(Lrr,InitValues,_),
	replace_params(Lr),

	g_read(timesim0,N),
	objectivelist(Varlisti,Obji,Fval),
        print('Simulation'), nl,
	numerical_simulation(N),
        print('Analyze'), nl,
	trace_analyze6(Speci),
	g_read(tdom(1),L),
	f_normalize(Speci,S0b),
	memberchk((S0b,Dom),L),
	empty_domains,
	dist_ppl(Fval,Dom,Di),
	clean_ppl,
	g_read(sumdist,Sum),
	Sump is Sum + Di,
	g_assign(sumdist,Sump),
	g_inc(done),
	set_init_values(Lrr,InitValues),
	fail.

multi_aux:-
	multi_aux.

replace_params([]).
replace_params([(P1,P2)|L]):-
	k_parameter(P1,P1val),
	retract(k_parameter(P2,_)),
	assertz(k_parameter(P2,P1val)),
	replace_params(L).

makelist([],[]).
makelist([(P1,P2)|L],[P1,P2|L2]):-

	makelist(L,L2).



disp_initial_multi(Plist):-
	distance_multi(D),
	(
               have_gui
            ->
		format("[GUI] checkLTL Initial Parameters :~n",[]),
	       	writeall_multiCond((member(P,Plist),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A)),
		format("[GUI] checkLTL Initial Cost : ~4f ~n",[D]),
		format("[GUI] checkLTL In progress..... Please wait........................~n",[])
            ;
               	format("Initial Parameters :~n",[]),
		writeall((member(P,Plist),k_parameter(P,V),format_to_atom(A,"~9g",[V])),parameter(P,A)),
		format("~nInitial Cost : ~4f ~n",[D])
        ),nl.


check_valid_fss_params(ParamList,IntervalList,VarList,ObjList):-
	length(ParamList,Np),
	length(IntervalList,Ni),
	length(VarList,Nv),
	length(ObjList,No),

	((Np=Ni)
	->
	 true
	;
 		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL There should be equal number of search intervals and searched parameters~n",[])
   			  ;
      				format("There should be equal number of search intervals and searched parameters~n",[])
   		),
	 	fail
	 ),

	((Nv=No)
	->
	 true
	;
		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL There should be equal number of objective values and variables in the formula~n",[])
   			  ;
      				format("There should be equal number of objective values and variables in the formula~n",[])
   		),
          	fail
	 ),

	check_nonzero(ParamList),
	check_intervals(ParamList, IntervalList),
	check_variable_names(VarList).


check_valid_cmaes_params(ParamList,IntervalList,VarList,ObjList):-
	length(ParamList,Np),
	length(IntervalList,Ni),
	length(VarList,Nv),
	length(ObjList,No),

	((Np=Ni)
	->
	 true
	;
 		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL There should be equal number of search intervals and searched parameters~n",[])
   			  ;
      				format("There should be equal number of search intervals and searched parameters~n",[])
   		),
	 	fail
	 ),

	((Nv=No)
	->
	 true
	;
		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL There should be equal number of objective values and variables in the formula~n",[])
   			  ;
      				format("There should be equal number of objective values and variables in the formula~n",[])
   		),
          	fail
	 ),

	check_nonzero(ParamList),
	check_intervals(ParamList, IntervalList),
	check_variable_names(VarList).


check_variable_names([]).
check_variable_names([H|T]):-
	((k_parameter(H,_))
	->
		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL ~w is already a parameter name~n",[H])
   			  ;
      				format("~w is already a parameter name~n",[H])
   		),	 	
	 	fail
	;
	 true
	),
	((k_macro(H,_))
	->
		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL ~w is already a macro name~n",[H])
   			  ;
      				format("~w is already a macro name~n",[H])
   		),fail
	;
	 true
	),
	check_variable_names(T).

check_nonzero([]).
check_nonzero([H|T]):-
	((k_parameter(H,HV))
	->
	 true
	;
	 (
	  have_gui
	 ->
	  format("[GUI] ~w is not a parameter ~n",[H]),fail
	 ;
	  format("~w is not a parameter ~n",[H]),fail
	 )
	),
	
	(
	 (HV=:=0)
	->
	 retract(k_parameter(H,HV)),
	 assertz(k_parameter(H,0.1))
	 ;
	 true
	 ),
	check_nonzero(T).



check_mol(M):-
	(
	 (molecule(M,_))
	->
	 true
	;
	 (
	  have_gui
	 ->
	  format("[GUI] checkLTL Unknown molecule ~w ~n",[M]),fail
	 ;
	  format(" Unknown molecule ~w ~n",[M]),fail
	 ),
	 fail
	).
	

check_intervals([],[]).

check_intervals([P|PT],[(Min,Max)|T]):-
	k_parameter(P,PV),
	(((PV < Min);(PV>Max))
	->
		(
    		  have_gui
  			 ->
      				format("[GUI] checkLTL Parameter ~w initial value should be in its search interval [~g,~g]~n",[P,Min,Max])
   			  ;
      				format("Parameter ~w initial value should be in its search interval [~g,~g]~n",[P,Min,Max])
   		),fail	
	 ;
	 true
	 ),
	check_intervals(PT,T).

%%%%%%sensitivity analysis%%%%%%%



%analyses sensibilité
gsa_morris(ParamList,IntervalList,Spec,VarList,ObjList,N,Levels,Paths):-
%initi
	statistics(cpu_time,[T1,_]),
	g_read(debug,Debug),
	g_assign(debug,-1),

%affectations	
	g_assign(paramlist,ParamList),
	g_assign(timesimul,N),
	g_assign(interval0,IntervalList),
%calculs	
	length(ParamList,Dim),

	get_set_init_values2(ParamList,InitValues,_),
	objectivelist(VarList,ObjList,V_OList),
	g_assign(fval,V_OList),
	
	set_init_cmaes(ParamList,Plist),
 	g_assign(formula,Spec),
 	g_assign(p_t_list,Plist),
	

 	%disp_initial(ParamList,N),

%call_morris C function
	
	call_morris(Dim,Levels,Paths,1.0),

 	set_init_values(ParamList,InitValues),
 	g_assign(debug,Debug),

	clean_obj(1),

 	statistics(cpu_time,[T2,_]),
 	T is (T2-T1),
 	TT is T/1000,
 	format("Time elapsed ~2f s",[TT]).

morris_effects(Vlist):-
	g_read(paramlist,Plist),
	morris_effects_merge(Vlist,Plist,VPlist),
	sort(VPlist,Vsorted),
	disp_effects(Vsorted),
	write(done),nl.

disp_effects([]).
disp_effects([(V,P)|M]):-
	format("Parameter ~w  mstar ~5f ~n",[P,V]),
	disp_effects(M).

morris_effects_merge([],[],[]).

morris_effects_merge([Hv|Tv],[Hp|Tp],[(Hv,Hp)|M]):-
	morris_effects_merge(Tv,Tp,M).



%analyses sensibilité
sa_local(ParamList,Spec,VarList,ObjList,N,Delta):-
%initi
	statistics(cpu_time,[T1,_]),
	g_read(debug,Debug),
	g_assign(debug,-1),

%affectations	
	g_assign(paramlist,ParamList),
	g_assign(timesimul,N),

%calculs
	%get_set_init_values2(ParamList,InitValues,_),
	objectivelist(VarList,ObjList,V_OList),
	g_assign(fval,V_OList),
	
 	g_assign(formula,Spec),
 
%call_sa local function
	%Delta is 0.05,
	sa_local(ParamList,SA_list,Delta),
 	%set_init_values(ParamList,InitValues),
 	g_assign(debug,Debug),

	sort(SA_list),
	format("Local sensitivities (delta violation degree / delta k): ~n",[]),
	disp_sa(SA_list),
	clean_obj(1),

 	statistics(cpu_time,[T2,_]),
 	T is (T2-T1),
 	TT is T/1000,
 	format("Time elapsed ~2f s",[TT]).

sa_local([],[],_).

sa_local([P|Plist],[(SA_P,P)|SAlist],Delta):-
	k_parameter(P,Pval),
	distance4(D),
	Pvalp is Pval*(1+Delta),

	retract(k_parameter(P,_)),
	assertz(k_parameter(P,Pvalp)),

	distance4(Dp),

	retract(k_parameter(P,_)),
	assertz(k_parameter(P,Pval)),
	SA_P is (Dp -D)/Delta,
	sa_local(Plist,SAlist,Delta).

disp_sa([]).

disp_sa([(S,P)|R]):-
	format("~w : ~g ~n",[P,S]),
	disp_sa(R).


distance_morris(VPlist,D):-
	%write(d1),nl,
	g_read(p_t_list,Plist),
	g_read(interval0,Ilist),
	%write(d2),nl,
	%%format("Plist ~w Ilist ~w VPList ~w ~n",[Plist,Ilist,VPlist]),
	set_param_morris(VPlist,Plist,Ilist),
	%write(d3),nl,
	%write(dist4call),nl,
	distance4(D).
	%%format("dist ~w ~n",[D]).



set_param_morris([],[],[]).
set_param_morris([V|Vlist],[(P,_)|Plist],[(Min,Max)|Ilist]):-

	V2 is Min+(Max-Min)*V,

	retract(k_parameter(P,_)),
	%format("parameter(~w,~w). ~n",[P,V2]),
	assertz(k_parameter(P,V2)),

	set_param_morris(Vlist,Plist,Ilist).


%%%%%% generate initial conditions
% liste params
%intervalles
%nb cond initiales

%write one initial condition number Id with values Vlist
write_one_init_cond(Vlist,Id):-
%g_read..
	IId is Id +7200,
	g_read(plist_g_init,ParamList),
	g_read(ilist_g_init,IntervalList),
	%create file
	format_to_atom(F,"init_job~w.bc",[IId]),
	open(F,write,Stream),
	format(Stream,"set_job_id(~w).~n",[IId]),
	%format("Plist ~w  ~n IList ~w  ~n Vlist ~w  ~n",[ParamList,IntervalList,Vlist]),
	write_params(ParamList,IntervalList,Vlist,Stream),
	close(Stream).
%write_params.


write_params([],[],[],_).
write_params([P|Plist],[(Min,Max)|Ilist],[V|Vlist],S):-
	%%X is Min +(Max-Min)*V,
	XX is Max/Min,
	X is Min*(XX**(V)),
	%%X is 10**(V*3 -2),
	format(S,"parameter(~w,~g).~n",[P,X]),
	write_params(Plist,Ilist,Vlist,S).

	
