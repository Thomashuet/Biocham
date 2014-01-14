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
% GNU prolog file importSbml.pl by Sylvain Soliman

%%% Importing

% :- include(parseXml).

:- dynamic(mathml_function/3).   % mathml functions

% forget (rules, etc.) then import

load_sbml(F):-
	clear_biocham,
	add_sbml(F),!. 

% import SBML file

add_sbml(F):-
   format_debug(9,"Adding XML file ~w~n",[F]),
   statistics(user_time,_),
   write_to_atom(FF,F),
   decompose_file_name(FF, Dir, FFF, Ext),
   (
      (Dir = '')
   ->
      CurDir=Dir
   ;  
                                % g_assign('current_dir', Dir)
      working_directory(OldDir),
      (  change_directory(Dir); change_directory(OldDir) ),
      atom_concat(Dir,'/',CurDir)
   ),
                                %   g_read('current_dir', CurDir),
   (
      Ext = ''
   ->
      EExt = '.xml'
   ;
      EExt = Ext
   ),
   atom_concat(CurDir, FFF, GG),
   atom_concat(GG, EExt, G),
   format_debug(9,"Parsing XML file ~w~n",[G]),
   xml_parse_file(G,T),!,
   format_debug(9,"Parsed XML file:~n~w~n",[T]),
   sbml_to_bc(T),!,
   statistics(user_time,[_,Time]),
   format_debug(3,"Time: ~w~n",[Time]),
	assertset(mols_added),
	assertset(rules_added).

% Parse top of tree, then try to parse model

sbml_to_bc(xml(_, SBML)):-
   member(sbml(O, LM), SBML),
   member((level,L),O),
   member((version,V),O),
   format_debug(6, "sbml level ~w, version ~w~n", [L, V]),
   member(model(_,M), LM),!,
   format_debug(6, "sbml level ~w, version ~w~n", [L, V]),
   number_atom(NL,L),
   number_atom(NV,V),
   (
      ((NL > 2);(NL+NV > 3))
   ->
      format_to_atom(
         Atom,
         "Warning: support for SBML level ~w version ~w is experimental",
         [L, V]
      ),
      write_error(Atom)
   ;
      true
   ),
   g_assign(sbml_current_reac, ''),
   model_to_bc(M).

%% Parse model

% model_to_bc([H | _]) :-
%    format_debug(8, "model_to_bc(~w)~n", [H]),
%    fail.

% if no element left, we're done

model_to_bc([]).

% try to support functions

model_to_bc([listOfFunctionDefinitions(_, LF)|L]):-
   !,
   retractall(mathml_function(_, _, _)),
   format_debug(5,"SBML functions ~w~n",[LF]),
   get_func_list(LF),
   format_debug(5,"done~n",[]),
   model_to_bc(L).

% if <listOfCompartments> found -> add them as parameters

model_to_bc([listOfCompartments(_,LC)|L]):-
   !,
   format_debug(5,"SBML compart ~w~n",[LC]),
   get_compart_list(LC),
   format_debug(5,"done~n",[]),
   model_to_bc(L).

% if <listOfSpecies> add them to initial state

model_to_bc([listOfSpecies(_,LS)|L]):-
   !,
   format_debug(5,"SBML species ~w~n",[LS]),
   species_to_bc(LS),
   format_debug(5,"done~n",[]),
   model_to_bc(L).

% if <listOfParameters> found -> add them

model_to_bc([listOfParameters(_,LP)|L]):-
   !,
   format_debug(5,"SBML param ~w~n",[LP]),
   get_param_list(LP),
   format_debug(5,"done~n",[]),
   model_to_bc(L).

% Support for some rules (currently only assignments -> macros)

model_to_bc([listOfRules(_,LR)|L]):-
   !,
   format_debug(5,"SBML rules ~w~n",[LR]),
   get_rule_list(LR),
   format_debug(5,"done~n",[]),
   model_to_bc(L).

% if <listOfReactions> add them to rules

model_to_bc([listOfReactions(_,LR)|L]):-
   !,
   format_debug(5,"SBML reactions ~w~n",[LR]),
   reactions_to_bc(LR),
   format_debug(5,"done~n",[]),
   g_assign(sbml_current_reac, ''),
   model_to_bc(L).

% if <listOfReactions> add them to rules

model_to_bc([listOfEvents(_,LE)|L]):-
   !,
   format_debug(5,"SBML events ~w~n",[LE]),
   events_to_bc(LE),
   format_debug(5,"done~n",[]),
   model_to_bc(L).

% else, throw away

model_to_bc([T|L]):-
   format_debug(5,"SBML throwing ~w~n",[T]),
   model_to_bc(L).

%% Add species to initial state

% none left -> we're done

species_to_bc([]).

% <specie> or <species> found, add it as absent if initial amount is 0
% present otherwise

species_to_bc([H|T]):-
   (
      (H = specie(S,_))
   ;
      (H = species(S,_))
   ),
   (
      member((id,N),S),!
   ;
      member((name,N),S)
   ),
   member((compartment, Loc), S),
   (
      (
         member((initialAmount,C),S),!,
         number_sbml_atom(CC1,C),
         k_volume(Loc, Vol),
         format_debug(1, "~w~n", [species(S, CC1, Vol)]),
         CC is CC1/Vol
      ;
         member((initialConcentration,C),S),
         number_sbml_atom(CC,C)
      )
   ->
      true
   ;
      CC = 0
   ),
   !,
   (
      (
         % concentration never changes
         member((constant, true), S)
      ;
         % concentration can change (e.g. events) but not in ODEs
         member((boundaryCondition, true), S)
      )
   ->
      parameter(N, CC)
   ;
      present(N::Loc,CC)
   ),
   species_to_bc(T).


pretty_sum(A, B, C) :-
   A = 0 -> C = B;
   B = 0 -> C = A;
   C = A + B.


pretty_product(A, B, C) :-
   A = 0 -> C = 0;
   B = 0 -> C = 0;
   C = A * B.


pretty_division(A, B, C) :-
   A = 0 -> C = 0;
   C = A / B.


cross_product((AL, AR), (BL, BR), (VL, VR)) :- %VL = AL BL + AR BR, VR = AL BR + AR BL
   pretty_product(AL, BL, ALBL),
   pretty_product(AL, BR, ALBR),
   pretty_product(AR, BL, ARBL),
   pretty_product(AR, BR, ARBR),
   pretty_sum(ALBL, ARBR, VL),
   pretty_sum(ALBR, ARBL, VR).


prettify_reversible(V, VV) :-
   nonvar(V), !,
   (
      V = V1 - V2
   ->
      VV = (V1, V2)
   ;
      V = V1 + (- V2)
   ->
      prettify_reversible(V1 - V2, VV)  
   ;   
      V = A * B
   ->
      prettify_reversible(A, (AL, AR)),
      prettify_reversible(B, (BL, BR)),
      %if both right sides have been developped to a nonzero value,
      %the cross-product WILL be nonsensical
      % NOOOOOOOoooooooooooo -> (a-b)*(c-d) is "sensical", and failing is very
      % wrong here!!!
      cross_product((AL, AR), (BL, BR), VV)
   ;
      V = A / B
   ->
      prettify_reversible(A, (AL, AR)),
      pretty_division(AL, B, VVL),
      pretty_division(AR, B, VVR),
      VV = (VVL, VVR)
   ;
      VV = (V, 0)
   ).

prettify_reversible(V, (V, 0)).


%% Add reactions to rules

% none left -> done

reactions_to_bc([]).

% <reaction> found
reactions_to_bc([reaction(O,R)|L]):-
   (
      member((id,N),O),!  % at least one name or id is compulsory
   ;
      member((name,N),O)
   ),
   format_debug(8,"treating reaction: ~w~n",[O]),
   g_assign(sbml_current_reac, N),
   get_reac(R,LL,LR,V),    % get reactants and products
   format_debug(9,"with reactants: ~w and products:~w and kinetics:~w~n",
      [LL,LR,V]),
   (
      % default is true!!
      \+(member((reversible,'false'),O))
   ->
      (Type = (<=>)),
      prettify_reversible(V, VV)      
   ;
      (Type = (=>)),
      VV=V
   ),
   !,
   (
      var(V)
   ->
      (Reac = :(N,RR))
   ;
      (Reac = :(N,VV for RR))
   ),
   % FIXME if a reaction with same reactants and products exists
   % add kinetics instead of error
   soup_to_solution(LL,AL),
   soup_to_solution(LR,AR),
   RR =.. [Type,AL,AR],
   format_debug(5,"adding rule: ~w~n",[Reac]),
   add_rules(Reac),
   reactions_to_bc(L).

%% return list of reactants and products from XML

% nothing left -> done

get_reac([],[],[],_).

% <listOfReactants> found -> add to left part of rule

get_reac([listOfReactants(_,R)|L],LL,LR,V):-
   !,
   get_list(R,L1),
   get_reac(L,L2,LR,V),
   append(L1,L2,LL).

% <listOfProducts> found -> add to right part of rule

get_reac([listOfProducts(_,R)|L],LL,LR,V):-
   !,
   get_list(R,L1),
   get_reac(L,LL,L2,V),
   append(L1,L2,LR).

% <listOfModifiers> found -> add to both parts of rule

get_reac([listOfModifiers(_,R)|L],LL,LR,V):-
   !,
   get_list(R,L1),
   get_reac(L,L2,L3,V),
   append(L1,L2,LL),
   append(L1,L3,LR).


get_reac([kineticLaw(KL,C)|L],LL,LR,V):-
   !,
   (
      member(listOfParameters(_,LP),C)
   ->
      get_param_list(LP)
   ;
      true
   ),
   (
      member((formula,V1),KL)
   ->
      format_debug(7, "kinetics from formula ~w~n", [V1]),
      get_value_from_formula(V1,V)
   ;
      (
         member(math(_,[M]),C)
      ->
         format_debug(7, "kinetics from mathml ~w~n", [M]),
         eval_mathml(M,V)
      ;
         V=0
      )
   ),
   get_reac(L,LL,LR,V).

   
% else ignore

get_reac([_|L],LL,LR,V):-
   get_reac(L,LL,LR,V).

%% Add events

% none left -> done

events_to_bc([]).

% <event> found
events_to_bc([event(_, EE)|L]) :-
   member(trigger(_, TT), EE),
   member(math(_, [T]), TT),
   member(listOfEventAssignments(_, LEA), EE),!,
   eval_mathml(T, Cond),
   get_event_assignments(LEA, LN, LK),
   format_debug(5,"adding event: ~w -> ~w ~w~n",[Cond, LN, LK]),
   add_event(Cond, LN, LK),
   events_to_bc(L).

get_event_assignments([], [], []).

get_event_assignments([eventAssignment([(variable, V)], MM)|LEA],
   [V|LV], [K|LK]) :-
   member(math(_, [M]), MM),!,
   eval_mathml(M, K),
   get_event_assignments(LEA, LV, LK).

%%% Evaluate a MathML formula

eval_mathml(apply([],[times([],[])|L]),E) :-
   !,
   eval_mathml_rec(L,E,'*').

eval_mathml(apply([],[divide([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'/').

eval_mathml(apply([],[plus([],[])|L]),E) :-
   !,
   eval_mathml_rec(L,E,'+').

eval_mathml(apply([], [minus([], []), L]), -E) :-
   !,
   eval_mathml(L, E).

eval_mathml(apply([],[minus([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'-').

eval_mathml(apply([],[power([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'^').

eval_mathml(apply([], [ln([], []), L]), log(E)) :-
   !,
   eval_mathml(L, E).

eval_mathml(apply([], [exp([], []), L]), exp(E)) :-
   !,
   eval_mathml(L, E).

eval_mathml(apply([], [root([], []), degree([], [L1]), L2]), '^'(E2, 1/E1)) :-
   !,
   eval_mathml(L1, E1),
   eval_mathml(L2, E2).

% conditions

eval_mathml(apply([],[lt([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'<').

eval_mathml(apply([],[gt([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'>').

eval_mathml(apply([],[leq([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'=<').

eval_mathml(apply([],[geq([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'>=').

eval_mathml(apply([],[eq([],[]),L1,L2]),E) :-
   !,
   eval_mathml_rec([L1,L2],E,'=').

% time

eval_mathml(csymbol([(encoding,text),('definitionURL',
      'http://www.sbml.org/sbml/symbols/time')],
   ['$text'([(value,time)],[])]), 'Time').

% functions
eval_mathml(apply([], [ci([], ['$text'([(value, F)], [])]) | Args]), E) :-
   format_debug(9, "evaluating func ~w on args ~w~n", [F, Args]),
   mathml_function(F, Args, FF),
   format_debug(9, "evaluating func ~w on args ~w -> ~w~n", [F, Args, FF]),
   eval_mathml(FF, E), !.

eval_mathml(ci([],['$text'([(value,V)],[])]),R):-
   !,
   % text_normalize(V,V1),
   get_value(V,R).
   
eval_mathml(cn(_, ['$text'([(value,V)],[])]),R):-
   !,
   % text_normalize(V,V1),
   number_sbml_atom(R, V).
   
% handle exp notation
eval_mathml(
   cn(
      [(type, 'e-notation')],
      [
         '$text'([(value, V1)], []),
         sep([], []),
         '$text'([(value, V2)], [])
      ]),
   R) :-
   atom_concat(V1, 'e', V3),
   atom_concat(V3, V2, V4),
   number_sbml_atom(R, V4).


eval_mathml(E, 0) :-
   format_debug(8, "unknown mathml: ~w~n", [E]).

   
eval_mathml_rec([A,B],E,Op):-
   eval_mathml(A,E1),
   eval_mathml(B,E2),
   E=..[Op,E1,E2].

eval_mathml_rec([A,B,L1|L],E,Op):-
   eval_mathml(A,E1),
   eval_mathml_rec([B,L1|L],E2,Op),
   E=..[Op,E1,E2].

get_value(V,R):-
   number_sbml_atom(R, V), !.

% local parameter
get_value(V,VV):-
   atom(V),
   g_read(sbml_current_reac, R),
   atom_concat(R, V, VV),
   k_parameter(VV, _),
   !.

get_value(V,V):-
   k_parameter(V,_),!.

get_value(V,V):-
   k_macro(V,_),!.

get_value(V,[V::Loc]):-
   initconc(V::Loc, _), !.
   
get_value(V,V):-
   format_debug(8, "unknown mathml value: ~w~n", [V]).

text_normalize(V,VV):-
   remove_spaces_before(V,V1),
   remove_spaces_after(V1,VV).

remove_spaces_before(V,VV):-
   atom_concat(' ',V1,V),!,
   remove_spaces_before(V1,VV).

remove_spaces_before(V,V).

remove_spaces_after(V,VV):-
   atom_concat(V1,' ',V),!,
   remove_spaces_after(V1,VV).

remove_spaces_after(V,V).

% get a compartment list and add them as parameters

get_compart_list([]).

get_compart_list([compartment(P,_)|LP]):-
   (
      member((id,Id),P),!
   ;
      member((name,Id),P)
   ),
   format_debug(5,"compart with name ~w~n",[Id]),
   (
      member((size,Val),P)
   ->
      true
   ;
      Val='1.0'
   ),
   format_debug(5,"compart with size ~w~n",[Val]),
   number_sbml_atom(Value,Val),
   format_debug(5,"compart with size3 ~w~n",[Value]),
   parameter(Id,Value),
   volume(Id, Value),
   get_compart_list(LP).

number_sbml_atom(N, A) :-
   (
      sub_atom(A, _, 1, After, '.')
   -> % contains a dot
      (
         After == 0
      -> % ends with dot, add final 0
         atom_concat(A, '0', Val)
      ;  % else do nothing
         Val = A
      )
   ;  % no dot
      (
         (sub_atom(A, Bef, 1, Aft, 'e'); sub_atom(A, Bef, 1, Aft, 'E'))
      -> % exp notation
         sub_atom(A, 0, Bef, _, Val0),
         atom_concat(Val0, '.0e', Val1),
         BBef is Bef + 1,
         sub_atom(A, BBef, _, 0, Val2),
         atom_concat(Val1, Val2, Val)
      ;
         atom_concat(A, '.0', Val)
      )
   ),
   catch(number_atom(N, Val),error(_,number_atom/2),fail),!.

% get a list of mathml functions

get_func_list([]).

get_func_list([functionDefinition(FA, FD)|LF]):-
   member((id, Id), FA),
   member(math(_, [lambda([], F)]), FD),!,
   format_debug(5, "function with id ~w~n",[Id]),
   get_func_args_body(F, Args, Body, []),
   format_debug(5, "function with args ~w~n",[Args]),
   format_debug(5, "function with body ~w~n",[Body]),
   assertz(mathml_function(Id, Args, Body)),
   get_func_list(LF).

get_func_args_body([bvar([],[V])|F], [NewVar | VL], B, L):-
   !,
   get_func_args_body(F, VL, B, [(V, NewVar) | L]).

get_func_args_body([B], [], BB, L):-
   replace(L, B, BB).

% get a MathML parameter list

get_param_list([]).

get_param_list([parameter(P,_)|LP]):-
   (
      member((id,Id),P)
   ;
      member((name,Id),P)
   ),
   g_read(sbml_current_reac, R),
   atom_concat(R, Id, Idd),
   (
      member((value,Val),P)
   ->
      number_sbml_atom(Value,Val),
      parameter(Idd,Value)
   ;
      (
         member((constant,false), P)
      ->
         true  % nothing to do -> assignment will take care
      ;
         parameter(Idd, '0.0') % Error ?
      )
   ),
   get_param_list(LP).

% in a formula

get_value_from_formula(V,V2):-
   atom_concat(V,'.',V1),
   read_term_from_atom(V1,T,[variable_names(L)]),
   name_vars(L),
   get_value_rec(T,V2).

get_value_rec(T,V):-
   atom(T),!,
   get_value(T,V).

get_value_rec(T,V):-
   number(T),!,
   number_atom(T,TT),
   get_value(TT,V).

get_value_rec(T,V):-
   T=..[F|Args],
   get_value_list(Args,VArgs),
   V=..[F|VArgs].

get_value_list([],[]).

get_value_list([H|T],[VH|VT]):-
   get_value_rec(H,VH),
   get_value_list(T,VT).

%% Get a list of species as a list of molecules

get_list([],[]).

get_list([SR|SL], ML) :-
   (
      (SR=specieReference(S,_))
   ;
      (SR=speciesReference(S,_))
   ;
      (SR=modifierSpeciesReference(S,_))
   ),!,
   get_list(SL,L1),
   (
      member((specie,SN1),S)   % Level 1 Version 1
   ;
      member((species,SN1),S)  % Level 1 Version 2
   ),!,
   (
      initconc(SN1::Loc, _)   % real mol
   ->
      (
         member((stoichiometry,N),S)
      ->
         number_atom(NN,N),
         (SN=(NN*(SN1::Loc)))
      ;
         (SN=SN1::Loc)
      ),
      ML=[SN|L1]
   ;
      format_debug(8, "throwin away fake species: ~w~n", [SN1]),
      ML=L1
   ).
   
soup_to_solution([],'_').

soup_to_solution([A],A).

soup_to_solution([A,B|L],+(S,A)):-
   soup_to_solution([B|L],S).

% get a rule list

get_rule_list([]).

get_rule_list([assignmentRule(RV, RM)|LR]):-
   !,
   member((variable, V), RV),
   member(math(_, [M]), RM),
   format_debug(7, "found assignment of ~w by ~w~n", [V, M]),
   eval_mathml(M, B),
   macro(V, B),
   format_debug(8, "added macro(~w, ~w)~n", [V, B]),
   get_rule_list(LR).

get_rule_list([R | L]) :-
   format_debug(5,"SBML throwing ~w~n",[R]),
   get_rule_list(L).

%%% Exporting

%%% writes the rules into an SBML file F.

% write header

export_sbml(F):-
   \+(atom(F)),!,
  
        (
                have_gui
        ->
                format("[GUI] errors Export SBML: Filename must be an atom, or be enclosed in simple quotes.~n",[])
        ;
   write_line_col('Error'),
   write('Filename must be an atom, or be enclosed in simple quotes\n')
        ).

export_sbml(F):-
	(sub_atom(F,_,1,_,'.') -> (G=F) ; atom_concat(F,'.xml',G)),
        open(G, write, _, [alias(sbml)]),
   write(sbml,'<?xml version="1.0" encoding="UTF-8"?>\n'),
   write(sbml,'<sbml xmlns="http://www.sbml.org/sbml/level2" level="2" version="1">\n'),
   write(sbml,'   <model id="Model_generated_by_BIOCHAM">\n'),
   sbml_functions, % added by Elisabetta
   write(sbml,'      <listOfCompartments>\n'),
   write(sbml,'         <compartment id="compartmentOne" size="1"/>\n'),
   write(sbml,'      </listOfCompartments>\n'),
   sbml_initial_state,
   sbml_parameters,
   sbml_macros, % added by Elisabetta
   sbml_rules,
   sbml_events, % added by Elisabetta
   write(sbml,'   </model>\n</sbml>\n'),
	close(sbml).

% Added by Elisabetta
sbml_functions:-
   (
      (
      k_macro(_,sq_wave(_,_,_,_));
      k_macro(_,sq_wave(_,_,_,_,_,_))
      )
   ->
      (
      write(sbml,'      <listOfFunctionDefinitions>\n'),
      write_function_frac, 
      write(sbml,'      </listOfFunctionDefinitions>\n')
      )
   ;
       true
   ).
 
% Added by Elisabetta. Such a function computes the fractional part of a given real number. It is needed to encode square_waves
write_function_frac :-
  write(sbml,'         <functionDefinition id="frac">\n'),
  write(sbml,'            <math xmlns="http://www.w3.org/1998/Math/MathML">\n'),
  write(sbml,'               <lambda>\n'),
  write(sbml,'                  <bvar><ci> x </ci></bvar>\n'),
  write(sbml,'                  <apply>\n'),
  write(sbml,'                     <minus/>\n'),
  write(sbml,'                     <ci> x </ci>\n'),
  write(sbml,'                     <apply>\n'),
  write(sbml,'                        <floor/>\n'),
  write(sbml,'                        <ci> x </ci>\n'),
  write(sbml,'                     </apply>\n'),
  write(sbml,'                  </apply>\n'),
  write(sbml,'               </lambda>\n'),
  write(sbml,'            </math>\n'),
  write(sbml,'         </functionDefinition>\n').

% write listOfSpecies

sbml_initial_state :-
   write(sbml,'      <listOfSpecies>\n'),
   nusmv_search_molecules,
   findall(M,molecule(M,_),LM),     % get all the molecules
   sbml_write_molecules(LM),
   write(sbml,'      </listOfSpecies>\n').

% write molecules, all in compartmentOne

sbml_write_molecules([]).

sbml_write_molecules([H|T]) :-
   sbml_convert_molecule_name(H,HH),
   write(sbml,'         <species\n           id="'),
   write(sbml,HH),
   write(sbml,'"\n           name="'),
   write(sbml,H),
   write(sbml,'"\n           compartment="compartmentOne"'),
   (
      initconc(H,C)
   -> 
      (
         number(C)
      ->
         V=C
      ;
         k_parameter(C,V)
      ),
      format(sbml,"~n           initialConcentration=\"~w\"",[V])
   ;
      true
   ),
   write(sbml,' />\n'),
   sbml_write_molecules(T).

sbml_convert_molecule_name(A-B,C):-
   !,
   sbml_convert_molecule_name(A,AA),
   sbml_convert_molecule_name(B,BB),
   atom_concat(AA,BB,C).

sbml_convert_molecule_name(A~{B},C):-
   !,
   sbml_convert_molecule_name(A,AA),
   sbml_convert_molecule_name(B,BB),
   atom_concat(AA,BB,C).

sbml_convert_molecule_name((A,B),C):-
   !,
   sbml_convert_molecule_name(A,AA),
   sbml_convert_molecule_name(B,BB),
   atom_concat(AA,BB,C).

sbml_convert_molecule_name(A::B,C):-
   !,
   sbml_convert_molecule_name(A,AA),
   sbml_convert_molecule_name(B,BB),
   atom_concat(AA,BB,C).

sbml_convert_molecule_name(A,A).

% write listOfParameters

% Modified by Elisabetta
sbml_parameters :-
   write(sbml,'      <listOfParameters>\n'),
   findall((P,V),k_parameter(P,V),LP), % get all param, value pairs
   sbml_write_params(LP),
   (
      k_macro(_,_)
    ->
      ( 
      findall(M,k_macro(M,_),LM), % get all macro names
      sbml_write_params_macros(LM)
      )
   ;
      true
   ),
   write(sbml,'      </listOfParameters>\n').

sbml_write_params([]).

% Modified by Elisabetta
sbml_write_params([(P,V)|T]) :-
   write(sbml,'         <parameter id="'),
   write(sbml,P),
   write(sbml,'" name="'),
   write(sbml,P),
   write(sbml,'" value="'),
   write(sbml,V),
   write(sbml,'"'),
   (  
      event3(_,A,_),
      member(P,A)
   ->
      write(sbml,' constant="false"') % if a parameter value is changed by an event, then the constant attribute of the parameter is set to false
   ;
      true
   ),
   write(sbml,' />\n'),
   sbml_write_params(T).

% Added by Elisabetta
sbml_write_params_macros([]).

sbml_write_params_macros([P|T]) :-
   write(sbml,'         <parameter id="'),
   write(sbml,P),
   write(sbml,'" name="'),
   write(sbml,P),
   write(sbml,'" constant="false" />\n'), % The constant attribute of parameters used for macros is set to false
   sbml_write_params_macros(T).

% Macros, added by Elisabetta
sbml_macros :-
   (
     (
     k_macro(_,V),
     V\=sq_wave(_,_,_,_),
     V\=sq_wave(_,_,_,_,_,_)
     )
   ->
     (
     write(sbml,'      <listOfRules>\n'),
     findall((M,VV),(k_macro(M,VV),VV\=sq_wave(_,_,_,_),VV\=sq_wave(_,_,_,_,_,_)),LM),   % get all the macros different from sq_wave
     sbml_write_macros(LM),
     write(sbml,'      </listOfRules>\n')
     )
   ;
     true
   ).


sbml_write_macros([]).

sbml_write_macros([(M,VV)|T]) :-
   write(sbml,'         <assignmentRule variable="'),
   write(sbml,M),
   write(sbml,'">\n'),
   write(sbml,'            <math xmlns="http://www.w3.org/1998/Math/MathML">\n'),
   g_assign(sbml_indent,18),
   sbml_kinetics(VV),
   write(sbml,'            </math>\n'),
   write(sbml,'         </assignmentRule>\n'),
   sbml_write_macros(T).

% write rules as reactions

sbml_rules :-
   write(sbml,'      <listOfReactions>\n'),
   g_assign(sbml_number,1),
   sbml_write_rules,
   write(sbml,'      </listOfReactions>\n').

% use Name as id, or rule number

sbml_write_rules:-
   rule(R,LL,LR,_,_,_,KL),
   g_read(sbml_number,Nb),
   (
      (R = (Name:RR))
   ->
      format_to_atom(N, "~w~w", [Name, Nb])
   ;
      (N = Nb),
      (RR = R)
   ),
   NNb is Nb+1,
   g_assign(sbml_number,NNb),
   write(sbml,'         <reaction  id="R_'),
   write(sbml,N),
   write(sbml,'" reversible="'),
   (
      (RR = (_ <=> _))
   ->
      write(sbml,'true')
   ;
      write(sbml,'false')
   ),
   write(sbml,'">\n'),
   (
      LL \= []
   ->
      write(sbml,'            <listOfReactants>\n'),
      sbml_species(LL),
      write(sbml,'            </listOfReactants>\n')
   ;
      true
   ),
   (
      LR \= []
   ->
      write(sbml,'            <listOfProducts>\n'),
      sbml_species(LR),
      write(sbml,'            </listOfProducts>\n')
   ;
      true
   ),
   (
      KL = 0
   ->
      true
   ;
      write(sbml,'            <kineticLaw>\n'),
      write(sbml,'               <math xmlns="http://www.w3.org/1998/Math/MathML">\n'),
      g_assign(sbml_indent,18),
      sbml_kinetics(KL),
      write(sbml,'               </math>\n'),
      write(sbml,'            </kineticLaw>\n')
   ),
   write(sbml,'         </reaction>\n'),
   fail.

sbml_write_rules.

% for reactants and products write the list as speciesReferences

sbml_species([]).

sbml_species([(C,H)|T]):-
   sbml_convert_molecule_name(H,HH),
   write(sbml,'               <speciesReference species="'),
   write(sbml,HH),
   write(sbml,'" stoichiometry="'),
   write(sbml,C),
   write(sbml,'"/>\n'),
   sbml_species(T).

% write MathML corresponding to a ratelaw

sbml_kinetics(A*B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<times/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(A/B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<divide/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(A+B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<plus/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(A-B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<minus/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').


sbml_kinetics(min(A,B)):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<min/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').


sbml_kinetics(max(A,B)):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<max/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(A^B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<power/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(abs(A)):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<abs/>\n'),
   sbml_kinetics(A),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(cos(A)):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<cos/>\n'),
   sbml_kinetics(A),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(sin(A)):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<sin/>\n'),
   sbml_kinetics(A),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

sbml_kinetics(frac(A)):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<ci> frac </ci>\n'),
   sbml_kinetics(A),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

%%% Case of a number, added by Elisabetta
   sbml_kinetics(A):-
   number(A),!,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   format(sbml,"<cn> ~w </cn>~n",[A]).

%%% Case of Time, added by Elisabetta
sbml_kinetics(A):-
   A='Time',!,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<csymbol encoding="text" definitionURL="http://www.sbml.org/sbml/symbols/time">\n'),
   J is I+3,
   write_spaces(sbml,J),
   write(sbml,'t\n'),
   write_spaces(sbml,I),
   write(sbml,'</csymbol>\n').

sbml_kinetics(A):-
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   (
      ((A = [C]),sbml_convert_molecule_name(C,B))      
   ->
      true
   ;
      (A = B)
   ),
   format(sbml,"<ci> ~w </ci>~n",[B]).

% Events, added by Elisabetta
sbml_events :-
   (
      (
      event3(_,_,_);
      tevent(_,_,_,_);
      k_macro(_,sq_wave(_,_,_,_));
      k_macro(_,sq_wave(_,_,_,_,_,_))
      )
   ->
      (
      write(sbml,'      <listOfEvents>\n'),
      findall((C,P,V), event3(C,P,V),LE),   % get all the events
      sbml_write_events(LE),
      findall(((('Time'=T) and C),P,V), tevent(T,C,P,V),LTE),   % get all time events
      sbml_write_events(LTE),
      findall((M,V1,D1,V2,D2), k_macro(M,sq_wave(V1,D1,V2,D2)), LM4), % get all sq_wave/4
      sbml_write_sq_wave_4(LM4),
      findall((M,V0,D0,V1,D1,V2,D2), k_macro(M,sq_wave(V0,D0,V1,D1,V2,D2)), LM6), % get all sq_wave/6
      sbml_write_sq_wave_6(LM6),
      write(sbml,'      </listOfEvents>\n')
      )
   ;
      true
   ).

sbml_write_events([]).

sbml_write_events([(C,P,V)|T]) :-
   write(sbml,'         <event>\n'),
   write(sbml,'            <trigger>\n'),
   write(sbml,'               <math xmlns="http://www.w3.org/1998/Math/MathML">\n'),
   write_trigger(C),
   write(sbml,'               </math>\n'),
   write(sbml,'            </trigger>\n'),
   write(sbml,'            <listOfEventAssignments>\n'),
   write_assignments(P,V),
   write(sbml,'            </listOfEventAssignments>\n'),
   write(sbml,'         </event>\n'),
   sbml_write_events(T).

write_trigger(C1 and C2):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<and/>\n'),
   write_trigger(C1),
   write_trigger(C2),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

write_trigger(A<B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<lt/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

write_trigger(A>B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<gt/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

write_trigger(A=B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<eq/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

write_trigger(A=<B):-
   !,
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<leq/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

write_trigger(A>=B):-
   g_read(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'<apply>\n'),
   J is I+3,
   g_assign(sbml_indent,J),
   write_spaces(sbml,J),
   write(sbml,'<geq/>\n'),
   sbml_kinetics(A),
   sbml_kinetics(B),
   g_assign(sbml_indent,I),
   write_spaces(sbml,I),
   write(sbml,'</apply>\n').

write_assignments([],[]).

write_assignments([P|R],[C|G]):-
   write(sbml,'               <eventAssignment variable="'),
   write(sbml,P),
   write(sbml,'">\n'),
   write(sbml,'                  <math xmlns="http://www.w3.org/1998/Math/MathML">\n'),
   sbml_kinetics(C),
   write(sbml,'                  </math>\n'),
   write(sbml,'               </eventAssignment>\n'),
   write_assignments(R,G).

% Square waves are encoded using events
sbml_write_sq_wave_4([]).

sbml_write_sq_wave_4([(M,V1,D1,V2,D2)|T]):-
   sbml_write_events([(frac('Time'/(D1+D2)) < D1/(D1+D2),[M],[V1]), (frac('Time'/(D1+D2)) > D1/(D1+D2),[M],[V2])]),
   sbml_write_sq_wave_4(T).
   

sbml_write_sq_wave_6([]).

sbml_write_sq_wave_6([(M,V0,D0,V1,D1,V2,D2)|T]):-
   sbml_write_events([('Time'=<D0,[M],[V0]),(frac(('Time'-D0)/(D1+D2)) < D1/(D1+D2) and 'Time' > D0,[M],[V1]),(frac(('Time'-D0)/(D1+D2)) > D1/(D1+D2) and 'Time' > D0,[M],[V2])]),
   sbml_write_sq_wave_6(T).
