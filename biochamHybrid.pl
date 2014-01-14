/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2013, INRIA Paris-Rocquencourt, EPI Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * GNU Prolog file biochamHybrid.pl
 * by Katherine HJ Chiang
 */
%=======================================================================%
% #Input
%  Read in two .bc files. 
%   The 1st argument is the filename for ODE simulation; 
%   The 2nd argument is the filename for Stochastic simulation.
%  Volume is the converting ratio from concentration to particle number.
%  Specify the required simulation period in "Time".
%
% #Note
%  1. Please list out all the species initial value
%     (by 'present') in input files.
%  2. If one single species is specified in both input files, 
%     the initial value should be the same.
%
% #Output
%  The outputfile will be named 'out.bc'
%========================================================================%
compose_ode_stochastic(ODEFilename, StochFilename) :-
   compose_ode_stochastic(ODEFilename, StochFilename, 1, 200).
compose_ode_stochastic(ODEFilename, StochFilename, Volume, Time) :-
  %============================%
  %== Preprocessing ODE file ==%
  %============================%
  % 1. Record the ODE species
  OutFilename = 'out.bc',

  load_biocham(ODEFilename),
  findall(R1,
          (
            rule(_, _, _, _, R1_list, _, _), 
            member(R1_pair, R1_list),
            R1_pair = (_Number, R1)
          ),
          Reactants1),
  findall(P1,
          ( 
            rule(_, _, _, _, _, P1_list, _),
            member(P1_pair, P1_list),
            P1_pair = (_Number, P1)
          ),
          Products1),  
  append(Reactants1, Products1, ODESpecies),
  sort(Reactants1), % Used for later hybrid-reactants finding
  sort(ODESpecies),
  clear_rules,

  %===================================%
  %== Preprocessing Stochastic file ==%
  %===================================%
  % 1. Copy all the parameters.
  % 2. Change all "present" into "parameter", with the same initial value.
  %    Change the species name by appending "_stoch" to the original name   
  % 3. Add two parameters for stochastic simulation: 
  %    parameter (tau, 0).
  %    parameter (ran, 0).
  % 4. Build Macro for the species appearing in both paradigms.
  % 5. User-defined volume.
  % 6. Build hybrid-reactants list.      

  % process the parameters and presents
  load_biocham(StochFilename),
  open(OutFilename, append, Stream),
 
  findall([Parameter, Value],(
          k_parameter(Parameter, Value),
          write(Stream, 'parameter('), write(Stream, Parameter),
          write(Stream, ', '), 
          write(Stream, Value), 
          write(Stream, ').'),
          nl(Stream)),
          _),
  write(Stream, 'parameter(tau, 0).'), nl(Stream), 
  write(Stream, 'parameter(ran, 0).'), nl(Stream),

  findall(Hybrid_original,(
          initconc(Hybrid_original, Init_value_pre),
          member(Hybrid_original, ODESpecies)
          ), Hybrid_originals),

  findall(Hybrid_species,(
          initconc(Present_pre, Init_value_pre),
          (
            member(Present_pre, ODESpecies)
           ->
            (
              atom_concat(Present_pre, '_stoch', Present),
              Init_value = 0,
              atom_concat(Present_pre, '_total', Hybrid_species),
              write(Stream, 'macro('), 
              write(Stream, Hybrid_species),
              write(Stream, ', '), 
              write(Stream, Volume), 
              write(Stream, '* ['), 
              write(Stream, Present_pre), 
              write(Stream, '] + '),
              write(Stream, Present), 
              write(Stream, ').'), 
              nl(Stream)
            )
           ;
            (
              atom_concat(Present_pre, '_stoch', Present),
              Init_value = Init_value_pre
            )
          ),
          write(Stream, 'parameter('), write(Stream, Present),
          write(Stream, ', '), write(Stream, Init_value), 
          write(Stream, ').'), nl(Stream)
        ),_Hybrids),

  clear_rules,
  load_biocham(ODEFilename),
  alpha_loop_ode(Hybrid_originals),
  export_biocham('out', _),
  clear_rules,

  load_biocham(StochFilename),

  findall(R2,( 
          rule(_, _, _, _, R2_list, _, _), 
          member(R2_pair, R2_list),
          R2_pair = (_Number, R2_pre),

          print(R2_pair), print('\n'),
 
          atom_concat(R2_pre, '_stoch', R2)),
          Reactants2),

  % Add constraints for hybrid species! (28/9/12)
  % For hybrid species, the determinisic part can make the particle number
  % lower than one discrete step needed for stochastic simulation.
  findall(Rc_list, rule(_, _, _, _, Rc_list, _, _), Reactants_by_rules),
  
  build_constraints_by_rules(Reactants_by_rules, Reactants1, 1, 
                             [], Hybrid_reactants_list),  
  % print('Hybrid_reactants_list: '),
  % print(Hybrid_reactants_list), print('\n'),
  build_constraints_list(Hybrid_reactants_list, 1, [], Constraints_list),
  % print('out from build_constraints_list\n'),
  % print(Constraints_list), print('\n'),

  findall(P2, (
          rule(_, _, _, _, _, P2_list, _), 
          member(P2_pair, P2_list),
          P2_pair = (_Number, P2_pre),
          atom_concat(P2_pre, '_stoch', P2)),
       Products2),
  append(Reactants2, Products2, StochSpecies),
  sort(StochSpecies), % To eliminate duplication.

  % build StochReactants(with '_total') from StochSpecies.
  length(StochSpecies, Species_number),
  build_reactants(StochSpecies, ODESpecies, Species_number, 
                  1, [], StochReactants),

  %========================================%
  %==== Build Data Lists for Reactions ====%
  %========================================%
    alpha_loop(ODESpecies),
     findall([Species_change, Alpha],
    (
        rule(_, Reactants, Products, _, _, _, Alpha),

        % To get [RSpecies, PSpecies] from [Reactants, Products]
        findall([Species1, Minus_number],
          (
            member(R, Reactants),
            R = (Minus_number, Rname),
            atom_concat(Rname, '_stoch', Species1)
          ), RSpecies),

        findall([Species2, Add_number],
          (
            member(P, Products),
            P = (Add_number, Pname),
            atom_concat(Pname, '_stoch', Species2)
          ), PSpecies),

  %== Build list (Species_change) to record species change ==% 
        append_reactants(RSpecies, 1, [], Temp_list, StochSpecies),
        append_products(PSpecies, 1, Temp_list, Changes, StochSpecies),
 
        build_change_list(Changes, 1, Species_number, [], Species_change)
    ),
    Reactions_data
  ),

%  print('Change list: '), print(Reactions_data), print('\n'),
  

%== Build Events and Calaculate Alphas==%
 print_event(Reactions_data, StochSpecies, StochReactants,
             Hybrid_reactants_list, Constraints_list, Stream),

  print_common_alpha_macros(Reactions_data, 1, Stream),
  
  write(Stream, 'numerical_method(rk).'), nl(Stream),

/*
% Plotting Part; depends on what the user want to show on the plot.
  write(Stream, 'show_macros.'), nl(Stream),
  write(Stream, 'show_parameters.'), nl(Stream),
  write(Stream, 'show_macros({'),
  concat_species2(Hybrids, '', Hybrid_Results),
  write(Stream, Hybrid_Results),
  write(Stream, '}).'), nl(Stream),	
*/
  write(Stream, 'show_parameters({'),
  concat_species2(StochSpecies, '', Species_Results),
  write(Stream, Species_Results),
  write(Stream, '}).'), nl(Stream),

  write(Stream, 'numerical_simulation('), 
  write(Stream, Time), 
  write(Stream, ').'), 
  nl(Stream),
  write(Stream, 'plot.'), 
  close(Stream).

%===========================================================%
%================== END of Preprocessor ====================%
%===========================================================%

% The 'Current_counter' here is used for indexing reactions.
build_constraints_by_rules(Reactants_by_rules, ODEReactants, 
                           Current_counter, Orig_list, Results_list) :-
  length(Reactants_by_rules, Number_of_rules),
  nth(Current_counter, Reactants_by_rules, This_reactants_list),
  findall(Constraints,(
     member(Reactants_pair, This_reactants_list),
     Reactants_pair = (Least_number, Reactant_name),
%     print('ode_reactants: '), print(ODEReactants), print('\n'),
     member(Reactant_name, ODEReactants)
   ->
    (
     atom_concat(Reactant_name, '_total >= ', Temp),
     Reactants_pair = (Least_number, Reactant_name),
     number_atom(Least_number, Least_number_atom),
     atom_concat(Temp, Least_number_atom, Constraints)
    )
   ;
    Constraints = 0
  ),This_constraints_list),
%   print(This_constraints_list),print('###\n'),
  append(Orig_list, [This_constraints_list], Next_list),
 (
  Current_counter < Number_of_rules
 ->
  (
    Next_counter is Current_counter + 1,
    build_constraints_by_rules(Reactants_by_rules, ODEReactants, 
                               Next_counter, Next_list, Results_list)
  )
 ;
  (
    Results_list = Next_list,
    !
  )
 ).

build_constraints_list(Hybrid_reactants_list, Reaction_counter, 
                       Init_list, Constraints_list) :-
  nth(Reaction_counter, Hybrid_reactants_list, This_list),
 (
  This_list \== [0]
 ->
  (
    number_atom(Reaction_counter, Reaction_counter_atom),
    atom_concat('lower_level', Reaction_counter_atom, Corresponding_constraint),
    append(Init_list, [Corresponding_constraint], Next_list)
  )
 ;
  (
    append(Init_list, ['0'], Next_list)
  )
 ),
 (
  length(Hybrid_reactants_list, Total_count),
  Reaction_counter < Total_count
 ->
  (
    Next_counter is Reaction_counter + 1,
    build_constraints_list(Hybrid_reactants_list, Next_counter, 
                           Next_list, Constraints_list)
  )
 ;
  Constraints_list = Next_list
 ).

alpha_loop_ode(Hybrid_originals) :-
  retract(rule(A1, A2, A3, A4, A5, A6, Alpha)),
  transform_alpha_ode(Alpha, Result_alpha, Hybrid_originals),
  assertz(rule(A1, A2, A3, A4, A5, A6, Result_alpha)),
%  print('assertz!\n'),
  fail.

alpha_loop_ode(_) :- true.

transform_alpha_ode(Orig, Result, Hybrid_originals) :-
  Orig =.. Orig_list,
  transform_ode(Orig_list, Result_list, Hybrid_originals),
  Result =.. Result_list.

transform_ode([], [], _).

transform_ode([Head | Tail], [Head_post | Tail_post], Hybrid_originals) :-
  nth(3, [Head | Tail], '[]')
 ->
  (
    nth(2, [Head | Tail], Species),
    member(Species, Hybrid_originals)
    ->
     (
       atom_concat(Species, '_total', New),
       Head_post = New, Tail_post = []
     )
    ;
     (
       Head_post = Head,
       Tail_post = Tail
     )
  )
 ;
  (
   (
    atomic(Head)
   ->
      Head = Head_post
   ;
    (
      Head =.. Head_list,
      transform_ode(Head_list, Head_list_post, Hybrid_originals),
      Head_post =.. Head_list_post
    )
   ),
      transform_ode(Tail, Tail_post, Hybrid_originals)
  )
 .



alpha_loop(ODESpecies) :- 
  retract(rule(A1, A2, A3, A4, A5, A6, Alpha)),
  transform_alpha(Alpha, Result_alpha, ODESpecies),
  assertz(rule(A1, A2, A3, A4, A5, A6, Result_alpha)),
  fail.

alpha_loop(_) :- true.

transform_alpha(Orig, Result, ODESpecies) :-
  Orig =.. Orig_list,
  transform(Orig_list, Result_list, ODESpecies),
  Result =.. Result_list.

transform([], [], _).

transform([Head | Tail], [Head_post | Tail_post], ODESpecies) :-
  nth(3, [Head | Tail], '[]')
 ->
  (
    nth(2, [Head | Tail], Species),
    member(Species, ODESpecies)
    ->
     (
       atom_concat(Species, '_total', New),
       Head_post = New, Tail_post = []
     )
    ;
     (
       nth(2, [Head | Tail], Species),
       atom_concat(Species, '_stoch', New),
       Head_post = New, Tail_post = []
     )
  )
 ;
  (
   (
    atomic(Head)
   ->
      Head = Head_post
   ;
    (
      Head =.. Head_list,
      transform(Head_list, Head_list_post, ODESpecies),
      Head_post =.. Head_list_post
    )
   ),
      transform(Tail, Tail_post, ODESpecies)
  )
 .


build_reactants(StochSpecies, ODESpecies, Species_number, 
                Counter, Init, StochReactants) :-
  nth(Counter, StochSpecies, This_species),
  member(This_species, ODESpecies)
 ->
  (
    atom_concat(This_species, '_total', This_reactant)
 ;
    atom_concat(This_species, '_stoch', This_reactant)
  )
 ,
  append(Init, [This_reactant], Next),
  Counter < Species_number
 ->
  (
    Next_counter is Counter + 1,
    build_reactants(StochSpecies, ODESpecies, Species_number, 
                    Next_counter, Next, StochReactants)
  )
 ;
  StochReactants = Next.

%=============================%
%== Get Data from Reactions ==%
%=============================%

initialize_list(0, _Element, Init_List, List) :- Init_List = List, !.

initialize_list(Length, Element, Init_list, List) :-
  Length >= 1,
  append(Init_list, [Element], New_list),
  Next_length is (Length - 1),
  initialize_list(Next_length, Element, New_list, List).


append_reactants(Reactants, Counter, Init_list, Middle_list, Species_order) :-
  length(Reactants, Number_of_reactants),
  Counter =< Number_of_reactants
 ->
  (
    nth(Counter, Reactants, [RName, RNumber]),
    nth(RPosition, Species_order, RName),
    number_atom(RNumber, RNumber_atom),
    atom_concat('-', RNumber_atom, Rchange),
    append(Init_list, [[RPosition, Rchange]], Next_init),

    Next_counter is Counter + 1,
    append_reactants(Reactants, Next_counter, Next_init, 
                     Middle_list, Species_order)
  )
 ;
  Middle_list = Init_list.


append_products(Products, Counter, Init_list, Changes, Species_order) :-
  length(Products, Number_of_products),
  Counter =< Number_of_products
 ->
  (
    nth(Counter, Products, [PName, PNumber]),
    nth(PPosition, Species_order, PName),
    number_atom(PNumber, PNumber_atom),
    atom_concat('+', PNumber_atom, Pchange),
    append(Init_list, [[PPosition, Pchange]], Next_init),
    Next_counter is Counter + 1,
    append_products(Products, Next_counter, Next_init, Changes, Species_order)
  )
 ;
  (
    Changes = Init_list
  ).


summation(Input_list, Count, Init, Final) :-
  length(Input_list, Length),
  nth(Count, Input_list, To_add),
  atom_concat(Init, To_add, Temp),
(
  Count < Length
 ->
  (
    Next_count is Count + 1,
    summation(Input_list, Next_count, Temp, Final)
  )
 ;
  (
     Final = Temp
  )
).


build_change_list(Changes, Counter, Length, Init, Result) :-

 Counter =< Length
->
 (
   (
    \+ member([Counter, _], Changes)
   ->
    (
      number_atom(0, Zero),
      atom_concat('+', Zero, No_change),
      append(Init, [No_change], Temp_list)
    )
   ;
    (
      findall(N, member([Counter, N], Changes), N_list),
      summation(N_list, 1, '', Sum),
      append(Init, [Sum], Temp_list)
    )
  ),
  Next_counter is (Counter + 1),
  build_change_list(Changes, Next_counter, Length, Temp_list, Result)
 )
;
 (
    Result = Init
 ).
%================================%
%==== Print event and alphas ====%
%================================%
% add about the hybrid reactants'constraint (28/9/12)
print_event(Reactions_data, Stoch_species_order, _Stoch_reactants_order, 
            Hybrid_reactants_list, Constraints_list, Stream) :-  

% original version
% print_event(Reactions_data, Stoch_species_order, 
%            Stoch_reactants_order, Stream) :-

  length(Reactions_data, Number_of_alpha),
  print_alpha_parameters(Number_of_alpha, 1, Stream),
  print_constraints_parameters(Constraints_list, 1, Stream),

% 1. Build Starting String for the event.
  write(Stream, 'event(Time > tau, [tau, ran'),
%%%  print('Checkpoint 1\n'),
  print_static_alpha(Number_of_alpha, 1, Stream),
  print_event_constraints(Constraints_list, 1, Stream), 
  concat_species(Stoch_species_order, 1, '', Species_for_event),
  write(Stream, Species_for_event),
  write(Stream, '], [Time + delta_t, random'),
%%%  print('Checkpoint 2\n'),
  print_updating_alpha(Number_of_alpha, 1, Stream),
%%%  print('Checkpoint 3\n'),
  print_hybrid_species_constraint(Hybrid_reactants_list, 1, Stream),
%%%  print('Checkpoint 4\n'),

% 2. Build alpha list and alpha subsums.
  length(Reactions_data, Number_of_alpha),

  % Build Alpha_list
  build_alpha_list(1, [], Number_of_alpha, Alpha_list),

  % Build Alpha subsums
  nth(1, Alpha_list, First_alpha),
  append([], [First_alpha], Init_list),
  alpha_accumulation(Alpha_list, Init_list, Alpha_accumulation_list, 2),

% 3. Print reactions in event
  length(Stoch_species_order, Number_of_species),
  initialize_list(Number_of_species, [], [], Init_list2),

  % Build the print_helper_list.
  print_normal(Reactions_data, Init_list2, Print_helper_list, Alpha_list, 
            Alpha_accumulation_list, Constraints_list, Stoch_species_order), 
%%%  print('Checkpoint 5\n'),
  % Use print_helper_list to really print out the event part.
  print_change_list(Print_helper_list, Stoch_species_order, 
                    1, Number_of_species, Stream),
%%%  print('Checkpoint 6\n'),
  
  % 4. Print Macros
%  nth(Number_of_alpha, Alpha_accumulation_list, Alpha_constitution),
  write(Stream, 'macro(alpha_summ'),
  write(Stream, ', '),
  print_alpha_sum(Number_of_alpha, 1, Stream),
%  write(Stream, Alpha_constitution),
  write(Stream, ').'),
  nl(Stream),
  write(Stream, 'macro(delta_t'),
  write(Stream, ', (-1 / alpha_summ) * log(random)).'),
  nl(Stream).

%=========================%
%== Print common alphas ==%
%=========================%
print_common_alpha_macros(Reactions_data, Start_counter, Stream) :-
  length(Reactions_data, L1),
  Start_counter =< L1
 ->
  (
    write(Stream, 'macro(alpha'),
    write(Stream, Start_counter),
    write(Stream, 'm, '),
    nth(Start_counter, Reactions_data, Data_list),
    nth(2, Data_list, N_alpha),
    write(Stream, N_alpha),
    write(Stream, ').'),
    nl(Stream),
    Next_counter is (Start_counter + 1),
    print_common_alpha_macros(Reactions_data, Next_counter, Stream)
  )
 ;
   nl(Stream).

%========================================%  
%== Build Alpha list for current event ==%
%========================================%  
build_alpha_list(Counter_for_alpha, Init, Number_of_alpha, Alpha_list) :-
  Counter_for_alpha =< Number_of_alpha
 ->
  (
    number_atom(Counter_for_alpha, Counter_for_alpha_atom),
    atom_concat('alpha', Counter_for_alpha_atom, Current_alpha),
    append(Init, [Current_alpha], Next_init),
    Next_counter_alpha is (Counter_for_alpha + 1),
    build_alpha_list(Next_counter_alpha, Next_init, 
                     Number_of_alpha, Alpha_list)
  )
 ;
  Alpha_list = Init.

%========================================%
%== Help Predicates for event building ==%
%========================================%
print_alpha_sum(Number_of_alpha, Current_count, Stream) :-
  write(Stream, 'alpha'), 
  write(Stream, Current_count),
  write(Stream, 'm + '),
  Current_count < (Number_of_alpha - 1)
 ->
  (
    Next_count is Current_count + 1,
    print_alpha_sum(Number_of_alpha, Next_count, Stream)
  )
 ;
  (
    Next_count is Current_count + 1,
    write(Stream, 'alpha'),
    write(Stream, Next_count),
    write(Stream, 'm')
  ).


print_alpha_parameters(Number_of_alpha, Current_count, Stream) :-
  write(Stream, 'parameter(alpha'),
  write(Stream, Current_count),
  write(Stream, ', 0).\n'),
  Current_count < Number_of_alpha
 ->
  (
    Next_count is Current_count + 1,
    print_alpha_parameters(Number_of_alpha, Next_count, Stream)
  )
 ;
  write(Stream, 'parameter(alpha_sum, 0).\n').


print_static_alpha(Number_of_alpha, Current_count, Stream) :-
  write(Stream, ', alpha'),
  write(Stream, Current_count),
  Current_count < Number_of_alpha
 ->
  (
    Next_count is Current_count + 1,
    print_static_alpha(Number_of_alpha, Next_count, Stream)
  )
 ;
  write(Stream, ', alpha_sum').


print_updating_alpha(Number_of_alpha, Current_count, Stream) :-
  write(Stream, ', alpha'),
  write(Stream, Current_count),
  write(Stream, 'm'),
  Current_count < Number_of_alpha
 ->
  (
    Next_count is Current_count + 1,
    print_updating_alpha(Number_of_alpha, Next_count, Stream)
  )
 ;
  write(Stream, ', alpha_summ\n'), !.


print_constraints_parameters(Constraints_list, Current_counter, Stream) :-
  length(Constraints_list, Total_count),
  nth(Current_counter, Constraints_list, This_reaction_constraints),
  (
    This_reaction_constraints \== '0'
   ->
     (
       write(Stream, 'parameter(lower_level'),
       write(Stream, Current_counter),
       write(Stream, ', 0).\n')
     )
   ;
     !
  ),
  Current_counter < Total_count
 ->
  (
    Next_counter is Current_counter + 1,
    print_constraints_parameters(Constraints_list, Next_counter, Stream)
  )
 ;
  !.

print_event_constraints(Constraints_list, Current_counter, Stream) :-
  length(Constraints_list, Total_count),
  nth(Current_counter, Constraints_list, This_reaction_constraints),
  (
    This_reaction_constraints \== '0'
   ->
     (
       write(Stream, ', lower_level'),
       write(Stream, Current_counter)
     )
   ;
     !
  ),
  Current_counter < Total_count
 ->
  (
    Next_counter is Current_counter + 1,
    print_event_constraints(Constraints_list, Next_counter, Stream)
  )
 ;
  !.
 

print_single_reaction_constraints(Constraints, Total, Current, Stream) :-
  Current < Total
 ->
  (
    nth(Current, Constraints, To_print),
    write(Stream, To_print),
    write(Stream, ' and '),
    Next is Current + 1,
    print_single_reaction_constraints(Constraints, Total, Next, Stream)
  )
 ;
  (
    nth(Current, Constraints, To_print),
    write(Stream, To_print),
    !
  ).

print_hybrid_species_constraint(Hybrid_reactants_list, 
                                Current_counter, Stream) :-
  
  length(Hybrid_reactants_list, Total_count),
  nth(Current_counter, Hybrid_reactants_list, This_reaction_constraints),
  length(This_reaction_constraints, Number_of_constraints),
  (
   This_reaction_constraints \== [0]
 ->
  (
    write(Stream, ', if '), 
    print_single_reaction_constraints(This_reaction_constraints, 
                                      Number_of_constraints, 1, Stream),
    write(Stream, ' then 1 else 0')
  )
  ;
    !
  ),
 
  Current_counter < Total_count
 ->
  (
    Next_counter is Current_counter + 1,
    print_hybrid_species_constraint(Hybrid_reactants_list, Next_counter, Stream)
  )
 ;
  !.

alpha_accumulation(Alpha_list, Previous_list, 
                   Alpha_accumulation_list, Current_count) :-
  length(Alpha_list, Number_of_alpha),
  Current_count =< Number_of_alpha
 ->
  (
    nth(Current_count, Alpha_list, Alpha_2add),
    Previous_count is (Current_count - 1),
    nth(Previous_count, Previous_list, Previous_sum),
    atom_concat(Previous_sum, ' + ', Temp),
    atom_concat(Temp, Alpha_2add, New_sum),
    append(Previous_list, [New_sum], New_list),
    New_count is (Current_count + 1),
    alpha_accumulation(Alpha_list, New_list, 
                       Alpha_accumulation_list, New_count)
  )
 ;
  Alpha_accumulation_list = Previous_list.


concat_species(Species, Count, Init, Results) :-
  length(Species, L),
  Count =< L
 ->
  (
    nth(Count, Species, Species1),
    atom_concat(', ', Species1, Species1_processed),
    atom_concat(Init, Species1_processed, NewSubstring),
    Next_count is Count + 1,
    concat_species(Species, Next_count, NewSubstring, Results)
  )
 ;
  Init = Results.


concat_species2(Species, Init, Results) :-
  nth(1, Species, First),
  atom_concat(Init, First, Newsubstring),
  concat_species(Species, 2, Newsubstring, Results).

%=========================%
%== Building HelperList ==%
%=========================%
% 1. 
print_normal(Reactions_data, Init_list, N_finish_list, Alpha_list, 
          Alpha_accumulation_list, Constraints_list, Stoch_species_order) :- 

  % Find alpha.
  nth(1, Alpha_list, First_alpha),

  % Build Connections to constraints.
  nth(1, Constraints_list, Constraint),  

  % Check species changes
  nth(1, Reactions_data, First_datalist),
  nth(1, First_datalist, First_species_change),
  check_change1(First_alpha, Constraint, Stoch_species_order, 
                1, First_species_change, Init_list, Later_list),

  % Recursive call for all Normal reactions.
  print_normal_middle(Reactions_data, Later_list, N_finish_list,
                      Alpha_list, Alpha_accumulation_list, Constraints_list,
                      Stoch_species_order, 2).


check_change1(First_alpha, Constraint, Stoch_species_order, Species_counter, 
              First_species_change, Orig_list, Later_list) :-
  nth(Species_counter, First_species_change, Species_change),

  Species_change == '+0'
 ->
  (
    length(Stoch_species_order, Last_count),
    Species_counter < Last_count
  ->
    (
      Next_species_counter is (Species_counter + 1),
      check_change1(First_alpha, Constraint, Stoch_species_order, 
            Next_species_counter, First_species_change, Orig_list, Later_list)
    )
   ;
      Later_list = Orig_list
  )
 ;
  (
    nth(Species_counter, Stoch_species_order, Species),
    nth(Species_counter, First_species_change, Species_change),
    
    atom_concat(Species, Species_change, Change),
    nth(Species_counter, Orig_list, _Append_position),

    % !!! Putting things into the helper list !!!
    add_n_copy_list(Orig_list, 1, Species_counter, 
                    [[0, First_alpha], Change, Constraint], [], Next_list),
%%%    print('Next list = '), print(Next_list), print('\n'),
    (
    length(Stoch_species_order, Last_count),
    Species_counter < Last_count
  ->
    (
      Next_counter is (Species_counter + 1),
      check_change1(First_alpha, Constraint, Stoch_species_order, Next_counter,
                    First_species_change, Next_list, Later_list)
    )
   ;
      Later_list = Next_list
   )
  ).


add_n_copy_list(Orig_list, Start_counter, Species_counter, 
                Next_list_temp, Out_init, Next_list) :-
  length(Orig_list, Total_length),
  Start_counter =< Total_length
 ->
  (
   (
    Start_counter == Species_counter
   ->
    (
      nth(Start_counter, Orig_list, To_add),
      append(To_add, Next_list_temp, Next_insert),
      append(Out_init, [Next_insert], Middle_list)
    )
   ;
    (
      nth(Start_counter, Orig_list, To_add),
%%%   print('to add = '), print(To_add), print('\n'),
      append(Out_init, [To_add], Middle_list)
    )),
   Next_counter is (Start_counter + 1),
   add_n_copy_list(Orig_list, Next_counter, Species_counter, 
                   Next_list_temp, Middle_list, Next_list)
  )
 ;
   Next_list = Out_init. 

% 2.
print_normal_middle(Reactions_data, Orig_list, Result_list, 
                    Alpha_list, Alpha_accumulation_list, Constraints_list,
                    Stoch_species_order, Counter_for_reaction) :-
%%%  print('Orig_list: '), print(Orig_list), print('\n'),
  % Find alpha and the range of constraints.
  nth(Counter_for_reaction, Reactions_data, Datalist),
  nth(Counter_for_reaction, Constraints_list, Constraint),

  Previous_reaction is (Counter_for_reaction - 1),

  % Get the species changes.
  nth(1, Datalist, Species_change),
  nth(Previous_reaction, Alpha_accumulation_list, First_alpha),
  nth(Counter_for_reaction, Alpha_accumulation_list, Second_alpha),
  check_change(First_alpha, Second_alpha, Constraint, Stoch_species_order,
               1, Species_change, Orig_list, Middle_list),
%%%  print('middle list: '), print(Middle_list), print('\n'),
  % Recursive condition.
 (
  length(Reactions_data, Last_reaction),
  Counter_for_reaction < Last_reaction
 ->
  (
    Next_reaction is (Counter_for_reaction + 1),
    print_normal_middle(Reactions_data, Middle_list, Result_list,
                        Alpha_list, Alpha_accumulation_list, Constraints_list,
                        Stoch_species_order, Next_reaction)
  )
 ;
  (
    Result_list = Middle_list
%%%    print(Result_list), print('\n')
  )
 ).

% check_change is inside the same reaction.
check_change(First_alpha, Second_alpha, Constraint, Stoch_species_order,
       Species_counter, Species_change, Orig_list, Result_list) :-
  nth(Species_counter, Species_change, Change),

  Change == '+0'
 ->
  (
    length(Stoch_species_order, Last_count),

    Species_counter < Last_count
  ->
    (
       Next_species is (Species_counter + 1), 
       check_change(First_alpha, Second_alpha, Constraint, Stoch_species_order,
                    Next_species, Species_change, Orig_list, Result_list)
    )
   ;
      Result_list = Orig_list
  )
 ;
  (
    nth(Species_counter, Stoch_species_order, Species),
    nth(Species_counter, Species_change, Change),
    atom_concat(Species, Change, Numerous_change),
   
    % !!! Putting things into the helper list !!!

    add_n_copy_list(Orig_list, 1, Species_counter, 
    [[First_alpha, Second_alpha], Numerous_change, Constraint], [], Next_list),
   (
    length(Stoch_species_order, Last_count),
    Species_counter < Last_count
   ->
    (
       Next_species is (Species_counter + 1), 
       check_change(First_alpha, Second_alpha, Constraint, Stoch_species_order,
                    Next_species, Species_change, Next_list, Result_list)
    )
   ;
       Next_list = Result_list
    )
  ).

%======================%
%== Real Output Part ==%
%======================%

print_change_list(Print_helper_list, Species_order, 
                  Current_counter, Last_counter, Stream) :-
%%% print(Print_helper_list), print('\n'),
  nth(Current_counter, Print_helper_list, Change_list),
(
  length(Change_list, Triple_of_change),
  Triple_of_change > 0
 ->
  (
   (
    % First Change start with only "if".
    nl(Stream),
    write(Stream, ', if alpha_sum * ran > '),
    nth(1, Change_list, First_alpha),
    nth(1, First_alpha, Lower),
    write(Stream, Lower),

    write(Stream, ' and alpha_sum * ran =< '),
    nth(2, First_alpha, Upper),
    write(Stream, Upper),

    nth(3, Change_list, Constraint),
    (
      Constraint \== '0'
     -> 
      (
        write(Stream, ' and '),
        write(Stream, Constraint),
        write(Stream, ' > 0 ')
      )
     ;
      print('No constraint for this\n')
    ),
    
    write(Stream, ' then '),
  
    nth(2, Change_list, First_change),
    write(Stream, First_change),
   (
    Change_number is (Triple_of_change / 3),
    Change_number > 1
   ->
      print_ifThenElse(Change_list, 2, Change_number, Stream)
   ;
    !
   )
  ),
    % The last one.
    nl(Stream),
    write(Stream, ' else '),
    nth(Current_counter, Species_order, Species_name),
    write(Stream, Species_name),
    nl(Stream)
 ) 
 ;
 (
    nth(Current_counter, Species_order, Species_name),
    write(Stream, Species_name)
 )
),
  % Recursive condition on species
(
  Current_counter < Last_counter
 ->
  (
    Next_counter is (Current_counter + 1),
    print_change_list(Print_helper_list, Species_order, Next_counter, 
                      Last_counter, Stream)
  )
 ;
  write(Stream, ']'), write(Stream, ').'), nl(Stream), !).


print_ifThenElse(Change_list, Start, End, Stream) :-
  nl(Stream),
  write(Stream, ' else if alpha_sum * ran > '),

  Alpha_pos is (3 * Start - 2), 
  nth(Alpha_pos, Change_list, [Lower, Upper]),
  write(Stream, Lower),
  write(Stream, ' and alpha_sum * ran =< '),
  write(Stream, Upper),

  Constraint_pos is (3 * Start),
  nth(Constraint_pos, Change_list, Constraint),
  (
   Constraint \== '0'
    -> 
    (
      write(Stream, ' and '),
      write(Stream, Constraint),
      write(Stream, ' > 0 ')
    )
   ;
    print('no constraint!\n')
  ),
  write(Stream, ' then '),

  Change_pos is (3 * Start - 1),
  nth(Change_pos, Change_list, Change),
  write(Stream, Change),
  
  Start < End
 ->
  (
    Next_start is (Start + 1),
    print_ifThenElse(Change_list, Next_start, End, Stream)
  )
 ;
  !.

/* Currently unused predicates:
% String Processing function--splits.
splits(S,Separator,Separator,R):-
  name(Separator,L1),
  name(S,L3),
  append(L1,L2,L3),
  name(R,L2),
  !.

splits(S,Separator,L,Separator):-
  name(Separator,L2),
  name(S,L3),
  append(L1,L2,L3),
  name(L,L1),
  !.

splits(S,Separator,Left,Right):-
  name(S,L3),
  append(Lleft,Lrest,L3),
  name(Separator,L4),
  append(L4,Lright,Lrest),
  name(Left,Lleft),
  name(Right,Lright),!.

splits(S,_,S,''):-!.

complement_condition(Original_condition, Complemented_condition) :-
  name(Original_condition, Original_list),
  name(' and ', And_list),
  name(' or ', Or_list),

  sublist(And_list, Original_list)
 ->
  (
    splits(Original_condition, 'and', Left, Right),
    complement_condition(Left, Left_complemented),
    complement_condition(Right, Right_complemented),
    name(Left_complemented, Left_complemented_list),
    name(Right_complemented, Right_complemented_list),
    append(Left_complemented_list, Or_list, Temp_list),
    append(Temp_list, Right_complemented_list, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 ;
  other_relations1(Original_condition, Complemented_condition).

other_relations1(Original_condition, Complemented_condition) :-
  name(Original_condition, Original_list),
  name(' or ', Or_list),
  name(' and ', And_list),
  sublist(Or_list, Original_list)
 ->
  (
    splits(Original_condition, 'or', Left, Right),
    complement_condition(Left, Left_complemented),
    complement_condition(Right, Right_complemented),
    name(Left_complemented, Left_complemented_list),
    name(Right_complemented, Right_complemented_list),
    append(Left_complemented_list, And_list, Temp_list),
    append(Temp_list, Right_complemented_list, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 ;
  other_relations2(Original_condition, Complemented_condition).

other_relations2(Original_condition, Complemented_condition) :-
  name(Original_condition, Original_list),
  name('>=', Larger_equ_list),
  name('<', Smaller_list),
  sublist(Larger_equ_list, Original_list)
 ->
  (
    splits(Original_condition, '>=', Left, Right),
    name(Left, Left_list),
    name(Right, Right_list),
    append(Left_list, Smaller_list, Temp_list),
    append(Temp_list, Right_list, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 ;
  other_relations3(Original_condition, Complemented_condition).

other_relations3(Original_condition, Complemented_condition) :-
  name(Original_condition, Original_list),
  name('=<', Smaller_equ_list),
  name('>', Larger_list),
  sublist(Smaller_equ_list, Original_list)
 ->
  (
    splits(Original_condition, '=<', Left, Right),
    name(Left, Left_list),
    name(Right, Right_list),
    append(Left_list, Larger_list, Temp_list),
    append(Temp_list, Right_list, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 ;
  other_relations4(Original_condition, Complemented_condition).

other_relations4(Original_condition, Complemented_condition) :- 
  name(Original_condition, Original_list),
  name('>=', Larger_equ_list),
  name('<', Smaller_list),
  sublist(Smaller_list, Original_list)
 ->
  (
    splits(Original_condition, '<', Left, Right),
    name(Left, Left_list),
    name(Right, Right_list),
    append(Left_list, Larger_equ_list, Temp_list),
    append(Temp_list, Right_list, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 ;
  other_relations5(Original_condition, Complemented_condition).

other_relations5(Original_condition, Complemented_condition) :-
  name(Original_condition, Original_list),
  name('=<', Smaller_equ_list),
  name('>', Larger_list),
  sublist(Larger_list, Original_list)
 ->
  (
    splits(Original_condition, '>', Left, Right),
    name(Left, Left_list),
    name(Right, Right_list),
    append(Left_list, Smaller_equ_list, Temp_list),
    append(Temp_list, Right_list, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 ;
  other_relations6(Original_condition, Complemented_condition).

other_relations6(Original_condition, Complemented_condition) :-
  name(Original_condition, Original_list),
  name('<', Smaller_list),
  name('>', Larger_list),
  name('=', Equal_list),
  name(' or ', Or_list),
  sublist(Equal_list, Original_list)
 ->
  (
    splits(Original_condition, '=', Left, Right),
    name(Left, Left_list),
    name(Right, Right_list),
    append(Left_list, Smaller_list, Temp_list1),
    append(Temp_list1, Right_list, Complemented_half1),
    append(Left_list, Larger_list, Temp_list2),
    append(Temp_list2, Right_list, Complemented_half2),
    append(Complemented_half1, Or_list, Complemented_half),
    append(Complemented_half, Complemented_half2, Complemented_list),
    name(Complemented_condition, Complemented_list)
  )
 .
*/
