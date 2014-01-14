" BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
" Copyright 2003-2006, INRIA, Projet Contraintes
"
" This program is free software; you can redistribute it and/or
" modify it under the terms of the GNU General Public License
" as published by the Free Software Foundation; either version 2
" of the License, or (at your option) any later version.
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
"
" Vim syntax file
" Language:    Biocham
" Maintainers: Sylvain Soliman <Sylvain.Soliman@inria.fr>

syn keyword Keyword  where in and parts_of not
syn keyword Keyword  for if then else
syn keyword Define   present absent undefined parameter macro
syn keyword Define   make_present_not_absent make_absent_not_present
syn keyword Define   clear_initial_state initial_state parameter
syn keyword Define   list_parameters macro list_macros

syn match   Operator    "+\|?\|-\|lt\|gt\|leq\|geq\|eq\|\*\|/\|^\|log\|exp"
syn match   Function    "\~{[^}]*}"
syn match   Delimiter   "(\|)"
syn match   Delimiter   "<\?=>\|<\?=\[[^]]*]=>"
syn match   Typedef     "\[[^]]*\]"
syn match   Typedef     "\w\+ *:"
syn match   Comment     "%.*"
syn match   String      "\(#\|@\)\w\+"
syn match   Macro       "\$\w\+"
syn match   Number      "\<\d\+\(\.\d\+\)\?\>"

syn region Typedef start="{" skip="{[^}]*}" end="}" 
syn region Function matchgroup=Keyword start="declare" end="\~"

let b:current_syntax = "biocham"

syn keyword Keyword  add_biocham
syn keyword Keyword  add_event
syn keyword Keyword  add_genCTL
syn keyword Keyword  add_ltl
syn keyword Keyword  add_ode
syn keyword Keyword  add_rule
syn keyword Keyword  add_rules
syn keyword Keyword  add_sbml
syn keyword Keyword  add_search_condition
syn keyword Keyword  add_spec
syn keyword Keyword  add_specs
syn keyword Keyword  add_time_event
syn keyword Keyword  boolean_enumeration
syn keyword Keyword  boolean_simulation
syn keyword Keyword  change_directory
syn keyword Keyword  check_all_spec
syn keyword Keyword  check_checkpoint
syn keyword Keyword  check_conservations
syn keyword Keyword  check_ctl
syn keyword Keyword  check_loop
syn keyword Keyword  check_ltl
syn keyword Keyword  check_ltl_spec
syn keyword Keyword  check_molecules
syn keyword Keyword  check_oscil
syn keyword Keyword  check_reachable
syn keyword Keyword  check_spec
syn keyword Keyword  check_stable
syn keyword Keyword  check_steady
syn keyword Keyword  check_why
syn keyword Keyword  check_why_spec
syn keyword Keyword  clear_ltl
syn keyword Keyword  clear_rules
syn keyword Keyword  clear_spec
syn keyword Keyword  cmaes_init_std
syn keyword Keyword  cmaes_multi_conditions
syn keyword Keyword  cmaes_params
syn keyword Keyword  compose_ode_stochastic
syn keyword Keyword  conservation
syn keyword Keyword  continue
syn keyword Keyword  conversion_factor
syn keyword Keyword  critical_reaction_threshold
syn keyword Keyword  current_directory
syn keyword Keyword  declare
syn keyword Keyword  delete_conservation
syn keyword Keyword  delete_conservations
syn keyword Keyword  delete_declaration
syn keyword Keyword  delete_event
syn keyword Keyword  delete_events
syn keyword Keyword  delete_ltl
syn keyword Keyword  delete_macro
syn keyword Keyword  delete_parameter
syn keyword Keyword  delete_rules
syn keyword Keyword  delete_spec
syn keyword Keyword  delete_specs
syn keyword Keyword  delete_time_event
syn keyword Keyword  domains
syn keyword Keyword  dot
syn keyword Keyword  draw_influences
syn keyword Keyword  draw_neighborhood
syn keyword Keyword  draw_reactions
syn keyword Keyword  dtc
syn keyword Keyword  dump_commands
syn keyword Keyword  event
syn keyword Keyword  expand_biocham
syn keyword Keyword  expand_rules
syn keyword Keyword  export_biocham
syn keyword Keyword  export_biopepa
syn keyword Keyword  export_dot
syn keyword Keyword  export_influences_dot
syn keyword Keyword  export_influences_ginml
syn keyword Keyword  export_init
syn keyword Keyword  export_lotos
syn keyword Keyword  export_neighborhood_dot
syn keyword Keyword  export_nusmv
syn keyword Keyword  export_ode
syn keyword Keyword  export_ode_latex
syn keyword Keyword  export_param
syn keyword Keyword  export_plot
syn keyword Keyword  export_prolog
syn keyword Keyword  export_sbml
syn keyword Keyword  export_slp
syn keyword Keyword  fairness_path
syn keyword Keyword  filtering
syn keyword Keyword  first_search_condition
syn keyword Keyword  fit_x
syn keyword Keyword  fit_xmax
syn keyword Keyword  fit_xmin
syn keyword Keyword  fit_y
syn keyword Keyword  fit_ymax
syn keyword Keyword  fit_ymin
syn keyword Keyword  fss_params
syn keyword Keyword  genCTL
syn keyword Keyword  get_max_from_trace
syn keyword Keyword  get_min_from_trace
syn keyword Keyword  get_period_from_trace
syn keyword Keyword  gsa_morris
syn keyword Keyword  hide_macros
syn keyword Keyword  hide_molecules
syn keyword Keyword  hide_parameters
syn keyword Keyword  keep_plot
syn keyword Keyword  landscape
syn keyword Keyword  landscape_log
syn keyword Keyword  learn_one_addition
syn keyword Keyword  learn_one_deletion
syn keyword Keyword  learn_parameters
syn keyword Keyword  list_all_molecules
syn keyword Keyword  list_conservations
syn keyword Keyword  list_declarations
syn keyword Keyword  list_dimensions
syn keyword Keyword  list_events
syn keyword Keyword  list_functions
syn keyword Keyword  list_influences
syn keyword Keyword  list_initial_state
syn keyword Keyword  list_kinetics
syn keyword Keyword  list_ltl
syn keyword Keyword  list_model
syn keyword Keyword  list_molecules
syn keyword Keyword  list_neighborhood
syn keyword Keyword  list_ODE
syn keyword Keyword  list_rules
syn keyword Keyword  list_spec
syn keyword Keyword  list_volumes
syn keyword Keyword  load_biocham
syn keyword Keyword  load_ode
syn keyword Keyword  load_sbml
syn keyword Keyword  load_trace
syn keyword Keyword  no_fairness_path
syn keyword Keyword  no_filtering
syn keyword Keyword  no_step_doubling
syn keyword Keyword  numerical_method
syn keyword Keyword  numerical_simulation
syn keyword Keyword  nusmv
syn keyword Keyword  nusmv_direct
syn keyword Keyword  nusmv_disable_dynamic_reordering
syn keyword Keyword  nusmv_dynamic_reordering
syn keyword Keyword  nusmv_non_direct
syn keyword Keyword  nusmv_why
syn keyword Keyword  pathway
syn keyword Keyword  plot
syn keyword Keyword  prolog
syn keyword Keyword  quit
syn keyword Keyword  rdelete
syn keyword Keyword  reduce_model
syn keyword Keyword  revise_model
syn keyword Keyword  revise_model_interactive
syn keyword Keyword  rmerge
syn keyword Keyword  robustness
syn keyword Keyword  robustness_log
syn keyword Keyword  rule
syn keyword Keyword  sa_local
syn keyword Keyword  satisfaction_degree
syn keyword Keyword  satisfaction_degree_plot
syn keyword Keyword  satisfaction_degree_plot_log
syn keyword Keyword  sdelete
syn keyword Keyword  search_all_mreductions
syn keyword Keyword  search_all_parameters
syn keyword Keyword  search_all_reductions
syn keyword Keyword  search_conservations
syn keyword Keyword  search_mreduction
syn keyword Keyword  search_parameters
syn keyword Keyword  search_parameters_cmaes
syn keyword Keyword  search_parameters_cmaes_log
syn keyword Keyword  search_parameters_fss
syn keyword Keyword  search_parameters_fss_log
syn keyword Keyword  search_random_all_parameters
syn keyword Keyword  search_random_parameters
syn keyword Keyword  search_reduction
syn keyword Keyword  seed
syn keyword Keyword  set_color
syn keyword Keyword  set_dimension
syn keyword Keyword  set_init_from_trace
syn keyword Keyword  set_png_size
syn keyword Keyword  set_xmax
syn keyword Keyword  set_xmin
syn keyword Keyword  set_ymax
syn keyword Keyword  set_ymin
syn keyword Keyword  show_hide
syn keyword Keyword  show_macros
syn keyword Keyword  show_molecules
syn keyword Keyword  show_parameters
syn keyword Keyword  smerge
syn keyword Keyword  solve
syn keyword Keyword  step_doubling
syn keyword Keyword  step_size
syn keyword Keyword  test_plot
syn keyword Keyword  time_event
syn keyword Keyword  trace_analyze
syn keyword Keyword  trace_check
syn keyword Keyword  volume
syn keyword Keyword  why
