; BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
; Copyright 2003-2006, INRIA, Projet Contraintes
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; bc-mode.el

; TODO indent

;;;;;(defun bc-indent-line ()
;;;;;   "Indent current line as Biocham code"
;;;;;   (interactive)
;;;;;   (beginning-of-line)
;;;;;   (if (bobp)  ; At the start of the file use zero indent.
;;;;;      (indent-line-to 0)
;;;;;   (let ((not-indented t) cur-indent)
;;;;;   (if (looking-at "^[ \t]*for") ; if line starts with 'for' 1 tab
;;;;;      (setq cur-indent default-tab-width)
;;;;;   (if (looking-at "^[ \t]*and") ; if line starts with 'and'
;;;;;      (save-excursion
;;;;;         (forward-line -1)
;;;;;         (beginning-of-line)
;;;;;         (if (looking-at "^[ \t]*where") ; and previous is 'where' -> same
;;;;;            (setq cur-indent (current-indentation))
;;;;;            ; else add 1 tab width
;;;;;            (setq cur-indent (+ (current-indentation) default-tab-width))))
;;;;;   (if (looking-at "^[ \t]*else") ; if line starts with 'else'
;;;;;      (save-excursion
;;;;;         (forward-line -1)
;;;;;         (beginning-of-line)
;;;;;         (if (looking-at "^[ \t]*then") ; and previous is 'then' -> same
;;;;;            (setq cur-indent (current-indentation))
;;;;;            ; else add 1 tab width
;;;;;            (setq cur-indent (+ (current-indentation) default-tab-width))))
;;;;;   (save-excursion
;;;;;      (forward-line -1)
;;;;;      (beginning-of-line)
;;;;;      (if (looking-at "^[ \t]*%") ; previous was a comment -> same
;;;;;         (setq cur-indent (current-indentation))
;;;;;      (if (looking-at "^.*.[ \t]$") ; end of rule/def -> 0
;;;;;         (setq cur-indent 0)
;;;;;         ; else add 1 tab width
;;;;;         (setq cur-indent (+ (current-indentation) default-tab-width)))))))))
;;;;;   (indent-line-to cur-indent)))

; (require 'generic-x)

(defvar bc-mode-hook nil)

(defvar biocham-mode-keywords '(
   "add_biocham"
   "add_event"
   "add_genCTL"
   "add_ltl"
   "add_ode"
   "add_rule"
   "add_rules"
   "add_sbml"
   "add_search_condition"
   "add_spec"
   "add_specs"
   "add_time_event"
   "boolean_enumeration"
   "boolean_simulation"
   "change_directory"
   "check_all_spec"
   "check_checkpoint"
   "check_conservations"
   "check_ctl"
   "check_loop"
   "check_ltl"
   "check_ltl_spec"
   "check_molecules"
   "check_oscil"
   "check_reachable"
   "check_spec"
   "check_stable"
   "check_steady"
   "check_why"
   "check_why_spec"
   "clear_ltl"
   "clear_rules"
   "clear_spec"
   "cmaes_init_std"
   "cmaes_multi_conditions"
   "cmaes_params"
   "compose_ode_stochastic"
   "conservation"
   "continue"
   "conversion_factor"
   "critical_reaction_threshold"
   "current_directory"
   "declare"
   "delete_conservation"
   "delete_conservations"
   "delete_declaration"
   "delete_event"
   "delete_events"
   "delete_ltl"
   "delete_macro"
   "delete_parameter"
   "delete_rules"
   "delete_spec"
   "delete_specs"
   "delete_time_event"
   "domains"
   "dot"
   "draw_influences"
   "draw_neighborhood"
   "draw_reactions"
   "dtc"
   "dump_commands"
   "event"
   "expand_biocham"
   "expand_rules"
   "export_biocham"
   "export_biopepa"
   "export_dot"
   "export_influences_dot"
   "export_influences_ginml"
   "export_init"
   "export_lotos"
   "export_neighborhood_dot"
   "export_nusmv"
   "export_ode"
   "export_ode_latex"
   "export_param"
   "export_plot"
   "export_prolog"
   "export_sbml"
   "export_slp"
   "fairness_path"
   "filtering"
   "first_search_condition"
   "fit_x"
   "fit_xmax"
   "fit_xmin"
   "fit_y"
   "fit_ymax"
   "fit_ymin"
   "fss_params"
   "genCTL"
   "get_max_from_trace"
   "get_min_from_trace"
   "get_period_from_trace"
   "gsa_morris"
   "hide_macros"
   "hide_molecules"
   "hide_parameters"
   "keep_plot"
   "landscape"
   "landscape_log"
   "learn_one_addition"
   "learn_one_deletion"
   "learn_parameters"
   "list_all_molecules"
   "list_conservations"
   "list_declarations"
   "list_dimensions"
   "list_events"
   "list_functions"
   "list_influences"
   "list_initial_state"
   "list_kinetics"
   "list_ltl"
   "list_model"
   "list_molecules"
   "list_neighborhood"
   "list_ODE"
   "list_rules"
   "list_spec"
   "list_volumes"
   "load_biocham"
   "load_ode"
   "load_sbml"
   "load_trace"
   "no_fairness_path"
   "no_filtering"
   "no_step_doubling"
   "numerical_method"
   "numerical_simulation"
   "nusmv"
   "nusmv_direct"
   "nusmv_disable_dynamic_reordering"
   "nusmv_dynamic_reordering"
   "nusmv_non_direct"
   "nusmv_why"
   "pathway"
   "plot"
   "prolog"
   "quit"
   "rdelete"
   "reduce_model"
   "revise_model"
   "revise_model_interactive"
   "rmerge"
   "robustness"
   "robustness_log"
   "rule"
   "sa_local"
   "satisfaction_degree"
   "satisfaction_degree_plot"
   "satisfaction_degree_plot_log"
   "sdelete"
   "search_all_mreductions"
   "search_all_parameters"
   "search_all_reductions"
   "search_conservations"
   "search_mreduction"
   "search_parameters"
   "search_parameters_cmaes"
   "search_parameters_cmaes_log"
   "search_parameters_fss"
   "search_parameters_fss_log"
   "search_random_all_parameters"
   "search_random_parameters"
   "search_reduction"
   "seed"
   "set_color"
   "set_dimension"
   "set_init_from_trace"
   "set_png_size"
   "set_xmax"
   "set_xmin"
   "set_ymax"
   "set_ymin"
   "show_hide"
   "show_macros"
   "show_molecules"
   "show_parameters"
   "smerge"
   "solve"
   "step_doubling"
   "step_size"
   "test_plot"
   "time_event"
   "trace_analyze"
   "trace_check"
   "volume"
   "why"
      ("%.*" . font-lock-comment-face)
      ("\\<\\(where\\|in\\|and\\|parts_of\\|not\\|for\\|if\\|then\\|else\\)\\>"
         . font-lock-keyword-face)
      ("\\<\\(present\\|absent\\|undefined\\|parameter\\|macro\\|list_\\(parameter\\|macro\\)s\\|make\\(pre\\|ab\\)sent_not_\\(pre\\|ab\\)sent\\|\\(clear_\\)\\?initial_state\\)\\>"
         . font-lock-builtin-face)
      ("\\<[0-9.]+\\>" . font-lock-constant-face)
   ))

(defvar bc-mode-map
   (let 
      ((bc-mode-map (make-sparse-keymap)))
      ; (define-key bc-mode-map "\C-j" 'newline-and-indent)
      bc-mode-map)
   "Keymap for Biocham mode")

(defvar bc-mode-syntax-table
   (let ((bc-mode-syntax-table (make-syntax-table)))
      ; _ is a word constituent
      (modify-syntax-entry ?_ "w" bc-mode-syntax-table)
      bc-mode-syntax-table)
  "Syntax table for Biocham mode")

(defun bc-mode ()
   "Major mode for editing Biocham files."
   (interactive)
   (kill-all-local-variables)
   (use-local-map bc-mode-map)
   (set-syntax-table bc-mode-syntax-table)
   (set (make-local-variable 'comment-start) "% ")
   (set (make-local-variable 'font-lock-defaults) '(biocham-mode-keywords))
   (setq major-mode 'bc-mode)
   (setq mode-name "Biocham Mode")
   (run-hooks 'bc-mode-hook))

(provide 'bc-mode)
