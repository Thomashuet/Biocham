/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2008, INRIA, Projet Contraintes
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
 * BioCommands.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham.commandLine;

import java.io.*;
import java.util.*;
import java.awt.*;

public class BiochamCommands {
   // array of sorted BIOCHAM commands
   static String[] knownCommands = {
      "absent",
      "add_biocham",
      "add_event",
      "add_genCTL",
      "add_ltl",
      "add_ode",
      "add_rule",
      "add_rules",
      "add_sbml",
      "add_search_condition",
      "add_spec",
      "add_specs",
      "add_time_event",
      "boolean_enumeration",
      "boolean_simulation",
      "change_directory",
      "check_all_spec",
      "check_checkpoint",
      "check_conservations",
      "check_ctl",
      "check_loop",
      "check_ltl",
      "check_ltl_spec",
      "check_molecules",
      "check_oscil",
      "check_reachable",
      "check_spec",
      "check_stable",
      "check_steady",
      "check_why",
      "check_why_spec",
      "clear_initial_state",
      "clear_ltl",
      "clear_rules",
      "clear_spec",
      "cmaes_init_std",
      "cmaes_multi_conditions",
      "cmaes_params",
      "compose_ode_stochastic",
      "conservation",
      "continue",
      "conversion_factor",
      "critical_reaction_threshold",
      "current_directory",
      "declare",
      "delete_conservation",
      "delete_conservations",
      "delete_declaration",
      "delete_event",
      "delete_events",
      "delete_ltl",
      "delete_macro",
      "delete_parameter",
      "delete_rules",
      "delete_spec",
      "delete_specs",
      "delete_time_event",
      "domains",
      "dot",
      "draw_influences",
      "draw_neighborhood",
      "draw_reactions",
      "dtc",
      "event",
      "expand_biocham",
      "expand_rules",
      "export_biocham",
      "export_biopepa",
      "export_dot",
      "export_influences_dot",
      "export_influences_ginml",
      "export_init",
      "export_lotos",
      "export_neighborhood_dot",
      "export_nusmv",
      "export_ode",
      "export_ode_latex",
      "export_param",
      "export_plot",
      "export_prolog",
      "export_sbml",
      "export_slp",
      "fairness_path",
      "filtering",
      "first_search_condition",
      "fit_x",
      "fit_xmax",
      "fit_xmin",
      "fit_y",
      "fit_ymax",
      "fit_ymin",
      "fss_params",
      "genCTL",
      "get_max_from_trace",
      "get_min_from_trace",
      "get_period_from_trace",
      "gsa_morris",
      "hide_macros",
      "hide_molecules",
      "hide_parameters",
      "keep_plot",
      "landscape",
      "landscape_log",
      "learn_one_addition",
      "learn_one_deletion",
      "learn_parameters",
      "list_all_molecules",
      "list_conservations",
      "list_declarations",
      "list_dimensions",
      "list_events",
      "list_functions",
      "list_influences",
      "list_initial_state",
      "list_kinetics",
      "list_ltl",
      "list_macros",
      "list_model",
      "list_molecules",
      "list_neighborhood",
      "list_ODE",
      "list_parameters",
      "list_rules",
      "list_spec",
      "list_volumes",
      "load_biocham",
      "load_ode",
      "load_sbml",
      "load_trace",
      "macro",
      "make_absent_not_present",
      "make_present_not_absent",
      "no_fairness_path",
      "no_filtering",
      "no_step_doubling",
      "numerical_method",
      "numerical_simulation",
      "nusmv",
      "nusmv_direct",
      "nusmv_disable_dynamic_reordering",
      "nusmv_dynamic_reordering",
      "nusmv_non_direct",
      "nusmv_why",
      "parameter",
      "pathway",
      "plot",
      "present",
      "prolog",
      "quit",
      "rdelete",
      "reduce_model",
      "revise_model",
      "revise_model_interactive",
      "rmerge",
      "robustness",
      "robustness_log",
      "rule",
      "sa_local",
      "satisfaction_degree",
      "satisfaction_degree_plot",
      "satisfaction_degree_plot_log",
      "sdelete",
      "search_all_mreductions",
      "search_all_parameters",
      "search_all_reductions",
      "search_conservations",
      "search_mreduction",
      "search_parameters",
      "search_parameters_cmaes",
      "search_parameters_cmaes_log",
      "search_parameters_fss",
      "search_parameters_fss_log",
      "search_random_all_parameters",
      "search_random_parameters",
      "search_reduction",
      "seed",
      "set_color",
      "set_dimension",
      "set_init_from_trace",
      "set_png_size",
      "set_xmax",
      "set_xmin",
      "set_ymax",
      "set_ymin",
      "show_hide",
      "show_macros",
      "show_molecules",
      "show_parameters",
      "smerge",
      "solve",
      "step_doubling",
      "step_size",
      "test_plot",
      "time_event",
      "trace_analyze",
      "trace_check",
      "undefined",
      "volume",
      "why",
      "***"
   };

   /** Return longest unambiguous command starting with a given string.*/
   static String getContinuation(String cmd) {
      int l = knownCommands.length - 1; // ignore last dummy command
      for (int i=0;i<l;i++)
         if (knownCommands[i].startsWith(cmd)) {
            return commonPrefix(cmd,i,l);
         }
      Toolkit.getDefaultToolkit().beep();
      return cmd;
   }

   /** Return longest common prefix in commands.
    *
    * Starts at a given position in the array of commands and continue up to 
    * a given last position or when @cmd is not a prefix any more.
    */
   static String commonPrefix(String cmd, int start, int end)
   {
      Vector<String> v = new Vector<String> ();
      
      int i=start;

      while (i<end && knownCommands[i].startsWith(cmd)) {
         v.add(knownCommands[i]);
         i++;
      }

      if (v.size() == 1)
         return v.elementAt(0);
 
      Toolkit.getDefaultToolkit().beep();
      
      StringBuffer s = new StringBuffer(v.elementAt(0));
      for (i=1;i<v.size();++i) {
         getCommonPrefix(s, v.elementAt(i));
      }
      return s.toString();
   }

   /** Update a StringBuffer to longest prefix with a given String.*/
   static void getCommonPrefix(StringBuffer sb, String s)
   {
      int i=0;
      while (i<sb.length() && i<s.length()
            && sb.charAt(i) == s.charAt(i))
         i++;
      sb.setLength(i);
   }
}
