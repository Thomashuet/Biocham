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
% GNU prolog file biosyntax.pl
% by Nathalie Chabrier-Rivier, Fran�ois Fages, Daniel de Rauglaudre and Sylvain Soliman

% $Id: biosyntax.pl,v 1.153 2009-02-27 21:11:38 soliman Exp $

read_biosyntax(Stream, T1) :-
   g_read(g_biosyntax_initialized, 1),
   (Stream == user_input -> P = 0 ; character_count(Stream, P)),
   llist_alloc(next_char_pred, Stream, ChrL),
   llist_alloc(next_token_pred, ChrL, TokL),
   catch(
      (
         llist_hd_tl(TokL, tok(ta_loc(BP, EP), some(_)), _) ->
            (
               parsing_call(bs_biocham(T, TokL, _), 2000, call) ->
                  true
               ;
                  throw(exc_loc(BP, EP, parsing_error('')))
            )
         ;
            T = end_of_file
      ),
      E,
      bios_error(Stream, P, E, ChrL, TokL, T)
   ),
   (
      g_read(g_syntax_only, 0) ->
         T1 = T
      ;
         write('\nterm = '),
         write_term(T, [quoted(true), portrayed(true)]),
         nl,
         (
            T = end_of_file ->
               halt(0)
            ;
               !, read_biosyntax(Stream, T1)
         )
   ).

read_biosyntax(Stream, T) :-
   g_read(g_biosyntax_initialized, 0),
   g_assign(g_biosyntax_initialized, 1),
   (environ('SYNTAX_ONLY', '1') -> g_assign(g_syntax_only, 1) ; true), 
   !,
   catch(biosyntax_init, S, bs_init_error(S)),
   read_biosyntax(Stream, T).

next_token_pred(_, Tok, ChrL1, ChrL2) :-
   biocham_next_token(T, ChrL1, ChrL2),
   Tok = T.
next_token_pred(_, _, ChrL1, ChrL2) :-
   biocham_skip_spaces(ChrL1, ChrL2),
   (
      llist_hd_tl(ChrL2, chr(BP, _), _) ->
         EP is BP + 1,
         throw(exc_loc(BP, EP, lexing_error))
   ).

next_char_pred(P, eof(P), Stream, Stream) :-
   at_end_of_stream(Stream), !.
next_char_pred(P, R, Stream, Stream) :-
   get_code(Stream, C),
   (C = -1 -> R = eof(P) ; R = chr(P, C)).

%
% Initialization of BIOCHAM parser and lexer
%

biosyntax_init :-
   biosyntax_init_lexer, !,
   biosyntax_init_parser, !.

% Parsing...

biosyntax_init_parser :-
% bs_biocham
   parsing_create_entry(bs_biocham, call),
   bc_op(bs_biocham, 0, pf, [bs_command-X, '.'], X),
   bc_op(bs_biocham, 0, pf, [bs_reaction-X, '.'], X),
   
% bs_set_of_names
   parsing_create_entry(bs_set_of_names, call),
   bc_op(bs_set_of_names, 1000, xfy, [X, ',', Y], (X, Y)),
   bc_op(bs_set_of_names, 0, t, [name(X)], X),
% bs_set_of_set_of_names
   parsing_create_entry(bs_set_of_set_of_names, call),
   bc_op(bs_set_of_set_of_names, 0, ff, ['{','}'], {}),
   bc_op(bs_set_of_set_of_names, 0, fpf, ['{',bs_set_of_names-X,'}'], {X}),
   bc_op(bs_set_of_set_of_names, 1000, xfy, [X, ',', Y], (X, Y)),
% bs_object
   parsing_create_entry(bs_object, call),
   bc_op(bs_object, 0, p, [bs_molecule-X], X),
   bc_op(bs_object, 0, p, [bs_abstract-X], X),
   bc_op(bs_object, 0, pft, [bs_molecule-X, '::', name(Y)], '::'(X, Y)),
   bc_op(bs_object, 0, pfp, [bs_molecule-X, '::', bs_variable-Y], '::'(X, Y)),
% bs_set_of_objects
   parsing_create_entry(bs_set_of_objects, call),
   bc_op(bs_set_of_objects, 0, f, ['_'], '_'),
   bc_op(bs_set_of_objects, 0, p, [bs_object-X], X),
   bc_op(bs_set_of_objects, 1000, xfy, [X, ',', Y], (X, Y)),
% bs_set_of_set_of_objects
   parsing_create_entry(bs_set_of_set_of_objects, call),
   bc_op(bs_set_of_set_of_objects, 0, ff, ['{','}'], {}),
   bc_op(bs_set_of_set_of_objects, 0, fpf, ['{',bs_set_of_objects-X,'}'], {X}),
   bc_op(bs_set_of_set_of_objects, 1000, xfy, [X, ',', Y], (X, Y)),
% bs_molecule
   parsing_create_entry(bs_molecule, call),
   bc_op(bs_molecule, 0, fzf, ['(', X, ')'], X),
   bc_op(bs_molecule, 0, t, [name(X)], X),
   bc_op(bs_molecule, 0, p, [bs_variable-X], X),
   bc_op(bs_molecule, 0, p, [bs_gene-X], X),
   bc_op(bs_molecule, 490, yfx, [X, '-', Y], (X - Y)),
   bc_op(bs_molecule, 400, yffpf,
         [X, '~', '{', bs_set_of_molecules-Y, '}'], '~'(X, {Y})),
   bc_op(bs_molecule, 400, yfp, [X, '~', bs_variable-Y], '~'(X, Y)),
% bs_set_of_molecules
   parsing_create_entry(bs_set_of_molecules, call),
   bc_op(bs_set_of_molecules, 1000, xfy, [X, ',', Y], (X, Y)),
   bc_op(bs_set_of_molecules, 0, p, [bs_molecule-X], X),
% bs_variable
   parsing_create_entry(bs_variable, call),
   bc_op(bs_variable, 0, f, ['?'], ('?')),
   bc_op(bs_variable, 0, ft, ['$', name(X)], '$'(X)),
% bs_gene
   parsing_create_entry(bs_gene, call),
   bc_op(bs_gene, 0, ft, ['#', name(X)], '#'(X)),
% bs_abstract
   parsing_create_entry(bs_abstract, call),
   bc_op(bs_abstract, 0, ft, ['@', name(X)], '@'(X)),
% bs_reaction
   parsing_create_entry(bs_reaction, call),
   bc_op(bs_reaction, 1001, tfx,
         [name(X), ':', Y], ':'(X, Y)),
   bc_op(bs_reaction, 999, pfx,
         [bs_kinetics-X, 'for', Y], 'for'(X, Y)),
   bc_op(bs_reaction, 998, xfp,
         [X, 'where', bs_constraints-Y], 'where'(X, Y)),
   bc_op(bs_reaction, 0, f, ['complexation'],
	 'complexation'),
   bc_op(bs_reaction, 1, ffpf, ['complexation','(',bs_object-Y,')'],
	 'complexation'(Y)),
   bc_op(bs_reaction, 0, f, ['decomplexation'],
	 'decomplexation'),
   bc_op(bs_reaction, 1, ffpf, ['decomplexation','(',bs_object-Y,')'],
	 'decomplexation'(Y)),
   bc_op(bs_reaction, 0, f, ['re_complexation'],
  	 're_complexation'),
   bc_op(bs_reaction, 1, ffpf, ['re_complexation','(',bs_object-Y,')'],
  	 're_complexation'(Y)),
   bc_op(bs_reaction, 0, f, ['phosphorylation'],
	 'phosphorylation'),
   bc_op(bs_reaction, 1, ffpf, ['phosphorylation','(',bs_object-Y,')'],
	 'phosphorylation'(Y)),
   bc_op(bs_reaction, 0, f, ['dephosphorylation'],
	 'dephosphorylation'),
   bc_op(bs_reaction, 1, ffpf, ['dephosphorylation','(',bs_object-Y,')'],
	 'dephosphorylation'(Y)),
   bc_op(bs_reaction, 0, f, ['re_phosphorylation'],
  	 're_phosphorylation'),
   bc_op(bs_reaction, 1, ffpf, ['re_phosphorylation','(',bs_object-Y,')'],
  	 're_phosphorylation'(Y)),
   bc_op(bs_reaction, 0, f, ['synthesis'],
	 'synthesis'),
   bc_op(bs_reaction, 1, ffpf, ['synthesis','(',bs_object-Y,')'],
	 'synthesis'(Y)),
   bc_op(bs_reaction, 0, f, ['degradation'],
	 'degradation'),
   bc_op(bs_reaction, 1, ffpf, ['degradation','(',bs_object-Y,')'],
	 'degradation'(Y)),
%    bc_op(bs_reaction, 0, f, ['complexation_phosphorylation'],
% 	 'complexation_phosphorylation'),
%    bc_op(bs_reaction, 0, f, ['complexation_dephosphorylation'],
% 	 'complexation_dephosphorylation'),
%    bc_op(bs_reaction, 0, f, ['decomplexation_phosphorylation'],
% 	 'decomplexation_phosphorylation'),
%    bc_op(bs_reaction, 0, f, ['decomplexation_dephosphorylation'],
% 	 'decomplexation_dephosphorylation'),
%    bc_op(bs_reaction, 0, f, ['re_complexation_phosphorylation'],
% 	 're_complexation_phosphorylation'),
%    bc_op(bs_reaction, 0, f, ['re_complexation_dephosphorylation'],
% 	 're_complexation_dephosphorylation'),
%    bc_op(bs_reaction, 0, f, ['re_complexation_de_phosphorylation'],
% 	 're_complexation_de_phosphorylation'),
%    bc_op(bs_reaction, 1, ffpf, ['re_complexation_de_phosphorylation','(',bs_object-Y,')'],
% 	 're_complexation_de_phosphorylation'(Y)),
   bc_op(bs_reaction, 0, f, ['elementary_interaction_rules'],
	 'elementary_interaction_rules'),
   bc_op(bs_reaction, 1, ffpf, ['elementary_interaction_rules','(',bs_object-Y,')'],
	 elementary_interaction_rules(Y)),
%    bc_op(bs_reaction, 0, f, ['other_elementary_interaction_rules'],
% 	 'other_elementary_interaction_rules'),
%    bc_op(bs_reaction, 1, ffpf, ['other_elementary_interaction_rules','(',bs_object-Y,')'],
% 	 'other_elementary_interaction_rules'(Y)),
%    bc_op(bs_reaction, 0, f, ['elementary_interaction_rules_more'],
% 	 'elementary_interaction_rules_more'),
%    bc_op(bs_reaction, 1, ffpf, ['elementary_interaction_rules_more','(',bs_object-Y,')'],
% 	 'elementary_interaction_rules_more'(Y)),
   bc_op(bs_reaction, 0, f, ['more_elementary_interaction_rules'],
	 'more_elementary_interaction_rules'),
   bc_op(bs_reaction, 1, ffpf, ['more_elementary_interaction_rules','(',bs_object-Y,')'],
	 'more_elementary_interaction_rules'(Y)),
   bc_op(bs_reaction, 0, pffpfpffp,
         [bs_solution-X, '=', '[', bs_solution-Y, '=>', bs_solution-Z,
          ']', '=>', bs_solution-T],
         '=>'(X = ['=>'(Y, Z)], T)),
   bc_op(bs_reaction, 0, pffpffp,
         [bs_solution-X, '<=', '[', bs_object-Y, ']', '=>',
          bs_solution-Z],
         '=>'('<='(X, [Y]), Z)),
   bc_op(bs_reaction, 0, pfp,
         [bs_solution-X, '<=>', bs_solution-Y],
         '<=>'(X, Y)),
   bc_op(bs_reaction, 0, pffpffp,
         [bs_solution-X, '=', '[', bs_object-Y, ']', '=>', bs_solution-Z],
         '=>'(X = [Y], Z)),
   bc_op(bs_reaction, 0, pfp,
         [bs_solution-X, '=>', bs_solution-Y],
         '=>'(X, Y)),
% bs_set_of_reactions
   parsing_create_entry(bs_set_of_reactions, call),
   bc_op(bs_set_of_reactions, 0, p, [bs_reaction-X], X),
   bc_op(bs_set_of_reactions, 1000, xfy, [X, ',', Y], (X, Y)),
% bs_solution
   parsing_create_entry(bs_solution, call),
   bc_op(bs_solution, 0, f, ['_'], '_'),
   bc_op(bs_solution, 0, p, [bs_variable-X], X),
   bc_op(bs_solution, 0, p, [bs_object-X], X),
   bc_op(bs_solution, 400, pfp, [bs_num-X, '*', bs_object-Y], X * Y),
   bc_op(bs_solution, 500, yfx, [X, '+', Y], X + Y),
   bc_op(bs_solution, 0, fzf, ['(', X, ')'], X),
% bs_constraints
	parsing_create_entry(bs_constraints, call),
	bc_op(bs_constraints, 700, pff,
	      [bs_object-X, 'in', 'all'],
	      'in'(X, all)),
	bc_op(bs_constraints, 700, pff,
	      [bs_object-X, 'in', 'all_simple'],
	      'in'(X, all_simple)),
	bc_op(bs_constraints, 700, pfp,
	      [bs_variable-X, 'diff', bs_object-Y],
	      'diff'(X, Y)),
	bc_op(bs_constraints, 700, pfp,
	      [bs_variable-X, 'phos_form', bs_object-Y],
	      'phos_form'(X, Y)),
	bc_op(bs_constraints, 700, pffp,
	      [bs_variable-X, 'not', 'phos_form', bs_object-Y],
	      'phos_form'('not'(X), Y)),
	bc_op(bs_constraints, 700, pfp,
	      [bs_variable-X, 'more_phos_than', bs_object-Y],
	      'more_phos_than'(X, Y)),
	bc_op(bs_constraints, 700, pffp,
	      [bs_variable-X, 'not', 'more_phos_than', bs_object-Y],
	      'more_phos_than'('not'(X), Y)),
	bc_op(bs_constraints, 700, pfp,
	      [bs_variable-X, 'sub_mol', bs_object-Y],
	      'sub_mol'(X, Y)),
	bc_op(bs_constraints, 700, pffp,
	      [bs_variable-X, 'not', 'sub_mol', bs_object-Y],
	      'sub_mol'('not'(X), Y)),
   bc_op(bs_constraints, 700, pfp,
	      [bs_variable-X, 'has_simple_mol_in_common', bs_object-Y],
	      'has_simple_mol_in_common'(X, Y)),
	bc_op(bs_constraints, 700, pfp,
	      [bs_variable-X, 'has_no_simple_mol_in_common', bs_object-Y],
	      'has_no_simple_mol_in_common'(X, Y)),
	bc_op(bs_constraints, 700, pffpf,
	      [bs_object-X, 'in', '{', bs_set_of_objects-Y, '}'],
	      'in'(X, {Y})),
	bc_op(bs_constraints, 700, pffffpff,
	      [bs_object-X, 'in','parts_of','(', '{', bs_set_of_names-Y, '}',')'],
	      'in'(X, 'parts_of'({Y}))),
	bc_op(bs_constraints, 700, pffpf,
	      [bs_object-X, 'in', '{', bs_set_of_set_of_names-Y, '}'],
	      'in'(X, {Y})),
	bc_op(bs_constraints, 700, pfffpf,
	      [bs_object-X, 'not', 'in', '{', bs_set_of_objects-Y, '}'],
	      'in'('not'(X), {Y})),
	bc_op(bs_constraints, 700, pfffpf,
	      [bs_object-X, 'not', 'in', '{', bs_set_of_set_of_names-Y, '}'],
	      'in'('not'(X), {Y})),
	bc_op(bs_constraints, 700, pfffffpff,
	      [bs_object-X, 'not','in','parts_of','(', '{', bs_set_of_objects-Y, '}',')'],
	      'in'('not'(X), 'parts_of'({Y}))),
	bc_op(bs_constraints, 700, pffft,
	      [bs_object-X, 'not', 'in', '$', name(Y)],
	      'in'('not'(X), '$'(Y))),
	bc_op(bs_constraints, 800, xfy, [X, and, Y], 'and'(X, Y)),
% bs_kinetics
   parsing_create_entry(bs_kinetics, call),
   bc_op(bs_kinetics, 0, fpf, ['[', bs_object-X, ']'], [X]),
   bc_op(bs_kinetics, 0, t, [float(X)], X),
   bc_op(bs_kinetics, 0, t, [int(X)], X),
   bc_op(bs_kinetics, 0, t, [name(X)], X),
% FF
%  bc_op(bs_kinetics, 0, fftf, ['MA','(',name(X),')'], 'MA'(X)),
   bc_op(bs_kinetics, 0, ffpf, ['MA','(',bs_kinetics-X,')'], 'MA'(X)),
%  bc_op(bs_kinetics, 0, ffpftf, ['MM','(',bs_kinetics-X,',',name(Y),')'], 'MM'(X,Y)),
   bc_op(bs_kinetics, 0, ffpfpf, ['MM','(',bs_kinetics-X,',',bs_kinetics-Y,')'], 'MM'(X,Y)),
% FF Hill for one reactant, real number exponent allowed
   bc_op(bs_kinetics, 0, ffpfpfpf, ['H','(',bs_kinetics-X,',', bs_kinetics-Y,',', bs_kinetics-Z,')'], 'H'(X,Y,Z)),
%   bc_op(bs_kinetics, 0, ffpfpftf, ['H','(',bs_kinetics-X,',', bs_kinetics-Y,',', int(Z),')'], 'H'(X,Y,Z)),
%   bc_op(bs_kinetics, 0, ffpfpftf, ['HN','(',bs_kinetics-X,',', bs_kinetics-Y, ',', int(Z),')'], 'HN'(X,Y,Z)),
   bc_op(bs_kinetics, 400, yfx, [X, '*', Y], X * Y),
   bc_op(bs_kinetics, 400, yfx, [X, '/', Y], X / Y),
   bc_op(bs_kinetics, 500, yfx, [X, '+', Y], X + Y),
   bc_op(bs_kinetics, 500, yfx, [X, '-', Y], X - Y),
   bc_op(bs_kinetics, 200, xfy, [X, '^', Y], X ^ Y),
   bc_op(bs_kinetics, 200, fx, ['-', X], - X),
   bc_op(bs_kinetics, 0, ffpfpf, ['min','(',bs_kinetics-X,',',bs_kinetics-Y,')'], 'min'(X,Y)),
   bc_op(bs_kinetics, 0, ffpfpf, ['max','(',bs_kinetics-X,',',bs_kinetics-Y,')'], 'max'(X,Y)),
   bc_op(bs_kinetics, 50, ffzf, [log, '(', X, ')'], log(X)),
   bc_op(bs_kinetics, 50, ffzf, [exp, '(', X, ')'], exp(X)),
   bc_op(bs_kinetics, 50, ffzf, [sin, '(', X, ')'], sin(X)),
   bc_op(bs_kinetics, 50, ffzf, [cos, '(', X, ')'], cos(X)),
   bc_op(bs_kinetics, 50, ffzf, [abs, '(', X, ')'], abs(X)),
   bc_op(bs_kinetics, 50, ffzf, [frac, '(', X, ')'], frac(X)),
   bc_op(bs_kinetics, 0, fzfzf, ['(', X, ',', Y, ')'], (X, Y)),
   bc_op(bs_kinetics, 0, fffpffff, ['d', '(', '[', bs_molecule-X, ']', ')',
      '/', 'dt'], '/'('d'([X]),'dt')),
   bc_op(bs_kinetics, 0, fzf, ['(', X, ')'], X),
   bc_op(bs_kinetics, 890, fpfyfy, ['if', bs_condition-X, 'then',
      Y, 'else', Z], if(then(X, else(Y, Z)))),
% bs_condition
   parsing_create_entry(bs_condition, call),
   bc_op(bs_condition, 700, pfp, [bs_kinetics-X, '<', bs_kinetics-Y],
      '<'(X, Y)),
   bc_op(bs_condition, 700, pfp, [bs_kinetics-X, '>', bs_kinetics-Y],
      '>'(X, Y)),
   bc_op(bs_condition, 700, pfp, [bs_kinetics-X, '=', bs_kinetics-Y],
      '='(X, Y)),
%   bc_op(bs_condition, 700, pfp, [bs_kinetics-X, '<=', bs_kinetics-Y], %FF not added
%      '=<'(X, Y)),
   bc_op(bs_condition, 700, pfp, [bs_kinetics-X, '=<', bs_kinetics-Y],
      '=<'(X, Y)),
   bc_op(bs_condition, 700, pfp, [bs_kinetics-X, '>=', bs_kinetics-Y],
      '>='(X, Y)),
   bc_op(bs_condition, 800, xfy, [X, 'and', Y], 'and'(X, Y)),
%FF don't know how to write it:
   % bc_op(bs_condition, 0, t, ['true'], true), 
% bs_biocham_query
   parsing_create_entry(bs_biocham_query, call),
   bc_op(bs_biocham_query, 0, ffpf, ['Ai', '(', bs_ctl_query-X, ')'],
         'Ai'(X)),
   bc_op(bs_biocham_query, 0, ffpf, ['Ei', '(', bs_ctl_query-X, ')'],
         'Ei'(X)),
% bs_set_of_biocham_queries
   parsing_create_entry(bs_set_of_biocham_queries, call),
   bc_op(bs_set_of_biocham_queries, 0, p, [bs_biocham_query-X], X),
   bc_op(bs_set_of_biocham_queries, 1000, xfy, [X, ',', Y], (X, Y)),
% bs_ctl_query
   parsing_create_entry(bs_ctl_query, call),
   bc_op(bs_ctl_query, 0, p, [bs_object-X], X),
   bc_op(bs_ctl_query, 0, fzf, ['(', X, ')'], X),
   bc_op(bs_ctl_query, 0, ffzf, ['EF', '(', X, ')'], 'EF'(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['AF', '(', X, ')'], 'AF'(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['EG', '(', X, ')'], 'EG'(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['AG', '(', X, ')'], 'AG'(X)),
   bc_op(bs_ctl_query, 0, ffzfzf, ['E', '(', X, 'U', Y, ')'], 'E'('U'(X, Y))),
   bc_op(bs_ctl_query, 0, ffzfzf, ['A', '(', X, 'U', Y, ')'], 'A'('U'(X, Y))),
% FF
   bc_op(bs_ctl_query, 0, ffzfzf, ['E', '(', X, 'W', Y, ')'], 'E'('W'(X, Y))),
   bc_op(bs_ctl_query, 0, ffzfzf, ['A', '(', X, 'W', Y, ')'], 'A'('W'(X, Y))),
   bc_op(bs_ctl_query, 0, ffzf, ['EX', '(', X, ')'], 'EX'(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['AX', '(', X, ')'], 'AX'(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['!', '(', X, ')'], '!'(X)),
% FF
%   bc_op(bs_ctl_query, 0, ffzf, ['reachable', '(', X, ')'], 'EF'(X)),
%   bc_op(bs_ctl_query, 0, ffzf, ['stable', '(', X, ')'], 'AG'(X)),
%   bc_op(bs_ctl_query, 0, ffzf, ['steady', '(', X, ')'], 'EG'(X)),
%   bc_op(bs_ctl_query, 0, ffzfzf, ['checkpoint', '(', X, ',', Y, ')'],
%         '!'('E'('U'('!'(X),Y)))),
%   bc_op(bs_ctl_query, 0, ffzfzf, ['loop', '(', X, ',', Y, ')'],
%         'AG'('&'('->'(X,'EF'(Y)),'->'(Y,'EF'(X))))),
%   bc_op(bs_ctl_query, 0, ffzf, ['oscil', '(', X, ')'],
%         'AG'('&'('->'(X,'EF'('!'(X))),'->'('!'(X),'EF'(X))))),
   bc_op(bs_ctl_query, 0, ffzf, ['reachable', '(', X, ')'], reachable(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['stable', '(', X, ')'], stable(X)),
   bc_op(bs_ctl_query, 0, ffzf, ['steady', '(', X, ')'], steady(X)),
   bc_op(bs_ctl_query, 0, ffzfzf, ['checkpoint', '(', X, ',', Y, ')'],checkpoint(X,Y)),
   bc_op(bs_ctl_query, 0, ffzfzf, ['loop', '(', X, ',', Y, ')'],loop(X,Y)),
   bc_op(bs_ctl_query, 0, ffzf, ['oscil', '(', X, ')'],oscil(X)),
   bc_op(bs_ctl_query, 500, yfx, [X, '&', Y], '&'(X, Y)),
   bc_op(bs_ctl_query, 500, yfx, [X, '|', Y], '|'(X, Y)),
   bc_op(bs_ctl_query, 500, yfx, [X, 'xor', Y], 'xor'(X, Y)),
   bc_op(bs_ctl_query, 500, yfx, [X, '->', Y], '->'(X, Y)),
   bc_op(bs_ctl_query, 500, yfx, [X, '<->', Y], '<->'(X, Y)),
% bs_ltl_query
   parsing_create_entry(bs_ltl_query, call),
   bc_op(bs_ltl_query, 0, p, [bs_condition-X], X),
   bc_op(bs_ltl_query, 0, fzf, ['(', X, ')'], X),
   bc_op(bs_ltl_query, 0, ffpftf, ['oscil', '(', bs_molecule-X, ',', int(N), ')'], 'oscil'(X,N)),
   bc_op(bs_ltl_query, 0, ffpftfpf, ['oscil', '(', bs_molecule-X, ',', int(N), ',', bs_num-V, ')'], 'oscil'(X,N,V)), % FF
   bc_op(bs_ltl_query, 0, ffpfpf, ['period', '(', bs_molecule-X, ',',
      bs_num-P, ')'], 'period'(X,P)),
   bc_op(bs_ltl_query, 0, ffpfpfpf, ['phase_shift', '(', bs_molecule-X, ',',
      bs_molecule-Y, ',', bs_num-N, ')'], 'phase_shift'(X,Y,N)),
   bc_op(bs_ltl_query, 0, ffpffpfffpff, ['curve_fit', '(',
					    bs_molecule-M,  ',',
					   '[', bs_list_of_nums-T, ']', ',',
					   '[', bs_list_of_names-V, ']', ')'], 'curve_fit'(M,T,V)),
   bc_op(bs_ltl_query, 0, ffpffpfffpfffpff, ['curve_fit_error', '(',
					    bs_molecule-M,',',
					   '[', bs_list_of_nums-Va, ']', ',',
					   '[', bs_list_of_nums-T, ']', ',',
					   '[', bs_list_of_names-V, ']',')'], 'curve_fit_error'(M,Va,T,V)),
   bc_op(bs_ltl_query, 0, ffpfpftf, ['cross', '(', bs_molecule-X, ',',
	 bs_molecule-Y, ',', int(N), ')'], 'cross'(X,Y,N)),
   bc_op(bs_ltl_query, 0, ffzf, ['F', '(', X, ')'], 'F'(X)),
   bc_op(bs_ltl_query, 0, ffzf, ['G', '(', X, ')'], 'G'(X)),
   bc_op(bs_ltl_query, 0, ffzf, ['X', '(', X, ')'], 'X'(X)),
   bc_op(bs_ltl_query, 0, fzfffzf, ['(', X, ')', 'U', '(', Y, ')'], 'U'(X, Y)),
   bc_op(bs_ltl_query, 0, ffzf, ['!', '(', X, ')'], '!'(X)),
   bc_op(bs_ltl_query, 500, yfx, [X, '&', Y], '&'(X, Y)),
   bc_op(bs_ltl_query, 500, yfx, [X, '|', Y], '|'(X, Y)),
   bc_op(bs_ltl_query, 500, yfx, [X, 'xor', Y], 'xor'(X, Y)),
   bc_op(bs_ltl_query, 500, yfx, [X, '->', Y], '->'(X, Y)),
   bc_op(bs_ltl_query, 500, yfx, [X, '<->', Y], '<->'(X, Y)),
% bs_set_of_ltl_queries
   parsing_create_entry(bs_set_of_ltl_queries, call),
   bc_op(bs_set_of_ltl_queries, 0, p, [bs_ltl_query-X], X),
   bc_op(bs_set_of_ltl_queries, 1000, xfy, [X, ',', Y], (X, Y)),
% bs_name
   parsing_create_entry(bs_name, call),
   bc_op(bs_name, 0, t, [name(X)], call_act(act_loc(name-X))),
% bs_command
   parsing_create_entry(bs_command, call),
   bc_op(bs_command, 0, pp, [bs_name-X, command_params-(X-Y)], Y),

   % commands kept here because semantic actions do not match syntax...
%   bc_op(bs_command, 0, ffpf, [change_directory, '(', bs_filename-N, ')'], change_dir(N)),
%   bc_op(bs_command, 0, ffpf, [export_biocham, '(', bs_filename-N, ')'],
%	 export_biocham(N,'no')),
%   bc_op(bs_command, 0, ffpf, [expand_biocham, '(', bs_filename-N, ')'],
%	 export_biocham(N,'yes')),
   %bc_op(bs_command, 0, ffpf, [delete_rule, '(', bs_reaction-X, ')'],
   % delete_rules(X)),
   %bc_op(bs_command, 0, fffpff,
   % [delete_rule, '(', '{', bs_set_of_reactions-X, '}', ')'],
   % delete_rules({X})),
   bc_op(bs_command, 0, fp, [declare, bs_molecule_declaration-X],
	 declare(X)),
   %bc_op(bs_command, 0, ffpf, [delete_spec, '(', bs_biocham_query-X, ')'],
   %      delete_specs(X)),
   %bc_op(bs_command, 0, ffpftfpf, [add_event, '(', bs_condition-X, ',', 
   %      name(Y), ',', bs_kinetics-Z, ')'],
   %      add_event(X,Y,Z)),
   bc_op(bs_command, 0, fftffftfpftfpff,
      [macro, '(', name(X), ',', sq_wave, '(', name(V1), ',', bs_num-D1, ',',
      name(V2), ',', bs_num-D2, ')', ')'], macro(X, sq_wave(V1,D1,V2,D2))),
   bc_op(bs_command, 0, fftffftfpftfpftfpff,
      [macro, '(', name(X), ',', sq_wave, '(', name(V0), ',', bs_num-D0, ',',
         name(V1), ',', bs_num-D1, ',',
         name(V2), ',', bs_num-D2, ')', ')'],
         macro(X, sq_wave(V0, D0, V1, D1, V2, D2))),
   bc_op(bs_command, 0, fftfpf,
      [set_dimension, '(', name(X), ',', bs_int-Y, ')'],
      set_dimension(X, Y)),

% bs_filename
   parsing_create_entry(bs_filename, call),
   bc_op(bs_filename, 0, t, [quote(X)], X),
   bc_op(bs_filename, 0, t, [name(X)], X),
% bs_savename
   parsing_create_entry(bs_savename, call),
   bc_op(bs_savename, 0, t, [quote(X)], X),
   bc_op(bs_savename, 0, t, [name(X)], X),
% bs_int
   parsing_create_entry(bs_int, call),
   bc_op(bs_int, 0, t, [int(X)], X),
   bc_op(bs_int, 0, ft, ['-', int(X)], -X),
% bs_num
   parsing_create_entry(bs_num, call),
   bc_op(bs_num, 0, t, [int(X)], X),
   bc_op(bs_num, 0, t, [float(X)], X),
% bs_list_of_options
   parsing_create_entry(bs_list_of_options, call),
   bc_op(bs_list_of_options, 0, '', [], []),
   bc_op(bs_list_of_options, 0, pp, [bs_option-X, bs_kont_list-Y],
         [X | Y]),
   parsing_create_entry(bs_kont_list, call),
   bc_op(bs_kont_list, 0, '', [], []),
   bc_op(bs_kont_list, 0, fpp, [',', bs_option-X, bs_kont_list-Y],
         [X | Y]),
   parsing_create_entry(bs_option, call),
   bc_op(bs_option, 0, f, [init_up], init_up),
   bc_op(bs_option, 0, f, [mod_double], mod_double),
   bc_op(bs_option, 0, f, [col_path], col_path),
   bc_op(bs_option, 0, f, [double_size], double_size),
   bc_op(bs_option, 0, f, [state], state),
% bs_list_of_intervals
   parsing_create_entry(bs_list_of_intervals, call),
   bc_op(bs_list_of_intervals, 0, '', [], []),
   bc_op(bs_list_of_intervals, 0, fpfpfp, ['(', bs_num-X, ',', bs_num-Y, ')',
      bs_kont_list_of_intervals-Z], [(X, Y) | Z]),
   parsing_create_entry(bs_kont_list_of_intervals, call),
   bc_op(bs_kont_list_of_intervals, 0, '', [], []),
   bc_op(bs_kont_list_of_intervals, 0, ffpfpfp, [',', '(', bs_num-X, ',',
      bs_num-Y, ')', bs_kont_list_of_intervals-Z], [(X, Y) | Z]),
% bs_list_of_ppairs
   parsing_create_entry(bs_list_of_ppairs, call),
   bc_op(bs_list_of_ppairs, 0, '', [], []),
   bc_op(bs_list_of_ppairs, 0, ftftfp, ['(', name(X), ',', name(Y), ')',
      bs_kont_list_of_ppairs-Z], [(X, Y) | Z]),
   parsing_create_entry(bs_kont_list_of_ppairs, call),
   bc_op(bs_kont_list_of_ppairs, 0, '', [], []),
   bc_op(bs_kont_list_of_ppairs, 0, fftftfp, [',', '(', name(X), ',',
      name(Y), ')', bs_kont_list_of_ppairs-Z], [(X, Y) | Z]),
% bs_list_of_names
   parsing_create_entry(bs_list_of_names, call),
   bc_op(bs_list_of_names, 0, '', [], []),
   bc_op(bs_list_of_names, 0, tp, [name(X), bs_kont_list_of_names-Y], [X | Y]),
   parsing_create_entry(bs_kont_list_of_names, call),
   bc_op(bs_kont_list_of_names, 0, '', [], []),
   bc_op(bs_kont_list_of_names, 0, ftp, [',', name(X), bs_kont_list_of_names-Y],
      [X | Y]),
% bs_list_of_ints
   parsing_create_entry(bs_list_of_ints, call),
   bc_op(bs_list_of_ints, 0, '', [], []),
   bc_op(bs_list_of_ints, 0, tp, [int(X), bs_kont_list_of_ints-Y], [X | Y]),
   parsing_create_entry(bs_kont_list_of_ints, call),
   bc_op(bs_kont_list_of_ints, 0, '', [], []),
   bc_op(bs_kont_list_of_ints, 0, ftp, [',', int(X), bs_kont_list_of_ints-Y],
      [X | Y]),
% bs_list_of_nums
   parsing_create_entry(bs_list_of_nums, call),
   bc_op(bs_list_of_nums, 0, '', [], []),
   bc_op(bs_list_of_nums, 0, pp, [bs_num-X, bs_kont_list_of_nums-Y], [X | Y]),
   parsing_create_entry(bs_kont_list_of_nums, call),
   bc_op(bs_kont_list_of_nums, 0, '', [], []),
   bc_op(bs_kont_list_of_nums, 0, fpp, [',', bs_num-X, bs_kont_list_of_nums-Y],
      [X | Y]),
% bs_list_of_molecules
   parsing_create_entry(bs_list_of_molecules, call),
   bc_op(bs_list_of_molecules, 0, '', [], []),
   bc_op(bs_list_of_molecules, 0, pp, [bs_molecule-M, bs_kont_list_of_molecules-Y], [M | Y]),
   parsing_create_entry(bs_kont_list_of_molecules, call),
   bc_op(bs_kont_list_of_molecules, 0, '', [], []),
   bc_op(bs_kont_list_of_molecules, 0, fpp, [',', bs_molecule-M, bs_kont_list_of_molecules-Y],
      [M | Y]),
% bs_conservation_list
   parsing_create_entry(bs_conservation_list, call),
   bc_op(bs_conservation_list, 0, '', [], []),
   bc_op(bs_conservation_list, 0, tfpp,
      [int(X), '*', bs_molecule-M, bs_kont_conserv-Y], [X*M | Y]),
   bc_op(bs_conservation_list, 0, pp,
      [bs_molecule-M, bs_kont_conserv-Y], [M | Y]),
   parsing_create_entry(bs_kont_conserv, call),
   bc_op(bs_kont_conserv, 0, '', [], []),
   bc_op(bs_kont_conserv, 0, ftfpp, [',', int(X), '*',
      bs_molecule-M, bs_kont_conserv-Y], [X*M | Y]),
   bc_op(bs_kont_conserv, 0, fpp, [',', 
      bs_molecule-M, bs_kont_conserv-Y], [M | Y]),
% bs_list_of_kinetics
   parsing_create_entry(bs_list_of_kinetics, call),
   bc_op(bs_list_of_kinetics, 0, '', [], []),
   bc_op(bs_list_of_kinetics, 0, pp, [bs_kinetics-X, bs_kont_list_of_kinetics-Y], [X | Y]),
   parsing_create_entry(bs_kont_list_of_kinetics, call),
   bc_op(bs_kont_list_of_kinetics, 0, '', [], []),
   bc_op(bs_kont_list_of_kinetics, 0, fpp, [',', bs_kinetics-X, bs_kont_list_of_kinetics-Y],
      [X | Y]),
                                       
% non definis dans la syntaxe, mais utilises...

% bc_op(bs_command, 0, f, [quit], quit),
   parsing_create_entry(bs_molecule_declaration, call),
   bc_op(bs_molecule_declaration, 0, tffffpff,
         [name(X), '~', parts_of, '(', '{', bs_set_of_names-Y, '}', ')'],
         '~'(X, parts_of({Y}))),
   bc_op(bs_molecule_declaration, 0, tffpf,
         [name(X), '~', '{', bs_set_of_set_of_names-Y, '}'],
         '~'(X, {Y})),
   true.

bc_op(Term, Lev, Spec, Args, Act) :-
   atom_chars(Spec, SpecL),
   map_specargs_rule(SpecL, Args, Rule),
   parsing_rule_asserta(Term, biocham_token, Lev, Rule, Act, call).

map_specargs_rule([], [], []).
map_specargs_rule([p | PL], [A1-A2 | AL], [p(A1, A2) | RL]) :-
   map_specargs_rule(PL, AL, RL), !.
map_specargs_rule([P | PL], [A | AL], [R | RL]) :-
   R =.. [P, A],
   map_specargs_rule(PL, AL, RL).

% Commands
comm_type(compose_ode_stochastic, [filename, filename]).
comm_type(compose_ode_stochastic, [filename, filename, num, num]).
comm_type(load_biocham, [filename]).
comm_type(add_biocham, [filename]).
comm_type(current_directory, []).
comm_type(change_directory, [filename]). 
comm_type(export_biocham, [savename]). % -----> export_biocham(N, 'no')
comm_type(expand_biocham, [filename]). % -----> export_biocham(N, 'yes')
comm_type(list_model, []).
comm_type(export_param, [savename]).
comm_type(export_init, [savename]).
comm_type(export_dot, [savename]).
comm_type(export_dot, [savename, list_of_options]).
comm_type(dot, []).
comm_type(draw_influences, []).
comm_type(draw_reactions, []).
comm_type(draw_neighborhood, []).
comm_type(export_nusmv, [savename]).
comm_type(export_lotos, [savename]).
comm_type(export_sbml, [savename]).
comm_type(export_prolog, [savename]).
comm_type(export_slp, [savename]).
comm_type(export_biopepa, [savename]).
comm_type(load_sbml, [filename]).
comm_type(add_sbml, [filename]).
comm_type(export_ode, [savename]).
comm_type(export_ode_latex, [savename]).
%comm_type(import_ode, [filename]).
comm_type(load_ode, [filename]).
comm_type(add_ode, [filename]).
comm_type(list_rules, []).
comm_type(list_rules, [reaction]).
comm_type(expand_rules, []).
comm_type(expand_rules, [reaction]).
comm_type(add_rule, [reaction]).
comm_type(add_rule, [set_of_reactions]).
comm_type(add_rules, [reaction]).
comm_type(add_rules, [set_of_reactions]).
comm_type(delete_rules, [reaction]).
comm_type(delete_rules, [set_of_reactions]).
comm_type(clear_rules, []).
comm_type(rule, [int]).
comm_type(rule, [name]).
comm_type(pathway, []).
comm_type(pathway, [list_of_ints]).
%comm_type(show_kinetics, []).
%comm_type(show_kinetics, [object]).
%FF
comm_type(list_ODE, []).
comm_type(list_ODE, [object]).
comm_type(list_kinetics, []).
comm_type(list_kinetics, [object]).
% comm_type(declare, [molecule_declaration]) -----> no parentheses in syntax
comm_type(list_events, []).
comm_type(delete_events, []).
comm_type(delete_event, [condition, name, kinetics]).
comm_type(delete_event, [condition, list_of_names, list_of_kinetics]).
comm_type(delete_time_event, [kinetics,condition, name, kinetics]).
comm_type(add_event, [condition, list_of_names, list_of_kinetics]).
comm_type(event, [condition, name, kinetics]).
comm_type(event, [condition, list_of_names, list_of_kinetics]).
comm_type(delete_time_event, [kinetics,condition, list_of_names, list_of_kinetics]).
comm_type(add_event, [condition, name, kinetics]).
comm_type(add_time_event, [kinetics,condition, name, kinetics]).
comm_type(add_time_event, [kinetics,condition, list_of_names, list_of_kinetics]).
comm_type(time_event, [kinetics,condition, name, kinetics]).
comm_type(time_event, [kinetics,condition, list_of_names, list_of_kinetics]).
comm_type(list_declarations, []).
comm_type(list_all_molecules, []).
comm_type(list_all_molecules, [object]).
comm_type(list_all_molecules, [set_of_objects]).
comm_type(list_molecules, []).
comm_type(list_molecules, [object]).
comm_type(list_molecules, [set_of_objects]).
comm_type(conservation, [set_of_objects]).
comm_type(conservation, [conservation_list]).
comm_type(delete_conservation, [set_of_objects]).
comm_type(delete_conservation, [conservation_list]).
comm_type(delete_conservations, []).
comm_type(list_conservations, []).
% comm_type(find_all_pinvar, []). -----> often too computationnaly expensive
%comm_type(find_pinvar, []).
%comm_type(find_pinvar, [int]).
comm_type(search_conservations, []).
comm_type(search_conservations, [int]).
comm_type(check_molecules, []).
%comm_type(show_functions, []).
%comm_type(show_neighborhood, []).
%comm_type(show_influences, []).
comm_type(list_functions, []).
comm_type(list_neighborhood, []).
comm_type(list_influences, []).
comm_type(export_neighborhood_dot, [savename]).
comm_type(export_influences_dot, [savename]).
comm_type(export_influences_ginml, [savename]).
comm_type(check_conservations, []).
%comm_type(show_initial_state, []).
comm_type(list_initial_state, []).
comm_type(clear_initial_state, []).
comm_type(present, [object]).
comm_type(present, [set_of_objects]).
comm_type(present, [object, num]). %FF perhaps we should allow negative values
comm_type(present, [object, name]).
comm_type(absent, [object]).
comm_type(absent, [set_of_objects]).
comm_type(undefined, [object]).
comm_type(undefined, [set_of_objects]).
% comm_type(new_molecule_for_learning, [object]).
% comm_type(new_molecule_for_learning, [set_of_objects]).
comm_type(make_present_not_absent, []).
comm_type(make_absent_not_present, []).
comm_type(parameter, [name, num]).
comm_type(parameter, [name]).
comm_type(list_parameters, []).
comm_type(macro, [name, kinetics]).
comm_type(macro, [name]).
comm_type(list_macros, []).
comm_type(volume, [name, kinetics]).
comm_type(volume, [name]).
comm_type(list_volumes, []).
comm_type(boolean_simulation, []).
comm_type(boolean_simulation, [int]).
comm_type(boolean_enumeration, []).
comm_type(numerical_simulation, []).
comm_type(numerical_simulation, [num]).
comm_type(continue, [num]).
comm_type(numerical_method, []).
comm_type(numerical_method, [name]).
comm_type(step_size, []).
comm_type(step_size, [num]).
comm_type(step_doubling, []).
comm_type(step_doubling, [num]).
comm_type(no_step_doubling, []).
comm_type(plot, []).
comm_type(plot, [object, object]).
comm_type(plot, [object, object, object]).
comm_type(export_plot, [savename]).
comm_type(show_molecules, [object]).
comm_type(show_molecules, [set_of_objects]).
comm_type(hide_molecules, [object]).
comm_type(hide_molecules, [set_of_objects]).
comm_type(show_macros, []).
comm_type(show_macros, [set_of_names]).
comm_type(hide_macros, []).
comm_type(show_parameters, []).
comm_type(show_parameters, [set_of_names]).
comm_type(hide_parameters, []).
comm_type(show_hide, []).
comm_type(keep_plot, []).
comm_type(set_png_size, [int, int]).
comm_type(set_color, [object, int]).
comm_type(set_color, [name, int]).
comm_type(test_plot, []).
comm_type(set_xmin, [num]).
comm_type(set_xmax, [num]).
comm_type(set_ymin, [num]).
comm_type(set_ymax, [num]).
comm_type(fit_xmin, []).
comm_type(fit_xmax, []).
comm_type(fit_x, []).
comm_type(fit_ymin, []).
comm_type(fit_ymax, []).
comm_type(fit_y, []).
comm_type(nusmv, [biocham_query]).
comm_type(nusmv_why, [biocham_query]).
%FF
comm_type(check_ctl, [biocham_query]).
comm_type(check_why, [biocham_query]).
comm_type(add_spec, [biocham_query]).
comm_type(add_specs, [set_of_biocham_queries]).
comm_type(delete_spec, [biocham_query]).
comm_type(delete_specs, [set_of_biocham_queries]).
comm_type(list_spec, []).
comm_type(clear_spec, []).
comm_type(add_ltl, [ltl_query]).
comm_type(add_ltl, [set_of_ltl_queries]).
comm_type(delete_ltl, [ltl_query]).
comm_type(delete_ltl, [set_of_ltl_queries]).
comm_type(list_ltl, []).
comm_type(clear_ltl, []).
comm_type(check_ltl_spec, []).
comm_type(check_ltl_spec, [num]).
comm_type(why, []).
comm_type(nusmv_dynamic_reordering, []).
comm_type(nusmv_disable_dynamic_reordering, []).
comm_type(fairness_path, []).
comm_type(no_fairness_path, []).
comm_type(nusmv_direct, []).
comm_type(nusmv_non_direct, []).
comm_type(reduce_model, []).
comm_type(reduce_model, [reaction]).
comm_type(reduce_model, [set_of_reactions]).
comm_type(genCTL, []).
comm_type(genCTL, [filename]).
comm_type(add_genCTL, []).
comm_type(revise_model, []).
comm_type(revise_model, [reaction]).
comm_type(revise_model, [set_of_reactions]).
comm_type(revise_model_interactive, [reaction]).
comm_type(revise_model_interactive, []).
comm_type(revise_model_interactive, [set_of_reactions]).
comm_type(learn_one_addition, [reaction]).
comm_type(learn_one_addition, [set_of_reactions]).
comm_type(learn_one_deletion, [reaction]).
comm_type(learn_one_deletion, [set_of_reactions]).
comm_type(learn_one_deletion, []).
comm_type(check_spec, []).
comm_type(check_why_spec, []).
comm_type(check_all_spec, []).
comm_type(check_reachable, [ctl_query]).
comm_type(check_stable, [ctl_query]).
comm_type(check_steady, [ctl_query]).
comm_type(check_oscil, [ctl_query]).
comm_type(check_checkpoint, [ctl_query, ctl_query]).
comm_type(check_loop, [ctl_query, ctl_query]).
comm_type(trace_check, [ltl_query]).
%FF
comm_type(check_ltl, [ltl_query]).
comm_type(learn_parameters, [list_of_names, list_of_intervals, int, num]).
comm_type(learn_parameters, [list_of_names, list_of_intervals, int, ltl_query, num]).
comm_type(search_parameters, [list_of_names, list_of_intervals, int, num]).
comm_type(search_parameters, [list_of_names, list_of_intervals, int, ltl_query, num]).
comm_type(search_all_parameters, [list_of_names, list_of_intervals, int, num]).
comm_type(search_all_parameters, [list_of_names, list_of_intervals, int, ltl_query, num]).
comm_type(search_random_parameters, [list_of_names, list_of_intervals, int, num]).
comm_type(search_random_parameters, [list_of_names, list_of_intervals, int, ltl_query, num]).
comm_type(search_random_all_parameters, [num, num, int, num]).
comm_type(search_random_all_parameters, [num, num, int, ltl_query, num]).
%FF
comm_type(domains, [ltl_query]).
comm_type(solve, [ltl_query]). %obsolete
comm_type(trace_analyze, [ltl_query]). %obsolete
comm_type(load_trace, [filename]).
comm_type(satisfaction_degree, [ltl_query, list_of_names, list_of_nums, num]).
comm_type(search_parameters_cmaes, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, num]).
comm_type(search_parameters_fss,[list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, num]).
comm_type(robustness, [list_of_names, list_of_nums, ltl_query, list_of_names, list_of_nums, int,num]).
comm_type(landscape, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, int,num,filename]).
comm_type(satisfaction_degree_plot, [name, list_of_intervals, ltl_query, list_of_names, list_of_nums, int,num,filename]).
comm_type(satisfaction_degree_plot_log, [name, list_of_intervals, ltl_query, list_of_names, list_of_nums, int,num,filename]).
comm_type(search_parameters_cmaes_log, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, num]).
comm_type(search_parameters_fss_log, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, num]).
comm_type(robustness_log, [list_of_names, list_of_nums, ltl_query, list_of_names, list_of_nums, int,num]).
comm_type(landscape_log, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, int,num,filename]).
comm_type(gsa_morris, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, num, int, int]).
comm_type(sa_local, [list_of_names, ltl_query, list_of_names, list_of_nums, num,num]).
comm_type(cmaes_params, [int,num,num]).
comm_type(fss_params, [int,int,num,num,num,int]).
comm_type(seed, [int]).
comm_type(cmaes_init_std, [num]).
comm_type(cmaes_multi_conditions,[]).
comm_type(first_search_condition, [list_of_names, list_of_intervals, ltl_query, list_of_names, list_of_nums, num]).
comm_type(add_search_condition, [ltl_query,list_of_names,list_of_nums,list_of_ppairs]).
comm_type(get_max_from_trace, [molecule]).
comm_type(get_min_from_trace, [molecule]).
comm_type(get_period_from_trace, [molecule]).
comm_type(dtc, [name]).
comm_type(set_init_from_trace, [num]).
comm_type(prolog, [filename]). %FF filename en attendant de pouvoir modifier ce parser

comm_type(search_reduction, [filename, filename]).
comm_type(search_all_reductions, [filename, filename]).
comm_type(search_mreduction, [filename, filename]).
comm_type(search_all_mreductions, [filename, filename]).
%comm_type(search_ireduction, [filename, filename]).
%comm_type(search_all_ireductions, [filename, filename]).
comm_type(smerge, [set_of_objects, molecule]).
comm_type(smerge, [object, molecule]).
comm_type(sdelete, [set_of_objects]).
comm_type(sdelete, [object]).

comm_type(rmerge, [set_of_reactions]).
comm_type(rmerge, [reaction]).
comm_type(rdelete, [set_of_reactions]).
comm_type(rdelete, [reaction]).

%Dragana delete parameter,declaration and macro.
comm_type(delete_parameter,[name]).
comm_type(delete_declaration,[name]).
comm_type(delete_macro,[name]).

comm_type(critical_reaction_threshold, []).
comm_type(critical_reaction_threshold, [int]).
/* CHH
comm_type(filtering, []).
comm_type(filtering, [num,num]).
comm_type(no_filtering, []).
*/
comm_type(conversion_factor, []).
comm_type(conversion_factor, [int]).
comm_type(quit, []).

% dimensions
% comm_type(set_dimension, [name, num]).
comm_type(list_dimensions, []).

comm_type(dump_commands, []).

apply_param(P, filename, X, Y) :- llist_hd_tl(X, tok(_, some(name(P))), Y).
apply_param(P, filename, X, Y) :- llist_hd_tl(X, tok(_, some(quote(P))), Y).
apply_param(P, savename, X, Y) :- llist_hd_tl(X, tok(_, some(name(P))), Y).
apply_param(P, savename, X, Y) :- llist_hd_tl(X, tok(_, some(quote(P))), Y).
apply_param(P, int, X, Y) :- llist_hd_tl(X, tok(_, some(int(P))), Y).
apply_param(P, name, X, Y) :- llist_hd_tl(X, tok(_, some(name(P))), Y).
apply_param(P, num, X, Y) :- llist_hd_tl(X, tok(_, some(int(P))), Y).
apply_param(P, num, X, Y) :- llist_hd_tl(X, tok(_, some(float(P))), Y).

apply_param(P, biocham_query, X, Y) :- call(bs_biocham_query(P, X, Y)).
apply_param(P, ctl_query, X, Y) :- call(bs_ctl_query(P, X, Y)).
apply_param(P, kinetics, X, Y) :- call(bs_kinetics(P, X, Y)).
apply_param(P, condition, X, Y) :- call(bs_condition(P, X, Y)).
apply_param(P, ltl_query, X, Y) :- call(bs_ltl_query(P, X, Y)).
apply_param(P, molecule, X, Y) :- call(bs_molecule(P, X, Y)).
apply_param(P, object, X, Y) :- call(bs_object(P, X, Y)).
apply_param(P, reaction, X, Y) :- call(bs_reaction(P, X, Y)).

apply_param(P, list_of_ints, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_ints(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_nums, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_nums(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_intervals, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_intervals(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_ppairs, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_ppairs(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_names, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_names(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_molecules, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_molecules(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_options, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_options(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, conservation_list, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_conservation_list(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).
apply_param(P, list_of_kinetics, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('['))), LL2),
   call(bs_list_of_kinetics(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct(']'))), LL4).

apply_param(P, set_of_biocham_queries, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('{'))), LL2),
   call(bs_set_of_biocham_queries(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct('}'))), LL4).
apply_param({P}, set_of_objects, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('{'))), LL2),
   call(bs_set_of_objects(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct('}'))), LL4).
apply_param({P}, set_of_names, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('{'))), LL2),
   call(bs_set_of_names(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct('}'))), LL4).
apply_param({P}, set_of_reactions, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('{'))), LL2),
   call(bs_set_of_reactions(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct('}'))), LL4).
apply_param(P, set_of_ltl_queries, LL1, LL4) :-
   llist_hd_tl(LL1, tok(_, some(punct('{'))), LL2),
   call(bs_set_of_ltl_queries(P, LL2, LL3)),
   llist_hd_tl(LL3, tok(_, some(punct('}'))), LL4).

apply_params([], [], LL, LL).
apply_params(PL, TL, LL1, LL2) :- apply_params_no_empty(PL, TL, LL1, LL2).

apply_params_no_empty([P], [T], LL1, LL2) :- apply_param(P, T, LL1, LL2).
apply_params_no_empty([P|PL], [T|TL], LL1, LL4) :-
   apply_param(P, T, LL1, LL2),
   llist_hd_tl(LL2, tok(_, some(punct(','))), LL3),
   apply_params_no_empty(PL, TL, LL3, LL4).

param_position(Tok-expected(T), [T|_], LL) :-
   llist_hd_tl(LL, Tok, _).
param_position(Tok-too_many_params, [], LL) :-
   llist_hd_tl(LL, Tok, _).
param_position(E, [T|TL], LL1) :-
   apply_param(_, T, LL1, LL2),
   llist_hd_tl(LL2, tok(_, some(punct(','))), LL3),
   param_position(E, TL, LL3).
param_position(Tok-parsing_error, [T], LL1) :-
   apply_param(_, T, LL1, LL2),
   llist_hd_tl(LL2, tok(_, some(punct(')'))), LL3),
   llist_hd_tl(LL3, Tok, _).
param_position(Tok-not_enough_params, [T,_|_], LL1) :-
   apply_param(_, T, LL1, LL2),
   llist_hd_tl(LL2, Tok, _),
   Tok = tok(_, some(punct(')'))).
param_position(Tok-extra_token, [T|_], LL1) :-
   apply_param(_, T, LL1, LL2),
   llist_hd_tl(LL2, Tok, _).

comm_type_error(E, C, LL) :-
   comm_type(C, TL),
   param_position(E, TL, LL).

furthest_error(BP-EP-[T], [tok(ta_loc(BP, EP), _)-T]).
furthest_error(BP-EP-TL, [tok(ta_loc(BP1, EP1), _)-T1 | EL]) :-
   furthest_error(BP2-EP2-TL2, EL),
   (
      BP2 > BP1 ->
         BP = BP2, EP = EP2, TL = TL2
      ;
      BP2 < BP1 ->
         BP = BP1, EP = EP1, TL = [T1]
      ;
         BP = BP1, EP = EP1,
         (member(T1, TL2) -> TL = TL2 ; TL = [T1 | TL2])
   ).

command_list_text([T], TT) :- format_to_atom(TT, '<~w>', [T]).
command_list_text([T | TL], TT) :-
   command_list_text(TL, TT1),
   format_to_atom(TT, '<~w> or ~w', [T, TT1]).

only_expected([], []).
only_expected([expected(T) | TL1], [T | TL2]) :- only_expected(TL1, TL2).
only_expected([_ | TL1], TL2) :- only_expected(TL1, TL2).

format_error(TL, E) :-
   only_expected(TL, TL1),
   command_list_text(TL1, TT),
  
        (
                have_gui
        ->
                format("[GUI] errors Value of type ~w expected.~n",[TT])
        ;
                 format_to_atom(M, 'Value of type ~w expected', [TT])
        ),
   E = typing_error(M).
format_error(TL, parsing_error('')) :-
   member(parsing_error, TL).
format_error(TL, typing_error('Too many parameters (or syntax error)')) :-
        (
                have_gui
        ->
                format("[GUI] errors Too many parameters (or syntax error).~n",[])
        ;
                true
        ),member(too_many_params, TL).
format_error(TL, typing_error('Not enough parameters')) :-
        (
                have_gui
        ->
                format("[GUI] errors Not enough parameters.~n",[])
        ;
                true
        ),member(not_enough_params, TL).
format_error(TL, parsing_error('')) :-
   member(extra_token, TL).

command_params(_-_-C-R, LL1, LL2) :-
   comm_type(C, TL),
   (
      TL = [] ->
         LL2 = LL1,
         PL = []
      ;
         llist_hd_tl(LL1, tok(_ ,some(punct('('))), LL11),
         apply_params(PL, TL, LL11, LL12),
         llist_hd_tl(LL12, tok(_ ,some(punct(')'))), LL2)
   ),
   R =.. [C | PL].
command_params(_-_-C-_, LL1, _) :-
   llist_hd_tl(LL1, tok(_ ,some(punct('('))), LL2),
   findall(E, comm_type_error(E, C, LL2), EL),
   furthest_error(BP-EP-TL, EL),
   format_error(TL, E),
   throw(exc_loc(BP, EP, E)).
command_params(_-_-C-_, LL1, _) :-
   llist_hd_tl(LL1, tok(ta_loc(BP, EP), some(symb('.'))), _),
   comm_type(C, _),
   throw(exc_loc(BP, EP, typing_error('Parameter expected'))),  (
                have_gui
        ->
                format("[GUI] errors Parameter expected.~n",[])
        ;
                true
        ).
command_params((BP, EP)-_-C-_, LL1, _) :-
   llist_hd_tl(LL1, tok(_, Tok), _),
   member(Tok, [some(symb('.')), some(punct('('))]),
   format_to_atom(M, 'Unknown command: ~q', [C]),
        (
                have_gui
        ->
                format("[GUI] errors Unknown command: ~q.~n",[C])
        ;
                true
        ),
   throw(exc_loc(BP, EP, typing_error(M))).

act_loc(Typ-Tok, (BP, EP)-Typ-Tok, LL, _) :-
   llist_hd_tl(LL, tok(ta_loc(BP, EP), _), _).

% Lexing...

:- dynamic(biocham_next_token/3).
:- dynamic(biocham_skip_spaces/2).

biosyntax_init_lexer :-
   lexing_create(biocham, call),
   lexing_add_spaces(biocham, " \n\r\t", call),
   lexing_add_comment(biocham, "%", "\n", [eof], call),
   lexing_add_token(biocham, "[a-zA-Z][a-zA-Z0-9_]*", X-name(X), call),
   lexing_add_token(biocham, "[()@[@]{},|]", X-punct(X), call),
   lexing_add_token(biocham, "[_?$#!]", X-symb(X), call),
   lexing_add_token(biocham, "[&*+@-./:<=>@@^~]+", X-symb(X), call),
   lexing_add_token(biocham, "'([^']*)'", X-quote(X), call),
   lexing_add_token(biocham, "[0-9]+", mktok(int), call),
   lexing_add_token(biocham, "[0-9]+[eE]-?[0-9]+",
                    mktok(efloat), call),
   lexing_add_token(biocham, "[0-9]+@.[0-9]+(?:[eE]-?[0-9]+)?",
                    mktok(float), call).

mktok(int, Arg, int(Num), L, L) :- number_atom(Num, Arg).
mktok(float, Arg, float(Num), L, L) :- number_atom(Num, Arg).
mktok(efloat, Arg, float(Num), L, L) :-
   atom_codes(Arg, CL1),
   add_dot(CL1, CL2),
   atom_codes(Arg2, CL2),
   number_atom(Num, Arg2).

add_dot([], []).
add_dot([0'e | CL], [0'., 0'0, 0'e | CL]).
add_dot([0'E | CL], [0'., 0'0, 0'E | CL]).
add_dot([C | CL1], [C | CL2]) :- add_dot(CL1, CL2).

% Errors

bs_init_error(parsing_rule_asserta_error(M, E, Lex, Prio, Rule, SemAct)) :-
   copy_term(Rule-SemAct, Rule1-SemAct1),
   numbervars(Rule1-SemAct1),
   format('\n\
Failure while adding a syntax rule; ~w:\n\
     parsing_rule_asserta(\n\
         ~q, ~q, ~q,\n\
         ~q,\n\
         ~q\n\
     )\n',
   [M, E, Lex, Prio, Rule1, SemAct1]),
   throw(parsing_initialization_error).
bs_init_error(S) :- write(S), nl, halt(2).

:- dynamic(error_position/0).

bios_error(Stream, P, exc_loc(BP, EP, E), ChrL, _, fail) :-
   nl,
   (Stream == user_input ->
      write('Toplevel input:\n')
   ;
      stream_property(Stream, file_name(FN)),
      format('File "%s", ', [FN]),
      BP1 is P + BP,
      EP1 is P + EP,
      SP is EP1 - 1,
      (position_line_column(FN, BP1, Line, P1) ->
         format('line ~q, ', [Line]),
         P2 is P1 - BP1 + SP
      ;
         P1 = BP1, P2 = SP),
      (P1 = P2 ->	
	format('character ~q', [P1])
         
      ;
	
	format('characters ~q-~q', [P1, P2])
	),
      write(':\n'),
	(
                have_gui
        ->
	(P1 = P2 ->	
		format('[GUI] errors File "%s" , line ~q, character ~q~n',[FN,Line, P1])
         
     		 ;
	
		format('[GUI] errors File "%s" , line ~q, characters ~q-~q~n',[FN,Line, P1, P2])
	)
               
        ;
         	true
        )

),
   !,
   char_list_lines(ChrL, EP, [], [], RLines),
   error_lines(BP, EP, RLines, RLines2),
   (
      Stream == user_input,
      error_position
   ->
      format('<error begin="~q" end="~q"/>~n', [BP, EP])
   ;
      highlight_position(RLines2, BP, EP)
   ), !,
   (
      E = parsing_error(_) ->
        
        (
                have_gui
        ->
                format("[GUI] errors Syntax error.~n",[])
        ;
         format('Syntax error.\n', [])
        )
      ;
      E = lexing_error ->
        
        (
                have_gui
        ->
                format("[GUI] errors Lexing error (this character is invalid).~n",[])
        ;
         write('Lexing error (this character is invalid)\n')
        )
      ;
      E = typing_error(M) ->
         write(M), nl
      ;
        
        (
                have_gui
        ->
                format("[GUI] errors ~w Exception thrown: ~q.~n",[M,E])
        ;
         format('Exception thrown: ~q\n', [E])
        )
   ).
bios_error(_, _, E, _, _, _) :-
   nl,
   write(E), nl,
        (
                have_gui
        ->
                format("[GUI] errors Bios error ~w.~n",[E])
        ;
                true
        ).

char_list_lines(ChrL, _, RLine, RLines1, RLines2) :-
   llist_nil(ChrL),
   reverse(RLine, Line),
   (RLine = [] -> RLines2 = RLines1 ; RLines2 = [Line | RLines1]).
char_list_lines(ChrL, EP, RLine, RLines1, RLines2) :-
   llist_hd_tl(ChrL, chr(P, C), ChrL2),
   (
      C = 0'\n ->
         reverse([chr(P, C) | RLine], Line),
         (
            P < EP ->
               char_list_lines(ChrL2, EP, [], [Line | RLines1], RLines2)
            ;
               (RLine = [] -> RLines2 = RLines1 ; RLines2 = [Line | RLines1])
         )
      ;
         char_list_lines(ChrL2, EP, [chr(P, C) | RLine], RLines1, RLines2)
   ). 

position_line_column(FN, Pos, Line, Col) :-
   open(FN, read, S),
   (stream_position_line_col(S, 0, 1, 1, Pos, Line, Col) -> close(S) ;
      close(S), fail).

stream_position_line_col(S, _, _, _, _, _, _) :- at_end_of_stream(S), !, fail.
stream_position_line_col(_, Pos, Line, Col, Pos, Line, Col) :- !.
stream_position_line_col(S, Pos, Line, _, PosR, LineR, ColR) :-
   peek_code(S, 0'\n), !,
   get_code(S, _),
   Pos2 is Pos + 1,
   Line2 is Line + 1,
   stream_position_line_col(S, Pos2, Line2, 1, PosR, LineR, ColR).
stream_position_line_col(S, Pos, Line, Col, PosR, LineR, ColR) :-
   peek_code(S, 0'\t), !,
   get_code(S, _),
   Pos2 is Pos + 1,
   Col2 is (Col + 7) // 8 * 8 + 1,
   stream_position_line_col(S, Pos2, Line, Col2, PosR, LineR, ColR).
stream_position_line_col(S, Pos, Line, Col, PosR, LineR, ColR) :-
   get_code(S, _), !,
   Pos2 is Pos + 1,
   Col2 is Col + 1,
   stream_position_line_col(S, Pos2, Line, Col2, PosR, LineR, ColR).

error_lines(BP, EP, [[chr(P, _) | _] | RLines1], RLines2) :-
   EP < P,
   error_lines(BP, EP, RLines1, RLines2).
error_lines(BP, EP, [[eof(_) | _] | RLines1], RLines2) :-
   error_lines(BP, EP, RLines1, RLines2).
error_lines(BP, EP, [[] | RLines1], RLines2) :-
   error_lines(BP, EP, RLines1, RLines2).
error_lines(BP, _, [CL | _], [CL]) :-
   CL = [chr(P, _) | _],
   BP >= P.
error_lines(BP, EP, [CL | RLines1], [CL | RLines2]) :-
   error_lines(BP, EP, RLines1, RLines2).
error_lines(_, _, [], []).

highlight_position([CL | CLL], BP, EP) :-
   highlight_position(CLL, BP, EP),
   write('> '),
   write_char_list(CL),
   write('  '),
   show_position(CL, BP, EP).
highlight_position([], _, _).

write_char_list([chr(_, C) | CL]) :- !,
   put_code(C),
   write_char_list(CL).
write_char_list(_).

show_position([chr(P, _) | _], _, EP) :-
   P >= EP, !,
   nl.
show_position([chr(P, _) | CL], BP, EP) :-
   BP =< P, !,
   write('^'),
   show_position(CL, BP, EP).
show_position([chr(_, 0'\t) | CL], BP, EP) :- !,
   write('\t'),
   show_position(CL, BP, EP).
show_position([_ | CL], BP, EP) :-
   write(' '),
   show_position(CL, BP, EP).
