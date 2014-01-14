% BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
% Copyright 2010, INRIA Paris-Rocquencourt, EPI Contraintes
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
% GNU prolog file biochamGRN.pl
% by Sylvain Soliman
%
% Reduce (see biochamreduce.pl) a reaction graph in order to get as influences
% (see biochamTypes.pl) the ones written by hand by people from the GRN
% community.

%% input(+Molecule)
%
% checks if a molecule is only used as input.
input(M) :-
   rule(_, LL, _, _, _, _, _),
   member((_, M), LL),
   \+((
      rule(_, _, LR, _, _, _, _),
      member((_, M), LR)
   )).


%% output(+Molecule)
%
% checks if a molecule is only used as output.
output(M) :-
   rule(_, _, LR, _, _, _, _),
   member((_, M), LR),
   \+((
      rule(_, LL, _, _, _, _, _),
      member((_, M), LL)
   )).


%% intermediate(-Molecule, -GeneratingRule, -ConsumingRule)
%
% checks if a molecule is only appearing as an intermediate between two
% reactions.
intermediate(M, R1, R2) :-
   rule(R1, LL1, LR1, _, _, _, _),
   remove_common(LL1, LR1, _, [(_, M)]),
   % TODO justify allow modifiers in R2 ?
   rule(R2, LL2, RR2, _, _, _, _),
   remove_common(LL2, RR2, [(_, M)], _),
   \+((
      rule(R, LL, LR, _, _, _, _),
      R \= R1,
      R \= R2,
      (
         member((_, M), LR)
      ;
         member((_, M), LL)
      )
   )).


%% delete_gene_inputs
%
% deletes all inputs which are genes.
% TODO arbitrary pattern
delete_gene_inputs :-
   molecule(#(M), _),
   input(#(M)),
   sdelete(#(M)),
   fail.

delete_gene_inputs.


%% delete_outputs
%
% delete all outputs
delete_outputs :-
   molecule(M, _),
   output(M),
   sdelete(M),
   fail.

delete_outputs.


%% delete_intermediary
%
% deletes all intermediate species which are such that:
% - they are a reversible complex (then delete decomplexation)
% - they are intermediate between disjoint compounds (then merge both
%   reactions)
delete_intermediary :-
   g_assign(grn_fix, 1),
   intermediate(M, R1, R2),
   format_debug(6, "trying to delete intermediate species ~w\n", [M]),
   rule(R1, LL1, LR1, _, _, _, _),
   rule(R2, LL2, LR2, _, _, _, _),
   (
      \+((
         member((_, N), LR2),
         member((_, N), LL1)
      ))
   ->
      % enforce output of R2 \cup input of R1 = \empty
      % then merge
      format_debug(6, "disjoint species -> merging ~w and ~w\n", [R1, R2]),
      rmerge({R1, R2})
   ;
      % no modifiers in R2, some in R1
      % and out R2 = in R1 -> delete R2
      LL2 = [(_, M)],
      remove_common(LL1, LR1, L, _),
      remove_common(LL2, LR2, _, L),
      format_debug(6, "same species and mods R1 only -> deleting ~w\n", [R2]),
      rdelete(R2)
   ),
   sdelete(M),
   format_debug(6, "deleted intermediate species ~w\n", [M]),
   g_assign(grn_fix, 0),
   fail.

delete_intermediary.


%% delete_identity
%
% deletes useless reactions
delete_identity :-
   retractall(rule(_, LL, LL, _, _, _, _)).


%% delete_linear_degradations
%
% deletes useless reactions
delete_linear_degradations :-
   retractall(rule(_, [_], [], _, _, _, _)).


%% grn_transform_model
%
% main predicate
% makes all necessary model transformations
grn_transform_model :-
   % allows to find more "outputs", "inputs", "intermed"
   format_debug(6, "deleting linear degradations\n", []),
   delete_linear_degradations,
   search_molecules,
   format_debug(6, "deleting gene inputs\n", []),
   delete_gene_inputs,
   format_debug(6, "deleting all outputs\n", []),
   delete_outputs,
   format_debug(6, "deleting identity\n", []),
   delete_identity,
   format_debug(6, "deleting intermediary species\n", []),
   (
      repeat,
      (
         intermediate(_, _, _),
         g_read(grn_fix, 0)
      ->
         delete_intermediary,
         fail
      ;
         true
      ),
      !
   ).
