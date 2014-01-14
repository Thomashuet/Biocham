%TODO : code was instrumented so that find_assignation would count backtrack.
%       When it won't be of use, this instrumentation should be removed.

%Transforms source arcs in their image arcs
m_arcs_to_varcs([(A, B)|Arcs], AVars, BVars, [(AVar, BVar)|VArcs]) :-
   nth(A, AVars, AVar),
   nth(B, BVars, BVar),
   m_arcs_to_varcs(Arcs, AVars, BVars, VArcs).

m_arcs_to_varcs([], _, _, []).

%fd_relation uses lists of lists for relations
m_arcs_to_lists([(A, B)|Arcs], [[A, B] | Lists]) :-
   m_arcs_to_lists(Arcs, Lists).

m_arcs_to_lists([], []).


list_left([], []).

list_left([(L, _R) | Arcs], [L | LArcs]) :-
   list_left(Arcs, LArcs).


list_right([], []).

list_right([(_L, R) | Arcs], [R | LArcs]) :-
   list_right(Arcs, LArcs).


find_inclusions(ALio, Inclusions) :-
   findall(Inclusion, (member((X, (XL1, XL2)), ALio),
                       member((Y, (YL1, YL2)), ALio),
                       X \= Y,
                       sorted_list_inclusion(XL1, YL1),
                       sorted_list_inclusion(XL2, YL2),
                       Inclusion = (X, Y)
                      ),
           Inclusions).


left_couple_part_rewrite_pattern([], []).

left_couple_part_rewrite_pattern([C|Cs], [P|Ps]) :-
   left_couple_part_rewrite_pattern(C, P),
   left_couple_part_rewrite_pattern(Cs, Ps).

left_couple_part_rewrite_pattern((A, B), ((A, _X), (B, _X))).


%Sets up all needed variables to modelize a morphism problem
m_make_graph_problem((NS1, NR1, ASR1, ARS1), VSpec, VReac, VASR1, VARS1) :-
   m_generate_vars(NS1, VSpec),
   m_generate_vars(NR1, VReac),
   m_arcs_to_varcs(ASR1, VSpec, VReac, VASR1),
   m_arcs_to_varcs(ARS1, VReac, VSpec, VARS1).


m_epimorphism(G1, G2, _SVars, _RVars, VASR1, VARS1, EnumMe) :-
   G1 = (NS1, NR1, ASR1, ARS1),
   G2 = (NS2, NR2, ASR2, ARS2),

   NS1 >= NS2,
   NR1 >= NR2,

   m_arc_surjectivity_constraints(ASR1, VASR1, ASR2, BackArcsSR),
   m_arc_surjectivity_constraints(ARS1, VARS1, ARS2, BackArcsRS),
   append(BackArcsSR, BackArcsRS, EnumMe),

   m_morphism_constraints(VASR1, ASR2),
   m_morphism_constraints(VARS1, ARS2).


m_subgraph_epimorphism(G1, G2, _SVars, _RVars, VASR1, VARS1, EnumMe) :-
   G1 = (NS1, NR1, ASR1, ARS1),
   G2 = (NS2, NR2, ASR2, ARS2),

   NS1 >= NS2,
   NR1 >= NR2,

   m_arc_surjectivity_constraints(ASR1, VASR1, ASR2, BackArcsSR),
   m_arc_surjectivity_constraints(ARS1, VARS1, ARS2, BackArcsRS),
   append(BackArcsSR, BackArcsRS, EnumMe),

   m_pointed_graph(G2, G2p),
   G2p = (_, _, ASR2p, ARS2p),
   
   m_morphism_constraints(VASR1, ASR2p),
   m_morphism_constraints(VARS1, ARS2p).
   

m_internal(G1, G2, SVars, RVars, VASR1, VARS1, EnumMe) :-
   G1 = (NS1, NR1, ASR1, ARS1),
   G2 = (NS2, NR2, ASR2, ARS2),

   NS1 >= NS2,
   NR1 >= NR2,

   m_arc_surjectivity_constraints(ASR1, VASR1, ASR2, BackArcsSR),
   m_arc_surjectivity_constraints(ARS1, VARS1, ARS2, BackArcsRS),
   initial_segment(NS1, S1Nodes),
   initial_segment(NR1, R1Nodes),
   initial_segment(NS2, S2Nodes),
   initial_segment(NR2, R2Nodes),
   m_node_surjectivity_constraints(S1Nodes, SVars, S2Nodes, BackNodesS),
   m_node_surjectivity_constraints(R1Nodes, RVars, R2Nodes, BackNodesR),

   appendall([BackNodesS, BackNodesR, BackArcsSR, BackArcsRS], EnumMe),
   m_spiked_graph(G2, G2p),

%   write_graph(G2,   'ipattern.dot'),
%   write_graph(G2p0, 'ipatterns.dot'),
%   write_graph(G2p,  'ipatternsp.dot'),

   G2p = (_, _, ASR2p, ARS2p),
   
   m_morphism_constraints(VASR1, ASR2p),
   m_morphism_constraints(VARS1, ARS2p).


find_assignations(Varss) :-
%   format("Looking for assignations~N", []),
   appendall(Varss, V),!,
   find_assignation(V).


find_assignation([Var|Vars]) :-
   fd_dom(Var, L),
   member(Value, L),
   g_inc(backtracks),
   Var #= Value,
   g_dec(backtracks),
   find_assignation(Vars).

find_assignation([]).


find_morphism(G1, G2, MorphismType, SVars, RVars) :-
%   m_nsq(G2,  G2Q,  SClasses,  RClasses),
%   m_nsq(G2Q, G2QQ, QSClasses, QRClasses),

   m_make_graph_problem(G1, SVars,   RVars,   VASR,   VARS),
%   m_make_graph_problem(G1, QSVars,  QRVars,  VASRQ,  VARSQ),
%   m_make_graph_problem(G1, QQSVars, QQRVars, VASRQQ, VARSQQ),

   (
      MorphismType = m_lonely_epimorphism
   ->
      MorphismFun = m_epimorphism,
      no_internal_merge(G1, G2, SVars, RVars)
   ;
      MorphismType = m_lonely_internal_epimorphism
   ->
      MorphismFun = m_internal,
      no_internal_merge(G1, G2, SVars, RVars)
   ;
      MorphismFun = MorphismType
   ),

   Constraints   =.. [MorphismFun, G1, G2,   SVars,   RVars,   VASR,   VARS,   EnumMe],   call(Constraints),
%   ConstraintsQ  =.. [MorphismFun, G1, G2Q,  QSVars,  QRVars,  VASRQ,  VARSQ,  EnumMeQ],  call(ConstraintsQ),
%   ConstraintsQQ =.. [MorphismFun, G1, G2QQ, QQSVars, QQRVars, VASRQQ, VARSQQ, EnumMeQQ], call(ConstraintsQQ),

%   link_morphisms(SVars,  RVars,  QSVars,  QRVars,  SClasses,  RClasses),
%   link_morphisms(QSVars, QRVars, QQSVars, QQRVars, QSClasses, QRClasses),
   !,

%   find_assignations([EnumMeQQ, EnumMeQ, EnumMe, QQSVars, QQRVars, QSVars, QRVars, SVars, RVars]).
%   find_assignations([EnumMeQ, EnumMe, QSVars, QRVars, SVars, RVars]).
   find_assignations([EnumMe, SVars, RVars]).


find_morphism(F1, F2, MorphismF, OneOrAll) :-
   format_debug(2, "searching for reduction between ~w and ~w.~n", [F1, F2]),
   g_assign(sepi_dist, -2),   % it should never stay like that
   bg_get_graph_from_file(F1, G1, MNames1),
   format_debug(6, "read graph for ~w.~n", [F1]),
   bg_get_graph_from_file(F2, G2, MNames2),
   format_debug(6, "read graph for ~w.~n", [F2]),

   G1 = (NS1, NR1, _ASR1, _ARS1),
   G2 = (NS2, NR2, _ASR2, _ARS2),

   NVectorMax is 2*(NS2 + NR2 + 1), %We might use spiked graphs and the like..
   fd_vector_max(VectorMax), 
   (
       VectorMax < NVectorMax 
    -> 
       fd_set_vector_max(NVectorMax)
    ;  
       true
   ),
   format_debug(5, "set vector_max.~n", []),

   (
       OneOrAll = one 
   ->

       (
		(
	  	   have_gui
			->
			   	   format("[GUI] reduction Looking for a reduction....~n",[])
		        ;
			           format("Looking for a reduction~N", [])
		),
           find_morphism(G1, G2, MorphismF, SVars, RVars)
       ->   
		(
	  	   have_gui
			->
			   	   format("[GUI] reduction Reduction found : ~n",[])
		        ;
			 	   format("Reduction found :~N", [])
		),           
           % store the distance for D3S API
           SepiDist is NS1 + NR1 - NS2 - NR2,
           g_assign(sepi_dist, SepiDist),
           show_morphism(NS1, NR1, NS2, NR2, SVars, RVars, MNames1, MNames2)
       ;
		(
	  	   have_gui
			->
			   	   format("[GUI] reduction No reduction found.~n",[])
		        ;
			           format("No reduction found~N", [])
		),
           g_assign(sepi_dist, -1)
       )
   ;
		(
	  	   have_gui
			->
			   	   format("[GUI] reduction Looking for all reductions....~n",[])
		        ;
			           format("Looking for all reductions~N", [])
		),      
       g_assign(reduction_total, 0),
       
       find_morphism(G1, G2, MorphismF, SVars, RVars),
       g_inc(reduction_total, Reduction_total),
	(
	   have_gui
		->
		   	format("[GUI] reduction Reduction ~d found :~n",[Reduction_total])
	        ;
			format("Reduction ~d found :~N", [Reduction_total])
	),        
       show_morphism(NS1, NR1, NS2, NR2, SVars, RVars, MNames1, MNames2),
       fail
       ;
	(
	   have_gui
		->
		   	format("[GUI] reduction End of reductions.~n",[])
	        ;
			format("End of reductions~N", [])
	)       
   ),
   true.


show_morphism(NS1, NR1, NS2, NR2, VS, VR, MNames1, MNames2) :-
   for(Species1, 1, NS1),
   nth(Species1, MNames1, Name1),
   nth(Species1, VS, Image1),
   (Image1 > NS2  -> IName1 = '_' ; nth(Image1, MNames2, IName1)),
	(
	   have_gui
		->
		   	format("[GUI] reduction ~w -> ~w~n",[Name1, IName1])
	        ;
			format("~w -> ~w~N", [Name1, IName1])
	),      
   fail
   ;
   for(Reaction1, 1, NR1),
   nth(Reaction1, VR, Image1),
   (Image1 > NR2  -> IName1 = '_' ; IName1 = Image1),
   (
	   have_gui
		->
		   	format("[GUI] reduction ~w -> ~w~n",[Reaction1, IName1])
	        ;
			format("~w -> ~w~N", [Reaction1, IName1])
   ),     
   fail
   ;
   true.


search_reduction(F1, F2) :-
   find_morphism(F1, F2, m_subgraph_epimorphism, one).

search_all_reductions(F1, F2) :-
   
   find_morphism(F1, F2, m_subgraph_epimorphism, all).

search_mreduction(F1, F2) :-
   find_morphism(F1, F2, m_epimorphism, one).

search_all_mreductions(F1, F2) :-
   find_morphism(F1, F2, m_epimorphism, all).

%search_ireduction(F1, F2) :-
%   find_morphism(F1, F2, m_internal, one).
%
%search_all_ireductions(F1, F2) :-
%   find_morphism(F1, F2, m_internal, all).

%format has a better syntax for newline characters
format(S) :- format(S, []).

%generates a list of unconstrained fd_variables
m_generate_vars(Size, List) :-
   m_generate_vars(Size, List, []).

m_generate_vars(Size, List, Acc) :-
   (
      Size =< 0
   ->
      List = Acc
   ;
      Size0 is Size - 1,
      X #> 0,
      m_generate_vars(Size0, List, [X | Acc])
   ).


%initial_segment(Max, L) is succeeds iff L is [|1; Max|]
initial_segment(Max, L) :-
   initial_segment(Max, [], L).

initial_segment(0, Acc, Acc) :- !.

initial_segment(N, Acc, L) :-
   N1 is N - 1,
   initial_segment(N1, [N | Acc], L).


%sorted_list_inclusion(L1, L2) succeeds if L1 is included in L2
sorted_list_inclusion([], _).

sorted_list_inclusion([A|As], [A|Bs]) :- !, sorted_list_inclusion(As, Bs).

sorted_list_inclusion([A|As], [B|Bs]) :- A > B -> sorted_list_inclusion([A|As], Bs) ; fail.
   

%rewrite_term(SourceTerm, TargetTerm, SourcePattern, TargetPattern)
rewrite_term([], [], _, _) :- !.

rewrite_term([S|Ss], Targets, Source, Target) :- !,
   findall(XT, (member(XS, [S|Ss]), 
                rewrite_term(XS, XT, Source, Target)),
           Targets).

rewrite_term(Source, Target, Source, Target) :- !.

rewrite_term(SourceTerm, TargetTerm, Source, Target) :-
   SourceTerm =.. [SourceF | SourceArgs],
   rewrite_term(SourceArgs, TargetArgs, Source, Target),
   TargetTerm =.. [SourceF | TargetArgs].

%rewrites_term(SourceTerm, TargetTerm, Couples).
rewrites_term(SourceTerm, SourceTerm, []) :- !.

rewrites_term(SourceTerm, TargetTerm, [(S, T)|Couples]) :-
   rewrite_term(SourceTerm, SourceTerm0, S, T),
   rewrites_term(SourceTerm0, TargetTerm, Couples).


list_add_index(L, LIndexed) :-
   list_add_index(L, [], LIndexed, 1).

list_add_index([], Acc, RAcc, _Index) :- reverse(Acc, RAcc).

list_add_index([E|Es], Acc, LIndexed, Index) :-
   Index1 is Index + 1,
   list_add_index(Es, [(Index, E) | Acc], LIndexed, Index1).


zip_lists(L1, L2, Lzip) :-
   zip_lists(L1, L2, [], RLzip),
   reverse(RLzip, Lzip).

zip_lists([], [], Acc, Acc).

zip_lists([H1|T1], [H2|T2], Acc, L) :-
   zip_lists(T1, T2, [(H1, H2)|Acc], L).

appendall([Vars], Vars) :- !.

appendall([Vars|Varss], R) :-
   append(Vars, Vs, R),
   appendall(Varss, Vs).   
add_constraints([]).

add_constraints([C|Cs]) :-
%   format("Adding ~w~N", [C]),
   C,
   add_constraints(Cs).

%Graph morphism constraints, that is (a, b) \in G => (m(a), m(b)) \in G'
m_morphism_constraints_arcs([(A, B)|SourceArcs], PatternArcs) :-
   fd_relation(PatternArcs, [A, B]),
   m_morphism_constraints_arcs(SourceArcs, PatternArcs).

m_morphism_constraints_arcs([], _PatternArcs).


m_morphism_constraints(SourceArcs, PatternArcs) :-
   m_arcs_to_lists(PatternArcs, LPatternArcs),
   m_morphism_constraints_arcs(SourceArcs, LPatternArcs).


%Some tools to build constraints
m_arc_equality_constraints([(X, Y) | Arcs], (A, B), [Constraint | Constraints]) :-
   Constraint = ((X #=# A) #/\ (Y #=# B)),
   m_arc_equality_constraints(Arcs, (A, B), Constraints).

m_arc_equality_constraints([], _, []).


m_term_equality_constraints([Node|Nodes], A, [Node #=# A | Constraints]) :-
   m_term_equality_constraints(Nodes, A, Constraints).

m_term_equality_constraints([], _, []).


m_term_inequality_constraints([Node|Nodes], A, [Node #\=# A | Constraints]) :-
   m_term_inequality_constraints(Nodes, A, Constraints).

m_term_inequality_constraints([], _, []).


m_constraints_or([C], C) :- !.

m_constraints_or([C | Cs], C #\/ OCs) :-
   m_constraints_or(Cs, OCs).

m_constraints_or([], 0).


m_constraints_and([C], C) :- !.

m_constraints_and([C | Cs], C #/\ OCs) :-
   m_constraints_and(Cs, OCs).

m_constraints_and([], 1).


%Constraints is the list of
%\or_{v \in Vars} v #=# i
%for every i in [|1; ImageMax|]
m_node_surjectivity_soft_constraints(Vars, ImageMax, Constraints) :-
   (
      ImageMax > 0
   ->
      m_term_equality_constraints(Vars, ImageMax, VC),      
      m_constraints_or(VC, C),
      ImageMax1 is ImageMax - 1,
      m_node_surjectivity_soft_constraints(Vars, ImageMax1, Constraints1),
      Constraints = [C | Constraints1]
   ;
      Constraints = []
   ).


m_arc_surjectivity_constraints0(_SourceArcs, []) :- !.
   
m_arc_surjectivity_constraints0(SourceArcs, [ImageArc|ImageArcs]) :- !,
   m_arc_equality_constraints(SourceArcs, ImageArc, ImageArcConstraints),
   fd_at_least_one(ImageArcConstraints),
   m_arc_surjectivity_constraints0(SourceArcs, ImageArcs).


m_arc_surjectivity_constraints(SourceArcs, VSourceArcs, ImageArcs, BackArcs) :-
   %Generate a variable for each pattern arc
   length(ImageArcs, NImageArcs),
   length(BackArcs,  NImageArcs),

   %Initialize BackArcs
   length(SourceArcs, NSourceArcs),
   initial_segment(NSourceArcs, SourceDomain),
   fd_domain(BackArcs, SourceDomain),
   fd_all_different(BackArcs),

   %Link BackArcs to VSourceArcs
   list_left( VSourceArcs, VSourceArcsLeft),
   list_right(VSourceArcs, VSourceArcsRight),
   list_left( ImageArcs,   ImageArcsLeft),
   list_right(ImageArcs,   ImageArcsRight),
   link_back(BackArcs, VSourceArcsLeft,  ImageArcsLeft),
   link_back(BackArcs, VSourceArcsRight, ImageArcsRight).

%VSources is considered as an array, and
%VSources[BackVar] = Image
link_back([BackVar | BackVars], VSources, [Image | Images]) :-
   fd_element_var(BackVar, VSources, Image),
   link_back(BackVars, VSources, Images).

link_back([], _, []).


m_node_surjectivity_constraints(SourceNodes, VSourceNodes, ImageNodes, BackNodes) :-
   %Generate a variable for each pattern node
   length(ImageNodes, NImageNodes),
   length(BackNodes,  NImageNodes),

   %Initialize BackNodes
   length(SourceNodes, NSourceNodes),
   initial_segment(NSourceNodes, SourceDomain),
   fd_domain(BackNodes, SourceDomain),
   fd_all_different(BackNodes),

   %Link BackNodes to VSourceNodes
   link_back(BackNodes, VSourceNodes, ImageNodes).


link_morphisms(SVars, RVars, QSVars, QRVars, SClasses, RClasses) :-
   link_morphisms_half(SVars, QSVars, SClasses),
   link_morphisms_half(RVars, QRVars, RClasses).


link_morphisms_half(Vars, QVars, Classes) :-
   %Change from an image based representation of the classes to
   %a (representant, representee list) list.
   length(Classes, LClasses),
   list_add_index(Classes, IClasses),
   m_reverse_arcs(IClasses, RIClasses),
   m_arcs_to_adjacency_list(RIClasses, RE0, LClasses),
   findall(Nonempty, (member(Nonempty, RE0),
                      Nonempty = (_, [_|_])),
           RE),

   %Put each Var with its corresponding QVar
   zip_lists(Vars, QVars, ZVars),

   apply_link_constraints(ZVars, RE).


apply_link_constraints([], _).

apply_link_constraints([_ | _], []).

apply_link_constraints([(V, VQ) | ZVars], [(Rep, Els) | RE]) :-
   m_term_equality_constraints(Els, V, LeftTerms),
   m_constraints_or(LeftTerms, Left),
   (Left #<=> (VQ #=# Rep)),
   apply_link_constraints([(V, VQ) | ZVars], RE).

%for every node s of the target graph G1,
%if every neighbour of s has the same image i,
%then the image of s will only have i as a neighbour.
no_internal_merge(G1, G2, SVars, RVars) :-
%   format("no internal merge called with~N SVars:~N~w~N RVars:~N~w~N", [SVars, RVars]),   
   nonpeninsula_nodes(G2, SNonp, RNonp),
%   format("nonpeninsula nodes computed~N", []),
   neighbours(G1, SNeighbours, RNeighbours),
%   format("neighbours nodes computed~N SNeighbours:~N~w~N RNeighbours:~N~w~N", [SNeighbours, RNeighbours]),
   no_internal_merge_constraints(SNeighbours, RVars, SNonp, SVars),
   no_internal_merge_constraints(RNeighbours, SVars, RNonp, RVars).


no_internal_merge_constraints([(A, AN) | As], BV, ANonImages, AVars) :-
   all_equal_constraint(AN, BV, AN_allequal),
%   format("all equal constraints computed~N", []),

   nth(A, AVars, AV),
   m_term_inequality_constraints(ANonImages, AV, AVConstraints),
%   m_constraints_and(AVConstraints, AVIsAPeninsula),

%   format("AN_allequal:~N~w~NAVIsAPeninsula:~N~w~N", [AN_allequal, AVIsAPeninsula]),
%   ((AN_allequal) #==> (AVIsAPeninsula)),
%TODO : test if above -> ; is of any use, or if we should put the else case
   imply_constraints(AVConstraints, AN_allequal),

   no_internal_merge_constraints(As, BV, ANonImages, AVars).
   
no_internal_merge_constraints([], _, _, _).
   
%A implies all of Bs
imply_constraints([B|Bs], A) :-
  A #==> B,
  imply_constraints(Bs, A).

imply_constraints([], _).

   
all_equal_constraint([], _, 1).

all_equal_constraint([_], _, 1).

all_equal_constraint([A, B | Nodes], Vars, (AV #=# BV) #/\ Constraints) :-
   nth(A, Vars, AV),
   nth(B, Vars, BV),
   all_equal_constraint([B | Nodes], Vars, Constraints).
nonpeninsula_nodes(G, SNonp, RNonp) :-
   neighbours(G, SN, RN),
   findall(X, (member(A, SN),
               A = (X, [N1, N2 | _]) % X has at least two neighbours
              ),
           SNonp),
   findall(X, (member(A, RN),
               A = (X, [N1, N2 | _])
              ),
           RNonp).


neighbours((_, _, ASR, ARS), SNeigh, RNeigh) :-
   m_reverse_arcs(ARS, ASR2),
   append(ASR, ASR2, AAllASR),
   sort(AAllASR, AllASR),
   m_arcs_to_adjacency_list(AllASR, SNeigh),

   m_reverse_arcs(ASR, ARS2),
   append(ARS, ARS2, AAllARS),
   sort(AAllARS, AllARS),
   m_arcs_to_adjacency_list(AllARS, RNeigh).


%Transforms an arc-based representation to adjacency list.
%Isolated nodes do not appear in the list.
m_arcs_to_adjacency_list(Arcs, AL) :-
   sort(Arcs, SArcs),
   m_arcs_to_adjacency_list_aux(SArcs, [], AL).

m_arcs_to_adjacency_list_aux([], Acc, Acc).

m_arcs_to_adjacency_list_aux([(N, V)|Arcs], [(N, L)|Acc], Res) :- !,
   m_arcs_to_adjacency_list_aux(Arcs, [(N, [V|L]) | Acc], Res).

m_arcs_to_adjacency_list_aux([(N, V)|Arcs], Acc, Res) :- !,
   m_arcs_to_adjacency_list_aux(Arcs, [(N, [V]) | Acc], Res).


%Transforms an arc-based representation to adjacency list.
%Needs a Max to be able to send back empty lists of successors (for isolated nodes)
m_arcs_to_adjacency_list(Arcs, AL, Max) :-
   sort(Arcs, SArcs),
   reverse(SArcs, RSArcs),
   !,
   m_arcs_to_adjacency_list0(RSArcs, [(Max, [])], AL).

m_arcs_to_adjacency_list0(_Arcs, [(0, _HAcc)| Acc], Acc) :- !.

m_arcs_to_adjacency_list0([(A, B) | Arcs], [(A, AL) | Acc], FAL) :- !,
   m_arcs_to_adjacency_list0(Arcs, [(A, [B|AL]) | Acc], FAL).

m_arcs_to_adjacency_list0(Arcs, [(A, AL) | Acc], FAL) :-
   A1 is A - 1,
   m_arcs_to_adjacency_list0(Arcs, [(A1, []), (A, AL) | Acc], FAL).


%As its name suggests
m_adjacency_list_to_arcs(AL, Arcs) :-
   m_adjacency_list_to_arcs(AL, [], Arcs).

m_adjacency_list_to_arcs([(A, [V|Vs]) | AL], Acc, Arcs) :- !,
   m_adjacency_list_to_arcs([(A, Vs) | AL], [(A, V) | Acc], Arcs).

m_adjacency_list_to_arcs([(_A, []) | AL], Acc, Arcs) :-
   m_adjacency_list_to_arcs(AL, Acc, Arcs).

m_adjacency_list_to_arcs([], Acc, Acc).


%Takes a bipartite graph, adds a special node for each class,
%and links the special nodes to every other node in the relevant class,
%including the other special node
m_pointed_graph((NS, NR, ASR, ARS), (NS, NR, ASR2, ARS2)) :-
   findall(S, member((S, _R), ASR), S1),
   findall(S, member((_R, S), ARS), S2),
   append(S1, S2, AAllS),
   sort(AAllS, AllS),

   findall(R, member((_S, R), ASR), R1),
   findall(R, member((R, _S), ARS), R2),
   append(R1, R2, AAllR),
   sort(AAllR, AllR),

   SPoint is 1 + NR,
   RPoint is 1 + NS,

   findall(SRArc, (member(SRArc, ASR);
                   (member(S, AllS), SRArc = (S, SPoint));
                   (member(R, AllR), SRArc = (RPoint, R));
		   SRArc = (RPoint, SPoint)
                  ),
	   ASR2),
   findall(RSArc, (member(RSArc, ARS);
                   (member(R, AllR), RSArc = (R, RPoint));
                   (member(S, AllS), RSArc = (SPoint, S));
		   RSArc = (SPoint, RPoint)
                  ),
	   ARS2).


%G spiked is G with every vertex pointed, with points not interlinked
%Only existing nodes are spiked.
m_spiked_graph((NS, NR, ASR, ARS), (NS, NR, ASR2, ARS2)) :-
   findall(S, member((S, _R), ASR), S1),
   findall(S, member((_R, S), ARS), S2),
   append(S1, S2, AAllS),
   sort(AAllS, AllS),

   findall(R, member((_S, R), ASR), R1),
   findall(R, member((R, _S), ARS), R2),
   append(R1, R2, AAllR),
   sort(AAllR, AllR),

   findall(SRArc, (member(SRArc, ASR);
                   (member(S, AllS), SRArc = (S, SPoint), SPoint is S + NR);
                   (member(R, AllR), SRArc = (RPoint, R), RPoint is R + NS)
                  ),
	   ASR2),
   findall(RSArc, (member(RSArc, ARS);
                   (member(R, AllR), RSArc = (R, RPoint), RPoint is R + NS);
                   (member(S, AllS), RSArc = (SPoint, S), SPoint is S + NR)
                  ),
	   ARS2).


%Removes arcs to/from original nodes that are only connected to
%non-original nodes.
m_pruned_graph((NS, NR, ASR, ARS), (NS, NR, ASR2, ARS2)) :-
%   format("Pruning graph, NS = ~w, NR = ~w~N", [NS, NR]),
   findall(S, (member((S, _), ASR);
               member((_, S), ARS)),
           AllS),
   findall(R, (member((R, _), ARS);
               member((_, R), ASR)),
           AllR),
   max_list([0|AllS], NS2),
   max_list([0|AllR], NR2),

   m_arcs_to_adjacency_list(ASR, ALSR, NS2),
%   format("~NALSR =~N~w~N", [ALSR]),
   findall(Entry, (member(Entry, ALSR),
                   Entry = (X, L), X =< NS,
                   sort(L, L2), L2 = [H | _], H =< NR),
           ALSR2),
   m_adjacency_list_to_arcs(ALSR2, ASR2),

   m_arcs_to_adjacency_list(ARS, ALRS, NR2),
%   format("~NALRS =~N~w~N", [ALRS]),
   findall(Entry, (member(Entry, ALRS),
                   Entry = (X, L), X =< NR,
                   sort(L, L2), L2 = [H | _], H =< NS),
           ALRS2),
   m_adjacency_list_to_arcs(ALRS2, ARS2).
   

%The symmetrised graph of G has (u, v) as arcs if G has either (u, v) or (v, u) as an arc.
m_reverse_arcs(Arcs, RArcs) :-
   m_reverse_arcs(Arcs, [], RArcs).

m_reverse_arcs([], Acc, Acc).

m_reverse_arcs([(A, B) | Arcs], Acc, RArcs) :-
   m_reverse_arcs(Arcs, [(B, A) | Acc], RArcs).


%neighbour subset quotient of a graph
adjacency_merge([], [], []).

adjacency_merge([(H, L1) | T1], [(H, L2) | T2], [(H, (L1, L2)) | T]) :-
   adjacency_merge(T1, T2, T).


m_nsq((NS, NR, ASRsucc, ARSsucc), (NS, NR, ASRsuccQsorted, ARSsuccQsorted), SClasses, RClasses) :- %4 arguments
%   format("Entering m_nsq, NS is ~d, NR is ~d~N", [NS, NR]),
   m_reverse_arcs(ARSsucc, ASRpred),
   m_nsq_half(NS, NR, ASRsucc, ASRpred, ASRsuccQ0, ASRpredQ0, SClasses),
   m_reverse_arcs(ASRpredQ0, ARSsucc0),
   m_reverse_arcs(ASRsuccQ0, ARSpred0),
%   format("First half done~N", []),
   m_nsq_half(NR, NS, ARSsucc0, ARSpred0, ARSsuccQ, ARSpredQ, RClasses),
   m_reverse_arcs(ARSpredQ, ASRsuccQ),
   sort(ASRsuccQ, ASRsuccQsorted),
   sort(ARSsuccQ, ARSsuccQsorted),
%   format("m_nsq sending back~NSClasses:~N~w~NRClasses:~N~w~N", [SClasses, RClasses]),
   !.


remove_supp_arcs([], [], _Max1, _Max2).

remove_supp_arcs([(A, B)|Arcs], FArcs0, Max1, Max2) :-
   (A =< Max1, B =< Max2 -> FArcs0 = [(A, B) | FArcs]; FArcs0 = FArcs),
   remove_supp_arcs(Arcs, FArcs, Max1, Max2).


m_nsq_half(NIn, NOut, Asucc, Apred, AsuccQ, ApredQ, AClasses) :-
   remove_supp_arcs(Asucc, FAsucc, NIn, NOut),
   remove_supp_arcs(Apred, FApred, NIn, NOut),

   m_arcs_to_adjacency_list(FAsucc, ALsucc, NIn),
   m_arcs_to_adjacency_list(FApred, ALpred, NIn),
%   format("Adjacency lists are~N~w~N~w~N", [ALsucc, ALpred]),
   adjacency_merge(ALsucc, ALpred, ALsuccpred),
%   format("Computing inclusions...", []),
   find_inclusions(ALsuccpred, AInclusions0),
%   format("done:~N~w~N", [AInclusions0]),
   left_couple_part_rewrite_pattern(AInclusions0, AInclusions),

   rewrites_term(Asucc, AsuccQ, AInclusions),
   rewrites_term(Apred, ApredQ, AInclusions),

   initial_segment(NIn, AClasses0),
   rewrites_term(AClasses0, AClasses, AInclusions0).


%writes a graph to specified file
write_graph((_NS, _NR, ASR, ARS), Filename) :-
   open(Filename, write, _, [alias(dot)]),
   format(dot, "digraph {~N", []),

   member((A, B), ASR),
   format(dot, "r~d[shape=box];~Ns~d -> r~d;~N", [B, A, B]),
   fail
   ;

   member((A, B), ARS),
   format(dot, "r~d[shape=box];~Nr~d -> s~d;~N", [A, A, B]),
   fail
   ;

   format(dot, "}~N", []),
   close(dot).


display_graph(File) :-
   bg_get_graph(NS, NR, ASR, ARS, _),
   display_graph(NS, NR, ASR, ARS, File).

display_graph(NS, NR, ASR, ARS, File) :-
   open(File, write, _, [alias(out)]),   
   format(out, "~d~N~d~N", [NS, NR]),
   m_arcs_to_adjacency_list(ASR, ALSR, NS),
   member((_Node, NodeSucc), ALSR),
   length(NodeSucc, NodeSuccN),
   format(out, "~d", [NodeSuccN]),
   (
      member(Succ, NodeSucc),
      Succ0 is Succ + NS, %enforce disjoint names for species and reactions
      format(out, " %d", [Succ0]),
      fail
      ;
      nl(out)
   ),
   fail
   ;

   m_arcs_to_adjacency_list(ARS, ALRS, NR),
   member((_Node, NodeSucc), ALRS),
   length(NodeSucc, NodeSuccN),
   format(out, "~d", [NodeSuccN]),
   (
      member(Succ, NodeSucc),
      format(out, " %d", [Succ]),
      fail
      ;
      nl(out)
   ),
   fail
   ;
   close(out).


:-dynamic(bg_species2id/2).


bg_id_of_species(S, I) :-
   parse_molecule(S, NS),
   (
      bg_species2id(NS, Id)
   ->
      I = Id
   ;
      g_inc(bg_species_count, I),
      asserta(bg_species2id(NS, I))
   ).


bg_id_of_stoipair((_N, S), I) :-
   bg_id_of_species(S, I).		


bg_add_arc(S1, S2, A) :-
   g_read(A, AL),
   g_assign(A, [(S1, S2)|AL]).


bg_add_arcs(L1, L2, A) :-
   member(N1, L1),
   member(N2, L2),
   bg_add_arc(N1, N2, A),
   fail
   ;
   true.   


bg_get_graph(NS, NR, ASR, ARS, MNames) :-
   g_assign(bg_species_count, 0),
   g_assign(bg_reactions_count, 0),
   g_assign(bg_sr_arcs, []),
   g_assign(bg_rs_arcs, []),
   retractall(bg_species2id(_, _)),
   
   rule(_, B, C, _, _, _, _),
   g_inc(bg_reactions_count, RCount),
   map_list(B, B1, bg_id_of_stoipair, []),
   map_list(C, C1, bg_id_of_stoipair, []),   
   bg_add_arcs(B1, [RCount], bg_sr_arcs),
   bg_add_arcs([RCount], C1, bg_rs_arcs),
   fail
   ;
   
   g_read(bg_species_count, NS),
   g_read(bg_reactions_count, NR),
   g_read(bg_sr_arcs, ASR),
   g_read(bg_rs_arcs, ARS),
   findall(X,
	   (bg_species2id(Name, Identifier),
	    X = (Identifier, Name)),
	   Names),
   sort(Names, SNames),
   findall(N, member((_, N), SNames), MNames).	   


bg_get_graph_from_file(F, (NS, NR, ASR, ARS), MNames) :-
   clear_rules,
   (
      (
         atom_concat(_, '.xml', F)
      ;
         atom_concat(_, '.sbml', F)
      )
   ->
      format_debug(8, "reading ~w as SBML.~n", [F]),
      load_sbml(F),
      format_debug(8, "done reading ~w as SBML.~n", [F])
   ;
      format_debug(8, "reading ~w as Biocham.~n", [F]),
      load_biocham(F),
      format_debug(8, "done reading ~w as Biocham.~n", [F])
   ),
   bg_get_graph(NS, NR, ASR, ARS, MNames).
