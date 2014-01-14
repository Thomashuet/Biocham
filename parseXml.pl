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
% GNU prolog file parseXml.pl by Sylvain Soliman

:- dynamic(code/1).

% Main predicate: parses file F or F.xml to term T

xml_parse_file(F,T):-
   (
      sub_atom(F,_,1,_,'.')
   ->
      (G=F)
   ;
      atom_concat(F,'.xml',G)
   ),
   open(G,read,S),
   retractall(code(_)),
   repeat,
      get_code(S,C),
      (
         at_end_of_stream(S)
      ->
         assertz(code(C))
      ;
         assertz(code(C)),
         fail
      ),
   !,
   close(S),
   findall(X,code(X),L),
   retractall(code(_)),
   xml_term(L,T).

% Read an XML doc from list of codes X and return it as
% xml(header attributes, body)

xml_term(X,xml(Att,Elt)):-
   read_comment(X,X1),
   read_header(X1,Att,X2),
   xml_elts_term(X2,Elt,L),
   read_comment(L,[]).

%%% Auxiliary functions

is_lower(X):-
   X > 96, X < 123.
   
is_upper(X):-
   X > 64, X < 91.

is_letter(X):-
   is_lower(X),!;
   is_upper(X).

is_digit(X):-
   X > 47, X < 58.

is_namechar(X):-
   is_letter(X),!;
   is_digit(X),!;
   X == 95,!;  %_
   X == 58,!;  %:
   X == 46,!;  %.
   X == 45,!.  %-

is_white(X):-
   X == 32,!;  % ' '
   X == 10,!;  % \n
   X == 13,!;  % \r
   X == 9,!.   % \t

% consume whitespace

read_white([X|L1],L):-
   is_white(X),!,
   read_white(L1,L).

read_white(L,L).

remove_final_white(L1, L2):-
   reverse(L1, L),
   read_white(L, LL),
   reverse(LL, L2).

%%%

% Read XML header

read_header([60,63,120,109,108|L],Att,X) :-  % <?xml
   xml_att_term(L,Att,L1),
   L1=[63,62|X].                             % ?>

% read a sequence of attributes="value"

xml_att_term(L,Att,L1):-
   read_white(L,[H|T]),
   (
      is_namechar(H)
   ->
      xml_att([H|T],A,L2),
      xml_att_term(L2,Atts,L1),
      Att=[A|Atts]
   ;
      Att=[],
      L1=L
   ).

xml_att(L,A,L1):-
   read_att(L,A1,L2),
   read_val(L2,A2,L1),
   atom_codes(AA1,A1),
   atom_codes(AA2,A2),
   A=(AA1,AA2).

% get att name until ="

read_att([61, 34 | L], [], [34 | L]) :- !.  % ="

read_att([61, 39 | L], [], [39 | L]) :- !.  % ='

read_att([H | T], [H | A], L) :-
   is_namechar(H),
   read_att(T, A, L).


% get value until " or ' (head of the first argument)
read_val([X | V], A, L) :-
   read_val2(V, A, L, X).


% read expected end
read_val2([X | L], [], L, X) :- !.
read_val2([H | T], [H | A], L, X) :-
   read_val2(T, A, L, X).


% get children, thus a sequence of xml tags

xml_elts_term(L,Elts,L1):-
   xml_elt_term(L,Elt,L2),!,
   %write('read'(Elt)),nl,
   Elts=[Elt|E],
   xml_elts_term(L2,E,L1).

xml_elts_term(L,[],L).

% get one XML tag

xml_elt_term(L,Elt,L1):-
   read_comment(L,L2),
   xml_elt_term2(L2,Elt,L1).

% special tags <!...

xml_elt_term2([60,33|L],Elt,L1):-   % <!
   !,
   read_special_name(L,E,L2),       % ENTITY ELEMENT ATTLIST ...
   atom_codes(E1,E),
   Elt=..[E1,[],[]],                % TODO: not throw all away 
   read_special_content(L2,L1).

% standard element

xml_elt_term2(L,Elt,L1):-
   read_elt_name(L,E,L2,EL),!,
   xml_att_term(L2,Att,L3),
   read_white(L3,L4),
   read_end_elt(L4,X,L1,EL),
   Elt=.. [E,Att,X].

% textual element, saved as '$text(...)'

xml_elt_term2(L,'$text'([(value,Elt)],[]),L1):-
   read_white(L,L2),
   read_text(L2,E,L1),
   remove_final_white(E, E2),
   atom_codes(Elt,E2).

% read the element's name

read_elt_name([60|L],E,L1,Elt):- % <
   read_elt(L,Elt,L1),
   (
      append(_NameSpace,[58|Elt1],Elt) % ':' ns separator
   ->
      true
   ;
      Elt1=Elt
   ),
   atom_codes(E,Elt1).

read_elt([H|T],[H|A],L):-
   is_namechar(H),!,
   read_elt(T,A,L).

read_elt(L,[],L).

% end of element, simple or with children

read_end_elt([47,62|L],[],L,_):-!.  % />

read_end_elt([62|L],Elts,L1,E):-    % >
   xml_elts_term(L,Elts,L2),
   read_white(L2,L3),
   append([60,47|E],">",LE),        % </
   append(LE,L1,L3).

% Comments

read_comment(L,L1):-
   read_white(L,[60,33,45,45|L2]),  % <!--
   !,
   read_end_comment(L2,L1).

read_comment(L,L1):-
   read_white(L,L1).

read_end_comment([45,45,62|L],L1):-  % -->
   !,read_white(L,L1).

read_end_comment([_|T],L):-
   read_end_comment(T,L).

% Read textual content up to a new '<'

read_text([60|L],[],[60|L]):-!.     % <

read_text([H|T],[H|A],L):-
   read_text(T,A,L).

% read the name of a special (<!...>)

read_special_name([H|T],[H|A],L):-
   is_upper(H),!,
   read_special_name(T,A,L).

read_special_name(L,[],L).

% throw away its content

read_special_content([62|L],L):-!.  % >

read_special_content([91|L],L1):-   % [
   !,read_to_square_bracket(L,L2),
   read_special_content(L2,L1).

read_special_content([_|L],L1):-
   read_special_content(L,L1).

% ignore what is in square brackets, including >
% if get ]] get that too (for CDATA and such

read_to_square_bracket([93,93|L],L):-!.   % ]]

read_to_square_bracket([93|L],L):-!.   % ]

read_to_square_bracket([_|L],L1):-
   read_to_square_bracket(L,L1).

%%%

% Pretty printing

% opens a file

xml_write_file(F,X) :-
	(sub_atom(F,_,_,0,'.') -> (G=F) ; atom_concat(F,'.xml',G)),
   open(G, write, S, []),
   write_xml(S,X),
   close(S).

% if no Stream given, use user_output

write_xml(X) :-
   write_xml(user_output,X).

% writes to a stream

write_xml(S,xml(Atts,Elts)):-
   write(S,'<?xml'),
   write_atts(S,Atts),
   write(S,'?>\n'),
   g_assign(xml_indent,0),
   write_xml_elts(S,Elts).

% writes a sequence of pairs att,value
% if more than 2 atts, newline for each

write_atts(S,L):-
   length(L,N),
   (
      (N > 2)
   ->
      g_read(xml_indent,I),
      J is I+3,
      make_spaces(J,K),
      format_to_atom(Sep,"~n~S",[K])
   ;
      (Sep = ' ')
   ),
   write_atts2(S,L,Sep).

write_atts2(_,[],_).

write_atts2(S,[(A,V)|L],Sep):-
   write(S,Sep),
   write(S,A),
   write(S,'="'),
   write(S,V),
   write(S,'"'),
   write_atts2(S,L,Sep).

% write an element

write_xml_elts(_,[]).

write_xml_elts(S,[E|L]):-
   E=..[Elt,Atts,Children],
   write_xml_elt(S,Elt,Atts,Children),
   write_xml_elts(S,L).

% case of the special element $text

write_xml_elt(S,'$text',[(value,L)],[]):-
   !,
   g_read(xml_indent,I),
   write_spaces(S,I),
   write(S,L),nl(S).

% normal element

write_xml_elt(S,E,A,C):-
   g_read(xml_indent,I),
   write_spaces(S,I),
   write(S,'<'),
   write(S,E),
   write_atts(S,A),
   (
      (C == [])
   ->
      write(S,'/>\n')
   ;
      write(S,'>\n'),
      J is I+3,
      g_assign(xml_indent,J),
      write_xml_elts(S,C),
      g_assign(xml_indent,I),
      write_spaces(S,I),
      write(S,'</'),
      write(S,E),
      write(S,'>\n')
   ).

write_spaces(S,N):-
   make_spaces(N,L),
   format(S,"~S",[L]).

make_spaces(0,[]):-!.

make_spaces(I,[' '|L]):-
   J is I-1,
   make_spaces(J,L).

