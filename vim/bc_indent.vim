" BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
" Copyright 2003-2005, INRIA, Projet Contraintes
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
" Vim indent file
" Language:     Biocham
" Maintainer:   Sylvain Soliman <Sylvain.Soliman@inria.fr>
" Last Change:	 2004 January 19

if exists("b:did_indent")
   finish
endif

let b:did_indent = 1

setlocal indentexpr=GetBCIndent()
setlocal nolisp
setlocal nosmartindent
setlocal autoindent
setlocal indentkeys-=:,0#,0),0},0{,e
setlocal indentkeys+=0%,\.,0=where,0=and,0=for,0=if,0=then,0=else,*<Return>


" Only define the function once
if exists("*GetBCIndent")
   finish
endif

function GetBCIndent()
   " Find a non-blank line above the current line.
   let pnum = prevnonblank(v:lnum - 1)

   " At the start of the file use zero indent.
   if pnum == 0
      return 0 
   endif

   let pline = getline(pnum)             " last line
   let cline = getline(v:lnum)          " current line
   let ind = indent(pnum)

   " If previous line was a comment, use same indent
   if pline =~ '^\s*%'
      return ind
   endif

   " if previous line ended a rule/definition, use 0
   if pline =~ '\.\s*$'
      return 0
   endif

   " if current line starts with 'for' -> constant indent of 1 sw
   if cline =~ '^\s*\(for\)'
      return &sw
   endif

   " if current line starts with 'and' and previous line
   " starts with 'where' -> keep same
   if cline =~ '^\s*\(and\)' && pline =~ '^\s*\(where\)'
      return ind
   endif

   " if current line starts with 'else' and previous line
   " starts with 'then' -> keep same
   if cline =~ '^\s*\(else\)' && pline =~ '^\s*\(then\)'
      return ind
   endif

   " else add a shiftwidth
   return ind + &sw
endfunction

