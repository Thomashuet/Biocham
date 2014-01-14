# BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
# Copyright 2003-2006, INRIA, Projet Contraintes
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# Makefile.am

dist_noinst_DATA = bc0.vim get_commands
dist_pkgdata_DATA = bc_indent.vim bc_syntax.vim

bc_syntax.vim : ../biosyntax.pl bc0.vim
	sed -n -e 's/syn keyword Define[ \t]*//p' bc0.vim | tr ' ' '\n' > bc0.vim.tmp
	cp bc0.vim $@
	./get_commands < $< | egrep -v -w -f bc0.vim.tmp | sort | uniq | sed -e 's/^/syn keyword Keyword  /' >> $@
	rm bc0.vim.tmp

TESTS = doc_test.sh

clean :
	$(RM) bc_syntax.vim
