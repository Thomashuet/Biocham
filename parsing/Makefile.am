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

EXTRA_DIST = $(wildcard *.pl)

clean :
	rm -f *.o core *.core

compare :
	@for f in lazylist.pl parsing.pl regexp.pl lexing.pl; do \
	  echo '===================='; \
	  echo $$f; \
	  (sed '/This file has/,$$d' $$f; \
	   cat ../../silcc/test/parsing/gprolog_src/$$f) | diff $$f -; \
	done

copy :
	@for f in lazylist.pl parsing.pl regexp.pl lexing.pl; do \
	  echo '===================='; \
	  echo $$f; \
	  (sed '/This file has/,$$d' $$f; \
	   cat ../../silcc/test/parsing/gprolog_src/$$f) > tmp; \
	  cmp -s $$f tmp || \
	  (echo 'files differ => copy...'; mv tmp $$f); \
	done
