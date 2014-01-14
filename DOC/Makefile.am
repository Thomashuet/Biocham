# BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
# Copyright 2003-2009, INRIA, Projet Contraintes
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

MANUALS = manual.pdf manual.html
#nobase_pkgdata_DATA = $(MANUALS) $(wildcard *.png)
docdir = $(datadir)/DOC
doc_DATA = $(MANUALS) $(wildcard *.png)
EXTRA_DIST = $(MANUALS) $(wildcard *.png)

SUFFIXES = .html .pdf .tex

all: $(MANUALS)

.tex.html:
	"$(TEX2HTML)" $(T2HOPTS) $<

.tex.pdf:
	pdflatex $<
	bibtex `echo $< | sed s/\.tex$$//` 
	pdflatex $<
	pdflatex $<
	makeindex -c `echo $< | sed s/\.tex$$//`
	pdflatex $<
	pdflatex $<

clean:
	rm manual.pdf manual.html
