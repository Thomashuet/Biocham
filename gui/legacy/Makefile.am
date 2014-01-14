# BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
# Copyright 2003-2010, INRIA, Projet Contraintes
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

JAVAROOT = .
JAVAC="@JAVAC@"
dist_noinst_JAVA = $(wildcard *.java) BiochamCommands.java BiochamApp.java
dist_pkgdata_DATA = biocham_legacy.jar
JAVAPKG = fr/inria/contraintes/biocham
CLEANFILES = $(pkgdata_DATA)
EXTRA_DIST = ../../biocham_gui.in BiochamUpdater.java.in BiochamCommands.java.in BiochamApp.java.mac BiochamApp.java.else $(wildcard images/*) Manifest.txt

BiochamCommands.java: BiochamCommands.java.in ../../biosyntax.pl
	if [ ! `command -v perl` ] ; then \
	   cp BiochamCommands.java.in BiochamCommands.java; \
	else \
	   ../build_commands ; \
	fi

BiochamApp.java: BiochamApp.java.mac BiochamApp.java.else ../../config.status
	if test `uname` = Darwin ; then \
		cp BiochamApp.java.mac $@; \
	else \
		cp BiochamApp.java.else $@; \
	fi

SUFFIXES = .java .class

classnoinst.stamp: $(dist_noinst_JAVA)

biocham_legacy.jar: Manifest.txt BiochamCommands.java BiochamApp.java
	mkdir -p $(JAVAPKG)
	$(JAVAC) -d $(JAVAROOT) $(dist_noinst_JAVA)
	if test -L $(JAVAPKG)/images ; then \
		rm $(JAVAPKG)/images ; mkdir $(JAVAPKG)/images; \
		fi
	if ! test -e $(JAVAPKG)/images ; then \
		mkdir $(JAVAPKG)/images; \
		fi
	cp -f images/*.png $(JAVAPKG)/images
	jar cmf Manifest.txt $@ $(JAVAPKG)/*.class $(JAVAPKG)/images/*.png
	$(RM) classdist_noinst.stamp

clean:
	$(RM) *.class $(JAVAPKG)/*.class
	$(RM) classdist_noinst.stamp
	$(RM) biocham_legacy.jar
	$(RM) BiochamCommands.java

