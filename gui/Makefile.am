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

JAVAROOT = .
JAVAC="@JAVAC@"
dist_pkgdata_DATA = biocham.jar $(filter-out biocham.jar,$(wildcard *.jar))
JAVACFLAGS= -classpath @JAVAC_ADDS@ 
JAVAPKG = fr/inria/contraintes/biocham
CLEANFILES = $(pkgdata_DATA)
EXTRA_DIST =../biocham_gui.in commandLine/BiochamCommands.java.in plotting/pltTemplate.flt $(wildcard commandLine/*.java) $(wildcard customComponents/*.java) $(wildcard dialogs/*.java) $(wildcard documentation/*.java) $(wildcard filters/*.java) $(wildcard graphicalEditor/*.java) $(wildcard menus/*.java) $(wildcard modelData/*.java) $(wildcard parsers/*.java) $(wildcard plotting/*.java) $(wildcard projectImages/*) $(wildcard resources/*) $(wildcard tutorials/*) $(wildcard utils/*.java) MANIFEST.mf $(wildcard *.jar) $(wildcard *.java)
LAUNCH4J=@LAUNCH4J@

SUFFIXES = .java .class

BiochamCommands.java: 

Biocham.ico : projectImages/SSicon.png
	-$(ICOTOOL) $@ $<


biocham.jar: MANIFEST.mf $(wildcard projectImages/*) $(wildcard resources/*) $(wildcard plotting/pltTemplate.flt) plotting/pltTemplate.flt ../biosyntax.pl
	
	if [ ! `command -v perl` ] ; then \
	   cp BiochamCommands.java.in BiochamCommands.java; echo "copied." ;\
	else \
	   ./build_commands ; \
	fi

	$(JAVAC) -d $(JAVAROOT) $(JAVACFLAGS) *.java commandLine/*.java customComponents/*.java dialogs/*.java documentation/*.java filters/*.java graphicalEditor/*.java menus/*.java modelData/*.java parsers/*.java plotting/*.java utils/*.java
	if ! test -e $(JAVAPKG)/projectImages; then \
	   mkdir $(JAVAPKG)/projectImages; \
	fi
	cp projectImages/* $(JAVAPKG)/projectImages/
	cp plotting/pltTemplate.flt $(JAVAPKG)/plotting/pltTemplate.flt
	for file in $$(find resources -not -regex '.*\.svn.*') ; do \
	   if [ -d "$$file" ] ; then \
		   mkdir -p "$(JAVAPKG)/$$file" ; \
		else \
		   cp -f "$$file" "$(JAVAPKG)/$$file" ; \
		fi ; \
	done
	jar cmf MANIFEST.mf $@ $(JAVAPKG)
	
BiochamGUI.exe : Biocham_l4j.xml Biocham.ico biocham.jar
	-$(LAUNCH4J) "@SRCFULLDIR@gui/$<"

clean:
	$(RM) $(JAVAPKG)/*.class $(JAVAPKG)/commandLine/*.class  $(JAVAPKG)/customComponents/*.class $(JAVAPKG)/dialogs/*.class $(JAVAPKG)/documentation/*.class $(JAVAPKG)/filters/*.class $(JAVAPKG)/graphicalEditor/*.class $(JAVAPKG)/menus/*.class $(JAVAPKG)/modelData/*.class $(JAVAPKG)/parsers/*.class $(JAVAPKG)/plotting/*.class $(JAVAPKG)/utils/*.class
	$(RM) $(JAVAPKG)/projectImages/*
	rm -f classdist_noinst.stamp
	rm -f biocham.jar
	rm -f BiochamGUI.exe
	rm -f BiochamCommands.java
	rm -rf fr
