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
# 
# put independent files in biocham_SOURCES
# put files necessary to get biocham.o (i.e. files included by biocham.pl) in
# PLFILES

@SET_MAKE@
export RM = rm -f
FLAGS2 = --no-top-level --min-fd-bips --strip --global-size 32768
FLAGS3 = @LDFLAGS@
EXTRA_SCRIPTS = biocham_gui
bin_SCRIPTS = @GUIBINS@
bin_PROGRAMS = biocham
biocham_SOURCES = biocham.pl biochamLotos.pl formal.pl parseXml.pl biosyntax.pl path.pl biochamType.pl biochamConserv.pl biochamAnalyze.pl biochamTrace.pl biochamPinvar.pl biochamGRN.pl biochamMatlab.pl biochamBioPepa.pl biochamHybrid.pl fss.c fss_biocham.c ppl_biocham.c biocham_gsa.c cmaes.h cmaes_interface.h fss.h fss_interface.h spawn_redirect.pl spawn_redirect_c.c cmaes.c cmaes_biocham.c cmaes_biocham.h biocham_mpi.c biocham_mpi.h
PARSING = $(patsubst %.pl,%.o,$(wildcard parsing/*.pl))
biocham_LDADD = $(PARSING) @LIBS_ADDS@ $(FLAGS3)

dist_noinst_DATA = index.html gui/Biocham.ico $(wildcard Biocham*.icns) $(wildcard Biocham*.tiff)

appsdir = $(datadir)/applications
dist_apps_DATA = biocham.desktop
pixmapsdir = $(datadir)/pixmaps
dist_pixmaps_DATA = biocham.png
mimedir = $(datadir)/mime-info
dist_mime_DATA = biocham.mime biocham.keys
sharedmimedir = $(datadir)/mime/packages
dist_sharedmime_DATA = biocham.xml

dist_man_MANS = biocham.1

biocham_LINK = $(GPLC) -o biocham $(FLAGS2) $(GPLCFLAGS)

PLFILES = $(filter-out $(biocham_SOURCES),$(wildcard *.pl))

# include both executables if present and sources of biocham.pl
EXTRA_DIST = $(PLFILES) BiochamUpdates.xml

TAGS_FILES = $(PARSING) $(PLFILES)

SUBDIRS = EXAMPLES DOC parsing @VIM@ @EMACS@ @GUIDIR@

.PHONY: deb minicheck BiochamGUI

SUFFIXES = .pl .o

CLEANFILES = $(dist_man_MANS) $(bin_SCRIPTS) biocham.iss Biocham_l4j.xml biocham.png

biocham.o : $(PLFILES)

biochamAnalyze.o : biochamAnalyze.pl biocham_gsa.c fss.c fss_biocham.c ppl_biocham.c
	$(GPLC) -c $(GPLCFLAGS) $^

spawn_redirect.o : spawn_redirect.pl spawn_redirect_c.c
	$(GPLC) -c $(GPLCFLAGS) $^

.pl.o:
	$(GPLC) -c $(GPLCFLAGS) $<

.c.o:
	$(GPLC) -c $(GPLCFLAGS) $<

ChangeLog :
	svn -v log > $@

biocham.1: biocham.pl biochamVars.pl
	-$(HELP2MAN) --output=$@ -N ./biocham

deb: dist
	cp $(distdir).tar.gz ../$(subst B,b,$(PACKAGE))_$(VERSION).orig.tar.gz
	dpkg-buildpackage -rfakeroot -i'.*(png|o)'
	mv ../$(subst B,b,$(PACKAGE))_$(VERSION)* .

biocham_gui: biocham_gui.in Makefile
	sed -e 's,@datadir\@,$(pkgdatadir),g' -e 's,@JARDIR\@,$(JARDIR),g' $< > $@
	chmod a+x $@

minicheck: $(bin_PROGRAMS)
	for file in EXAMPLES/cell_cycle/*.sh EXAMPLES/LEARNING/*/*.sh ; do mv $$file $$file.check; done
	($(MAKE) -C EXAMPLES check && \
		for file in EXAMPLES/cell_cycle/*.sh.check EXAMPLES/LEARNING/*/*.sh.check ; do mv $$file $${file%.check}; done) || \
		(for file in EXAMPLES/cell_cycle/*.sh.check EXAMPLES/LEARNING/*/*.sh.check ; do mv $$file $${file%.check}; done && exit 1)

Biocham.icns : Biocham.tiff
	-tiff2icns $<

Biocham.tiff : Biocham16.tiff Biocham32.tiff Biocham48.tiff Biocham128.tiff
	-tiffutil -cat $^ -out $@

BiochamSetup.exe : biocham.iss biocham.exe BiochamGUI
	rm -Rf .dll
	mkdir .dll
	true # start dot, NuSMV and biocham for 10 sec
	((sleep 10 ; echo "quit") | NuSMV.exe -int &> /dev/null) &
	((sleep 10 ; echo "quit.") | ./biocham.exe &> /dev/null) &
	((sleep 10 ; echo "") | dot.exe &> /dev/null) &
	@true # grab all "modules" used by the running programs and not coming from
	@true # standard places (removed Program Files)
	@true # - uses Mark Russinovich's ListDLLs.exe (http://download.sysinternals.com/files/ListDlls.zip) for Win7
	@true # - uses pv.exe (http://www.teamcti.com/pview/prcview.htm) for WinXP
	@true # both utilities hereinbefore theorically work either on Win7 and WinXP, but pv doesn't see processes launched from shell in MinGW, and Listdlls doesn't give full path of dlls in WinXP
	if [ @EXTRACT_DLL_TOOL@ = pv ] ; then \
	(pv.exe -m NuSMV.exe ; pv.exe -m biocham.exe ; pv.exe -m dot.exe) | grep -i '\.dll' | egrep -iv 'system32|comctl32' | sed -n -e 's/^[^ ]*\.\w* \+\w* \+\w* \+\(.*\)$$/\1/p' | (while read -r dll ; do cp "$$dll" .dll ; done); \
	else \
	(Listdlls.exe NuSMV.exe ; Listdlls.exe biocham.exe ; Listdlls.exe dot.exe) | grep -i '\.dll' | egrep -iv 'system32|comctl32|syswow64|mingw\\|winsxs' | sed -n -e 's/^\w*\s*\w*\s*\(.*\)$$/\1/p' | (while read -r dll ; do cp "$$dll" .dll ; done); \
	fi
	extNuSMV=`which NuSMV.exe` && extNuSMV=`readlink -f "$$extNuSMV"` && cp "$$extNuSMV" .dll # grab NuSMV executable
	extDOT=`which dot.exe` && extDOT=`ls "$$extDOT"`&& extDOT=`readlink -f "$$extDOT"` && cp "$$extDOT" .dll && cp -r "$${extDOT%dot.exe}config6" .dll && mkdir .dll/etc && dot="$${extDOT%bin/dot.exe}" && mkdir .dll/lib&& cp -r "$${dot}lib/pango" .dll/lib # grab dot and some auxiliary files
	sleep 5
	ISCC.exe $< # run InnoSetup

biocham.png : gui/projectImages/SSicon.png
	cp $< $@

Biocham.dmg : biocham Biocham.icns Info.plist PkgInfo gui/biocham.jar dot.sh $(nobase_pkgdata_DATA)
	rm -Rf .Biocham_MacOSX/
	mkdir .Biocham_MacOSX/
	cp -r EXAMPLES .Biocham_MacOSX/
	mkdir -p .Biocham_MacOSX/Biocham.app/Contents/MacOS/
	cp Info.plist PkgInfo .Biocham_MacOSX/Biocham.app/Contents/
	cp /System/Library/Frameworks/JavaVM.framework/Versions/Current/Resources/MacOS/JavaApplicationStub .Biocham_MacOSX/Biocham.app/Contents/MacOS/
	mkdir -p .Biocham_MacOSX/Biocham.app/Contents/Resources/
	cp -r DOC Biocham.icns .Biocham_MacOSX/Biocham.app/Contents/Resources/
	cp biocham .Biocham_MacOSX/Biocham.app/Contents/Resources/biocham.mac
	cp `which NuSMV` .Biocham_MacOSX/Biocham.app/Contents/Resources/NuSMV
	cp dot.sh .Biocham_MacOSX/Biocham.app/Contents/Resources/dot
	for file in `DYLD_PRINT_LIBRARIES=1 ./biocham EXAMPLES/quit.bc 2>&1 | grep '/local/' | cut -d ' ' -f 3`; do cp -f $$file .Biocham_MacOSX/Biocham.app/Contents/Resources/ ; done
	echo "[InternetShortcut]" > .Biocham_MacOSX/Install\ AT\&T\ Graphviz.url
	echo "http://www.graphviz.org/Download_macos.php" >> .Biocham_MacOSX/Install\ AT\&T\ Graphviz.url
	cp biocham.sh .Biocham_MacOSX/Biocham.app/Contents/Resources/biocham
	mkdir -p .Biocham_MacOSX/Biocham.app/Contents/Resources/Java/
	cp gui/*.jar .Biocham_MacOSX/Biocham.app/Contents/Resources/Java/
	cp gui/legacy/*.jar .Biocham_MacOSX/Biocham.app/Contents/Resources/Java/
	ln -s /Applications .Biocham_MacOSX/
# hdiutil create -format UDBZ -scrub -srcfolder .Biocham_MacOSX -volname Biocham -ov Biocham.dmg
	hdiutil create -scrub -srcfolder .Biocham_MacOSX -volname Biocham -ov Biocham.dmg
	hdiutil internet-enable -yes Biocham.dmg
	rm -Rf .Biocham_MacOSX

dist-hook:
	cp -f $(wildcard biocham) $(wildcard *.exe) $(wildcard *.dmg) $(distdir)

install-data-hook :
	-$(UPDMIMEDB) $(sharedmimedir)/..
	-$(UPDDESKDB)

uninstall-hook :
	-$(UPDMIMEDB) $(sharedmimedir)/..
	-$(UPDDESKDB)

spawn_redirect_test: spawn_redirect_test.pl spawn_redirect.pl spawn_redirect_c.c
	$(GPLC) $(GPLCFLAGS) --no-top-level -o spawn_redirect_test \
	  spawn_redirect_test.pl spawn_redirect.pl spawn_redirect_c.c

sepi.dll: biocham.o parseXml.o biosyntax.o path.o formal.o spawn_redirect.o biochamTrace.o biochamConserv.o $(PARSING) spawn_redirect_c.c sepi_dassault.c
	$(GPLC) -o $@ $(patsubst --strip,,$(FLAGS2)) $(GPLCFLAGS) -L -shared $^ $(FLAGS3)

sepi.lib: sepi.dll
	echo "EXPORTS" > sepi.def && nm sepi.dll | sed -n -e's/^.* T _\(.*\)$$/\1/p' >> sepi.def
	dlltool --def sepi.def --dllname sepi.dll --output-lib sepi.lib

dassault.zip: sepi.dll sepi.exe
	rm -f $@
	zip -j $@ $^ `ldd $< | sed -n -e 's/^.*=> \(\/usr\/bin\/.*\.dll\).*$$/\1/p'`

sepi.exe: sepi.c sepi.dll
	rm -f sepi.lib
	gcc -o $@ $< -L. -lsepi

BiochamGUI:	gui/Makefile
	cd gui && make BiochamGUI.exe