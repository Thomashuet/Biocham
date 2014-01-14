# BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
# Copyright 2003-2012, INRIA, Projet Contraintes
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
# configure.ac

AC_INIT([Biocham],[3.5],[biocham@inria.fr],[Biocham])

dnl dirty hack to check for AM version 1.11 or older
m4_ifdef(
   [AM_SILENT_RULES],
   [m4_define([my_color_tests], [color-tests])],
   [m4_define([my_color_tests], [])]
)
AM_INIT_AUTOMAKE([dist-bzip2 -Wno-portability ]my_color_tests)
dnl we have to use silent rules otherwise the ifdef will fail
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES])

AC_PREREQ([2.53])

AC_PROG_INSTALL
AC_CANONICAL_HOST

dnl find and test the C compiler, set CC and CFLAGS
AC_PROG_CC
AC_LANG([C])

has_cygwin=`uname -a | grep -i Cygwin`
has_mingw=`uname -a | grep -i mingw`

if test "$(uname)" = "MINGW32_NT-5.1" ; then
	AC_SUBST(EXTRACT_DLL_TOOL, "pv")
else
	AC_SUBST(EXTRACT_DLL_TOOL, "listdll")
fi

dnl look for progs in the path and exit if not found
AC_ARG_WITH([gplc],
            AC_HELP_STRING([--with-gplc=PATH],
                           [location of the gplc executable]),
            [GPLC=$withval],
            [AC_PATH_PROG([GPLC],[gplc])])
if test "x$GPLC" = "x" -o "x$GPLC" = "xno" -o ! -x "$GPLC" ; then
   AC_MSG_ERROR([Cannot find gplc in your PATH or cannot execute provided gplc,
                 but gplc is needed to build biocham])
fi
gplc_version=`$GPLC --version 2>&1 | head -n 1 | cut -d ' ' -f 5`
gplc_major=`echo $gplc_version | cut -d '.' -f 2`
gplc_minor=`echo $gplc_version | cut -d '.' -f 3`
if test $gplc_major -lt 3 ; then
   AC_MSG_ERROR([gplc version >= 1.3.1 is needed, found $gplc_version])
elif test $gplc_major -eq 3 -a $gplc_minor -lt 1 ; then
   AC_MSG_WARN([gplc version >= 1.3.1 is needed to handle CTRL-C properly])
   AC_SUBST(GPLC_CTRLC, "%%% ")
   AC_SUBST(GPLC_MSORT, "")
elif test $gplc_major -eq 3 -a $gplc_minor -eq 1 ; then
   AC_SUBST(GPLC_CTRLC, "")
   AC_SUBST(GPLC_MSORT, "")
else
   AC_SUBST(GPLC_CTRLC, "")
   AC_SUBST(GPLC_MSORT, "%%% ")
fi
if test "$has_cygwin" != "" ; then
   AC_SUBST([GPLC_PATCHED],["%%% "])
   AC_SUBST(LIBS_ADDS," -L -lppl_c.dll -L -lppl.dll -L -lgmpxx.dll -L -lgmp.dll")
else
   AC_SUBST([GPLC_PATCHED],[""])
   AC_SUBST(LIBS_ADDS," -L -lppl_c -L -lppl -L -lgmpxx -L -lgmp -L -lstdc++ ")
fi
GPLCFLAGS="-C -Wall"

dnl look for gmp and ppl libraries for biochamAnalyze
dnl second -l for gplc preprocessing
AC_ARG_WITH([gmp],
            AC_HELP_STRING([--with-gmp=PATH],
                           [prefix location of the gmp library]),
            [GMP=$withval],
            [AC_CHECK_LIB([gmp], [__gmp_printf])])
if test "$ac_cv_lib_gmp___gmp_printf" != "yes" ; then
   if test "$GMP" != "no" -a -n "$GMP" ; then
      dnl second -l for gplc preprocessing
      LDFLAGS="-L -L$GMP $LDFLAGS"
   else
      AC_MSG_ERROR([Cannot find the gmp library])
   fi
fi

AC_ARG_WITH([ppl],
            AC_HELP_STRING([--with-ppl=PATH],
                           [prefix location of the ppl library]),
            [PPL=$withval],
            [AC_CHECK_LIB([ppl_c], [ppl_initialize], [], [], [-lppl -lgmpxx -lgmp -lstdc++])])
if test "$ac_cv_lib_ppl_c_ppl_initialize" != "yes" ; then
   if test "$PPL" != "no" -a -n "$PPL" ; then
      dnl second -l for gplc preprocessing
      LDFLAGS="-L -L$PPL $LDFLAGS"
   else
      AC_MSG_ERROR([Cannot find the ppl library])
   fi
fi

dnl LIBS="spawn_redirect.pl spawn_redirect_c.c"
LIBS=''
dnl LDFLAGS="-L -lstdc++ $LDFLAGS"

dnl check for perl
AC_ARG_WITH([perl],
            AC_HELP_STRING([--with-perl=PATH],
                           [location of the perl executable]),
            [PERL=$withval],
            [AC_PATH_PROG([PERL],[perl])])
if test "x$PERL" = "x" ; then
   AC_MSG_WARN([Cannot find perl in your PATH,])
   AC_MSG_WARN([you will not be able to build the emacs/vim modes.])
else
   if test "x$PERL" != "xno" ; then
      if test ! -x $PERL ; then
         AC_MSG_ERROR([provided perl $PERL not found/executable])
      fi
      AC_SUBST(VIM,[vim])
      AC_SUBST(EMACS,[emacs])
   else
      AC_MSG_NOTICE([Skipping perl...])
   fi
fi

dnl check for javac
AC_ARG_WITH([javac],
            AC_HELP_STRING([--with-javac=PATH],
                           [location of the javac executable]),
            [JAVAC=$withval],
            [AC_PATH_PROG([JAVAC],[javac])])
if test "x$JAVAC" = "x" ; then
   AC_MSG_WARN([Cannot find javac in your PATH,])
   AC_MSG_WARN([you will not be able to build the GUI.])
else
   if test "x$JAVAC" != "xno" ; then
      if test ! -x "$JAVAC" ; then
         AC_MSG_ERROR([provided javac $JAVAC not found])
      fi
      javajava=`echo $JAVAC | sed -e 's/javac/java/'`
      javac_version=`"$javajava" -version 2>&1 | head -n 1 | cut -d ' ' -f 3`
      javac_v2=`echo $javac_version | cut -d '.' -f 2`
      if test $javac_v2 -lt 5 ; then
         AC_MSG_WARN([javac version >= 1.5.0 is needed, found $javac_version])
      else
         if test "$has_cygwin" != "" ; then
            AC_SUBST(JAVAC_ADDS,"\"`cygpath -wp poi-3.5-beta5-20090219.jar:balloontip_2009-03-19.jar:commons-collections-3.2.1.jar:freemarker.jar:MultiSplit.jar:plot.jar:PDFRenderer-2009_08_09.jar:jgraph.jar:dtdparser121.jar:jox-1.17b3.jar:xerces.jar:jfreechart-1.0.13.jar:jcommon-1.0.16.jar`\"")
            AC_SUBST(JARDIR,['`cygpath -wp $datadir`'])
         elif test "$has_mingw" != "" ; then
            AC_SUBST(JAVAC_ADDS," poi-3.5-beta5-20090219.jar\;balloontip_2009-03-19.jar\;freemarker.jar\;commons-collections-3.2.1.jar\;MultiSplit.jar\;plot.jar\;PDFRenderer-2009_08_09.jar\;jgraph.jar\;dtdparser121.jar\;jox-1.17b3.jar\;xerces.jar\;jfreechart-1.0.13.jar\;jcommon-1.0.16.jar")
         else
            AC_SUBST(JAVAC_ADDS," poi-3.5-beta5-20090219.jar:balloontip_2009-03-19.jar:freemarker.jar:commons-collections-3.2.1.jar:MultiSplit.jar:plot.jar:PDFRenderer-2009_08_09.jar:jgraph.jar:dtdparser121.jar:jox-1.17b3.jar:xerces.jar:jfreechart-1.0.13.jar:jcommon-1.0.16.jar")
            AC_SUBST(JARDIR,['$(pkgdatadir)'])
         fi
         AC_SUBST(GUIDIR,["gui gui/legacy"])
         AC_SUBST(GUIBINS,[biocham_gui])
      fi
   else
      AC_MSG_NOTICE([Skipping javac...])
   fi
fi

dnl check for help2man
AC_ARG_WITH([help2man],
            AC_HELP_STRING([--with-help2man=PATH],
                           [location of the help2man executable]),
            [HELP2MAN=$withval],
            [AC_PATH_PROG([HELP2MAN],[help2man])])
if test "x$HELP2MAN" = "x" ; then
   AC_MSG_WARN([Cannot find help2man in your PATH,])
   AC_MSG_WARN([you will not be able to build the man page.])
   HELP2MAN=false
else
   if test "x$HELP2MAN" != "xno" ; then
      if test ! -x $HELP2MAN ; then
         AC_MSG_ERROR([provided help2man $HELP2MAN not found/executable])
      fi
   else
      AC_MSG_NOTICE([Skipping help2man...])
      HELP2MAN=false
   fi
fi

dnl check for .ico building tool
AC_ARG_WITH([icotool],
            AC_HELP_STRING([--with-icotool=PATH],
                           [location of the icotool executable]),
            [ICOTOOL=$withval],
            [AC_PATH_PROG([ICOTOOL],[icotool])])
if test "x$ICOTOOL" = "x" ; then
   AC_MSG_WARN([Cannot find icotool in your PATH,])
   ICOTOOL=false
else
   if test "x$ICOTOOL" != "xno" ; then
      if test ! -x $ICOTOOL ; then
         AC_MSG_ERROR([provided icotool $ICOTOOL not found/executable])
      else
         ICOTOOL="$ICOTOOL -c -o"
      fi
   else
      AC_MSG_NOTICE([Skipping icotool...])
      ICOTOOL=false
   fi
fi
if test "x$ICOTOOL" = "xfalse" ; then
   AC_ARG_WITH([png2ico],
               AC_HELP_STRING([--with-png2ico=PATH],
                              [location of the png2ico executable]),
               [ICOTOOL=$withval],
               [AC_PATH_PROG([ICOTOOL],[png2ico])])
   if test "x$ICOTOOL" = "x" ; then
      AC_MSG_WARN([Cannot find png2ico in your PATH,])
      AC_MSG_WARN([you will not be able to build the .ico file.])
      ICOTOOL=false
   else
      if test "x$ICOTOOL" != "xno" ; then
         if test ! -x $ICOTOOL ; then
            AC_MSG_ERROR([provided png2ico $ICOTOOL not found/executable])
         else
            ICOTOOL="$ICOTOOL"
         fi
      else
         AC_MSG_NOTICE([Skipping png2ico...])
         ICOTOOL=false
      fi
   fi
fi

dnl check for .tex -> .html converter
AC_ARG_WITH([tth],
            AC_HELP_STRING([--with-tth=PATH],
                           [location of the tth executable]),
                           [TEX2HTML=$withval],
                           [AC_PATH_PROG([TEX2HTML],[tth])])
if test "x$TEX2HTML" != "x" ; then
   if test "x$TEX2HTML" != "xno" ; then
      if test ! -x $TEX2HTML ; then
         AC_MSG_ERROR([provided tth $TEX2HTML not found/executable])
      fi
      AC_SUBST([T2HOPTS], ["-e2 -i"])
   else
      AC_MSG_NOTICE([Skipping tth...])
      AC_SUBST([TEX2HTML], [""])
   fi
fi
if test "x$TEX2HTML" = "x" ; then
   AC_ARG_WITH([hevea],
               AC_HELP_STRING([--with-hevea=PATH],
                              [location of the hevea executable (if no tth)]),
                              [TEX2HTML=$withval],
                              [AC_PATH_PROG([TEX2HTML],[hevea])])
   if test "x$TEX2HTML" = "x" ; then
      AC_MSG_WARN([Cannot find tth or hevea in your PATH,])
      AC_MSG_WARN([you will not be able to build the HTML manual.])
      AC_SUBST([TEX2HTML], [true])
      AC_SUBST([T2HOPTS], [""])
   else
      if test "x$TEX2HTML" != "xno" ; then
         if [[ ! -x $TEX2HTML ]] ; then
            AC_MSG_ERROR([provided hevea $TEX2HTML not found/executable])
         else
            AC_SUBST([T2HOPTS], ["-fix"])
         fi
      else
         AC_MSG_NOTICE([Skipping hevea...])
         AC_SUBST([TEX2HTML], [true])
         AC_SUBST([T2HOPTS], [""])
      fi
   fi
fi

dnl get source dir in nice format
srcfulldir=`pwd`/

if test "$has_cygwin" != "" ; then
   srcfulldir=`cygpath -wp "$srcfulldir"`
fi
AC_SUBST([SRCFULLDIR],["$srcfulldir"])

dnl get launch4j executable

if test "$has_mingw" != "" ; then
   launch4j="launch4jc.exe"
else
   launch4j="launch4j"
fi
AC_SUBST([LAUNCH4J], [$launch4j])

dnl check for update-mime/desktop-database
AC_ARG_WITH([update-mime-database],
            AC_HELP_STRING([--with-update-mime-database=PATH],
                           [location of the update-mime-database executable]),
            [UPDMIMEDB=$withval],
            [AC_PATH_PROG([UPDMIMEDB],[update-mime-database])])
if test "x$UPDMIMEDB" = "x" ; then
   AC_MSG_WARN([Cannot find update-mime-database in your PATH,])
   AC_MSG_WARN([You will have to update the database yourself.])
   UPDMIMEDB=false
   AC_SUBST([UPDDESKDB],[false])
else
   if test "x$UPDMIMEDB" != "xno" ; then
      if test ! -x $UPDMIMEDB ; then
         AC_MSG_ERROR([provided update-mime-database $UPDMIMEDB not found/executable])
      else
         AC_PATH_PROG([UPDDESKDB],[update-desktop-database])
      fi
   else
      AC_MSG_NOTICE([Skipping update-mime-database...])
      UPDMIMEDB=false
      AC_SUBST([UPDDESKDB],[false])
   fi
fi

dnl compile Tiger compatible on Leopard
if test -d "/Developer/SDKs/MacOSX10.4u.sdk/" ; then
   LDFLAGS="-L -mmacosx-version-min=10.4 $LDFLAGS"
fi
if test -d "/Developer/SDKs/MacOSX10.5.sdk/" ; then
   LDFLAGS="-L -mmacosx-version-min=10.5 $LDFLAGS"
fi
if test -d "/Developer/SDKs/MacOSX10.6.sdk/" ; then
   LDFLAGS="-L -mmacosx-version-min=10.6 $LDFLAGS"
fi

dnl check for MPI
AC_ARG_WITH([mpi],
            AC_HELP_STRING([--with-mpi=PATH],
                           [location of the mpicc executable]),
                           [MPICC=$withval],
                           [AC_PATH_PROG([MPICC],[mpicc])])
if test "x$MPICC" != "x" ; then
   if test "x$MPICC" != "xno" ; then
      save_CC="$CC"
      CC="$MPICC"
      AC_CHECK_FUNC(MPI_Init, [], [MPICC=""])
      CC="$save_CC"
   else
      AC_MSG_NOTICE([Skipping mpicc...])
      MPICC="gcc"
   fi
   MPICFLAGS="--c-compiler $MPICC -C -DUSE_MPI"
   MPILDFLAGS=""
else
   MPICC="gcc"
   MPICFLAGS="-C -DUSE_MPI"
   MPILDFLAGS="-L -lmpi"
fi
if test "x$MPICC" != "x" ; then
   save_CC="$CC"
   CC="$MPICC"
   AC_MSG_CHECKING([for mpi.h])
   AC_TRY_COMPILE([#include <mpi.h>],[],[AC_MSG_RESULT(yes)], [MPICC=""
                   AC_MSG_RESULT(no)])
   CC="$save_CC"
fi
if test "x$MPICC" != "x" ; then
   GPLCFLAGS="$MPICFLAGS $GPLCFLAGS"
   LDFLAGS="$MPILDFLAGS $LDFLAGS"
fi
AC_SUBST(GPLCFLAGS)

dnl get short version
AC_SUBST([PACKAGE_SHORTVERSION],[${PACKAGE_VERSION%%-*}])

dnl read .in files and do substitutions in them
dnl then execute the chmod
AC_CONFIG_FILES([biochamVars.pl Makefile EXAMPLES/Makefile parsing/Makefile vim/Makefile emacs/Makefile gui/Makefile gui/legacy/Makefile biocham.iss gui/Biocham_l4j.xml biocham.desktop Info.plist gui/legacy/BiochamUpdater.java DOC/Makefile])

AC_OUTPUT
