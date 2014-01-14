/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2011, INRIA Paris-Rocquencourt, EPI Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * C file spawn_redirect_c.c
 * by Thierry Martinez
 */
#include <gprolog.h>
#include <stdio.h>
#include <stdlib.h>

/* From BipsPl/os_interf_c.c */
#define MAX_SPAWN_ARGS             1024

/* From BipsPl/stream_supp.h */
#define STREAM_MODE_READ           0
#define STREAM_MODE_WRITE          1
#define STREAM_MODE_APPEND         2

extern int
Pl_Add_Stream_For_Stdio_Desc(FILE *f, int atom_path, int mode, int text);


/* From EnginePl/machine1.h */
extern int
Pl_M_Spawn_Redirect(
   char *arg[], int detach, FILE **f_in, FILE **f_out, FILE **f_err
);

/* From EnginePl/engine.h */
extern char pl_glob_buff[];

/* From EnginePl/bool.h */
#define TRUE 1

PlBool
Pl_Spawn_Redirect_6(
   char *Cmd, PlTerm LArg, PlLong *StmIn, PlLong *StmOut, PlLong *StmErr,
   PlLong *Pid
) {
   PlTerm larg = LArg;
   PlTerm *larg_dec;
   char *arg[MAX_SPAWN_ARGS];
   char **p = arg;
   FILE *f_in, *f_out, *f_err;
   int atom;
   int pid;

   *p++ = Cmd;

   while ((larg_dec = Pl_Rd_List_Check(larg)) != NULL) {
      *p++ = Pl_Rd_String_Check(larg_dec[0]);
      larg = larg_dec[1];
   }
   *p = NULL;

   pid = Pl_M_Spawn_Redirect(arg, 0, &f_in, &f_out, &f_err);

   if (pid == -1) {
      goto syserr;
   }

   sprintf(pl_glob_buff, "exec_stream('%.1024s')", Cmd);
   atom = Pl_Create_Allocate_Atom(pl_glob_buff);

   *Pid = pid;
   *StmIn = Pl_Add_Stream_For_Stdio_Desc(f_in, atom, STREAM_MODE_WRITE, TRUE);
   *StmOut = Pl_Add_Stream_For_Stdio_Desc(f_out, atom, STREAM_MODE_READ, TRUE);
   *StmErr = Pl_Add_Stream_For_Stdio_Desc(f_err, atom, STREAM_MODE_READ, TRUE);

   return PL_TRUE;

 syserr:
   Pl_Os_Error();
   return PL_EXCEPTION;
}
