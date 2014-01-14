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
 * GNU Prolog file spawn_redirect.pl
 * by Thierry Martinez
 */

:- foreign(
   'Pl_Spawn_Redirect_6'(+string, +term, -positive, -positive, -positive, -positive)
).

spawn_redirect(Cmd, LArg, StreamIn, StreamOut, StreamErr, Pid) :-
	set_bip_name(spawn_redirect, 6),
	'$get_open_stm'(StreamIn, StmIn),
	'$get_open_stm'(StreamOut, StmOut),
	'$get_open_stm'(StreamErr, StmErr),
	'$sys_var_write'(0, 0),
	'Pl_Spawn_Redirect_6'(Cmd, LArg, StmIn, StmOut, StmErr, Pid).
