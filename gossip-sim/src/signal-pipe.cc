/*  gossip-sim - synchronous data flow simulations

    Copyright (C) 2000, 2001, 2002  Marius Vollmer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "signal-pipe.h"
#include <unistd.h>
#include <signal.h>

static int signal_pipe[2];
static SCM signal_outlet;

SCM_SYMBOL (sym_signal_pipe, "signal-pipe");

SCM_DEFINE (sim_get_signal_outlet, "get-signal-outlet", 0, 0, 0,
	    (),
	    "Return the port where signal notifications can be observed.")
#define FUNC_NAME s_sim_get_signal_outlet
{
  return signal_outlet;
}
#undef FUNC_NAME

static void signal_notify_handler (int signo)
{
  unsigned char signo_byte = signo;
  write (signal_pipe[1], &signo_byte, 1);
}

SCM_DEFINE (sim_add_signal_notify, "add-signal-notify", 1, 0, 0,
	    (SCM signo),
	    "Arrange things so that upon receipt of the signal @var{signo}, "
	    "a byte with value @var{signo} is written to the"
	    " signal-outlet port")
#define FUNC_NAME s_sim_add_signal_notify
{
  signal (SCM_NUM2INT (1, signo), signal_notify_handler);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
sim_init_signal_pipe ()
#define FUNC_NAME "init-signal-pipe"
{
#include "signal-pipe.x"

  if (pipe (signal_pipe) < 0)
    SCM_SYSERROR;

  signal_outlet =
    scm_permanent_object (scm_fdes_to_port (signal_pipe[0], "r",
					    sym_signal_pipe));
}
