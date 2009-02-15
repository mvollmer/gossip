/* Accessing Matlab from Gossip

   Copyright (C) 2000  Marius Vollmer

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

/* MATLAB - a block for invoking Matlab from Gossip
 */

#include <gossip/sim.h>
#include <iostream.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <signal.h>

#include "libmateng.h"

sim_generic generics[] = {
  SIM_GENERIC ("command"),
  SIM_GENERIC ("init-command"),
  SIM_GENERIC ("epilog-command"),
  SIM_GENERIC ("in-rows"),
  SIM_GENERIC ("out-rows"),
  NULL
};

sim_port inputs[] = {
  SIM_MULTI_PORT ("in", sim_complex, 1),
  NULL
};

sim_port outputs[] = {
  SIM_MULTI_PORT ("out", sim_complex, 1),
  NULL
};

struct matlab : sim_complex_comp {

  int in_rows, out_rows;
  const char *command;
  const char *epilog_command;

  matlab_engine *m;

  matlab ()
  {
    m = NULL;
  }

  ~matlab ()
  {
    if (m)
      delete m;
  }

  void
  init ()
  {
    const char *init_command;
    
    get (command, "command");
    get (init_command, "init-command", (const char *)NULL);
    get (epilog_command, "epilog-command", (const char *)NULL);
    get (in_rows, "in-rows", 1);
    get (out_rows, "out-rows", 1);

    set_in_chunk (0, in_rows);
    set_out_chunk (0, out_rows);

    m = new matlab_engine ("gossip");
    if (init_command)
      m->exchange (0, 0, NULL,
		   init_command,
		   0, 0, NULL);
  }

  void
  step (const sim_complex **in, sim_complex **out)
  {

#if 0
    {
      for (int i = 0; i < in_rows; i++)
	{
	  cout << get_name () << "< ";
	  for (int j = 0; j < n_in; j++)
	    cout << in[j][i] << " ";
	  cout << "\n";
	}
      cout << "\n";
    }
#endif

    if (!m->exchange (in_rows, n_in, in,
		      command,
		      out_rows, n_out, out))
      {
	fprintf (stderr, "Gossip Client: engine failed!\n");
	finish ();
      }

#if 0
    {
      for (int i = 0; i < out_rows; i++)
	{
	  cout << get_name () << "> ";
	  for (int j = 0; j < n_out; j++)
	    cout << out[j][i] << " ";
	  cout << "\n";
	}
      cout << "\n";
    }
#endif

  }

  void
  epilog ()
  {
    if (epilog_command)
      m->exchange (0, 0, NULL,
		   epilog_command,
		   0, 0, NULL);
    
    delete m;
    m = NULL;
  }
};

SIM_DECLARE_BLOCK (matlab, generics, NULL, inputs, outputs);
