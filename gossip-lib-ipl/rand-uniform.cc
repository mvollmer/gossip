/* rand-uniform - uniformly distributed random numbers

   Copyright (C) 2002  Marius Vollmer

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

#include <gossip/sim.h>

extern "C" {
#include "rng-double.h"
}

sim_generic generics[] = {
  SIM_GENERIC ("seed"),
  NULL
};

sim_port inputs[] = {
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", double, 1),
  NULL
};

struct rand_uniform : sim_comp {

  ranf_state state;

  void
  init ()
  {
    long seed;

    get (seed, "seed", 0);
    ranf_start (&state, seed);
  }

  void
  step (const sim_data **, sim_data **out)
  {
    double *out0 = (double *)out[0];
    out0[0] = ranf_next (&state);
  }

};
  
SIM_DECLARE_BLOCK (rand_uniform, generics, NULL, inputs, outputs);
