/* mini-exec - evaluate a miniature, stack-based language

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

/* This block can compute a new output value for each input value by
   executing a program in a simple, stack-based language.  The idea is
   to have this block as the execution engine, and layer hierachical
   'compiler' blocks on top of it.

   Ideally, we wouldn't need this block because we can write a block
   in C++ that computes the desired function.  This in invoncenient,
   however, as C++ blocks must be re-compiled when the function
   changes, and setting up a C++ block is a hassle compared to just
   writing down the expression in the simulation description.

   Instead of C++, we could write a generic block that can evaluate
   generic Scheme expressions at simulation time.  Such a block exists
   (scheme-exec), but it is slow.  Ideally (again), this should be
   cured by making Scheme faster, but in the mean-time, we have this
   middle ground.

   The compiler block 'eval' will use 'mini-exec' for a subset of
   Scheme that can be compiled to the mini language, and 'scheme-exec'
   for the rest.
*/

sim_generic generics[] = {
  SIM_GENERIC ("prog"),
  NULL
};

sim_port inputs[] = {
  SIM_PORT ("in", double, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", double, 1),
  NULL
};

struct mini_exec : sim_comp {

  void
  init ()
  {
  }

  void
  step (const sim_data **, sim_data **out)
  {
    out[0][0] = execute (in[0[0]]);
  }

};
  
SIM_DECLARE_BLOCK (mini_exec, generics, NULL, inputs, outputs);
