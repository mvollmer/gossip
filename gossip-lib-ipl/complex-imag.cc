/* complex-imag  -  extract imaginary part of a complex number

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

sim_generic generics[] = {
  NULL
};

sim_port inputs[] = {
  SIM_PORT ("in", sim_complex, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", double, 1),
  NULL
};

struct complex_imag : sim_comp {

  void
  step (const sim_data **in_, sim_data **out_)
  {
    const sim_complex *in = (const sim_complex *)in_[0];
    double *out = (double *)out_[0];

    out[0] = imag(in[0]);
  }

};
  
SIM_DECLARE_BLOCK (complex_imag, generics, NULL, inputs, outputs);
