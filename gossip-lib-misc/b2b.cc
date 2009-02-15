/* b2b - bit width converter

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

#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("in-width"),
  SIM_GENERIC ("out-width"),
  NULL
};

sim_port inputs[] = {
  SIM_INT_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_INT_PORT ("out", 1),
  NULL
};

#define MAX_BITS (CHAR_BIT*sizeof(sim_int))

struct b2b : sim_int_comp {

  int in_width, out_width, in_chunk, out_chunk;

  int
  gcd (int a, int b)
  {
    if (b == 0) return a;
    return gcd (b, a % b);
  }

  void
  init ()
  {
    int bit_chunk;

    get (in_width, "in-width");
    get (out_width, "out-width");

    if (in_width < 1 || in_width > MAX_BITS)
      error ("in_width must be between 1 and %d, inclusive.", MAX_BITS);
    if (out_width < 1 || out_width > MAX_BITS)
      error ("out_width must be between 1 and %d, inclusive.", MAX_BITS);

    bit_chunk = in_width*out_width/gcd(in_width, out_width);
    in_chunk = bit_chunk/in_width;
    out_chunk = bit_chunk/out_width;

    set_in_chunk (0, in_chunk);
    set_out_chunk (0, out_chunk);

#if 0
    fprintf (stderr, "%s: (%d,%d) -> (%d,%d)\n", get_name (),
	     in_chunk, in_width, out_chunk, out_width);
#endif
  }

  void
  twiddle_bits (const sim_int *in, sim_int *out)
  {
    assert (in_chunk > 0 && in_width > 0);
    assert (in_chunk * in_width == out_chunk * out_width);
    
    int i1 = 0, j1 = in_width;
    int i2 = 0, j2 = out_width;
    
    sim_int m1 = (in_width == MAX_BITS)? ~0 : (1<<in_width)-1;
    sim_int m2 = (out_width == MAX_BITS)? ~0 : (1<<out_width)-1;
    sim_int bits = in[0] & m1;
    out[0] = 0;
    
    while (true) {
      if (j1 < j2) {
	out[i2] |= bits << (j2 - j1);
	j2 -= j1;
	j1 = in_width;
	i1++;
	if (i1 >= in_chunk)
	  break;
	bits = in[i1] & m1;
      } else if (j2 < j1) {
	out[i2] |= bits >> (j1 - j2);
	out[i2] &= m2;
	j1 -= j2;
	j2 = out_width;
	i2++;
	if (i2 >= out_chunk)
	  break;
	out[i2] = 0;
      } else {
	out[i2] |= bits;
	out[i2] &= m2;
	j1 = in_width;
	j2 = out_width;
	i1++;
	i2++;
	if (i1 >= in_chunk || i2 >= out_chunk)
	  break;
	bits = in[i1] & m1;
	out[i2] = 0;
      }
    }
    assert (i1 == in_chunk && i2 == out_chunk);
  }

  void
  step (const sim_int **in, sim_int **out)
  {
    twiddle_bits (in[0], out[0]);
  }

};
  
SIM_DECLARE_BLOCK (b2b, generics, NULL, inputs, outputs);

