#include <stdio.h>
#include <stdlib.h>

#include <gossip/sim.h>

static sim_generic generics[] = {
  SIM_GENERIC ("in-chunk"),
  SIM_GENERIC ("out-chunk"),
  NULL
};

static sim_port inputs[] = {
  SIM_MULTI_COMPLEX_PORT ("in", 1),
  NULL
};

static sim_port outputs[] = {
  SIM_MULTI_COMPLEX_PORT ("out", 1),
  NULL
};

struct thru : sim_complex_comp
{
  int in_chunk, out_chunk;

  void 
  init ()
  {
    get (in_chunk, "in-chunk", 1);
    get (out_chunk, "out-chunk", 1);

    set_in_chunk (0, in_chunk);
    set_out_chunk (0, out_chunk);
  }

#if 0
  void
  step (const sim_complex **in, sim_complex **out)
  {
    sim_complex sum = 0;
    for (int j = 0; j < n_in; j++)
      for (int i = 0; i < in_chunk; i++)
	sum += in[j][i];
    for (int i = 0; i < out_chunk; i++)
      out[0][i] = sum;
  }
#else
  void
  step (int step_count, const sim_complex **in, sim_complex **out)
  {
    sim_complex sum = 0;
    for (int j = 0; j < n_in; j++)
      for (int i = 0; i < in_chunk*step_count; i++)
	sum += in[j][i];
    for (int i = 0; i < out_chunk*step_count; i++)
      out[0][i] = sum;
  }
#endif

};

SIM_DECLARE_BLOCK (thru, generics, NULL, inputs, outputs);
