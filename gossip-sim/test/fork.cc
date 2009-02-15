#include <stdio.h>
#include <stdlib.h>

#include <gossip/sim.h>

static sim_port inputs[] = {
  SIM_COMPLEX_PORT ("in", 1),
  NULL
};

static sim_port outputs[] = {
  SIM_MULTI_COMPLEX_PORT ("out", 1),
  NULL
};

struct fork : sim_complex_comp
{
  void
  step (int step_count, const sim_complex **in, sim_complex **out)
  {
    for (int j = 0; j < n_out; j++)
      for (int i = 0; i < step_count; i++)
	out[j][i] = in[0][i];
  }
};

SIM_DECLARE_BLOCK (fork, NULL, NULL, inputs, outputs);
