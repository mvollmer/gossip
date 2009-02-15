#include <stdio.h>
#include <stdlib.h>

#include <gossip/sim.h>

static sim_port inputs[] = {
  SIM_MULTI_COMPLEX_PORT ("in", 1),
  NULL
};

static sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct adder : sim_complex_comp
{
  void
  step (const sim_complex **in, sim_complex **out)
  {
    sim_complex sum = 0;
    for (int i = 0; i < n_in; i++)
      sum += in[i][0];
    out[0][0] = sum;
  }
};

SIM_DECLARE_BLOCK (adder, NULL, NULL, inputs, outputs);
