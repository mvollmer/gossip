#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_INT_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct modulate : sim_int_complex_comp {

  void step (int steps, const sim_int **in, sim_complex **out)
  {
    for (int s = 0; s < steps; s++)
      out[0][s] = 2*in[0][s]-1;
  }

};

SIM_DECLARE_BLOCK (modulate, NULL, NULL, inputs, outputs);
