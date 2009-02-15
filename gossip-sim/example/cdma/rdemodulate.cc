#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_PORT ("in", float, 1),
  NULL
};

sim_port outputs[] = {
  SIM_INT_PORT ("out", 1),
  NULL
};

struct rdemodulate : sim_comp {

  void step (int steps, const sim_data **in, sim_data **out)
  {
    for (int s = 0; s < steps; s++)
      ((sim_int **)out)[0][s] = ((float **)in)[0][s] > 0;
  }

};

SIM_DECLARE_BLOCK (rdemodulate, NULL, NULL, inputs, outputs);
