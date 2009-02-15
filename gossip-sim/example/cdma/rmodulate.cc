#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_INT_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", float, 1 ),
  NULL
};

struct rmodulate : sim_comp {

  void step (int steps, const sim_data **in, sim_data **out)
  {
    for (int s = 0; s < steps; s++)
      ((float **)out)[0][s] = 2*((sim_int **)in)[0][s]-1;
  }

};

SIM_DECLARE_BLOCK (rmodulate, NULL, NULL, inputs, outputs);
