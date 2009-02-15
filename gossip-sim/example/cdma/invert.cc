#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_INT_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_INT_PORT ("out", 1),
  NULL
};

struct invert : sim_int_comp {

  void step (const sim_int **in, sim_int **out)
  {
    out[0][0] = !in[0][0];
  }

};

SIM_DECLARE_BLOCK (invert, NULL, NULL, inputs, outputs);

