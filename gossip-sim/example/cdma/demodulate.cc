#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_COMPLEX_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_INT_PORT ("out", 1),
  NULL
};

struct demodulate : sim_complex_int_comp {

  void step (int steps, const sim_complex **in, sim_int **out)
  {
    for (int s = 0; s < steps; s++)
      out[0][s] = real(in[0][s]) > 0;
  }

};

SIM_DECLARE_BLOCK (demodulate, NULL, NULL, inputs, outputs);
