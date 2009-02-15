#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_COMPLEX_PORT ("in", 1),
  SIM_COMPLEX_PORT ("power-est", 1),
  NULL
};

sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct power_control : sim_complex_comp {

  void step (int steps, const sim_complex **in, sim_complex **out)
  {
    // ignore power-est for now
    for (int s = 0; s < steps; s++)
      out[0][s] = in[0][s];
  }

};

SIM_DECLARE_BLOCK (power_control, NULL, NULL, inputs, outputs);
