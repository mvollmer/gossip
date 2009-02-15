#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_MULTI_COMPLEX_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct add : sim_complex_comp {

  void step (int steps, const sim_complex **in, sim_complex **out)
  {
    for (int s = 0; s < steps; s++)
      {
	sim_complex sum = 0;
	for (int i = 0; i < n_in; i++)
	  sum += in[i][s];
	out[0][s] = sum;
      }
  }

};

SIM_DECLARE_BLOCK (add, NULL, NULL, inputs, outputs);
