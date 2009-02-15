#include <gossip/sim.h>

sim_port inputs[] = {
  SIM_MULTI_PORT ("in", float, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", float, 1),
  NULL
};

struct radd : sim_comp {

  void step (int steps, const sim_data **in, sim_data **out)
  {
    for (int s = 0; s < steps; s++)
      {
	float sum = 0;
	for (int i = 0; i < n_in; i++)
	  sum += ((float **)in)[i][s];
	((float **)out)[0][s] = sum;
      }
  }

};

SIM_DECLARE_BLOCK (radd, NULL, NULL, inputs, outputs);
