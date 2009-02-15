#include <gossip/sim.h>
#include <stdlib.h>

sim_port outputs[] = {
  SIM_INT_PORT ("out", 1),
  NULL
};

struct random_bits : sim_int_comp {

  void step (int steps, const sim_int **in, sim_int **out)
  {
    for (int s = 0; s < steps; s++)
      out[0][s] = (rand() > RAND_MAX/2);
  }

};

SIM_DECLARE_BLOCK (random_bits, NULL, NULL, NULL, outputs);

