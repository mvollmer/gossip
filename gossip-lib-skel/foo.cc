#include <gossip/sim.h>

struct foo : sim_comp {

  void step (const sim_data **in, sim_data **out)
  {
  }

};

SIM_DECLARE_BLOCK (foo, NULL, NULL, NULL, NULL);
