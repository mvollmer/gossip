#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("value"),
  NULL
};

sim_port outputs[] = {
  SIM_INT_PORT ("out", 1),
  NULL
};

struct source : sim_int_comp {

  sim_int value;

  void
  init ()
  {
    get (value, "value", 1);
  }

  void
  step (const sim_int **in, sim_int **out)
  {
    out[0][0] = value;
  }

};

SIM_DECLARE_BLOCK (source, generics, NULL, NULL, outputs);
