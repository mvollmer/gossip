#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("value"),
  NULL
};

sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct source : sim_complex_comp {

  sim_complex value;

  void
  init ()
  {
    get (value, "value", 1.0);
  }

  void
  step (const sim_complex **in, sim_complex **out)
  {
    out[0][0] = value;
  }

};

SIM_DECLARE_BLOCK (source, generics, NULL, NULL, outputs);
