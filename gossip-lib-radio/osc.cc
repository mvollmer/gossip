#include <gossip/sim.h>
#define twopi 2 * 3.14159

sim_generic generics[] = {
  SIM_GENERIC ("frequency"),
  NULL
};

sim_port inputs[] = {
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("cos", double, 1),
  SIM_PORT ("sin", double, 1),
  NULL
};

struct osc : sim_complex_comp {

  double phase;
  double deltaph;

  void
  init ()
  {
    double frequency;
    get (frequency, "frequency");
    phase = 0;
    deltaph = twopi / frequency;
  }

  void
  step (const double **in, double **out)
  {
    out[0][0] = sin(phase);
    out[1][0] = cos(phase);
    phase += deltaph;
    if(phase>=twopi)phase -= twopi;
  }

};

SIM_DECLARE_BLOCK (osc, generics, NULL, inputs, outputs);
