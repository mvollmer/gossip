#include <gossip/sim.h>
#define twopi 2 * 3.14159

sim_generic generics[] = {
  NULL
};

sim_port inputs[] = {
  SIM_PORT ("frequency", double, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("cos", double, 1),
  SIM_PORT ("sin", double, 1),
  NULL
};

struct vco : sim_complex_comp {

  double phase;
  double frequency;
  double deltaph;

  void
  init ()
  {
    phase = 0;
  }

  void
  step (const double **in, double **out)
  {
    if(in[0][0] != frequency)
    {
      deltaph = twopi / in[0][0];
      frequency = in[0][0];
    }
    out[0][0] = sin(phase);
    out[1][0] = cos(phase);
    phase += deltaph;
    if(phase>=twopi)phase -= twopi;
  }

};

SIM_DECLARE_BLOCK (vco, generics, NULL, inputs, outputs);
