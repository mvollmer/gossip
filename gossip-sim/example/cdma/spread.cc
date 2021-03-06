#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("factor"),
  NULL
};

sim_port inputs[] = {
  SIM_COMPLEX_PORT ("in", 1),
  SIM_COMPLEX_PORT ("spread-seq", 1),
  NULL
};

sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct spread : sim_complex_comp {

  int factor;
  
  void init ()
  {
    get (factor, "factor", 1);
    set_in_chunk (1, factor);
    set_out_chunk (0, factor);
  }

  void step (int steps, const sim_complex **in, sim_complex **out)
  {
    const sim_complex *in0 = in[0], *in1 = in[1];
    sim_complex *out0 = out[0];
    for (int s = 0; s < steps; s++)
      {
	for (int i = 0; i < factor; i++)
	  *out0++ = (*in0)*(*in1++);
	in0++;
      }
  }

};

SIM_DECLARE_BLOCK (spread, generics, NULL, inputs, outputs);
