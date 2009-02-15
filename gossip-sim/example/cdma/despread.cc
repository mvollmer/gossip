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

struct despread : sim_complex_comp {

  int factor;
  
  void init ()
  {
    get (factor, "factor", 1);
    set_in_chunk (0, factor);
    set_in_chunk (1, factor);
  }

  void step (int steps, const sim_complex **in, sim_complex **out)
  {
    const sim_complex *in0 = in[0], *in1 = in[1];
    sim_complex *out0 = out[0], sum;

    for (int s = 0; s < steps; s++)
      {
	sum = 0;
	for (int i = 0; i < factor; i++)
	  sum += (*in0++)*(*in1++);
	*out0++ = sum;
      }
  }

};

SIM_DECLARE_BLOCK (despread, generics, NULL, inputs, outputs);
