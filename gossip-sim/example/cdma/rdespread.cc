#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("factor"),
  NULL
};

sim_port inputs[] = {
  SIM_PORT ("in", float, 1),
  SIM_PORT ("spread-seq", float, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", float, 1),
  NULL
};

struct rdespread : sim_comp {

  int factor;
  
  void init ()
  {
    get (factor, "factor", 1);
    set_in_chunk (0, factor);
    set_in_chunk (1, factor);
  }

  void step (int steps, const sim_data **in, sim_data **out)
  {
    const float *in0 = (float *)in[0], *in1 = (float *)in[1];
    float *out0 = (float *)out[0], sum;

    for (int s = 0; s < steps; s++)
      {
	sum = 0;
	for (int i = 0; i < factor; i++)
	  sum += (*in0++)*(*in1++);
	*out0++ = sum;
      }
  }

};

SIM_DECLARE_BLOCK (rdespread, generics, NULL, inputs, outputs);
