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

struct rspread : sim_comp {

  int factor;
  
  void init ()
  {
    get (factor, "factor", 1);
    set_in_chunk (1, factor);
    set_out_chunk (0, factor);
  }

  void step (int steps, const sim_data **in, sim_data **out)
  {
    const float *in0 = (float *)in[0], *in1 = (float *)in[1];
    float *out0 = (float *)out[0];
    for (int s = 0; s < steps; s++)
      {
	for (int i = 0; i < factor; i++)
	  *out0++ = (*in0)*(*in1++);
	in0++;
      }
  }

};

SIM_DECLARE_BLOCK (rspread, generics, NULL, inputs, outputs);
