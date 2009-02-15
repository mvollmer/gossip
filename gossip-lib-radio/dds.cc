#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("output_bits"),
  SIM_GENERIC ("phase_bits"),
  SIM_GENERIC ("lookup_bits"),
  NULL
};

sim_port inputs[] = {
  SIM_INT_PORT ("freq_word", 1),
  NULL
};

sim_port outputs[] = {
  SIM_INT_PORT ("cos", 1),
  SIM_INT_PORT ("sin", 1),
  NULL
};

struct dds : sim_complex_comp {

  sim_int output_bits;
  sim_int phase_bits;
  sim_int lookup_bits;
  sim_int *table;
  sim_int tablesize;
  sim_int phase;
  sim_int sin_offset;

  void
  init ()
  {
    int i,j;
    int scale;
    double k;

    get (output_bits, "output_bits", 10);
    get (phase_bits, "phase_bits", 32);
    get (lookup_bits, "lookup_bits", 12);

    tablesize = (int)pow(2,lookup_bits);
    scale = (int)pow(2,output_bits);
    sin_offset = tablesize/4;
    phase = 0;

    table = (sim_int *)malloc (sizeof(int)*tablesize);
    for(i=0;i<tablesize;i++)
	table[i]=(int)(scale*cos(i/tablesize*2*3.14159));
  }

  void
  step (const sim_int **in, sim_int **out)
  {
    out[0][0] = table[phase];
    out[1][0] = table[(phase+sin_offset)%tablesize];
    phase += in[0][0];
    if(phase>=tablesize)phase %= tablesize;
  }

};

SIM_DECLARE_BLOCK (dds, generics, NULL, inputs, outputs);
