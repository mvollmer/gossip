#include <stdio.h>
#include <stdlib.h>

#include <gossip/sim.h>

static sim_generic generics[] = {
  SIM_GENERIC ("chunk"),
  SIM_GENERIC ("vals"),
  SIM_GENERIC ("count"),
  NULL
};

static sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

sim_complex default_vals[] = { 1.0 };
int default_vals_len = 1;

struct source : sim_complex_comp
{
  int chunk, count, n;
  sim_complex *vals;
  int vals_len, vals_index;

  void
  init ()
  {
    n = 0;

    get (chunk, "chunk", 1);
    get (count, "count", -1);
    get (vals, vals_len, "vals", default_vals, default_vals_len);
    set_out_chunk (0, chunk);

    vals_index = 0;
  }

  void
  step (int step_count, const sim_complex **in, sim_complex **out)
  {
    for (int i = 0; i < chunk*step_count; i++)
      {
	out[0][i] = vals[vals_index];
	vals_index = (vals_index+1) % vals_len;
      }
    if (count >= 0 && (n+=step_count) > count)
      finish ();
  }
};

SIM_DECLARE_BLOCK (source, generics, NULL, NULL, outputs);
