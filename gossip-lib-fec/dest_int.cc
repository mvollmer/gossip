#include <gossip/sim.h>
#include <iostream.h>

sim_generic generics[] = {
  SIM_GENERIC ("count"),
  NULL
};

sim_port inputs[] = {
  SIM_MULTI_INT_PORT ("in", 1),
  NULL
};

struct destination : sim_int_comp {

  int count, n;
  
  void
  init ()
  {
    n = 0;
    get (count, "count");
    if (count == 0)
      finish ();
  }

  void
  step (const sim_int **in, sim_int **out)
  {
    n++;
    cout << get_name () << ": ";
    for (int i = 0; i < n_in; i++)
      cout << in[i][0] << " ";
    cout << "\n";

    if (count >= 0 && n == count)
      finish ();
  }

};

SIM_DECLARE_BLOCK (destination, generics, NULL, inputs, NULL);
