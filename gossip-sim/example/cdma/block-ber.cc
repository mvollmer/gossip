#include <gossip/sim.h>

sim_generic generics[] = {
  SIM_GENERIC ("count"), 
  NULL
};

sim_result results[] = {
  SIM_RESULT ("ber"),
  NULL
};

sim_port inputs[] = {
  SIM_INT_PORT ("received", 1),
  SIM_INT_PORT ("sent", 1),
  NULL
};

struct block_ber : sim_int_comp {

  int count, goal_count;
  int errors;
  
  void init ()
  {
    get (goal_count, "count");
    count = 0;
    errors = 0;
    if (count == goal_count)
      finish ();
  }

  void step (const sim_int **in, sim_int **out)
  {
    count++;
    if (in[0][0] != in[1][0])
      errors++;
    if (count == goal_count)
      finish ();
  }

  void epilog ()
  {
    set ("ber", double(errors)/double(count));
  }

};

SIM_DECLARE_BLOCK (block_ber, generics, results, inputs, NULL);

