#include <gossip/sim.h>
#include <math.h>
#include <iostream.h>

sim_generic generics[] = {
  SIM_GENERIC ("factor"),
  SIM_GENERIC ("signal-power"),
  SIM_GENERIC ("snr"),
  NULL
};

sim_port inputs[] = {
  SIM_PORT ("in", float, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", float, 1),
  NULL
};

struct rawgn : sim_comp {

  double sigma2;

  void init ()
  {
    double snr, factor, signal_power;
    get (factor, "factor", 1.0);
    get (signal_power, "signal-power", 1.0);
    get (snr, "snr");

    sigma2 = sqrt (factor*signal_power*pow(10.0, -snr/10.0));
  }
  
  double randd ()
  {
    double n = double(rand ()) / RAND_MAX;
    return n;
  }

  double noise ()
  {
    while (true)
      {
	double v1 = 2*randd()-1, v2 = 2*randd()-1;
	double s = v1*v1 + v2*v2;
	if (s >= 1)
	  continue;
	double w = sqrt (-2*log(s)/s);
	return sigma2*v1*w;
      }
  }

  void step (int steps, const sim_data **in, sim_data **out)
  {
    for (int s = 0; s < steps; s++)
      {
	((float **)out)[0][s] = ((float **)in)[0][s] + noise ();
      }
  }

};

SIM_DECLARE_BLOCK (rawgn, generics, NULL, inputs, outputs);
