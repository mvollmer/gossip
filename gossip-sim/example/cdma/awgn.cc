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
  SIM_COMPLEX_PORT ("in", 1),
  NULL
};

sim_port outputs[] = {
  SIM_COMPLEX_PORT ("out", 1),
  NULL
};

struct awgn : sim_complex_comp {

  double sigma2;

  void init ()
  {
    double snr, factor, signal_power;
    get (factor, "factor", 1.0);
    get (signal_power, "signal-power", 1.0);
    get (snr, "snr");

    sigma2 = sqrt (0.5*factor*signal_power*pow(10.0, -snr/10.0));
  }
  
  double randd ()
  {
    double n = double(rand ()) / RAND_MAX;
    return n;
  }

  sim_complex noise ()
  {
    while (true)
      {
	double v1 = 2*randd()-1, v2 = 2*randd()-1;
	double s = v1*v1 + v2*v2;
	if (s >= 1)
	  continue;
	double w = sqrt (-2*log(s)/s);
	sim_complex n = sigma2*sim_complex (v1*w, v2*w);
	// cerr << "n = " << n << "\n";
	return n;
      }
  }

  void step (int steps, const sim_complex **in, sim_complex **out)
  {
    for (int s = 0; s < steps; s++)
      out[0][s] = in[0][s] + noise ();
  }

};

SIM_DECLARE_BLOCK (awgn, generics, NULL, inputs, outputs);
