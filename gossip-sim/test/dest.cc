#include <stdio.h>
#include <sys/time.h>
#include <stdlib.h>

#include <gossip/sim.h>

static sim_generic generics[] = {
  SIM_GENERIC ("chunk"),
  SIM_GENERIC ("stat-count"),
  SIM_GENERIC ("stat-delay"),
  NULL
};

static sim_result results[] = {
  SIM_RESULT ("rate"),
  NULL
};

static sim_port inputs[] = {
  SIM_COMPLEX_PORT ("in", 1),
  NULL
};

static int
timeval_substract (timeval *result, timeval *x, timeval *y)
{
  /* Perform the carry for the later subtraction by updating Y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }
  
  /* `tv_usec' is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;
  
  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}

static double
subtimes (timeval *t1, timeval *t2)
{
  timeval res;
  timeval_substract (&res, t1, t2);
  return res.tv_sec + 1e-6*res.tv_usec;
}

struct dest : sim_complex_comp
{
  int stat_count, stat_delay, chunk;

  int num, goal, count;
  timeval then;
  double last_rate;

  void
  init ()
  {
    num = 0;
    goal = 100;
    count = 0;

    get (chunk, "chunk", 1);
    get (stat_count, "stat-count", -1);
    get (stat_delay, "stat-delay", 2);

    set_in_chunk (0, chunk);

    gettimeofday (&then, NULL);
    last_rate = 0;
  }

#if 0
  void
  step (const sim_complex **in, sim_complex **out)
  {
    num++;
    if (num > goal)
      {
	report (num);
	num = 0;
      }
  }
#else
  void
  step (int step_count, const sim_complex **in, sim_complex **out)
  {
    num += step_count;
    if (num > goal)
      {
	report (num);
	num = 0;
      }
  }
#endif

  void
  report (int n)
  {
    timeval now;
    gettimeofday (&now, NULL);
    double delta = subtimes(&now, &then);
    last_rate = chunk*(n/delta);
    printf ("%s: rate %g syms/sec\n", name, last_rate);
    then = now;
    goal = int(stat_delay*(n/delta));

    if (stat_count > 0)
      {
	count++;
	if (count == stat_count)
	  finish ();
      }
  }

  void
  epilog ()
  {
    set ("rate", last_rate);
  }
};

SIM_DECLARE_BLOCK (dest, generics, results, inputs, NULL);
