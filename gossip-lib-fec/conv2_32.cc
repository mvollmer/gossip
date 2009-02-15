/*
 * Soft decision Fano sequential decoder for K=32 r=1/2 convolutional code
 * Copyright 1994, Phil Karn, KA9Q
 */
#define	LL 1	/* Select Layland-Lushbaugh code */

#include <gossip/sim.h>
#include <iostream.h>
#include <math.h>
#include "parity.cc"

#ifdef	NASA_STANDARD
/* "NASA standard" code by Massey & Costello
 * Nonsystematic, quick look-in, dmin=11, dfree=23
 * used on Pioneer 10-12, Helios A,B
 */
#define	POLY1	0xbbef6bb7
#define	POLY2	0xbbef6bb5
#endif

#ifdef	MJ
/* Massey-Johannesson code
 * Nonsystematic, quick look-in, dmin=13, dfree>=23
 * Purported to be more computationally efficient than Massey-Costello
 */
#define	POLY1	0xb840a20f
#define POLY2	0xb840a20d
#endif

#ifdef	LL
/* Layland-Lushbaugh code
 * Nonsystematic, non-quick look-in, dmin=?, dfree=?
 */
#define	POLY1	0xf2d05351
#define	POLY2	0xe4613c47
#endif

sim_port inputs[] =
{
	SIM_INT_PORT ("in",1),
	NULL
};

sim_port outputs[] =
{
	SIM_INT_PORT ("out",2),
	NULL
};


struct conv2_32 : sim_int_comp {

  unsigned int encstate;
  void init()
  {
	encstate = 0;
  }

  void step (const sim_int **in, sim_int **out)
  {
	unsigned int tmp;
	encstate = (encstate << 1) | (in[0][0] & 1);

	tmp = encstate & POLY1;
	tmp ^= tmp >> 16;
	out[0][0] = Partab[(tmp ^ (tmp >> 8)) & 0xff];

	tmp = encstate & POLY2;
	tmp ^= tmp >> 16;
	out[0][1] = Partab[(tmp ^ (tmp >> 8)) & 0xff];
  }
};

SIM_DECLARE_BLOCK (conv2_32, NULL, NULL, inputs, outputs);
