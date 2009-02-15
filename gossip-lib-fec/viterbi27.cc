/* Viterbi decoder for K=7 rate=1/2 convolutional code
 * continuous traceback version
 * Copyright 1996 Phil Karn, KA9Q
 *
 * This version of the Viterbi decoder reads a continous stream of
 * 8-bit soft decision samples from standard input in offset-binary
 * form, i.e., a 255 sample is the strongest possible "1" symbol and a
 * 0 is the strongest possible "0" symbol. 128 is an erasure (unknown).
 *
 * The decoded output is written to stdout in big-endian form (the first
 * decoded bit appears in the high order bit of the first output byte).
 *
 * The metric table is fixed, and no attempt is made (yet) to find proper
 * symbol synchronization. These are likely future enhancements.
 */
#include <stdio.h>
#include <limits.h>
#include <gossip/sim.h>
#include <iostream.h>
#include "viterbi27.h"

/* This parameter sizes the path memory in bits, which is organized as a
 * circular buffer through which we periodically "trace back" to
 * produce the decoded data. PATHMEM must be greater than
 * MERGEDIST+TRACECHUNK, and for efficiency it should also be a power of 2.
 * Don't make it *too* large, or it will spill out of the CPU's on-chip cache
 * and decrease performance. Each bit of path memory costs 8 bytes for the
 * K=7 code.
 */
#define PATHMEM		64

/* In theory, a Viterbi decoder is true maximum likelihood only if
 * the path memory is as long as the entire message and a single traceback
 * is made from the terminal state (usually zero) after the entire message
 * is received.
 *
 * In practice, performance is essentially optimum as long as decoding
 * decisions are deferred by at least 4-5 constraint lengths (28-35 bits
 * for K=7) from the most recently received symbols. MERGEDIST sets this
 * parameter. We give ourselves some margin here in case the code is
 * punctured (which slows merging) and also to let us start each traceback
 * from an arbitrary current state instead of taking the time to find the
 * path with the highest current metric.
 */
#define	MERGEDIST	32	/* Distance to trace back before decoding */

/* Since each traceback is costly (thanks to the overhead of having to
 * go back MERGEDIST bits before we produce our first decoded bit) we'd like
 * to decode as many bits as possible per traceback at the expense of
 * increased decoding delay. TRACECHUNK sets how many bits to
 * decode on each traceback. Since output is produced in 8-bit bytes,
 * TRACECHUNK MUST be a multiple of 8.
 */
#define	TRACECHUNK	8	/* How many bits to decode on each traceback */

/* The path metrics need to be periodicially adjusted downward
 * to prevent an integer overflow that could cause the signed comparisons
 * in the butterfly macros to fail.
 *
 * It's possible to code the comparisons to work in modulo fashion, e.g.,
 * as 'if((a-b) > 0)' rather than 'if(a >b)'. A good optimizer would generate
 * code like 'cmp a,b;js foo' for this, but GCC doesn't.
 *
 * This constant should be larger than the maximum path metric spread.
 * Experimentally this seems to be 2040, which is probably related to the
 * free distance of the code (10) and the symbol metric scale (0-255).
 */
#define	RENORMALIZE	10000

#if (TRACECHUNK + MERGEDIST > PATHMEM)
#error "TRACECHUNK + MERGEDIST > PATHMEM"
#endif

#if ((TRACECHUNK % 8) != 0)
#error "TRACECHUNK not multiple of 8"
#endif

sim_port inputs[] =
{
	SIM_COMPLEX_PORT ("in",2*TRACECHUNK),
	NULL
};

sim_port outputs[] =
{
	SIM_INT_PORT ("out",TRACECHUNK),
	NULL
};

struct viterbi27 : sim_complex_int_comp {

long cmetric[64],nmetric[64];
unsigned long paths[2*PATHMEM];
unsigned long dec;
int mets[4];
unsigned int pi,first;
unsigned char symbols[2];
int mettab[2][256];

void init()
{
	/* Initialize metric table (make this an option)
	 * This table assumes a symbol of 0 is the
	 * strongest possible '0', and a symbol
	 * of 255 is the strongest possible '1'. A symbol
	 * of 128 is an erasure
	 */
	int i;
	for(i=0;i<256;i++){
		mettab[0][i] = 128 - i;
		mettab[1][255-i] = 127 - i;
	}
	cmetric[0] = 0;
	for(i=1;i<64;i++)
		cmetric[i] = -99999;
	pi = 0;
	first = 1;
}

unsigned char scale_func (double value)
{
//	if(value < 0) printf("Recv 0\n");
//	else printf("Recv 1\n");
	if(value < 0)return 0;
	return 255;
}

void step (const sim_complex **in, sim_int **out)
{
	/* Main loop -- read input symbols and run ACS butterflies,
	 * periodically tracing back to produce decoded output data.
	 * The loop is unrolled to process two bits per iteration.
	 */
	int i;
	int counter;
	counter = 0;
	int beststate,j;
	unsigned int nopi;
	for(;;){
		/* Renormalize metrics to prevent overflow */
		if(cmetric[0] > (LONG_MAX - RENORMALIZE)){
			for(i=0;i<64;i++)
				cmetric[i] -= LONG_MAX;
		} else if(cmetric[0] < LONG_MIN+RENORMALIZE){
			for(i=0;i<64;i++)
				cmetric[i] += LONG_MAX;
		}
		/* Read input symbol pair and compute branch metrics */
		symbols[0] = scale_func(real(in[0][counter++]));
		symbols[1] = scale_func(real(in[0][counter++]));
		mets[0] = mettab[0][symbols[0]] + mettab[0][symbols[1]];
		mets[1] = mettab[0][symbols[0]] + mettab[1][symbols[1]];
		mets[3] = mettab[1][symbols[0]] + mettab[1][symbols[1]];
		mets[2] = mettab[1][symbols[0]] + mettab[0][symbols[1]];

printf("Metrics %d %d %d %d\n",mets[0],mets[1],mets[2],mets[3]);
		/* On even numbered bits, the butterflies read from cmetrics[]
		 * and write to nmetrics[]. On odd numbered bits, the reverse
		 * is done
		 */
		dec = 0;
		BUTTERFLY(0,0);
		BUTTERFLY(6,0);
		BUTTERFLY(8,0);
		BUTTERFLY(14,0);
		BUTTERFLY(2,3);
		BUTTERFLY(4,3);
		BUTTERFLY(10,3);
		BUTTERFLY(12,3);
		BUTTERFLY(1,1);
		BUTTERFLY(7,1);
		BUTTERFLY(9,1);
		BUTTERFLY(15,1);
		BUTTERFLY(3,2);
		BUTTERFLY(5,2);
		BUTTERFLY(11,2);
		BUTTERFLY(13,2);
		paths[2*pi] = dec;
		dec = 0;
		BUTTERFLY(19,0);
		BUTTERFLY(21,0);
		BUTTERFLY(27,0);
		BUTTERFLY(29,0);
		BUTTERFLY(17,3);
		BUTTERFLY(23,3);
		BUTTERFLY(25,3);
		BUTTERFLY(31,3);
		BUTTERFLY(18,1);
		BUTTERFLY(20,1);
		BUTTERFLY(26,1);
		BUTTERFLY(28,1);
		BUTTERFLY(16,2);
		BUTTERFLY(22,2);
		BUTTERFLY(24,2);
		BUTTERFLY(30,2);
		paths[2*pi+1] = dec;
		pi++;

		/* Read input symbol pair and compute branch metrics */
		symbols[0] = scale_func(real(in[0][counter++]));
		symbols[1] = scale_func(real(in[0][counter++]));
		mets[0] = mettab[0][symbols[0]] + mettab[0][symbols[1]];
		mets[1] = mettab[0][symbols[0]] + mettab[1][symbols[1]];
		mets[3] = mettab[1][symbols[0]] + mettab[1][symbols[1]];
		mets[2] = mettab[1][symbols[0]] + mettab[0][symbols[1]];
printf("Metrics %d %d %d %d\n",mets[0],mets[1],mets[2],mets[3]);

		dec = 0;
		BUTTERFLY2(0,0);
		BUTTERFLY2(6,0);
		BUTTERFLY2(8,0);
		BUTTERFLY2(14,0);
		BUTTERFLY2(2,3);
		BUTTERFLY2(4,3);
		BUTTERFLY2(10,3);
		BUTTERFLY2(12,3);
		BUTTERFLY2(1,1);
		BUTTERFLY2(7,1);
		BUTTERFLY2(9,1);
		BUTTERFLY2(15,1);
		BUTTERFLY2(3,2);
		BUTTERFLY2(5,2);
		BUTTERFLY2(11,2);
		BUTTERFLY2(13,2);
		paths[2*pi] = dec;
		dec = 0;
		BUTTERFLY2(19,0);
		BUTTERFLY2(21,0);
		BUTTERFLY2(27,0);
		BUTTERFLY2(29,0);
		BUTTERFLY2(17,3);
		BUTTERFLY2(23,3);
		BUTTERFLY2(25,3);
		BUTTERFLY2(31,3);
		BUTTERFLY2(18,1);
		BUTTERFLY2(20,1);
		BUTTERFLY2(26,1);
		BUTTERFLY2(28,1);
		BUTTERFLY2(16,2);
		BUTTERFLY2(22,2);
		BUTTERFLY2(24,2);
		BUTTERFLY2(30,2);
		paths[2*pi+1] = dec;
		pi = (pi + 1) % PATHMEM;
		if((pi % TRACECHUNK) == 0)
			break;
	}
	if(!first)
	{
		beststate = 0;	/* arbitrary */
		nopi = (pi - 1) % PATHMEM;	/* Undo last increment of pi */
		for(i=0;i < MERGEDIST-6;i++)
		{
			if(paths[2*nopi + (beststate >> 5)] & (1 << (beststate & 31)))
			{
				beststate |= 64;	/* 2^(K-1) */
			}
			beststate >>= 1;
			nopi = (nopi - 1) % PATHMEM;
		}
		for(j=TRACECHUNK/8-1;j >= 0;j--)
		{
			counter = TRACECHUNK - 1;
			for(i=0;i<8;i++)
			{
				if(paths[2*nopi + (beststate >> 5)] & (1 << (beststate & 31)))
				{
					beststate |= 64;	/* 2^(K-1) */
					out[0][counter--] = 1;
				}
				else
					out[0][counter--] = 0;
				beststate >>= 1;
				nopi = (nopi - 1) % PATHMEM;
			}
		}
		for(i=0;i<TRACECHUNK;i++)
			if(out[0][i])printf("%d 1\n",i);
			else printf("%d 0\n",i);
	}
	else
		first = 0;
}

};

SIM_DECLARE_BLOCK (viterbi27, NULL, NULL, inputs, outputs);
