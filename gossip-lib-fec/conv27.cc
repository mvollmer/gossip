/* Encode standard input in the r=1/2 K=7 standard convolutional code.
 * Encoded output is big-endian, one byte per symbol, with 1 represented
 * by 1 and 0 by 0
 */

#include <stdio.h>
#include <gossip/sim.h>
#include <iostream.h>
#include "viterbi27.h"
#include "parity.cc"


sim_port inputs[] =
{
	SIM_INT_PORT ("in",1),
	NULL
};

sim_port outputs[] =
{
	SIM_INT_PORT ("out",16),
	NULL
};


struct conv27 : sim_int_comp {

//extern unsigned char Partab[];
unsigned int encstate;

void init()
{
	cout << "parity 1 " << Partab[1] << ".\n";
  	encstate = 0;
}

void step (const sim_int **in, sim_int **out)
{
  	int c,i;
	int counter=0;
	c = in[0][0];
    	for(i=7;i>=0;i--)
	{
      		encstate = (encstate << 1) | ((c >> 7) & 1);
		if((c >> 7)&1)
			printf("Sent: 1\n");
		else
			printf("Sent: 0\n");
      		c <<= 1;
      		out[0][counter++] = Partab[encstate & POLYA];
      		out[0][counter++] = Partab[encstate & POLYB];

		if(Partab[encstate & POLYA]) {cout << "A=1\t";}
		else {cout << "A=0\t";}
		if(Partab[encstate & POLYB]) {cout << "B=1\n";}
		else {cout << "B=0\n";}
//		cout << "Encstate " << encstate << " A " << Partab[encstate & POLYA] << " B " << Partab[encstate & POLYB] << "\n";
    	}
}

};

SIM_DECLARE_BLOCK (conv27, NULL, NULL, inputs, outputs);
