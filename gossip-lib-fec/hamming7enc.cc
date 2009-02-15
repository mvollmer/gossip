#include <gossip/sim.h>
#include <iostream.h>

sim_port inputs[] =
{
	SIM_INT_PORT ("in",1),
	NULL
};

sim_port outputs[] =
{
	SIM_INT_PORT ("out",1),
	NULL
};


struct hamming7enc : sim_int_comp {

unsigned char table[16];
void init()
{
	int i,temp;
	for(i=0;i<16;i++)
	{
		table[i] = i;
	if(table[i]&1)table[i] ^= 16 | 32 | 64;
	if(table[i]&2)table[i] ^= 16 | 32;
	if(table[i]&4)table[i] ^= 32 | 64;
	if(table[i]&8)table[i] ^= 16 | 64;
	}
}
  void step (const sim_int **in, sim_int **out)
  {
	out[0][0] = table[in[0][0]];
  }

};

SIM_DECLARE_BLOCK (hamming7enc, NULL, NULL, inputs, outputs);
