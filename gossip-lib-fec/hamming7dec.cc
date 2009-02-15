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

unsigned char table[16];

struct hamming7dec : sim_int_comp {

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
	sim_int tmp;
	int syndrome;
	tmp = table[in[0][0]&0x0F];
	syndrome = tmp ^ in[0][0];

	if(syndrome == (64 | 32 | 16)) tmp ^= 1;
	if(syndrome == (32 | 16)) tmp ^= 2;
	if(syndrome == (64 | 32)) tmp ^= 4;
	if(syndrome == (64 | 16)) tmp ^= 8;

	out[0][0]=tmp&0x0F;
  }
};

SIM_DECLARE_BLOCK (hamming7dec, NULL, NULL, inputs, outputs);
