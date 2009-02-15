/*
 * This program generates and decodes (15,7) BCH codes.
 *
 * Reference:
 *
 * MIL-STD-188-220B Appendix K
 *
 * C.H Brain G4GUO emailto:chbrain@dircon.co.uk
 *
 */

#include <gossip/sim.h>
#include <iostream.h>

#define A1 0x080D
#define A2 0x2203
#define A3 0x5101
#define A4 0x00D1

#define BCH_CODEWORD_SIZE 15

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


struct bch_enc : sim_int_comp {

  unsigned char G[8];

  void init()
  {
	G[0]=0x68;
	G[1]=0x34;
	G[2]=0x1A;
	G[3]=0x0D;
	G[4]=0x6E;
	G[5]=0x37;
	G[6]=0x73;
	G[7]=0x51;
  }

  static unsigned int parity(unsigned int word, int length)
  {
    int i,count;

    for(i=0,count=0;i<length;i++)
    {
      if(word&(1<<i))count++;
    }
    return count&1;
  }

  unsigned int encode_bch_codeword(unsigned char data)
  {
    unsigned int word;
    int i;
  
    for(i=0,word=0; i<8; i++)
    {
      word <<=1;
      word |= parity((data&G[i]),8);
    }
    word = (word<<7)+data;
    return word;
  }

  void step (const sim_int **in, sim_int **out)
  {
	out[0][0]=   encode_bch_codeword((unsigned char)in[0][0]);
  }

};

SIM_DECLARE_BLOCK (bch_enc, NULL, NULL, inputs, outputs);
