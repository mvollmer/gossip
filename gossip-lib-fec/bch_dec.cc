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

struct bch_dec : sim_int_comp {

unsigned char decode_bch_codeword(unsigned int code)
{
  int i,bit;
  int a1,a2,a3,a4;
  unsigned int output;

  for(i=0,output=0; i<BCH_CODEWORD_SIZE; i++)
  {
    a1 = parity(A1&code,BCH_CODEWORD_SIZE);
    a2 = parity(A2&code,BCH_CODEWORD_SIZE);
    a3 = parity(A3&code,BCH_CODEWORD_SIZE);
    a4 = parity(A4&code,BCH_CODEWORD_SIZE);
    /* Majority vote */
    if((a1+a2+a3+a4) >= 3)
    {
      bit = (code^1)&1;
    }
    else
    {
      bit = (code^0)&1;
    }
    code   >>=1;
    code   |= bit?0x4000:0;
    output >>=1;
    output |= bit?0x4000:0;
  }
  return (unsigned char)(output&0x7F);
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

  void step (const sim_int **in, sim_int **out)
  {
	out[0][0]=decode_bch_codeword((unsigned int)in[0][0]);
  }

};

SIM_DECLARE_BLOCK (bch_dec, NULL, NULL, inputs, outputs);
