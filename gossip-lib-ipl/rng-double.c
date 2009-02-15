/*    This program by D E Knuth is in the public domain and freely copyable
 *    AS LONG AS YOU MAKE ABSOLUTELY NO CHANGES!
 *    It is explained in Seminumerical Algorithms, 3rd edition, Section 3.6
 *    (or in the errata to the 2nd edition --- see
 *        http://www-cs-faculty.stanford.edu/~knuth/taocp.html
 *    in the changes to Volume 2 on pages 171 and following).              */

/*    N.B. The MODIFICATIONS introduced in the 9th printing (2002) are
      included here; there's no backwards compatibility with the original. */

/*    If you find any bugs, please report them immediately to
 *                 taocp@cs.stanford.edu
 *    (and you will be rewarded if the bug is genuine). Thanks!            */

/* Modified to work as a library routine. */

#include "rng-double.h"

/************ see the book for explanations and caveats! *******************/
/************ in particular, you need two's complement arithmetic **********/

#define mod_sum(x,y) (((x)+(y))-(int)((x)+(y)))   /* (x+y) mod 1.0 */

static void
ranf_array (ranf_state *st, double aa[], int n)
{
  register int i,j;
  for (j=0;j<RANF_KK;j++)
    aa[j]=st->ran_u[j];
  for (;j<n;j++)
    aa[j]=mod_sum(aa[j-RANF_KK],aa[j-RANF_LL]);
  for (i=0;i<RANF_LL;i++,j++)
    st->ran_u[i]=mod_sum(aa[j-RANF_KK],aa[j-RANF_LL]);
  for (;i<RANF_KK;i++,j++)
    st->ran_u[i]=mod_sum(aa[j-RANF_KK],st->ran_u[i-RANF_LL]);
}

/* the following routines are adapted from exercise 3.6--15 */
/* after calling ranf_start, get new randoms by, e.g., "x=ranf_arr_next()" */

double
ranf_next (ranf_state *st)
{
  if (*(st->ranf_arr_ptr) >= 0)
    return *(st->ranf_arr_ptr)++;
  
  ranf_array (st, st->ranf_arr_buf, RANF_QUALITY);
  st->ranf_arr_buf[100]=-1;
  st->ranf_arr_ptr=st->ranf_arr_buf+1;
  return st->ranf_arr_buf[0];
}

#define TT  70   /* guaranteed separation between streams */
#define is_odd(s) ((s)&1)

void
ranf_start (ranf_state *st, long seed)
{
  register int t,s,j;
  double u[RANF_KK+RANF_KK-1];
  double ulp=(1.0/(1L<<30))/(1L<<22);               /* 2 to the -52 */
  double ss=2.0*ulp*((seed&0x3fffffff)+2);

  for (j=0;j<RANF_KK;j++) {
    u[j]=ss;                                /* bootstrap the buffer */
    ss+=ss; if (ss>=1.0) ss-=1.0-2*ulp;  /* cyclic shift of 51 bits */
  }
  u[1]+=ulp;                     /* make u[1] (and only u[1]) "odd" */
  for (s=seed&0x3fffffff,t=TT-1; t; ) {
    for (j=RANF_KK-1;j>0;j--)
      u[j+j]=u[j],u[j+j-1]=0.0;                         /* "square" */
    for (j=RANF_KK+RANF_KK-2;j>=RANF_KK;j--) {
      u[j-(RANF_KK-RANF_LL)]=mod_sum(u[j-(RANF_KK-RANF_LL)],u[j]);
      u[j-RANF_KK]=mod_sum(u[j-RANF_KK],u[j]);
    }
    if (is_odd(s)) {                             /* "multiply by z" */
      for (j=RANF_KK;j>0;j--) u[j]=u[j-1];
      u[0]=u[RANF_KK];                    /* shift the buffer cyclically */
      u[RANF_LL]=mod_sum(u[RANF_LL],u[RANF_KK]);
    }
    if (s) s>>=1; else t--;
  }
  for (j=0;j<RANF_LL;j++) st->ran_u[j+RANF_KK-RANF_LL]=u[j];
  for (;j<RANF_KK;j++) st->ran_u[j-RANF_LL]=u[j];
  for (j=0;j<10;j++) ranf_array(st,u,RANF_KK+RANF_KK-1);  /* warm things up */

  st->ranf_arr_ptr = &(st->ranf_arr_sentinel);
  st->ranf_arr_sentinel = -1;
}
