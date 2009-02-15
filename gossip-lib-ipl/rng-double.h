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

/************ see the book for explanations and caveats! *******************/
/************ in particular, you need two's complement arithmetic **********/

#ifndef RNG_DOUBLE_H
#define RNG_DOUBLE_H

#define RANF_KK       100 /* the long lag */
#define RANF_LL        37 /* the short lag */
#define RANF_QUALITY 1009 /* recommended quality level for high-res use */

struct ranf_state {
  double ran_u[RANF_KK];           /* the generator state */
  double ranf_arr_buf[RANF_QUALITY];
  double ranf_arr_sentinel;
  double *ranf_arr_ptr;
};

typedef struct ranf_state ranf_state;

void ranf_start (ranf_state *st, long seed);
double ranf_next (ranf_state *st);

#endif /* RNG_DOUBLE_H */
