#include <math.h>
#include "gossip-art.h"

#ifndef MAX
#define MAX(a, b)  (((a) > (b)) ? (a) : (b))
#endif /* MAX */

#ifndef MIN
#define MIN(a, b)  (((a) < (b)) ? (a) : (b))
#endif /* MIN */

/* Return true if the rectangle is empty. */
int
art_drect_empty (const ArtDRect *src) {
  return (src->x1 <= src->x0 || src->y1 <= src->y0);
}

/* Make a copy of the rectangle. */
void
art_drect_copy (ArtDRect *dest, const ArtDRect *src) {
  dest->x0 = src->x0;
  dest->y0 = src->y0;
  dest->x1 = src->x1;
  dest->y1 = src->y1;
}

/* Find the smallest rectangle that includes both source rectangles. */
void
art_drect_union (ArtDRect *dest, const ArtDRect *src1, const ArtDRect *src2) {
  if (art_drect_empty (src1)) {
    art_drect_copy (dest, src2);
  } else if (art_drect_empty (src2)) {
    art_drect_copy (dest, src1);
  } else {
    dest->x0 = MIN (src1->x0, src2->x0);
    dest->y0 = MIN (src1->y0, src2->y0);
    dest->x1 = MAX (src1->x1, src2->x1);
    dest->y1 = MAX (src1->y1, src2->y1);
  }
}

/* According to a strict interpretation of the libart structure, this
   routine should go into its own module, art_point_affine.  However,
   it's only two lines of code, and it can be argued that it is one of
   the natural basic functions of an affine transformation.
*/

void
art_affine_point (ArtPoint *dst, const ArtPoint *src,
		  const double affine[6])
{
  double x, y;

  x = src->x;
  y = src->y;
  dst->x = x * affine[0] + y * affine[2] + affine[4];
  dst->y = x * affine[1] + y * affine[3] + affine[5];
}

void
art_affine_invert (double dst[6], const double src[6])
{
  double r_det;

  r_det = 1.0 / (src[0] * src[3] - src[1] * src[2]);
  dst[0] = src[3] * r_det;
  dst[1] = -src[1] * r_det;
  dst[2] = -src[2] * r_det;
  dst[3] = src[0] * r_det;
  dst[4] = -src[4] * dst[0] - src[5] * dst[2];
  dst[5] = -src[4] * dst[1] - src[5] * dst[3];
}

/* dst is the composition of doing affine src1 then src2. Note that
   the PostScript concat operator multiplies on the left, i.e. if
   "M concat" is equivalent to "CTM = multiply (M, CTM)";

   It is safe to call this function with dst equal to one of the args.
 */
void
art_affine_multiply (double dst[6], const double src1[6], const double src2[6])
{
  double d0, d1, d2, d3, d4, d5;

  d0 = src1[0] * src2[0] + src1[1] * src2[2];
  d1 = src1[0] * src2[1] + src1[1] * src2[3];
  d2 = src1[2] * src2[0] + src1[3] * src2[2];
  d3 = src1[2] * src2[1] + src1[3] * src2[3];
  d4 = src1[4] * src2[0] + src1[5] * src2[2] + src2[4];
  d5 = src1[4] * src2[1] + src1[5] * src2[3] + src2[5];
  dst[0] = d0;
  dst[1] = d1;
  dst[2] = d2;
  dst[3] = d3;
  dst[4] = d4;
  dst[5] = d5;
}

/* set up the identity matrix */
void
art_affine_identity (double dst[6])
{
  dst[0] = 1;
  dst[1] = 0;
  dst[2] = 0;
  dst[3] = 1;
  dst[4] = 0;
  dst[5] = 0;
}

/* set up a translation matrix */
void
art_affine_translate (double dst[6], double tx, double ty)
{
  dst[0] = 1;
  dst[1] = 0;
  dst[2] = 0;
  dst[3] = 1;
  dst[4] = tx;
  dst[5] = ty;
}

/* find the affine's "expansion factor", i.e. the scale amount */
double
art_affine_expansion (const double src[6])
{
  return sqrt (src[0] * src[3] - src[1] * src[2]);
}

void
art_drect_affine_transform (ArtDRect *dst, const ArtDRect *src, const double matrix[6])
{
  double x00, y00, x10, y10;
  double x01, y01, x11, y11;

  x00 = src->x0 * matrix[0] + src->y0 * matrix[2] + matrix[4];
  y00 = src->x0 * matrix[1] + src->y0 * matrix[3] + matrix[5];
  x10 = src->x1 * matrix[0] + src->y0 * matrix[2] + matrix[4];
  y10 = src->x1 * matrix[1] + src->y0 * matrix[3] + matrix[5];
  x01 = src->x0 * matrix[0] + src->y1 * matrix[2] + matrix[4];
  y01 = src->x0 * matrix[1] + src->y1 * matrix[3] + matrix[5];
  x11 = src->x1 * matrix[0] + src->y1 * matrix[2] + matrix[4];
  y11 = src->x1 * matrix[1] + src->y1 * matrix[3] + matrix[5];
  dst->x0 = MIN (MIN (x00, x10), MIN (x01, x11));
  dst->y0 = MIN (MIN (y00, y10), MIN (y01, y11));
  dst->x1 = MAX (MAX (x00, x10), MAX (x01, x11));
  dst->y1 = MAX (MAX (y00, y10), MAX (y01, y11));
}
