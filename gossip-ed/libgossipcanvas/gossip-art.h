/* Random collected bits of the libart_lgp library. */

#ifndef GOSSIP_ART_H
#define GOSSIP_ART_H

typedef struct _ArtPoint ArtPoint;

struct _ArtPoint {
  double x, y;
};

typedef struct _ArtDRect ArtDRect;

struct _ArtDRect {
  double x0, y0, x1, y1;
};

/* Find the smallest rectangle that includes both source rectangles. */
void art_drect_union (ArtDRect *dest,
		      const ArtDRect *src1, const ArtDRect *src2);

void
art_affine_point (ArtPoint *dst, const ArtPoint *src,
		  const double affine[6]);

void
art_affine_invert (double dst_affine[6], const double src_affine[6]);

void
art_affine_multiply (double dst[6],
		     const double src1[6], const double src2[6]);

/* set up the identity matrix */
void
art_affine_identity (double dst[6]);

/* set up a translation matrix */
void
art_affine_translate (double dst[6], double tx, double ty);

/* find the affine's "expansion factor", i.e. the scale amount */
double
art_affine_expansion (const double src[6]);

void
art_drect_affine_transform (ArtDRect *dst, const ArtDRect *src, const double matrix[6]);

#endif
