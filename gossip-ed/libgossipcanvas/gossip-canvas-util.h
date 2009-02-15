/* Miscellaneous utility functions for the GossipCanvas widget
 *
 * GossipCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#ifndef GOSSIP_CANVAS_UTIL_H
#define GOSSIP_CANVAS_UTIL_H

/* This structure defines an array of points.  X coordinates are stored in the even-numbered
 * indices, and Y coordinates are stored in the odd-numbered indices.  num_points indicates the
 * number of points, so the array is 2*num_points elements big.
 */
typedef struct {
	int num_points;
	double *coords;
} GossipCanvasPoints;


/* Allocate a new GossipCanvasPoints structure with enough space for the specified number of points */
GossipCanvasPoints *gossip_canvas_points_new (int num_points);

/* Free a points structure */
void gossip_canvas_points_free (GossipCanvasPoints *points);

/* Given three points forming an angle, compute the coordinates of the inside and outside points of
 * the mitered corner formed by a line of a given width at that angle.
 *
 * If the angle is less than 11 degrees, then FALSE is returned and the return points are not
 * modified.  Otherwise, TRUE is returned.
 */
int gossip_canvas_get_miter_points (double x1, double y1, double x2, double y2, double x3, double y3,
				   double width,
				   double *mx1, double *my1, double *mx2, double *my2);

/* Compute the butt points of a line segment.  If project is FALSE, then the results are as follows:
 *
 *            -------------------* (bx1, by1)
 *                               |
 *   (x1, y1) *------------------* (x2, y2)
 *                               |
 *            -------------------* (bx2, by2)
 *
 * that is, the line is not projected beyond (x2, y2).  If project is TRUE, then the results are as
 * follows:
 *
 *            -------------------* (bx1, by1)
 *                      (x2, y2) |
 *   (x1, y1) *-------------*    |
 *                               |
 *            -------------------* (bx2, by2)
 */
void gossip_canvas_get_butt_points (double x1, double y1, double x2, double y2,
				   double width, int project,
				   double *bx1, double *by1, double *bx2, double *by2);

/* Calculate the distance from a polygon to a point.  The polygon's X coordinates are in the even
 * indices of the poly array, and the Y coordinates are in the odd indices.
 */
double gossip_canvas_polygon_to_point (double *poly, int num_points, double x, double y);


#if 0
/* Render the svp over the buf. */
void
gossip_canvas_render_svp (GossipCanvasBuf *buf,
			 ArtSVP *svp,
			 guint32 rgba);

/* Sets the svp to the new value, requesting repaint on what's changed. This function takes responsibility for
 * freeing new_svp.
 */
void gossip_canvas_update_svp (GossipCanvas *canvas, ArtSVP **p_svp, ArtSVP *new_svp);

/* Sets the svp to the new value, clipping if necessary, and requesting repaint on what's changed. This function takes
 * responsibility for freeing new_svp.
 */
void gossip_canvas_update_svp_clip (GossipCanvas *canvas, ArtSVP **p_svp, ArtSVP *new_svp, ArtSVP *clip_svp);

/* Sets the svp to the new value, requesting repaint on what's changed. This function takes responsibility for
 * freeing new_svp. This routine also adds the svp's bbox to the item's.
 */
void gossip_canvas_item_reset_bounds (GossipCanvasItem *item);

/* Sets the svp to the new value, requesting repaint on what's changed. This function takes responsibility for
 * freeing new_svp. This routine also adds the svp's bbox to the item's.
 */
void gossip_canvas_item_update_svp (GossipCanvasItem *item, ArtSVP **p_svp, ArtSVP *new_svp);

/* Sets the svp to the new value, clipping if necessary, and requesting repaint on what's changed. This function takes
 * responsibility for freeing new_svp.
 */
void gossip_canvas_item_update_svp_clip (GossipCanvasItem *item, ArtSVP **p_svp, ArtSVP *new_svp, ArtSVP *clip_svp);

/* Request redraw of the svp if in aa mode, or the entire item in in xlib mode.
 */ 
void gossip_canvas_item_request_redraw_svp (GossipCanvasItem *item, const ArtSVP *svp);
#endif

/* Sets the bbox to the new value, requesting full repaint. */
void gossip_canvas_update_bbox (GossipCanvasItem *item,
			       int x1, int y1, int x2, int y2);

#if 0
/* Ensure that the buffer is in RGB format, suitable for compositing. */
void
gossip_canvas_buf_ensure_buf (GossipCanvasBuf *buf);
#endif

#endif
