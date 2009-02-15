/* Polygon item type for GossipCanvas widget
 *
 * GossipCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#include <math.h>
#include <string.h>
#include "gossip-canvas-polygon.h"
#include "gossip-canvas-util.h"
#include "gossip-art.h"

#define NUM_STATIC_POINTS 256	/* Number of static points to use to avoid allocating arrays */


#define GROW_BOUNDS(bx1, by1, bx2, by2, x, y) {	\
	if (x < bx1)				\
		bx1 = x;			\
						\
	if (x > bx2)				\
		bx2 = x;			\
						\
	if (y < by1)				\
		by1 = y;			\
						\
	if (y > by2)				\
		by2 = y;			\
}


enum {
	ARG_0,
#if 0
	ARG_POINTS,
#endif
	ARG_FILL_COLOR,
	ARG_FILL_COLOR_GDK,
	ARG_FILL_COLOR_RGBA,
	ARG_OUTLINE_COLOR,
	ARG_OUTLINE_COLOR_GDK,
	ARG_OUTLINE_COLOR_RGBA,
	ARG_FILL_STIPPLE,
	ARG_OUTLINE_STIPPLE,
	ARG_WIDTH_PIXELS,
	ARG_WIDTH_UNITS
};


static void gossip_canvas_polygon_class_init (GossipCanvasPolygonClass *class);
static void gossip_canvas_polygon_init       (GossipCanvasPolygon      *poly);
static void gossip_canvas_polygon_destroy    (GtkObject               *object);
static void gossip_canvas_polygon_set_arg    (GtkObject               *object,
					     GtkArg                  *arg,
					     guint                    arg_id);
static void gossip_canvas_polygon_get_arg    (GtkObject               *object,
					     GtkArg                  *arg,
					     guint                    arg_id);

static void   gossip_canvas_polygon_set_coords  (GossipCanvasItem *item, int n_coords, double *coords);
static void   gossip_canvas_polygon_update      (GossipCanvasItem *item, double *affine, int flags);
static void   gossip_canvas_polygon_realize     (GossipCanvasItem *item);
static void   gossip_canvas_polygon_unrealize   (GossipCanvasItem *item);
static void   gossip_canvas_polygon_draw        (GossipCanvasItem *item, GdkDrawable *drawable,
						int x, int y, int width, int height);
static double gossip_canvas_polygon_point       (GossipCanvasItem *item, double x, double y,
						int cx, int cy, GossipCanvasItem **actual_item);
static void   gossip_canvas_polygon_translate   (GossipCanvasItem *item, double dx, double dy);
static void   gossip_canvas_polygon_bounds      (GossipCanvasItem *item, double *x1, double *y1, double *x2, double *y2);
#if 0
static void   gossip_canvas_polygon_render      (GossipCanvasItem *item, GossipCanvasBuf *buf);
#endif

static GossipCanvasItemClass *parent_class;


GtkType
gossip_canvas_polygon_get_type (void)
{
	static GtkType polygon_type = 0;

	if (!polygon_type) {
		GtkTypeInfo polygon_info = {
			"GossipCanvasPolygon",
			sizeof (GossipCanvasPolygon),
			sizeof (GossipCanvasPolygonClass),
			(GtkClassInitFunc) gossip_canvas_polygon_class_init,
			(GtkObjectInitFunc) gossip_canvas_polygon_init,
			NULL, /* reserved_1 */
			NULL, /* reserved_2 */
			(GtkClassInitFunc) NULL
		};

		polygon_type = gtk_type_unique (gossip_canvas_item_get_type (), &polygon_info);
	}

	return polygon_type;
}

static void
gossip_canvas_polygon_class_init (GossipCanvasPolygonClass *class)
{
	GtkObjectClass *object_class;
	GossipCanvasItemClass *item_class;

	object_class = (GtkObjectClass *) class;
	item_class = (GossipCanvasItemClass *) class;

	parent_class = gtk_type_class (gossip_canvas_item_get_type ());

#if 0
	gtk_object_add_arg_type ("GossipCanvasPolygon::points", GTK_TYPE_GOSSIP_CANVAS_POINTS, GTK_ARG_READWRITE, ARG_POINTS);
#endif
	gtk_object_add_arg_type ("GossipCanvasPolygon::fill_color", GTK_TYPE_STRING, GTK_ARG_WRITABLE, ARG_FILL_COLOR);
	gtk_object_add_arg_type ("GossipCanvasPolygon::fill_color_gdk", GTK_TYPE_GDK_COLOR, GTK_ARG_READWRITE, ARG_FILL_COLOR_GDK);
	gtk_object_add_arg_type ("GossipCanvasPolygon::fill_color_rgba", GTK_TYPE_UINT, GTK_ARG_READWRITE, ARG_FILL_COLOR_RGBA);
	gtk_object_add_arg_type ("GossipCanvasPolygon::outline_color", GTK_TYPE_STRING, GTK_ARG_WRITABLE, ARG_OUTLINE_COLOR);
	gtk_object_add_arg_type ("GossipCanvasPolygon::outline_color_gdk", GTK_TYPE_GDK_COLOR, GTK_ARG_READWRITE, ARG_OUTLINE_COLOR_GDK);
	gtk_object_add_arg_type ("GossipCanvasPolygon::outline_color_rgba", GTK_TYPE_UINT, GTK_ARG_READWRITE, ARG_OUTLINE_COLOR_RGBA);
	gtk_object_add_arg_type ("GossipCanvasPolygon::fill_stipple", GTK_TYPE_GDK_WINDOW, GTK_ARG_READWRITE, ARG_FILL_STIPPLE);
	gtk_object_add_arg_type ("GossipCanvasPolygon::outline_stipple", GTK_TYPE_GDK_WINDOW, GTK_ARG_READWRITE, ARG_OUTLINE_STIPPLE);
	gtk_object_add_arg_type ("GossipCanvasPolygon::width_pixels", GTK_TYPE_UINT, GTK_ARG_WRITABLE, ARG_WIDTH_PIXELS);
	gtk_object_add_arg_type ("GossipCanvasPolygon::width_units", GTK_TYPE_DOUBLE, GTK_ARG_WRITABLE, ARG_WIDTH_UNITS);

	object_class->destroy = gossip_canvas_polygon_destroy;
	object_class->set_arg = gossip_canvas_polygon_set_arg;
	object_class->get_arg = gossip_canvas_polygon_get_arg;

	item_class->set_coords = gossip_canvas_polygon_set_coords;
	item_class->update = gossip_canvas_polygon_update;
	item_class->realize = gossip_canvas_polygon_realize;
	item_class->unrealize = gossip_canvas_polygon_unrealize;
	item_class->draw = gossip_canvas_polygon_draw;
	item_class->point = gossip_canvas_polygon_point;
	item_class->translate = gossip_canvas_polygon_translate;
	item_class->bounds = gossip_canvas_polygon_bounds;
#if 0
	item_class->render = gossip_canvas_polygon_render;
#endif
}

static void
gossip_canvas_polygon_init (GossipCanvasPolygon *poly)
{
	poly->width = 0.0;
}

static void
gossip_canvas_polygon_destroy (GtkObject *object)
{
	GossipCanvasPolygon *poly;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GOSSIP_IS_CANVAS_POLYGON (object));

	poly = GOSSIP_CANVAS_POLYGON (object);

	if (poly->coords)
		g_free (poly->coords);

	if (poly->fill_stipple)
		gdk_bitmap_unref (poly->fill_stipple);

	if (poly->outline_stipple)
		gdk_bitmap_unref (poly->outline_stipple);

#if 0
	if (poly->fill_svp)
		art_svp_free (poly->fill_svp);

	if (poly->outline_svp)
		art_svp_free (poly->outline_svp);
#endif

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		(* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

/* Computes the bounding box of the polygon.  Assumes that the number of points in the polygon is
 * not zero.
 */
static void
get_bounds (GossipCanvasPolygon *poly, double *bx1, double *by1, double *bx2, double *by2)
{
	double *coords;
	double x1, y1, x2, y2;
	double width;
	int i;

	/* Compute bounds of vertices */

	x1 = x2 = poly->coords[0];
	y1 = y2 = poly->coords[1];

	for (i = 1, coords = poly->coords + 2; i < poly->num_points; i++, coords += 2) {
		GROW_BOUNDS (x1, y1, x2, y2, coords[0], coords[1]);
	}

	/* Add outline width */

	if (poly->width_pixels)
		width = (poly->width? poly->width:1) / poly->item.canvas->pixels_per_unit;
	else
		width = poly->width;

	width /= 2.0;

	x1 -= width;
	y1 -= width;
	x2 += width;
	y2 += width;

	/* Done */

	*bx1 = x1;
	*by1 = y1;
	*bx2 = x2;
	*by2 = y2;
}

/* Computes the bounding box of the polygon, in canvas coordinates.  Assumes that the number of points in the polygon is
 * not zero.
 */
static void
get_bounds_canvas (GossipCanvasPolygon *poly, double *bx1, double *by1, double *bx2, double *by2, double affine[6])
{
	GossipCanvasItem *item;
	ArtDRect bbox_world;
	ArtDRect bbox_canvas;
	double i2w[6], w2c[6], i2c[6];

	item = GOSSIP_CANVAS_ITEM (poly);

	get_bounds (poly, &bbox_world.x0, &bbox_world.y0, &bbox_world.x1, &bbox_world.y1);

	art_drect_affine_transform (&bbox_canvas, &bbox_world, affine);
	/* include 1 pixel of fudge */
	*bx1 = bbox_canvas.x0 - 1;
	*by1 = bbox_canvas.y0 - 1;
	*bx2 = bbox_canvas.x1 + 1;
	*by2 = bbox_canvas.y1 + 1;
}

/* Recalculates the canvas bounds for the polygon */
static void
recalc_bounds (GossipCanvasPolygon *poly)
{
	GossipCanvasItem *item;
	double x1, y1, x2, y2;
	int cx1, cy1, cx2, cy2;
	double dx, dy;

	item = GOSSIP_CANVAS_ITEM (poly);

	if (poly->num_points == 0) {
		item->x1 = item->y1 = item->x2 = item->y2 = 0;
		return;
	}

	/* Get bounds in world coordinates */

	get_bounds (poly, &x1, &y1, &x2, &y2);

	/* Convert to canvas pixel coords */

	dx = dy = 0.0;
	gossip_canvas_item_i2w (item, &dx, &dy);

	gossip_canvas_w2c (item->canvas, x1 + dx, y1 + dy, &cx1, &cy1);
	gossip_canvas_w2c (item->canvas, x2 + dx, y2 + dy, &cx2, &cy2);
	item->x1 = cx1;
	item->y1 = cy1;
	item->x2 = cx2;
	item->y2 = cy2;

	/* Some safety fudging */

	item->x1--;
	item->y1--;
	item->x2++;
	item->y2++;

	gossip_canvas_group_child_bounds (GOSSIP_CANVAS_GROUP (item->parent), item);
}

/* Sets the points of the polygon item to the specified ones.  If needed, it will add a point to
 * close the polygon.
 */
static void
set_points (GossipCanvasPolygon *poly, int n_coords, double *coords)
{
	int duplicate;

	/* See if we need to duplicate the first point */

	duplicate = ((coords[0] != coords[n_coords-2])
		     || (coords[1] != coords[n_coords-1]));

	if (duplicate)
		poly->num_points = n_coords/2 + 1;
	else
		poly->num_points = n_coords/2;

	poly->coords = g_new (double, 2 * poly->num_points);
	memcpy (poly->coords, coords, 2 * (n_coords/2) * sizeof (double));

	if (duplicate) {
		poly->coords[2 * poly->num_points - 2] = poly->coords[0];
		poly->coords[2 * poly->num_points - 1] = poly->coords[1];
	}
}

/* Convenience function to set a GC's foreground color to the specified pixel value */
static void
set_gc_foreground (GdkGC *gc, gulong pixel)
{
	GdkColor c;

	if (!gc)
		return;

	c.pixel = pixel;
	gdk_gc_set_foreground (gc, &c);
}

/* Sets the stipple pattern for the specified gc */
static void
set_stipple (GdkGC *gc, GdkBitmap **internal_stipple, GdkBitmap *stipple, int reconfigure)
{
	if (*internal_stipple && !reconfigure)
		gdk_bitmap_unref (*internal_stipple);

	*internal_stipple = stipple;
	if (stipple && !reconfigure)
		gdk_bitmap_ref (stipple);

	if (gc) {
		if (stipple) {
			gdk_gc_set_stipple (gc, stipple);
			gdk_gc_set_fill (gc, GDK_STIPPLED);
		} else
			gdk_gc_set_fill (gc, GDK_SOLID);
	}
}

/* Recalculate the outline width of the polygon and set it in its GC */
static void
set_outline_gc_width (GossipCanvasPolygon *poly)
{
	int width;

	if (!poly->outline_gc)
		return;

	if (poly->width_pixels)
		width = (int) poly->width;
	else
		width = (int) (poly->width * poly->item.canvas->pixels_per_unit + 0.5);

	gdk_gc_set_line_attributes (poly->outline_gc, width,
				    GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);
}

static void
gossip_canvas_polygon_set_coords (GossipCanvasItem *item, int n_coords, double *coords)
{
  GossipCanvasPolygon *poly = GOSSIP_CANVAS_POLYGON(item);
  set_points (poly, n_coords, coords);
}

static void
gossip_canvas_polygon_set_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{
	GossipCanvasItem *item;
	GossipCanvasPolygon *poly;
	GossipCanvasPoints *points;
	GdkColor color;

	item = GOSSIP_CANVAS_ITEM (object);
	poly = GOSSIP_CANVAS_POLYGON (object);

	switch (arg_id) {
#if 0
	case ARG_POINTS:
		points = GTK_VALUE_POINTER (*arg);

		if (poly->coords) {
			g_free (poly->coords);
			poly->coords = NULL;
		}

		if (!points)
			poly->num_points = 0;
		else
			set_points (poly, points);

		gossip_canvas_item_request_update (item);
		break;
#endif

	case ARG_FILL_COLOR:
		if (gossip_canvas_get_color (item->canvas, GTK_VALUE_STRING (*arg), &color)) {
			poly->fill_set = TRUE;
			poly->fill_pixel = color.pixel;
#if 0
			if (item->canvas->aa)
				poly->fill_rgba =
					((color.red & 0xff00) << 16) |
					((color.green & 0xff00) << 8) |
					(color.blue & 0xff00) |
					0xff;
			else
#endif
				set_gc_foreground (poly->fill_gc, poly->fill_pixel);
		} else {
			poly->fill_set = FALSE;
#if 0
			poly->fill_rgba = 0;
#endif
		}

		gossip_canvas_item_request_update (item);
		break;

	case ARG_FILL_COLOR_GDK:
		poly->fill_set = TRUE;
		poly->fill_pixel = ((GdkColor *) GTK_VALUE_BOXED (*arg))->pixel;
		set_gc_foreground (poly->fill_gc, poly->fill_pixel);
		gossip_canvas_item_request_update (item);
		break;

#if 0
	case ARG_FILL_COLOR_RGBA:
		poly->fill_set = TRUE;
		poly->fill_rgba = GTK_VALUE_UINT (*arg);

		/* should probably request repaint on the fill_svp */
		gossip_canvas_item_request_update (item);

		break;
#endif

	case ARG_OUTLINE_COLOR:
		if (gossip_canvas_get_color (item->canvas, GTK_VALUE_STRING (*arg), &color)) {
			poly->outline_set = TRUE;
			poly->outline_pixel = color.pixel;
#if 0
			if (item->canvas->aa)
				poly->outline_rgba =
					((color.red & 0xff00) << 16) |
					((color.green & 0xff00) << 8) |
					(color.blue & 0xff00) |
					0xff;
			else
#endif
				set_gc_foreground (poly->outline_gc, poly->outline_pixel);
		} else {
			poly->outline_set = FALSE;
#if 0
			poly->outline_rgba = 0;
#endif
		}

		gossip_canvas_item_request_update (item);
		break;

	case ARG_OUTLINE_COLOR_GDK:
		poly->outline_set = TRUE;
		poly->outline_pixel = ((GdkColor *) GTK_VALUE_BOXED (*arg))->pixel;
		set_gc_foreground (poly->outline_gc, poly->outline_pixel);
		gossip_canvas_item_request_update (item);
		break;

	case ARG_FILL_STIPPLE:
		set_stipple (poly->fill_gc, &poly->fill_stipple, GTK_VALUE_BOXED (*arg), FALSE);
		gossip_canvas_item_request_update (item);
		break;

	case ARG_OUTLINE_STIPPLE:
		set_stipple (poly->outline_gc, &poly->outline_stipple, GTK_VALUE_BOXED (*arg), FALSE);
		gossip_canvas_item_request_update (item);
		break;

	case ARG_WIDTH_PIXELS:
		poly->width = GTK_VALUE_UINT (*arg);
		poly->width_pixels = TRUE;
		set_outline_gc_width (poly);
#ifdef OLD_XFORM
		recalc_bounds (poly);
#else
		gossip_canvas_item_request_update (item);
#endif
		break;

	case ARG_WIDTH_UNITS:
		poly->width = fabs (GTK_VALUE_DOUBLE (*arg));
		poly->width_pixels = FALSE;
		set_outline_gc_width (poly);
#ifdef OLD_XFORM
		recalc_bounds (poly);
#else
		gossip_canvas_item_request_update (item);
#endif
		break;

	default:
		break;
	}
}

/* Allocates a GdkColor structure filled with the specified pixel, and puts it into the specified
 * arg for returning it in the get_arg method.
 */
static void
get_color_arg (GossipCanvasPolygon *poly, gulong pixel, GtkArg *arg)
{
	GdkColor *color;

	color = g_new (GdkColor, 1);
	color->pixel = pixel;
	gdk_color_context_query_color (poly->item.canvas->cc, color);
	GTK_VALUE_BOXED (*arg) = color;
}

static void
gossip_canvas_polygon_get_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{
	GossipCanvasPolygon *poly;
	GossipCanvasPoints *points;
	GdkColor *color;

	poly = GOSSIP_CANVAS_POLYGON (object);

	switch (arg_id) {
#if 0
	case ARG_POINTS:
		if (poly->num_points != 0) {
			points = gossip_canvas_points_new (poly->num_points);
			memcpy (points->coords, poly->coords, 2 * poly->num_points * sizeof (double));
			GTK_VALUE_POINTER (*arg) = points;
		} else
			GTK_VALUE_POINTER (*arg) = NULL;
		break;
#endif

	case ARG_FILL_COLOR_GDK:
		get_color_arg (poly, poly->fill_pixel, arg);
		break;
		
	case ARG_OUTLINE_COLOR_GDK:
		get_color_arg (poly, poly->outline_pixel, arg);
		break;

	case ARG_FILL_COLOR_RGBA:
		GTK_VALUE_UINT (*arg) = poly->fill_color;
		break;

	case ARG_FILL_STIPPLE:
		GTK_VALUE_BOXED (*arg) = poly->fill_stipple;
		break;

	case ARG_OUTLINE_STIPPLE:
		GTK_VALUE_BOXED (*arg) = poly->outline_stipple;
		break;

	default:
		arg->type = GTK_TYPE_INVALID;
		break;
	}
}

#if 0
static void
gossip_canvas_polygon_render (GossipCanvasItem *item,
			     GossipCanvasBuf *buf)
{
	GossipCanvasPolygon *poly;
	guint32 fg_color, bg_color;

	poly = GOSSIP_CANVAS_POLYGON (item);

	if (poly->fill_svp != NULL)
		gossip_canvas_render_svp (buf, poly->fill_svp, poly->fill_rgba);

	if (poly->outline_svp != NULL)
		gossip_canvas_render_svp (buf, poly->outline_svp, poly->outline_rgba);
}
#endif

static void
gossip_canvas_polygon_update (GossipCanvasItem *item, double *affine, int flags)
{
	GossipCanvasPolygon *poly;
#if 0
	ArtVpath *vpath;
	ArtPoint pi, pc;
	ArtSVP *svp;
#endif
	int i;
	double width;
	double x1, y1, x2, y2;

	poly = GOSSIP_CANVAS_POLYGON (item);

	if (parent_class->update)
		(* parent_class->update) (item, affine, flags);

#if 0
	if (item->canvas->aa) {
		gossip_canvas_item_reset_bounds (item);

		vpath = art_new (ArtVpath, poly->num_points + 2);

		for (i = 0; i < poly->num_points; i++) {
			pi.x = poly->coords[i * 2];
			pi.y = poly->coords[i * 2 + 1];
			art_affine_point (&pc, &pi, affine);
			vpath[i].code = i == 0 ? ART_MOVETO : ART_LINETO;
			vpath[i].x = pc.x;
			vpath[i].y = pc.y;
		}
		vpath[i].code = ART_END;
		vpath[i].x = 0;
		vpath[i].y = 0;

		if (poly->fill_set) {
			svp = art_svp_from_vpath (vpath);
			gossip_canvas_item_update_svp_clip (item, &poly->fill_svp, svp, clip_path);
		}

		if (poly->outline_set) {
			if (poly->width_pixels)
				width = poly->width;
			else
				width = poly->width * item->canvas->pixels_per_unit;
		
			if (width < 0.5)
				width = 0.5;
		
			svp = art_svp_vpath_stroke (vpath,
						    ART_PATH_STROKE_JOIN_MITER,
						    ART_PATH_STROKE_CAP_BUTT,
						    width,
						    4,
						    0.5);

			gossip_canvas_item_update_svp_clip (item, &poly->outline_svp, svp, clip_path);
		}
		art_free (vpath);

	} else 
#endif
	  {
		set_outline_gc_width (poly);
		set_gc_foreground (poly->fill_gc, poly->fill_pixel);
		set_gc_foreground (poly->outline_gc, poly->outline_pixel);
		set_stipple (poly->fill_gc, &poly->fill_stipple, poly->fill_stipple, TRUE);
		set_stipple (poly->outline_gc, &poly->outline_stipple, poly->outline_stipple, TRUE);

		get_bounds_canvas (poly, &x1, &y1, &x2, &y2, affine);
		gossip_canvas_update_bbox (item, x1, y1, x2, y2);		
	}
}

static void
gossip_canvas_polygon_realize (GossipCanvasItem *item)
{
	GossipCanvasPolygon *poly;

	poly = GOSSIP_CANVAS_POLYGON (item);

	if (parent_class->realize)
		(* parent_class->realize) (item);

	poly->fill_gc = gdk_gc_new (item->canvas->layout.bin_window);
	poly->outline_gc = gdk_gc_new (item->canvas->layout.bin_window);

#ifdef OLD_XFORM
	(* GOSSIP_CANVAS_ITEM_CLASS (item->object.klass)->update) (item, NULL, NULL, 0);
#endif
}

static void
gossip_canvas_polygon_unrealize (GossipCanvasItem *item)
{
	GossipCanvasPolygon *poly;

	poly = GOSSIP_CANVAS_POLYGON (item);

	gdk_gc_unref (poly->fill_gc);
	gdk_gc_unref (poly->outline_gc);

	if (parent_class->unrealize)
		(* parent_class->unrealize) (item);
}

/* Converts an array of world coordinates into an array of canvas pixel coordinates.  Takes in the
 * item->world deltas and the drawable deltas.
 */
static void
item_to_canvas (GossipCanvas *canvas, double *item_coords, GdkPoint *canvas_coords, int num_points,
		double i2c[6])
{
	int i;
	ArtPoint pi, pc;

#ifdef VERBOSE
	{
		char str[128];
		art_affine_to_string (str, i2c);
		g_print ("polygon item_to_canvas %s\n", str);
	}
#endif

	for (i = 0; i < num_points; i++) {
		pi.x = item_coords[i * 2];
		pi.y = item_coords[i * 2 + 1];
		art_affine_point (&pc, &pi, i2c);
		canvas_coords->x = floor (pc.x + 0.5);
		canvas_coords->y = floor (pc.y + 0.5);
		canvas_coords++; 
	}
}

static void
gossip_canvas_polygon_draw (GossipCanvasItem *item, GdkDrawable *drawable,
			   int x, int y, int width, int height)
{
	GossipCanvasPolygon *poly;
	GdkPoint static_points[NUM_STATIC_POINTS];
	GdkPoint *points;
	double dx, dy;
	int cx, cy;
	int i;
	double i2c[6];

	poly = GOSSIP_CANVAS_POLYGON (item);

	if (poly->num_points == 0)
		return;

	/* Build array of canvas pixel coordinates */

	if (poly->num_points <= NUM_STATIC_POINTS)
		points = static_points;
	else
		points = g_new (GdkPoint, poly->num_points);

	gossip_canvas_item_i2c_affine (item, i2c);

	i2c[4] -= x;
	i2c[5] -= y;

	item_to_canvas (item->canvas, poly->coords, points, poly->num_points, i2c);

	if (poly->fill_set) {
		if (poly->fill_stipple)
			gossip_canvas_set_stipple_origin (item->canvas, poly->fill_gc);

		gdk_draw_polygon (drawable, poly->fill_gc, TRUE, points, poly->num_points);
	}

	if (poly->outline_set) {
		if (poly->outline_stipple)
			gossip_canvas_set_stipple_origin (item->canvas, poly->outline_gc);

		gdk_draw_polygon (drawable, poly->outline_gc, FALSE, points, poly->num_points);
	}

	/* Done */

	if (points != static_points)
		g_free (points);
}

static double
gossip_canvas_polygon_point (GossipCanvasItem *item, double x, double y,
			    int cx, int cy, GossipCanvasItem **actual_item)
{
	GossipCanvasPolygon *poly;
	double dist;
	double width;

	poly = GOSSIP_CANVAS_POLYGON (item);

	*actual_item = item;

	dist = gossip_canvas_polygon_to_point (poly->coords, poly->num_points, x, y);

	if (poly->outline_set) {
		if (poly->width_pixels)
			width = poly->width / item->canvas->pixels_per_unit;
		else
			width = poly->width;

		dist -= width / 2.0;

		if (dist < 0.0)
			dist = 0.0;
	}

	return dist;
}

static void
gossip_canvas_polygon_translate (GossipCanvasItem *item, double dx, double dy)
{
	GossipCanvasPolygon *poly;
	int i;
	double *coords;

	poly = GOSSIP_CANVAS_POLYGON (item);

	for (i = 0, coords = poly->coords; i < poly->num_points; i++, coords += 2) {
		coords[0] += dx;
		coords[1] += dy;
	}

	recalc_bounds (poly);
}

static void
gossip_canvas_polygon_bounds (GossipCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
	GossipCanvasPolygon *poly;

	g_return_if_fail (item != NULL);
	g_return_if_fail (GOSSIP_IS_CANVAS_POLYGON (item));

	poly = GOSSIP_CANVAS_POLYGON (item);

	if (poly->num_points == 0) {
		*x1 = *y1 = *x2 = *y2 = 0.0;
		return;
	}

	get_bounds (poly, x1, y1, x2, y2);
}
