/* Rectangle and ellipse item types for GossipCanvas widget
 *
 * GossipCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#ifndef GOSSIP_CANVAS_RECT_ELLIPSE_H
#define GOSSIP_CANVAS_RECT_ELLIPSE_H

#include "gossip-canvas.h"

/* Base class for rectangle and ellipse item types.  These are defined by their top-left and
 * bottom-right corners.  Rectangles and ellipses share the following arguments:
 *
 * name			type		read/write	description
 * ------------------------------------------------------------------------------------------
 * fill_color		string		W		X color specification for fill color,
 *							or NULL pointer for no color (transparent)
 * fill_color_gdk	GdkColor*	RW		Allocated GdkColor for fill
 * outline_color	string		W		X color specification for outline color,
 *							or NULL pointer for no color (transparent)
 * outline_color_gdk	GdkColor*	RW		Allocated GdkColor for outline
 * fill_stipple		GdkBitmap*	RW		Stipple pattern for fill
 * outline_stipple	GdkBitmap*	RW		Stipple pattern for outline
 * width_pixels		uint		RW		Width of the outline in pixels.  The outline will
 *							not be scaled when the canvas zoom factor is changed.
 * width_units		double		RW		Width of the outline in canvas units.  The outline
 *							will be scaled when the canvas zoom factor is changed.
 */


#define GOSSIP_TYPE_CANVAS_RE            (gossip_canvas_re_get_type ())
#define GOSSIP_CANVAS_RE(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS_RE, GossipCanvasRE))
#define GOSSIP_CANVAS_RE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS_RE, GossipCanvasREClass))
#define GOSSIP_IS_CANVAS_RE(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS_RE))
#define GOSSIP_IS_CANVAS_RE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS_RE))


typedef struct _GossipCanvasRE      GossipCanvasRE;
typedef struct _GossipCanvasREClass GossipCanvasREClass;

struct _GossipCanvasRE {
	GossipCanvasItem item;

	double x1, y1, x2, y2;		/* Corners of item */
	double width;			/* Outline width */

	guint fill_color;		/* Fill color, RGBA */
	guint outline_color;		/* Outline color, RGBA */

	gulong fill_pixel;		/* Fill color */
	gulong outline_pixel;		/* Outline color */

	GdkBitmap *fill_stipple;	/* Stipple for fill */
	GdkBitmap *outline_stipple;	/* Stipple for outline */

	GdkGC *fill_gc;			/* GC for filling */
	GdkGC *outline_gc;		/* GC for outline */

#if 0
	/* Antialiased specific stuff follows */

	ArtSVP *fill_svp;		/* The SVP for the filled shape */
	ArtSVP *outline_svp;		/* The SVP for the outline shape */
#endif

	/* Configuration flags */

	unsigned int fill_set : 1;	/* Is fill color set? */
	unsigned int outline_set : 1;	/* Is outline color set? */
	unsigned int width_pixels : 1;	/* Is outline width specified in pixels or units? */
};

struct _GossipCanvasREClass {
	GossipCanvasItemClass parent_class;
};


/* Standard Gtk function */
GtkType gossip_canvas_re_get_type (void);


/* Rectangle item.  No configurable or queryable arguments are available (use those in
 * GossipCanvasRE).
 */


#define GOSSIP_TYPE_CANVAS_RECT            (gossip_canvas_rect_get_type ())
#define GOSSIP_CANVAS_RECT(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS_RECT, GossipCanvasRect))
#define GOSSIP_CANVAS_RECT_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS_RECT, GossipCanvasRectClass))
#define GOSSIP_IS_CANVAS_RECT(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS_RECT))
#define GOSSIP_IS_CANVAS_RECT_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS_RECT))


typedef struct _GossipCanvasRect GossipCanvasRect;
typedef struct _GossipCanvasRectClass GossipCanvasRectClass;

struct _GossipCanvasRect {
	GossipCanvasRE re;
};

struct _GossipCanvasRectClass {
	GossipCanvasREClass parent_class;
};


/* Standard Gtk function */
GtkType gossip_canvas_rect_get_type (void);


/* Ellipse item.  No configurable or queryable arguments are available (use those in
 * GossipCanvasRE).
 */


#define GOSSIP_TYPE_CANVAS_ELLIPSE            (gossip_canvas_ellipse_get_type ())
#define GOSSIP_CANVAS_ELLIPSE(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS_ELLIPSE, GossipCanvasEllipse))
#define GOSSIP_CANVAS_ELLIPSE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS_ELLIPSE, GossipCanvasEllipseClass))
#define GOSSIP_IS_CANVAS_ELLIPSE(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS_ELLIPSE))
#define GOSSIP_IS_CANVAS_ELLIPSE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS_ELLIPSE))


typedef struct _GossipCanvasEllipse GossipCanvasEllipse;
typedef struct _GossipCanvasEllipseClass GossipCanvasEllipseClass;

struct _GossipCanvasEllipse {
	GossipCanvasRE re;
};

struct _GossipCanvasEllipseClass {
	GossipCanvasREClass parent_class;
};


/* Standard Gtk function */
GtkType gossip_canvas_ellipse_get_type (void);

#endif
