/* Grid item type for GossipCanvas widget
 *
 * GossipCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#ifndef GOSSIP_CANVAS_GRID_H
#define GOSSIP_CANVAS_GRID_H

#include "gossip-canvas.h"


/* Grid item for the canvas.  Grid items always occupy the whole canvas.
 *
 * The grid spacing is specified by the first two coordinates.
 *
 * The following object arguments are available:
 *
 * name			type			read/write	description
 * ------------------------------------------------------------------------------------------
 * color		string			W		X color specification for grid points
 * color_gdk	        GdkColor*		RW		Pointer to an allocated GdkColor
 */

#define GOSSIP_TYPE_CANVAS_GRID            (gossip_canvas_grid_get_type ())
#define GOSSIP_CANVAS_GRID(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS_GRID, GossipCanvasGrid))
#define GOSSIP_CANVAS_GRID_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS_GRID, GossipCanvasGridClass))
#define GOSSIP_IS_CANVAS_GRID(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS_GRID))
#define GOSSIP_IS_CANVAS_GRID_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS_GRID))


typedef struct _GossipCanvasGrid GossipCanvasGrid;
typedef struct _GossipCanvasGridClass GossipCanvasGridClass;

struct _GossipCanvasGrid {
  GossipCanvasItem item;

  double grid_x, grid_y;        /* Grid spacing */
  gulong pixel;		        /* Grid color */
  GdkGC *gc;                    /* GC for drawing grid */
};

struct _GossipCanvasGridClass {
  GossipCanvasItemClass parent_class;
};


/* Standard Gtk function */
GtkType gossip_canvas_grid_get_type (void);

#endif
