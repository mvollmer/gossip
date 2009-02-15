/* GossipCanvas widget - Tk-like canvas widget for Gnome
 *
 * GossipCanvas is basically a port of the Tk toolkit's most excellent canvas
 * widget.  Tk is copyrighted by the Regents of the University of California,
 * Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Authors: Federico Mena <federico@nuclecu.unam.mx>
 *          Raph Levien <raph@gimp.org>
 */

#ifndef GOSSIP_CANVAS_H
#define GOSSIP_CANVAS_H

#include <gtk/gtklayout.h>
#include <stdarg.h>


/* "Small" value used by canvas stuff */
#define GOSSIP_CANVAS_EPSILON 1e-10


/* Macros for building colors.  The values are in [0, 255] */

#define GOSSIP_CANVAS_COLOR(r, g, b) ((((int) (r) & 0xff) << 24)	\
				     | (((int) (g) & 0xff) << 16)	\
				     | (((int) (b) & 0xff) << 8)	\
				     | 0xff)

#define GOSSIP_CANVAS_COLOR_A(r, g, b, a) ((((int) (r) & 0xff) << 24)	\
					  | (((int) (g) & 0xff) << 16)	\
					  | (((int) (b) & 0xff) << 8)	\
					  | ((int) (a) & 0xff))


typedef struct _GossipCanvas           GossipCanvas;
typedef struct _GossipCanvasClass      GossipCanvasClass;
typedef struct _GossipCanvasGroup      GossipCanvasGroup;
typedef struct _GossipCanvasGroupClass GossipCanvasGroupClass;


/* GossipCanvasItem - base item class for canvas items
 *
 * All canvas items are derived from GossipCanvasItem.  The only information a GossipCanvasItem
 * contains is its parent canvas, its parent canvas item group, and its bounding box in canvas pixel
 * coordinates.
 *
 * Items inside a canvas are organized in a tree of GossipCanvasItemGroup nodes and GossipCanvasItem
 * leaves.  Each canvas has a single root group, which can be obtained with the
 * gossip_canvas_get_root() function.
 *
 * The abstract GossipCanvasItem class does not have any configurable or queryable attributes.
 */


/* Object flags for items */
enum {
	GOSSIP_CANVAS_ITEM_REALIZED      = 1 << 4,
	GOSSIP_CANVAS_ITEM_MAPPED        = 1 << 5,
	GOSSIP_CANVAS_ITEM_ALWAYS_REDRAW = 1 << 6,
	GOSSIP_CANVAS_ITEM_VISIBLE       = 1 << 7,
	GOSSIP_CANVAS_ITEM_NEED_UPDATE	= 1 << 8,
	GOSSIP_CANVAS_ITEM_NEED_AFFINE	= 1 << 9,
	GOSSIP_CANVAS_ITEM_NEED_CLIP	= 1 << 10,
	GOSSIP_CANVAS_ITEM_NEED_VIS	= 1 << 11,
	GOSSIP_CANVAS_ITEM_AFFINE_FULL  = 1 << 12
};

/* Update flags for items */
enum {
	GOSSIP_CANVAS_UPDATE_REQUESTED  = 1 << 0,
	GOSSIP_CANVAS_UPDATE_AFFINE     = 1 << 1,
	GOSSIP_CANVAS_UPDATE_CLIP       = 1 << 2,
	GOSSIP_CANVAS_UPDATE_VISIBILITY = 1 << 3,
	GOSSIP_CANVAS_UPDATE_IS_VISIBLE = 1 << 4
};

#define GOSSIP_TYPE_CANVAS_ITEM            (gossip_canvas_item_get_type ())
#define GOSSIP_CANVAS_ITEM(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS_ITEM, GossipCanvasItem))
#define GOSSIP_CANVAS_ITEM_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS_ITEM, GossipCanvasItemClass))
#define GOSSIP_IS_CANVAS_ITEM(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS_ITEM))
#define GOSSIP_IS_CANVAS_ITEM_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS_ITEM))


typedef struct _GossipCanvasItem GossipCanvasItem;
typedef struct _GossipCanvasItemClass GossipCanvasItemClass;

struct _GossipCanvasItem {
	GtkObject object;

	GossipCanvas *canvas;		/* The parent canvas for this item */
	GossipCanvasItem *parent;	/* The parent canvas group (of type GossipCanvasGroup) */

	double x1, y1, x2, y2;		/* Bounding box for this item, in canvas pixel coordinates.
					 * The bounding box contains (x1, y1) but not (x2, y2).
					 */

	double *xform;			/* If NULL, then the identity transform. If !AFFINE_FULL, then a two element array
					   containing a translation. If AFFINE_FULL, then a six element array containing
					   an affine transformation. */
};

struct _GossipCanvasItemClass {
	GtkObjectClass parent_class;

        /* Set the coords of the item. */
        void (* set_coords) (GossipCanvasItem *item, int n_coords, double *coords);

	/* Tell the item to update itself.  The flags are from the update flags
	 * defined above.  The item should update its internal state from its
	 * queued state, recompute and request its repaint area, etc.  The
	 * affine, if used, is a pointer to a 6-element array of doubles.
	 * The update method also recomputes the bounds of the item.
	 */
	void (* update) (GossipCanvasItem *item, double *affine, int flags);

	/* Realize an item -- create GCs, etc. */
	void (* realize) (GossipCanvasItem *item);

	/* Unrealize an item */
	void (* unrealize) (GossipCanvasItem *item);

	/* Map an item - normally only need by items with their own GdkWindows */
	void (* map) (GossipCanvasItem *item);

	/* Unmap an item */
	void (* unmap) (GossipCanvasItem *item);

	/* Draw an item of this type.  (x, y) are the upper-left canvas pixel coordinates of the *
	 * drawable, a temporary pixmap, where things get drawn.  (width, height) are the dimensions
	 * of the drawable.
	 */
	void (* draw) (GossipCanvasItem *item, GdkDrawable *drawable,
		       int x, int y, int width, int height);

	/* Calculate the distance from an item to the specified point.  It also returns a canvas
         * item which is the item itself in the case of the object being an actual leaf item, or a
         * child in case of the object being a canvas group.  (cx, cy) are the canvas pixel
         * coordinates that correspond to the item-relative coordinates (x, y).
	 */
	double (* point) (GossipCanvasItem *item, double x, double y, int cx, int cy, GossipCanvasItem **actual_item);

	/* Move an item by the specified amount */
	void (* translate) (GossipCanvasItem *item, double dx, double dy);

	/* Fetch the item's bounding box (need not be exactly tight) */
	void (* bounds) (GossipCanvasItem *item, double *x1, double *y1, double *x2, double *y2);

	/* Signal: an event ocurred for an item of this type.  The (x, y) coordinates are in the
	 * canvas world coordinate system.
	 */
	gint (* event) (GossipCanvasItem *item, GdkEvent *event);
};


/* Standard Gtk function */
GtkType gossip_canvas_item_get_type (void);

/* Create a canvas item using the standard Gtk argument mechanism.  The item is automatically
 * inserted at the top of the specified canvas group.  The last argument must be a NULL pointer.
 */
GossipCanvasItem *gossip_canvas_item_new (GossipCanvasGroup *parent, GtkType type,
					  int n_coords, double *coords,
					  const gchar *first_arg_name, ...);

/* Same as above, with parsed args */
GossipCanvasItem *gossip_canvas_item_newv (GossipCanvasGroup *parent, GtkType type,
					   int n_coords, double *coords,
					   guint nargs, GtkArg *args);

/* Constructors for use in derived classes and language wrappers */
void gossip_canvas_item_construct (GossipCanvasItem *item, GossipCanvasGroup *parent,
				   int n_coords, double *coords,
				   const gchar *first_arg_name, va_list args);

void gossip_canvas_item_constructv (GossipCanvasItem *item, GossipCanvasGroup *parent,
				    int n_coords, double *coords,
				    guint nargs, GtkArg *args);

/* Configure an item using the standard Gtk argument mechanism.  The last argument must be a NULL pointer.
   N_COORDS == -1 means no change in coordinates. */
void gossip_canvas_item_set (GossipCanvasItem *item,
			     int n_corrds, double *coords,
			     const gchar *first_arg_name, ...);

/* Same as above, with parsed args */
void gossip_canvas_item_setv (GossipCanvasItem *item,
			      int n_corrds, double *coords,
			      guint nargs, GtkArg *args);

/* Used only for language wrappers and the like; ignore. */
void gossip_canvas_item_set_valist (GossipCanvasItem *item,
				    int n_corrds, double *coords,
				    const gchar *first_arg_name, va_list var_args);

/* Move an item by the specified amount */
void gossip_canvas_item_move (GossipCanvasItem *item, double dx, double dy);

/* Apply a relative affine transformation to the item. */
void gossip_canvas_item_affine_relative (GossipCanvasItem *item, const double affine[6]);

/* Apply an absolute affine transformation to the item. */
void gossip_canvas_item_affine_absolute (GossipCanvasItem *item, const double affine[6]);

/* Scale an item about a point by the specified factors */
void gossip_canvas_item_scale (GossipCanvasItem *item, double x, double y, double scale_x, double scale_y);

/* Rotate an item about a point by the specified number of degrees */
void gossip_canvas_item_rotate (GossipCanvasItem *item, double x, double y, double angle);

/* Raise an item in the z-order of its parent group by the specified
 * number of positions.  The specified number must be larger than or
 * equal to 1.
 */
void gossip_canvas_item_raise (GossipCanvasItem *item, int positions);

/* Lower an item in the z-order of its parent group by the specified
 * number of positions.  The specified number must be larger than or
 * equal to 1.
 */
void gossip_canvas_item_lower (GossipCanvasItem *item, int positions);

/* Raise an item to the top of its parent group's z-order. */
void gossip_canvas_item_raise_to_top (GossipCanvasItem *item);

/* Lower an item to the bottom of its parent group's z-order */
void gossip_canvas_item_lower_to_bottom (GossipCanvasItem *item);

/* Show an item (make it visible).  If the item is already shown, it has no effect. */
void gossip_canvas_item_show (GossipCanvasItem *item);

/* Hide an item (make it invisible).  If the item is already invisible, it has no effect. */
void gossip_canvas_item_hide (GossipCanvasItem *item);

/* Grab the mouse for the specified item.  Only the events in event_mask will be reported.  If
 * cursor is non-NULL, it will be used during the duration of the grab.  Time is a proper X event
 * time parameter.  Returns the same values as XGrabPointer().
 */
int gossip_canvas_item_grab (GossipCanvasItem *item, unsigned int event_mask, GdkCursor *cursor, guint32 etime);

/* Ungrabs the mouse -- the specified item must be the same that was passed to
 * gossip_canvas_item_grab().  Time is a proper X event time parameter.
 */
void gossip_canvas_item_ungrab (GossipCanvasItem *item, guint32 etime);

/* These functions convert from a coordinate system to another.  "w" is world coordinates and "i" is
 * item coordinates.
 */
void gossip_canvas_item_w2i (GossipCanvasItem *item, double *x, double *y);
void gossip_canvas_item_i2w (GossipCanvasItem *item, double *x, double *y);

/* Gets the affine transform that converts from item-relative coordinates to world coordinates.
 */
void gossip_canvas_item_i2w_affine (GossipCanvasItem *item, double affine[6]);

/* Gets the affine transform that converts from item-relative coordinates to canvas coordinates.
 */
void gossip_canvas_item_i2c_affine (GossipCanvasItem *item, double affine[6]);

/* Remove the item from its parent group and make the new group its parent.  The item will be put on
 * top of all the items in the new group.  The item's coordinates relative to its new parent to
 * *not* change -- this means that the item could potentially move on the screen.
 * 
 * The item and the group must be in the same canvas.  An item cannot be reparented to a group that
 * is the item itself or that is an inferior of the item.
 */
void gossip_canvas_item_reparent (GossipCanvasItem *item, GossipCanvasGroup *new_group);

/* Used to send all of the keystroke events to a specific item as well as GDK_FOCUS_CHANGE events. */
void gossip_canvas_item_grab_focus (GossipCanvasItem *item);

/* Fetch the bounding box of the item.  The bounding box may not be exactly tight, but the canvas
 * items will do the best they can.
 */
void gossip_canvas_item_get_bounds (GossipCanvasItem *item, double *x1, double *y1, double *x2, double *y2);

/* Request that the update method eventually gets called. */
void gossip_canvas_item_request_update (GossipCanvasItem *item);


/* GossipCanvasGroup - a group of canvas items
 *
 * A group is a node in the hierarchical tree of groups/items inside a canvas.  Groups serve to
 * give a logical structure to the items.
 *
 * Consider a circuit editor application that uses the canvas for its schematic display.
 * Hierarchically, there would be canvas groups that contain all the components needed for an
 * "adder", for example -- this includes some logic gates as well as wires.  You can move stuff
 * around in a convenient way by doing a gossip_canvas_item_move() of the hierarchical groups -- to
 * move an adder, simply move the group that represents the adder.
 *
 * The (xpos, ypos) fields of a canvas group are the relative origin for the group's children.
 *
 * The following arguments are available:
 *
 * name		type		read/write	description
 * --------------------------------------------------------------------------------
 * x		double		RW		X coordinate of group's origin
 * y		double		RW		Y coordinate of group's origin
 */


#define GOSSIP_TYPE_CANVAS_GROUP            (gossip_canvas_group_get_type ())
#define GOSSIP_CANVAS_GROUP(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS_GROUP, GossipCanvasGroup))
#define GOSSIP_CANVAS_GROUP_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS_GROUP, GossipCanvasGroupClass))
#define GOSSIP_IS_CANVAS_GROUP(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS_GROUP))
#define GOSSIP_IS_CANVAS_GROUP_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS_GROUP))


struct _GossipCanvasGroup {
	GossipCanvasItem item;

	GList *item_list;
	GList *item_list_end;

	/* The position of the group has been subsumed into the xform of all items */
#ifdef OLD_XFORM
	double xpos, ypos;	/* Point that defines the group's origin */
#endif
};

struct _GossipCanvasGroupClass {
	GossipCanvasItemClass parent_class;
};


/* Standard Gtk function */
GtkType gossip_canvas_group_get_type (void);

/* For use only by the core and item type implementations.  If item is non-null, then the group adds
 * the item's bounds to the current group's bounds, and propagates it upwards in the item hierarchy.
 * If item is NULL, then the group asks every sub-group to recalculate its bounds recursively, and
 * then propagates the bounds change upwards in the hierarchy.
 */
void gossip_canvas_group_child_bounds (GossipCanvasGroup *group, GossipCanvasItem *item);


/*** GossipCanvas ***/


#define GOSSIP_TYPE_CANVAS            (gossip_canvas_get_type ())
#define GOSSIP_CANVAS(obj)            (GTK_CHECK_CAST ((obj), GOSSIP_TYPE_CANVAS, GossipCanvas))
#define GOSSIP_CANVAS_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GOSSIP_TYPE_CANVAS, GossipCanvasClass))
#define GOSSIP_IS_CANVAS(obj)         (GTK_CHECK_TYPE ((obj), GOSSIP_TYPE_CANVAS))
#define GOSSIP_IS_CANVAS_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GOSSIP_TYPE_CANVAS))


struct _GossipCanvas {
	GtkLayout layout;

	guint idle_id;				/* Idle handler ID */

	GossipCanvasItem *root;			/* root canvas group */
	guint root_destroy_id;			/* Signal handler ID for destruction of root item */

	double scroll_x1, scroll_y1;		/* scrolling limit */
	double scroll_x2, scroll_y2;

	double pixels_per_unit;			/* scaling factor to be used for display */

	int redraw_x1, redraw_y1;
	int redraw_x2, redraw_y2;		/* Area that needs redrawing.  Contains (x1, y1)
						 * but not (x2, y2) -- specified in canvas pixel units.
						 */

	int draw_xofs, draw_yofs;		/* Offsets of the temporary drawing pixmap */

	int zoom_xofs, zoom_yofs; 		/* Internal pixel offsets for when zoomed out */

	int state;				/* Last known modifier state, for deferred repick
						 * when a button is down
						 */

	GossipCanvasItem *current_item;		/* The item containing the mouse pointer, or NULL if none */
	GossipCanvasItem *new_current_item;	/* Item that is about to become current
						 * (used to track deletions and such)
						 */
	GossipCanvasItem *grabbed_item;		/* Item that holds a pointer grab, or NULL if none */
	guint grabbed_event_mask;		/* Event mask specified when grabbing an item */

	GossipCanvasItem *focused_item;		/* If non-NULL the currently focused item */

	GdkEvent pick_event;			/* Event on which selection of current item is based */

	int close_enough;			/* Tolerance distance for picking items */

	GdkColorContext *cc;			/* Color context used for color allocation */
	GdkGC *pixmap_gc;			/* GC for temporary pixmap */

	unsigned int need_update : 1;		/* Will update at next idle loop iteration */
	unsigned int need_redraw : 1;		/* Will redraw at next idle loop iteration */
	unsigned int need_repick : 1;		/* Will repick current item at next idle loop iteration */
	unsigned int left_grabbed_item : 1;	/* For use by the internal pick_event function */
	unsigned int in_repick : 1;		/* For use by the internal pick_event function */

  GossipCanvasItem *floater;
  double floater_x, floater_y;
  double floater_grid_x, floater_grid_y;
};

struct _GossipCanvasClass {
	GtkLayoutClass parent_class;
};


/* Standard Gtk function */
GtkType gossip_canvas_get_type (void);

/* Creates a new canvas.  You should check that the canvas is created with the proper visual and
 * colormap if you want to insert imlib images into it.  You can do this by doing
 * gtk_widget_push_visual(gdk_imlib_get_visual()) and
 * gtk_widget_push_colormap(gdk_imlib_get_colormap()) before calling gossip_canvas_new().  After
 * calling it, you should do gtk_widget_pop_visual() and gtk_widget_pop_colormap().
 *
 * You should call gossip_canvas_set_scroll_region() soon after calling this function to set the
 * desired scrolling limits for the canvas.
 */
GtkWidget *gossip_canvas_new (void);

/* Returns the root canvas item group of the canvas */
GossipCanvasGroup *gossip_canvas_root (GossipCanvas *canvas);

/* Sets the limits of the scrolling region */
void gossip_canvas_set_scroll_region (GossipCanvas *canvas, double x1, double y1, double x2, double y2);

/* Gets the limits of the scrolling region */
void gossip_canvas_get_scroll_region (GossipCanvas *canvas, double *x1, double *y1, double *x2, double *y2);

/* Sets the number of pixels that correspond to one unit in world coordinates */
void gossip_canvas_set_pixels_per_unit (GossipCanvas *canvas, double n);

/* Scrolls the canvas to the specified offsets, given in canvas pixel coordinates */
void gossip_canvas_scroll_to (GossipCanvas *canvas, int cx, int cy);

/* Returns the scroll offsets of the canvas in canvas pixel coordinates.  You can specify NULL for
 * any of the values, in which case that value will not be queried.
 */
void gossip_canvas_get_scroll_offsets (GossipCanvas *canvas, int *cx, int *cy);

/* Requests that the canvas be repainted immediately instead of in the idle loop. */
void gossip_canvas_update_now (GossipCanvas *canvas);

/* Returns the item that is at the specified position in world coordinates, or
 * NULL if no item is there.
 */
GossipCanvasItem *gossip_canvas_get_item_at (GossipCanvas *canvas, double x, double y);

/* For use only by item type implementations.  Request that the canvas eventually redraw the
 * specified region.  The region contains (x1, y1) but not (x2, y2).
 */
void gossip_canvas_request_redraw (GossipCanvas *canvas, int x1, int y1, int x2, int y2);

/* Gets the affine transform that converts world coordinates into canvas pixel coordinates.
 */
void gossip_canvas_w2c_affine (GossipCanvas *canvas, double affine[6]);

/* These functions convert from a coordinate system to another.  "w" is world coordinates (the ones
 * in which objects are specified), "c" is canvas coordinates (pixel coordinates that are (0,0) for
 * the upper-left scrolling limit and something else for the lower-left scrolling limit).
 */
void gossip_canvas_w2c (GossipCanvas *canvas, double wx, double wy, int *cx, int *cy);
void gossip_canvas_w2c_d (GossipCanvas *canvas, double wx, double wy, double *cx, double *cy);
void gossip_canvas_c2w (GossipCanvas *canvas, int cx, int cy, double *wx, double *wy);

/* This function takes in coordinates relative to the GTK_LAYOUT (canvas)->bin_window and converts
 * them to world coordinates.
 */
void gossip_canvas_window_to_world (GossipCanvas *canvas, double winx, double winy, double *worldx, double *worldy);

/* This is the inverse of gossip_canvas_window_to_world */
void gossip_canvas_world_to_window (GossipCanvas *canvas, double worldx, double worldy, double *winx, double *winy);

/* Takes a string specification for a color and allocates it into the specified GdkColor.  If the
 * string is null, then it returns FALSE. Otherwise, it returns TRUE.
 */
int gossip_canvas_get_color (GossipCanvas *canvas, char *spec, GdkColor *color);

/* Sets the stipple origin of the specified gc so that it will be aligned with all the stipples used
 * in the specified canvas.  This is intended for use only by canvas item implementations.
 */
void gossip_canvas_set_stipple_origin (GossipCanvas *canvas, GdkGC *gc);

/* This is only here because guile-gtk is too stupid. */

int gossip_canvas_get_width (GossipCanvas *canvas);
int gossip_canvas_get_height (GossipCanvas *canvas);

/* Set the floating item of a canvas.  If there is a floating item, it
   will follow the pointer around when the pointer is over the canvas.
   When the pointer is outside the canvas, the floating item will be
   hidden.

   The reference point of the floating item is assumed to be at X,Y in
   canvas coordinates.  The floating item will move in increments of
   GRID_X,GRID_Y. */

void gossip_canvas_set_float_item (GossipCanvas *canvas,
				   GossipCanvasItem *item,
				   double x, double y,
				   double grid_x, double grid_y);

/* Get the position of the floating item. */

void gossip_canvas_get_float_pos (GossipCanvas *canvas,
				  double *x, double *y);

#endif
