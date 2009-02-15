/* Grid item type for GossipCanvas widget
 *
 * GossipCanvas is basically a port of the Tk toolkit's most excellent
 * canvas widget.  Tk is copyrighted by the Regents of the University
 * of California, Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx> */

#include <math.h>
#include "gossip-canvas-grid.h"
#include <gdk/gdkx.h> /* for BlackPixel */

#include "gossip-canvas-util.h"
#include "gossip-art.h"

enum {
  ARG_0,
  ARG_COLOR,
  ARG_COLOR_GDK,
};


static void gossip_canvas_grid_class_init (GossipCanvasGridClass *class);
static void gossip_canvas_grid_init       (GossipCanvasGrid      *grid);
static void gossip_canvas_grid_destroy    (GtkObject            *object);
static void gossip_canvas_grid_set_arg    (GtkObject            *object,
					  GtkArg               *arg,
					  guint                 arg_id);
static void gossip_canvas_grid_get_arg    (GtkObject            *object,
					  GtkArg               *arg,
					  guint                 arg_id);

static void   gossip_canvas_grid_set_coords  (GossipCanvasItem *item, int n_coords, double *coords);
static void   gossip_canvas_grid_update      (GossipCanvasItem *item, double *affine, int flags);
static void   gossip_canvas_grid_realize     (GossipCanvasItem *item);
static void   gossip_canvas_grid_unrealize   (GossipCanvasItem *item);
static void   gossip_canvas_grid_draw        (GossipCanvasItem *item, GdkDrawable *drawable,
					     int x, int y, int width, int height);
static double gossip_canvas_grid_point       (GossipCanvasItem *item, double x, double y,
					     int cx, int cy, GossipCanvasItem **actual_item);
static void   gossip_canvas_grid_translate   (GossipCanvasItem *item, double dx, double dy);
static void   gossip_canvas_grid_bounds      (GossipCanvasItem *item, double *x1, double *y1, double *x2, double *y2);

static GossipCanvasItemClass *parent_class;

GtkType
gossip_canvas_grid_get_type (void)
{
  static GtkType grid_type = 0;
  
  if (!grid_type) 
    {
      GtkTypeInfo grid_info = {
	"GossipCanvasGrid",
	sizeof (GossipCanvasGrid),
	sizeof (GossipCanvasGridClass),
	(GtkClassInitFunc) gossip_canvas_grid_class_init,
	(GtkObjectInitFunc) gossip_canvas_grid_init,
	NULL, /* reserved_1 */
	NULL, /* reserved_2 */
	(GtkClassInitFunc) NULL
      };

      grid_type = gtk_type_unique (gossip_canvas_item_get_type (), &grid_info);
    }

  return grid_type;
}

static void
gossip_canvas_grid_class_init (GossipCanvasGridClass *class)
{
  GtkObjectClass *object_class;
  GossipCanvasItemClass *item_class;

  object_class = (GtkObjectClass *) class;
  item_class = (GossipCanvasItemClass *) class;

  parent_class = gtk_type_class (gossip_canvas_item_get_type ());
  
  gtk_object_add_arg_type ("GossipCanvasGrid::color", GTK_TYPE_STRING, GTK_ARG_WRITABLE, ARG_COLOR);
  gtk_object_add_arg_type ("GossipCanvasGrid::color_gdk", GTK_TYPE_GDK_COLOR, GTK_ARG_READWRITE, ARG_COLOR_GDK);

  object_class->destroy = gossip_canvas_grid_destroy;
  object_class->set_arg = gossip_canvas_grid_set_arg;
  object_class->get_arg = gossip_canvas_grid_get_arg;

  item_class->set_coords = gossip_canvas_grid_set_coords;
  item_class->update = gossip_canvas_grid_update;
  item_class->realize = gossip_canvas_grid_realize;
  item_class->unrealize = gossip_canvas_grid_unrealize;
  item_class->draw = gossip_canvas_grid_draw;
  item_class->point = gossip_canvas_grid_point;
  item_class->translate = gossip_canvas_grid_translate;
  item_class->bounds = gossip_canvas_grid_bounds;
}

static void
gossip_canvas_grid_init (GossipCanvasGrid *grid)
{
  grid->grid_x = 10;
  grid->grid_y = 10;
}

static void
gossip_canvas_grid_destroy (GtkObject *object)
{
  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
get_bounds (GossipCanvasGrid *grid, double *px1, double *py1, double *px2, double *py2)
{
  GossipCanvas *canvas;

  /* The grid is everywhere. */
  *px1 = INT_MIN;
  *py1 = INT_MIN;
  *px2 = INT_MAX;
  *py2 = INT_MAX;
}

/* Recalculates the bounding box of the grid item. */
static void
recalc_bounds (GossipCanvasGrid *grid)
{
  GossipCanvasItem *item;

  item = GOSSIP_CANVAS_ITEM (grid);

  get_bounds (grid, &item->x1, &item->y1, &item->x2, &item->y2);

  gossip_canvas_group_child_bounds (GOSSIP_CANVAS_GROUP (item->parent), item);
}

/* Convenience function to set the grid's GC's foreground color */
static void
set_grid_gc_foreground (GossipCanvasGrid *grid)
{
  GdkColor c;

  if (!grid->gc)
    return;

  c.pixel = grid->pixel;
  gdk_gc_set_foreground (grid->gc, &c);
}

static void
gossip_canvas_grid_set_coords (GossipCanvasItem *item, int n_coords, double *coords)
{
  GossipCanvasGrid *grid = GOSSIP_CANVAS_GRID(item);
  if (n_coords >= 2)
    {
      grid->grid_x = coords[0];
      grid->grid_y = coords[1];
    }
}

static void
gossip_canvas_grid_set_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{
  GossipCanvasItem *item;
  GossipCanvasGrid *grid;
  GdkColor color;
  GdkColor *colorp;

  item = GOSSIP_CANVAS_ITEM (object);
  grid = GOSSIP_CANVAS_GRID (object);

  switch (arg_id) 
    {
    case ARG_COLOR:
      if (gossip_canvas_get_color (item->canvas, GTK_VALUE_STRING (*arg), &color)) 
	grid->pixel = color.pixel;
      else 
	grid->pixel = 0;
      set_grid_gc_foreground (grid);
      break;

    case ARG_COLOR_GDK:
      colorp = (GdkColor *) GTK_VALUE_BOXED (*arg);
      grid->pixel = colorp->pixel;
      set_grid_gc_foreground (grid);
      break;

    default:
      break;
    }
}

static void
gossip_canvas_grid_get_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{
  GossipCanvasGrid *grid;
  GdkColor *color;

  grid = GOSSIP_CANVAS_GRID (object);

  switch (arg_id) 
    {
    case ARG_COLOR_GDK:
      color = g_new (GdkColor, 1);
      color->pixel = grid->pixel;
      gdk_color_context_query_color (grid->item.canvas->cc, color);
      GTK_VALUE_BOXED (*arg) = color;
      break;

    default:
      arg->type = GTK_TYPE_INVALID;
      break;
    }
}

static void
gossip_canvas_grid_update (GossipCanvasItem *item, double *affine, int flags)
{
  GossipCanvasGrid *grid;
  double x1, y1, x2, y2;
  ArtDRect i_bbox, c_bbox;
  int i;

  grid = GOSSIP_CANVAS_GRID (item);

  if (parent_class->update)
    (* parent_class->update) (item, affine, flags);
  
  set_grid_gc_foreground (grid);
  get_bounds (grid, &x1, &y1, &x2, &y2);
  gossip_canvas_update_bbox (item, x1, y1, x2, y2);
}

static void
gossip_canvas_grid_realize (GossipCanvasItem *item)
{
  GossipCanvasGrid *grid;

  grid = GOSSIP_CANVAS_GRID (item);

  if (parent_class->realize)
    (* parent_class->realize) (item);

  grid->gc = gdk_gc_new (item->canvas->layout.bin_window);
}

static void
gossip_canvas_grid_unrealize (GossipCanvasItem *item)
{
  GossipCanvasGrid *grid;

  grid = GOSSIP_CANVAS_GRID (item);
  
  gdk_gc_unref (grid->gc);

  if (parent_class->unrealize)
    (* parent_class->unrealize) (item);
}

/* After much fun with doubles and rounding errors, etc, I decided to
   try fixed point numbers.  They are not able to represent the grid
   spacing exactly, but it's easier to get reproducible pixel
   positions out of them.  There should only be a difference between
   the `true' pixel positions (as dictated by the `true' grid spacing)
   for visible canvas sizes larger than FRACFAC/2.

   The fraction are susceptible to overflow errors, however.  They are
   only good for canvas coordinates from INT_MIN/FRACFAC to
   INT_MAX/FRACFAC.  We need to find a compromise here.  Assuming that
   the typical screen has no more than 4000 pixels in one direction,
   we get a FRACFAC of 8192, which leads on a 32bit machine to maximum
   canvas coordinates of -262144:262143.  But even the overflow is
   deterministic, so we can adapt to it, should it ever be necessary.

   I don't think using floating point for this is a win.  */

/* FRACFAC must be even and powers of two are nice. */
#define FRACFAC 10000
typedef long frac;

static frac
make_frac (double d)
{
  return (int)floor((d*FRACFAC+0.5));
}

static int
round_frac (frac f)
{
  if (f > 0)
    return (f+FRACFAC/2)/FRACFAC;
  else
    return (f-FRACFAC/2+1)/FRACFAC;
}

static frac
start_coord (int c, double o, frac g)
{
  frac cf = c*FRACFAC - make_frac (o);  /* XXX - deal with overflow. */
  frac m;

  if (cf > 0)
    {
      m = cf % g;
      if (m == 0)
	return m;
      else
	return g - m;
    }
  else
    return (-cf) % g;
}

static void
gossip_canvas_grid_draw (GossipCanvasItem *item, GdkDrawable *drawable,
			int x, int y, int width, int height)
{
  GossipCanvasGrid *grid;
  double i2c[6];
  ArtPoint delta_i, delta_c;
  frac gx, gy, dx, dy, sgx, sgy;
  double ox, oy;
  frac fwidth=width*FRACFAC, fheight = height*FRACFAC;
  int n;

  grid = GOSSIP_CANVAS_GRID (item);

  /* Can't cope with non-rectilinear transformations. */

  gossip_canvas_item_i2c_affine (item, i2c);
  ox = i2c[4]; i2c[4] = 0.0;
  oy = i2c[5]; i2c[5] = 0.0;

  delta_i.x = grid->grid_x;
  delta_i.y = 0.0;
  art_affine_point (&delta_c, &delta_i, i2c);
  dx = make_frac (delta_c.x);

  delta_i.x = 0.0;
  delta_i.y = grid->grid_y;
  art_affine_point (&delta_c, &delta_i, i2c);
  dy = make_frac (delta_c.y);

  // g_print ("\ngrid is %d, %d\n", dx, dy);
  // g_print ("drawing %d %d %d %d\n", x, y, width, height);

  if (dx == 0 || dy == 0)
    return;

  sgx = start_coord (x, ox, dx) - dx;
  sgy = start_coord (y, oy, dy) - dy;

  // g_print ("starting %d %d\n", sgx, sgy);
  // g_print ("first point: %d %d\n", round_frac (sgx), round_frac (sgy));

  n = 0;
  for (gx = sgx; gx <= fwidth; gx += dx)
    for (gy = sgy; gy <= fheight; gy += dy)
      {
	int cx = round_frac (gx);
	int cy = round_frac (gy);
	n++;
	gdk_draw_point (drawable, grid->gc, cx, cy);
      }
  // g_print ("%d drawn\n", n);
}

static double
gossip_canvas_grid_point (GossipCanvasItem *item, double x, double y,
			  int cx, int cy, GossipCanvasItem **actual_item)
{
  /* I told you the grid is everywhere. */

  *actual_item = item;
  return 0.0;
}

static void
gossip_canvas_grid_translate (GossipCanvasItem *item, double dx, double dy)
{
}

static void
gossip_canvas_grid_bounds (GossipCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
  g_assert (FALSE);
}
