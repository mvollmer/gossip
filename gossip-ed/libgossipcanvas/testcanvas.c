#include <gtk/gtk.h>
#include <libgossipcanvas/gossip-canvas.h>
#include <libgossipcanvas/gossip-canvas-util.h>
#include <libgossipcanvas/gossip-canvas-line.h>
#include <libgossipcanvas/gossip-canvas-rect-ellipse.h>
#include <libgossipcanvas/gossip-canvas-polygon.h>

int
main (int argc, char **argv)
{
  GtkWidget *w;
  GtkWidget *c;
  GtkWidget *b;
  double pts[6];
  GossipCanvasItem *l;

  gtk_init (&argc, &argv);

  w = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  c = gossip_canvas_new ();
  gossip_canvas_set_scroll_region (GOSSIP_CANVAS(c), 0, 0, 200, 200);

  pts[0] = 10.0;
  pts[1] = 10.0;
  pts[2] = 100.0;
  pts[3] = 100.0;
  pts[4] = 190.0;
  pts[5] = 10.0;

  l = gossip_canvas_item_new (gossip_canvas_root (GOSSIP_CANVAS(c)),
			      GOSSIP_TYPE_CANVAS_POLYGON,
			      6, pts,
			      "fill_color", "red",
			      "outline_color", "green",
			      "width_pixels", 5,
			      NULL);

  b = gtk_button_new_with_label ("hallo");

  gtk_container_add (GTK_CONTAINER(w), c);
  gtk_widget_show_all (w);

  gtk_main ();
}
