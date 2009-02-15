#include <libguile.h>
#include <guile-gtk.h>
#include <libgossipcanvas/gossip-canvas.h>

/* helpers for double */

static int
_sgtk_helper_valid_double (SCM obj)
{
  return obj == SCM_BOOL_F || (sgtk_valid_double (obj));
}

static void
_sgtk_helper_fromscm_double (SCM obj, void *mem)
{
  *(double*)mem = sgtk_scm2double (obj);
}

static SCM
_sgtk_helper_toscm_copy_double (void *mem)
{
  return sgtk_double2scm ((*(double*)mem));
}

static SCM
_sgtk_helper_toscm_nocopy_double (void *mem)
{
  return sgtk_double2scm ((*(double*)mem));
}

static char s_gossip_canvas_item_new[] = "gossip-canvas-item-new";

SCM
sgtk_gossip_canvas_item_new (SCM p_parent, SCM p_type, SCM p_coords, SCM p_args)
{
  GossipCanvasItem* cr_ret;
  GossipCanvasGroup* c_parent;
  GtkType c_type;
  sgtk_cvec c_coords;
  int n_args;
  GtkArg *args;
  sgtk_object_info *info;

  SCM_ASSERT (sgtk_is_a_gtkobj (gossip_canvas_group_get_type (), p_parent), p_parent, SCM_ARG1, s_gossip_canvas_item_new);
  SCM_ASSERT (sgtk_valid_type (p_type), p_type, SCM_ARG2, s_gossip_canvas_item_new);
  SCM_ASSERT (sgtk_valid_composite (p_coords, _sgtk_helper_valid_double), p_coords, SCM_ARG3, s_gossip_canvas_item_new);

  n_args = scm_ilength (p_args);
  SCM_ASSERT (n_args >= 0 && (n_args%2) == 0, p_args,
	      SCM_ARG4, s_gossip_canvas_item_new);
  n_args = n_args/2;

  c_type = sgtk_scm2type (p_type);
  info = sgtk_find_object_info_from_type (c_type);
  SCM_ASSERT (info != NULL, p_type, SCM_ARG2, s_gossip_canvas_item_new);

  SCM_DEFER_INTS;
  c_parent = (GossipCanvasGroup*)sgtk_get_gtkobj (p_parent);
  c_coords = sgtk_scm2cvec (p_coords, _sgtk_helper_fromscm_double, sizeof (double));
  args = sgtk_build_args (info, &n_args, p_args, SCM_BOOL_F, s_gossip_canvas_item_new);
  cr_ret = gossip_canvas_item_newv (c_parent, c_type, c_coords.count, (double*)c_coords.vec, n_args, args);
  sgtk_cvec_finish (&c_coords, p_coords, NULL, sizeof(double));
  SCM_ALLOW_INTS;

  return sgtk_wrap_gtkobj ((GtkObject*)cr_ret);
}

static char s_gossip_canvas_item_set[] = "gossip-canvas-item-set";

SCM
sgtk_gossip_canvas_item_set (SCM p_item, SCM p_coords, SCM p_args)
{
  GossipCanvasItem* c_item;
  sgtk_cvec c_coords;
  int n_args;
  GtkArg *args;
  sgtk_object_info *info;

  SCM_ASSERT (sgtk_is_a_gtkobj (gossip_canvas_item_get_type (), p_item), p_item, SCM_ARG1, s_gossip_canvas_item_set);
  SCM_ASSERT (p_coords == SCM_BOOL_F || sgtk_valid_composite (p_coords, _sgtk_helper_valid_double),
	      p_coords, SCM_ARG2, s_gossip_canvas_item_set);
  n_args = scm_ilength (p_args);
  SCM_ASSERT (n_args >= 0 && (n_args%2) == 0, p_args,
	      SCM_ARG3, s_gossip_canvas_item_set);
  n_args = n_args/2;

  c_item = (GossipCanvasItem*)sgtk_get_gtkobj (p_item);
  info = sgtk_find_object_info_from_type (GTK_OBJECT_TYPE(c_item));
  SCM_ASSERT (info != NULL, p_item, SCM_ARG1, s_gossip_canvas_item_set);

  SCM_DEFER_INTS;
  args = sgtk_build_args (info, &n_args, p_args, p_item, s_gossip_canvas_item_set);
  if (p_coords != SCM_BOOL_F)
    {
      c_coords = sgtk_scm2cvec (p_coords, _sgtk_helper_fromscm_double, sizeof (double));
      gossip_canvas_item_setv (c_item, c_coords.count, (double*)c_coords.vec, n_args, args);
      sgtk_cvec_finish (&c_coords, p_coords, NULL, sizeof(double));
    }
  else
    gossip_canvas_item_setv (c_item, -1, NULL, n_args, args);
    
  SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

void
gossip_canvas_init_extra_glue ()
{
  scm_c_define_gsubr (s_gossip_canvas_item_set, 2, 0, 1,
		      sgtk_gossip_canvas_item_set);
  scm_c_define_gsubr (s_gossip_canvas_item_new, 3, 0, 1,
		      sgtk_gossip_canvas_item_new);
}
