/*  gossip-sim - synchronous data flow simulations

    Copyright (C) 2000, 2001, 2002, 2003  Marius Vollmer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <assert.h>

#include <gossip/sim.h>
#include "dyncomp.h"
#include "dynblock.h"
#include "dynlink.h"

scm_t_bits scm_tc16_dyncomp;

static SCM
mark_dyncomp (SCM ptr)
{
  dyncomp *dc = SCM_DYNCOMP (ptr);
  dc->comp->mark_scms ();
  return dc->block;
}

static size_t
free_dyncomp (SCM ptr)
{
  dyncomp *dc = SCM_DYNCOMP(ptr);
  if (dc) 
    {
      delete dc->comp;
      dyn_unref (dc->dynhandle);
      delete dc;
    }
  return 0;
}

static int
print_dyncomp (SCM exp, SCM port, scm_print_state *pstate)
{
  dyncomp *dc = SCM_DYNCOMP (exp);
  scm_puts ("#<dyncomp ", port);
  scm_puts (dc->comp->name, port);
  scm_putc('>', port);
  return 1;
}

SCM_DEFINE (sim_dyncompp, "dyncomp?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a dyncomp.")
#define FUNC_NAME s_sim_dyncompp
{
  return SCM_BOOL (SCM_DYNCOMPP (obj));
}
#undef FUNC_NAME

SCM 
sim_make_dyncomp (SCM block, sim_comp *comp, char *name)
{
  dyncomp *dc = new dyncomp;
  dc->block = block;
  dc->dynhandle = SCM_DYNBLOCK(block)->dynhandle;
  dyn_ref (dc->dynhandle);
  dc->comp = comp;
  comp->set_name (name);

  SCM_RETURN_NEWSMOB (scm_tc16_dyncomp, dc);
}

#ifndef SCM_VECTOR_SET
#define SCM_VECTOR_SET(obj,i,v) (SCM_VELTS(obj)[i]=(v))
#endif

SCM_DEFINE (sim_dyncomp_init, "dyncomp-init", 5, 0, 0,
	    (SCM dc, SCM gvals, SCM undefined,
	     SCM n_in, SCM n_out),
	    "Initialize a freshly created dyncomp.")
#define FUNC_NAME s_sim_dyncomp_init
{
  int n_gvals, c_n_in, c_n_out;
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_LONG_COPY (SCM_ARG4, n_in, c_n_in);
  SCM_VALIDATE_LONG_COPY (SCM_ARG5, n_out, c_n_out);

  n_gvals = scm_ilength (gvals);
  SCM_ASSERT (n_gvals >= 0 || SCM_VECTORP(gvals), gvals, SCM_ARG2, FUNC_NAME);
  if (n_gvals >= 0)
    gvals = scm_vector (gvals);
  n_gvals = SCM_VECTOR_LENGTH (gvals);

  for (int i = 0; i < n_gvals; i++)
    if (SCM_VELTS(gvals)[i] == undefined)
      SCM_VECTOR_SET (gvals,i, SCM_UNDEFINED);  // XXX - should not expose
                                                // SCM_UNDEFINED to Scheme

  dyncomp *c_dc = SCM_DYNCOMP (dc);
  SCM rvec = scm_make_vector (SCM_MAKINUM (c_dc->comp->block->n_results),
			      SCM_UNDEFINED);
  c_dc->comp->set_gvals (gvals);
  c_dc->comp->set_rvec (rvec);
  c_dc->comp->set_n_in (c_n_in);
  c_dc->comp->set_n_out (c_n_out);
  c_dc->comp->init ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* ins: ((src-dyncomp src-id bufsize delay) ...)
   outs: ((bufsize) ...)
*/

SCM_DEFINE (sim_dyncomp_connect, "dyncomp-connect", 3, 0, 0,
	    (SCM dc, SCM ins, SCM outs),
	    "Connect the dyncomp @var{dc} according to @var{ins} and "
	    "@var{outs}.")
#define FUNC_NAME s_sim_dyncomp_connect
{
  int n_in, n_out;

  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG2, ins, n_in);
  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG3, outs, n_out);

  sim_comp *comp = SCM_DYNCOMP (dc)->comp;

  if (n_in != comp->n_in || n_out != comp->n_out)
    scm_misc_error (FUNC_NAME, "wrong number of connections", SCM_EOL);

  for (int i = 0; i < n_out; i++)
    {
      SCM c = SCM_CAR(outs);
      SCM_ASSERT (scm_ilength(c) == 1, c, SCM_ARG3, FUNC_NAME);
      int bufsize = scm_num2long (SCM_CAR(c), SCM_ARG3, FUNC_NAME);
      comp->out_connects[i].make_writer (bufsize);
      outs = SCM_CDR(outs);
    }
  
  for (int i = 0; i < n_in; i++)
    {
      SCM c = SCM_CAR(ins);
      SCM_ASSERT (scm_ilength(c) == 4, c, SCM_ARG2, FUNC_NAME);
      SCM_ASSERT (SCM_CAR(c) == SCM_BOOL_F || SCM_DYNCOMPP (SCM_CAR(c)),
		  SCM_CAR(c), SCM_ARG2, FUNC_NAME);
      sim_comp *src = ((SCM_CAR(c) == SCM_BOOL_F)?
		       NULL : SCM_DYNCOMP(SCM_CAR(c))->comp);
      int src_id = scm_num2long (SCM_CADR(c), SCM_ARG2, FUNC_NAME);
      int bufsize = scm_num2long (SCM_CADDR(c), SCM_ARG2, FUNC_NAME);
      int delay = scm_num2long (SCM_CADDDR(c), SCM_ARG2, FUNC_NAME);
      comp->in_connects[i].make_reader (src, src_id, bufsize, delay);
      ins = SCM_CDR(ins);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (sim_dyncomp_dynblock, "dyncomp-dynblock", 1, 0, 0,
	    (SCM dc),
	    "Return the dynblock that @var{dc} is a instance of.")
#define FUNC_NAME s_sim_dyncomp_dynblock
{
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  return SCM_DYNCOMP(dc)->block;
}
#undef FUNC_NAME

SCM_DEFINE (sim_dyncomp_in_size, "dyncomp-in-size", 2, 0, 0,
	    (SCM dc, SCM i),
	    "Return the input size of port @var{i} of @var{dc}.")
#define FUNC_NAME s_sim_dyncomp_in_size
{
  long c_i;
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_LONG_COPY (SCM_ARG2, i, c_i);

  dyncomp *c_dc = SCM_DYNCOMP (dc);
  if (c_i < 0 || c_i >= c_dc->comp->n_in)
    scm_out_of_range (FUNC_NAME, i);

  return SCM_MAKINUM (c_dc->comp->in_connects[c_i].size);
}
#undef FUNC_NAME

SCM_DEFINE (sim_dyncomp_out_size, "dyncomp-out-size", 2, 0, 0,
	    (SCM dc, SCM i),
	    "Return the output size of port @var{i} of @var{dc}.")
#define FUNC_NAME s_sim_dyncomp_out_size
{
  long c_i;
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_LONG_COPY (SCM_ARG2, i, c_i);

  dyncomp *c_dc = SCM_DYNCOMP (dc);
  c_i = SCM_INUM (i);

  if (c_i < 0 || c_i >= c_dc->comp->n_out)
    scm_out_of_range (FUNC_NAME, i);
  
  return SCM_MAKINUM (c_dc->comp->out_connects[c_i].size);
}
#undef FUNC_NAME

SCM_DEFINE (sim_dyncomp_in_type_size, "dyncomp-in-type-size", 2, 0, 0,
	    (SCM dc, SCM i),
	    "Return the input type size of port @var{i} of @var{dc}.")
#define FUNC_NAME s_sim_dyncomp_in_type_size
{
  long c_i;
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_LONG_COPY (SCM_ARG2, i, c_i);

  dyncomp *c_dc = SCM_DYNCOMP (dc);
  c_i = SCM_INUM (i);
  
  if (c_i < 0 || c_i >= c_dc->comp->n_in)
    scm_out_of_range (FUNC_NAME, i);

  return SCM_MAKINUM (c_dc->comp->in_connects[c_i].type_size);
}
#undef FUNC_NAME

SCM_PROC(s_dyncomp_out_type_size, "dyncomp-out-type-size", 2, 0, 0, sim_dyncomp_out_type_size);

SCM_DEFINE (sim_dyncomp_out_type_size, "dyncomp-out-type-size", 2, 0, 0,
	    (SCM dc, SCM i),
	    "Return the output type size of port @var{i} of @var{dc}.")
#define FUNC_NAME s_sim_dyncomp_out_type_size
{
  long c_i;
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_LONG_COPY (SCM_ARG2, i, c_i);

  dyncomp *c_dc = SCM_DYNCOMP (dc);
  c_i = SCM_INUM (i);

  if (c_i < 0 || c_i >= c_dc->comp->n_out)
    scm_out_of_range (FUNC_NAME, i);
  
  return SCM_MAKINUM (c_dc->comp->out_connects[c_i].type_size);
}
#undef FUNC_NAME

SCM_DEFINE (sim_dyncomp_finished_p, "dyncomp-finished?", 1, 0, 0,
	    (SCM dc),
	    "Return @code{#t} iff @var{dc} has raised its finished flags.")
#define FUNC_NAME s_sim_dyncomp_finished_p
{
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  dyncomp *c_dc = SCM_DYNCOMP (dc);
  return c_dc->comp->finished? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (sim_dyncomp_result, "dyncomp-result", 2, 0, 0,
	    (SCM dc, SCM name),
	    "Return the value of the result of @var{dc} with name @var{name}.")
#define FUNC_NAME s_sim_dyncomp_result
{
  SCM_VALIDATE_SMOB (SCM_ARG1, dc, dyncomp);
  SCM_VALIDATE_STRING (SCM_ARG2, name);

  dyncomp *c_dc = SCM_DYNCOMP (dc);
  SCM res;

  if (!c_dc->comp->get_rval (SCM_STRING_CHARS (name), res))
    scm_misc_error (FUNC_NAME,
		    "no such result: ~S",
		    scm_cons (name, SCM_EOL));

  return res;
}
#undef FUNC_NAME

void
sim_init_dyncomp ()
{
  scm_tc16_dyncomp = scm_make_smob_type ("dyncomp", 0);
  scm_set_smob_mark (scm_tc16_dyncomp, mark_dyncomp);
  scm_set_smob_free (scm_tc16_dyncomp, free_dyncomp);
  scm_set_smob_print (scm_tc16_dyncomp, print_dyncomp);

#include "dyncomp.x"
}
