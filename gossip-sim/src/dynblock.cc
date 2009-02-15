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

#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <gossip/sim.h>
#include "dynblock.h"
#include "dyncomp.h"
#include "dynlink.h"

scm_t_bits scm_tc16_dynblock;

static SCM
mark_dynblock (SCM obj)
{
  dynblock *db = SCM_DYNBLOCK(obj);
  if (db) 
    return db->name;
  return SCM_BOOL_F;
}

static size_t
free_dynblock (SCM obj)
{
  dynblock *db = SCM_DYNBLOCK(obj);
  if (db) 
    {
      dyn_unref (db->dynhandle);
      delete db;
    }
  return 0;
}


static int
print_dynblock (SCM exp, SCM port, scm_print_state *pstate)
{
  dynblock *db = SCM_DYNBLOCK(exp);
  scm_puts ("#<dynblock ", port);
  if (db)
    scm_display (db->name, port);
  else
    scm_puts ("?", port);
  scm_putc ('>', port);
  return 1;
}

SCM_DEFINE (sim_dynblockp, "dynblock?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a dynblock.")
#define FUNC_NAME s_sim_dynblockp
{
  return (SCM_DYNBLOCKP (obj)) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_PROC(s_dynblock_load, "dynblock-load", 1, 0, 0, sim_dynblock_load);

SCM_DEFINE (sim_dynblock_load, "dynblock-load", 1, 0, 0,
	    (SCM filename),
	    "Load a dynblock from @var{filename}.")
#define FUNC_NAME s_sim_dynblock_load
{
  SCM_VALIDATE_STRING (SCM_ARG1, filename);

  dynblock *db = new dynblock;
  db->dynhandle = dyn_link (SCM_STRING_CHARS (filename));
  if (db->dynhandle == NULL) {
    delete db;
    scm_misc_error (s_dynblock_load,
		    "~A",
		    scm_cons (scm_makfrom0str (dyn_error ()), SCM_EOL));

    /* NOTREACHED */
    abort ();
  }

  sim_block_dispatcher_t *dsp =
    (sim_block_dispatcher_t *) dyn_symbol (db->dynhandle,
					   "sim_plugin_block_dispatcher");
    
  if (dsp == NULL)
    {
      SCM err = scm_makfrom0str (dyn_error ());
      dyn_unref (db->dynhandle);
      scm_misc_error (s_dynblock_load,
		      "~A",
		      scm_cons (err, SCM_EOL));
      /* NOTREACHED */
      abort ();
    }

  db->name = filename;
  db->dispatch = dsp;
    
  SCM_RETURN_NEWSMOB (scm_tc16_dynblock, db);
}
#undef FUNC_NAME

SCM_DEFINE (sim_dynblock_prototype, "dynblock-prototype", 1, 0, 0,
	    (SCM db),
	    "Return the prototype of the dynblock @var{db} as a string.")
#define FUNC_NAME s_sim_dynblock_prototype
{
  SCM_VALIDATE_SMOB (SCM_ARG1, db, dynblock);
  dynblock *c_db = SCM_DYNBLOCK (db);
  
  SCM_DEFER_INTS;
  char *result = (char *) c_db->dispatch (simbo_prototype);
  SCM_ALLOW_INTS;
  return scm_makfrom0str (result);
}
#undef FUNC_NAME

SCM_DEFINE (sim_dynblock_instantiate, "dynblock-instantiate", 2, 0, 0,
	    (SCM db, SCM name),
	    "Instantiate the dynblock @var{db} into a unconnected dyncomp "
	    "and return that.  The dyncomp is given the name @var{name}.")
#define FUNC_NAME s_sim_dynblock_load
{
  SCM_VALIDATE_SMOB (SCM_ARG1, db, dynblock);
  SCM_VALIDATE_STRING (SCM_ARG2, name);

  SCM_DEFER_INTS;

  dynblock *c_db = SCM_DYNBLOCK (db);
  sim_comp *comp = (sim_comp *) c_db->dispatch (simbo_create);

  if (comp == NULL)
    {
      SCM_ALLOW_INTS;
      scm_misc_error (FUNC_NAME, "can't create component", SCM_EOL);
    }

  comp->check_errors = 0;
  comp->check ();
  if (comp->check_errors > 0)
    {
      delete comp;
      SCM_ALLOW_INTS;
      scm_misc_error (FUNC_NAME, "errors while creating component", SCM_EOL);
    }

  SCM_ALLOW_INTS;

  SCM dc = sim_make_dyncomp (db, comp, SCM_STRING_CHARS (name));
  scm_remember_upto_here_1 (name);
  return dc;
}

void
sim_init_dynblock ()
{
  scm_tc16_dynblock = scm_make_smob_type ("dynblock", 0);
  scm_set_smob_mark (scm_tc16_dynblock, mark_dynblock);
  scm_set_smob_free (scm_tc16_dynblock, free_dynblock);
  scm_set_smob_print (scm_tc16_dynblock, print_dynblock);

#include "dynblock.x"
}
