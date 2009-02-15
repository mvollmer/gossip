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

#include <signal.h>
#include <setjmp.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <libguile.h>
#include <guile/gh.h>

#include <gossip/sim.h>
#include "dynsim.h"
#include "dyncomp.h"
#include "dynblock.h"

scm_t_bits scm_tc16_dynsim;

static void
free_dynsched (dynsched *s)
{
  dynsched *s2;
  while (s) 
    {
      s2 = s->next;
      if (!s->is_comp)
	free_dynsched (s->sched);
      delete s;
      s = s2;
    }
}

static SCM
mark_dynsim (SCM obj)
{
  return SCM_DYNSIM(obj)->dyncomps;
}

static size_t
free_dynsim (SCM ptr)
{
  free_dynsched (SCM_DYNSIM(ptr)->sched);
  return 0;
}

static int
print_dynsim (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<dynsim ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_putc('>', port);
  return 1;
}

SCM_PROC(s_dynsimp, "dynsim?", 1, 0, 0, sim_dynsimp);

SCM_DEFINE (sim_dynsimp, "dynsim?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a dynsim.")
#define FUNC_NAME s_sim_dynsimp
{
  return SCM_BOOL (SCM_DYNSIMP (obj));
}
#undef FUNC_NAME

SCM_PROC(s_make_dynsim, "make-dynsim", 1, 0, 0, sim_make_dynsim);

/* The `schedule' data structure.

   schedule = (tick ...) ;
   tick = (count comp) ;
   count = fixnum ;
   comp = dyncomp
        | schedule ;
*/

static void invalid_schedule (SCM s);
static void validate_schedule (SCM s);
static dynsched *convert_schedule (SCM s);
static SCM schedule_comps (SCM s, SCM tail = SCM_EOL);

SCM_DEFINE (sim_make_dynsim, "make-dynsim", 1, 0, 0,
	    (SCM schedule),
	    "Make a dynsim from the template in @var{schedule}.")
#define FUNC_NAME s_sim_make_dynsim
{
  validate_schedule (schedule);
  dynsim *s = new dynsim;
  s->sched = convert_schedule (schedule);
  s->dyncomps = schedule_comps (schedule);
  SCM_RETURN_NEWSMOB (scm_tc16_dynsim, s);
}

static void
invalid_schedule (SCM s)
{
  scm_misc_error (FUNC_NAME,
		  "invalid schedule: ~S",
		  scm_cons (s, SCM_EOL));
}

static void
validate_schedule (SCM s)
{
  int n = scm_ilength (s);
  if (n < 0)
    invalid_schedule (s);
  for (int i = 0; i < n; i++) {
    SCM ss = SCM_CAR (s);
    if (scm_ilength (ss) != 2)
      invalid_schedule (ss);
    if (SCM_NINUMP (SCM_CAR (ss)))
      invalid_schedule (SCM_CAR (ss));
    if (sim_dyncompp (SCM_CADR (ss)) == SCM_BOOL_F)
      validate_schedule (SCM_CADR (ss));
    s = SCM_CDR (s);
  }
}

static dynsched *
convert_schedule (SCM s)
{
  dynsched *ds, **ds_tail = &ds;

  while (SCM_NIMP (s)) 
    {
      dynsched *d = new dynsched;
      SCM ss = SCM_CAR (s);
      d->count = SCM_INUM (SCM_CAR (ss));
      if (SCM_DYNCOMPP (SCM_CADR (ss)))
	{
	  d->is_comp = true;
	  d->comp = SCM_DYNCOMP (SCM_CADR (ss))->comp;
	}
      else 
	{
	  d->is_comp = false;
	  d->sched = convert_schedule (SCM_CADR (ss));
	}
      *ds_tail = d;
      ds_tail = &d->next;
      s = SCM_CDR (s);
    }
  *ds_tail = NULL;
  return ds;
}

static SCM
schedule_comps (SCM s, SCM tail)
{
  while (SCM_NIMP (s))
    {
      SCM ss = SCM_CAR (s);
      if (SCM_DYNCOMPP (SCM_CADR (ss)))
	tail = scm_cons (SCM_CADR (ss), tail);
      else
	tail = schedule_comps (SCM_CADR (ss), tail);
      s = SCM_CDR(s);
    }
  return tail;
}
#undef FUNC_NAME

SCM_PROC(s_dynsim_run, "dynsim-run", 2, 0, 0, sim_dynsim_run);

enum stop_code {
  no_code,
  completed,
  failed,
  interrupted,
  forcefully_interrupted
};

static SCM notify_proc;        // XXX - this is not explicitely protected.
static volatile sig_atomic_t sigint_pending;
static enum stop_code stop_code;
static jmp_buf simstop_return;

static void
stop_run (enum stop_code code = no_code)
{
  if (code != no_code)
    stop_code = code;
  longjmp (simstop_return, 1);
}

SCM_SYMBOL (sym_finished, "finished");
SCM_SYMBOL (sym_interrupted, "interrupted");

static SCM
inner_notify (void *data)
{
  SCM op = (SCM)data;
  return scm_apply (notify_proc, scm_cons (op, SCM_EOL), SCM_EOL);
}

static SCM
handle_by_message_noexit_and_return_false (void *handler_data,
					   SCM tag, SCM args)
{
  scm_handle_by_message_noexit (handler_data, tag, args);
  return SCM_BOOL_F;
}

static void
notify (SCM op)
{
  SCM_STACKITEM stack_item;

  if (notify_proc == SCM_BOOL_F
      || (scm_internal_catch (SCM_BOOL_T,
			      inner_notify, (void *)op,
			      handle_by_message_noexit_and_return_false,
			      (char *)"sim")
	  == SCM_BOOL_F))
    stop_run ();
}

static void
run_sched (dynsched *ds)
{
  while (ds)
    {
      if (ds->is_comp) 
	{
	  sim_comp *comp = ds->comp;
	  comp->tick (ds->count);
	  if (comp->finished_trigger)
	    {
	      comp->finished_trigger = false;
	      stop_code = completed;
	      notify (sym_finished);
	    }
	  if (sigint_pending > 0)
	    {
	      sigint_pending = 0;
	      stop_code = interrupted;
	      notify (sym_interrupted);
	    }
	}
      else 
	{
	  for (int i = 0; i < ds->count; i++) 
	    run_sched (ds->sched);
	}
      ds = ds->next;
    }
}

static void
sigint_handler (int n)
{
  write (2, "yo!\n", 4);
  switch (sigint_pending)
    {
    case 0:
      /* First SIGINT, just record it. */
      sigint_pending = 1;
      break;
    case 1:
      /* Second SIGINT.  Print Warning. */
      {
	static char msg[] = 
	  "sim: no response to interrupt request.  Send again to "
	  "forcefully interrupt simulation.\n";
	write (2, msg, sizeof (msg));
	sigint_pending = 2;
      }
      break;
    case 2:
      /* Third SIGINT.  Interrupt with longjump. */
      stop_run (forcefully_interrupted);
      break;
    }
}

SCM_DEFINE (sim_dynsim_run, "dynsim-run", 2, 0, 0,
	    (SCM ds, SCM ntfy),
	    "Run dynsim @var{ds}, calling @var{ntfy} in certain situations.")
#define FUNC_NAME s_sim_dynsim_run
{
  SCM_VALIDATE_SMOB (SCM_ARG1, ds, dynsim);
  SCM_ASSERT (ntfy == SCM_BOOL_F || gh_procedure_p (ntfy), ntfy,
	      SCM_ARG2, FUNC_NAME);

  SCM_DEFER_INTS;

  dynsim *c_ds = SCM_DYNSIM (ds);

  struct sigaction action, oldaction;

  sigint_pending = 0;
  notify_proc = ntfy;

  stop_code = completed;
  if (setjmp (simstop_return) == 0)
    {
      // Give them a chance to stop the simulation before it even
      // begins.
      //
      notify (sym_finished);

      sigemptyset(&action.sa_mask);
      action.sa_flags = SA_RESTART;
      action.sa_handler = sigint_handler;
      if (sigaction (SIGINT, &action, &oldaction) == -1)
	perror ("sigaction");
      while (true)
	run_sched (c_ds->sched);
    }
  else
    {
      static char *msg[] = {
	"Huh?",
	"simulation completed",
	"simulation failed",
	"simulation interrupted",
	"simulation forcefully interrupted"
      };
	
      /* SIGINT is still blocked. */

      fprintf (stderr, "gossip: %s.\n", msg[stop_code]);

      sigset_t set;
      sigemptyset (&set);
      sigaddset (&set, SIGINT);
      sigprocmask (SIG_UNBLOCK, &set, NULL);
    }

  for (SCM c = c_ds->dyncomps; SCM_NIMP(c); c = SCM_CDR(c))
    SCM_DYNCOMP(SCM_CAR(c))->comp->epilog ();

  sigaction (SIGINT, &oldaction, NULL);

  SCM_ALLOW_INTS;

  scm_remember_upto_here_1 (ds);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
sim_init_dynsim ()
{
  scm_tc16_dynsim = scm_make_smob_type ("dynsim", 0);
  scm_set_smob_mark (scm_tc16_dynsim, mark_dynsim);
  scm_set_smob_free (scm_tc16_dynsim, free_dynsim);
  scm_set_smob_print (scm_tc16_dynsim, print_dynsim);

#include "dynsim.x"
}
