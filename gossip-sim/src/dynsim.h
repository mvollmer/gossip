/*  gossip-sim - synchronous data flow simulations

    Copyright (C) 2000, 2001, 2002  Marius Vollmer

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

#ifndef DYNSIM_H
#define DYNSIM_H

#include <gossip/sim.h>
#include "dyncomp.h"

extern "C" {
#include <libguile.h>
}

extern scm_t_bits scm_tc16_dynsim;

struct dynsched {
  dynsched *next;

  int count;
  bool is_comp;
  union {
    sim_comp *comp;
    dynsched *sched;
  };
};

struct dynsim {
  dynsched *sched;
  SCM dyncomps;
};

#define SCM_DYNSIMP(x)          SCM_SMOB_PREDICATE(scm_tc16_dynsim, (x))
#define SCM_DYNSIM(obj)         ((dynsim *)SCM_SMOB_DATA(obj))

SCM sim_dynsimp (SCM obj);
SCM sim_make_dynsim (SCM schedule);
SCM sim_dynsim_run (SCM sim, SCM notify);

void sim_init_dynsim ();

#endif
