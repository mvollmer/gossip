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

#ifndef DYNCOMP_H
#define DYNCOMP_H

#include <gossip/sim.h>

extern "C" {
#include <libguile.h>
}

extern scm_t_bits scm_tc16_dyncomp;

struct dyncomp {
  SCM block;
  sim_comp *comp;
  void *dynhandle;
  SCM genvals, resvals;
};

#define SCM_DYNCOMPP(x)   SCM_SMOB_PREDICATE(scm_tc16_dyncomp, (x))
#define SCM_DYNCOMP(obj)  ((dyncomp *)SCM_SMOB_DATA(obj))

SCM sim_dyncompp (SCM obj);
SCM sim_make_dyncomp (SCM block, sim_comp *comp, char *name);
SCM sim_dyncomp_dynblock (SCM comp);
SCM sim_dyncomp_init (SCM comp, SCM genvals, SCM undef, SCM n_in, SCM n_out);
SCM sim_dyncomp_in_size (SCM comp, SCM i);
SCM sim_dyncomp_out_size (SCM comp, SCM i);
SCM sim_dyncomp_connect (SCM comp, SCM ins, SCM outs);
SCM sim_dyncomp_finished_p (SCM comp);
SCM sim_dyncomp_result (SCM comp, SCM name);

void sim_init_dyncomp ();

#endif
