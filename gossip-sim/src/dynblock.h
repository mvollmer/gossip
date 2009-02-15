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

#ifndef DYNBLOCK_H
#define DYNBLOCK_H

#include <gossip/sim.h>

extern "C" {
#include <libguile.h>
}

extern scm_t_bits scm_tc16_dynblock;

struct dynblock {
    SCM name;
    void *dynhandle;
    sim_block_dispatcher_t *dispatch;
};

#define SCM_DYNBLOCKP(x)          (SCM_SMOB_PREDICATE(scm_tc16_dynblock, (x)))
#define SCM_DYNBLOCK(obj)         ((dynblock *)SCM_SMOB_DATA(obj))

SCM sim_dynblockp (SCM obj);
SCM sim_dynblock_load (SCM filename);
SCM sim_dynblock_prototype (SCM block);
SCM sim_dynblock_instantiate (SCM block, SCM name);

void sim_init_dynblock ();

#endif
