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

#ifndef DYNLINK_H
#define DYNLINK_H

// A dynlinking abstraction that is more suited to our needs.

// This does not use a cache.  It always opens a new shared object so
// that we can have multiple versions of the `same' shared object in
// memory.  This is important for interactive use of the simulator.
//
void *dyn_link (char *filename); 
void dyn_ref (void *handle);
void dyn_unref (void *handle);

void *dyn_symbol (void *handle, const char *symbol);

char *dyn_error ();

#endif
