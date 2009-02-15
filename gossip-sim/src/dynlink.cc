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

#define _GNU_SOURCE

#include <dlfcn.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <malloc.h>
#include <assert.h>

#include "dynlink.h"

struct dyn {
  int ref_count;
  void *handle;
};

/* When dlopening a shared object, we want to make sure that we really
   load it and wont get some previously loaded object.  We do this by
   creating a unique symlink in /tmp and dlopening that.  */

void *
dyn_link (char *filename)
{
  static int count = 1;
  char *name;

  // XXX - works only for absolute pathnames yet.
  assert (filename && filename[0] == '/');

  while (true)
    {
      asprintf (&name, "/tmp/gossip-unique-object-%d", count++);
      if (symlink (filename, name) == -1)
	{
	  if (errno == EEXIST)
	    {
	      free (name);
	      continue;
	    }
	  else
	    {
	      free (name);
	      return NULL;
	    }
	}
      void *handle = dlopen (name, RTLD_LAZY);
      unlink (name);
      free (name);
      if (handle)
	{
	  dyn *d = new dyn;
	  d->ref_count = 1;
	  d->handle = handle; 
	  return d;
	}
      else
	return NULL;
    }
}

void
dyn_ref (void *handle)
{
  dyn *d = (dyn *)handle;
  d->ref_count++;
}

void
dyn_unref (void *handle)
{
  dyn *d = (dyn *)handle;
  d->ref_count--;
  if (d->ref_count == 0)
    {
      dlclose (d->handle);
      delete d;
    }
}

void *
dyn_symbol (void *handle, const char *symbol)
{
  dyn *d = (dyn *)handle;
  return dlsym (d->handle, symbol);
}

char *
dyn_error ()
{
  return dlerror ();
}
