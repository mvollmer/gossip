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

#include <gossip/sim.h>
#include <malloc.h>
#include <unistd.h>
#include <string.h>

struct dynstring {

  dynstring ();
  ~dynstring ();

  void add0 (const char *str);
  void add0_quoted (const char *str);
  
  char *get ();

private:
  char *str;
  int len, max;
};

dynstring::dynstring ()
{
  str = NULL;
  len = max = 0;
  add0 ("");
}

dynstring::~dynstring ()
{
  free (str);
}

void
dynstring::add0 (const char *s)
{
  int l = strlen (s);
  if (len+l >= max) 
    {
      max = (((len+l+1) + 63) & ~63);
      str = (char *) realloc (str, max);
      if (str == NULL) 
	{
	  write (2, "out of memory.\n", 15);
	  exit (2);
	}
    }
  strcpy (str+len, s);
  len += l;
}

static char escape_this[] = "\"\n\t";
static char escape_with[] = "\"nt";
 
void
dynstring::add0_quoted (const char *s)
{
  char buf[2*strlen(s)+3];
  char *cp = buf;

  *cp++ = '"';
  for (const char *cp2 = s; *cp2; cp2++) 
    {
      if (char *esc = strchr (escape_this, *cp2)) 
	{
	  *cp++ = '\\';
	  *cp++ = escape_with[esc-escape_this];
	} 
      else
	*cp++ = *cp2;
    }
  *cp++ = '"';
  *cp++ = '\0';
  add0 (buf);
}

char *
dynstring::get ()
{
  char *res = str;
  str = NULL;
  len = max = 0;
  return res;
}

void
add_port (dynstring &p, char *sign, sim_port *port)
{
  p.add0 ("(");
  p.add0 (sign);
  p.add0 (" ");
  p.add0 (port->name);
  p.add0 (" ");
  if (port->multi)
    {
      p.add0 ("(multi ");
      p.add0 (port->type);
      p.add0 (")");
    }
  else
    p.add0 (port->type);
  p.add0 (")");
}

char *
sim_block_prototype (sim_block_info *info)
{
  sim_generic *gens = info->generics;
  sim_port *ins = info->inputs;
  sim_port *outs = info->outputs;

  dynstring p;

  p.add0 ("(gossip-sim \"0.0\" (");
  for (int i = 0; gens && gens[i].name; i++) 
    {
      p.add0 ("(= ");
      p.add0 (gens[i].name);
      p.add0 (" #t)");
      if (gens[i+1].name || (ins && ins[0].name) || (outs && outs[0].name))
	p.add0 (" ");
    }
  for (int i = 0; ins && ins[i].name; i++) 
    {
      add_port (p, "<", ins+i);
      if (ins[i+1].name || (outs && outs[0].name))
	p.add0 (" ");
    }
  for (int i = 0; outs && outs[i].name; i++) 
    {
      add_port (p, ">", outs+i);
      if (outs[i+1].name)
	p.add0 (" ");
    }
  p.add0 ("))");
  return p.get ();
}
