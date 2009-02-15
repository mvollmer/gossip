/*  gossip-sim - synchronous data flow simulations

    Copyright (C) 2000, 2001, 2002, 2002, 2003  Marius Vollmer

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
#include <string.h>
#include <libguile.h>
#include <guile/gh.h>

int
sim_comp::genindex (const char *name)
{
  for (int i = 0; i < block->n_generics; i++)
    if (!strcmp (name, block->generics[i].name))
      return i;
  scm_misc_error ("get",
		  "no such generic: ~S",
		  scm_list_1 (scm_makfrom0str (name)));
}

SCM
sim_comp::get_gval (int index)
{
  if (gvec != SCM_BOOL_F && index >= 0 && index < SCM_VECTOR_LENGTH(gvec))
    return SCM_VELTS(gvec)[index];
  else
    return SCM_UNDEFINED;
}

static void
unset_generic_error (const char *comp, const char *name)
{
  scm_misc_error (comp,
		  "no value for generic ~S",
		  scm_list_1 (scm_makfrom0str (name)));
}

bool
sim_comp::get (bool &var, const char *name, bool def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = SCM_NFALSEP (x);
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (bool &var, const char *name)
{
  if (!get (var, name, false))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (int &var, const char *name, int def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = gh_scm2int (x);
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (int &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (unsigned int &var, const char *name, unsigned int def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      unsigned long ul;
      ul = gh_scm2ulong (x);
      var = ul;
      if ((unsigned long)var != ul)
	scm_out_of_range ("get-unsigned-long", x);
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (unsigned int &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (long &var, const char *name, long def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = gh_scm2long (x);
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (long &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (unsigned long &var, const char *name, unsigned long def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = gh_scm2ulong (x);
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (unsigned long &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (long long &var, const char *name, long long def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = scm_num2long_long (x, SCM_ARG1, "get");
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (long long &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (unsigned long long &var, const char *name,
	       unsigned long long def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = scm_num2ulong_long (x, SCM_ARG1, "get");
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (unsigned long long &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (double &var, const char *name, double def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = gh_scm2double (x);
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (double &var, const char *name)
{
  if (!get (var, name, 0.0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (sim_complex &var, const char *name, sim_complex def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = sim_complex (scm_num2dbl (scm_real_part (x), "get"),
			 scm_num2dbl (scm_imag_part (x), "get"));
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (sim_complex &var, const char *name)
{
  if (!get (var, name, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (int *&var, int &len, const char *name,
	       int *def, int def_len)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      if (SCM_VECTORP(x))
	{
	  len = SCM_VECTOR_LENGTH(x);
	  var = new int[len];
	  register_for_delete (var);
	  for (int i = 0; i < len; i++)
	    {
	      SCM y = SCM_VELTS(x)[i];
	      var[i] = gh_scm2int (y);
	    }
	}
      else
	scm_misc_error ("get",
			"generic value is not a vector: ~S",
			scm_list_1 (scm_makfrom0str (name)));
      return true;
    }
  var = def;
  len = def_len;
  return false;
}

void
sim_comp::get (int *&var, int &len, const char *name)
{
  if (!get (var, len, name, NULL, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (double *&var, int &len, const char *name,
	       double *def, int def_len)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      if (SCM_VECTORP(x))
	{
	  len = SCM_VECTOR_LENGTH(x);
	  var = new double[len];
	  register_for_delete (var);
	  for (int i = 0; i < len; i++)
	    {
	      SCM y = SCM_VELTS(x)[i];
	      var[i] = scm_num2dbl (y, "get");
	    }
	}
      else
	scm_misc_error ("get",
			"generic value is not a vector: ~S",
			scm_list_1 (scm_makfrom0str (name)));
      return true;
    }
  var = def;
  len = def_len;
  return false;
}

void
sim_comp::get (double *&var, int &len, const char *name)
{
  if (!get (var, len, name, NULL, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (sim_complex *&var, int &len, const char *name,
	       sim_complex *def, int def_len)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      if (SCM_VECTORP(x))
	{
	  len = SCM_VECTOR_LENGTH(x);
	  var = new sim_complex[len];
	  register_for_delete (var);
	  for (int i = 0; i < len; i++)
	    {
	      SCM y = SCM_VELTS(x)[i];
	    var[i] = sim_complex (scm_num2dbl (scm_real_part (y), "get"),
				  scm_num2dbl (scm_imag_part (y), "get"));
	    }
	}
      else
	scm_misc_error ("get",
			"generic value is not a vector: ~S",
			scm_list_1 (scm_makfrom0str (name)));
      return true;
    }
  var = def;
  len = def_len;
  return false;
}

void
sim_comp::get (sim_complex *&var, int &len, const char *name)
{
  if (!get (var, len, name, NULL, 0))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (const char *&var, const char *name, const char *def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = gh_scm2newstr (x, NULL);
      // XXX - register memory for freeing.
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (const char *&var, const char *name)
{
  if (!get (var, name, ""))
    unset_generic_error (get_name (), name);
}

bool
sim_comp::get (SCM &var, const char *name, SCM def)
{
  int index = genindex (name);
  SCM x = get_gval (index);
  if (x != SCM_UNDEFINED)
    {
      var = x;
      return true;
    }
  var = def;
  return false;
}

void
sim_comp::get (SCM &var, const char *name)
{
  if (!get (var, name, SCM_BOOL_F))
    unset_generic_error (get_name (), name);
}
  
bool
sim_comp::get_rval (const char *name, SCM &var)
{
  if (rvec == SCM_BOOL_F)
    return false;
  for (int i = 0; i < block->n_results && i < SCM_VECTOR_LENGTH(rvec); i++)
    if (!strcmp (name, block->results[i].name))
      {
	var = SCM_VELTS(rvec)[i];
	return true;
      }
  return false;
}

#ifndef SCM_VECTOR_SET
#define SCM_VECTOR_SET(obj,i,v) (SCM_VELTS(obj)[i]=(v))
#endif

void
sim_comp::set_rval (const char *name, SCM val)
{
  for (int i = 0; i < block->n_results && i < SCM_VECTOR_LENGTH(rvec); i++)
    if (!strcmp (name, block->results[i].name))
      {
	SCM_VECTOR_SET (rvec, i, val);
	return;
      }
  scm_misc_error ("set",
		  "no such result: ~S",
		  scm_list_1 (scm_makfrom0str (name)));
}

void
sim_comp::set (const char *name, int val)
{
  set_rval (name, gh_int2scm (val));
}

void
sim_comp::set (const char *name, double val)
{
  set_rval (name, gh_double2scm (val));
}

void
sim_comp::set (const char *name, sim_complex val)
{
  set_rval (name, scm_make_complex (real(val), imag(val)));
}
