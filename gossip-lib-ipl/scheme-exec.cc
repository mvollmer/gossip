/* scheme-exec - evaluate a Scheme expression

   Copyright (C) 2002  Marius Vollmer

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

/* This block can compute a new output value for each input value by
   applying a Scheme function to the value.
*/

sim_generic generics[] = {
  SIM_GENERIC ("func"),
  NULL
};

sim_port inputs[] = {
  SIM_MULTI_PORT ("in", sim_complex, 1),
  NULL
};

sim_port outputs[] = {
  SIM_PORT ("out", sim_complex, 1),
  NULL
};

static sim_complex execute (struct scheme_exec *comp);

struct scheme_exec : sim_complex_comp {

  bool error;
  SCM func;

  sim_complex *args, ret_val;
  int n_args;

  void
  init ()
  {
    error = false;
    get (func, "func");
    
    n_args = get_n_in ();
    args = new sim_complex[n_args];
  }

  ~scheme_exec ()
  {
    delete args;
  }

  void
  step (const sim_complex **in, sim_complex **out)
  {
    for (int i = 0; i < n_args; i++)
      args[i] = in[i][0];
    out[0][0] = execute (this);
  }

};

static SCM
apply_func (void *data)
{
  SCM a = SCM_EOL, r;
  SCM *a_tail = &a;

  scheme_exec *se = (scheme_exec *)data;

  for (int i = 0; i < se->n_args; i++)
    {
      *a_tail = scm_cons (scm_make_complex (real(se->args[i]),
					    imag(se->args[i])),
			  SCM_EOL);
      a_tail = SCM_CDRLOC (*a_tail);
    }

  r = scm_apply_0 (se->func, a);
  se->ret_val = sim_complex (scm_num2dbl (scm_real_part (r), "scheme-exec"),
			     scm_num2dbl (scm_imag_part (r), "scheme-exec"));
  return SCM_BOOL_T;
}

static sim_complex
execute (scheme_exec *se)
{
  if (se->error)
    return 0.0;

  if (SCM_FALSEP (scm_internal_catch (SCM_BOOL_T, apply_func, se,
				      scm_handle_by_message_noexit,
				      (char *)"scheme-exec")))
    {
      se->error = true;
      se->finish ();
      return 0.0;
    }
  else
    return se->ret_val;
}
  
SIM_DECLARE_BLOCK (scheme_exec, generics, NULL, inputs, outputs);
