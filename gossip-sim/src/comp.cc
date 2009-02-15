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

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

#include <gossip/sim.h>

#include <libguile.h>

sim_comp::sim_comp ()
{
  block = NULL;

  n_in = n_out = 0;
  in_connects = NULL;
  out_connects = NULL;
  finished = finished_trigger = false;

  gvec = SCM_BOOL_F;
  rvec = SCM_BOOL_F;

  deletables = NULL;
}

sim_comp::~sim_comp ()
{
  for (int j = 0; j < n_out; j++)
    delete out_connects[j].buf;
  delete in_connects;
  delete out_connects;

  if (name)
    free (name);

  deleteme *d2;
  for (deleteme *d = deletables; d; d = d2)
    {
      d2 = d->next;
      delete d->ptr;
      delete d;
    }
}

void
sim_comp::register_for_delete (void *ptr)
{
  deleteme *d = new deleteme;
  d->ptr = ptr;
  d->next = deletables;
  deletables = d;
}

void
sim_comp::set_block_info (sim_block_info *info)
{
  assert (info->version == SIM_BLOCK_INFO_VERSION);

  if (info->block)
    {
      block = info->block;
      return;
    }

  int n;

  block = new sim_block;
  block->name = info->name;

  block->generics = info->generics;
  for (n = 0; block->generics && block->generics[n].name; n++)
    ;
  block->n_generics = n;

  block->results = info->results;
  for (n = 0; block->results && block->results[n].name; n++)
    ;
  block->n_results = n;

  block->inputs = info->inputs;
  for (n = 0; block->inputs && block->inputs[n].name; n++)
    ;
  block->n_inputs = n;

  block->outputs = info->outputs;
  for (n = 0; block->outputs && block->outputs[n].name; n++)
    ;
  block->n_outputs = n;
}

void
sim_comp::check ()
{
  // Only the last port can be a multi port.
  for (int i = 0; i < block->n_inputs-1; i++)
    if (block->inputs[i].multi)
      check_error ("input `%s' is a multi port, but not the last port",
		   block->inputs[i].name);
  for (int i = 0; i < block->n_outputs-1; i++)
    if (block->outputs[i].multi)       // XXX - should do for now.
      check_error ("output `%s' is a multi port, but not the last port",
		   block->outputs[i].name);
}

void
sim_comp::check_error (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  check_verror (fmt, ap);
  va_end (ap);
}

void
sim_comp::check_verror (const char *fmt, va_list ap)
{
  fprintf (stderr, "%s: ", name);
  vfprintf (stderr, fmt, ap);
  fprintf (stderr, "\n");
  check_errors++;
}

void
sim_comp::check_homogenous_types (char *in_type, char *out_type)
{
  for (int i = 0; i < block->n_inputs; i++)
    if (strcmp (block->inputs[i].type, in_type))
      check_error ("input `%s' must be of type `%s'\n",
		   block->inputs[i].name, in_type);
  for (int i = 0; i < block->n_outputs; i++)
    if (strcmp (block->outputs[i].type, out_type))
      check_error ("output `%s' must be of type `%s'\n",
		   block->outputs[i].name, out_type);
}

void
sim_comp::set_name (char *nm)
{
  name = strdup (nm);
}

void
sim_comp::set_gvals (SCM vec)
{
  gvec = vec;
}

void
sim_comp::set_rvec (SCM vec)
{
  rvec = vec;
}

void
sim_comp::mark_scms ()
{
  scm_gc_mark (gvec);
  scm_gc_mark (rvec);
}

void
sim_comp::init ()
{
}

void
sim_comp::set_n_in (int n)
{
  if (block->n_inputs > 0 && block->inputs[block->n_inputs-1].multi)
    assert (n >= block->n_inputs-1);
  else
    assert (n == block->n_inputs);

  n_in = n;
  in_connects = new connection[n];

  int i = 0;
  while (i < block->n_inputs && i < n)
    {
      in_connects[i].type_size = block->inputs[i].type_size;
      in_connects[i].size = 
	block->inputs[i].initial_chunk*in_connects[i].type_size;
      i++;
    }
  for (int j = i; j < n; j++)
    {
      in_connects[j].type_size = in_connects[i-1].type_size;
      in_connects[j].size = in_connects[i-1].size;
    }

  in_data = new (sim_data*)[n];
}

void
sim_comp::set_n_out (int n)
{
  if (block->n_outputs > 0 && block->outputs[block->n_outputs-1].multi)
    assert (n >= block->n_outputs-1);
  else
    assert (n == block->n_outputs);
    
  n_out = n;
  out_connects = new connection[n];

  int i = 0;
  while (i < block->n_outputs && i < n)
    {
      out_connects[i].type_size = block->outputs[i].type_size;
      out_connects[i].size =
	block->outputs[i].initial_chunk*out_connects[i].type_size;
      i++;
    }
  for (int j = i; j < n; j++)
    {
      out_connects[j].type_size = out_connects[i-1].type_size;
      out_connects[j].size = out_connects[i-1].size;
    }

  out_data = new (sim_data*)[n];
}

void
sim_comp::set_in_chunk (int id, size_t chunk)
{
  assert (id >= 0 && id < block->n_inputs);
  if (id == block->n_inputs-1)
    for (int i = id; i < n_in; i++)
      in_connects[i].size = chunk*in_connects[i].type_size;
  else
    in_connects[id].size = chunk*in_connects[id].type_size;
}

void
sim_comp::set_out_chunk (int id, size_t chunk)
{
  assert (id >= 0 && id < block->n_outputs);
  if (id == block->n_outputs-1)
    for (int i = id; i < n_out; i++)
      out_connects[i].size = chunk*out_connects[i].type_size;
  else
    out_connects[id].size = chunk*out_connects[id].type_size;
}

void
sim_comp::error (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  verror (fmt, ap);
  va_end (ap);
}

void
sim_comp::verror (const char *fmt, va_list ap)
{
  // XXX - include the message in the misc error
  fprintf (stderr, "%s: ", name);  
  vfprintf (stderr, fmt, ap);
  fprintf (stderr, "\n");
  scm_misc_error ("init", "error during initialization", SCM_EOL);
}

void
sim_comp::connection::make_writer (size_t bufsize)
{
  assert (bufsize % size == 0);

  if (buf)
    {
      assert (buf_end - buf == bufsize);
    }
  else
    {
      buf = new sim_data[bufsize];
      buf_end = buf + bufsize;
    }
  ptr = buf;
}

void
sim_comp::connection::make_reader (sim_comp *src, int src_id,
				   size_t bufsize, size_t delay)
{
  assert (bufsize % size == 0);
  assert (delay % size == 0);

  if (src == NULL)
    {
      // open input port
      buf = new sim_data[bufsize];
      buf_end = buf + bufsize;
      ptr = buf;
      for (int i = 0; i < bufsize; i++)
	ptr[i] = 0;  // XXX - provide more control over the initial value
    }
  else
    {
      connection *out = src->out_connects+src_id;
      if (out->buf == NULL)
	{
	  out->buf = new sim_data[bufsize];
	  out->buf_end = out->buf + bufsize;
	}
      else
	assert (out->buf_end - out->buf == bufsize);
      buf = out->buf;
      buf_end = out->buf_end;
      if (delay > 0)
	{
	  ptr = buf_end - delay;
	  for (int i = 0; i < delay; i++)
	    ptr[i] = 0;  // XXX - provide more control over the initial value
	}
      else
	ptr = buf;
    }
}

void
sim_comp::step (int count, const sim_data **in, sim_data **out)
{
  for (int i = 1; i < count; i++)
    {
      step (in, out);
      for (int j = 0; j < n_in; j++)
	in[j] += in_connects[j].size;
      for (int j = 0; j < n_out; j++)
	out[j] += out_connects[j].size;
    }
  step (in, out);
}

void
sim_comp::step_not_redefined ()
{
  scm_misc_error (name,
		  "step function not redefined in ~A",
		  scm_list_1 (scm_makfrom0str (block->name)));
}

void
sim_comp::step (const sim_data **in, sim_data **out)
{
  step_not_redefined ();
}

void
sim_comp::tick (int count)
{
  for (int i = 0; i < n_in; i++)
    {
      in_data[i] = in_connects[i].ptr;
      in_connects[i].advance_ptr (count);
    }
  for (int i = 0; i < n_out; i++)
    {
      out_data[i] = out_connects[i].ptr;
      out_connects[i].advance_ptr (count);
    }
  step (count, (const sim_data **)in_data, out_data);
}

void
sim_comp::finish ()
{
  if (!finished)
    {
      printf ("%s: finished\n", name);
      finished_trigger = true;
      finished = true;
    }
}

void
sim_comp::epilog ()
{
}

void
sim_complex_comp::check ()
{
  sim_comp::check ();
  check_homogenous_types ("sim_complex", "sim_complex");
}

void
sim_complex_comp::step (int count, const sim_data **in, sim_data **out)
{
  step (count, (const sim_complex **)in, (sim_complex **)out);
}

void
sim_complex_comp::step (const sim_data **in, sim_data **out)
{
  step ((const sim_complex **)in, (sim_complex **)out);
}

void
sim_complex_comp::step (int count, const sim_complex **in, sim_complex **out)
{
  for (int i = 1; i < count; i++)
    {
      step (in, out);
      for (int j = 0; j < n_in; j++)
	in[j] += in_connects[j].size/sizeof(sim_complex);
      for (int j = 0; j < n_out; j++)
	out[j] += out_connects[j].size/sizeof(sim_complex);
    }
  step (in, out);
}

void
sim_complex_comp::step (const sim_complex **in, sim_complex **out)
{
  step_not_redefined ();
}

void
sim_complex_int_comp::check ()
{
  sim_comp::check ();
  check_homogenous_types ("sim_complex", "sim_int");
}

void
sim_complex_int_comp::step (int count, const sim_data **in, sim_data **out)
{
  step (count, (const sim_complex **)in, (sim_int **)out);
}

void
sim_complex_int_comp::step (const sim_data **in, sim_data **out)
{
  step ((const sim_complex **)in, (sim_int **)out);
}

void
sim_complex_int_comp::step (int count, const sim_complex **in, sim_int **out)
{
  for (int i = 1; i < count; i++)
    {
      step (in, out);
      for (int j = 0; j < n_in; j++)
	in[j] += in_connects[j].size/sizeof(sim_complex);
      for (int j = 0; j < n_out; j++)
	out[j] += out_connects[j].size/sizeof(sim_int);
    }
  step (in, out);
}

void
sim_complex_int_comp::step (const sim_complex **in, sim_int **out)
{
  step_not_redefined ();
}

void
sim_int_complex_comp::check ()
{
  sim_comp::check ();
  check_homogenous_types ("sim_int", "sim_complex");
}

void
sim_int_complex_comp::step (int count, const sim_data **in, sim_data **out)
{
  step (count, (const sim_int **)in, (sim_complex **)out);
}

void
sim_int_complex_comp::step (const sim_data **in, sim_data **out)
{
  step ((const sim_int **)in, (sim_complex **)out);
}

void
sim_int_complex_comp::step (int count, const sim_int **in, sim_complex **out)
{
  for (int i = 1; i < count; i++)
    {
      step (in, out);
      for (int j = 0; j < n_in; j++)
	in[j] += in_connects[j].size/sizeof(sim_int);
      for (int j = 0; j < n_out; j++)
	out[j] += out_connects[j].size/sizeof(sim_complex);
    }
  step (in, out);
}

void
sim_int_complex_comp::step (const sim_int **in, sim_complex **out)
{
  step_not_redefined ();
}

void
sim_int_comp::check ()
{
  sim_comp::check ();
  check_homogenous_types ("sim_int", "sim_int");
}

void
sim_int_comp::step (int count, const sim_data **in, sim_data **out)
{
  step (count, (const sim_int **)in, (sim_int **)out);
}

void
sim_int_comp::step (const sim_data **in, sim_data **out)
{
  step ((const sim_int **)in, (sim_int **)out);
}

void
sim_int_comp::step (int count, const sim_int **in, sim_int **out)
{
  for (int i = 1; i < count; i++)
    {
      step (in, out);
      for (int j = 0; j < n_in; j++)
	in[j] += in_connects[j].size/sizeof(sim_int);
      for (int j = 0; j < n_out; j++)
	out[j] += out_connects[j].size/sizeof(sim_int);
    }
  step (in, out);
}

void
sim_int_comp::step (const sim_int **in, sim_int **out)
{
  step_not_redefined ();
}
