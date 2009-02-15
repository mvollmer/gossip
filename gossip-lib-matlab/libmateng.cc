/* Accessing Matlab from Gossip

   Copyright (C) 2000  Marius Vollmer

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

/* Library of routines for accessing Matlab. */

#include "libmateng.h"

#include <iostream.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <signal.h>
#include <stddef.h>

#include <sys/socket.h>
#include <sys/un.h>

struct prot_req {
  int in_rows;
  int in_cols;
  int out_rows;
  int out_cols;
  int cmd_len;
  // char cmd[cmd_len];
  // complex<double> in[in_rows][in_cols];
};

struct prot_rep {
  int success;
  // complex<double> out[out_rows][out_cols];
};

static const char *
getusername ()
{
  const char *l = getenv ("LOGNAME");
  if (l)
    return l;
  return getlogin ();
}

static const char *
getid (const char *id)
{
  if (id)
    return id;
  return "default";
}

static void
perror_exit (const char *msg)
{
  perror (msg);
  exit (1);
}

matlab_engine::matlab_engine (const char *id)
{
  char *pwd = getcwd (NULL, 0);
  const char *login = getusername ();
  const char *rid = getid (id);
  
  char *dir = new char[12 + strlen(login) + 1 + strlen(rid) + 1];

  sprintf (dir, "/tmp/mateng-%s-%s", login, rid);

  fprintf (stderr, "Matlab Client starting up for `%s'.\n", dir);

  if (access (dir, F_OK) < 0)
    {
      // Rendevous directory does not exits: startup Matlab.

      if (mkdir (dir, S_IRWXU) < 0)
	perror_exit (dir);

      chdir (dir);
  
      {
	FILE *f = fopen ("startup.m", "w");
	if (f == NULL)
	  perror_exit ("startup.m");
	
	fprintf (f, "matlabengine ('%s')\n", pwd);
	
	fclose (f);
      }

      if (symlink (MEXDIR "/matlabengine.mexglx",
		   "matlabengine.mexglx") < 0)
	perror_exit ("matlabengine.mexglx");

      system ("matlab &");
      
      // blocks until Matlab is there
      mkfifo ("startup-semaphore", S_IRUSR | S_IWUSR);
      close (open ("startup-semaphore", O_RDONLY));
      
      unlink ("startup-semaphore");
      unlink ("startup.m");
      unlink ("matlabengine.mexglx");
    }

  // Connect to Matlab server

  chdir (dir);

  {
    sockaddr_un addr;
    size_t size;

    server = socket (PF_UNIX, SOCK_STREAM, 0);
    if (server < 0)
      perror_exit ("socket");

    addr.sun_family = AF_UNIX;
    strcpy (addr.sun_path, "socket");

    size = (offsetof (struct sockaddr_un, sun_path)
	    + strlen (addr.sun_path) + 1);
    if (connect (server, (sockaddr *)&addr, size) < 0)
      perror_exit ("connect");

  }

  chdir (pwd);
  free (pwd);

  // Run "startup.m" in the original directory if it exists.
  if (access ("startup.m", F_OK) == 0)
    exchange (0, 0, NULL,
	      "startup",
	      0, 0, NULL);
}

matlab_engine::~matlab_engine ()
{
  fprintf (stderr, "Matlab Client closing down.\n");
  close (server);
}

bool
matlab_engine::exchange (int in_rows, int in_cols, const complex<double> **in,
			 const char *cmd,
			 int out_rows, int out_cols, complex<double> **out)
{
  sigset_t sigpipe;
  sigemptyset (&sigpipe);
  sigaddset (&sigpipe, SIGPIPE);

  sigprocmask (SIG_BLOCK, &sigpipe, NULL);

  prot_req req;

  req.in_rows = in_rows;
  req.in_cols = in_cols;
  req.out_rows = out_rows;
  req.out_cols = out_cols;
  req.cmd_len = strlen (cmd);

  if (write (server, &req, sizeof(req)) < sizeof(req))
    goto fail;
  if (write (server, cmd, req.cmd_len) < req.cmd_len)
    goto fail;
  
  for (int j = 0; j < in_cols; j++)
    for (int i = 0; i < in_rows; i++)
      {
	double x = real(in[j][i]);
	if (write (server, &x, sizeof(double)) < sizeof(double))
	  goto fail;
      }
  for (int j = 0; j < in_cols; j++)
    for (int i = 0; i < in_rows; i++)
      {
	double x = imag(in[j][i]);
	if (write (server, &x, sizeof(double)) < sizeof(double))
	  goto fail;
      }

  prot_rep rep;
  if (read (server, &rep, sizeof(rep)) < sizeof(rep))
    goto fail;

  if (rep.success)
    {
      for (int j = 0; j < out_cols; j++)
	for (int i = 0; i < out_rows; i++)
	  {
	    double x;
	    if (read (server, &x, sizeof(double)) < sizeof(double))
	      goto fail;
	    out[j][i] = x;
	  }
      for (int j = 0; j < out_cols; j++)
	for (int i = 0; i < out_rows; i++)
	  {
	    double x;
	    if (read (server, &x, sizeof(double)) < sizeof(double))
	      goto fail;
	    out[j][i] += complex<double>(0,x);
	  }
      sigprocmask (SIG_UNBLOCK, &sigpipe, NULL);
      return true;
    }

 fail:
  sigprocmask (SIG_UNBLOCK, &sigpipe, NULL);
  return false;
}
