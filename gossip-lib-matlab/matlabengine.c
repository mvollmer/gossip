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

#include "mex.h"
#include "fcntl.h"

#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>

struct prot_req {
  int in_rows;
  int in_cols;
  int out_rows;
  int out_cols;
  int cmd_len;
  /* char cmd[cmd_len]; */
  /* complex<double> in[in_rows][in_cols]; */
};

struct prot_rep {
  int success;
  /* complex<double> out[out_rows][out_cols]; */
};

static void
usage ()
{
  mexErrMsgTxt ("usage: matlabengine (pwd)");
}

static int
eager_read (int fd, void *buf, size_t size)
{
  char *cbuf = (char *)buf;
  int n = 0, nn;

  while (n < size)
    {
      nn = read (fd, cbuf+n, size-n);
      if (nn < 0)
	return nn;
      if (nn == 0)
	return n;
      n += nn;
    }

  return n;
}

/* XXX - we assume that we have only one display open, and that it's */
/* connection number is less than FDMAX.  Then we use some guesswork */
/* to identify the socket by looking at its peer name. */
/*  */
/* A better strategy would probably be to open $DISPLAY ourselves and */
/* look for another socket with the same peer name. */

static int
guess_matlab_X11_connection (int fdmax)
{
  int fd;

  for (fd = 3; fd < fdmax; fd++)
    {
      struct sockaddr_un addr;
      int addr_size = sizeof(addr);
      if (getpeername (fd, (struct sockaddr *)&addr, &addr_size) >= 0)
	{
	  if (addr.sun_family == AF_UNIX)
	    {
	      /* fprintf (stderr, "%s\n", addr.sun_path); */
	      if (!strncmp (addr.sun_path, "/tmp/.X11-unix", 14))
		return fd;
	    }
	  else if (addr.sun_family == AF_INET)
	    {
	      int port;
	      struct sockaddr_in *addr_in = (struct sockaddr_in *)&addr;
	      port = ntohs (addr_in->sin_port);
	      /* fprintf (stderr, "%d\n", ntohs (addr_in->sin_port)); */
	      if (port >= 6000 && port <= 6020)
		return fd;
	    }
	}
    }

  return -1;
}

static int
handle_client (int client)
{
  mxArray *in, *out = NULL;
  double *in_re, *in_im;
  double zero = 0;
  
  int n;
  struct prot_req req;
  struct prot_rep rep;
  char *cmd;
  
  /* fprintf (stderr, "waiting for request\n"); */

  n = eager_read (client, &req, sizeof(req));
  if (n < 0)
    mexErrMsgTxt ("read");
  else if (n < sizeof(req))
    return 0;

/*   // fprintf (stderr, "received request: %d %d %d %d %d\n", */
/*   // 	 req.in_rows, req.in_cols, req.out_rows, req.out_cols, */
/*   // 	 req.cmd_len); */
  
  cmd = malloc (req.cmd_len+1);
  n = eager_read (client, cmd, req.cmd_len);
  if (n < 0)
    mexErrMsgTxt ("read");
  else if (n < req.cmd_len)
    return 0;
  cmd[req.cmd_len] = '\0';
	
  /* fprintf (stderr, "command: %s\n", cmd); */

  in = mxCreateDoubleMatrix (req.in_rows, req.in_cols, mxCOMPLEX);
  in_re = mxGetPr (in);
  in_im = mxGetPi (in);
  mxSetName (in, "in");

  n = eager_read (client, in_re, sizeof(double)*req.in_rows*req.in_cols);
  if (n < 0)
    mexErrMsgTxt ("read");
  else if (n < sizeof(double)*req.in_rows*req.in_cols)
    return 0;
      
  n = eager_read (client, in_im, sizeof(double)*req.in_rows*req.in_cols);
  if (n < 0)
    mexErrMsgTxt ("read");
  else if (n < sizeof(double)*req.in_rows*req.in_cols)
    return 0;
      
  mexPutArray (in, "base");
  rep.success = !mexEvalString (cmd);
  free (cmd);
  if (write (client, &rep, sizeof(rep)) < 0)
    goto stop;

  if (rep.success)
    {
      out = mexGetArray ("out", "base");

      if (out == NULL)
	{
	  int i;
	  for (i = 0; i < 2*req.out_rows*req.out_cols; i++)
	    if (write (client, &zero, sizeof(double)) < 0)
	      goto stop;
	}
      else
	{
	  int i, j, ii, jj;
	  double *out_re, *out_im;
	  double zero = 0;
	  
	  ii = mxGetM (out);
	  jj = mxGetN (out);
	  out_re = mxGetPr (out);
	  out_im = mxGetPi (out);
	  
	  for (j = 0; j < req.out_cols; j++)
	    for (i = 0; i < req.out_rows; i++)
	      if (i < ii && j < jj)
		{
		  if (write (client, &out_re[j*ii+i], sizeof(double)) < 0)
		    goto stop;
		}
	      else
		{
		  if (write (client, &zero, sizeof(double)) < 0)
		    goto stop;
		}
	  
	  for (j = 0; j < req.out_cols; j++)
	    for (i = 0; i < req.out_rows; i++)
	      if (out_im && i < ii && j < jj)
		{
		  if (write (client, &out_im[j*ii+i], sizeof(double)) < 0)
		    goto stop;
		}
	      else
		{
		  if (write (client, &zero, sizeof(double)) < 0)
		    goto stop;
		}
	}

      if (out)
	mxDestroyArray (out);
      return 1;

    stop:
      if (errno)
	perror ("Matlab Engine: write");
      if (out)
	mxDestroyArray (out);
      return 0;
    }

  return 1;
}

static void
sigterm (int signum)
{
  signal (signum, sigterm);
}

void mexFunction( int nlhs, mxArray *plhs[], 
		  int nrhs, const mxArray*prhs[] )
{
  char *pwd;
  char *cmd;
  int sock, i, X11_connection;
  fd_set active_fd_set, read_fd_set;
  struct timeval timeout;

  if (nrhs != 1
      || !mxIsChar (prhs[0]))
    usage ();

  pwd = getcwd (NULL, 0);

  fprintf (stderr, "Matlab Engine starting up in `%s'.\n", pwd);

  sock = socket (PF_UNIX, SOCK_STREAM, 0);
  
  {
    struct sockaddr_un addr;
    size_t size;

    addr.sun_family = AF_UNIX;
    strcpy (addr.sun_path, "socket");
    size = (offsetof (struct sockaddr_un, sun_path)
	    + strlen (addr.sun_path) + 1);
    bind (sock, &addr, size);
  }

  {
    FILE *f = fopen ("pid", "w");
    if (f)
      {
	fprintf (f, "%d\n", getpid ());
	fclose (f);
      }
  }

  listen (sock, 5);

  /* signal client that we are ready. */

  close (open ("startup-semaphore", O_WRONLY));

  {
    char *pwd = mxArrayToString (prhs[0]);
    if (pwd)
      {
	char cmd[3+strlen(pwd)+1];
	strcpy (cmd, "cd ");
	strcat (cmd, pwd);
	mexEvalString (cmd);
	mxFree (pwd);
      }
  }

  signal (SIGTERM, sigterm);
  signal (SIGINT, sigterm);

  X11_connection = guess_matlab_X11_connection (100);

  fprintf (stderr, "Matlab Engine ready (listening on %d, X11 on %d).\n",
	   sock, X11_connection);

  /* Initialize the set of active sockets. */
  FD_ZERO (&active_fd_set);
  FD_SET (sock, &active_fd_set);
  if (X11_connection >= 0)
    FD_SET (X11_connection, &active_fd_set);

  while (1)
    {
      /* Block until input arrives on one or more active sockets. */
      read_fd_set = active_fd_set;
      timeout.tv_sec = 5*60;
      timeout.tv_usec = 0;
      i = select (FD_SETSIZE, &read_fd_set, NULL, NULL, &timeout);
      if (i == 0)
	{
	  fprintf (stderr, "Matlab Engine timed out.\n");
	  break;
	}
      if (i < 0)
	{
	  if (errno == EINTR)
	    fprintf (stderr, "Matlab Engine killed.\n");
	  else
	    perror ("Matlab Engine: select");
	  break;
	}

      /* Service all the sockets with input pending. */
      for (i = 0; i < FD_SETSIZE; ++i)
	if (FD_ISSET (i, &read_fd_set))
	  {
	    if (i == sock)
	      {
		int client = accept (sock, NULL, NULL);
		if (client < 0)
		  perror ("accept");
		fprintf (stderr, "Matlab Engine connected (%d).\n", client);
		FD_SET (client, &active_fd_set);
		/* cause M-files to reload */
		mexEvalString ("clear functions;");
	      }
	    else if (i == X11_connection)
	      {
		/* fprintf (stderr, "X11!\n"); */
		mexEvalString("drawnow;");
	      }
	    else
	      {
		if (!handle_client (i))
		  {
		    fprintf (stderr, "Matlab Engine disconnected (%d).\n", i);
		    close (i);
		    FD_CLR (i, &active_fd_set);
		  }
	      }
	  }
    }

  fprintf (stderr, "Matlab Engine closing down.\n");

  close (sock);

  chdir (pwd);
  unlink ("socket");
  unlink ("pid");
  rmdir (pwd);

  exit (0);
}
