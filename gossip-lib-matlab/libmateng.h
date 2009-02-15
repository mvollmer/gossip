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

#ifndef LIBMATENG_H
#define LIBMATENG_H

#include <complex.h>

struct matlab_engine {
  
  matlab_engine (const char *id);
  ~matlab_engine ();

  bool exchange (int in_cols, int in_rows, const complex<double> **in,
		 const char *cmd,
		 int out_cols, int out_rows, complex<double> **out);

private:
  int server;
};

#endif
