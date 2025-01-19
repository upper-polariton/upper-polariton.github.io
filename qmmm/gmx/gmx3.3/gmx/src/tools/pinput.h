/*
 * $Id: pinput.h,v 1.6 2002/02/28 11:00:29 spoel Exp $
 * 
 *                This source code is part of
 * 
 *                 G   R   O   M   A   C   S
 * 
 *          GROningen MAchine for Chemical Simulations
 * 
 *                        VERSION 3.1
 * Copyright (c) 1991-2001, University of Groningen, The Netherlands
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * If you want to redistribute modifications, please consider that
 * scientific software is very special. Version control is crucial -
 * bugs must be traceable. We will be happy to consider code for
 * inclusion in the official distribution, but derived work must not
 * be called official GROMACS. Details are found in the README & COPYING
 * files - if they are missing, get the official version at www.gromacs.org.
 * 
 * To help us fund GROMACS development, we humbly ask that you cite
 * the papers on the package - you can find them in the top README file.
 * 
 * For more info, check our website at http://www.gromacs.org
 * 
 * And Hey:
 * Green Red Orange Magenta Azure Cyan Skyblue
 */

#ifndef _pinput_h
#define _pinput_h

static char *SRCID_pinput_h = "$Id: pinput.h,v 1.6 2002/02/28 11:00:29 spoel Exp $";
#include "typedefs.h"
#include "string2.h"

enum { ptMC, ptREC, ptPTRJ, ptNR };

typedef struct {
  real step;
  real tol;
  real v0;
  char base[STRLEN];
  char recomb[STRLEN];
  char gamma[STRLEN];
  int  funct;
  int  nsteps;
  int  nframes;
  int  nskip;
  int  nSel;
  int  nev;
} t_pinp;
	
extern void read_inp(char *fnin,char *fnout,t_pinp *p);

#endif
