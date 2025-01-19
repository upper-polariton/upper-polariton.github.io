/*
 * $Id: pinput.c,v 1.6 2002/02/28 11:00:29 spoel Exp $
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
static char *SRCID_pinput_c = "$Id: pinput.c,v 1.6 2002/02/28 11:00:29 spoel Exp $";
#include "paramio.h"
#include "pinput.h"

#define PPIN \
  ITYPE("nframes",              p->nframes,        1) \
  ETYPE("optimize",             p->nSel)              \
        "radius", "twist", "rise", "len", "nhx", "dipole", "rms", "cphi", NULL, \
  ETYPE("funct",                p->funct)              \
        "MC", "RECOMB", "PROPTRJ", NULL, \
  ITYPE("nsteps",               p->nsteps,         100) \
  ITYPE("nev",                  p->nev,            10) \
  ITYPE("nskip",                p->nskip,          0) \
  STYPE("projection",           p->base,           "WEDPRJVEC10.DAT") \
  STYPE("recomb",               p->recomb,         "WEDRECOMB10.DAT") \
  STYPE("gamma",                p->gamma,          "WEDGAMMA10.DAT") \
  RTYPE("stepsize",             p->step,           0.1) \
  RTYPE("tolerance",            p->tol,            1e-6) \
  RTYPE("ref-fluc",             p->v0,             1e-3) \
  NULL

void read_inp(char *fnin,char *fnout,t_pinp *p)
{
  read_params(fnin,PPIN);
  write_params(fnout,PPIN);
}
