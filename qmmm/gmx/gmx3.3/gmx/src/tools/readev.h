/*
 * $Id: readev.h,v 1.6 2002/02/28 11:00:29 spoel Exp $
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
static char *SRCID_readev_h = "$Id: readev.h,v 1.6 2002/02/28 11:00:29 spoel Exp $";
#include "typedefs.h"

#ifndef _readev_h
#define _readev_h
	
extern rvec **read_ev(char *fn,int natoms);
/* Read eigenvectors from file fn */

extern real **read_proj(int nev,int nframes,char *base);
/* Read the projections of a traj onto ev from files base1 thru basenev */

#endif
