/*
 * $Id: angstat.h,v 1.6 2002/02/28 11:00:23 spoel Exp $
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
 * Gromacs Runs One Microsecond At Cannonball Speeds
 */

#ifndef _angstat_h
#define _angstat_h

static char *SRCID_angstat_h = "$Id: angstat.h,v 1.6 2002/02/28 11:00:23 spoel Exp $";
#include <stdio.h>
#include <typedefs.h>

extern void do_angav(FILE *status, char *title,
		     atom_id index[], int nind, 
		     char *outfile, bool bAT);

extern void do_dihav(FILE *status, char *title,
		     atom_id index[], int nind, 
		     char *outfile, bool bAT);

extern void ramachandran(FILE *status,atom_id index[],int nind,char *outfile);

extern void dis_mon(FILE *status,atom_id index[],int nind,char *outfile,
		    t_first_x *fx, t_next_x *nx);

#endif
