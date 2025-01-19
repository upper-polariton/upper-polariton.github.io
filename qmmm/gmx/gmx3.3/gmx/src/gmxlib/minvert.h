/*
 * $Id: minvert.h,v 1.6 2002/02/28 10:49:26 spoel Exp $
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
 * Gyas ROwers Mature At Cryogenic Speed
 */

#ifndef _minvert_h
#define _minvert_h

static char *SRCID_minvert_h = "$Id: minvert.h,v 1.6 2002/02/28 10:49:26 spoel Exp $";
#include "typedefs.h"

/* A bunch of routines that works on matrices that run from 1 thru n
 * although they are allocated from 0 thru n
 */
	
extern void mat_mult(int n,real **A,real **B,real **C);

extern real **mk_mat(int n);

extern real **mk_mat2(int nrow,int ncol);

extern void cp_mat(int n,real **src,real **dest);

extern void print_mat(FILE *fp,char *title,int n,real **a,int *indx);
/* index may be NULL */

extern void invert_mat(int n,real **A,real **Ainv);

#endif
