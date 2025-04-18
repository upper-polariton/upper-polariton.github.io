/*
 * $Id: xtcio.h,v 1.10 2002/02/28 21:55:52 spoel Exp $
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
 * Getting the Right Output Means no Artefacts in Calculating Stuff
 */

#ifndef _xtcio_h
#define _xtcio_h

static char *SRCID_xtcio_h = "$Id: xtcio.h,v 1.10 2002/02/28 21:55:52 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef CPLUSPLUS
extern "C" {
#endif

#include "typedefs.h"
#include "xdrf.h"

/* All functions return 1 if succesfull, 0 otherwise 
 * bOK tells if a frame is not corrupted 
 */  

extern int open_xtc(char *filename,char *mode);
/* Open a file for xdr I/O */
  
extern void close_xtc(int fp);
/* Close the file for xdr I/O */
  
extern int read_first_xtc(int fp,
			  int *natoms,int *step,real *time,
			  matrix box,rvec **x,real *prec,bool *bOK);
/* Open xtc file, read xtc file first time, allocate memory for x */

extern int read_next_xtc(int fp,
			 int natoms,int *step,real *time,
			 matrix box,rvec *x,real *prec,bool *bOK);
/* Read subsequent frames */

extern int write_xtc(int fp,
		     int natoms,int step,real time,
		     matrix box,rvec *x,real prec);
/* Write a frame to xtc file */

extern int xtc_check(char *str,bool bResult,char *file,int line);
#define XTC_CHECK(s,b) xtc_check(s,b,__FILE__,__LINE__)

extern void xtc_check_fat_err(char *str,bool bResult,char *file,int line);
#define XTC_CHECK_FAT_ERR(s,b) xtc_check_fat_err(s,b,__FILE__,__LINE__)

#ifdef CPLUSPLUS
}
#endif

#endif
