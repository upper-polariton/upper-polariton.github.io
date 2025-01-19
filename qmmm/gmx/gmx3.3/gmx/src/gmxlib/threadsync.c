/*
 * $Id: threadsync.c,v 1.4 2002/02/28 10:49:31 spoel Exp $
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
 * Gnomes, ROck Monsters And Chili Sauce
 */
static char *SRCID_threadsync_c = "$Id: threadsync.c,v 1.4 2002/02/28 10:49:31 spoel Exp $";
/* Since most fortran compilers dont support threads started
 * in a calling c program we call these wrapper syncronization
 * routines from the fortran innerloops
 */
#ifdef USE_FORTRAN
void FUNCTION(inlsync)(int *nri,int *nthreads,int *count,int *ii0,
		       int *ii1, pthread_mutex_t *mtx)
{
  int t0,t1;
  pthread_mutex_lock(mtx);
  t0=*count;
  t1=t0+(*nri-t0)/nthreads+1;
  *count=t1;
  pthread_mutex_unlock(mtx);
  *ii0=t0;
  *ii1=t1;
}
#endif

