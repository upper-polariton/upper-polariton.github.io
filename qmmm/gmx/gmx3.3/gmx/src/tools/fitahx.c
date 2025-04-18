/*
 * $Id: fitahx.c,v 1.8 2002/02/28 11:00:24 spoel Exp $
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
static char *SRCID_fitahx_c = "$Id: fitahx.c,v 1.8 2002/02/28 11:00:24 spoel Exp $";
#include "fitahx.h"
#include "vec.h"
#include "do_fit.h"
#include "smalloc.h"
	
static void my_calc_xcm(int nbb,atom_id bbind[],rvec x[],rvec xcm)
{
  int    i,m,ai;
  
  clear_rvec(xcm);
  for(i=0; (i<nbb); i++) {
    ai=bbind[i];
    rvec_inc(xcm,x[ai]);
  }
  for(m=0; (m<DIM); m++)
    xcm[m]/=(nbb);
}

static void my_sub_xcm(int nbb,atom_id bbind[],rvec x[],rvec xcm)
{
  int i,ai;
  
  for(i=0; (i<nbb); i++) {
    ai=bbind[i];
    rvec_dec(x[ai],xcm);
  }
}

real fit_ahx(int nres,t_bb bb[],int natoms,int nall,atom_id allindex[],
	     rvec x[],int nca,
	     atom_id caindex[],matrix box,bool bFit)
{
  static rvec *xref=NULL;
  static real *mass=NULL;
  const  real d=0.15;     /* Rise per residue (nm)    */
  const  real tw=1.745;   /* Twist per residue (rad)  */
  const  real rad=0.23;   /* Radius of the helix (nm) */
  real   phi0,trms,rms;
  rvec   dx,xcm;
  int    ai,i,nmass;
  
  if (nca < 3)
    fatal_error(0,"Need at least 3 Calphas to fit to, (now %d)...\n",nca);
  
  if (xref == NULL) {
    snew(xref,natoms);
    snew(mass,natoms);
  }
  phi0=0;
  for(i=0; (i<nca); i++) {
    ai=caindex[i];
    xref[ai][XX]=rad*cos(phi0);
    xref[ai][YY]=rad*sin(phi0);
    xref[ai][ZZ]=d*i;
    
    /* Set the mass to select that this Calpha contributes to fitting */
    mass[ai]=10.0;
/*#define DEBUG*/
#ifdef DEBUG
    fprintf(stderr,"%5d  %8.3f  %8.3f  %8.3f  %8.3f  %8.3f  %8.3f\n",ai,
	    x[ai][XX],x[ai][YY],x[ai][ZZ],
	    xref[ai][XX],xref[ai][YY],xref[ai][ZZ]);
#endif
    phi0+=tw;
  }

  /* Center the referece around the origin */
  my_calc_xcm(nca,caindex,xref,xcm);
  my_sub_xcm(nca,caindex,xref,xcm);
  
  if (bFit) {
    /* Now center the to-be-fitted coords around the origin */
    my_calc_xcm(nca,caindex,x,xcm);
    my_sub_xcm(nall,allindex,x,xcm);
  }
#ifdef DEBUG
  dump_ahx(nres,bb,xref,box,0);
#endif
  
  nmass=0;
  for(i=0; (i<natoms); i++)
    if (mass[i] > 0)
      nmass++;
  if (nmass != nca)
    fatal_error(0,"nmass=%d, nca=%d\n",nmass,nca);
    
  /* Now call the fitting routine */
  if (bFit)
    do_fit(natoms,mass,xref,x);

  /* Reset masses and calc rms */  
  trms=0.0;
  for(i=0; (i<nres); i++) {
    ai=bb[i].CA;
    
    if (mass[ai] > 0.0) {
      rvec_sub(x[ai],xref[ai],dx);
      rms=iprod(dx,dx);
      bb[i].rmsa+=sqrt(rms);
      bb[i].nrms++;
      trms+=rms;
      mass[ai]=0.0;
    }
  }
  return sqrt(trms/nca);
}

