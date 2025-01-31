/*
 * $Id: g_rotacf.c,v 1.16 2002/02/28 11:00:27 spoel Exp $
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
 * GROtesk MACabre and Sinister
 */
static char *SRCID_g_rotacf_c = "$Id: g_rotacf.c,v 1.16 2002/02/28 11:00:27 spoel Exp $";
#include <math.h>
#include <string.h>
#include "sysstuff.h"
#include "physics.h"
#include "typedefs.h"
#include "smalloc.h"
#include "futil.h"
#include "statutil.h"
#include "copyrite.h"
#include "rdgroup.h"
#include "macros.h"
#include "fatal.h"
#include "xvgr.h"
#include "gstat.h"
#include "vec.h"

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "g_rotacf calculates the rotational correlation function",
    "for molecules. Three atoms (i,j,k) must be given in the index",
    "file, defining two vectors ij and jk. The rotational acf",
    "is calculated as the autocorrelation function of the vector",
    "n = ij x jk, i.e. the cross product of the two vectors.",
    "Since three atoms span a plane, the order of the three atoms",
    "does not matter. Optionally, controlled by the -d switch, you can",
    "calculate the rotational correlation function for linear molecules",
    "by specifying two atoms (i,j) in the index file.",
    "[PAR]",
    "EXAMPLES[PAR]",
    "g_rotacf -P 1 -nparm 2 -fft -n index -o rotacf-x-P1",
    "-fa expfit-x-P1 -beginfit 2.5 -endfit 20.0[PAR]",
    "This will calculate the rotational correlation function using a first",
    "order Legendre polynomial of the angle of a vector defined by the index",
    "file. The correlation function will be fitted from 2.5 ps till 20.0 ps",
    "to a two parameter exponential",


    ""
  };
  static bool bVec    = FALSE,bAver=TRUE;

  t_pargs pa[] = {
    { "-d",   FALSE, etBOOL, {&bVec},
      "Use index doublets (vectors) for correlation function instead of triplets (planes)" },
    { "-aver",FALSE, etBOOL, {&bAver},
      "Average over molecules" }
  };

  int        status,isize;
  atom_id    *index;
  char       *grpname;
  rvec       *x,*x_s;
  matrix     box;
  real       **c1;
  rvec       xij,xjk,n;
  int        i,m,teller,n_alloc,natoms,nvec,ai,aj,ak;
  unsigned long mode;
  real       t,t0,t1,dt;
  t_topology *top;
  t_filenm   fnm[] = {
    { efTRX, "-f", NULL,  ffREAD  },
    { efTPX, NULL, NULL,  ffREAD },
    { efNDX, NULL, NULL,  ffREAD  },
    { efXVG, "-o", "rotacf",  ffWRITE }
  };
#define NFILE asize(fnm)
  int     npargs;
  t_pargs *ppa;
  
  CopyRight(stderr,argv[0]);
  npargs = asize(pa);
  ppa    = add_acf_pargs(&npargs,pa);
  
  parse_common_args(&argc,argv,PCA_CAN_VIEW | PCA_CAN_TIME | PCA_BE_NICE,
		    NFILE,fnm,npargs,ppa,asize(desc),desc,0,NULL);
  
  rd_index(ftp2fn(efNDX,NFILE,fnm),1,&isize,&index,&grpname);
  
  if (bVec) 
    nvec = isize/2;
  else
    nvec = isize/3;
  
  if (((isize % 3) != 0) && !bVec)
    fatal_error(0,"number of index elements not multiple of 3, "
		"these can not be atom triplets\n");
  if (((isize % 2) != 0) && bVec)
    fatal_error(0,"number of index elements not multiple of 2, "
		"these can not be atom doublets\n");
  
  top=read_top(ftp2fn(efTPX,NFILE,fnm));
  
  snew(c1,nvec);
  for (i=0; (i<nvec); i++)
    c1[i]=NULL;
  n_alloc=0;

  natoms=read_first_x(&status,ftp2fn(efTRX,NFILE,fnm),&t,&x,box);
  snew(x_s,natoms);
  
  /* Start the loop over frames */
  t1 = t0 = t;
  teller  = 0;
  do {
    if (teller >= n_alloc) {
      n_alloc+=100;
      for (i=0; (i<nvec); i++)
	srenew(c1[i],DIM*n_alloc);
    }
    t1 = t;
    
    /* Remove periodicity */
    rm_pbc(&(top->idef),natoms,box,x,x_s);
    
    /* Compute crossproducts for all vectors, if triplets.
     * else, just get the vectors in case of doublets.
     */
    if (bVec == FALSE) {
      for (i=0; (i<nvec); i++) {
	ai=index[3*i];
	aj=index[3*i+1];
	ak=index[3*i+2];
	rvec_sub(x_s[ai],x_s[aj],xij);
	rvec_sub(x_s[aj],x_s[ak],xjk);
	oprod(xij,xjk,n);
	for(m=0; (m<DIM); m++)
	  c1[i][DIM*teller+m]=n[m];
      }
    }
    else {
      for (i=0; (i<nvec); i++) {
	ai=index[2*i];
	aj=index[2*i+1];
	rvec_sub(x_s[ai],x_s[aj],n);
	for(m=0; (m<DIM); m++)
	  c1[i][DIM*teller+m]=n[m];
      }
    }
    /* Increment loop counter */
    teller++;
  } while (read_next_x(status,&t,natoms,x,box));  
  close_trj(status); 
  fprintf(stderr,"\nDone with trajectory\n");
  
  /* Autocorrelation function */
  if (teller < 2)
    fprintf(stderr,"Not enough frames for correlation function\n");
  else {
    dt=(t1 - t0)/(teller-1);
    
    mode = eacVector;
    
    do_autocorr(ftp2fn(efXVG,NFILE,fnm),"Rotational Correlation Function",
		teller,nvec,c1,dt,mode,bAver);
  }

  do_view(ftp2fn(efXVG,NFILE,fnm),NULL);
    
  thanx(stderr);
    
  return 0;
}
