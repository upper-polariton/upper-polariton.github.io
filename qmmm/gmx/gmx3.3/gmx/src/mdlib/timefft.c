/*
 * $Id: timefft.c,v 1.5 2002/02/28 10:32:06 spoel Exp $
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
 * GROup of MAchos and Cynical Suckers
 */
static char *SRCID_timefft_c = "$Id: timefft.c,v 1.5 2002/02/28 10:32:06 spoel Exp $";
#include <math.h>
#include <stdio.h>
#include <time.h>
#include "typedefs.h"
#include "macros.h"
#include "smalloc.h"
#include "xvgr.h"
#include "complex.h"
#include "copyrite.h"
#include "fftgrid.h"
#include "mdrun.h"
#include "main.h"
#include "statutil.h"

int main(int argc,char *argv[])
{
  int       mmm[] = { 8, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40,
		      45, 48, 50, 54, 60, 64, 72, 75, 80, 81, 90, 100 };
  int       nnn[] = { 24, 32, 48, 60, 72, 84, 96 };
#define NNN asize(nnn)
  FILE      *fp;
  int       *niter;
  int       i,j,n,nit,ntot,n3,rsize;
  double    t,nflop,start;
  double    *rt,*ct;
  t_fftgrid *g;
  t_commrec *cr;
  static bool bOptFFT = FALSE;
  static int  nnode    = 1;
  static int  nitfac  = 1;
  t_pargs pa[] = {
    { "-opt",   FALSE, etBOOL, {&bOptFFT}, 
      "Optimize FFT" },
    { "-np",    FALSE, etINT, {&nnode},
      "Number of NODEs" },
    { "-itfac", FALSE, etINT, {&nitfac},
      "Multiply number of iterations by this" }
  };
  static t_filenm fnm[] = {
    { efLOG, "-g", "fft",      ffWRITE },
    { efXVG, "-o", "fft",      ffWRITE }
  };
#define NFILE asize(fnm)
  
  cr = init_par(&argc,&argv);
  if (MASTER(cr))
    CopyRight(stdout,argv[0]);
  parse_common_args(&argc,argv,
		    PCA_CAN_SET_DEFFNM | (MASTER(cr) ? 0 : PCA_QUIET),
		    TRUE,NFILE,fnm,asize(pa),pa,0,NULL,0,NULL);
  open_log(ftp2fn(efLOG,NFILE,fnm),cr);

  snew(niter,NNN);
  snew(ct,NNN);
  snew(rt,NNN);
  rsize = sizeof(real);
  for(i=0; (i<NNN); i++) {
    n  = nnn[i];
    if (n < 16)
      niter[i] = 50;
    else if (n < 26)
      niter[i] = 20;
    else if (n < 51)
      niter[i] = 10;
    else
      niter[i] = 5;
    niter[i] *= nitfac;
    nit = niter[i];
    
    if (MASTER(cr))
      fprintf(stderr,"\r3D FFT (%s precision) %3d^3, niter %3d     ",
	      (rsize == 8) ? "Double" : "Single",n,nit);
    
    g  = mk_fftgrid(stdlog,(nnode > 1),n,n,n,bOptFFT);

    if (PAR(cr))
      start = time(NULL);
    else
      start_time();
    for(j=0; (j<nit); j++) {
      gmxfft3D(g,FFTW_FORWARD,cr);
      gmxfft3D(g,FFTW_BACKWARD,cr);
    }
    if (PAR(cr)) 
      rt[i] = time(NULL)-start;
    else {
      update_time();
      rt[i] = node_time();
    }
    done_fftgrid(g);
    sfree(g);
  }
  if (MASTER(cr)) {
    fprintf(stderr,"\n");
    fp=xvgropen(ftp2fn(efXVG,NFILE,fnm),
		"FFT timings","n^3","t (s)");
    for(i=0; (i<NNN); i++) {
      n3 = 2*niter[i]*nnn[i]*nnn[i]*nnn[i];
      fprintf(fp,"%10d  %10g\n",nnn[i],rt[i]/(2*niter[i]));
    }
    fclose(fp);
  }
  return 0;
}
