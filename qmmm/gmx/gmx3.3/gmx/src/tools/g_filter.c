/*
 * $Id: g_gyrate.c,v 1.17 2002/02/28 11:00:26 spoel Exp $
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
static char *SRCID_g_filter_c = "$Id: g_gyrate.c,v 1.17 2002/02/28 11:00:26 spoel Exp $";
#include <math.h>
#include <string.h>
#include "statutil.h"
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "vec.h"
#include "statutil.h"
#include "rdgroup.h"
#include "tpxio.h"
#include "princ.h"
#include "do_fit.h"

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "g_filter performs frequency filtering on a trajectory.",
    "The filter shape is cos(pi t/A) + 1 from -A to +A, where A is given",
    "by the option [TT]-nf[tt] times the time step in the input trajectory.",
    "This filter reduces fluctuations with period [TT]nf[tt] by 85%,",
    "with period 2*[TT]nf[tt] by 50% and with period 3*[TT]nf[tt] by 17%",
    "for low-pass filtering.",
    "Both a low-pass and high-pass filtered trajectory can be written.",
    "For low-pass filtering a frame is written every [TT]nf[tt] frames.",
    "This ratio of filter length and output interval ensures a good",
    "suppression of aliasing of high-frequency motion, which is useful for",
    "making smooth movies. Also averages of properties which are linear",
    "in the coordinates are preserved, since all input frames are weighted",
    "equally in the output.",
    "When all frames are needed, use the [TT]-all[tt] option.",
    "The high-pass filtered coordinates are added to the coordinates",
    "from the structure file. When using high-pass filtering use [TT]-fit[tt]",
    "or make sure you use a trajectory which has been fitted on",
    "the coordinates in the structure file."
  };
  
  static int nf=10;
  static bool bFit = FALSE,bLowAll = FALSE;
  t_pargs pa[] = {
    { "-nf", FALSE, etINT, {&nf},
      "Sets the filter length as well as the output interval for low-pass filtering" },
    { "-all", FALSE, etBOOL, {&bLowAll},
      "Write all lowpass filtered frames" },
    { "-fit", FALSE, etBOOL, {&bFit},
      "Fit all frames to a reference structure" }
  };
  char       *topfile,*lowfile,*highfile;
  bool       bTop=FALSE;
  t_topology top;
  rvec       *xtop;
  matrix     topbox,*box,boxf;
  char       title[256],*grpname;
  int        isize;
  atom_id    *index;
  real       *w_rls;
  int        in,outl,outh;
  int        nffr,i,fr,nat,j,d;
  atom_id    *ind;
  real       flen,*filt,sum,*t;
  rvec       xcmtop,xcm,**x,*ptr,*xf;

#define NLEG asize(leg)
  t_filenm fnm[] = { 
    { efTRX, "-f", NULL, ffREAD  }, 
    { efTPS, NULL, NULL, ffOPTRD },
    { efNDX, NULL, NULL, ffOPTRD },
    { efTRX, "-ol", "lowpass",  ffOPTWR }, 
    { efTRX, "-oh", "highpass", ffOPTWR } 
  }; 
#define NFILE asize(fnm) 

  CopyRight(stderr,argv[0]); 
  parse_common_args(&argc,argv,PCA_CAN_TIME | PCA_CAN_VIEW | PCA_BE_NICE,
		    NFILE,fnm,asize(pa),pa,asize(desc),desc,0,NULL); 

  highfile = opt2fn_null("-oh",NFILE,fnm);
  if (highfile) {
    topfile = ftp2fn(efTPS,NFILE,fnm);
    lowfile = opt2fn_null("-ol",NFILE,fnm);
  } else {
    topfile = ftp2fn_null(efTPS,NFILE,fnm);
    lowfile = opt2fn("-ol",NFILE,fnm);
  }

  if (topfile) {
    bTop = read_tps_conf(ftp2fn(efTPS,NFILE,fnm),title,&top,&xtop,NULL,topbox,
			 TRUE);
    if (bTop) {
      init_pbc(topbox);
      rm_pbc(&(top.idef),top.atoms.nr,topbox,xtop,xtop);
    }
  }

  clear_rvec(xcmtop);
  if (bFit) {
    fprintf(stderr,"Select group for least squares fit\n");
    get_index(&top.atoms,ftp2fn_null(efNDX,NFILE,fnm),1,&isize,&index,&grpname);
    /* Set the weight */
    snew(w_rls,top.atoms.nr);
    for(i=0; i<isize; i++) 
      w_rls[index[i]] = top.atoms.atom[index[i]].m;
    calc_xcm(xtop,isize,index,top.atoms.atom,xcmtop,FALSE);
    for(j=0; j<top.atoms.nr; j++)
      rvec_dec(xtop[j],xcmtop);
  }

  /* The actual filter length flen can actually be any real number */
  flen = 2*nf;
  /* nffr is the number of frames that we filter over */
  nffr = 2*nf - 1;
  snew(filt,nffr);
  sum = 0;
  for(i=0; i<nffr; i++) {
    filt[i] = cos(2*M_PI*(i - nf + 1)/(real)flen) + 1;
    sum += filt[i];
  }
  fprintf(stdout,"filter weights:");
  for(i=0; i<nffr; i++) {
    filt[i] /= sum;
    fprintf(stdout," %5.3f",filt[i]);
  }
  fprintf(stdout,"\n");
  
  snew(t,nffr);
  snew(x,nffr);
  snew(box,nffr);

  nat = read_first_x(&in,opt2fn("-f",NFILE,fnm),
		     &(t[nffr - 1]),&(x[nffr - 1]),box[nffr - 1]);
  snew(ind,nat);
  for(i=0; i<nat; i++)
    ind[i] = i;
  /* x[nffr - 1] was already allocated by read_first_x */
  for(i=0; i<nffr-1; i++)
    snew(x[i],nat);
  snew(xf,nat);
  if (lowfile)
    outl = open_trx(lowfile,"w");
  else
    outl = NULL;
  if (highfile)
    outh = open_trx(highfile,"w");
  else
    outh = NULL;

  fr = 0;
  do {
    if (bTop) {
      init_pbc(box[nffr - 1]);
      rm_pbc(&(top.idef),nat,box[nffr - 1],x[nffr - 1],x[nffr - 1]);
    }
    if (bFit) {
      calc_xcm(x[nffr - 1],isize,index,top.atoms.atom,xcm,FALSE);
      for(j=0; j<nat; j++)
	rvec_dec(x[nffr - 1][j],xcm);
      do_fit(nat,w_rls,xtop,x[nffr - 1]);
      for(j=0; j<nat; j++)
	rvec_inc(x[nffr - 1][j],xcmtop);
    }
    if (fr >= nffr && (outh || bLowAll || fr % nf == nf - 1)) {
      /* Lowpass filtering */
      for(j=0; j<nat; j++)
	clear_rvec(xf[j]);
      clear_mat(boxf);
      for(i=0; i<nffr; i++) {
	for(j=0; j<nat; j++)
	  for(d=0; d<DIM; d++)
	    xf[j][d] += filt[i]*x[i][j][d];
	for(j=0; j<DIM; j++)
	  for(d=0; d<DIM; d++)
	    boxf[j][d] += filt[i]*box[i][j][d];
      }
      if (outl && (bLowAll || fr % nf == nf - 1))
	write_trx(outl,nat,ind,topfile ? &(top.atoms) : NULL,
		  0,t[nf - 1],bFit ? topbox : boxf,xf,NULL);
      if (outh) {
	/* Highpass filtering */
	for(j=0; j<nat; j++)
	  for(d=0; d<DIM; d++)
	    xf[j][d] = xtop[j][d] + x[nf - 1][j][d] - xf[j][d];
	if (bFit)
	  for(j=0; j<nat; j++)
	    rvec_inc(xf[j],xcmtop);
	for(j=0; j<DIM; j++)
	  for(d=0; d<DIM; d++)
	    boxf[j][d] = topbox[j][d] + box[nf - 1][j][d] - boxf[j][d];
	write_trx(outh,nat,ind,topfile ? &(top.atoms) : NULL,
		  0,t[nf - 1],bFit ? topbox : boxf,xf,NULL);
      }
    }
    ptr = x[0];
    for(i=0; i<nffr-1; i++) {
      t[i] = t[i+1];
      x[i] = x[i+1];
      copy_mat(box[i+1],box[i]);
    }
    x[nffr - 1] = ptr;
    fr++;
  } while (read_next_x(in,&(t[nffr - 1]),nat,x[nffr - 1],box[nffr - 1]));
  
  if (outh)
    close_trx(outh);
  if (outl)
    close_trx(outl);
  close_trx(in);
  
  return 0;
}
