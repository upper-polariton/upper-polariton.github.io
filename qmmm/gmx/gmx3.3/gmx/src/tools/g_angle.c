/*
 * $Id: g_angle.c,v 1.27 2002/02/28 11:00:24 spoel Exp $
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
static char *SRCID_g_angle_c = "$Id: g_angle.c,v 1.27 2002/02/28 11:00:24 spoel Exp $";
#include <math.h>
#include <string.h>
#include "sysstuff.h"
#include "physics.h"
#include "typedefs.h"
#include "smalloc.h"
#include "futil.h"
#include "statutil.h"
#include "copyrite.h"
#include "vec.h"
#include "rdgroup.h"
#include "macros.h"
#include "fatal.h"
#include "xvgr.h"
#include "gstat.h"

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "g_angle computes the angle distribution for a number of angles",
    "or dihedrals. This way you can check whether your simulation",
    "is correct. With option -ov you can plot the average angle of",
    "a group of angles as a function of time. With the -all option",
    "the first graph is the average, the rest are the individual angles.[PAR]",
    "With the -of option g_angle also calculates the fraction of trans",
    "dihedrals (only for dihedrals) as function of time, but this is",
    "probably only fun for a selected few.[PAR]",
    "With option -oc a dihedral correlation function is calculated.[PAR]",
    "It should be noted that the indexfile should contain",
    "atom-triples for angles or atom-quadruplets for dihedrals.",
    "If this is not the case, the program will crash."
  };
  static char *opt[] = { NULL, "angle", "dihedral", "improper", "ryckaert-bellemans", NULL };
  static bool bALL=FALSE,bChandler=FALSE,bAverCorr=FALSE;
  static real binwidth=1;
  t_pargs pa[] = {
    { "-type", FALSE, etENUM, {opt},
      "Type of angle to analyse" },
    { "-all",    FALSE,  etBOOL, {&bALL},
      "Plot all angles separately in the averages file, in the order of appearance in the index file." },
    { "-binwidth", FALSE, etREAL, {&binwidth},
      "binwidth (degrees) for calculating the distribution" },
    { "-chandler", FALSE,  etBOOL, {&bChandler},
      "Use Chandler correlation function (N[trans] = 1, N[gauche] = 0) rather than cosine correlation function. Trans is defined as phi < -60 || phi > 60." },
    { "-avercorr", FALSE,  etBOOL, {&bAverCorr},
      "Average the correlation functions for the individual angles/dihedrals" }
  };
  static char *bugs[] = {
    "Counting transitions only works for dihedrals with multiplicity 3"
  };
  
  FILE       *out;
  real       tmp,dt;
  int        status,isize;
  atom_id    *index;
  char       *grpname;
  real       maxang,Jc,S2,norm_fac,maxstat;
  unsigned long mode;
  int        nframes,maxangstat,mult,*angstat;
  int        i,j,total,nangles,natoms,nat2,first,last,angind;
  bool       bAver,bRb,bPeriodic,
    bFrac,          /* calculate fraction too?  */
    bTrans,         /* worry about transtions too? */
    bCorr;          /* correlation function ? */    
  real       t,aa,aver,aver2,aversig,fraction;       /* fraction trans dihedrals */
  double     tfrac=0;
  char       title[256];
  real       **dih=NULL;          /* mega array with all dih. angles at all times*/
  char       buf[80];       
  real       *time,*trans_frac,*aver_angle;
  t_filenm   fnm[] = {
    { efTRX, "-f", NULL,  ffREAD  },
    { efTPX, NULL, NULL,  ffREAD  },
    { efNDX, NULL, "angle",  ffREAD  },
    { efXVG, "-od", "angdist",  ffWRITE },
    { efXVG, "-ov", "angaver",  ffOPTWR },
    { efXVG, "-of", "dihfrac",  ffOPTWR },
    { efXVG, "-ot", "dihtrans", ffOPTWR },
    { efXVG, "-oh", "trhisto",  ffOPTWR },
    { efXVG, "-oc", "dihcorr",  ffOPTWR }
  };
#define NFILE asize(fnm)
  int     npargs;
  t_pargs *ppa;
  
  CopyRight(stderr,argv[0]);
  npargs = asize(pa);
  ppa    = add_acf_pargs(&npargs,pa);
  parse_common_args(&argc,argv,PCA_CAN_VIEW | PCA_CAN_TIME | PCA_BE_NICE,
		    NFILE,fnm,npargs,ppa,asize(desc),desc,asize(bugs),bugs);
		    
  mult   = 4;
  maxang = 360.0;
  bRb    = FALSE;
  switch(opt[0][0]) {
  case 'a':
    mult   = 3;
    maxang = 180.0;
    break;
  case 'd':
    break;
  case 'i':
    break;
  case 'r':
    bRb = TRUE;
    break;
  }

  /* Calculate bin size */
  maxangstat=(int)(maxang/binwidth+0.5);
  binwidth=maxang/maxangstat;
    
  rd_index(ftp2fn(efNDX,NFILE,fnm),1,&isize,&index,&grpname);
  nangles=isize/mult;
  if ((isize % mult) != 0) 
    fatal_error(0,"number of index elements not multiple of %d, "
		"these can not be %s\n",
		mult,(mult==3) ? "angle triplets" : "dihedral quadruplets");
  

  /* Check whether specific analysis has to be performed */
  bCorr=opt2bSet("-oc",NFILE,fnm);
  bAver=opt2bSet("-ov",NFILE,fnm);
  bTrans=opt2bSet("-ot",NFILE,fnm);
  bFrac=opt2bSet("-of",NFILE,fnm);

  if (bChandler && !bCorr)
    bCorr=TRUE;
    
  if (bFrac && !bRb) {
    fprintf(stderr,"Warning:"
	    " calculating fractions as defined in this program\n"
	    "makes sense for Ryckaert Bellemans dihs. only. Ignoring -of\n\n"); 
    bFrac = FALSE;
  }
  
  if ( (bTrans || bFrac || bCorr) && mult==3)
    fatal_error(0,"Can only do transition, fraction or correlation\n"
		"on dihedrals. Select -d\n");
  
  /* 
   * We need to know the nr of frames so we can allocate memory for an array 
   * with all dihedral angles at all timesteps. Works for me.
   */
  if (bTrans || bCorr  || bALL)
    snew(dih,nangles);
  
  snew(angstat,maxangstat);

  read_ang_dih(ftp2fn(efTRX,NFILE,fnm),ftp2fn(efTPX,NFILE,fnm),(mult == 3),
	       bALL || bCorr || bTrans,bRb,maxangstat,angstat,
	       &nframes,&time,isize,index,&trans_frac,&aver_angle,dih);
  
  dt=(time[nframes-1]-time[0])/(nframes-1);
  
  if (bAver) {
    sprintf(title,"Average Angle: %s",grpname);
    out=xvgropen(opt2fn("-ov",NFILE,fnm),
		 title,"Time (ps)","Angle (degrees)");
    for(i=0; (i<nframes); i++) {
      fprintf(out,"%10.5f  %8.3f",time[i],aver_angle[i]*RAD2DEG);
      if (bALL)
	for(j=0; (j<nangles); j++)
	  fprintf(out,"  %8.3f",dih[j][i]*RAD2DEG);
      fprintf(out,"\n");
    }	
    fclose(out);
  }
  
  if (bFrac) {
    sprintf(title,"Trans fraction: %s",grpname);
    out=xvgropen(opt2fn("-of",NFILE,fnm),
		  title,"Time (ps)","Fraction");
    tfrac = 0.0;
    for(i=0; (i<nframes); i++) {
      fprintf(out,"%10.5f  %10.3f\n",time[i],trans_frac[i]);
      tfrac += trans_frac[i];
    }
    fclose(out);
    
    tfrac/=nframes;
    fprintf(stderr,"Average trans fraction: %g\n",tfrac);
  }
  sfree(trans_frac);
  
  if (bTrans) 
    ana_dih_trans(opt2fn("-ot",NFILE,fnm),opt2fn("-oh",NFILE,fnm),
		  dih,nframes,nangles,grpname,time[0],dt,bRb);
		  
  if (bCorr) {
    /* Autocorrelation function */
    if (nframes < 2)
      fprintf(stderr,"Not enough frames for correlation function\n");
    else {
      
      if (bChandler) {
	real dval,sixty=DEG2RAD*60;
	bool bTest;

	for(i=0; (i<nangles); i++)
	  for(j=0; (j<nframes); j++) {
	    dval = dih[i][j];
	    if (bRb)
	      bTest=(dval > -sixty) && (dval < sixty);
	    else
	      bTest=(dval < -sixty) || (dval > sixty);
	    if (bTest)
	      dih[i][j] = dval-tfrac;
	    else
	      dih[i][j] = -tfrac;
	  }
      }
      if (bChandler)
	mode = eacNormal;
      else
	mode = eacCos;
      do_autocorr(opt2fn("-oc",NFILE,fnm),"Dihedral Autocorrelation Function",
		  nframes,nangles,dih,dt,mode,bAverCorr);
    }
  }

  
  /* Determine the non-zero part of the distribution */
  for(first=0; (first < maxangstat-1) && (angstat[first+1] == 0); first++)
    ;
  for(last=maxangstat-1; (last > 0) && (angstat[last-1] == 0) ; last--)
    ;

  aver=aver2=0;
  for(i=0; (i<nframes); i++) {
    aver  += RAD2DEG*aver_angle[i];
    aver2 += sqr(RAD2DEG*aver_angle[i]);
  }
  aver   /= (real) nframes;
  aver2  /= (real) nframes;
  aversig = sqrt(aver2-sqr(aver));
  printf("Found points in the range from %d to %d (max %d)\n",
	 first,last,maxangstat);
  printf(" < angle >  = %g\n",aver);
  printf("< angle^2 > = %g\n",aver2);
  printf("Std. Dev.   = %g\n",aversig);
    
  if (mult == 3)
    sprintf(title,"Angle Distribution: %s",grpname);
  else {
    sprintf(title,"Dihedral Distribution: %s",grpname);
    
    calc_distribution_props(maxangstat,angstat,-180.0,0,NULL,&S2);
    fprintf(stderr,"Order parameter S^2 = %g\n",S2);
  }
  
  bPeriodic=(mult==4) && (first==0) && (last==maxangstat-1);
  
  out=xvgropen(opt2fn("-od",NFILE,fnm),title,"Degrees","");
  fprintf(out,"@    subtitle \"average angle: %g\\So\\N\"\n",aver*RAD2DEG);
  norm_fac=1.0/(nangles*nframes*binwidth);
  if (bPeriodic) {
    maxstat=0;
    for(i=first; (i<=last); i++) 
      maxstat=max(maxstat,angstat[i]*norm_fac);
    fprintf(out,"@with g0\n");
    fprintf(out,"@    world xmin -180\n");
    fprintf(out,"@    world xmax  180\n");
    fprintf(out,"@    world ymin 0\n");
    fprintf(out,"@    world ymax %g\n",maxstat*1.05);
    fprintf(out,"@    xaxis  tick major 60\n");
    fprintf(out,"@    xaxis  tick minor 30\n");
    fprintf(out,"@    yaxis  tick major 0.005\n");
    fprintf(out,"@    yaxis  tick minor 0.0025\n");
  }
  for(i=first; (i<=last); i++) 
    fprintf(out,"%10g  %10f\n",i*binwidth+180.0-maxang,angstat[i]*norm_fac);
  if ( bPeriodic )
    /* print first bin again as last one */
    fprintf(out,"%10g  %10f\n",180.0,angstat[0]*norm_fac);
  
  fclose(out);

  do_view(opt2fn("-od",NFILE,fnm),NULL);
  if (bAver)
    do_view(opt2fn("-ov",NFILE,fnm),"-nxy");
    
  thanx(stderr);
    
  return 0;
}
