/*
 * $Id: g_dielectric.c,v 1.26 2002/04/01 21:06:06 lindahl Exp $
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
static char *SRCID_g_dielectric_c = "$Id: g_dielectric.c,v 1.26 2002/04/01 21:06:06 lindahl Exp $";
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include "copyrite.h"
#include "typedefs.h"
#include "string2.h"
#include "gstat.h"
#include "smalloc.h"
#include "futil.h"
#include "macros.h"
#include "maths.h"
#include "xvgr.h"
#include "gmxcomplex.h"
#include "nr.h"

/* Determines at which point in the array the fit should start */
int calc_nbegin(int nx,real x[],real tbegin)
{
  int  nbegin;
  real dt,dtt;
  
  /* Assume input x is sorted */  
  for(nbegin=0; (nbegin < nx) && (x[nbegin] <= tbegin); nbegin++)
    ;
  if ((nbegin == nx) || (nbegin == 0))
    fatal_error(0,"Begin time %f not in x-domain [%f through %f]\n",
		tbegin,x[0],x[nx-1]);

  /* Take the one closest to tbegin */
  if (fabs(x[nbegin]-tbegin) > fabs(x[nbegin-1]-tbegin))
    nbegin--;

  printf("nbegin = %d, x[nbegin] = %g, tbegin = %g\n",
	  nbegin,x[nbegin],tbegin);
      
  return nbegin;
}

real numerical_deriv(int nx,real x[],real y[],real fity[],real combined[],real dy[],
		     real tendInt,int nsmooth)
{
  FILE *tmpfp;
  int  i,nbegin,i0,i1;
  real fac,fx,fy,integralSmth;
  
  nbegin = calc_nbegin(nx,x,tendInt);
  if (nsmooth == 0) {
    for(i=0; (i<nbegin); i++)
      combined[i]=y[i];
    fac = y[nbegin]/fity[nbegin];
    printf("scaling fitted curve by %g\n",fac);
    for(i=nbegin; (i<nx); i++)
      combined[i]=fity[i]*fac;
  }
  else {
    i0 = max(0,nbegin);
    i1 = min(nx-1,nbegin+nsmooth);
    printf("Making smooth transition from %d thru %d\n",i0,i1);
    for(i=0; (i<i0); i++)
      combined[i]=y[i];
    for(i=i0; (i<=i1); i++) {
      fx = (i1-i)/(real)(i1-i0);
      fy = (i-i0)/(real)(i1-i0);
      if (debug)
	fprintf(debug,"x: %g factors for smoothing: %10g %10g\n",x[i],fx,fy);
      combined[i] = fx*y[i] + fy*fity[i];
    } 
    for(i=i1+1; (i<nx); i++)
      combined[i]=fity[i];
  }
  
  tmpfp = ffopen("integral_smth.xvg","w");
  integralSmth=print_and_integrate(tmpfp,nx,x[1]-x[0],combined,NULL,1);
  printf("SMOOTH integral = %10.5e\n",integralSmth);

  dy[0] = (combined[1]-combined[0])/(x[1]-x[0]);
  for(i=1; (i<nx-1); i++) {
    dy[i] = (combined[i+1]-combined[i-1])/(x[i+1]-x[i-1]);
  }
  dy[nx-1] = (combined[nx-1]-combined[nx-2])/(x[nx-1]-x[nx-2]);
  
  for(i=0; (i<nx); i++)
    dy[i] *= -1;
    
  return integralSmth;
}

void do_four(char *fn,char *cn,int nx,real x[],real dy[],real eps0,real epsRF)
{
  FILE      *fp,*cp;
  t_complex *tmp,gw,hw,kw;
  int       i,nnx,nxsav;
  real      fac,nu,dt,*ptr,maxeps,numax;
  
  nxsav = nx;
  /*while ((dy[nx-1] == 0.0) && (nx > 0))
    nx--;*/
  if (nx == 0) 
    fatal_error(0,"Empty dataset in %s, line %d!",__FILE__,__LINE__);

  nnx=1;
  while (nnx < 2*nx) {
    nnx*=2;
  }
  snew(tmp,2*nnx);
  printf("Doing FFT of %d points\n",nnx);
  for(i=0; (i<nx); i++)
    tmp[i].re = dy[i];
  ptr=&tmp[0].re;
  four1(ptr-1,nnx,-1);
  
  dt=x[1]-x[0];
  if (epsRF == 0)
    fac = (eps0-1)/tmp[0].re;
  else
    fac=((eps0-1)/(2*epsRF+eps0))/tmp[0].re;  
  fp=xvgropen(fn,"Epsilon(\\8w\\4)","Freq. (GHz)","eps");
  cp=xvgropen(cn,"Cole-Cole plot","Eps'","Eps''");
  maxeps = 0;
  numax  = 0;
  for(i=0; (i<nxsav); i++) {
    if (epsRF == 0) {
      kw.re = 1+fac*tmp[i].re;
      kw.im = 1+fac*tmp[i].im;
    } 
    else {
      gw     = rcmul(fac,tmp[i]);
      hw     = rcmul(2*epsRF,gw);
      hw.re += 1.0;
      gw.re  = 1.0 - gw.re;
      gw.im  = -gw.im;
      kw     = cdiv(hw,gw);
    }
    kw.im *= -1;
    
    nu     = (i+1)*1000.0/(nnx*dt);
    if (kw.im > maxeps) {
      maxeps = kw.im;
      numax  = nu;
    }
    
    fprintf(fp,"%10.5e  %10.5e  %10.5e\n",nu,kw.re,kw.im);
    fprintf(cp,"%10.5e  %10.5e\n",kw.re,kw.im);
  }
  printf("MAXEPS = %10.5e at frequency %10.5e GHz (tauD = %8.1f ps)\n",
	  maxeps,numax,1000/(2*M_PI*numax));
  fclose(fp);
  fclose(cp);
  sfree(tmp);
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "dielectric calculates frequency dependent dielectric constants",
    "from the autocorrelation function of the total dipole moment in",
    "your simulation. This ACF can be generated by g_dipoles.",
    "For an estimate of the error you can run g_statistics on the",
    "ACF, and use the output thus generated for this program.",
    "The functional forms of the available functions are:[PAR]",
    "One parmeter  : y = Exp[-a1 x]",
    "Two parmeters : y = a2 Exp[-a1 x]",
    "Three parmeter: y = a2 Exp[-a1 x] + (1 - a2) Exp[-a3 x]",
    "Startvalues for the fit procedure can be given on the commandline.",
    "It is also possible to fix parameters at their start value, use -fix",
    "with the number of the parameter you want to fix.",
    "[PAR]",
    "Three output files are generated, the first contains the ACF,",
    "an exponential fit to it with 1, 2 or 3 parameters, and the",
    "numerical derivative of the combination data/fit.",
    "The second file contains the real and imaginary parts of the",
    "frequency-dependent dielectric constant, the last gives a plot",
    "known as the Cole-Cole plot, in which the  imaginary",
    "component is plotted as a function of the real component.",
    "For a pure exponential relaxation (Debye relaxation) the latter",
    "plot should be one half of a circle"
  };
  t_filenm fnm[] = {
    { efXVG, "-f", "Mtot",  ffREAD  },
    { efXVG, "-d", "deriv",  ffWRITE },
    { efXVG, "-o", "epsw",   ffWRITE },
    { efXVG, "-c", "cole",   ffWRITE }
  };
#define NFILE asize(fnm)
  int  i,j,nx,ny,nxtail,eFitFn,nfitparm;
  real **y,dt,integral,fitintegral,*fitparms,fac,rffac;
  char *legend[] = { "Correlation", "Std. Dev.", "Fit", "Combined", "Derivative" };
  static int fix=0,bFour = 0,bX = 1,nsmooth=3;
  static real tendInt=5.0,tbegin=5.0,tend=500.0;
  static real A=0.5,tau1=10.0,tau2=1.0,eps0=80,epsRF=78.5,tail=500.0;
  real   lambda;
  t_pargs pa[] = {
    { "-fft", FALSE, etBOOL, {&bFour},
      "use fast fourier transform for correlation function" },
    { "-x1",  FALSE, etBOOL, {&bX},
      "use first column as X axis rather than first data set" },
    { "-eint", FALSE, etREAL, {&tendInt},
      "Time were to end the integration of the data and start to use the fit"},
    { "-bfit", FALSE, etREAL, {&tbegin},
      "Begin time of fit" },
    { "-efit", FALSE, etREAL, {&tend},
      "End time of fit" },
    { "-tail", FALSE, etREAL, {&tail},
      "Length of function including data and tail from fit" },
    { "-A", FALSE, etREAL, {&A},
      "Start value for fit parameter A" },
    { "-tau1", FALSE, etREAL, {&tau1},
      "Start value for fit parameter tau1" },
    { "-tau2", FALSE, etREAL, {&tau2},
      "Start value for fit parameter tau2" },
    { "-eps0", FALSE, etREAL, {&eps0},
      "Epsilon 0 of your liquid" },
    { "-epsRF", FALSE, etREAL, {&epsRF},
      "Epsilon of the reaction field used in your simulation. A value of 0 means infinity." },
    { "-fix", FALSE, etINT,  {&fix},
      "Fix parameters at their start values, A (2), tau1 (1), or tau2 (4)" },
    { "-ffn",    FALSE, etENUM, {s_ffn},
      "Fit function" },
    { "-nsmooth", FALSE, etINT, {&nsmooth},
      "Number of points for smoothing" }
  };
  
  CopyRight(stderr,argv[0]);
  parse_common_args(&argc,argv,PCA_CAN_TIME | PCA_CAN_VIEW | PCA_BE_NICE,
		    NFILE,fnm,asize(pa),pa,asize(desc),desc,0,NULL);
  please_cite(stdout,"Spoel98a");
  
  nx     = read_xvg(opt2fn("-f",NFILE,fnm),&y,&ny);
  dt     = y[0][1]-y[0][0];
  nxtail = min(tail/dt,nx);
  
  printf("Read data set containing %d colums and %d rows\n",ny,nx);
  printf("Assuming (from data) that timestep is %g, nxtail = %d\n",
	  dt,nxtail);
  if (nxtail > nx) {
    for(i=0; (i<ny); i++)
      srenew(y[i],nxtail);
    for(i=nx; (i<nxtail); i++) {
      y[0][i] = dt*i+y[0][0];
      for(j=1; (j<ny); j++)
	y[j][i] = 0.0;
    }
    nx=nxtail;
  }
  
  /* We have read a file WITHOUT standard deviations, so we make our own... */
  if (ny==2) {
    printf("Creating standard deviation numbers ...\n");
    srenew(y,3);
    snew(y[2],nx);

    fac=1.0/((real)nx);
    for(i=0; (i<nx); i++) 
      y[2][i] = fac;
  }

  eFitFn = sffn2effn(s_ffn);
  nfitparm = nfp_ffn[eFitFn];
  snew(fitparms,4);
  fitparms[0]=tau1;
  if (nfitparm > 1)
    fitparms[1]=A;
  if (nfitparm > 2)
    fitparms[2]=tau2;  
  
  if (ny < 6) {
    srenew(y,6);
    snew(y[3],nx);
    snew(y[4],nx);
    snew(y[5],nx);
  } 
  integral = print_and_integrate(NULL,calc_nbegin(nx,y[0],tbegin),
				 dt,y[1],NULL,1);
  integral += do_lmfit(nx,y[1],y[2],dt,y[0],tbegin,tend,
		       TRUE,eFitFn,fitparms,fix);
  for(i=0; i<nx; i++)
    y[3][i] = fit_function(eFitFn,fitparms,y[0][i]);

  if (epsRF == 0) {
    /* This means infinity! */
    lambda = 0;
    rffac  = 1;
  }
  else {
    lambda = (eps0 - 1.0)/(2*epsRF - 1.0);
    rffac  = (2*epsRF+eps0)/(2*epsRF+1);
  }
  printf("DATA INTEGRAL: %5.1f, tauD(old) = %5.1f ps, "
	  "tau_slope = %5.1f, tau_slope,D = %5.1f ps\n",
	  integral,integral*rffac,fitparms[0],fitparms[0]*rffac);

  printf("tau_D from tau1 = %8.3g , eps(Infty) = %8.3f\n",
	  fitparms[0]*(1 + fitparms[1]*lambda),
	  1 + ((1 - fitparms[1])*(eps0 - 1))/(1 + fitparms[1]*lambda));

  fitintegral=numerical_deriv(nx,y[0],y[1],y[3],y[4],y[5],tendInt,nsmooth);
  printf("FIT INTEGRAL (tau_M): %5.1f, tau_D = %5.1f\n",
	  fitintegral,fitintegral*rffac);
	  
  /* Now we have the negative gradient of <Phi(0) Phi(t)> */
  write_xvg(opt2fn("-d",NFILE,fnm),"Data",nx-1,6,y,legend);
  
  /* Do FFT and analysis */  
  do_four(opt2fn("-o",NFILE,fnm),opt2fn("-c",NFILE,fnm),
	  nx-1,y[0],y[5],eps0,epsRF);

  do_view(opt2fn("-o",NFILE,fnm),"-nxy");
  do_view(opt2fn("-c",NFILE,fnm),NULL);
  do_view(opt2fn("-d",NFILE,fnm),"-nxy");
	    
  thanx(stderr);

  return 0;
}
