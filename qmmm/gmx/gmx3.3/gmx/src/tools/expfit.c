/*
 * $Id: expfit.c,v 1.23 2002/05/23 15:10:35 spoel Exp $
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
static char *SRCID_expfit_c = "$Id: expfit.c,v 1.23 2002/05/23 15:10:35 spoel Exp $";
#include <sysstuff.h>
#include <string.h>
#include <math.h>
#include "typedefs.h"
#include "smalloc.h"
#include "xvgr.h"
#include "futil.h"
#include "gstat.h"
#include "vec.h"
#include "statutil.h"
#include "rdgroup.h"

int  nfp_ffn[effnNR] = { 0, 1, 2, 3, 2, 4, 7, 3 };

char *s_ffn[effnNR+2] = { NULL, "none", "exp", "aexp", "exp_exp", "vac", 
			  "exp5", "exp7", NULL, NULL };
/* We don't allow errest as a choice on the command line */

char *longs_ffn[effnNR] = {
  "no fit",
  "y = exp(-a1 x)",
  "y = a2 exp(-x/a1)",
  "y = a2 exp(-x/a1) + (1-a2) exp(-x/a3)",
  "y = exp(-v) (cosh(wv) + 1/w sinh(wv)), v = x/(2 a1), w = sqrt(1 - a2)",
  "y = a1 exp(-x/a2) +  a3 exp(-x/a4) + a5",
  "y = a1 exp(-x/a2) +  a3 exp(-x/a4) + a5 exp(-x/a6) + a7",
  "y = sqrt(a2*ee(a1,x) + (1-a2)*ee(a2,x))"
};

extern bool mrqmin(real x[],real y[],real sig[],int ndata,real a[],
		   int ma,int lista[],int mfit,real **covar,real **alpha,
		   real *chisq,
		   void (*funcs)(real x,real a[],real *y,real dyda[]),
		   real *alamda);

extern bool mrqmin_new(real x[],real y[],real sig[],int ndata,real a[], 
		       int ia[],int ma,real **covar,real **alpha,real *chisq, 
		       void (*funcs)(real, real [], real *, real []), 
		       real *alamda);
		       
static real myexp(real x,real A,real tau)
{
  if ((A == 0) || (tau == 0))
    return 0;
  return A*exp(-x/tau);
}
		   
static void exp_one_parm(real x,real a[],real *y,real dyda[])
{
  /* Fit to function 
   *
   * y = exp(-a1 x)
   *
   */
   
  real e1;
  
  e1      = exp(-x/a[1]);
  *y      = e1;
  dyda[1] = x*e1/(a[1]*a[1]);
}

static void exp_two_parm(real x,real a[],real *y,real dyda[])
{
  /* Fit to function 
   *
   * y = a2 exp(-x/a1)
   *
   */
   
  real e1;
  
  e1      = exp(-x/a[1]);
  *y      = a[2]*e1;
  dyda[1] = x*a[2]*e1/(a[1]*a[1]);
  dyda[2] = e1;
}

static void exp_3_parm(real x,real a[],real *y,real dyda[])
{
  /* Fit to function 
   *
   * y = a2 exp(-x/a1) + (1-a2) exp(-x/a3)
   *
   */
   
  real e1,e2;
  
  e1      = exp(-x/a[1]);
  e2      = exp(-x/a[3]);
  *y      = a[2]*e1 + (1-a[2])*e2;
  dyda[1] = x*a[2]*e1/(a[1]*a[1]);
  dyda[2] = e1-e2;
  dyda[3] = x*(1-a[2])*e2/(a[3]*a[3]);
  /* fprintf(stderr,"exp3: x=%10.3e *y=%10.3e dyda=%10.3e %10.3e %10.3e\n",
    x,*y,dyda[1],dyda[2],dyda[3]);  */
}

static void exp_5_parm(real x,real a[],real *y,real dyda[])
{
  /* Fit to function 
   *
   * y = a1 exp(-x/a2) + a3 exp(-x/a4) + a5
   *
   */
   
  real e1,e2;

  e1      = exp(-x/a[2]);
  e2      = exp(-x/a[4]);
  *y      = a[1]*e1 + a[3]*e2 + a[5];

  if (debug)
    fprintf(debug,"exp_5_parm called: x = %10.3f  y = %10.3f\n"
	    "a = ( %8.3f  %8.3f  %8.3f  %8.3f  %8.3f)\n",
	    x,*y,a[1],a[2],a[3],a[4],a[5]);
  dyda[1] = e1;
  dyda[2] = -(x*e1)/sqr(a[2]);
  dyda[3] = e2;
  dyda[4] = -(x*e2)/sqr(a[4]);
  /* dyda[5] = 0;*/
}

static void exp_7_parm(real x,real a[],real *y,real dyda[])
{
  /* Fit to function 
   *
   * y = a1 exp(-x/a2) + a3 exp(-x/a4) + a5 exp(-x/a6) + a7
   *
   */
   
  real e1,e2,e3;
  
  e1      = exp(-x/a[2]);
  e2      = exp(-x/a[4]);
  e3      = exp(-x/a[6]);
  *y      = a[1]*e1 + a[3]*e2 + a[5]*e3 + a[7];

  dyda[1] = e1;
  dyda[2] = -(x*e1)/sqr(a[2]);
  dyda[3] = e2;
  dyda[4] = -(x*e2)/sqr(a[4]);
  dyda[5] = e3;
  dyda[6] = -(x*e3)/sqr(a[6]);
  dyda[7] = 0;
}

static void vac_2_parm(real x,real a[],real *y,real dyda[])
{
  /* Fit to function 
   *
   * y = 1/2 (1 - 1/w) exp(-(1+w)v) + 1/2 (1 + 1/w) exp(-(1-w)v)
   *
   *   = exp(-v) (cosh(wv) + 1/w sinh(wv))
   *
   *    v = x/(2 a1)
   *    w = sqrt(1 - a2)
   *
   *    For tranverse current autocorrelation functions:
   *       a1 = tau
   *       a2 = 4 tau (eta/rho) k^2
   *
   */
   
  double v,det,omega,omega2,invom,em,ec,es;
  
  v   = x/(2*a[1]);
  det = 1 - a[2];
  em  = exp(-v);
  if (det != 0) {
    omega2  = fabs(det);
    omega   = sqrt(omega2);
    if (det > 0) {
      ec = em*0.5*(exp(omega*v)+exp(-omega*v));
      es = em*0.5*(exp(omega*v)-exp(-omega*v))/omega;
    } else {
      ec = em*cos(omega*v);
      es = em*sin(omega*v)/omega;
    }
    *y      = ec + es;
    dyda[2] = (v/det*ec+(v-1/det)*es)/(-2.0);
    dyda[1] = (1-det)*v/a[1]*es;
  } else {
    *y      = (1+v)*em;
    dyda[2] = -v*v*em*(0.5+v/6);
    dyda[1] = v*v/a[1]*em;
  }
}

static void errest_3_parm(real x,real a[],real *y,real dyda[])
{
  real e1,e2,v1,v2;  

  if (a[1])
    e1 = exp(-x/a[1]) - 1;
  else
    e1 = 0;
  if (a[3])
    e2 = exp(-x/a[3]) - 1;
  else
    e2 = 0;

  if (x > 0) {
    v1 = 2*a[1]*(e1*a[1]/x + 1);
    v2 = 2*a[3]*(e2*a[3]/x + 1);
    *y      = sqrt(a[2]*v1 + (1-a[2])*v2);
    dyda[1] = (v1/a[1] + e1)/(*y);
    dyda[3] = (v2/a[3] + e2)/(*y);
    dyda[2] = (v1 - v2)/(2*(*y));
  } else {
    *y      = 0;
    dyda[1] = 0;
    dyda[3] = 0;
    dyda[2] = 0;
  }
}

typedef void (*myfitfn)(real x,real a[],real *y,real dyda[]);
myfitfn mfitfn[effnNR] = { 
  exp_one_parm, exp_one_parm, exp_two_parm, exp_3_parm, vac_2_parm,
  exp_5_parm,   exp_7_parm,   errest_3_parm 
};

real fit_function(int eFitFn,real *parm,real x)
{
  static real y,dum[4];

  mfitfn[eFitFn](x,parm-1,&y,dum);

  return y;
}

/* lmfit_exp supports up to 3 parameter fitting of exponential functions */
static bool lmfit_exp(int nfit,real x[],real y[],real dy[],real ftol,
		      real parm[],real dparm[],bool bVerbose,
		      int eFitFn,int fix)
{
  real chisq,ochisq,alamda;
  real *a,**covar,**alpha,*dum;
  bool bCont;
  int  i,j,ma,mfit,*lista,*ia;

  if ((eFitFn < 0) || (eFitFn >= effnNR))
    fatal_error(0,"fitfn = %d, should be in 0..%d (%s,%d)",
		effnNR-1,eFitFn,__FILE__,__LINE__);

  ma=mfit=nfp_ffn[eFitFn];         /* number of parameters to fit */
  snew(a,ma+1);
  snew(covar,ma+1);
  snew(alpha,ma+1);
  snew(lista,ma+1);
  snew(ia,ma+1);
  snew(dum,ma+1);
  for(i=1; (i<ma+1); i++) {
    lista[i] = i;
    ia[i] = i;
    snew(covar[i],ma+1);
    snew(alpha[i],ma+1);
  }
  if (fix) {
    if (bVerbose)
      fprintf(stderr,"Will keep parameters fixed during fit procedure: %d\n",
	      fix);
    for(i=0; i<ma; i++)
      if (fix & 1<<i)
	ia[i+1] = 0;
  }
  if (debug)
    fprintf(debug,"%d parameter fit\n",mfit);

  /* Initial params */
  alamda = -1;    /* Starting value   */
  chisq  = 1e12;
  for(i=0; (i<mfit); i++)
    a[i+1] = parm[i];

  j = 0;      
  if (bVerbose)
    fprintf(stderr,"%4s  %10s  %10s  %10s  %10s  %10s\n",
	    "Step","chi^2","Lambda","A1","A2","A3");
  do {
    ochisq = chisq;
    /* mrqmin(x-1,y-1,dy-1,nfit,a,ma,lista,mfit,covar,alpha,
     *   &chisq,expfn[mfit-1],&alamda)
     */
    if (!mrqmin_new(x-1,y-1,dy-1,nfit,a,ia,ma,covar,alpha,&chisq,
		    mfitfn[eFitFn],&alamda))
      return FALSE;
     
    if (bVerbose) {
      fprintf(stderr,"%4d  %10.5e  %10.5e  %10.5e",
	      j,chisq,alamda,a[1]);
      if (mfit > 1)
	fprintf(stderr,"  %10.5e",a[2]);
      if (mfit > 2)
	fprintf(stderr,"  %10.5e",a[3]);
      fprintf(stderr,"\n");
    }
    j++;
    bCont = ((fabs(ochisq - chisq) > fabs(ftol*chisq)) ||
	     ((ochisq == chisq)));
  } while (bCont && (alamda != 0.0) && (j < 50));
  if (bVerbose)
    fprintf(stderr,"\n");
    
  /* Now get the covariance matrix out */
  alamda = 0;

  /*  mrqmin(x-1,y-1,dy-1,nfit,a,ma,lista,mfit,covar,alpha,
   * &chisq,expfn[mfit-1],&alamda)
   */
  if ( !mrqmin_new(x-1,y-1,dy-1,nfit,a,ia,ma,covar,alpha,&chisq,
		   mfitfn[eFitFn],&alamda))
    return FALSE;

  for(j=0; (j<mfit); j++) {
    parm[j]  = a[j+1];
    dparm[j] = covar[j+1][j+1];
  }

  for(i=0; (i<ma+1); i++) {
    sfree(covar[i]);
    sfree(alpha[i]);
  }
  sfree(a);
  sfree(covar);
  sfree(alpha);
  sfree(lista);
  sfree(dum);
  
  return TRUE;
}

real do_lmfit(int ndata,real c1[],real sig[],real dt,real x0[],
	      real begintimefit,real endtimefit,bool bVerbose,
	      int eFitFn,real fitparms[],int fix)
{
  FILE *fp;
  char buf[32];

  int  i,j,nparm,nfitpnts;
  real integral,ttt;
  real *parm,*dparm;
  real *x,*y,*dy;
  real ftol = 1e-4;

  nparm = nfp_ffn[eFitFn];
  if (debug) {
    fprintf(debug,"There are %d points to fit %d vars!\n",ndata,nparm);
    fprintf(debug,"Fit from %g thru %g, dt=%g\n",
	    begintimefit,endtimefit,dt);
  }

  snew(x,ndata);
  snew(y,ndata);
  snew(dy,ndata);

  j=0;
  for(i=0; i<ndata; i++) {
    ttt = x0 ? x0[i] : dt*i;
    if (ttt>=begintimefit && ttt<=endtimefit) {
      x[j] = ttt;
      y[j] = c1[i];

      /* mrqmin does not like sig to be zero */
      if (sig[i]<1.0e-7)
	dy[j]=1.0e-7;
      else
	dy[j]=sig[i];
      if (debug)
	fprintf(debug,"j= %d, i= %d, x= %g, y= %g, dy= %g\n",
		j,i,x[j],y[j],dy[j]);
      j++;
    }
  }
  nfitpnts = j;
  integral = 0;
  if (nfitpnts < nparm) 
    fprintf(stderr,"Not enough data points for fitting!\n");
  else {
    snew(parm,nparm);
    snew(dparm,nparm);
    if (fitparms)
      for(i=0; (i < nparm); i++)
	parm[i]=fitparms[i];
    
    if (!lmfit_exp(nfitpnts,x,y,dy,ftol,parm,dparm,bVerbose,eFitFn,fix))
      fprintf(stderr,"Fit failed!\n");
    else if (nparm <= 3) {
      /* Compute the integral from begintimefit */
      if (nparm == 3) 
	integral=(parm[0]*myexp(begintimefit,parm[1],  parm[0]) +
		  parm[2]*myexp(begintimefit,1-parm[1],parm[2]));
      else if (nparm == 2)
	integral=parm[0]*myexp(begintimefit,parm[1],  parm[0]);
      else if (nparm == 1)
	integral=parm[0]*myexp(begintimefit,1,  parm[0]);
      else
	fatal_error(0,"nparm = %d in file %s, line %d",
		    nparm,__FILE__,__LINE__);
      
      /* Generate THE output */
      if (bVerbose) {
	fprintf(stderr,"FIT: # points used in fit is: %d\n",nfitpnts);
	fprintf(stderr,"FIT: %21s%21s%21s\n",
		"parm0     ","parm1 (ps)   ","parm2 (ps)    ");
	fprintf(stderr,"FIT: ------------------------------------------------------------\n");
	fprintf(stderr,"FIT: %8.3g +/- %8.3g%9.4g +/- %8.3g%8.3g +/- %8.3g\n",
		parm[0],dparm[0],parm[1],dparm[1],parm[2],dparm[2]);
	fprintf(stderr,"FIT: Integral (calc with fitted function) from %g ps to inf. is: %g\n",
		begintimefit,integral);
	
	sprintf(buf,"test%d.xvg",nfitpnts);
	fp = xvgropen(buf,"C(t) + Fit to C(t)","Time (ps)","C(t)");
	fprintf(fp,"# parm0 = %g, parm1 = %g, parm2 = %g\n",
		parm[0],parm[1],parm[2]);
	for(j=0; (j<nfitpnts); j++) {
	  ttt = x0 ? x0[j] : dt*j;
	  fprintf(fp,"%10.5e  %10.5e  %10.5e\n",
		  ttt,c1[j],fit_function(eFitFn,parm,ttt));
	}
	fclose(fp);
      }
    }
    for(i=0;(i<nparm);i++)
      fitparms[i] = parm[i];
    sfree(parm);
    sfree(dparm);
  }
  
  sfree(x);
  sfree(y);
  sfree(dy);
  
  return integral;
}

void do_expfit(int ndata,real c1[],real dt,real begintimefit,real endtimefit)
{
  int i,n;
  real *x,*y,*Dy;
  real aa,bb,saa,sbb,A,tau,dA,dtau;

  fprintf(stderr,"Will fit data from %g (ps) to %g (ps).\n",
	  begintimefit,endtimefit);

  snew(x,ndata);   /* allocate the maximum necessary space */
  snew(y,ndata);
  snew(Dy,ndata);
  n=0;

  for(i=0; (i<ndata); i++) {
    if ( (dt*i >= begintimefit) && (dt*i <= endtimefit) ) {
      x[n]=dt*i;
      y[n]=c1[i];
      Dy[n]=0.5;
      fprintf(stderr,"n= %d, i= %d, x= %g, y= %g\n",n,i,x[n],y[n]);
      n++;
    }
  }
  fprintf(stderr,"# of data points used in the fit is : %d\n\n",n);
  expfit(n,x,y,Dy,&aa,&saa,&bb,&sbb);

  A=exp(aa);
  dA=exp(aa)*saa;
  tau=-1.0/bb;
  dtau=sbb/sqr(bb);
  fprintf(stderr,"Fitted to y=exp(a+bx):\n");
  fprintf(stderr,"a = %10.5f\t b = %10.5f",aa,bb);
  fprintf(stderr,"\n");
  fprintf(stderr,"Fitted to y=Aexp(-x/tau):\n");
  fprintf(stderr,"A  = %10.5f\t tau  = %10.5f\n",A,tau);
  fprintf(stderr,"dA = %10.5f\t dtau = %10.5f\n",dA,dtau);
}


void expfit(int n, real *x, real *y, real *Dy, real *a, real *sa, 
	    real *b, real *sb)
{
  real *w,*ly,A,SA,B,SB;
  int  i;
  real sum,xbar,ybar,Sxx,Sxy,wr2,chi2,gamma,Db;
  
#define ZERO 0.0
#define ONE 1.0
#define ONEP5 1.5
#define TWO 2.0
  
#define sqr(x) ((x)*(x))

  /*allocate memory */
  snew(w,n);
  snew(ly,n);

  /* Calculate weights and values of ln(y). */
  for(i=0;(i<n); i++){
    w[i]=sqr(y[i]/Dy[i]);
    ly[i]=log(y[i]);
  }
  
  /* The weighted averages of x and y: xbar and ybar. */
  sum=ZERO;
  xbar=ZERO;
  ybar=ZERO;
  for(i=0;(i<n);i++){
    sum+=w[i];
    xbar+=w[i]*x[i];
    ybar+=w[i]*ly[i];
  }
  xbar/=sum;
  ybar/=sum;
  
  /* The centered product sums Sxx and Sxy, and hence A and B. */
  Sxx=ZERO;
  Sxy=ZERO;
  for(i=0;(i<n);i++){
    Sxx+=w[i]*sqr(x[i]-xbar);
    Sxy+=w[i]*(x[i]-xbar)*(ly[i]-ybar);
  }
  B=Sxy/Sxx;
  A=ybar-B*xbar;
  
  /* Chi-squared (chi2) and gamma. */
  chi2=ZERO;
  gamma=ZERO;
  for(i=0;(i<n);i++){
    wr2=w[i]*sqr(ly[i]-A-B*x[i]);
    chi2+=wr2;
    gamma+=wr2*(x[i]-xbar);
  }
  
  /* Refined values of A and B. Also SA and SB. */
  Db=-ONEP5*gamma/Sxx;
  B+=Db;
  A-=ONEP5*chi2/sum-xbar*Db;
  SB=sqrt((chi2/(n-2))/Sxx);
  SA=SB*sqrt(Sxx/sum+sqr(xbar));
  *a=A;
  *b=B;
  *sa=SA;
  *sb=SB;
}


