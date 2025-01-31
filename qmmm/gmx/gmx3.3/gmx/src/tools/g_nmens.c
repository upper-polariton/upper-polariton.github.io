/*
 * $Id: g_nmens.c,v 1.9 2002/02/28 11:00:27 spoel Exp $
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
static char *SRCID_g_nmens_c = "$Id: g_nmens.c,v 1.9 2002/02/28 11:00:27 spoel Exp $";
#include <math.h>
#include <string.h>
#include "statutil.h"
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "fatal.h"
#include "vec.h"
#include "copyrite.h"
#include "futil.h"
#include "statutil.h"
#include "rdgroup.h"
#include "pdbio.h"
#include "tpxio.h"
#include "txtdump.h"
#include "physics.h"
#include "random.h"
#include "eigio.h"

int read_eigval(char *fn,int nmax,real eigval[])
{
  FILE *fp;
  char line[STRLEN];
  int  n,num;
  double dbl;
  bool bEndOfSet;

  n=0;
  fp = ffopen(fn,"r");
  bEndOfSet = FALSE;
  while (fgets(line,STRLEN-1,fp) && !bEndOfSet) {
    bEndOfSet = (line[0] == '&');
    if ((line[0] != '#') && (line[0] != '@') && !bEndOfSet) {
      if ((sscanf(line,"%d %lf",&num,&dbl) != 2) || ((num < 1) || (num > nmax)))
fprintf(stderr,"Invalid line in %s: '%s'\n",fn,line);
      else {
	eigval[num-1] = dbl;
	n++;
      }
    }
  }
  fclose(fp);
  
  return n;
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "[TT]g_nmens[tt] generates an ensemble around an average structure",
    "in a subspace which is defined by a set of normal modes (eigenvectors).",
    "The eigenvectors are assumed to be mass-weighted.",
    "The position along each eigenvector is randomly taken from a Gaussian",
    "distribution with variance kT/eigenvalue.[PAR]",
    "By default the starting eigenvector is set to 7, since the first six",
    "normal modes are the translational and rotational degrees of freedom." 
  };
  static int  nstruct=100,first=7,last=-1,seed=-1;
  static real temp=300.0;
  t_pargs pa[] = {
    { "-temp",  FALSE, etREAL, {&temp}, 
      "Temperature in Kelvin" },
    { "-seed", FALSE, etINT, {&seed},     
      "Random seed, -1 generates a seed from time and pid" },
    { "-num", FALSE, etINT, {&nstruct},     
      "Number of structures to generate" },
    { "-first", FALSE, etINT, {&first},     
      "First eigenvector to use (-1 is select)" },
    { "-last",  FALSE, etINT, {&last}, 
      "Last eigenvector to use (-1 is till the last)" }
  };
#define NPA asize(pa)
  
  int        out;
  int        status,trjout;
  t_topology top;
  t_atoms    *atoms;
  rvec       *xtop,*xref,*xav,*xout1,*xout2;
  bool       bDMR,bDMA,bFit;
  int        nvec,*eignr=NULL;
  rvec       **eigvec=NULL;
  matrix     box;
  real       *eigval,totmass,*invsqrtm,t,disp;
  int        natoms,neigval;
  char       *grpname,*indexfile,title[STRLEN];
  int        i,j,d,s,v;
  int        nout,*iout,noutvec,*outvec;
  atom_id    *index;
  real       rfac,invfr,rhalf,jr;
  unsigned long      jran;
  const unsigned long im = 0xffff;
  const unsigned long ia = 1093;
  const unsigned long ic = 18257;

  t_filenm fnm[] = { 
    { efTRN, "-v",    "eigenvec",    ffREAD  },
    { efXVG, "-e",    "eigenval",    ffREAD  },
    { efTPS, NULL,    NULL,          ffREAD },
    { efNDX, NULL,    NULL,          ffOPTRD },
    { efTRX, "-o",    "ensemble",    ffWRITE }
  }; 
#define NFILE asize(fnm) 

  CopyRight(stderr,argv[0]); 
  parse_common_args(&argc,argv,PCA_BE_NICE,
		    NFILE,fnm,NPA,pa,asize(desc),desc,0,NULL); 

  indexfile=ftp2fn_null(efNDX,NFILE,fnm);

  read_eigenvectors(opt2fn("-v",NFILE,fnm),&natoms,&bFit,
		    &xref,&bDMR,&xav,&bDMA,&nvec,&eignr,&eigvec);

  read_tps_conf(ftp2fn(efTPS,NFILE,fnm),title,&top,&xtop,NULL,box,bDMA);
  atoms=&top.atoms;

  printf("\nSelect an index group of %d elements that corresponds to the eigenvectors\n",natoms);
  get_index(atoms,indexfile,1,&i,&index,&grpname);
  if (i!=natoms)
    fatal_error(0,"you selected a group with %d elements instead of %d",
		i,natoms);
  printf("\n");
  
  snew(eigval,DIM*natoms);
  neigval=read_eigval(ftp2fn(efXVG,NFILE,fnm),DIM*natoms,eigval);
  fprintf(stderr,"Read %d eigenvalues\n",neigval);

  snew(invsqrtm,natoms);
  if (bDMA) {
    for(i=0; (i<natoms); i++)
      invsqrtm[i] = invsqrt(atoms->atom[index[i]].m);
  } else {
    for(i=0; (i<natoms); i++)
      invsqrtm[i]=1.0;
  }
  
  if (last==-1)
    last=natoms*DIM;
  if (first>-1) {
    /* make an index from first to last */
    nout=last-first+1;
    snew(iout,nout);
    for(i=0; i<nout; i++)
      iout[i]=first-1+i;
  } else {
    printf("Select eigenvectors for output, end your selection with 0\n");
    nout=-1;
    iout=NULL;
    do {
      nout++;
      srenew(iout,nout+1);
      scanf("%d",&iout[nout]);
      iout[nout]--;
    } while (iout[nout]>=0);
    printf("\n");
  }
  /* make an index of the eigenvectors which are present */
  snew(outvec,nout);
  noutvec=0;
  for(i=0; i<nout; i++) {
    j=0;
    while ((j<nvec) && (eignr[j]!=iout[i]))
      j++;
    if ((j<nvec) && (eignr[j]==iout[i])) {
      outvec[noutvec] = j;
      iout[noutvec] = iout[i];
      noutvec++;
    }
  }
  fprintf(stderr,"%d eigenvectors selected for output\n",noutvec);

  if (seed == -1)
    seed = make_seed();
  fprintf(stderr,"Using seed %d and a temperature of %g K\n",seed,temp);

  snew(xout1,natoms);
  snew(xout2,atoms->nr);
  out=open_trx(ftp2fn(efTRX,NFILE,fnm),"w");
  jran = (unsigned long)((real)im*rando(&seed));
  for(s=0; s<nstruct; s++) {
    for(i=0; i<natoms; i++)
      copy_rvec(xav[i],xout1[i]);
    for(j=0; j<noutvec; j++) {
      v = outvec[j];
      /* (r-0.5) n times:  var_n = n * var_1 = n/12
	 n=4:  var_n = 1/3, so multiply with 3 */
      
      rfac  = sqrt(3.0 * BOLTZ*temp/eigval[iout[j]]);
      rhalf = 2.0*rfac; 
      rfac  = rfac/(real)im;

      jran = (jran*ia+ic) & im;
      jr = (real)jran;
      jran = (jran*ia+ic) & im;
      jr += (real)jran;
      jran = (jran*ia+ic) & im;
      jr += (real)jran;
      jran = (jran*ia+ic) & im;
      jr += (real)jran;
      disp = rfac * jr - rhalf;
      
      for(i=0; i<natoms; i++)
	for(d=0; d<DIM; d++)
	  xout1[i][d] += disp*eigvec[v][i][d]*invsqrtm[i];
    }
    for(i=0; i<natoms; i++)
      copy_rvec(xout1[i],xout2[index[i]]);
    t = s+1;
    write_trx(out,natoms,index,atoms,0,t,box,xout2,NULL);
    fprintf(stderr,"\rGenerated %d structures",s+1);
  }
  fprintf(stderr,"\n");
  close_trx(out);
  
  return 0;
}
  
