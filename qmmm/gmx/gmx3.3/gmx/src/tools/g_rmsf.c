/*
 * $Id: g_rmsf.c,v 1.38 2002/02/28 11:00:27 spoel Exp $
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
static char *SRCID_g_rmsf_c = "$Id: g_rmsf.c,v 1.38 2002/02/28 11:00:27 spoel Exp $";
#include "smalloc.h"
#include "math.h"
#include "macros.h"
#include "typedefs.h"
#include "xvgr.h"
#include "copyrite.h"
#include "statutil.h"
#include "string2.h"
#include "vec.h"
#include "rdgroup.h"
#include "pdbio.h"
#include "tpxio.h"
#include "pbc.h"
#include "fatal.h"
#include "futil.h"
#include "do_fit.h"
#include "princ.h"
#include "rmpbc.h"
#include "confio.h"
#include "ql77.h"

static real find_pdb_bfac(t_atoms *atoms,char *resnm,int resnr,char *atomnm)
{
  char rresnm[8];
  int i;
  
  strcpy(rresnm,resnm);
  rresnm[3]='\0';
  for(i=0; (i<atoms->nr); i++) {
    if ((resnr == atoms->atom[i].resnr) &&
	(strcmp(*atoms->resname[resnr],rresnm) == 0) &&
	(strstr(*atoms->atomname[i],atomnm) != NULL))
      break;
  }
  if (i == atoms->nr) {
    fprintf(stderr,"\rCan not find %s%d-%s in pdbfile\n",
	    rresnm,resnr,atomnm);
    return 0.0;
  }
    
  return atoms->pdbinfo[i].bfac;
}

void correlate_aniso(char *fn,t_atoms *ref,t_atoms *calc)
{
  FILE *fp;
  int  i,j;
  
  fp = xvgropen(fn,"Correlation between X-Ray and Computed Uij","X-Ray","Computed");
  for(i=0; (i<ref->nr); i++) {
    if (ref->pdbinfo[i].bAnisotropic) {
      for(j=U11; (j<=U23); j++)
	fprintf(fp,"%10d  %10d\n",ref->pdbinfo[i].uij[j],calc->pdbinfo[i].uij[j]);
    }
  }
  fclose(fp);
}

void average_residues(real f[],int isize,atom_id index[],real w_rls[],
		      t_atoms *atoms)
{
  int i,j,start;
  real av,m;
  
  start = 0;
  av = 0;
  m = 0;
  for(i=0; i<isize; i++) {
    av += w_rls[index[i]]*f[i];
    m += w_rls[index[i]];
    if (i+1==isize || 
	atoms->atom[index[i]].resnr!=atoms->atom[index[i+1]].resnr) {
      av /= m;
      for(j=start; j<=i; j++)
	f[j] = av;
      start = i+1;
      av = 0;
      m = 0;
    }
  }
}

void print_dir(FILE *fp,real *Uaver)
{
  real eigvec[DIM*DIM];
  rvec eigval;
  int d,m;

  fprintf(fp,"MSF     X         Y         Z\n");
  for(d=0; d<DIM; d++) {
    fprintf(fp," %c ",'X'+d-XX);
    for(m=0; m<DIM; m++)
      fprintf(fp," %9.2e",Uaver[3*m+d]);
    fprintf(fp,"%s\n",m==DIM ? " (nm^2)" : "");
  }
  
  for(m=0; m<DIM*DIM; m++)
    eigvec[m] = Uaver[m];

  ql77(DIM,eigvec,eigval);
  
  fprintf(fp,"\n             Eigenvectors\n\n");
  fprintf(fp,"Eigv  %-8.2e %-8.2e %-8.2e (nm^2)\n\n",
	  eigval[2],eigval[1],eigval[0]);
  for(d=0; d<DIM; d++) {
    fprintf(fp,"  %c   ",'X'+d-XX);
      for(m=DIM-1; m>=0; m--)
	fprintf(fp,"%7.4f  ",eigvec[3*m+d]);
    fprintf(fp,"\n");
  }
}

int main (int argc,char *argv[])
{
  static char *desc[] = {
    "g_rmsf computes the root mean square fluctuation (RMSF, i.e. standard ",
    "deviation) of atomic positions ",
    "after first fitting to a reference frame.[PAR]",
    "With option [TT]-oq[tt] the RMSF values are converted to B-factor",
    "values, which are written to a pdb file with the coordinates, of the",
    "structure file, or of a pdb file when [TT]-q[tt] is specified.",
    "Option [TT]-ox[tt] writes the B-factors to a file with the average",
    "coordinates.[PAR]",
    "With the option [TT]-od[tt] the root mean square deviation with",
    "respect to the reference structure is calculated.[PAR]",
    "With the option [TT]aniso[tt] g_rmsf will compute anisotropic",
    "temperature factors and then it will also output average coordinates",
    "and a pdb file with ANISOU records (corresonding to the [TT]-oq[tt]",
    "or [TT]-ox[tt] option). Please note that the U values",
    "are orientation dependent, so before comparison with experimental data",
    "you should verify that you fit to the experimental coordinates.[PAR]",
    "When a pdb input file is passed to the program and the [TT]-aniso[tt]",
    "flag is set",
    "a correlation plot of the Uij will be created, if any anisotropic",
    "temperature factors are present in the pdb file.[PAR]",
    "With option [TT]-dir[tt] the average MSF (3x3) matrix is diagonalized.",
    "This shows the directions in which the atoms fluctuate the most and",
    "the least."
  };
  static bool bRes=FALSE,bAniso=FALSE,bdevX=FALSE;
  t_pargs pargs[] = { 
    { "-res", FALSE, etBOOL, {&bRes},
      "Calculate averages for each residue" },
    { "-aniso",FALSE, etBOOL, {&bAniso},
      "Compute anisotropic termperature factors" }
  };
  int          step,nre,natom,natoms,i,g,m,teller=0;
  real         t,lambda,*w_rls,*w_rms;
  
  t_tpxheader  header;
  t_inputrec   ir;
  t_topology   top;
  t_atoms      *pdbatoms,*refatoms;
  bool         bCont;

  matrix       box,pdbbox;
  rvec         *x,*pdbx,*xref;
  int          status,npdbatoms,res0;
  char         buf[256],*label;
  char         title[STRLEN];
  
  FILE         *fp;               /* the graphics file */
  char         *devfn,*dirfn;
  int          resnr;

  bool         bReadPDB;  
  atom_id      *index;
  int          isize;
  char         *grpnames;

  real         bfac,pdb_bfac,*Uaver;
  matrix       *U=NULL;
  atom_id      aid;
  rvec         *xav,*rmsd_x=NULL;
  real         *rmsf,invcount,totmass;
  int          d;
  real         count=0;
  rvec         xcm;

  char         *leg[2] = { "MD", "X-Ray" };

  t_filenm fnm[] = {
    { efTRX, "-f",  NULL,     ffREAD  },
    { efTPS, NULL,  NULL,     ffREAD  },
    { efNDX, NULL,  NULL,     ffOPTRD },
    { efPDB, "-q",  NULL,     ffOPTRD },
    { efPDB, "-oq", "bfac",   ffOPTWR },
    { efPDB, "-ox", "xaver",  ffOPTWR },
    { efXVG, "-o",  "rmsf",   ffWRITE },
    { efXVG, "-od", "rmsdev", ffOPTWR },
    { efXVG, "-oc", "correl", ffOPTWR },
    { efLOG, "-dir", "rmsf",  ffOPTWR }
  };
#define NFILE asize(fnm)

  CopyRight(stderr,argv[0]);
  parse_common_args(&argc,argv,PCA_CAN_TIME | PCA_CAN_VIEW | PCA_BE_NICE ,
		    NFILE,fnm,asize(pargs),pargs,asize(desc),desc,0,NULL);

  bReadPDB = ftp2bSet(efPDB,NFILE,fnm);
  devfn    = opt2fn_null("-od",NFILE,fnm);
  dirfn    = opt2fn_null("-dir",NFILE,fnm);

  read_tps_conf(ftp2fn(efTPS,NFILE,fnm),title,&top,&xref,NULL,box,TRUE);
  snew(w_rls,top.atoms.nr);

  /* Set box type*/
  init_pbc(box);
  
  fprintf(stderr,"Select group(s) for root mean square calculation\n");
  get_index(&top.atoms,ftp2fn_null(efNDX,NFILE,fnm),1,&isize,&index,&grpnames);

  /* Set the weight */
  for(i=0; i<isize; i++) 
    w_rls[index[i]]=top.atoms.atom[index[i]].m;

  /* Malloc the rmsf arrays */
  snew(xav,isize);
  snew(U,isize);
  snew(rmsf,isize);
  if (devfn)
    snew(rmsd_x, isize);
  
  if (bReadPDB) {
    get_stx_coordnum(opt2fn("-q",NFILE,fnm),&npdbatoms);
    snew(pdbatoms,1);
    snew(refatoms,1);
    init_t_atoms(pdbatoms,npdbatoms,TRUE);
    init_t_atoms(refatoms,npdbatoms,TRUE);
    snew(pdbx,npdbatoms);
    /* Read coordinates twice */
    read_stx_conf(opt2fn("-q",NFILE,fnm),title,pdbatoms,pdbx,NULL,pdbbox);
    read_stx_conf(opt2fn("-q",NFILE,fnm),title,refatoms,pdbx,NULL,pdbbox);
  } else {
    pdbatoms  = &top.atoms;
    refatoms  = &top.atoms;
    pdbx      = xref;
    npdbatoms = pdbatoms->nr;
    snew(pdbatoms->pdbinfo,npdbatoms);
    copy_mat(box,pdbbox);
  }
  
  sub_xcm(xref,isize,index,top.atoms.atom,xcm,FALSE);

  natom = read_first_x(&status,ftp2fn(efTRX,NFILE,fnm),&t,&x,box);
    
  /* Now read the trj again to compute fluctuations */
  teller = 0;
  do {
    /* Remove periodic boundary */
    rm_pbc(&(top.idef),natom,box,x,x);
    
    /* Set center of mass to zero */
    sub_xcm(x,isize,index,top.atoms.atom,xcm,FALSE);
    
    /* Fit to reference structure */
    do_fit(natom,w_rls,xref,x);
 
    /* Calculate Anisotropic U Tensor */  
    for(i=0; i<isize; i++) {
      aid = index[i];
      for(d=0; d<DIM; d++) {
	xav[i][d] += x[aid][d];
	for(m=0; m<DIM; m++)
	  U[i][d][m] += x[aid][d]*x[aid][m];
      }
    }
    
    if (devfn) {
      /* Calculate RMS Deviation */
      for(i=0;(i<isize);i++) {
	aid = index[i];
	for(d=0;(d<DIM);d++) {
	  rmsd_x[i][d] += sqr(x[aid][d]-xref[aid][d]);
	}
      }
    } 
    count += 1.0;
    teller++;
  } while(read_next_x(status,&t,natom,x,box));
  close_trj(status);
  
  invcount = 1.0/count;
  snew(Uaver,DIM*DIM);
  totmass = 0;
  for(i=0; i<isize; i++) {
    svmul(invcount,xav[i],xav[i]);
    for(d=0; d<DIM; d++)
      for(m=0; m<DIM; m++) {
	U[i][d][m] = U[i][d][m]*invcount-xav[i][d]*xav[i][m];
	Uaver[3*d+m] += top.atoms.atom[index[i]].m*U[i][d][m];
      }
    totmass += top.atoms.atom[index[i]].m;
  }
  for(d=0; d<DIM*DIM; d++)
    Uaver[d] /= totmass;

  if (bAniso) {
    for(i=0; i<isize; i++) {
      aid = index[i];
      pdbatoms->pdbinfo[aid].bAnisotropic = TRUE;
      pdbatoms->pdbinfo[aid].uij[U11] = 1e6*U[i][XX][XX];
      pdbatoms->pdbinfo[aid].uij[U22] = 1e6*U[i][YY][YY];
      pdbatoms->pdbinfo[aid].uij[U33] = 1e6*U[i][ZZ][ZZ];
      pdbatoms->pdbinfo[aid].uij[U12] = 1e6*U[i][XX][YY];
      pdbatoms->pdbinfo[aid].uij[U13] = 1e6*U[i][XX][ZZ];
      pdbatoms->pdbinfo[aid].uij[U23] = 1e6*U[i][YY][ZZ];
    }
    sfree(U);
  }
  if (bRes) {
    average_residues(rmsf,isize,index,w_rls,&top.atoms);
    label = "Residue";
  } else
    label = "Atom";

  for(i=0; i<isize; i++)
    rmsf[i] = U[i][XX][XX] + U[i][YY][YY] + U[i][ZZ][ZZ];
  
  if (dirfn) {
    fprintf(stdout,"\n");
    print_dir(stdout,Uaver);
    fp = ffopen(dirfn,"w");
    print_dir(fp,Uaver);
    fclose(fp);
  }
  
  /* Write RMSF output */
  if (bReadPDB) {
    bfac = 8.0*M_PI*M_PI/3.0*100;
    fp   = xvgropen(ftp2fn(efXVG,NFILE,fnm),"B-Factors",
		    label,"(A\\b\\S\\So\\N\\S2\\N)");
    xvgr_legend(fp,2,leg);
    for(i=0;(i<isize);i++) {
      if (!bRes || i+1==isize ||
	  top.atoms.atom[index[i]].resnr!=top.atoms.atom[index[i+1]].resnr) {
	resnr    = top.atoms.atom[index[i]].resnr;
	pdb_bfac = find_pdb_bfac(pdbatoms,*(top.atoms.resname[resnr]),resnr,
				 *(top.atoms.atomname[index[i]]));
	
	fprintf(fp,"%5d  %10.5f  %10.5f\n",
		bRes ? top.atoms.atom[index[i]].resnr+1 : i+1,rmsf[i]*bfac,
		pdb_bfac);
      }
    }
    fclose(fp);
  } else {
    fp = xvgropen(ftp2fn(efXVG,NFILE,fnm),"RMS fluctuation",label,"(nm)");
    for(i=0; i<isize; i++)
      if (!bRes || i+1==isize ||
	  top.atoms.atom[index[i]].resnr!=top.atoms.atom[index[i+1]].resnr)
	fprintf(fp,"%5d %8.4f\n",
		bRes ? top.atoms.atom[index[i]].resnr+1 : i+1,sqrt(rmsf[i]));
    fclose(fp);
  }

  if (devfn) {
    for(i=0; i<isize; i++)
      rmsf[i] = (rmsd_x[i][XX]+rmsd_x[i][YY]+rmsd_x[i][ZZ])/count;
      if (bRes)
	average_residues(rmsf,isize,index,w_rls,&top.atoms); 
      /* Write RMSD output */
    fp = xvgropen(devfn,"RMS Deviation",label,"(nm)");
    for(i=0; i<isize; i++)
      if (!bRes || i+1==isize ||
	  top.atoms.atom[index[i]].resnr!=top.atoms.atom[index[i+1]].resnr)
	fprintf(fp,"%5d %8.4f\n",
		bRes ? top.atoms.atom[index[i]].resnr+1 : i+1,sqrt(rmsf[i]));
    fclose(fp);
  }

  for(i=0; i<isize; i++)
    pdbatoms->pdbinfo[index[i]].bfac = 800*M_PI*M_PI/3.0*rmsf[i];

  if (opt2bSet("-oq",NFILE,fnm)) {
    /* Write a pdb file with B-factors and optionally anisou records */
    for(i=0; i<isize; i++)
      rvec_inc(xref[index[i]],xcm);
    write_sto_conf(opt2fn("-oq",NFILE,fnm),title,pdbatoms,pdbx,NULL,pdbbox);
  }
  if (opt2bSet("-ox",NFILE,fnm)) {
    /* Misuse xref as a temporary array */
    for(i=0; i<isize; i++)
      rvec_add(xav[i],xcm,xref[index[i]]);
    /* Write a pdb file with B-factors and optionally anisou records */
    write_sto_conf_indexed(opt2fn("-ox",NFILE,fnm),title,pdbatoms,xref,NULL,
			   pdbbox,isize,index);
  }
  if (bAniso) { 
    correlate_aniso(opt2fn("-oc",NFILE,fnm),refatoms,pdbatoms);
    do_view(opt2fn("-oc",NFILE,fnm),"-nxy");
  }
  do_view(opt2fn("-o",NFILE,fnm),"-nxy");
  if (devfn)
    do_view(opt2fn("-od",NFILE,fnm),"-nxy");
    
  thanx(stderr);
  
  return 0;
}
