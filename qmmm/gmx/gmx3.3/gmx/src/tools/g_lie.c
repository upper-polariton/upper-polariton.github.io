/*
 * $Id: g_lie.c,v 1.10 2002/02/28 11:00:26 spoel Exp $
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
static char *SRCID_g_lie_c = "$Id: g_lie.c,v 1.10 2002/02/28 11:00:26 spoel Exp $";
#include <stdio.h>
#include <stdlib.h>
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
#include "txtdump.h"
#include "enxio.h"
#include "gstat.h"
#include "xvgr.h"

typedef struct {
  int nlj,nqq;
  int *lj;
  int *qq;
} t_liedata;

static t_liedata *analyze_names(int nre,char *names[],char *ligand)
{
  int       i;
  t_liedata *ld;
  char      self[256];
  
  /* Skip until we come to pressure */
  for(i=0; (i<F_NRE); i++)
    if (strcmp(names[i],interaction_function[F_PRES].longname) == 0)
      break;
      
  /* Now real analysis: find components of energies */
  sprintf(self,"%s-%s",ligand,ligand);
  snew(ld,1);
  for( ; (i<nre); i++) {
    if ((strstr(names[i],ligand) != NULL) && 
	(strstr(names[i],self) == NULL)) {
      if (strstr(names[i],"LJ") != NULL) {
	ld->nlj++;
	srenew(ld->lj,ld->nlj);
	ld->lj[ld->nlj-1] = i;
      }
      else if (strstr(names[i],"Coul") != NULL) {
	ld->nqq++;
	srenew(ld->qq,ld->nqq);
	ld->qq[ld->nqq-1] = i;
      }
    }
  }
  printf("Using the following energy terms:\n");
  printf("LJ:  ");
  for(i=0; (i<ld->nlj); i++)
    printf("  %12s",names[ld->lj[i]]);
  printf("\nCoul:");
  for(i=0; (i<ld->nqq); i++)
    printf("  %12s",names[ld->qq[i]]);
  printf("\n");
  
  return ld;
}

real calc_lie(t_liedata *ld,t_energy ee[],real lie_lj,real lie_qq,
	      real fac_lj,real fac_qq)
{
  int i;
  real lj_tot,qq_tot;
  
  lj_tot = 0;
  for(i=0; (i<ld->nlj); i++)
    lj_tot += ee[ld->lj[i]].e;
  qq_tot = 0;
  for(i=0; (i<ld->nqq); i++)
    qq_tot += ee[ld->qq[i]].e;
    
  /* And now the great LIE formula: */
  return fac_lj*(lj_tot-lie_lj)+fac_qq*(qq_tot-lie_qq);
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "g_lie computes a free energy estimate based on an energy analysis",
    "from. One needs an energy file with the following components:",
    "Coul (A-B) LJ-SR (A-B) etc."
  };
  static real lie_lj=0,lie_qq=0,fac_lj=0.181,fac_qq=0.5;
  static char *ligand="none";
  t_pargs pa[] = {
    { "-Elj",  FALSE, etREAL, {&lie_lj},
      "Lennard-Jones interaction between ligand and solvent" },
    { "-Eqq",  FALSE, etREAL, {&lie_qq},
      "Coulomb interaction between ligand and solvent" },
    { "-Clj",  FALSE, etREAL, {&fac_lj},
      "Factor in the LIE equation for Lennard-Jones component of energy" },
    { "-Cqq",  FALSE, etREAL, {&lie_qq},
      "Factor in the LIE equation for Coulomb component of energy" },
    { "-ligand",  FALSE, etSTR, {&ligand},
      "Name of the ligand in the energy file" }
  };
#define NPA asize(pa)

  FILE      *out;
  int       fp,nre,nframes=0,ct=0;
  char      **enm=NULL;
  bool      bCont;
  t_liedata *ld;
  t_enxframe *fr;
  real      lie;
  double    lieaver=0,lieav2=0;
    
  t_filenm fnm[] = { 
    { efENX, "-f",    "ener",     ffREAD   },
    { efXVG, "-o",    "lie",      ffWRITE  }
  }; 
#define NFILE asize(fnm) 

  CopyRight(stderr,argv[0]); 
  parse_common_args(&argc,argv,PCA_CAN_VIEW | PCA_CAN_TIME | PCA_BE_NICE,
		    NFILE,fnm,NPA,pa,asize(desc),desc,0,NULL); 
    
  fp = open_enx(ftp2fn(efENX,NFILE,fnm),"r");
  do_enxnms(fp,&nre,&enm);
  
  ld = analyze_names(nre,enm,ligand);
  snew(fr,1);
  out = xvgropen(ftp2fn(efXVG,NFILE,fnm),"LIE free energy estimate",
		 "Time (ps)","DGbind (kJ/mol)");
  do {
    bCont = do_enx(fp,fr);
    ct    = check_times(fr->t);
    if (ct == 0) {
      lie = calc_lie(ld,fr->ener,lie_lj,lie_qq,fac_lj,fac_qq);
      lieaver += lie;
      lieav2  += lie*lie;
      nframes ++;
      fprintf(out,"%10g  %10g\n",fr->t,lie);
    }
  } while (bCont);
  close_enx(fp);
  fclose(out);
  fprintf(stderr,"\n");
  
  if (nframes > 0)
    printf("DGbind = %.3f (%.3f)\n",lieaver/nframes,
	   sqrt(lieav2/nframes-sqr(lieaver/nframes)));
  
  do_view(ftp2fn(efXVG,NFILE,fnm),NULL);
    
  thanx(stderr);

  return 0;
}
  
