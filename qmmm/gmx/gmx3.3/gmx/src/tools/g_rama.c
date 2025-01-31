/*
 * $Id: g_rama.c,v 1.15 2002/02/28 11:00:27 spoel Exp $
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
static char *SRCID_g_rama_c = "$Id: g_rama.c,v 1.15 2002/02/28 11:00:27 spoel Exp $";
#include <math.h>
#include "sysstuff.h"
#include "string.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "vec.h"
#include "xvgr.h"
#include "physics.h"
#include "pbc.h"
#include "copyrite.h"
#include "futil.h"
#include "statutil.h"
#include "rdgroup.h"
#include "nrama.h"

static void plot_rama(FILE *out,t_xrama *xr)
{
  int i;
  real phi,psi;
  
  for(i=0; (i<xr->npp); i++) {
    phi=xr->dih[xr->pp[i].iphi].ang*RAD2DEG;
    psi=xr->dih[xr->pp[i].ipsi].ang*RAD2DEG;
    fprintf(out,"%g  %g  %s\n",phi,psi,xr->pp[i].label);
  }
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "g_rama selects the Phi/Psi dihedral combinations from your topology file",
    "and computes these as a function of time.",
    "Using simple Unix tools such as [IT]grep[it] you can select out", 
    "specific residues."
  };

  FILE      *out;
  t_xrama   *xr;
  int       j;
  t_filenm  fnm[] = {
    { efTRX, "-f", NULL,  ffREAD },
    { efTPX, NULL, NULL,  ffREAD },
    { efXVG, NULL, "rama",ffWRITE }
  };
#define NFILE asize(fnm)

  CopyRight(stderr,argv[0]);
  parse_common_args(&argc,argv,PCA_CAN_VIEW | PCA_CAN_TIME | PCA_BE_NICE,
		    NFILE,fnm,0,NULL,asize(desc),desc,0,NULL);

		      
  snew(xr,1);
  init_rama(ftp2fn(efTRX,NFILE,fnm),ftp2fn(efTPX,NFILE,fnm),xr);
  
  out=xvgropen(ftp2fn(efXVG,NFILE,fnm),"Ramachandran Plot","Phi","Psi");
  xvgr_line_props(out,0,elNone,ecFrank);
  xvgr_view(out,0.2,0.2,0.8,0.8);
  xvgr_world(out,-180,-180,180,180);
  fprintf(out,"@    xaxis  tick on\n@    xaxis  tick major 60\n@    xaxis  tick minor 30\n");
  fprintf(out,"@    yaxis  tick on\n@    yaxis  tick major 60\n@    yaxis  tick minor 30\n");
  fprintf(out,"@ s0 symbol 2\n@ s0 symbol size 0.4\n@ s0 symbol fill 1\n");
  
  j=0;
  do {
    plot_rama(out,xr);
    j++;
  } while (new_data(xr));
  fprintf(stderr,"\n");
  fclose(out);
  
  do_view(ftp2fn(efXVG,NFILE,fnm),NULL);
  
  thanx(stderr);
  
  return 0;
}
