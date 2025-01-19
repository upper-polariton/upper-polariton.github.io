/*
 * $Id: mgmxtest.c,v 1.8 2002/02/28 10:49:26 spoel Exp $
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
 * Gyas ROwers Mature At Cryogenic Speed
 */
static char *SRCID_mgmxtest_c = "$Id: mgmxtest.c,v 1.8 2002/02/28 10:49:26 spoel Exp $";
#include "copyrite.h"
#include "statutil.h"
#include "macros.h"

int main(int argc, char *argv[])
{
  static char *desc[] = {
    "g_chi computes phi, psi and chi dihedrals for all your sidechains.",
    "Output is in form of xvgr files, as well as a LaTeX table of the",
    "number of transitions per nanosecond.[PAR]",
    "Order parameters S2 for each of the dihedrals are calculated and",
    "output as xvgr file and optionally as a pdb file with the S2",
    "values as B-factor.[PAR]",
    "If option [TT]-c[tt] is given, the program will",
    "calculate dihedral autocorrelation functions. The function used",
    "is C(t) = < cos(chi(tau)) cos(chi(tau+t)) >. The use of cosines",
    "rather than angles themselves, resolves the problem of periodicity.[PAR]",
    "g_chi computes phi, psi and chi dihedrals for all your sidechains.",
    "Output is in form of xvgr files, as well as a LaTeX table of the",
    "number of transitions per nanosecond.[PAR]",
    "Order parameters S2 for each of the dihedrals are calculated and",
    "output as xvgr file and optionally as a pdb file with the S2",
    "values as B-factor.[PAR]",
    "If option [TT]-c[tt] is given, the program will",
    "calculate dihedral autocorrelation functions. The function used",
    "is C(t) = < cos(chi(tau)) cos(chi(tau+t)) >. The use of cosines",
    "rather than angles themselves, resolves the problem of periodicity.[PAR]",
    "g_chi computes phi, psi and chi dihedrals for all your sidechains.",
    "Output is in form of xvgr files, as well as a LaTeX table of the",
    "number of transitions per nanosecond.[PAR]",
    "Order parameters S2 for each of the dihedrals are calculated and",
    "output as xvgr file and optionally as a pdb file with the S2",
    "values as B-factor.[PAR]",
    "If option [TT]-c[tt] is given, the program will",
    "calculate dihedral autocorrelation functions. The function used",
    "is C(t) = < cos(chi(tau)) cos(chi(tau+t)) >. The use of cosines",
    "rather than angles themselves, resolves the problem of periodicity."
  };
  static char *bugs[] = {
    "Produces MANY output files (up to about 4 times the number of residues in the protein, twice that if autocorrelation functions are calculated). Typically several hundred files are output.",
    "Produces MANY output files (up to about 4 times the number of residues in the protein, twice that if autocorrelation functions are calculated). Typically several hundred files are output.",
    "Produces MANY output files (up to about 4 times the number of residues in the protein, twice that if autocorrelation functions are calculated). Typically several hundred files are output."
  };
  int    dummy;
  static int  r0=1,ndeg=1,maxchi=2,nf=10;
  static bool bAll=FALSE;
  static bool bPhi=FALSE,bPsi=FALSE,bChi=TRUE;
  static real bfac_init=-1.0;
  static bool bRama=FALSE,bShift=TRUE;
  static char *maxchistr[] = { "0", "1", "2", "3",  "4", "5", "6", NULL };

  t_pargs pa[] = {
    { "-r0",  FALSE, etINT, &r0,
      "Starting residue number" },
    { "-phi",  FALSE, etBOOL, &bPhi,
      "Output for Phi dihedral angles" },
    { "-psi",  FALSE, etBOOL, &bPsi,
      "Output for Psi dihedral angles" },
    { "-chi",  FALSE, etBOOL, &bChi,
      "Output for Chi dihedral angles" },
    { "-rama", FALSE, etBOOL, &bRama,
      "Generate Phi/Psi and Chi1/Chi2 ramachandran plots" },
    { "-all",  FALSE, etBOOL, &bAll,
      "Output separate files for every dihedral." },
    { "-nframes", FALSE, etINT, &nf,
      "Number of frames in your trajectory" },
    { "-shift", FALSE, etBOOL, &bShift,
      "Compute chemical shifts from Phi/Psi angles" },
    { "-run", FALSE, etINT, &ndeg,
      "Perform running average over ndeg degrees for histograms" },
    { "-maxchi", FALSE, etENUM, maxchistr,
      "Calculate first ndih Chi dihedrals" },
    { "-bfact", FALSE, etREAL, &bfac_init,
      "B-Factor value for pdb file for atoms with no calculated dihedral order parameter"}
  };

  t_filenm  fnm[] = {
    { efTPX, NULL,  NULL,     ffREAD  },
    { efTRX, "-f",  NULL,     ffREAD  },
    { efXVG, "-o",  "order",  ffWRITE },
    { efPDB, "-p",  "order",  ffOPTWR },
    { efXVG, "-jc", "Jcoupling", ffWRITE },
    { efXVG, "-c",  "dihcorr",ffOPTWR },
    { efTEX, "-t",  "trans",  ffWRITE },
    { efLOG, "-g",  "chi",    ffWRITE }
  };
#define NFILE asize(fnm)

  parse_common_args(&argc,argv,0,TRUE,
		    NFILE,fnm,asize(pa),pa,asize(desc),desc,asize(bugs),bugs);
  
  fprintf(stderr,"You made it back in main of mgmxtest!\n");
  fprintf(stderr,"Please enter an integer and press return:");
  scanf("%d",&dummy);
  
  thanx(stderr);
    
  return 0;
}
