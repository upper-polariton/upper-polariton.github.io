/*
 * $Id: dlist.c,v 1.11 2002/09/15 11:57:41 spoel Exp $
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
static char *SRCID_dlist_c = "$Id: dlist.c,v 1.11 2002/09/15 11:57:41 spoel Exp $";
#include <stdlib.h>
#include "string2.h"
#include "smalloc.h"
#include "gstat.h"
	
t_dlist *mk_dlist(FILE *log, 
		  t_atoms *atoms, int *nlist,
		  bool bPhi, bool bPsi, bool bChi, bool bHChi,
		  int maxchi,int r0,int naa,char **aa)
{
  int     ires,i,j,k,ii;
  t_dihatms atm,prev;
  int     nl=0,nc[edMax],ndih;
  bool    bDih;
  char    *thisres;
  t_dlist *dl;
 
  snew(dl,atoms->nres+1);
  prev.C = prev.O = -1;
  for(i=0; (i<edMax); i++)
    nc[i]=0;
  ires = -1;
  i    =  0;
  while (i<atoms->nr) {
    ires=atoms->atom[i].resnr;
    
    /* Initiate all atom numbers to -1 */
    atm.minC=atm.H=atm.N=atm.C=atm.O=-1;
    for(j=0; (j<MAXCHI+3); j++)
      atm.Cn[j]=-1;
      
    /* Look for atoms in this residue */
    /* maybe should allow for chis to hydrogens? */
    while ((i<atoms->nr) && (atoms->atom[i].resnr == ires)) {
      if ((strcmp(*(atoms->atomname[i]),"H") == 0) ||
	  (strcmp(*(atoms->atomname[i]),"H1") == 0) )
	atm.H=i;
      else if (strcmp(*(atoms->atomname[i]),"N") == 0)
	atm.N=i;
      else if (strcmp(*(atoms->atomname[i]),"C") == 0)
	atm.C=i;
      else if ((strcmp(*(atoms->atomname[i]),"O") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"O1") == 0))
	atm.O=i;
      else if (strcmp(*(atoms->atomname[i]),"CA") == 0)
	atm.Cn[1]=i;
      else if (strcmp(*(atoms->atomname[i]),"CB") == 0)
	atm.Cn[2]=i;
      else if ((strcmp(*(atoms->atomname[i]),"CG") == 0)  ||
	       (strcmp(*(atoms->atomname[i]),"CG1") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"OG") == 0)  ||
	       (strcmp(*(atoms->atomname[i]),"OG1") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"SG") == 0))
	atm.Cn[3]=i;
      else if ((strcmp(*(atoms->atomname[i]),"CD") == 0)  ||
	       (strcmp(*(atoms->atomname[i]),"CD1") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"SD") == 0)  ||
	       (strcmp(*(atoms->atomname[i]),"OD1") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"ND1") == 0))
	atm.Cn[4]=i;
      /* by grs - split the Cn[4] into 2 bits to check allowing dih to H */
      else if (bHChi && ((strcmp(*(atoms->atomname[i]),"HG")  == 0) ||
	       (strcmp(*(atoms->atomname[i]),"HG1")  == 0)) )
	atm.Cn[4]=i;
      else if ((strcmp(*(atoms->atomname[i]),"CE") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"CE1") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"OE1") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"NE") == 0))
	atm.Cn[5]=i;
      else if ((strcmp(*(atoms->atomname[i]),"CZ") == 0) ||
	       (strcmp(*(atoms->atomname[i]),"NZ") == 0))
	atm.Cn[6]=i;
      /* HChi flag here too */ 
      else if (bHChi && (strcmp(*(atoms->atomname[i]),"NH1") == 0))
	atm.Cn[7]=i;
      i++;
    }
    
    /* added by grs - special case for aromatics, whose chis above 2 are 
       not real and produce rubbish output - so set back to -1 */ 
    if (strcmp(*(atoms->resname[ires]),"PHE") == 0 ||
	strcmp(*(atoms->resname[ires]),"TYR") == 0 ||
	strcmp(*(atoms->resname[ires]),"PTR") == 0 ||
	strcmp(*(atoms->resname[ires]),"TRP") == 0 ||
	strcmp(*(atoms->resname[ires]),"HIS") == 0 ||
	strcmp(*(atoms->resname[ires]),"HISA") == 0 ||
	strcmp(*(atoms->resname[ires]),"HISB") == 0 )  {
      for (ii=5 ; ii<=7 ; ii++) 
	atm.Cn[ii]=-1; 
    }
    /* end fixing aromatics */ 

    /* Special case for Pro, has no H */
    if (strcmp(*(atoms->resname[ires]),"PRO") == 0) 
      atm.H=atm.Cn[4];
    /* Carbon from previous residue */
    if (prev.C != -1)
      atm.minC = prev.C;
    if (prev.O != -1)
      atm.minO = prev.O;
    prev = atm;
      
    thisres=*(atoms->resname[ires]);	
    
    /* Check how many dihedrals we have */
    if ((atm.N != -1) && (atm.Cn[1] != -1) && (atm.C != -1) &&
      (atm.O != -1) && ((atm.H != -1) || (atm.minC != -1))) {
      dl[nl].resnr     = ires+1;
      dl[nl].atm       = atm;
      dl[nl].atm.Cn[0] = atm.N;
      if ((atm.Cn[3] != -1) && (atm.Cn[2] != -1) && (atm.Cn[1] != -1)) {
	nc[0]++;
	if (atm.Cn[4] != -1) {
	  nc[1]++;
	  if (atm.Cn[5] != -1) {
	    nc[2]++;
	    if (atm.Cn[6] != -1) {
	      nc[3]++;
	      if (atm.Cn[7] != -1) {
		nc[4]++;
		if (atm.Cn[8] != -1) {
		  nc[5]++;
		}
	      }
	    }
	  }
	}
      }
      if ((atm.minC != -1) && (atm.minO != -1))
	nc[6]++;
      for(k=0; (k<naa); k++) {
	if (strcasecmp(aa[k],thisres) == 0)
	  break;
      }
      dl[nl].index=k;
      
      sprintf(dl[nl].name,"%s%d",thisres,ires+r0);
      nl++;
    }
    else if (debug)
      fprintf(debug,"Could not find N atom but could find other atoms"
	      " in residue %s%d\n",thisres,ires+r0);
  }
  fprintf(stderr,"\n");
  fprintf(log,"\n");
  fprintf(log,"There are %d residues with dihedrals\n",nl);
  j=0;
  if (bPhi) j+=nl;
  if (bPsi) j+=nl;
  if (bChi)
    for(i=0; (i<maxchi); i++)
      j+=nc[i];
  fprintf(log,"There are %d dihedrals\n",j);
  fprintf(log,"Dihedral: ");
  if (bPhi) 
    fprintf(log," Phi  ");
  if (bPsi) 
    fprintf(log," Psi  ");
  if (bChi)
    for(i=0; (i<maxchi); i++)
      fprintf(log,"Chi%d  ",i+1);
  fprintf(log,"\nNumber:   ");
  if (bPhi) 
    fprintf(log,"%4d  ",nl);
  if (bPsi) 
    fprintf(log,"%4d  ",nl);
  if (bChi)
    for(i=0; (i<maxchi); i++)
      fprintf(log,"%4d  ",nc[i]);
  fprintf(log,"\n");
  
  *nlist=nl;

  return dl;
}

bool has_dihedral(int Dih,t_dlist *dl)
{
  bool b = FALSE;
  int  ddd;
  
  switch (Dih) {
  case edPhi:
    b = ((dl->atm.H!=-1) && (dl->atm.N!=-1) && (dl->atm.Cn[1]!=-1) && (dl->atm.C!=-1));
    break;
  case edPsi:
    b = ((dl->atm.N!=-1) && (dl->atm.Cn[1]!=-1) && (dl->atm.C!=-1) && (dl->atm.O!=-1));
    break;
  case edOmega:
    b = ((dl->atm.minO!=-1) && (dl->atm.minC!=-1) && (dl->atm.N!=-1) && (dl->atm.Cn[1]!=-1));
    break;
  case edChi1:
  case edChi2:
  case edChi3:
  case edChi4:
  case edChi5:
  case edChi6:
    ddd = Dih - edChi1;
    b   = ((dl->atm.Cn[ddd]!=-1) &&  (dl->atm.Cn[ddd+1]!=-1)&&
	   (dl->atm.Cn[ddd+2]!=-1) && (dl->atm.Cn[ddd+3]!=-1));
    break;
  default:
    pr_dlist(stdout,1,dl,1,0,TRUE,TRUE,TRUE,TRUE,MAXCHI);
    fatal_error(0,"Non existant dihedral %d in file %s, line %d",
		Dih,__FILE__,__LINE__);
  }
  return b;
}

static void pr_one_ro(FILE *fp,t_dlist *dl,int nDih,real dt)
{
  int k ; 
  for(k=0;k<NROT;k++)
    fprintf(fp,"  %6.2f",dl->rot_occ[nDih][k]); 
  fprintf(fp,"\n"); 
}

static void pr_ntr_s2(FILE *fp,t_dlist *dl,int nDih,real dt)
{
  fprintf(fp,"  %6.2f  %6.2f\n",(dt == 0) ? 0 : dl->ntr[nDih]/dt,dl->S2[nDih]);
}

void pr_dlist(FILE *fp,int nl,t_dlist dl[],real dt, int printtype, 
bool bPhi, bool bPsi,bool bChi,bool bOmega, int maxchi)
{
  int i, Xi;

  void  (*pr_props)(FILE *, t_dlist *, int, real);  
  
  /* Analysis of dihedral transitions etc */

  if (printtype == edPrintST){
    pr_props=pr_ntr_s2 ; 
    fprintf(stderr,"Now printing out transitions and OPs...\n");
  }else{
    pr_props=pr_one_ro ;     
    fprintf(stderr,"Now printing out rotamer occupancies...\n");
    fprintf(fp,"\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\n");
  }

  /* change atom numbers from 0 based to 1 based */   
  for(i=0; (i<nl); i++) {
    fprintf(fp,"Residue %s\n",dl[i].name);
    if (printtype == edPrintST){
      fprintf(fp," Angle [   AI,   AJ,   AK,   AL]  #tr/ns  S^2D  \n"
	         "--------------------------------------------\n");
    } else {
      fprintf(fp," Angle [   AI,   AJ,   AK,   AL]  rotamers  0  g(-)  t  g(+)\n"
	         "--------------------------------------------\n");
    }
    if (bPhi) {
      fprintf(fp,"   Phi [%5d,%5d,%5d,%5d]",
	    (dl[i].atm.H == -1) ? 1+dl[i].atm.minC : 1+dl[i].atm.H,
	    1+dl[i].atm.N, 1+dl[i].atm.Cn[1], 1+dl[i].atm.C);
      pr_props(fp,&dl[i],edPhi,dt);
    }
    if (bPsi) {
      fprintf(fp,"   Psi [%5d,%5d,%5d,%5d]",1+dl[i].atm.N, 1+dl[i].atm.Cn[1],
	    1+dl[i].atm.C, 1+dl[i].atm.O);
      pr_props(fp,&dl[i],edPsi,dt);
    }
    if (bOmega && has_dihedral(edOmega,&(dl[i]))) {
      fprintf(fp," Omega [%5d,%5d,%5d,%5d]",1+dl[i].atm.minO, 1+dl[i].atm.minC,
	      1+dl[i].atm.N, 1+dl[i].atm.Cn[1]);
      pr_props(fp,&dl[i],edOmega,dt);    
    }
    for(Xi=0; Xi<MAXCHI; Xi++)
      if (bChi && (Xi < maxchi) && (dl[i].atm.Cn[Xi+3] != -1) ) {
	fprintf(fp,"   Chi%d[%5d,%5d,%5d,%5d]",Xi+1, 1+dl[i].atm.Cn[Xi],
		1+dl[i].atm.Cn[Xi+1], 1+dl[i].atm.Cn[Xi+2],
		1+dl[i].atm.Cn[Xi+3]);
	pr_props(fp,&dl[i],Xi+edChi1,dt); /* Xi+2 was wrong here */ 
      }
    fprintf(fp,"\n");
  }
}



int pr_trans(FILE *fp,int nl,t_dlist dl[],real dt,int Xi)
{
  /* never called at the moment */ 

  int  i,nn,nz;
  
  nz=0;
  fprintf(fp,"\\begin{table}[h]\n");
  fprintf(fp,"\\caption{Number of dihedral transitions per nanosecond}\n");
  fprintf(fp,"\\begin{tabular}{|l|l|}\n");
  fprintf(fp,"\\hline\n");
  fprintf(fp,"Residue\t&$\\chi_%d$\t\\\\\n",Xi+1);
  for(i=0; (i<nl); i++) {
    nn=dl[i].ntr[Xi]/dt;
    
    if (nn == 0) {
      fprintf(fp,"%s\t&\\HL{%d}\t\\\\\n",dl[i].name,nn);
      nz++;
    }
    else if (nn > 0)
      fprintf(fp,"%s\t&\\%d\t\\\\\n",dl[i].name,nn);
  }
  fprintf(fp,"\\hline\n");
  fprintf(fp,"\\end{tabular}\n");
  fprintf(fp,"\\end{table}\n\n");

  return nz;  
}

