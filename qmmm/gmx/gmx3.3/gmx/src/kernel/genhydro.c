/*
 * $Id: genhydro.c,v 1.6 2002/02/28 10:54:42 spoel Exp $
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
 * GROningen Mixture of Alchemy and Childrens' Stories
 */
static char *SRCID_genhydro_c = "$Id: genhydro.c,v 1.6 2002/02/28 10:54:42 spoel Exp $";
#include <time.h>
#include <ctype.h>
#include "assert.h"
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "copyrite.h"
#include "string2.h"
#include "confio.h"
#include "symtab.h"
#include "vec.h"
#include "statutil.h"
#include "futil.h"
#include "fatal.h"
#include "physics.h"
#include "calch.h"
#include "genhydro.h"
#include "h_db.h"
#include "ter_db.h"
#include "resall.h"
#include "pgutil.h"

static void copy_atom(t_atoms *atoms1,int a1,t_atoms *atoms2,int a2)
{
  atoms2->atom[a2] = atoms1->atom[a1];
  snew(atoms2->atomname[a2],1);
  *atoms2->atomname[a2]=strdup(*atoms1->atomname[a1]);
}

atom_id pdbasearch_atom(char *name,int resnr,t_atoms *pdba)
{
  int  i;
  
  for(i=0; (i<pdba->nr) && (pdba->atom[i].resnr != resnr); i++)
    ;
    
  return search_atom(name,i,pdba->nr,pdba->atom,pdba->atomname);
}

void hacksearch_atom(int *ii, int *jj, char *name, int nab[], t_hack *ab[], 
		     int resnr, t_atoms *pdba)
{
  int  i,j;
  
  *ii=-1;
  if (name[0] == '-') {
    name++;
    resnr--;
  }
  for(i=0; (i<pdba->nr) && (pdba->atom[i].resnr != resnr); i++)
    ;
  for(   ; (i<pdba->nr) && (pdba->atom[i].resnr == resnr) && (*ii<0); i++)
    for(j=0; (j < nab[i]) && (*ii<0); j++)
      if (ab[i][j].nname && strcmp(name,ab[i][j].nname) == 0) {
	*ii=i;
	*jj=j;
      }
  
  return;
}

void dump_ab(FILE *out,int natom,int nab[], t_hack *ab[], bool bHeader)
{
  int i,j;
  
#define SS(s) (s)?(s):"-"
  /* dump ab */
  if (bHeader)
    fprintf(out,"ADDBLOCK (t_hack) natom=%d\n"
	    "%4s %2s %-4s %-4s %2s %-4s %-4s %-4s %-4s %1s %s\n",
	    natom,"atom","nr","old","new","tp","ai","aj","ak","al","a","x");
  for(i=0; i<natom; i++)
    for(j=0; j<nab[i]; j++)
      fprintf(out,"%4d %2d %-4s %-4s %2d %-4s %-4s %-4s %-4s %s %g %g %g\n",
	      i+1, ab[i][j].nr, SS(ab[i][j].oname), SS(ab[i][j].nname),
	      ab[i][j].tp, 
	      SS(ab[i][j].AI), SS(ab[i][j].AJ),
	      SS(ab[i][j].AK), SS(ab[i][j].AL),
	      ab[i][j].atom?"+":"", 
	      ab[i][j].newx[XX], ab[i][j].newx[YY], ab[i][j].newx[ZZ]);
#undef SS
}

static t_hackblock *get_hackblocks(t_atoms *pdba, int nah, t_hackblock ah[],
				   int nterpairs,
				   t_hackblock **ntdb, t_hackblock **ctdb, 
				   int *rN, int *rC)
{
  int i,rnr;
  t_hackblock *hb,*ahptr;

  /* make space */
  snew(hb,pdba->nres);
  /* first the termini */
  for(i=0; i<nterpairs; i++) {
    copy_t_hackblock(ntdb[i], &hb[rN[i]]);
    merge_t_hackblock(ctdb[i], &hb[rC[i]]);
  }
  /* then the whole hdb */
  for(rnr=0; rnr < pdba->nres; rnr++) {
    ahptr=search_h_db(nah,ah,*pdba->resname[rnr]);
    if ( ahptr ) {
      if (hb[rnr].name==NULL)
	hb[rnr].name=strdup(ahptr->name);
      merge_hacks(ahptr, &hb[rnr]);
    }
  }
  return hb;
}

static char Hnum[] = "123456";

static void expand_hackblocks_one(t_hackblock *hbr, char *atomname, 
				  int *nabi, t_hack **abi, bool bN, bool bC)
{
  int j, k, l, d;
  bool bIgnore;
  /* recursion depth is recorded for debug purposes only: */
  static int depth=-1;
  
  depth++;
  /* we'll recursively add atoms to atoms */
  if (debug) fprintf(debug,"\n[%d] %s:",depth,atomname);
  for(j=0; j < hbr->nhack; j++) {
    /* first check if we're in the N- or C-terminus, then we should ignore 
       all hacks involving atoms from resp. previous or next residue
       (i.e. which name begins with '-' (N) or '+' (C) */
    bIgnore = FALSE;
    if ( bN ) /* N-terminus: ignore '-' */
      for(k=0; k<4 && hbr->hack[j].a[k] && !bIgnore; k++)
	bIgnore = hbr->hack[j].a[k][0]=='-';
    if ( bC ) /* C-terminus: ignore '+' */
      for(k=0; k<4 && hbr->hack[j].a[k] && !bIgnore; k++)
	bIgnore = hbr->hack[j].a[k][0]=='+';
    /* must be either hdb entry (tp>0) or add from tdb (oname==NULL)
       and first control aton (AI) matches this atom or
       delete/replace from tdb (oname!=NULL) and oname matches this atom */
    if (debug) fprintf(debug," %s",
		       hbr->hack[j].oname?hbr->hack[j].oname:hbr->hack[j].AI);
    if ( !bIgnore && 
	 ( ( ( hbr->hack[j].tp > 0 || hbr->hack[j].oname==NULL ) &&
	     strcmp(atomname, hbr->hack[j].AI) == 0 ) ||
	   ( hbr->hack[j].oname!=NULL && 
	     strcmp(atomname, hbr->hack[j].oname) == 0) ) ) {
      /* now expand all hacks for this atom */
      if (debug) fprintf(debug," +%dh",hbr->hack[j].nr);
      srenew(*abi,*nabi + hbr->hack[j].nr);
      for(k=0; k < hbr->hack[j].nr; k++) {
	copy_t_hack(&hbr->hack[j], &(*abi)[*nabi + k]);
	for(d=0; d<DIM; d++)
	  (*abi)[*nabi + k].newx[d]=NOTSET;
	/* if we're adding (oname==NULL) and don't have a new name (nname) 
	   yet, build it from atomname */
	if ( (*abi)[*nabi + k].nname==NULL ) {
	  if ( (*abi)[*nabi + k].oname==NULL ) {
	    (*abi)[*nabi + k].nname=strdup(atomname);
	    (*abi)[*nabi + k].nname[0]='H';
	  }
	} else {
	  sfree((*abi)[*nabi + k].nname);
	  (*abi)[*nabi + k].nname=strdup(hbr->hack[j].nname);
	}
	/* if adding more than one atom, number them */
	if ( hbr->hack[j].nr > 1 ) {
	  l = strlen((*abi)[*nabi + k].nname);
	  srenew((*abi)[*nabi + k].nname, l+2);
	  (*abi)[*nabi + k].nname[l] = Hnum[k]; /* 1, 2, 3 .... */
	  (*abi)[*nabi + k].nname[l+1] = '\0';
	}
      }
      (*nabi) += hbr->hack[j].nr;
      
      /* add hacks to atoms we've just added */
      if ( hbr->hack[j].tp > 0 || hbr->hack[j].oname==NULL )
	for(k=0; k < hbr->hack[j].nr; k++)
	  expand_hackblocks_one(hbr, (*abi)[*nabi-hbr->hack[j].nr+k].nname, 
				nabi, abi, bN, bC);
    }
  }
  depth--;
}

static void expand_hackblocks(t_atoms *pdba, t_hackblock hb[], 
			      int nab[], t_hack *ab[], 
			      int nterpairs, int *rN, int *rC)
{
  int i,j;
  bool bN,bC;
  
  for(i=0; i < pdba->nr; i++) {
    bN = FALSE;
    for(j=0; j<nterpairs && !bN; j++)
      bN = pdba->atom[i].resnr==rN[j];
    bC = FALSE;
    for(j=0; j<nterpairs && !bC; j++)
      bC = pdba->atom[i].resnr==rC[j];

    /* add hacks to this atom */
    expand_hackblocks_one(&hb[pdba->atom[i].resnr], *pdba->atomname[i], 
			  &nab[i], &ab[i], bN, bC);
  }
  if (debug) fprintf(debug,"\n");
}

static int check_atoms_present(t_atoms *pdba, int nab[], t_hack *ab[])
{
  int i, j, k, d, rnr, nadd;
  
  nadd=0;
  for(i=0; i < pdba->nr; i++) {
    rnr = pdba->atom[i].resnr;
    for(j=0; j<nab[i]; j++)
      if ( ab[i][j].oname==NULL ) { 
	/* we're adding */
	assert(ab[i][j].nname!=NULL);
	/* check if the atom is already present */
	k=pdbasearch_atom(ab[i][j].nname, rnr, pdba);
	if ( k != -1 ) {
	  /* we found the added atom, so move the hack there: */
	  srenew(ab[k], nab[k]+1);
	  ab[k][nab[k]] = ab[i][j];
	  ab[k][nab[k]].oname = strdup(ab[k][nab[k]].nname);
	  /* reset any possible new coordinates: */
	  for(d=0; d<DIM; d++)
	    ab[k][nab[k]].newx[d]=NOTSET;
	  /* keep count */
	  nab[k]++;
	  /* remove the hack from this atom: */
	  for(k=j+1; k<nab[i]; k++)
	    ab[i][k-1] = ab[i][k];
	  /* keep count */
	  nab[i]--;
	  j--;
	  srenew(ab[i], nab[i]);
	} else
	  /* count how many atoms we'll add */
	  nadd++;
      } else if ( ab[i][j].nname==NULL )
	/* we're deleting */
	nadd--;
  }
  
  return nadd;
}

static void calc_all_pos(t_atoms *pdba, rvec x[], int nab[], t_hack *ab[])
{
  int i, j, ii, jj, m, ia, d, rnr;
  rvec xa[4]; /* control atoms for calc_h_pos */
  rvec xh[3]; /* hydrogen positions from calc_h_pos */
  
  for(i=0; i < pdba->nr; i++) {
    rnr   = pdba->atom[i].resnr;
    for(j=0; j < nab[i]; j+=ab[i][j].nr) {
      /* check if we're adding: */
      if (ab[i][j].oname==NULL && ab[i][j].tp > 0) {
	for(m=0; m<ncontrol[ab[i][j].tp]; m++) {
	  ia = pdbasearch_atom(ab[i][j].a[m], rnr, pdba);
	  if (ia < 0) {
	    /* not found in original atoms, might still be in t_hack (ab) */
	    hacksearch_atom(&ii, &jj, ab[i][j].a[m], nab, ab, rnr, pdba);
	    if (ii < 0)
	      fatal_error(0,"Atom %s not found in residue %s%d"
			  " while adding hydrogens",
			  ab[i][j].a[m],*pdba->resname[rnr],rnr+1);
	    else
	      copy_rvec(ab[ii][jj].newx, xa[m]);
	  } else
	    copy_rvec(x[ia], xa[m]);
	}
	for(m=0; m<3; m++)
	  for(d=0; d<DIM; d++)
	    if (m<ab[i][j].nr)
	      xh[m][d] = 0;
	    else
	      xh[m][d] = NOTSET;
	calc_h_pos(ab[i][j].tp, xa, xh);
	for(m=0; m<ab[i][j].nr; m++)
	  copy_rvec(xh[m],ab[i][j+m].newx);
      }
    }
  }
}

int add_h(t_atoms **pdbaptr, rvec *xptr[], 
	  int nah, t_hackblock ah[],
	  int nterpairs, t_hackblock **ntdb, t_hackblock **ctdb, 
	  int *rN, int *rC, int **nabptr, t_hack ***abptr,
	  bool bUpdate_pdba, bool bKeep_old_pdba)
{
  t_atoms     *newpdba=NULL,*pdba=NULL;
  bool        bSet;
  int         nadd;
  int         i,newi,j,d,natoms;
  int         *nab=NULL;
  t_hack      **ab=NULL;
  t_hackblock *hb;
  rvec        *xn;
  bool        bKeep_ab;
  
  /* set flags for adding hydrogens (accoring to hdb) */
  pdba=*pdbaptr;
  natoms=pdba->nr;
  
  if (nabptr && abptr) {
    /* the first time these will be pointers to NULL, but we will
       return in them the completed arrays, which we will get back
       the second time */
    nab = *nabptr;
    ab  = *abptr;
    bKeep_ab=TRUE;
    if (debug) fprintf(debug,"pointer to ab found\n");
  } else
    bKeep_ab=FALSE;
  
  if (nab && ab) {
    /* WOW, everything was already figured out */
    bUpdate_pdba = FALSE;
    if (debug) fprintf(debug,"pointer to non-null ab found\n");
  } else {
    /* We'll have to do all the hard work */
    bUpdate_pdba = TRUE;
    /* first get all the hackblocks for each residue: */
    hb = get_hackblocks(pdba, nah, ah, nterpairs, ntdb, ctdb, rN, rC);
    if (debug) dump_hb(debug, pdba->nres, hb);
    
    /* expand the hackblocks to atom level */
    snew(nab,natoms);
    snew(ab,natoms);
    expand_hackblocks(pdba, hb, nab, ab, nterpairs, rN, rC);
    free_t_hackblock(pdba->nres, &hb);
  }
  
  if (debug) { 
    fprintf(debug,"before calc_all_pos\n");
    dump_ab(debug, natoms, nab, ab, TRUE);
  }
  
  /* Now calc the positions */
  calc_all_pos(pdba, *xptr, nab, ab);

  if (debug) { 
    fprintf(debug,"after calc_all_pos\n");
    dump_ab(debug, natoms, nab, ab, TRUE);
  }
  
  if (bUpdate_pdba) {
    /* we don't have to add atoms that are already present in pdba,
       so we will remove them from the ab (t_hack) */
    nadd = check_atoms_present(pdba, nab, ab);
    if (debug) {
      fprintf(debug, "removed add hacks that were already in pdba:\n");
      dump_ab(debug, natoms, nab, ab, TRUE);
      fprintf(debug, "will be adding %d atoms\n",nadd);
    }
    
    /* Copy old atoms, making space for new ones */
    snew(newpdba,1);
    init_t_atoms(newpdba,natoms+nadd,FALSE);
    newpdba->nres    = pdba->nres;   
    sfree(newpdba->resname);
    newpdba->resname = pdba->resname;
  } else {
    nadd = 0;
  }
  if (debug) fprintf(debug,"snew xn for %d old + %d new atoms %d total)\n",
		     natoms, nadd, natoms+nadd);
  snew(xn,natoms+nadd);
  newi=0;
  for(i=0; (i<natoms); i++) {
    /* check if this atom wasn't scheduled for deletion */
    if ( nab[i]==0 || ab[i][0].nname!=NULL ) {
      if (newi >= natoms+nadd) {
	nadd+=10;
	srenew(xn,natoms+nadd);
      }
      if (debug) fprintf(debug,"(%3d) %3d %4s %4s%3d %3d",
			 i+1, newi+1, *pdba->atomname[i],
			 *pdba->resname[pdba->atom[i].resnr],
			 pdba->atom[i].resnr+1, nab[i]);
      if (bUpdate_pdba)
	copy_atom(pdba,i, newpdba,newi);
      copy_rvec((*xptr)[i],xn[newi]);
      /* process the hacks for this atom */
      for(j=0; j<nab[i]; j++) {
	if ( ab[i][j].oname==NULL ) { /* add */
	  newi++;
	  if (newi >= natoms+nadd) {
	    nadd+=10;
	    srenew(xn,natoms+nadd);
	  }
	  if (bUpdate_pdba)
	    newpdba->atom[newi].resnr=pdba->atom[i].resnr;
	  if (debug) fprintf(debug," + %d",newi+1);
	}
	if ( ab[i][j].nname!=NULL ) { /* add or replace */
	  if (bUpdate_pdba) {
	    snew(newpdba->atomname[newi],1);
	    *newpdba->atomname[newi]=strdup(ab[i][j].nname);
	    if ( ab[i][j].oname!=NULL && ab[i][j].atom ) { /* replace */
/* 	      newpdba->atom[newi].m    = ab[i][j].atom->m; */
/* 	      newpdba->atom[newi].q    = ab[i][j].atom->q; */
/* 	      newpdba->atom[newi].type = ab[i][j].atom->type; */
	    }
	  }
	  bSet=TRUE;
	  for(d=0; d<DIM; d++)
	    bSet = bSet && ab[i][j].newx[d]!=NOTSET;
	  if (bSet)
	    copy_rvec(ab[i][j].newx, xn[newi]);
	  if (bUpdate_pdba && debug) 
	    fprintf(debug," %s %g %g",*newpdba->atomname[newi],
		    newpdba->atom[newi].m,newpdba->atom[newi].q);
	}
      }
      newi++;
      if (debug) fprintf(debug,"\n");
    }
  }
  
  if ( bKeep_ab ) {
    *nabptr=nab;
    *abptr=ab;
  } else {
    /* Clean up */
    for(i=0; i<natoms; i++)
      free_t_hack(nab[i], &ab[i]);
    sfree(nab);
    sfree(ab);
  }
    
  if ( bUpdate_pdba ) {
    if ( !bKeep_old_pdba ) {
      for(i=0; i < natoms; i++) {
	sfree(*(pdba->atomname[i]));
	/*       sfree(pdba->atomname[i]); */
      }
      sfree(pdba->atomname);
      sfree(pdba->atom);
      sfree(pdba->pdbinfo);
      done_block(&(pdba->excl));
      sfree(pdba);
    }
    *pdbaptr=newpdba;
  } else
    nadd = newi-natoms;
  
  sfree(*xptr);
  *xptr=xn;
  
  return natoms+nadd;
}

void deprotonate(t_atoms *atoms,rvec *x)
{
  int  i,j;
  
  j=0;
  for(i=0; i<atoms->nr; i++) {
    if ( (*atoms->atomname[i])[0] != 'H' ) {
      atoms->atomname[j]=atoms->atomname[i];
      atoms->atom[j]=atoms->atom[i];
      copy_rvec(x[i],x[j]);
      j++;
    }
  }
  atoms->nr=j;
}

int protonate(t_atoms **atomsptr,rvec **xptr,t_protonate *protdata)
{
#define NTERPAIRS 1
  t_atoms *atoms;
  bool    bUpdate_pdba,bKeep_old_pdba;
  int     nadd;
  
  atoms=NULL;
  if ( !protdata->bInit ) {
    if (debug) fprintf(debug,"protonate: Initializing protdata\n");
    /* set forcefield to use: */
    strcpy(protdata->FF,"ffgmx2");
    /* get the databases: */
    protdata->nah=read_h_db(protdata->FF,&protdata->ah);
    open_symtab(&protdata->tab); 
    protdata->atype=read_atype(protdata->FF,&protdata->tab);
    if(read_ter_db(protdata->FF,'n',&protdata->ntdb,protdata->atype) < 4) 
      fatal_error(0,"no n-terminus db");
    if(read_ter_db(protdata->FF,'c',&protdata->ctdb,protdata->atype) < 2) 
      fatal_error(0,"no c-terminus db");
    
    /* set terminus types: -NH3+ (different for Proline) and -COO- */
    atoms=*atomsptr;
    snew(protdata->sel_ntdb, NTERPAIRS);
    snew(protdata->sel_ctdb, NTERPAIRS);
    if (strncmp(*atoms->resname[atoms->atom[atoms->nr-1].resnr],"PRO",3)==0)
      protdata->sel_ntdb[0]=&(protdata->ntdb[3]);
    else
      protdata->sel_ntdb[0]=&(protdata->ntdb[1]);
    protdata->sel_ctdb[0]=&(protdata->ctdb[1]);
  
    /* set terminal residue numbers: */
    snew(protdata->rN, NTERPAIRS);
    snew(protdata->rC, NTERPAIRS);
    protdata->rN[0]=0;
    protdata->rC[0]=atoms->atom[atoms->nr-1].resnr;
    
    /* keep unprotonated topology: */
    protdata->upatoms = atoms;
    /* we don't have these yet: */
    protdata->patoms = NULL;
    bUpdate_pdba=TRUE;
    bKeep_old_pdba=TRUE;
    
    /* clear hackblocks: */
    protdata->nab=NULL;
    protdata->ab=NULL;
    
    /* set flag to show we're initialized: */
    protdata->bInit=TRUE;
  } else {
    if (debug) fprintf(debug,"protonate: using available protdata\n");
    /* add_h will need the unprotonated topoloy again: */
    atoms = protdata->upatoms;
    bUpdate_pdba=FALSE;
    bKeep_old_pdba=FALSE;
  }
  
  /* now protonate */
  nadd = add_h(&atoms, xptr, protdata->nah, protdata->ah,
	       NTERPAIRS, protdata->sel_ntdb, protdata->sel_ctdb,
	       protdata->rN, protdata->rC,
	       &protdata->nab, &protdata->ab, bUpdate_pdba, bKeep_old_pdba);
  if ( ! protdata->patoms )
    /* store protonated topology */
    protdata->patoms = atoms;
  *atomsptr = protdata->patoms;
  if (debug) fprintf(debug,"natoms: %d -> %d (nadd=%d)\n",
		     protdata->upatoms->nr, protdata->patoms->nr, nadd);
  return nadd;
#undef NTERPAIRS  
}

