/*
 * $Id: topexcl.c,v 1.16 2002/02/28 10:54:45 spoel Exp $
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
static char *SRCID_topexcl_c = "$Id: topexcl.c,v 1.16 2002/02/28 10:54:45 spoel Exp $";
#ifdef HAVE_IDENT
#ident "@(#) topexcl.c 1.38 2/2/97"
#endif

#include "assert.h"
#include "sysstuff.h"
#include "smalloc.h"
#include "macros.h"
#include "topexcl.h"
#include "toputil.h"

typedef struct {
  int ai,aj;
} sortable;

int bond_sort (const void *a, const void *b)
{
  sortable *sa,*sb;
  
  sa = (sortable *) a;
  sb = (sortable *) b;

  if (sa->ai == sb->ai)
    return (sa->aj-sb->aj);
  else
    return (sa->ai-sb->ai);
}

#ifdef DEBUG
#define prints(str, n, s) __prints(str, n, s)
static void __prints(char *str, int n, sortable *s)
{
  int i;

  if (debug) {
    fprintf(debug,"%s\n",str);
    fprintf(debug,"Sortables \n");
    for (i=0; (i < n); i++)
      fprintf(debug,"%d\t%d\n",s[i].ai,s[i].aj);
    
    fflush(debug);
  }
}
#else
#define prints(str, n, s)
#endif

void init_nnb(t_nextnb *nnb, int nr, int nrex)
{
  int i;

  /* initiate nnb */
  nnb->nr   = nr;
  nnb->nrex = nrex;

  snew(nnb->a,nr);
  snew(nnb->nrexcl,nr);
  for (i=0; (i < nr); i++) {
    snew(nnb->a[i],nrex+1);
    snew(nnb->nrexcl[i],nrex+1);
  }
}

static void add_nnb (t_nextnb *nnb, int nre, int i, int j)
{
  srenew(nnb->a[i][nre],nnb->nrexcl[i][nre]+1);
  nnb->a[i][nre][nnb->nrexcl[i][nre]] = j;
  nnb->nrexcl[i][nre]++;
}

void done_nnb (t_nextnb *nnb)
{
  int i,nre;
  
  for (i=0; (i < nnb->nr); i++) {
    for (nre=0; (nre <= nnb->nrex); nre++)
      if (nnb->nrexcl[i][nre] > 0)
	sfree (nnb->a[i][nre]);
    sfree (nnb->nrexcl[i]);
    sfree (nnb->a[i]);
  }
  sfree (nnb->a);
  sfree (nnb->nrexcl);
  nnb->nr   = 0;
  nnb->nrex = 0;
}

#ifdef DEBUG
void __print_nnb(t_nextnb *nnb, char *s)
{
  int i,j,k;

  if(debug) {
    fprintf(debug,"%s\n",s);
    fprintf(debug,"nnb->nr: %d\n",nnb->nr);
    fprintf(debug,"nnb->nrex: %d\n",nnb->nrex);
    for(i=0; (i<nnb->nr); i++)
      for(j=0; (j<=nnb->nrex); j++) {
	fprintf(debug,"nrexcl[%d][%d]: %d, excl: ",i,j,nnb->nrexcl[i][j]);
	for(k=0; (k<nnb->nrexcl[i][j]); k++)
	  fprintf(debug,"%d, ",nnb->a[i][j][k]);
	fprintf(debug,"\n");
      }
  }
}
#endif

void nnb2excl (t_nextnb *nnb, t_block *excl)
{
  int i,j,j_index;
  int nre,nrx,nrs,nr_of_sortables;
  sortable *s;

  srenew(excl->index, nnb->nr+1);
  excl->index[0]=0;
  for (i=0; (i < nnb->nr); i++) {
    /* calculate the total number of exclusions for atom i */
    nr_of_sortables=0;
    for (nre=0; (nre<=nnb->nrex); nre++)
      nr_of_sortables+=nnb->nrexcl[i][nre];

    if (debug)
      fprintf(debug,"nr_of_sortables: %d\n",nr_of_sortables);
    /* make space for sortable array */
    snew(s,nr_of_sortables);

    /* fill the sortable array and sort it */
    nrs=0;
    for (nre=0; (nre <= nnb->nrex); nre++)
      for (nrx=0; (nrx < nnb->nrexcl[i][nre]); nrx++) {
	s[nrs].ai = i;
	s[nrs].aj = nnb->a[i][nre][nrx];
	nrs++;
      }
    assert(nrs==nr_of_sortables);
    prints("nnb2excl before qsort",nr_of_sortables,s);
    if (nr_of_sortables > 1) {
      qsort ((void *)s,nr_of_sortables,(size_t)sizeof(s[0]),bond_sort);
      prints("nnb2excl after qsort",nr_of_sortables,s);
    }

    /* remove duplicate entries from the list */
    j_index=0;
    if (nr_of_sortables > 0) {
      for (j=1; (j < nr_of_sortables); j++)
	if ((s[j].ai!=s[j-1].ai) || (s[j].aj!=s[j-1].aj))
	  s[j_index++]=s[j-1];
      s[j_index++]=s[j-1];
    }
    nr_of_sortables=j_index;
    prints("after rm-double",j_index,s);

    /* make space for arrays */
    srenew(excl->a,excl->nra+nr_of_sortables);

    /* put the sorted exclusions in the target list */
    for (nrs=0; (nrs<nr_of_sortables); nrs++)
      excl->a[excl->nra+nrs] = s[nrs].aj;
    excl->nra+=nr_of_sortables;
    excl->index[i+1]=excl->nra;

    /* cleanup temporary space */
    sfree (s);
  }
  if (debug)
    print_block(debug,"Exclusions","Atom","Excluded",excl);
}

static void do_gen(int nrbonds,	       /* total number of bonds in s	*/
		   sortable *s,        /* bidirectional list of bonds 	*/
		   t_nextnb *nnb)      /* the tmp storage for excl     */
/* Assume excl is initalised and s[] contains all bonds bidirectional */
{
  int i,j,k,n,nb;

  /* exclude self */
  for(i=0; (i<nnb->nr); i++) 
    add_nnb(nnb,0,i,i);
  print_nnb(nnb,"After exclude self");

  /* exclude all the bonded atoms */
  if (nnb->nrex > 0)
    for (i=0; (i < nrbonds); i++)
      add_nnb(nnb,1,s[i].ai,s[i].aj);
  print_nnb(nnb,"After exclude bonds");

  /* for the nr of exclusions per atom */
  for (n=1; (n < nnb->nrex); n++) 

    /* now for all atoms */
    for (i=0; (i < nnb->nr); i++)

      /* for all directly bonded atoms of atom i */
      for (j=0; (j < nnb->nrexcl[i][1]); j++) {

	/* store the 1st neighbour in nb */
	nb = nnb->a[i][1][j];

	/* store all atoms in nb's n-th list into i's n+1-th list */
	for (k=0; (k < nnb->nrexcl[nb][n]); k++)
	  add_nnb(nnb,n+1,i,nnb->a[nb][n][k]);
      }
  print_nnb(nnb,"After exclude rest");
}

static void add_b(t_params *bonds, int *nrf, sortable *s)
{
  int i;
  int ai,aj;
  
  for (i=0; (i < bonds->nr); i++) {
    ai = bonds->param[i].AI;
    aj = bonds->param[i].AJ;
    if ((ai < 0) || (aj < 0)) 
      fatal_error(0,"Impossible atom numbers in bond %d: ai=%d, aj=%d",
		  i,ai,aj);
    /* Add every bond twice */
    s[(*nrf)].ai   = ai;
    s[(*nrf)++].aj = aj;
    s[(*nrf)].aj   = ai;
    s[(*nrf)++].ai = aj;
  }
}

void gen_nnb(t_nextnb *nnb,t_params plist[])
{
  sortable *s;
  int      i,nrbonds,nrf;

  nrbonds=0;
  for(i=0; (i<F_NRE); i++)
    if (IS_CHEMBOND(i))
      /* we need every bond twice (bidirectional) */
      nrbonds += 2*plist[i].nr;
  
  snew(s,nrbonds);

  nrf=0;
  for(i=0; (i<F_NRE); i++)
    if (IS_CHEMBOND(i))
      add_b(&plist[i],&nrf,s);

  /* now sort the bonds */
  prints("gen_excl before qsort",nrbonds,s);
  if (nrbonds > 1) {
    qsort((void *) s,nrbonds,(size_t)sizeof(sortable),bond_sort);
    prints("gen_excl after qsort",nrbonds,s);
  }

  do_gen(nrbonds,s,nnb);
  sfree(s);
}

void generate_excl (int nrexcl,int nratoms,t_params plist[],t_block *excl)
{
  t_nextnb nnb;

  if (nrexcl < 0)
    fatal_error(0,"Can't have %d exclusions...",nrexcl);
  init_nnb(&nnb,nratoms,nrexcl);
  gen_nnb(&nnb,plist);
  excl->nr=nratoms;
  nnb2excl (&nnb,excl);
  done_nnb (&nnb);
}


