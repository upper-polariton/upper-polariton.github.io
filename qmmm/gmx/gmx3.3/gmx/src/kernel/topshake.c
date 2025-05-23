/*
 * $Id: topshake.c,v 1.20 2002/09/15 10:58:07 spoel Exp $
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
static char *SRCID_topshake_c = "$Id: topshake.c,v 1.20 2002/09/15 10:58:07 spoel Exp $";
#include <ctype.h>

#include "sysstuff.h"
#include "physics.h"
#include "macros.h"
#include "readir.h"
#include "typedefs.h"
#include "topshake.h"
#include "toppush.h"
#include "toputil.h"
#include "topdirs.h"
#include "smalloc.h"

static void copy_bond (t_params *pr, int to, int from)
/* copies an entry in a bond list to another position.
 * does no allocing or freeing of memory
 */
{
  memcpy((char*) &(pr->param[to]),(char*) &(pr->param[from]),
	 (size_t)sizeof(pr->param[from]));
}

static int count_hydrogens (char ***atomname, int nra, atom_id a[])
{
  int  i,nh;
  
  if (!atomname) 
    fatal_error(0,"Cannot call count_hydrogens with no atomname (%s %d)",
		__FILE__,__LINE__);
    
  nh=0;
  for (i=0; (i<nra); i++) 
    if (toupper(**(atomname[a[i]]))=='H')
      nh++;
 
  return nh;
}

void make_shake (t_params plist[],t_atoms *atoms,t_atomtype *at,int nshake)
{
  char         ***info=atoms->atomname;
  t_params     *pr;
  t_params     *bonds;
  t_param      p,*bond,*ang;
  real         b_ij,b_jk;
  int          nb,b,i,j,ftype,ftype_a;
  bool         bFound;
  
  if (nshake != eshNONE) {
    switch (nshake) {
    case eshHBONDS:
      printf("turning H bonds into constraints...\n");
      break;
    case eshALLBONDS:
      printf("turning all bonds into constraints...\n");
      break;
    case eshHANGLES:
      printf("turning all bonds and H angles into constraints...\n");
      break;
    case eshALLANGLES:
      printf("turning all bonds and angles into constraints...\n");
      break;
    default:
      fatal_error(0,"Invalid option for make_shake (%d)",nshake);
    }
    
    if ((nshake == eshHANGLES) || (nshake == eshALLANGLES)) {
      /* Add all the angles with hydrogens to the shake list
       * and remove them from the bond list
       */
      for(ftype = 0; (ftype < F_NRE); ftype++) {
	if (interaction_function[ftype].flags & IF_BTYPE) {
	  bonds=&(plist[ftype]);

	  for(ftype_a = 0; (ftype_a < F_NRE); ftype_a++) {
	    if (interaction_function[ftype_a].flags & IF_ATYPE) {
	      pr = &(plist[ftype_a]);
	      
	      for (i=0; (i < pr->nr); ) {
		ang=&(pr->param[i]);
#ifdef DEBUG
		printf("Angle: %d-%d-%d\n",ang->AI,ang->AJ,ang->AK); 
#endif
		if ((nshake == eshALLANGLES) || 
		    (count_hydrogens(info,3,ang->a) > 0)) {
		  /* Can only add hydrogen angle shake, if the two bonds
		   * are constrained.
		   * append this angle to the shake list 
		   */
		  p.AI = ang->AI;
		  p.AJ = ang->AK;
		  
		  /* Calculate length of constraint */
		  bFound=FALSE;
		  b_ij=b_jk=0.0;
		  for (j=0; !bFound && (j<bonds->nr); j++) {
		    bond=&(bonds->param[j]);
		    if (((bond->AI==ang->AI) && 
			 (bond->AJ==ang->AJ)) ||
			((bond->AI==ang->AJ) && 
			 (bond->AJ==ang->AI)))
		      b_ij=bond->C0;
		    if (((bond->AI==ang->AK) && 
			 (bond->AJ==ang->AJ)) ||
			((bond->AI==ang->AJ) && 
			 (bond->AJ==ang->AK)))
		      b_jk=bond->C0;
		    bFound = (b_ij!=0.0) && (b_jk!=0.0);
		  }
		  if (bFound) {
		    /* apply law of cosines */
		    p.C0 = sqrt( b_ij*b_ij + b_jk*b_jk - 
				 2.0*b_ij*b_jk*cos(DEG2RAD*ang->C0) );
		    p.C1 = p.C0;
#ifdef DEBUG
		    printf("p: %d, q: %d, dist: %12.5e\n",p.AI,p.AJ,p.C0);
#endif
		    push_bondnow (&(plist[F_SHAKE]),&p);
		    /* move the last bond to this position */
		    copy_bond (pr,i,pr->nr-1);
		    /* should free memory here!! */
		    pr->nr--;
		  }
		}
		else
		  i++;
	      }
	    } /* if IF_ATYPE */
	  } /* for ftype_A */
	} /* if IF_BTYPE */
      } /* for ftype */
    } /* if shake angles */
  
    /* Add all the bonds with hydrogens to the shake list
     * and remove them from the bond list
     */
    for (ftype=0; (ftype < F_NRE); ftype++) {
      if (interaction_function[ftype].flags & IF_BTYPE) {
	pr = &(plist[ftype]);
	for (i=0; (i < pr->nr); ) {
	  if ( (nshake != eshHBONDS) || 
	       (count_hydrogens (info,2,pr->param[i].a) > 0) ) {
	    /* append this bond to the shake list */
	    p.AI = pr->param[i].AI;
	    p.AJ = pr->param[i].AJ;
	    p.C0 = pr->param[i].C0;
	    p.C1 = pr->param[i].C2;
	    push_bondnow (&(plist[F_SHAKE]),&p);
	    
	    /* move the last bond to this position */
	    copy_bond (pr,i,pr->nr-1);
	    
	    /* should free memory here!! */
	    pr->nr--;
	  }
	  else
	    i++;
	}
      }
    }
  }
  
  /* Add all non-connecting shakes to the shake list and throw away
     the shakenc list */
  for (i=0; i<plist[F_SHAKENC].nr; i++)
    push_bondnow(&(plist[F_SHAKE]), &(plist[F_SHAKENC].param[i]));
  plist[F_SHAKENC].nr=0;
  sfree(plist[F_SHAKENC].param);
  plist[F_SHAKENC].param=NULL;
}
