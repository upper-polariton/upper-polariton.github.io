/*
 * $Id: add_par.c,v 1.25 2002/06/13 18:05:17 lindahl Exp $
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
static char *SRCID_add_par_c = "$Id: add_par.c,v 1.25 2002/06/13 18:05:17 lindahl Exp $";
#include <string.h>
#include "typedefs.h"
#include "smalloc.h"
#include "grompp.h"
#include "toputil.h"
#include "hackblock.h"
#include "string2.h"

static void clear_atom_list(int i0, atom_id a[])
{
  int i;
  
  for (i=i0; i < MAXATOMLIST; i++)
    a[i]=-1;
}

static void clear_force_param(int i0, real c[])
{
  int i;
  
  for (i=i0; i < MAXFORCEPARAM; i++)
    c[i]=NOTSET;
}

void add_param(t_params *ps,int ai,int aj, real *c, char *s)
{
  int i;
  
  if ((ai < 0) || (aj < 0)) 
    fatal_error(0,"Trying to add impossible atoms: ai=%d, aj=%d",ai,aj);
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  clear_atom_list(2, ps->param[ps->nr].a);
  if (c==NULL) 
    clear_force_param(0, ps->param[ps->nr].c);
  else
    for(i=0; (i < MAXFORCEPARAM); i++)
      ps->param[ps->nr].c[i]=c[i];
  set_p_string(&(ps->param[ps->nr]),s);
  ps->nr++;
}

void add_imp_param(t_params *ps,int ai,int aj,int ak,int al,real c0, real c1,
		   char *s)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  ps->param[ps->nr].AL=al;
  clear_atom_list  (4, ps->param[ps->nr].a);
  ps->param[ps->nr].C0=c0;
  ps->param[ps->nr].C1=c1;
  clear_force_param(2, ps->param[ps->nr].c);
  set_p_string(&(ps->param[ps->nr]),s);
  ps->nr++;
}

void add_dih_param(t_params *ps,int ai,int aj,int ak,int al,real c0, real c1,
		   real c2,char *s)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  ps->param[ps->nr].AL=al;
  clear_atom_list  (4, ps->param[ps->nr].a);
  ps->param[ps->nr].C0=c0;
  ps->param[ps->nr].C1=c1;
  ps->param[ps->nr].C2=c2;
  clear_force_param(3, ps->param[ps->nr].c);
  set_p_string(&(ps->param[ps->nr]),s);
  ps->nr++;
}

void add_dum2_atoms(t_params *ps,int ai,int aj,int ak)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  clear_atom_list  (3, ps->param[ps->nr].a);
  clear_force_param(0, ps->param[ps->nr].c);
  set_p_string(&(ps->param[ps->nr]),"");
  ps->nr++;
}

void add_dum2_param(t_params *ps,int ai,int aj,int ak,real c0)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  clear_atom_list  (3, ps->param[ps->nr].a);
  ps->param[ps->nr].C0=c0;
  clear_force_param(1, ps->param[ps->nr].c);
  set_p_string(&(ps->param[ps->nr]),"");
  ps->nr++;
}

void add_dum3_param(t_params *ps,int ai,int aj,int ak,int al, 
		    real c0, real c1)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  ps->param[ps->nr].AL=al;
  clear_atom_list  (4, ps->param[ps->nr].a);
  ps->param[ps->nr].C0=c0;
  ps->param[ps->nr].C1=c1;
  clear_force_param(2, ps->param[ps->nr].c);
  set_p_string(&(ps->param[ps->nr]),"");
  ps->nr++;
}

void add_dum3_atoms(t_params *ps,int ai,int aj,int ak,int al, bool bSwapParity)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  ps->param[ps->nr].AL=al;
  clear_atom_list  (4, ps->param[ps->nr].a);
  clear_force_param(0, ps->param[ps->nr].c);
  if (bSwapParity)
    ps->param[ps->nr].C1=-1;
  set_p_string(&(ps->param[ps->nr]),"");
  ps->nr++;
}

void add_dum4_atoms(t_params *ps,int ai,int aj,int ak,int al,int am)
{
  pr_alloc(1,ps);
  ps->param[ps->nr].AI=ai;
  ps->param[ps->nr].AJ=aj;
  ps->param[ps->nr].AK=ak;
  ps->param[ps->nr].AL=al;
  ps->param[ps->nr].AM=am;
  clear_atom_list  (5, ps->param[ps->nr].a);
  clear_force_param(0, ps->param[ps->nr].c);
  set_p_string(&(ps->param[ps->nr]),"");
  ps->nr++;
}

int search_jtype(t_restp *rtp,char *name,bool bNterm)
{
  int  j,k,kmax,jmax;
  char *rtpname,searchname[12];
  
  strcpy(searchname,name);
  
  /* Do a best match comparison */
  /* for protein N-terminus, rename H1, H2 and H3 to H */
  if ( bNterm && (strlen(searchname)==2) && (searchname[0] == 'H') && 
       ( (searchname[1] == '1') || (searchname[1] == '2') || 
	 (searchname[1] == '3') ) )
    searchname[1]='\0';
  kmax=0;
  jmax=-1;
  for(j=0; (j<rtp->natom); j++) {
    rtpname=*(rtp->atomname[j]);
    if (strcasecmp(searchname,rtpname) == 0) {
      jmax=j;
      kmax=strlen(searchname);
      break;
    }
    for(k=0; k < min(strlen(searchname), strlen(rtpname)); k++) 
      if (searchname[k] != rtpname[k])
	break;
    if (k > kmax) {
      kmax=k;
      jmax=j;
    }
  }
  if (jmax == -1)
    fatal_error(0,"Atom %s not found in rtp database in residue %s",
		searchname,rtp->resname);
  if (kmax != strlen(searchname))
    fatal_error(0,"Atom %s not found in rtp database in residue %s, "
		"it looks a bit like %s",
		searchname,rtp->resname,*(rtp->atomname[jmax]));
  return jmax;
}

void cp_param(t_param *dest,t_param *src)
{
  int j;
  
  for(j=0; (j<MAXATOMLIST); j++)
    dest->a[j] = src->a[j];
  for(j=0; (j<MAXFORCEPARAM); j++)
    dest->c[j] = src->c[j];
  strcpy(dest->s,src->s);
}

