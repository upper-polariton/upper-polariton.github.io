/*
 * $Id: trnio.c,v 1.12 2002/04/02 11:33:45 hess Exp $
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
 * Gnomes, ROck Monsters And Chili Sauce
 */
static char *SRCID_trnio_c = "$Id: trnio.c,v 1.12 2002/04/02 11:33:45 hess Exp $";
 
#include <string.h>
#include "sysstuff.h"
#include "smalloc.h"
#include "fatal.h"
#include "txtdump.h"
#include "names.h"
#include "futil.h"
#include "trnio.h"
#include "gmxfio.h"

#define BUFSIZE		128
#define GROMACS_MAGIC   1993

static int nFloatSize(t_trnheader *sh)
{
  int nflsize=0;
  
  if (sh->box_size)
    nflsize = sh->box_size/(DIM*DIM);
  else if (sh->x_size)
    nflsize = sh->x_size/(sh->natoms*DIM);
  else if (sh->v_size)
    nflsize = sh->v_size/(sh->natoms*DIM);
  else if (sh->f_size)
    nflsize = sh->f_size/(sh->natoms*DIM);
  else 
    fatal_error(0,"Can not determine precision of trn file, quit!");
  
  if (((nflsize != sizeof(float)) && (nflsize != sizeof(double))))
    fatal_error(0,"Float size %d. Maybe different CPU?",nflsize);
      
  return nflsize;
}

static bool do_trnheader(int fp,bool bRead,t_trnheader *sh, bool *bOK)
{
  static int magic=GROMACS_MAGIC;
  static char *version = "GMX_trn_file";
  static bool bFirst=TRUE;
  char buf[256];
  bool bDouble;
  
  *bOK=TRUE;

  fio_select(fp);
  if (!do_int(magic))
    return FALSE;
  
  if (bRead) {
    *bOK = *bOK && do_string(buf);
    if (bFirst)
      fprintf(stderr,"trn version: %s ",buf);
  }
  else
    *bOK = *bOK && do_string(version);
  *bOK = *bOK && do_int(sh->ir_size);
  *bOK = *bOK && do_int(sh->e_size);
  *bOK = *bOK && do_int(sh->box_size);
  *bOK = *bOK && do_int(sh->vir_size);
  *bOK = *bOK && do_int(sh->pres_size);
  *bOK = *bOK && do_int(sh->top_size); 
  *bOK = *bOK && do_int(sh->sym_size); 
  *bOK = *bOK && do_int(sh->x_size); 
  *bOK = *bOK && do_int(sh->v_size); 
  *bOK = *bOK && do_int(sh->f_size); 
  
  if (!*bOK) return *bOK; 
  bDouble = (nFloatSize(sh) == sizeof(double));
  fio_setprecision(fp,bDouble);
  if (bRead && bFirst) {
    fprintf(stderr,"(%s precision)\n",bDouble ? "double" : "single");
    bFirst = FALSE;
  }
  
  *bOK = *bOK && do_int(sh->natoms); 
  *bOK = *bOK && do_int(sh->step); 
  *bOK = *bOK && do_int(sh->nre); 
  *bOK = *bOK && do_real(sh->t); 
  *bOK = *bOK && do_real(sh->lambda); 
  
  return *bOK;
}

void pr_trnheader(FILE *fp,int indent,char *title,t_trnheader *sh)
{
  if (sh) {
    indent=pr_title(fp,indent,title);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"box_size    = %d\n",sh->box_size);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"x_size      = %d\n",sh->x_size);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"v_size      = %d\n",sh->v_size);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"f_size      = %d\n",sh->f_size);
    
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"natoms      = %d\n",sh->natoms);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"step        = %d\n",sh->step);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"t           = %e\n",sh->t);
    (void) pr_indent(fp,indent);
    (void) fprintf(fp,"lambda      = %e\n",sh->lambda);
  }
}

static bool do_htrn(int fp,bool bRead,t_trnheader *sh,
		    rvec *box,rvec *x,rvec *v,rvec *f)
{
  matrix pv;
  bool bOK;

  bOK = TRUE;
  if (sh->box_size != 0) bOK = bOK && ndo_rvec(box,DIM);
  if (sh->vir_size != 0) bOK = bOK && ndo_rvec(pv,DIM);
  if (sh->pres_size!= 0) bOK = bOK && ndo_rvec(pv,DIM);
  if (sh->x_size   != 0) bOK = bOK && ndo_rvec(x,sh->natoms);
  if (sh->v_size   != 0) bOK = bOK && ndo_rvec(v,sh->natoms);
  if (sh->f_size   != 0) bOK = bOK && ndo_rvec(f,sh->natoms);

  return bOK;
}

static bool do_trn(int fp,bool bRead,int *step,real *t,real *lambda,
		   rvec *box,int *natoms,rvec *x,rvec *v,rvec *f)
{
  t_trnheader *sh;
  bool bOK;
  
  snew(sh,1);
  if (!bRead) {
    sh->box_size=(box)?sizeof(matrix):0;
    sh->x_size=((x)?(*natoms*sizeof(x[0])):0);
    sh->v_size=((v)?(*natoms*sizeof(v[0])):0);
    sh->f_size=((f)?(*natoms*sizeof(f[0])):0);
    sh->natoms = *natoms;
    sh->step   = *step;
    sh->nre    = 0;
    sh->t      = *t;
    sh->lambda = *lambda;
  }
  if (!do_trnheader(fp,bRead,sh,&bOK))
    return FALSE;
  if (bRead) {
    *natoms = sh->natoms;
    *step   = sh->step;
    *t      = sh->t;
    *lambda = sh->lambda;
    if (sh->ir_size)
      fatal_error(0,"inputrec in trn file");
    if (sh->e_size)
      fatal_error(0,"energies in trn file");
    if (sh->top_size)
      fatal_error(0,"topology in trn file");
    if (sh->sym_size)
      fatal_error(0,"symbol table in trn file");
  }
  bOK = do_htrn(fp,bRead,sh,box,x,v,f);

  sfree(sh);
  
  return bOK;
}

/************************************************************
 *
 *  The following routines are the exported ones
 *
 ************************************************************/
 
void read_trnheader(char *fn,t_trnheader *trn)
{
  int  fp;
  bool bOK;
  
  fp = open_trn(fn,"r");
  if (!do_trnheader(fp,TRUE,trn,&bOK))
    fatal_error(0,"Empty file %s",fn);
  close_trn(fp);
}

bool fread_trnheader(int fp,t_trnheader *trn, bool *bOK)
{
  return do_trnheader(fp,TRUE,trn,bOK);
}

void write_trn(char *fn,int step,real t,real lambda,
	       rvec *box,int natoms,rvec *x,rvec *v,rvec *f)
{
  int fp;
  
  fp = open_trn(fn,"w");
  do_trn(fp,FALSE,&step,&t,&lambda,box,&natoms,x,v,f);
  close_trn(fp);
}

void read_trn(char *fn,int *step,real *t,real *lambda,
	      rvec *box,int *natoms,rvec *x,rvec *v,rvec *f)
{
  int fp;
  
  fp = open_trn(fn,"r");
  (void) do_trn(fp,TRUE,step,t,lambda,box,natoms,x,v,f);
  close_trn(fp);
}

void fwrite_trn(int fp,int step,real t,real lambda,
		rvec *box,int natoms,rvec *x,rvec *v,rvec *f)
{
  (void) do_trn(fp,FALSE,&step,&t,&lambda,box,&natoms,x,v,f);
}


bool fread_trn(int fp,int *step,real *t,real *lambda,
	       rvec *box,int *natoms,rvec *x,rvec *v,rvec *f)
{
  return do_trn(fp,TRUE,step,t,lambda,box,natoms,x,v,f);
}

bool fread_htrn(int fp,t_trnheader *trn,rvec *box,rvec *x,rvec *v,rvec *f)
{
  return do_htrn(fp,TRUE,trn,box,x,v,f);
}

int open_trn(char *fn,char *mode)
{
  return fio_open(fn,mode);
}

void close_trn(int fp)
{
  fio_close(fp);
}
