/*
 * $Id: fgrid.h,v 1.6 2002/02/28 11:07:09 spoel Exp $
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
 * Glycine aRginine prOline Methionine Alanine Cystine Serine
 */

#ifndef _fgrid_h
#define _fgrid_h

static char *SRCID_fgrid_h = "$Id: fgrid.h,v 1.6 2002/02/28 11:07:09 spoel Exp $";
#ifdef HAVE_IDENT
#ident	"@(#) fgrid.h 1.3 9/29/92"
#endif /* HAVE_IDENT */

#include <typedefs.h>
#include <xdlg.h>

typedef struct {
  edlgitem edlg;
  bool bDef;
  int  nname;
  char **name;
  char *set,*get,*def,*help;
} t_fitem;

typedef struct {
  char *name;
  int x,y,w,h;
  int nfitem;
  t_fitem **fitem;
} t_fgroup;

typedef struct {
  int x,y,w,h;
  t_fitem *fitem;
} t_fsimple;

typedef struct {
  int       w,h;
  int       nfgroup;
  t_fgroup  **fgroup;
  int       nfsimple;
  t_fsimple **fsimple;
} t_fgrid;

typedef enum {
  eGRIDEXP, eACCOEXP, eACCCEXP, eGRPEXP, eITEMEXP, eSAMEPOINT, 
  eTOOWIDE, eTOOHIGH, eQUOTE,   eNOVALS 
  } eDLGERR;

extern void ReadDlgErr(char *infile, eDLGERR err, char *s);

extern t_fgrid *FGridFromFile(char *infile);

extern void DoneFGrid(t_fgrid *fgrid);

extern void DumpFGrid(t_fgrid *fgrid);

extern void ReadQuoteString(char *infile, FILE *in, char *buf);

#endif	/* _fgrid_h */
