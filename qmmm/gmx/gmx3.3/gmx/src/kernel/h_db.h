/*
 * $Id: h_db.h,v 1.3 2002/02/28 10:54:43 spoel Exp $
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

#ifndef _h_db_h
#define _h_db_h

static char *SRCID_h_db_h = "$Id: h_db.h,v 1.3 2002/02/28 10:54:43 spoel Exp $";
#ifdef HAVE_IDENT
#ident	"@(#) h_db.h 1.10 2/2/97"
#endif /* HAVE_IDENT */
#include "sysstuff.h"
#include "hackblock.h"

/* number of control atoms for each 'add' type */
extern int ncontrol[];
  
/* functions for the h-database */

extern void read_ab(char *line,char *fn,t_hack *ab);
/* Read one add block */

extern int read_h_db(char *fn,t_hackblock **ah);
/* Read the database from file */

extern void print_ab(FILE *out,t_hack *ab);
/* print one add block */

extern void print_h_db(FILE *out,int nh,t_hackblock ah[]);
/* Print the database to file */

extern int compaddh(const void *a,const void *b);

extern t_hackblock *search_h_db(int nh,t_hackblock ah[],char *key);
/* Search for an entry in the database */



#endif	/* _h_db_h */
