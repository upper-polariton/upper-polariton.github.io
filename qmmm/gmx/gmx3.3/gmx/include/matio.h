/*
 * $Id: matio.h,v 1.15 2002/02/28 21:55:49 spoel Exp $
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
 * Getting the Right Output Means no Artefacts in Calculating Stuff
 */

#ifndef _matio_h
#define _matio_h

static char *SRCID_matio_h = "$Id: matio.h,v 1.15 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_IDENT
#ident	"@(#) matio.h 1.11 5/20/97"
#endif /* HAVE_IDENT */
#include "typedefs.h"

extern bool matelmt_cmp(t_xpmelmt e1, t_xpmelmt e2);

extern t_matelmt searchcmap(int n,t_mapping map[],t_xpmelmt c);
/* Seach in the map for code 'c' and return entry number. 
 * return -1 if not found
 */

extern int getcmap(FILE *in,char *fn,t_mapping **map);
/* Read the mapping table from in, return number of entries */

extern int readcmap(char *fn,t_mapping **map);
/* Read the mapping table from fn, return number of entries */

extern void printcmap(FILE *out,int n,t_mapping map[]);
/* print mapping table to out */

extern void writecmap(char *fn,int n,t_mapping map[]);
/* print mapping table to fn */

extern int read_xpm_matrix(char *fnm, t_matrix **matrix);
/* Reads a number of matrices from .xpm file fnm and returns this number */

extern real **matrix2real(t_matrix *matrix,real **mat);
/* Converts an matrix in a t_matrix struct to a matrix of reals
 * When mat==NULL memory will be allocated 
 * Returns NULL when something went wrong
 */

extern void write_xpm_m(FILE *out, t_matrix m);
/* Writes a t_matrix struct to .xpm file */ 

extern void write_xpm3(FILE *out,
		       char *title,char *legend,char *label_x,char *label_y,
		       int n_x,int n_y,real axis_x[],real axis_y[],
		       real *matrix[],real lo,real mid,real hi,
		       t_rgb rlo,t_rgb rmid,t_rgb rhi,int *nlevels);
/* See write_xpm.
 * Writes a colormap varying as rlo -> rmid -> rhi.
 */

extern void write_xpm(FILE *out,
		      char *title,char *legend,char *label_x,char *label_y,
		      int n_x,int n_y,real t_x[],real t_y[],
		      real *matrix[],real lo,real hi,
		      t_rgb rlo,t_rgb rhi,int *nlevels);
/* out        xpm file
 * title      matrix title
 * legend     label for the continuous legend
 * label_x    label for the x-axis
 * label_y    label for the y-axis
 * n_x, n_y   size of the matrix
 * axis_x[]   the x-ticklabels
 * axis_y[]   the y-ticklables
 * *matrix[]  element x,y is matrix[x][y]
 * lo         output lower than lo is set to lo
 * hi         output higher than hi is set to hi
 * rlo        rgb value for level lo
 * rhi        rgb value for level hi
 * nlevels    number of color levels for the output
 */

extern real **mk_matrix(int nx, int ny, bool b1D);

extern void done_matrix(int nx, real ***m);

extern void clear_matrix(int nx, int ny, real **m);

#endif	/* _matio_h */
