/*
 * $Id: string2.h,v 1.13 2002/04/01 21:05:47 lindahl Exp $
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

#ifndef _string2_h
#define _string2_h

static char *SRCID_string2_h = "$Id: string2.h,v 1.13 2002/04/01 21:05:47 lindahl Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_IDENT
#ident	"@(#) string2.h 1.13 11/23/92"
#endif /* HAVE_IDENT */

/*
 *
 * string2.h
 * David van der Spoel
 *
 */

#ifdef CPLUSPLUS
extern "C" {
#endif

#include <string.h>
#include <stdio.h>
#include "typedefs.h"

#define CONTINUE    '\\'
#define COMMENTSIGN ';'

extern int continuing(char *s);

extern char *fgets2(char *s, int n, FILE *stream);

extern void strip_comment (char *line);

extern int break_line (char *line,
		       char *variable,
		       char *value);

extern void upstring (char *str);

extern void ltrim (char *str);

extern void rtrim (char *str);

extern void trim (char *str);

extern void nice_header (FILE *out,char *fn);

extern int strcasecmp_min(const char *str1, const char *str2);
/* This funny version of strcasecmp, is not only case-insensitive,
 * but also ignores '-' and '_'.
 */

extern int gmx_strcasecmp(const char *str1, const char *str2);
extern int gmx_strncasecmp(const char *str1, const char *str2, int n);

extern char *gmx_strdup(const char *src);

#ifndef HAVE_STRCASECMP
#define strcasecmp gmx_strcasecmp
#define strncasecmp gmx_strncasecmp
#endif

#ifndef HAVE_STRDUP
#define strdup gmx_strdup
#endif

extern char *wrap_lines(char *buf,int line_width, int indent);
/* wraps lines at 'linewidth', indenting all following
 * lines by 'indent' spaces. A temp buffer is allocated and returned,
 * which can be disposed of if no longer needed.
 */

#ifdef CPLUSPLUS
}
#endif

#endif	/* _string2_h */
