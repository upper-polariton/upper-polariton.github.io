/*
 * $Id: sysstuff.h,v 1.9 2002/04/01 21:05:48 lindahl Exp $
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

#ifndef _sysstuff_h
#define _sysstuff_h

static char *SRCID_sysstuff_h = "$Id: sysstuff.h,v 1.9 2002/04/01 21:05:48 lindahl Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_IDENT
#ident	"@(#) sysstuff.h 1.22 12/16/92"
#endif /* HAVE_IDENT */

#ifdef CPLUSPLUS
extern "C" { 
#endif

#ifndef _386_
#include <stdlib.h>
#endif
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#if ((!defined WIN32 && !defined _WIN32 && !defined WIN64 && !defined _WIN64) || defined __CYGWIN__ || defined __CYGWIN32__)
#  include <unistd.h>
#endif
#include <limits.h>
#include <time.h>

#ifdef CPLUSPLUS
}
#endif

#endif	/* _sysstuff_h */
