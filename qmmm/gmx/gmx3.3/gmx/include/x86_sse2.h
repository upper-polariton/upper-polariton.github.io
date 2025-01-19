/*
 * $Id: x86_sse2.h,v 1.2 2002/02/28 21:55:52 spoel Exp $
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

#ifndef _x86_sse2_h
#define _x86_sse2_h

static char *SRCID_x86_sse2_h = "$Id: x86_sse2.h,v 1.2 2002/02/28 21:55:52 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if (defined USE_X86_SSE2 && defined DOUBLE)

void checksse2();
void vecinvsqrt_sse2(double in[],double out[],int n);
void vecrecip_sse2(double in[],double out[],int n);

void inl0100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],int type[],int ntype,double nbfp[],
		  double Vnb[]);
void inl0300_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],int type[],int ntype,double nbfp[],
		  double Vnb[],double tabscale,double VFtab[]);
void inl1000_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[]);
void inl1020_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[]);
void inl1030_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[]);
void inl1100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[]);
void inl2000_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double krf, double crf);
void inl2100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double krf, double crf, int type[],int ntype,
		  double nbfp[],double Vnb[]);
void inl1120_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[]);
void inl2020_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double krf, double crf);
void inl2120_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double krf, double crf, int type[],int ntype,
		  double nbfp[],double Vnb[]);
void inl1130_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[]);
void inl2030_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double krf, double crf);
void inl2130_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double krf, double crf, int type[],int ntype,
		  double nbfp[],double Vnb[]);
void inl3000_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double tabscale,double VFtab[]); 
void inl3020_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double tabscale,double VFtab[]);
void inl3030_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  double tabscale,double VFtab[]);
void inl3100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[],
		  double tabscale, double VFtab[]);
void inl3120_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[],
		  double tabscale, double VFtab[]);
void inl3130_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[],
		  double tabscale, double VFtab[]);
void inl3300_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[],
		  double tabscale,double VFtab[]);
void inl3320_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[],
		  double tabscale,double VFtab[]);
void inl3330_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		  double shiftvec[],double fshift[],int gid[],double pos[],
		  double faction[],double charge[],double facel,double Vc[],
		  int type[],int ntype,double nbfp[],double Vnb[],
		  double tabscale,double VFtab[]);

void mcinl0100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    int type[],int ntype,double nbfp[],
		    double Vnb[]);
void mcinl0300_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    int type[],int ntype,double nbfp[],
		    double Vnb[],double tabscale,double VFtab[]);
void mcinl1000_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[]);
void mcinl1020_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[]);
void mcinl1030_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[]);
void mcinl1100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[]);
void mcinl2000_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double krf, double crf);
void mcinl2100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double krf, double crf, int type[],int ntype,
		    double nbfp[],double Vnb[]);
void mcinl1120_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[]);
void mcinl2020_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double krf, double crf);
void mcinl2120_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double krf, double crf, int type[],int ntype,
		    double nbfp[],double Vnb[]);
void mcinl1130_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[]);
void mcinl2030_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double krf, double crf);
void mcinl2130_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double krf, double crf, int type[],int ntype,
		    double nbfp[],double Vnb[]);
void mcinl3000_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double tabscale,double VFtab[]); 
void mcinl3020_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double tabscale,double VFtab[]);
void mcinl3030_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    double tabscale,double VFtab[]);
void mcinl3100_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[],
		    double tabscale, double VFtab[]);
void mcinl3120_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[],
		    double tabscale, double VFtab[]);
void mcinl3130_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[],
		    double tabscale, double VFtab[]);
void mcinl3300_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[],
		    double tabscale,double VFtab[]);
void mcinl3320_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[],
		    double tabscale,double VFtab[]);
void mcinl3330_sse2(int nri,int iinr[],int jindex[],int jjnr[],int shift[],
		    double shiftvec[],int gid[],double pos[],
		    double charge[],double facel,double Vc[],
		    int type[],int ntype,double nbfp[],double Vnb[],
		    double tabscale,double VFtab[]);

#endif
#endif

 
