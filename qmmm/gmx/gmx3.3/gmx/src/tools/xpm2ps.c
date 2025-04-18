/*
 * $Id: xpm2ps.c,v 1.49 2002/09/15 12:01:08 spoel Exp $
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
 * Green Red Orange Magenta Azure Cyan Skyblue
 */
static char *SRCID_xpm2ps_c = "$Id: xpm2ps.c,v 1.49 2002/09/15 12:01:08 spoel Exp $";
#include <math.h>
#include "string2.h"
#include "copyrite.h"
#include "typedefs.h"
#include "macros.h"
#include "statutil.h"
#include "writeps.h"
#include "futil.h"
#include "fatal.h"
#include "smalloc.h"
#include "string2.h"
#include "matio.h"
#include "viewit.h"

#define FUDGE 1.2
#define DDD   2

typedef struct {
  real major;
  real minor;
  real offset;
  bool first;
  int  lineatzero;
  real majorticklen;
  real minorticklen;
  char label[STRLEN];
  real fontsize;
  char font[STRLEN];
  real tickfontsize;
  char tickfont[STRLEN];
} t_axisdef;

typedef struct {
  int  bw;
  real linewidth;
  real xoffs,yoffs;
  bool bTitle;
  bool bTitleOnce;
  bool bYonce;
  real titfontsize;
  char titfont[STRLEN];
  bool legend;
  real legfontsize;
  char legfont[STRLEN];
  char leglabel[STRLEN];
  char leg2label[STRLEN];
  real xboxsize;
  real yboxsize;
  real boxspacing;
  real boxlinewidth;
  real ticklinewidth;
  real zerolinewidth;
  t_axisdef X,Y;
} t_psrec;

/* MUST correspond to char *legend[] in main() */
enum { elSel, elBoth, elFirst, elSecond, elNone, elNR };

/* MUST correspond to char *combine[] in main() */
enum { ecSel, ecHalves, ecAdd, ecSub, ecMult, ecDiv, ecNR };

  void get_params(char *mpin,char *mpout,t_psrec *psr)
{
  static char *bools[BOOL_NR+1]  = { "no", "yes", NULL };
  /* this must correspond to t_rgb *linecolors[] below */
  static char *colors[] = { "none", "black", "white", NULL };
  t_inpfile *inp;
  char      *tmp;
  int       ninp;
  
  if (mpin)
    inp = read_inpfile(mpin,&ninp);
  else
    inp = NULL;
  ETYPE("black&white",    psr->bw,             bools);
  RTYPE("linewidth",      psr->linewidth,      1.0);
  STYPE("titlefont",      psr->titfont,        "Helvetica");
  RTYPE("titlefontsize",  psr->titfontsize,    20.0);
  ETYPE("legend",         psr->legend,         bools);
  STYPE("legendfont",     psr->legfont,        psr->titfont);
  STYPE("legendlabel",    psr->leglabel,       "");
  STYPE("legend2label",   psr->leg2label,      psr->leglabel);
  RTYPE("legendfontsize", psr->legfontsize,    14.0);
  RTYPE("xbox",           psr->xboxsize,       2.0);
  RTYPE("ybox",           psr->yboxsize,       psr->xboxsize);
  RTYPE("matrixspacing",  psr->boxspacing,     20.0);
  RTYPE("xoffset",        psr->xoffs,          0.0);
  RTYPE("yoffset",        psr->yoffs,          psr->xoffs);
  RTYPE("boxlinewidth",   psr->boxlinewidth,   psr->linewidth);
  RTYPE("ticklinewidth",  psr->ticklinewidth,  psr->linewidth);
  RTYPE("zerolinewidth",  psr->zerolinewidth,  psr->ticklinewidth);
  ETYPE("x-lineat0value", psr->X.lineatzero,   colors);
  RTYPE("x-major",        psr->X.major,        NOTSET);
  RTYPE("x-minor",        psr->X.minor,        NOTSET);
  RTYPE("x-firstmajor",   psr->X.offset,       0.0);
  ETYPE("x-majorat0",     psr->X.first,        bools);
  RTYPE("x-majorticklen", psr->X.majorticklen, 8.0);
  RTYPE("x-minorticklen", psr->X.minorticklen, 4.0);
  STYPE("x-label",        psr->X.label,        "");
  RTYPE("x-fontsize",     psr->X.fontsize,     16.0);
  STYPE("x-font",         psr->X.font,         psr->titfont);
  RTYPE("x-tickfontsize", psr->X.tickfontsize, 10.0);
  STYPE("x-tickfont",     psr->X.tickfont,     psr->X.font);
  ETYPE("y-lineat0value", psr->Y.lineatzero,   colors);
  RTYPE("y-major",        psr->Y.major,        psr->X.major);
  RTYPE("y-minor",        psr->Y.minor,        psr->X.minor);
  RTYPE("y-firstmajor",   psr->Y.offset,       psr->X.offset);
  ETYPE("y-majorat0",     psr->Y.first,        bools);
  RTYPE("y-majorticklen", psr->Y.majorticklen, psr->X.majorticklen);
  RTYPE("y-minorticklen", psr->Y.minorticklen, psr->X.minorticklen);
  STYPE("y-label",        psr->Y.label,        psr->X.label);
  RTYPE("y-fontsize",     psr->Y.fontsize,     psr->X.fontsize);
  STYPE("y-font",         psr->Y.font,         psr->X.font);
  RTYPE("y-tickfontsize", psr->Y.tickfontsize, psr->X.tickfontsize);
  STYPE("y-tickfont",     psr->Y.tickfont,     psr->Y.font);
  if (mpout)
    write_inpfile(mpout,ninp,inp);
}

t_rgb black = { 0, 0, 0 };
t_rgb white = { 1, 1, 1 };
t_rgb red   = { 1, 0, 0 };
t_rgb blue  = { 0, 0, 1 };
#define BLACK (&black)
/* this must correspond to *colors[] in get_params */
t_rgb *linecolors[] = { NULL, &black, &white, NULL };

bool diff_maps(int nmap1,t_mapping *map1,int nmap2,t_mapping *map2)
{
  int i;
  bool bDiff,bColDiff=FALSE;

  if (nmap1 != nmap2) 
      bDiff=TRUE;
    else {
      bDiff=FALSE;
      for(i=0; i<nmap1; i++) {
	if (!matelmt_cmp(map1[i].code, map2[i].code)) bDiff=TRUE;
	if (strcmp(map1[i].desc,map2[i].desc) != 0) bDiff=TRUE;
	if ((map1[i].rgb.r!=map2[i].rgb.r) ||
	    (map1[i].rgb.g!=map2[i].rgb.g) ||
	    (map1[i].rgb.b!=map2[i].rgb.b))
	  bColDiff=TRUE;
      }
      if (!bDiff && bColDiff)
	fprintf(stderr,"Warning: two colormaps differ only in RGB value, using one colormap.\n");
    }
  
  return bDiff;
}
  
void leg_discrete(FILE *ps,real x0,real y0,char *label,
		  real fontsize,char *font,int nmap,t_mapping map[])
{
  int   i;
  real  yhh;
  real  boxhh;
  
  boxhh=fontsize+DDD;
  /* LANDSCAPE */
  ps_rgb(ps,BLACK);
  ps_strfont(ps,font,fontsize);
  yhh=y0+fontsize+3*DDD;
  if ((int)strlen(label) > 0)
    ps_ctext(ps,x0,yhh,label,eXLeft);
  ps_moveto(ps,x0,y0);
  for(i=0; (i<nmap); i++) {
    ps_setorigin(ps);
    ps_rgb(ps,&(map[i].rgb));
    ps_fillbox(ps,DDD,DDD,DDD+fontsize,boxhh-DDD);
    ps_rgb(ps,BLACK);
    ps_box(ps,DDD,DDD,DDD+fontsize,boxhh-DDD);
    ps_ctext(ps,boxhh+2*DDD,fontsize/3,map[i].desc,eXLeft);
    ps_unsetorigin(ps);
    ps_moverel(ps,DDD,-fontsize/3);
  }
}

void leg_continuous(FILE *ps,real x0,real x,real y0,char *label,
		    real fontsize,char *font,
		    int nmap,t_mapping map[])
{
  int   i;
  real  xx0;
  real  yhh,boxxh,boxyh;
  
  boxyh=fontsize;
  if (x<8*fontsize)
    x=8*fontsize;
  boxxh=(real)x/(real)nmap;
  if (boxxh>fontsize)
    boxxh=fontsize;
  
  /* LANDSCAPE */
  xx0=x0-(nmap*boxxh)/2.0;
  
  for(i=0; (i<nmap); i++) {
    ps_rgb(ps,&(map[i].rgb));
    ps_fillbox(ps,xx0+i*boxxh,y0,xx0+(i+1)*boxxh,y0+boxyh);
  }
  ps_strfont(ps,font,fontsize);
  ps_rgb(ps,BLACK);
  ps_box(ps,xx0,y0,xx0+nmap*boxxh,y0+boxyh);
  
  yhh=y0+boxyh+3*DDD;
  ps_ctext(ps,xx0+boxxh/2,yhh,map[0].desc,eXCenter);
  if ((int)strlen(label) > 0)
    ps_ctext(ps,x0,yhh,label,eXCenter);
  ps_ctext(ps,xx0+(nmap*boxxh)-boxxh/2,yhh,map[nmap-1].desc,eXCenter);
}

void leg_bicontinuous(FILE *ps,real x0,real x,real y0,char *label1,char *label2,
		      real fontsize,char *font,
		      int nmap1,t_mapping map1[],int nmap2,t_mapping map2[])
{
  real xx1,xx2,x1,x2;
  
  x1=x/(nmap1+nmap2)*nmap1;/* width of legend 1 */
  x2=x/(nmap1+nmap2)*nmap2;/* width of legend 2 */
  xx1=x0-(x2/2.0)-fontsize;/* center of legend 1 */
  xx2=x0+(x1/2.0)+fontsize;/* center of legend 2 */
  x1-=fontsize/2;/* adjust width */
  x2-=fontsize/2;/* adjust width */
  leg_continuous(ps,xx1,x1,y0,label1,fontsize,font,nmap1,map1);
  leg_continuous(ps,xx2,x2,y0,label2,fontsize,font,nmap2,map2);
}

static real box_height(t_matrix *mat,t_psrec *psr)
{
  return mat->ny*psr->yboxsize; 
}

static real box_dh(t_psrec *psr)
{
  return psr->boxspacing;
}

#define IS_ONCE (i==nmat-1)
static real box_dh_top(bool bOnce, t_psrec *psr)
{
  real dh;
  
  if (psr->bTitle || (psr->bTitleOnce && bOnce) )
    dh=2*psr->titfontsize;
  else
    dh=0;
  
  return  dh;
}

static bool box_do_all_x_maj_ticks(t_psrec *psr)
{
  return (psr->boxspacing>(1.5*psr->X.majorticklen));
}

static bool box_do_all_x_min_ticks(t_psrec *psr)
{
  return (psr->boxspacing>(1.5*psr->X.minorticklen));
}

static void draw_boxes(FILE *out,real x0,real y0,real w,
		       int nmat,t_matrix mat[],t_psrec *psr)
{
  char   buf[12];
  char   *mylab;
  real   xxx;
  char   **xtick,**ytick;
  real   xx,yy,dy,xx00,yy00;
  int    i,j,x,y,strlength;
  
  /* Only necessary when there will be no y-labels */ 
  strlength = 0;
  
  /* Draw the box */
  ps_rgb(out,BLACK);
  ps_linewidth(out,psr->boxlinewidth);
  yy00=y0;
  for(i=0; (i<nmat); i++) {
    dy=box_height(&(mat[i]),psr);
    ps_box(out,x0-1,yy00-1,x0+w+1,yy00+dy+1);
    yy00+=dy+box_dh(psr)+box_dh_top(IS_ONCE,psr);
  }
  
  /* Draw the ticks on the axes */
  ps_linewidth(out,psr->ticklinewidth);
  xx00=x0-1;
  yy00=y0-1;
  for (i=0; (i<nmat); i++) {
    snew(xtick,mat[i].nx);
    for(j=0; (j<mat[i].nx); j++) {
      sprintf(buf,"%g",mat[i].axis_x[j]);
      xtick[j]=strdup(buf);
    }
    ps_strfont(out,psr->X.tickfont,psr->X.tickfontsize);
    for(x=0; (x<mat[i].nx); x++) {
      xx=xx00+(x+0.7)*psr->xboxsize;
      if ( ( bRmod(mat[i].axis_x[x] - psr->X.offset, psr->X.major) || 
	     (psr->X.first && (x==0))) &&
	   ( (i == 0) || box_do_all_x_maj_ticks(psr) ) ) {
	/* Longer tick marks */
	ps_line (out,xx,yy00,xx,yy00-psr->X.majorticklen);
	/* Plot label on lowest graph only */
	if (i == 0)
	  ps_ctext(out,xx,
		   yy00-DDD-psr->X.majorticklen-psr->X.tickfontsize*0.8,
		   xtick[x],eXCenter);
      } else if ( bRmod(mat[i].axis_x[x] - psr->X.offset, psr->X.minor) &&
		( (i == 0) || box_do_all_x_min_ticks(psr) ) ){
	/* Shorter tick marks */
	ps_line(out,xx,yy00,xx,yy00-psr->X.minorticklen);
      } else if ( bRmod(mat[i].axis_x[x] - psr->X.offset, psr->X.major) ) {
	/* Even shorter marks, only each X.major */
	ps_line(out,xx,yy00,xx,yy00-(psr->boxspacing/2));
      }
    }
    ps_strfont(out,psr->Y.tickfont,psr->Y.tickfontsize);
    snew(ytick,mat[i].ny);
    for(j=0; (j<mat[i].ny); j++) {
      sprintf(buf,"%g",mat[i].axis_y[j]);
      ytick[j]=strdup(buf);
    }

    for(y=0; (y<mat[i].ny); y++) {
      yy=yy00+(y+0.7)*psr->yboxsize;
      if ( bRmod(mat[i].axis_y[y] - psr->Y.offset, psr->Y.major) || 
	   (psr->Y.first && (y==0))) {
	/* Major ticks */
	strlength=max(strlength,(int)strlen(ytick[y]));
	ps_line (out,xx00,yy,xx00-psr->Y.majorticklen,yy);
	ps_ctext(out,xx00-psr->Y.majorticklen-DDD,
		 yy-psr->Y.tickfontsize/3.0,ytick[y],eXRight);
      }
      else if ( bRmod(mat[i].axis_y[y] - psr->Y.offset, psr->Y.minor) ) {
	/* Minor ticks */
	ps_line(out,xx00,yy,xx00-psr->Y.minorticklen,yy);
      }
    }
    sfree(xtick);
    sfree(ytick);
    
    /* Label on Y-axis */
    if (!psr->bYonce || i==nmat/2) {
      if (strlen(psr->Y.label) > 0) 
	mylab = psr->Y.label;
      else
	mylab = mat[i].label_y;
      if (strlen(mylab) > 0) {
	fprintf(out,"%%%% Printing Y-label\n");
	ps_strfont(out,psr->Y.font,psr->Y.fontsize);
	ps_rotate(out,TRUE);
	xxx=x0-psr->X.majorticklen-psr->X.tickfontsize*strlength-DDD;
	ps_ctext(out,yy00+box_height(&mat[i],psr)/2.0,612.5-xxx,
		 mylab,eXCenter);
	ps_rotate(out,FALSE);
      }
    }
    
    yy00+=box_height(&(mat[i]),psr)+box_dh(psr)+box_dh_top(IS_ONCE,psr);
  }
  /* Label on X-axis */  
  if (strlen(psr->X.label) > 0) 
    mylab = psr->X.label;
  else
    mylab = mat[0].label_x;
  if (strlen(mylab) > 0) {
    fprintf(out,"%%%% Printing X-label\n");
    ps_strfont(out,psr->X.font,psr->X.fontsize);
    ps_ctext(out,x0+w/2,y0-DDD-psr->X.majorticklen-psr->X.tickfontsize*FUDGE-
	     psr->X.fontsize,mylab,eXCenter);
  }
}

static void draw_zerolines(FILE *out,real x0,real y0,real w,
			   int nmat,t_matrix mat[],t_psrec *psr)
{
  real   xx,yy,dy,xx00,yy00;
  int    i,x,y;
  
  xx00=x0-1.5;
  yy00=y0-1.5;
  ps_linewidth(out,psr->zerolinewidth);
  for (i=0; (i<nmat); i++) {
    dy=box_height(&(mat[i]),psr);
    /* mat[i].axis_x and _y were already set by draw_boxes */
    if (psr->X.lineatzero) {
      ps_rgb(out,linecolors[psr->X.lineatzero]);
      for(x=0; (x<mat[i].nx); x++) {
	xx=xx00+(x+0.7)*psr->xboxsize;
      /* draw lines whenever tick label almost zero (e.g. next trajectory) */
	if ( x!=0 && x<mat[i].nx-1 &&
	     abs(mat[i].axis_x[x]) < 
	     0.1*abs(mat[i].axis_x[x+1]-mat[i].axis_x[x]) ) {
	  ps_line (out,xx,yy00,xx,yy00+dy+2);
	}
      }
    }
    if (psr->Y.lineatzero) {
      ps_rgb(out,linecolors[psr->Y.lineatzero]);
      for(y=0; (y<mat[i].ny); y++) {
	yy=yy00+(y+0.7)*psr->yboxsize;
	/* draw lines whenever tick label almost zero (e.g. next trajectory) */
	if ( y!=0 && y<mat[i].ny-1 && 
	     abs(mat[i].axis_y[y]) < 
	     0.1*abs(mat[i].axis_y[y+1]-mat[i].axis_y[y]) ) {
	  ps_line (out,xx00,yy,xx00+w+2,yy);
	}
      }
    }
    yy00+=box_height(&(mat[i]),psr)+box_dh(psr)+box_dh_top(IS_ONCE,psr);
  }
}

static void box_dim(int nmat,t_matrix mat[],t_matrix *mat2,t_psrec *psr,
		    int elegend,bool bFrame,
		    real *w,real *h,real *dw,real *dh)
{
  int i,maxytick;
  real ww,hh,dww,dhh;
  
  hh=dww=dhh=0;
  maxytick=0;
  
  ww=0;
  for(i=0; (i<nmat); i++) {
    ww=max(ww,mat[i].nx*psr->xboxsize);
    hh+=box_height(&(mat[i]),psr);
    maxytick=max(maxytick,mat[i].nx);
  }
  if (bFrame) {
    if (mat[0].label_y[0])
      dww+=2.0*(psr->Y.fontsize+DDD);
    if (psr->Y.major > 0) 
      dww += psr->Y.majorticklen + DDD + 
	psr->Y.tickfontsize*(log(maxytick)/log(10.0));
    else if (psr->Y.minor > 0)
      dww+=psr->Y.minorticklen;
    
    if (mat[0].label_x[0])
      dhh+=psr->X.fontsize+2*DDD;
    if (/* fool emacs auto-indent */
	(elegend==elBoth && (mat[0].legend[0] || mat2[0].legend[0])) ||
	(elegend==elFirst && mat[0].legend[0]) ||
	(elegend==elSecond && mat2[0].legend[0]) )
      dhh+=2*(psr->legfontsize*FUDGE+2*DDD);
    else 
      dhh+=psr->legfontsize*FUDGE+2*DDD;
    if (psr->X.major > 0)
    dhh+=psr->X.tickfontsize*FUDGE+2*DDD+psr->X.majorticklen;
    else if (psr->X.minor > 0)
      dhh+=psr->X.minorticklen;
    
    hh+=(nmat-1)*box_dh(psr);
    hh+=box_dh_top(TRUE,psr);
    if (nmat>1)
      hh+=(nmat-1)*box_dh_top(FALSE,psr);
  }
  *w=ww;
  *h=hh;
  *dw=dww;
  *dh=dhh;
}

int add_maps(t_mapping **newmap, 
	     int nmap1, t_mapping map1[], int nmap2, t_mapping map2[])
{
  static char mapper[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-_=+{}|;:',<.>/?"; 
#define NMAP strlen(mapper)
  int       nmap,j,k;
  t_mapping *map;
  
  nmap=nmap1+nmap2;
  if (nmap > NMAP*NMAP) 
    fatal_error(0,"Not enough symbols to merge the two colormaps\n");
  printf("Combining colormaps of %d and %d elements into one of %d elements\n",
	 nmap1, nmap2, nmap);
  snew(map,nmap);
  for(j=0; j<nmap1; j++) {
    map[j].code.c1=mapper[j % NMAP];
    if (nmap > NMAP)
      map[j].code.c2=mapper[j/NMAP];
    map[j].rgb.r = map1[j].rgb.r;
    map[j].rgb.g = map1[j].rgb.g;
    map[j].rgb.b = map1[j].rgb.b;
    map[j].desc  = map1[j].desc;
  }
  for(j=0; j<nmap2; j++) {
    k=j+nmap1;
    map[k].code.c1=mapper[k % NMAP];
    if (nmap > NMAP)
      map[k].code.c2=mapper[k/NMAP];
    map[k].rgb.r = map2[j].rgb.r;
    map[k].rgb.g = map2[j].rgb.g;
    map[k].rgb.b = map2[j].rgb.b;
    map[k].desc  = map2[j].desc;
  }
  
  *newmap = map;
  return nmap;
}

void xpm_mat(char *outf,
	     int nmat,t_matrix *mat,t_matrix *mat2,bool bDiag,bool bFirstDiag)
{
  FILE   *out;
  char   buf[100];
  int    i,j,k,x,y,col;
  int    nmap;
  t_mapping *map=NULL;
  
  out=ffopen(outf,"w");
  
  for(i=0; i<nmat; i++) {
    if (!mat2 || !diff_maps(mat[i].nmap,mat[i].map,mat2[i].nmap,mat2[i].map))
      write_xpm_m(out,mat[0]);
    else {
      nmap = add_maps(&map, mat[i].nmap,mat[i].map, mat2[i].nmap,mat2[i].map);
      for(x=0; (x<mat[i].nx); x++) {
	for(y=0; (y<mat[i].nx); y++) {
	  if ((x<y) || ((x==y) && bFirstDiag)) /* upper left  -> map1 */
	    col=mat[i].matrix[x][y];
	  else /* lower right -> map2 */
	    col=mat[i].nmap+mat[i].matrix[x][y];
	  if ((bDiag) || (x!=y))
	    mat[i].matrix[x][y]=col;
	  else
	    mat[i].matrix[x][y]=0;
	}
      }
      sfree(mat[i].map);
      mat[i].nmap=nmap;
      mat[i].map=map;
      if (mat2 && (strcmp(mat[i].title,mat2[i].title) != 0))
	sprintf(mat[i].title,"%s / %s",mat[i].title,mat2[i].title);
      if (mat2 && (strcmp(mat[i].legend,mat2[i].legend) != 0))
	sprintf(mat[i].legend,"%s / %s",mat[i].legend,mat2[i].legend); 
      write_xpm_m(out,mat[i]);
    }
  }
  fclose(out);
}

static void tick_spacing(int n, real axis[], real offset, char axisnm, 
			 real *major, real *minor)
{
  real space;
  bool bTryAgain,bFive;
  int  i,j,t,f=0,ten;
#define NFACT 6
  int major_fact[NFACT] = {1, 2, 2.5, 4, 5, 7.5};
  int minor_fact[NFACT] = {5, 4, 5,   4, 5, 2.5};
  
  /* start with interval between 10 matrix points: */
  space = max(10*axis[1]-axis[0], axis[min(10,n-1)]-axis[0]);
  /* get power of 10 */
  ten = (int)ceil(log(space)/log(10))-1;
  bTryAgain = TRUE;
  for(t=ten-2; t<ten+3 && bTryAgain; t++) {
    for(f=0; f<NFACT && bTryAgain; f++) {
      space = pow(10,t) * major_fact[f];
      /* count how many ticks we would get: */
      i = 0;
      for(j=0; j<n; j++)
	if ( bRmod(axis[j] - offset, space) )
	  i++;
      /* do we have a reasonable number of ticks ? */
      bTryAgain = i > min(10, n-1) || i < 5;
    }
  }
  if (bTryAgain) {
    space = max(10*axis[1]-axis[0], axis[min(10,n-1)]-axis[0]);
    fprintf(stderr,"Auto tick spacing failed for %c-axis, guessing %g\n",
	    axisnm,space);
  }
  *major = space;
  *minor = space / minor_fact[f-1];
  fprintf(stderr,"Auto tick spacing for %c-axis: major %g, minor %g\n", 
	  axisnm, *major, *minor);
}

void ps_mat(char *outf,int nmat,t_matrix mat[],t_matrix mat2[],
	    bool bFrame,bool bDiag,bool bFirstDiag,
	    bool bTitle,bool bTitleOnce,bool bYonce,
	    int elegend,real boxx,real boxy,char *m2p,char *m2pout)
{
  char   *libm2p,buf[256],*legend;
  FILE   *out;
  t_psrec  psrec,*psr;
  int    W,H;
  int    i,j,x,y,col,leg=0;
  real   x0,y0,xx;
  real   w,h,dw,dh;
  int       nmap1=0,nmap2=0,leg_nmap;
  t_mapping *map1=NULL,*map2=NULL,*leg_map;
  bool   bMap1,bNextMap1,bDiscrete;
  
  libm2p = m2p ? libfn(m2p) : m2p;
  get_params(libm2p,m2pout,&psrec);

  psr=&psrec;

  if (psr->X.major <= 0 )
    tick_spacing(mat[0].nx, mat[0].axis_x, psr->X.offset, 'X', 
		 &(psr->X.major), &(psr->X.minor) );
  if (psr->X.minor <= 0 )
    psr->X.minor = psr->X.major / 2;
  if (psr->Y.major <= 0)
    tick_spacing(mat[0].ny, mat[0].axis_y, psr->Y.offset, 'Y',
		 &(psr->Y.major), &(psr->Y.minor) );
  if (psr->Y.minor <= 0)
    psr->Y.minor = psr->Y.major / 2;
  
  if (boxx>0) {
    psr->xboxsize=boxx;
    psr->yboxsize=boxx;
  }
  if (boxy>0)
    psr->yboxsize=boxy;  

  nmap1=0;
  for (i=0; (i<nmat); i++)
    if (mat[i].nmap>nmap1) {
      nmap1=mat[i].nmap;
      map1=mat[i].map;
      leg=i+1;
    }
  if (leg!=1)
    printf("Selected legend of matrix # %d for display\n",leg);
  if (mat2) {
    nmap2=0;
    for (i=0; (i<nmat); i++)
      if (mat2[i].nmap>nmap2) {
	nmap2=mat2[i].nmap;
	map2=mat2[i].map;
	leg=i+1;
  }
    if (leg!=1)
      printf("Selected legend of matrix # %d for second display\n",leg);
  }
  if ( (mat[0].legend[0]==0) && psr->legend )
    strcpy(mat[0].legend, psr->leglabel);

  bTitle     = bTitle     && mat[nmat-1].title[0];
  bTitleOnce = bTitleOnce && mat[nmat-1].title[0];
  psr->bTitle     = bTitle;
  psr->bTitleOnce = bTitleOnce;
  psr->bYonce     = bYonce;

  /* Set up size of box for nice colors */
  box_dim(nmat,mat,mat2,psr,elegend,bFrame,&w,&h,&dw,&dh);
  
  /* Set up bounding box */
  W=w+dw;
  H=h+dh;
  
  /* Start box at */
  x0=dw;
  y0=dh;
  x = W+psr->xoffs;
  y = H+psr->yoffs;
  if (bFrame) {
    x += 5*DDD;
    y += 4*DDD;
  }
  out=ps_open(outf,0,0,x,y);
  ps_linewidth(out,psr->linewidth);
  ps_init_rgb_box(out,psr->xboxsize,psr->yboxsize);
  ps_init_rgb_nbox(out,psr->xboxsize,psr->yboxsize);
  ps_translate(out,psr->xoffs,psr->yoffs);

  if (bFrame) {
    ps_comment(out,"Here starts the BOX drawing");  
    draw_boxes(out,x0,y0,w,nmat,mat,psr);
  }

  for(i=0; (i<nmat); i++) {
    if (bTitle || (bTitleOnce && i==nmat-1) ) {
      /* Print title, if any */
      ps_rgb(out,BLACK);
      ps_strfont(out,psr->titfont,psr->titfontsize); 
      if (!mat2 || (strcmp(mat[i].title,mat2[i].title) == 0))
	strcpy(buf,mat[i].title);
      else
	sprintf(buf,"%s / %s",mat[i].title,mat2[i].title);
      ps_ctext(out,x0+w/2,y0+box_height(&(mat[i]),psr)+psr->titfontsize,
	       buf,eXCenter);
    }
    sprintf(buf,"Here starts the filling of box #%d",i);
    ps_comment(out,buf);
    for(x=0; (x<mat[i].nx); x++) {
      int nexty;
      int nextcol;
      
      xx=x0+x*psr->xboxsize;
      ps_moveto(out,xx,y0);
      y=0;
      bMap1 = (!mat2 || (x<y || (x==y && bFirstDiag)));
      if ((bDiag) || (x!=y))
	col = mat[i].matrix[x][y];
      else
	col = -1;
      for(nexty=1; (nexty<=mat[i].ny); nexty++) {
	bNextMap1 = (!mat2 || (x<nexty || (x==nexty && bFirstDiag)));
	  /* TRUE:  upper left  -> map1 */
	  /* FALSE: lower right -> map2 */
	if ((nexty==mat[i].ny) || (!bDiag && (x==nexty)))
	  nextcol = -1;
	else
	  nextcol=mat[i].matrix[x][nexty];
	if ( (nexty==mat[i].ny) || (col!=nextcol) || (bMap1!=bNextMap1) ) {
	  if (col >= 0)
	    if (bMap1)
	      ps_rgb_nbox(out,&(mat[i].map[col].rgb),nexty-y);
	    else
	      ps_rgb_nbox(out,&(mat2[i].map[col].rgb),nexty-y);
	  else
	    ps_moverel(out,0,psr->yboxsize);
	  y=nexty;
	  bMap1=bNextMap1;
	  col=nextcol;
	  }
	}
    }
    y0+=box_height(&(mat[i]),psr)+box_dh(psr)+box_dh_top(IS_ONCE,psr);
  }
  
  if (psr->X.lineatzero || psr->Y.lineatzero) {
    /* reset y0 for first box */
    y0=dh;
    ps_comment(out,"Here starts the zero lines drawing");  
    draw_zerolines(out,x0,y0,w,nmat,mat,psr);
  }
  
  if (elegend!=elNone) {
    ps_comment(out,"Now it's legend time!");
    ps_linewidth(out,psr->linewidth);
    if ( mat2==NULL || elegend!=elSecond ) {
      bDiscrete = mat[0].bDiscrete;
      legend    = mat[0].legend;
      leg_nmap  = nmap1;
      leg_map   = map1;
    } else {
      bDiscrete = mat2[0].bDiscrete;
      legend    = mat2[0].legend;
      leg_nmap  = nmap2;
      leg_map   = map2;
    }
    if (bDiscrete)
      leg_discrete(out,psr->legfontsize,DDD,legend,
		   psr->legfontsize,psr->legfont,leg_nmap,leg_map);
    else {
      if ( elegend!=elBoth )
	leg_continuous(out,x0+w/2,w/2,DDD,legend,
		       psr->legfontsize,psr->legfont,leg_nmap,leg_map);
      else
	leg_bicontinuous(out,x0+w/2,w,DDD,mat[0].legend,mat2[0].legend,
			 psr->legfontsize,psr->legfont,nmap1,map1,nmap2,map2);
    }
    ps_comment(out,"Were there, dude");
  }
  
  ps_close(out);
}

void make_axis_labels(int nmat, t_matrix *mat)
{
  int i,j;
  
  for (i=0; (i<nmat); i++) {
    /* Make labels for x axis */
    if (mat[i].axis_x==NULL) {
      snew(mat[i].axis_x,mat[i].nx);
      for(j=0; (j<mat[i].nx); j++)
	mat[i].axis_x[j]=j;
    }
    /* Make labels for y axis */
    if (mat[i].axis_y==NULL) {
      snew(mat[i].axis_y,mat[i].ny);
      for(j=0; (j<mat[i].ny); j++)
	mat[i].axis_y[j]=j;
    }
  }
}  

void prune_mat(int nmat, t_matrix *mat,t_matrix *mat2, int skip)
{
  int i,x,y,xs,ys;
  
  for(i=0; i<nmat; i++) {
    fprintf(stderr,"converting %dx%d matrix to %dx%d\n",
	    mat[i].nx, mat[i].ny, 
	    (mat[i].nx+skip-1)/skip, (mat[i].ny+skip-1)/skip);
    /* walk through matrix */
    xs=0;
    for(x=0; (x<mat[i].nx); x++)
      if (x % skip == 0) {
	mat[i].axis_x[xs] = mat[i].axis_x[x];
	if (mat2)
	  mat2[i].axis_x[xs] = mat2[i].axis_x[x];
	ys=0;
	for(y=0; (y<mat[i].ny); y++) {
	  if(x==0) {
	    mat[i].axis_y[ys] = mat[i].axis_y[y];
	    if (mat2)
	      mat2[i].axis_y[ys] = mat2[i].axis_y[y];
	  }
	  if (y % skip == 0) {
	    mat[i].matrix[xs][ys] = mat[i].matrix[x][y];
	    if (mat2)
	      mat2[i].matrix[xs][ys] = mat2[i].matrix[x][y];
	    ys++;
	  }
	}
	xs++;
      }
    /* adjust parameters */
    mat[i].nx = (mat[i].nx+skip-1)/skip;
    mat[i].ny = (mat[i].ny+skip-1)/skip;
    if (mat2) {
      mat2[i].nx = (mat2[i].nx+skip-1)/skip;
      mat2[i].ny = (mat2[i].ny+skip-1)/skip;
    }
  }
}

void zero_lines(int nmat, t_matrix *mat, t_matrix *mat2)
{
  int i, x, y, m;
  t_matrix *mats;
  
  for(i=0; i<nmat; i++)
    for(m=0; m < (mat2 ? 2 : 1) ; m++) {
      if (m==0) 
	mats=mat;
      else 
	mats=mat2;
      for(x=0; x<mats[i].nx-1; x++)
	if (abs(mats[i].axis_x[x+1]) < 1e-5)
	  for(y=0; y<mats[i].ny; y++)
	    mats[i].matrix[x][y]=0;
      for(y=0; y<mats[i].ny-1; y++)
	if (abs(mats[i].axis_y[y+1]) < 1e-5)
	  for(x=0; x<mats[i].nx; x++)
	    mats[i].matrix[x][y]=0;
    }
}

void write_combined_matrix(int ecombine, char *fn,
			   int nmat, t_matrix *mat1, t_matrix *mat2)
{
  int i, j, k, nlevels;
  t_mapping *map=NULL;
  FILE *out;
  real **rmat1, **rmat2;
  real rhi, rlo;
  
  out = ffopen(fn, "w");
  for(k=0; k<nmat; k++) {
    if ( mat2[k].nx!=mat1[k].nx || mat2[k].ny!=mat1[k].ny )
      fatal_error(0,"Size of frame %d in 1st (%dx%d) and 2nd matrix (%dx%d) do"
		  " not match.\n",k,mat1[k].nx,mat1[k].ny,mat2[k].nx,mat2[k].ny);
    printf("Combining two %dx%d matrices\n",mat1[k].nx,mat1[k].ny);
    rmat1 = matrix2real(&mat1[k], NULL);
    rmat2 = matrix2real(&mat2[k], NULL);
    rlo=1e38;
    rhi=-1e38;
    for(j=0; j<mat1[k].ny; j++)
      for(i=0; i<mat1[k].nx; i++) {
	switch (ecombine) {
	case ecAdd:  rmat1[i][j] += rmat2[i][j]; break;
	case ecSub:  rmat1[i][j] -= rmat2[i][j]; break;
	case ecMult: rmat1[i][j] *= rmat2[i][j]; break;
	case ecDiv:  rmat1[i][j] /= rmat2[i][j]; break;
	default:
	  fatal_error(0,"No such combination rule %d for matrices",ecombine);
	}
	rlo = min(rlo, rmat1[i][j]);
	rhi = max(rhi, rmat1[i][j]);
      }
    nlevels = mat1[k].nmap+mat2[k].nmap;
    if (rhi==rlo)
      fprintf(stderr,
	      "combination results in uniform matrix (%g), no output\n",rhi);
    else if (rlo>=0 || rhi<=0)
      write_xpm(out, mat1[k].title, mat1[k].legend, 
		mat1[k].label_x, mat1[k].label_y,
		mat1[k].nx, mat1[k].ny, mat1[k].axis_x, mat1[k].axis_y, 
		rmat1, rlo, rhi, rhi<=0?red:white, rhi<=0?white:blue, 
		&nlevels);
    else 
      write_xpm3(out, mat1[k].title, mat1[k].legend, 
		 mat1[k].label_x, mat1[k].label_y,
		 mat1[k].nx, mat1[k].ny, mat1[k].axis_x, mat1[k].axis_y, 
		 rmat1, rlo, 0, rhi, red, white, blue, &nlevels);
  }
  fclose(out);
}

void do_mat(int nmat,t_matrix *mat,t_matrix *mat2,
	    bool bFrame,bool bZeroLine,bool bDiag,bool bFirstDiag,bool bTitle,
	    bool bTitleOnce,bool bYonce,int elegend,real boxx,real boxy,
	    char *epsfile,char *xpmfile,char *m2p,char *m2pout,int skip)
{
  int      i,j,k;

  if (mat2) {
    for (k=0; (k<nmat); k++) {
      if ((mat2[k].nx!=mat[k].nx) || (mat2[k].ny!=mat[k].ny)) 
	fatal_error(0,"WAKE UP!! Size of frame %d in 2nd matrix file (%dx%d) does not match size of 1st matrix (%dx%d) or the other way around.\n",
		    k,mat2[k].nx,mat2[k].ny,mat[k].nx,mat[k].ny);
      for (j=0; (j<mat[k].ny); j++)
	for (i=bFirstDiag?j+1:j; (i<mat[k].nx); i++)
	  mat[k].matrix[i][j]=mat2[k].matrix[i][j];
    }
  }
  for(i=0; (i<nmat); i++) 
    fprintf(stderr,"Matrix %d is %d x %d\n",i,mat[i].nx,mat[i].ny);

  make_axis_labels(nmat, mat);
  
  if (skip > 1)
    prune_mat(nmat,mat,mat2,skip);
    
  if (bZeroLine)
    zero_lines(nmat,mat,mat);
  
  if (epsfile!=NULL)
    ps_mat(epsfile,nmat,mat,mat2,bFrame,bDiag,bFirstDiag,
	   bTitle,bTitleOnce,bYonce,elegend,boxx,boxy,m2p,m2pout);
  if (xpmfile!=NULL)
    xpm_mat(xpmfile,nmat,mat,mat2,bDiag,bFirstDiag);
}

void gradient_map(rvec grad, int nmap, t_mapping map[])
{
  int i;
  real c;
  
  for(i=0; i<nmap; i++) {
    c = i/(nmap-1.0);
    map[i].rgb.r = 1-c*(1-grad[XX]);
    map[i].rgb.g = 1-c*(1-grad[YY]);
    map[i].rgb.b = 1-c*(1-grad[ZZ]);
  }
}
  
void gradient_mat(rvec grad, int nmat, t_matrix mat[])
{
  int m;
  
  for(m=0; m<nmat; m++)
    gradient_map(grad, mat[m].nmap, mat[m].map);
}

void rainbow_map(bool bBlue, int nmap, t_mapping map[])
{
  int i;
  real c,r,g,b;

  for(i=0; i<nmap; i++) {
    c = (map[i].rgb.r + map[i].rgb.g + map[i].rgb.b)/3;
    if (c > 1)
      c = 1;
    if (bBlue)
      c = 1 - c;
    if (c <= 0.25) { /* 0-0.25 */
      r = 0;
      g = pow(4*c,0.666);
      b = 1;
    } else if (c <= 0.5) { /* 0.25-0.5 */
      r = 0;
      g = 1;
      b = pow(2-4*c,0.666);
    } else if (c <= 0.75) { /* 0.5-0.75 */
      r = pow(4*c-2,0.666);
      g = 1;
      b = 0;
    } else { /* 0.75-1 */
      r = 1;
      g = pow(4-4*c,0.666);
      b = 0;
    }
    map[i].rgb.r = r;
    map[i].rgb.g = g;
    map[i].rgb.b = b;
  }
}

void rainbow_mat(bool bBlue, int nmat, t_matrix mat[])
{
  int m;
  
  for(m=0; m<nmat; m++)
    rainbow_map(bBlue, mat[m].nmap, mat[m].map);
}
  
int main(int argc,char *argv[])
{
  static char *desc[] = {
    "xpm2ps makes a beautiful color plot of an XPixelMap file.",
    "Labels and axis can be displayed, when they are supplied",
    "in the correct matrix format.",
    "Matrix data may be generated by programs such as do_dssp, g_rms or",
    "g_mdmat.[PAR]",
    "Parameters are set in the [TT]m2p[tt] file optionally supplied with",
    "[TT]-di[tt]. Reasonable defaults are provided. Settings for the y-axis",
    "default to those for the x-axis. Font names have a defaulting hierarchy:",
    "titlefont -> legendfont; titlefont -> (xfont -> yfont -> ytickfont)",
    "-> xtickfont, e.g. setting titlefont sets all fonts, setting xfont",
    "sets yfont, ytickfont and xtickfont.[PAR]",
    "With [TT]-f2[tt] a 2nd matrix file can be supplied, both matrix",
    "files will be read simultaneously and the upper left half of the",
    "first one ([TT]-f[tt]) is plotted together with the lower right",
    "half of the second one ([TT]-f2[tt]). The diagonal will contain",
    "values from the matrix file selected with [TT]-diag[tt].",
    "Plotting of the diagonal values can be suppressed altogether by",
    "setting [TT]-diag[tt] to [TT]none[tt]. With ",
    "[TT]-combine[tt] an alternative operation can be selected to combine",
    "the matrices. In this case, a new color map will be generated with",
    "a red gradient for negative numbers and a blue for positive.[PAR]",
    "If the color coding and legend labels of both matrices are identical,",
    "only one legend will be displayed, else two separate legends are",
    "displayed.[PAR]",
    "[TT]-title[tt] can be set to [TT]none[tt] to suppress the title, or to",
    "[TT]ylabel[tt] to show the title in the Y-label position (alongside",
    "the Y-axis).[PAR]",
    "With the [TT]-rainbow[tt] option dull grey-scale matrices can be turned",
    "into attractive color pictures.[PAR]",
    "Merged or rainbowed matrices can be written to an XPixelMap file with",
    "the [TT]-xpm[tt] option."
  };

  char      *fn,*epsfile=NULL,*xpmfile=NULL;
  int       i,nmat,nmat2,etitle,elegend,ediag,erainbow,ecombine;
  t_matrix *mat=NULL,*mat2=NULL;
  bool      bTitle,bTitleOnce,bDiag,bFirstDiag,bGrad;
  static bool bFrame=TRUE,bZeroLine=FALSE,bYonce=FALSE,bAdd=FALSE;
  static real boxx=0,boxy=0;
  static rvec grad={0,0,0};
  enum                    { etSel, etTop, etOnce, etYlabel, etNone, etNR };
  static char *title[]   = { NULL, "top", "once", "ylabel", "none", NULL };
  /* MUST correspond to enum elXxx as defined at top of file */
  static char *legend[]  = { NULL, "both", "first", "second", "none", NULL };
  enum                    { edSel, edFirst, edSecond, edNone, edNR };
  static char *diag[]    = { NULL, "first", "second", "none", NULL };
  enum                    { erSel, erNo, erBlue, erRed, erNR };
  static char *rainbow[] = { NULL, "no", "blue", "red", NULL };
  /* MUST correspond to enum ecXxx as defined at top of file */
  static char *combine[] = {
    NULL, "halves", "add", "sub", "mult", "div", NULL };
  static int skip=1;
  t_pargs pa[] = {
    { "-frame",   FALSE, etBOOL, {&bFrame},
      "Display frame, ticks, labels, title and legend" },
    { "-title",   FALSE, etENUM, {title},   "Show title at" },
    { "-yonce",   FALSE, etBOOL, {&bYonce}, "Show y-label only once" },
    { "-legend",  FALSE, etENUM, {legend},  "Show legend" },
    { "-diag",    FALSE, etENUM, {diag},    "Diagonal" },
    { "-combine", FALSE, etENUM, {combine}, "Combine two matrices" },
    { "-bx",      FALSE, etREAL, {&boxx},
      "Box x-size (also y-size when -by is not set)" },
    { "-by",      FALSE, etREAL, {&boxy},   "Box y-size" },
    { "-rainbow", FALSE, etENUM, {rainbow},
      "Rainbow colors, convert white to" },
    { "-gradient",FALSE, etRVEC, {grad},
      "Re-scale colormap to a smooth gradient from white {1,1,1} to {r,g,b}" },
    { "-skip",    FALSE, etINT,  {&skip},
      "only write out every nr-th row and column" },
    { "-zeroline",FALSE, etBOOL, {&bZeroLine},
      "insert line in xpm matrix where axis label is zero"}
  };
  t_filenm  fnm[] = {
    { efXPM, "-f",  NULL,      ffREAD },
    { efXPM, "-f2", "root2",   ffOPTRD },
    { efM2P, "-di", NULL,      ffLIBOPTRD },
    { efM2P, "-do", "out",     ffOPTWR },
    { efEPS, "-o",  NULL,      ffOPTWR },
    { efXPM, "-xpm",NULL,      ffOPTWR }
  };
#define NFILE asize(fnm)
  
  CopyRight(stderr,argv[0]);
  parse_common_args(&argc,argv,PCA_CAN_VIEW,
		    NFILE,fnm,asize(pa),pa,
		    asize(desc),desc,0,NULL);

  etitle   = nenum(title);
  elegend  = nenum(legend);
  ediag    = nenum(diag);
  erainbow = nenum(rainbow);
  ecombine = nenum(combine);
  bGrad    = opt2parg_bSet("-gradient",asize(pa),pa);
  for(i=0; i<DIM; i++)
    if (grad[i] < 0 || grad[i] > 1)
      fatal_error(0, "RGB value %g out of range (0.0-1.0)", grad[i]);
  if (!bFrame) {
    etitle = etNone;
    elegend = elNone;
  }

  epsfile=ftp2fn_null(efEPS,NFILE,fnm);
  xpmfile=opt2fn_null("-xpm",NFILE,fnm);
  if ( epsfile==NULL && xpmfile==NULL ) {
    if (ecombine!=ecHalves)
      xpmfile=opt2fn("-xpm",NFILE,fnm);
    else
      epsfile=ftp2fn(efEPS,NFILE,fnm);
  }
  if (ecombine!=ecHalves && epsfile) {
    fprintf(stderr,
	    "WARNING: can only write result of arithmetic combination "
	    "of two matrices to .xpm file\n"
	    "         file %s will not be written\n", epsfile);
    epsfile = NULL;
  }
  
  bDiag      = ediag!=edNone;
  bFirstDiag = ediag!=edSecond;
  
  fn=opt2fn("-f",NFILE,fnm);
  nmat=read_xpm_matrix(fn,&mat);
  fprintf(stderr,"There are %d matrices in %s\n",nmat,fn);
  fn=opt2fn_null("-f2",NFILE,fnm);
  if (fn) {
    nmat2=read_xpm_matrix(fn,&mat2);
    fprintf(stderr,"There are %d matrices in %s\n",nmat2,fn);
    if (nmat != nmat2) {
      fprintf(stderr,"Different number of matrices, using the smallest number.\n");
      nmat=nmat2=min(nmat,nmat2);
    }
  } else {
    if (ecombine!=ecHalves)
      fprintf(stderr,
	      "WARNING: arithmetic matrix combination selected (-combine), "
	      "but no second matrix (-f2) supplied\n"
	      "         no matrix combination will be performed\n");
    ecombine=0;
    nmat2=0;
  }
  bTitle     = etitle==etTop;
  bTitleOnce = etitle==etOnce;
  if ( etitle==etYlabel )
    for (i=0; (i<nmat); i++) {
      strcpy(mat[i].label_y, mat[i].title);
      if (mat2)
	strcpy(mat2[i].label_y, mat2[i].title);
    }
  if (bGrad) {
    gradient_mat(grad,nmat,mat);
    if (mat2)
      gradient_mat(grad,nmat2,mat2);
  }
  if (erainbow!=erNo) {
    rainbow_mat(erainbow==erBlue,nmat,mat);
    if (mat2)
      rainbow_mat(erainbow==erBlue,nmat2,mat2);
  }

  if ((mat2 == NULL) && (elegend!=elNone))
    elegend = elFirst;
  
  if (ecombine && ecombine!=ecHalves)
    write_combined_matrix(ecombine, xpmfile, nmat, mat, mat2);
  else
    do_mat(nmat,mat,mat2,bFrame,bZeroLine,bDiag,bFirstDiag,
	   bTitle,bTitleOnce,bYonce,
	   elegend, boxx,boxy,epsfile,xpmfile,
	   opt2fn_null("-di",NFILE,fnm),opt2fn_null("-do",NFILE,fnm), skip);
  
  view_all(NFILE, fnm);
    
  thanx(stderr);
  
  return 0;
}
