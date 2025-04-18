/*
 * $Id: fgrid.c,v 1.7 2002/02/28 11:07:08 spoel Exp $
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
 * Gromacs Runs On Most of All Computer Systems
 */
static char *SRCID_fgrid_c = "$Id: fgrid.c,v 1.7 2002/02/28 11:07:08 spoel Exp $";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "string2.h"
#include "smalloc.h"
#include "fgrid.h"
#include "futil.h"

static char *type[] = { 
  "button", "radiobuttons", "groupbox", "checkbox",
  "pixmap", "statictext",   "edittext", "defbutton"
  };

void ReadDlgError(char *infile,eDLGERR err,char *s,char *file,int line)
{
  fprintf(stderr,"Error: ");
  switch(err) {
  case eNOVALS:
    fprintf(stderr,"Not enough values for %s",s);
    break;
  case eGRIDEXP:
    fprintf(stderr,"'grid' expected instead of %s",s);
    break;
  case eACCOEXP:
    fprintf(stderr,"'{' expected instead of %s",s);
    break;
  case eACCCEXP:
    fprintf(stderr,"'}' expected instead of %s",s);
    break;
  case eGRPEXP:
    fprintf(stderr,"'group' expected instead of %s",s);
    break;
  case eITEMEXP:
    fprintf(stderr,"item expected instead of %s",s);
    break;
  case eSAMEPOINT:
    fprintf(stderr,"grid point for %s already in use",s);
    break;
  case eTOOWIDE:
    fprintf(stderr,"grid too wide for %s",s);
    break;
  case eTOOHIGH:
    fprintf(stderr,"grid too high for %s",s);
    break;
  case eQUOTE:
    fprintf(stderr,"quote expected instead of %s",s);
    break;
  default:
    fprintf(stderr,"????");
    break;
  }
  fprintf(stderr," in file %s\n",infile);
  fprintf(stderr,"C-File: %s, line: %d\n",file,line);
  exit(1);
}

#define ReadDlgErr(in,er,es) ReadDlgError(in,er,es,__FILE__,__LINE__)

static void GetBuf(FILE *in, char *buf)
{
  fscanf(in,"%s",buf);
}

static void ReadAccOpen(char *infile, FILE *in)
{
  char buf[STRLEN];
  
  GetBuf(in,buf);
  if (strcmp(buf,"{")!=0)
    ReadDlgErr(infile,eACCOEXP,buf);
}

static void ReadAccClose(char *infile, FILE *in)
{
  char buf[STRLEN];
  
  GetBuf(in,buf);
  if (strcmp(buf,"}")!=0)
    ReadDlgErr(infile,eACCCEXP,buf);
}

void ReadQuoteString(char *infile, FILE *in, char *buf)
{
  char c[2];
  int i=0;
  
  /* Read until first quote */
  while ((c[0]=fgetc(in))!='"') 
    if (!isspace(c[0])) {
      c[1]='\0';
      ReadDlgErr(infile,eQUOTE,c);
    }
  /* Read until second quote */
  while ((c[0]=fgetc(in))!='"')
    buf[i++]=c[0];
  buf[i]='\0';
}

static void ReadQuoteStringOrAccClose(FILE *in, char *buf)
{
  char c;
  int i=0;
  
  /* Read until first quote */
  do {
    c=fgetc(in);
    if (c=='}') {
      buf[0]=c;
      buf[1]='\0';
      return;
    }
  } while (c != '"');
  
  /* Read until second quote */
  while ((c=fgetc(in))!='"')
    buf[i++]=c;
  buf[i]='\0';
}

static bool bNotAccClose(char *buf)
{
  return (strcmp(buf,"}")!=0);
}

static t_fitem *NewFItem(void)
{
  t_fitem *fitem;
  
  snew(fitem,1);
  fitem->nname=0;
  fitem->name=NULL;
  fitem->set=NULL;
  fitem->get=NULL;
  fitem->def=NULL;
  fitem->help=NULL;

  return fitem;
}

static t_fsimple *NewFSimple(void)
{
  t_fsimple *fsimple;
  
  snew(fsimple,1);
  
  return fsimple;
}

static void AddFItemName(t_fitem *fitem, char *name)
{
  srenew(fitem->name,++fitem->nname);
  fitem->name[fitem->nname-1]=strdup(name);
}

static t_fgroup *NewFGroup(void)
{
  t_fgroup *fgroup;
  
  snew(fgroup,1);
  fgroup->name=NULL;
  fgroup->nfitem=0;
  fgroup->fitem=NULL;
  
  return fgroup;
}

static void AddFGroupFItem(t_fgroup *fgroup, t_fitem *fitem)
{
  srenew(fgroup->fitem,++fgroup->nfitem);
  fgroup->fitem[fgroup->nfitem-1]=fitem;
}

static t_fgroup *AddFGridFGroup(t_fgrid *fgrid)
{
  srenew(fgrid->fgroup,++fgrid->nfgroup);
  fgrid->fgroup[fgrid->nfgroup-1]=NewFGroup();
  return fgrid->fgroup[fgrid->nfgroup-1];
}

static t_fsimple *AddFGridFSimple(t_fgrid *fgrid)
{
  srenew(fgrid->fsimple,++fgrid->nfsimple);
  fgrid->fsimple[fgrid->nfsimple-1]=NewFSimple();
  return fgrid->fsimple[fgrid->nfsimple-1];
}

static t_fgrid *NewFGrid(void)
{
  t_fgrid *fgrid;
  
  snew(fgrid,1);
  fgrid->w=0;
  fgrid->h=0;
  fgrid->nfgroup=0;
  fgrid->fgroup=NULL;
  fgrid->nfsimple=0;
  fgrid->fsimple=NULL;
  
  return fgrid;
}

static void DoneFItem(t_fitem *fitem)
{
  int i;
  
  for(i=0; (i<fitem->nname); i++)
    sfree(fitem->name[i]);
  sfree(fitem->name);
  sfree(fitem->set);
  sfree(fitem->get);
  sfree(fitem->def);
  sfree(fitem->help);
}

static void DoneFGroup(t_fgroup *fgroup)
{
  int i;
  
  sfree(fgroup->name);
  for(i=0; (i<fgroup->nfitem); i++)
    DoneFItem(fgroup->fitem[i]);
  sfree(fgroup->fitem);
}

static void DoneFSimple(t_fsimple *fsimple)
{
  DoneFItem(fsimple->fitem);
  sfree(fsimple->fitem);
}

void DoneFGrid(t_fgrid *fgrid)
{
  int i;
  
  for(i=0; (i<fgrid->nfgroup); i++) 
    DoneFGroup(fgrid->fgroup[i]);
  sfree(fgrid->fgroup);
  for(i=0; (i<fgrid->nfsimple); i++) 
    DoneFSimple(fgrid->fsimple[i]);
  sfree(fgrid->fsimple);
}

static t_fitem *ScanFItem(char *infile, FILE *in, char *buf)
{
  char set[STRLEN],get[STRLEN],help[STRLEN],def[STRLEN];
  edlgitem edlg;
  t_fitem *fitem;
  
  fitem=NewFItem();
  
  for(edlg=(edlgitem)0; (edlg<edlgNR+1); edlg++)
    if (strcmp(buf,type[edlg])==0)
      break;
  if (edlg==edlgNR) {
    /* Special case */
    edlg=edlgBN;
    fitem->bDef=TRUE;
  }
  if (edlg==edlgNR+1) {
    ReadDlgErr(infile,eITEMEXP,buf);
  }
  
  fitem->edlg=edlg;
  switch (edlg) {
  case edlgBN:
  case edlgCB:
  case edlgET:
    ReadQuoteString(infile,in,buf);
    AddFItemName(fitem,buf);
    break;
  case edlgST:
  case edlgRB:
    ReadAccOpen(infile,in);
    ReadQuoteStringOrAccClose(in,buf);
    while (bNotAccClose(buf)) {
      AddFItemName(fitem,buf);
      ReadQuoteStringOrAccClose(in,buf);
    }
    break;
  case edlgPM:
  case edlgGB:
    ReadDlgErr(infile,eITEMEXP,type[edlg]);
    break;
  default:
    break;
  }
  ReadQuoteString(infile,in,set);
  ReadQuoteString(infile,in,get);
  ReadQuoteString(infile,in,def);
  ReadQuoteString(infile,in,help);
  fitem->set=strdup(set);
  fitem->get=strdup(get);
  fitem->def=strdup(def);
  fitem->help=strdup(help);
  
  return fitem;
}

t_fgrid *FGridFromFile(char *infile)
{
  FILE *in;
  char buf[STRLEN];
  char *gmxlib;
  char newinfile[STRLEN];
  
  t_fgrid   *fgrid;
  t_fgroup  *fgroup;
  t_fsimple *fsimple;
  int       gridx,gridy;  
  
  in = libopen(infile);
  GetBuf(in,buf);
  if (strcmp(buf,"grid")!=0)
    ReadDlgErr(infile,eGRIDEXP,buf);
  fgrid=NewFGrid();
  if ((fscanf(in,"%d%d",&gridx,&gridy))!=2)
    ReadDlgErr(infile,eNOVALS,"grid w,h");
  fgrid->w=gridx;
  fgrid->h=gridy;
  ReadAccOpen(infile,in);
  GetBuf(in,buf);
  while (bNotAccClose(buf)) {
    if (strcmp(buf,"group")==0) {
      fgroup=AddFGridFGroup(fgrid);
      ReadQuoteString(infile,in,buf);
      fgroup->name=strdup(buf);
      if ((fscanf(in,"%d%d%d%d",&fgroup->x,&fgroup->y,&fgroup->w,&fgroup->h))!=4)
	ReadDlgErr(infile,eNOVALS,"group x,y,w,h");
      if (fgroup->x+fgroup->w > gridx)
	ReadDlgErr(infile,eTOOWIDE,buf);
      if (fgroup->y+fgroup->h > gridy)
	ReadDlgErr(infile,eTOOHIGH,buf);
      ReadAccOpen(infile,in);
      GetBuf(in,buf);
      while (bNotAccClose(buf)) {
	AddFGroupFItem(fgroup,ScanFItem(infile,in,buf));
	GetBuf(in,buf);
      }
    }
    else if (strcmp(buf,"simple")==0) {
      fsimple=AddFGridFSimple(fgrid);
      if ((fscanf(in,"%d%d%d%d",&fsimple->x,&fsimple->y,&fsimple->w,&fsimple->h))!=4)
	ReadDlgErr(infile,eNOVALS,"simple x,y,w,h");
      if (fsimple->x+fsimple->w > gridx)
	ReadDlgErr(infile,eTOOWIDE,"simple");
      if (fsimple->y+fsimple->h > gridy)
	ReadDlgErr(infile,eTOOHIGH,"simple");
      ReadAccOpen(infile,in);
      GetBuf(in,buf);
      fsimple->fitem=ScanFItem(infile,in,buf);
      ReadAccClose(infile,in);
    }
    GetBuf(in,buf);
  }
  fclose(in);
  
  return fgrid;
}

static void DumpFItem(t_fitem *fitem)
{
  int i;
  
  printf("  type: %s, set: '%s', get: '%s', def: '%s', help: '%s'\n  {",
	 type[fitem->edlg],fitem->set,fitem->get,fitem->def,fitem->help);
  for(i=0; (i<fitem->nname); i++)
    printf("  '%s'",fitem->name[i]);
  printf("  }\n");
}

static void DumpFSimple(t_fsimple *fsimple)
{
  printf("Simple %dx%d at %d,%d\n",fsimple->w,fsimple->h,fsimple->x,fsimple->y);
  DumpFItem(fsimple->fitem);
}

static void DumpFGroup(t_fgroup *fgroup)
{
  int i;
  
  printf("Group %dx%d at %d,%d\n",fgroup->w,fgroup->h,fgroup->x,fgroup->y);
  for(i=0; (i<fgroup->nfitem); i++)
    DumpFItem(fgroup->fitem[i]);
}

void DumpFGrid(t_fgrid *fgrid)
{
  int i;
  
  printf("Grid %dx%d\n",fgrid->w,fgrid->h);
  for(i=0; (i<fgrid->nfgroup); i++)
    DumpFGroup(fgrid->fgroup[i]);
  for(i=0; (i<fgrid->nfsimple); i++)
    DumpFSimple(fgrid->fsimple[i]);
}
