/*
 * $Id: repfirst.c,v 1.6 2002/02/28 10:49:29 spoel Exp $
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
static char *SRCID_repfirst_c = "$Id: repfirst.c,v 1.6 2002/02/28 10:49:29 spoel Exp $";
#include "string2.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "replace.h"
#include "strdb.h"

bool replace1(char *string,char *buf,char *search,char *replace)
{
  char *ptr,*bufptr;
  int  blen,stringlen,slen,rlen;
  int  i,j,tmp;
  
  slen=strlen(search);
  stringlen=strlen(string);
  if ((string == NULL) || (slen == 0) || (stringlen == 0))
    return FALSE;
  
  rlen=strlen(replace);
  sprintf(buf,"%s",string);
  
  bufptr=buf;
  if ((ptr=strstr(bufptr,search)) != NULL) {
    if (rlen <= slen) {
      for(i=0; (i<rlen); i++)
	ptr[i]=replace[i];
      if (rlen < slen) {
	while (ptr[i+slen-rlen] != '\0') {
	  ptr[i]=ptr[i+slen-rlen];
	  i++;
	}
	ptr[i]='\0';
      }
    }
    else {
      tmp=strlen(ptr);
      for(j=tmp; (j>=slen); j--)
	ptr[rlen-slen+j]=ptr[j];
      for(i=0; (i<rlen); i++)
	ptr[i]=replace[i];
    }
    bufptr=ptr+rlen;
    
    return TRUE;
  }
  
  return FALSE;
}

void main(int argc,char *argv[])
{
#define MYBUFS 10240
  int  i,nstr;
  char **str;
  char **rep;
  bool *bRep;
  char buffer[MYBUFS];
  char newbuf[MYBUFS];
  
  nstr=get_strings("index.gmx",&str);
  snew(bRep,nstr);
  snew(rep,nstr);
  for(i=0; (i<nstr); i++) {
    sprintf(buffer,"\\myindex{%s}",str[i]);
    rep[i]=strdup(buffer);
  }
  
  while(fgets2(buffer,MYBUFS-1,stdin) != NULL) {
    newbuf[0]='\0';
    for(i=0; (i<nstr); i++)
      if (!bRep[i]) {
	bRep[i]=replace1(buffer,newbuf,str[i],rep[i]);
	if (bRep[i])
	  strcpy(buffer,newbuf);
      }
    printf("%s\n",newbuf);
  }
}
