/*
 * $Id: xmlio.c,v 1.4 2002/04/10 20:25:59 spoel Exp $
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
static char *SRCID_xmlio_c = "$Id: xmlio.c,v 1.4 2002/04/10 20:25:59 spoel Exp $";
#ifdef HAVE_XML
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <stdlib.h>
#include "typedefs.h"
#include "fatal.h"
#include "string.h"
#include "futil.h"
#include "smalloc.h"
#include "names.h"
#include "assert.h"
#include "symtab.h"
#include "macros.h"
#include "symtab.h"
#include "xmlio.h"

static char *xyz_names[]    = { 
  "x", "y", "z" 
};
static char *tensor_names[] = { 
  "xx", "xy", "xz", "yx", "yy", "yz", "zx", "zy", "zz"
};

typedef struct {
  int        nx,nv,nf,ntop,nbox,ninprec;
  int        step,natoms;
  real       t,lambda;
  t_inputrec *ir;
  rvec       *box;
  rvec       *x,*v,*f;
  t_topology *top;
} t_xmlrec;

typedef struct {
  char *name;
  real value;
} t_masstype;

static char *xmltypes[] = { 
  NULL, 
  "XML_ELEMENT_NODE",
  "XML_ATTRIBUTE_NODE",
  "XML_TEXT_NODE",
  "XML_CDATA_SECTION_NODE",
  "XML_ENTITY_REF_NODE",
  "XML_ENTITY_NODE",
  "XML_PI_NODE",
  "XML_COMMENT_NODE",
  "XML_DOCUMENT_NODE",
  "XML_DOCUMENT_TYPE_NODE",
  "XML_DOCUMENT_FRAG_NODE",
  "XML_NOTATION_NODE",
  "XML_HTML_DOCUMENT_NODE",
  "XML_DTD_NODE",
  "XML_ELEMENT_DECL",
  "XML_ATTRIBUTE_DECL",
  "XML_ENTITY_DECL",
  "XML_NAMESPACE_DECL",
  "XML_XINCLUDE_START",
  "XML_XINCLUDE_END"
};
#define NXMLTYPES asize(xmltypes)
	
extern int xmlDoValidityCheckingDefaultValue;
	
enum { 
  exmlGROMACS, 
  exmlINPUTREC,    exmlOUTPUT,      exmlCOUPLING,    exmlCUTOFF,
  exmlPMEPARM,     exmlTCOUPLING,   exmlPCOUPLING,   exmlTCPARM,      
  exmlREFPRES,     exmlCOMPRESS,    exmlRVEC,
  exmlSYSTEM,      exmlCOMPOSITION, exmlMOLECULE,    exmlATOM,
  exmlTOPOLOGY,    exmlBONDS,       exmlANGLES,      exmlDIHEDRALS,
  exmlFORCEFIELD,  exmlMASSTYPE,    exmlBOX,
  exmlCOORDINATES, exmlVELOCITIES,  exmlFORCES,      exmlNR 
};
  
static char *exml_names[] = {
  "gromacs",
  /* Inputrec stuff */
  "parameters",  "output",     "coupling", "cutoff",   "pmeparm",
  "tcoupling",   "pcoupling",  "tcparm",   "p-ref",    "compressibility", "rvec",
  /* System description */
  "system",      "composition", "molecule","atom",     
  /* Topology description */
  "topology",
  "bonds",       "angles",      "dihedrals",
  /* Force field */
  "forcefield",  "masstype",    "cell",
  /* Coordinates etc. */
  "coordinates", "velocities", "forces" 
};

static int find_elem(char *name,int nr,char *names[])
{
  int i;
  
  for(i=0; (i<nr); i++)
    if (strcmp(name,names[i]) == 0) 
      break;
  if (i == nr)
    fatal_error(0,"Unknown element name %s",name);
    
  return i;
}
	
static char *sp(int n)
{
  static char buf[80];
  int i;
  
  /* Don't indent more than 80 characters */
  n = n % 80;
  for(i=0; (i<n); i++)
    buf[i] = ' ';
  buf[i] = '\0';
  
  return buf;
}

static void process_attr(FILE *fp,xmlAttrPtr attr,int elem,
			 int indent,t_xmlrec *xml)
{
  char *attrname,*attrval;
  
  while (attr != NULL) {
    attrname = (char *)attr->name;
    attrval  = (char *)attr->children->content;
    
#define atest(s) ((strcmp(attrname,s) == 0) && (attrval != NULL))
    switch (elem) {
    case exmlGROMACS:
      if (atest("title"))
	xml->top->name = put_symtab(&xml->top->symtab,attrval);
      break;
    case exmlINPUTREC:
      if (atest("algorithm"))
	xml->ir->eI = find_elem(attrval,eiNR,ei_names);
      break;
    case exmlOUTPUT:      
    case exmlCOUPLING:    
    case exmlCUTOFF:
    case exmlPMEPARM:     
    case exmlTCOUPLING:  
    case exmlPCOUPLING:  
    case exmlTCPARM:      
    case exmlREFPRES:    
    case exmlCOMPRESS:
    case exmlRVEC:
    case exmlSYSTEM:   
    case exmlCOMPOSITION:
    case exmlMOLECULE:
    case exmlATOM:
    case exmlTOPOLOGY:  
    case exmlBONDS:  
    case exmlANGLES:  
    case exmlDIHEDRALS:
    case exmlFORCEFIELD: 
    case exmlMASSTYPE: 
    case exmlCOORDINATES:
    case exmlVELOCITIES: 
    case exmlFORCES: 
    default:
      if (fp)
	fprintf(fp,"%sProperty: '%s' Value: '%s'\n",sp(indent),
		attrname,attrval);
    }
    attr = attr->next;
#undef atest
  }
}

static void process_tree(FILE *fp,xmlNodePtr tree,int indent,t_xmlrec *xml)
{
  int elem;
  
  while (tree != NULL) {
    switch (tree->type) {
    case XML_ELEMENT_NODE:
      elem = find_elem((char *)tree->name,exmlNR,exml_names);
      if (fp)
	fprintf(fp,"%sElement node name %s\n",sp(indent),(char *)tree->name);
      
      process_attr(fp,tree->properties,elem,indent+2,xml);
      
      if (tree->children)
	process_tree(fp,tree->children,indent+2,xml);
      break;
    case XML_COMMENT_NODE:
      if (fp)
	fprintf(fp,"Comment node encountered\n");
      break;
    case XML_PI_NODE:
    case XML_TEXT_NODE:
      /* Silently ignore these for now */
      break;
    default:
      if (fp) {
	if ((tree->type > 0) && (tree->type < NXMLTYPES))
	  fprintf(fp,"Node type %s encountered with name %s\n",
		  xmltypes[tree->type],(char *)tree->name);
	else
	  fprintf(fp,"Node type %d encountered\n",tree->type);
      }
    }
    tree = tree->next;
  }
}
	
void read_xml(char *fn,int *step,real *t,real *lambda,
	      t_inputrec *ir,rvec *box,int *natoms,
	      rvec **x,rvec **v,rvec **f,t_topology *top)
{
  xmlDocPtr  doc;
  t_xmlrec   *xml;
  
  xmlDoValidityCheckingDefaultValue = 1;
  assert(asize(exml_names) == exmlNR);
  if ((doc = xmlParseFile(fn)) == NULL)
    fatal_error(0,"Reading XML file %s. Run a syntax checker such as nsgmls.",
		fn);

  snew(xml,1);
  xml->ir     = ir;
  xml->box    = box;
  xml->top    = top;
  process_tree(debug,doc->children,0,xml);
  
  xmlFreeDoc(doc);
  sfree(xml);
}

static char *itoa(int i)
{
  static char buf[32];
  sprintf(buf,"%d",i);
  return buf;
}

static char *dtoa(double d)
{
  static char buf[32];
  sprintf(buf,"%g",d);
  return buf;
}

static void add_xml_int(xmlNodePtr ptr,char *name,int val)
{
  if (xmlSetProp(ptr,name,itoa(val)) == 0)
    fatal_error(0,"Setting %s %d",name,val);
}

static void add_xml_real(xmlNodePtr ptr,char *name,real val)
{
  if (xmlSetProp(ptr,name,dtoa(val)) == 0)
    fatal_error(0,"Setting %s %f",name,val);
}

static void add_xml_rvec(xmlNodePtr parent,int id,rvec val)
{
  xmlNodePtr rvptr;
  int  m;

  if ((rvptr = xmlNewChild(parent,NULL,"rvec",NULL)) == NULL)
    fatal_error(0,"Creating rvec element");
  add_xml_int(rvptr,"id",id);
  for(m=0; (m<DIM); m++)
    if (xmlSetProp(rvptr,xyz_names[m],dtoa(val[m])) == 0)
      fatal_error(0,"Setting %s %f",xyz_names[m],val[m]);
}

static void add_xml_tensor(xmlNodePtr parent,tensor val)
{
  xmlNodePtr tptr;
  int  m,n;

  if ((tptr = xmlNewChild(parent,NULL,"tensor",NULL)) == NULL)
    fatal_error(0,"Creating tensor element");
    
  for(m=0; (m<DIM); m++)
    add_xml_real(tptr,tensor_names[m*DIM+m],val[m][m]);
  if ((val[XX][YY] != 0) || (val[XX][ZZ] != 0) ||
      (val[YY][XX] != 0) || (val[YY][ZZ] != 0) ||
      (val[ZZ][XX] != 0) || (val[ZZ][YY] != 0)) {
    for(m=0; (m<DIM); m++)
      for(n=0; (n<DIM); n++)
	if (m != n)
	  add_xml_real(tptr,tensor_names[m*DIM+n],val[m][n]);
  }
}

static void add_xml_char(xmlNodePtr ptr,char *name,char *val)
{
  if (xmlSetProp(ptr,name,val) == 0)
    fatal_error(0,"Setting %s %s",name,val);
}

static xmlNodePtr add_xml_child(xmlNodePtr parent,int type)
{
  xmlNodePtr child;
  
  if ((child = xmlNewChild(parent,NULL,exml_names[type],NULL)) == NULL)
    fatal_error(0,"Creating %s element",exml_names[type]);
    
  return child;
}

static xmlNodePtr add_xml_comment(xmlDocPtr doc,
				  xmlNodePtr prev,char *comment)
{
  xmlNodePtr comm,ptr;
  
  if ((comm = xmlNewComment((xmlChar *)comment)) == NULL)
    fatal_error(0,"Creating doc comment element");
  ptr = prev;
  while (ptr->next != NULL)
    ptr=ptr->next;
  ptr->next    = comm;
  comm->prev   = ptr;
  comm->doc    = doc;
  
  return comm;
}

static void add_xml_inputrec(xmlNodePtr parent,t_inputrec *ir,t_atoms *atoms)
{
  int        i;
  xmlNodePtr irptr,outputptr,tcptr,tcparm,pcptr,refpres,compress;
  xmlNodePtr cutoffptr,pmeptr;
  
  irptr = add_xml_child(parent,exmlINPUTREC);
  add_xml_char(irptr,"algorithm",ei_names[ir->eI]);
  add_xml_int(irptr,"nsteps",ir->nsteps);
  add_xml_real(irptr,"init-t",ir->init_t);
  add_xml_real(irptr,"delta-t",ir->delta_t);

  outputptr = add_xml_child(irptr,exmlOUTPUT);
  add_xml_int(outputptr,"log",ir->nstlog);
  add_xml_int(outputptr,"x-trr",ir->nstxout);
  add_xml_int(outputptr,"v-trr",ir->nstvout);
  add_xml_int(outputptr,"f-trr",ir->nstfout);
  add_xml_int(outputptr,"energy",ir->nstenergy);
  add_xml_int(outputptr,"x-xtc",ir->nstxtcout);
  add_xml_int(outputptr,"xtc-precision",ir->xtcprec);
  
  tcptr = add_xml_child(irptr,exmlTCOUPLING);
  add_xml_char(tcptr,"algorithm",etcoupl_names[ir->etc]);
  add_xml_char(tcptr,"annealing",yesno_names[ir->bSimAnn]);
  add_xml_real(tcptr,"annealtime",ir->zero_temp_time);
  
  assert(ir->opts.ngtc == atoms->grps[egcTC].nr);
  for(i=0; (i<ir->opts.ngtc); i++) {
    tcparm = add_xml_child(tcptr,exmlTCPARM);
    add_xml_char(tcparm,"groupname",*atoms->grpname[atoms->grps[egcTC].nm_ind[i]]);
    add_xml_real(tcparm,"t-ref",ir->opts.ref_t[i]);
    add_xml_real(tcparm,"tau-t",ir->opts.tau_t[i]);
  }
  
  pcptr = add_xml_child(irptr,exmlPCOUPLING);
  add_xml_char(pcptr,"algorithm",epcoupl_names[ir->epc]);
  add_xml_char(pcptr,"type",epcoupltype_names[ir->epct]);
  add_xml_real(pcptr,"tau-p",ir->tau_p);

  refpres = add_xml_child(pcptr,exmlREFPRES);
  add_xml_tensor(refpres,ir->ref_p);
    
  compress = add_xml_child(pcptr,exmlCOMPRESS);
  add_xml_tensor(compress,ir->compress);
    
  cutoffptr = add_xml_child(irptr,exmlCUTOFF);
  add_xml_real(cutoffptr,"rlist",ir->rlist);
  add_xml_real(cutoffptr,"rvdw",ir->rvdw);
  add_xml_real(cutoffptr,"rcoulomb",ir->rcoulomb);
  add_xml_real(cutoffptr,"rcoulswitch",ir->rcoulomb_switch);
  add_xml_real(cutoffptr,"rvdwswitch",ir->rvdw_switch);
  add_xml_real(cutoffptr,"epsilonr",ir->epsilon_r);
  add_xml_int(cutoffptr,"nstlist",ir->nstlist);
  add_xml_char(cutoffptr,"nstype",ens_names[ir->ns_type]);
  add_xml_char(cutoffptr,"domdecomp",yesno_names[ir->bDomDecomp]);
  add_xml_char(cutoffptr,"decompdir",xyz_names[ir->decomp_dir]);
  add_xml_char(cutoffptr,"coulombtype",eel_names[ir->coulombtype]);
  add_xml_char(cutoffptr,"vdwtype",evdw_names[ir->vdwtype]);
  if (ir->coulombtype == eelPME) {
    pmeptr = add_xml_child(cutoffptr,exmlPMEPARM);
    add_xml_int(pmeptr,"nkx",ir->nkx);
    add_xml_int(pmeptr,"nky",ir->nky);
    add_xml_int(pmeptr,"nkz",ir->nkz);
    add_xml_int(pmeptr,"pmeorder",ir->pme_order);
    add_xml_real(pmeptr,"ewaldrtol",ir->ewald_rtol);
    add_xml_real(pmeptr,"epssurface",ir->epsilon_surface);
    add_xml_char(pmeptr,"optfft",yesno_names[ir->bOptFFT]);
  }
}

static void add_xml_molecule(xmlNodePtr parent,t_atoms *atoms,
			     int nmt,t_masstype mt[])
{
  xmlNodePtr ptr;
  
  ptr = add_xml_child(parent,exmlMOLECULE);
}

static void add_xml_idef(xmlNodePtr parent,t_idef *idef)
{
  
}

static void add_xml_rvecs(xmlNodePtr parent,int type,int natoms,rvec *xvf)
{
  xmlNodePtr xptr;
  int i;
  
  xptr = add_xml_child(parent,type);
  for(i=0; (i<natoms); i++)
    add_xml_rvec(xptr,i+1,xvf[i]);
}

static t_masstype *mk_masstype(int nmol,t_atoms atoms[],int *nmt)
{
  int        i,j,k,nm;
  t_masstype *mt=NULL;

  nm = 0;
  for(i=0; (i<nmol); i++) {
    for(j=0; (j<atoms[i].nr); j++) {
      for(k=0; (k<nm); k++)
	if (strcmp(*atoms[i].atomname[j],mt[k].name) == 0)
	  break;
      if (k == nm) {
	srenew(mt,nm+1);
	mt[nm].name  = strdup(*atoms[i].atomname[j]);
	mt[nm].value = atoms[i].atom[j].m;
	nm++;
      }
    }
  }
  *nmt = nm;
  
  return mt;
}

static void add_xml_masstype(xmlNodePtr parent,int nmt,t_masstype mt[])
{
  int i;
  xmlNodePtr ptr;
  
  for(i=0; (i<nmt); i++) {
    ptr = add_xml_child(parent,exmlMASSTYPE);
    add_xml_char(ptr,"name",mt[i].name);
    add_xml_real(ptr,"value",mt[i].value);
  }
}

void write_xml(char *fn,char *title,t_inputrec *ir,rvec *box,
	       int natoms,rvec *x,rvec *v,rvec *f,
	       int nmol,t_atoms atoms[],t_idef *idef)
{
  xmlDocPtr  doc;
  xmlDtdPtr  dtd;
  xmlNodePtr myroot;
  t_masstype *mt;
  int        i,nmt;
  char       *libdtdname,*dtdname;
  
  dtdname    = "gromacs.dtd";
  libdtdname = libfn(dtdname);
  
  if ((doc = xmlNewDoc("1.0")) == NULL)
    fatal_error(0,"Creating XML document");
    
  if ((dtd = xmlCreateIntSubset(doc,"gromacs",libdtdname,dtdname)) == NULL)
    fatal_error(0,"Creating XML DTD");
    
  if ((myroot = xmlNewDocNode(doc,NULL,"gromacs",NULL)) == NULL)
    fatal_error(0,"Creating root element");
  dtd->next = myroot;
  myroot->prev = (xmlNodePtr) dtd;
    
  /* Title of the system */
  if (title)
    add_xml_char(myroot,"title",title);
  
  /* Add inputrec */
  if (ir) {
    xmlNodePtr ptr = myroot;
    while(ptr->next != NULL)
      ptr = ptr->next;
    ptr->next = xmlNewDocComment(doc,(xmlChar *)"Here starts parameter section");
    ptr->next->prev = ptr;
    fprintf(stderr,"Comment type is %s\n",xmltypes[ptr->next->type]);
    
    add_xml_inputrec(myroot,ir,&atoms[0]);
  }
  /* Generate masstypes */
  mt = mk_masstype(nmol,atoms,&nmt);
  add_xml_masstype(myroot,nmt,mt);
  
  /* Add molecule definitions */
  for(i=0; (i<nmol); i++)
    add_xml_molecule(myroot,&(atoms[i]),nmt,mt);

  /* Add force field */  
  if (idef)
    add_xml_idef(myroot,idef);

  /* Box */
  if (box) 
    add_xml_tensor(add_xml_child(myroot,exmlBOX),box);
  
  /* Coordinates */
  if (x)
    add_xml_rvecs(myroot,exmlCOORDINATES,natoms,x);
  
  /* Velocities */
  if (v)
    add_xml_rvecs(myroot,exmlVELOCITIES,natoms,v);
  
  /* Forces */  
  if (f)
    add_xml_rvecs(myroot,exmlFORCES,natoms,f);
  
  xmlSetDocCompressMode(doc,0);
  xmlIndentTreeOutput = 1;
  if (xmlSaveFormatFileEnc(fn,doc,"ISO-8859-1",2) == 0)
    fatal_error(0,"Saving file %s",fn);
  xmlFreeDoc(doc);
}

#endif
