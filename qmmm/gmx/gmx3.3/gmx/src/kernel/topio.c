/*
 * $Id: topio.c,v 1.50 2002/06/07 16:51:00 lindahl Exp $
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
 * Gromacs Runs One Microsecond At Cannonball Speeds
 */
static char *SRCID_topio_c = "$Id: topio.c,v 1.50 2002/06/07 16:51:00 lindahl Exp $";
#ifdef HAVE_IDENT
#ident "@(#) topio.c 1.87 9/30/97"
#endif

#include <math.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "futil.h"
#include "assert.h"
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "macros.h"
#include "txtdump.h"
#include "physics.h"
#include "macros.h"
#include "names.h"
#include "string2.h"
#include "symtab.h"
#include "fatal.h"
#include "dum_parm.h"

#include "toputil.h"
#include "toppush.h"
#include "topdirs.h"
#include "topexcl.h"
#include "topcat.h"
#include "topio.h"
#include "topshake.h"

#define CPPMARK  	'#'	/* mark from cpp			*/
#define OPENDIR  	'['	/* starting sign for directive		*/
#define CLOSEDIR 	']'	/* ending sign for directive		*/

#define PM()  \
  printf("line: %d, maxavail: %d\n",__LINE__,maxavail()); \
  fflush(stdout)

static void free_nbparam(t_nbparam **param,int nr)
{
  int i;

  for(i=0; i<nr; i++)
    sfree(param[i]);
  sfree(param);
}

static int copy_nbparams(t_nbparam **param,int ftype,t_params *plist,int nr)
{
  int i,j,f;
  int nrfp,ncopy;

  nrfp = NRFP(ftype);
  
  ncopy = 0;
  for(i=0; i<nr; i++)
    for(j=0; j<=i; j++)
      if (param[i][j].bSet) {
	for(f=0; f<nrfp; f++) {
	  plist->param[nr*i+j].c[f] = param[i][j].c[f];
	  plist->param[nr*j+i].c[f] = param[i][j].c[f];
	}
	ncopy++;
      }
  
  return ncopy;
}

static void gen_pairs(t_params *nbs,t_params *pairs,real fudge, bool bVerbose)
{
  int     i,j,ntp,nrfp,nrfpA,nrfpB,nnn;

  ntp       = nbs->nr;
  nnn       = sqrt(ntp);
  nrfp      = NRFP(F_LJ);
  nrfpA     = interaction_function[F_LJ14].nrfpA;
  nrfpB     = interaction_function[F_LJ14].nrfpB;
  pairs->nr = ntp;
  
  assert(nrfp  == nrfpA);
  assert(nrfpA == nrfpB);

  fprintf(stderr,"Generating 1-4 interactions: fudge = %g\n",fudge);
  if (debug) {
    fprintf(debug,"Fudge factor for 1-4 interactions: %g\n",fudge);
    fprintf(debug,"Holy Cow! there are %d types\n",ntp);
  }
  snew(pairs->param,pairs->nr);
  for(i=0; (i<ntp); i++) {
    /* Copy param.a */
    pairs->param[i].a[0] = i / nnn;
    pairs->param[i].a[1] = i % nnn;
    /* Copy normal and FEP parameters and multiply by fudge factor */
    for(j=0; (j<nrfp); j++) {
      pairs->param[i].c[j]=fudge*nbs->param[i].c[j];
      pairs->param[i].c[nrfp+j]=fudge*nbs->param[i].c[j];
    }
  }
}

real check_mol(t_atoms *atoms)
{
  char    buf[256];
  int     i,rn,pt;
  real    q,m;

  /* Check mass and charge */
  q=0.0;
  for (i=0; (i<atoms->nr); i++) {
    q += atoms->atom[i].q;
    m  = atoms->atom[i].m;
    pt = atoms->atom[i].ptype;
    /* If the particle is an atom or a nucleus it must have a mass,
     * else, if it is a shell, a dummy or a bondshell it can have mass zero
     */
    if ((m <= 0.0) && ((pt == eptAtom) || (pt == eptNucleus))) {
      rn=atoms->atom[i].resnr;
      sprintf(buf,"atom %s (Res %s-%d) has mass %g\n",
	      *(atoms->atomname[i]),*(atoms->resname[rn]),rn+1,m);
      warning(buf);
    } else 
      if ((m!=0) && (pt == eptDummy)) {
	rn=atoms->atom[i].resnr;
	sprintf(buf,"dummy atom %s (Res %s-%d) has non-zero mass %g,"
		"will be set to zero\n",
		*(atoms->atomname[i]),*(atoms->resname[rn]),rn+1,m);
	warning(buf);
	atoms->atom[i].m=0;
      }
  }
  return q;
}

static void sum_q(t_atoms *atoms,int n,double *qt,double *qBt)
{
  int     i;

  /* sum charge */
  for (i=0; (i<atoms->nr); i++) {
    *qt  += n*atoms->atom[i].q;
    *qBt += n*atoms->atom[i].qB;
  }
}

  
void preprocess(char *infile,char *outfile,
		char *cpp,char *define,
		char *include)
{
  static char libdir[1024];
  static bool bFirst=TRUE;
  char *lib;
  char command[2048];
  int  error;

  if (bFirst) {
    lib=getenv("GMXLIB");
    if (lib!=NULL) {
      strcpy(libdir,lib);
    } 
    else {
      if(!get_libdir(libdir))
	strcpy(libdir,GMXLIBDIR);
    }
    bFirst=FALSE;
  }

  /* build the command line. Second output name is not supported 
   * on OS X it seems, so we use redirection instead in that case.
   */
#ifdef __APPLE__
  sprintf(command,"%s %s -I%s %s %s > %s",
	  cpp,include,libdir,define,infile,outfile);
#else
  sprintf(command,"%s %s -I%s %s %s > %s",
	  cpp,include,libdir,define,infile,outfile);
#endif
  
  if (debug)
    fprintf(debug,"Command line for cpp:\n\t%s\n",command);

  /* execute preprocessor */
  error=system(command);
  if (error) {
    if (error>0)
      printf("cpp exit code: %d\n",error);
    else if (error<0)
      perror(cpp);
    printf("Tried to execute: '%s'\n",command); 
    printf("The '%s' command is defined in the .mdp file\n",cpp);
    if (error<0)
      fatal_error(0,"cpp failed");
  }
}

static void get_nbparm(char *nb_str,char *comb_str,int *nb,int *comb)
{
  int i;
  
  *nb   = -1;
  for(i=1; (i<eNBF_NR); i++)
    if (strcasecmp(nb_str,enbf_names[i]) == 0)
      *nb = i;
  if (*nb == -1)
    *nb = atoi(nb_str);
  if ((*nb < 1) || (*nb >= eNBF_NR)) {
    sprintf(warn_buf,"Invalid nonbond function selector '%s' using %s",
	    nb_str,enbf_names[1]);
    warning(NULL);
    *nb = 1;
  }
  *comb = -1;
  for(i=1; (i<eCOMB_NR); i++)
    if (strcasecmp(comb_str,ecomb_names[i]) == 0)
      *comb = i;
  if (*comb == -1)
    *comb = atoi(comb_str);
  if ((*comb < 1) || (*comb >= eCOMB_NR)) {
    sprintf(warn_buf,"Invalid combination rule selector '%s' using %s",
	    comb_str,ecomb_names[1]);
    warning(NULL);
    *comb = 1;
  }
}

static char **read_topol(char        *infile,
			 t_symtab    *symtab,
			 t_atomtype  *atype,
			 int         *nrmols,
			 t_molinfo   **molinfo,
			 t_params    plist[],
			 int         nshake,
			 real        *fudgeQQ,
			 int         *nsim,
			 t_simsystem **sims,
			 bool        bVerbose,
			 int         *combination_rule)
{
  FILE       *in;
  int        i,nb_funct,comb;
  char       *pline,**title=NULL;
  int        curline;
  char       curfile[STRLEN],line[STRLEN],errbuf[256],comb_str[256],nb_str[256];
  char       genpairs[32];
  char       *dirstr,*dummy2;
  int        nrcopies,nmol,nblock=0,Nsim=0,nscan,ncombs,ncopy;
  double     fLJ,fQQ,fPOW;
  t_simsystem *Sims=NULL;
  t_topology *block=NULL;
  t_molinfo  *blockinfo=NULL;
  t_topology *blk0;
  t_molinfo  *mi0=NULL,*bi0;
  DirStack   *DS;
  directive  d,newd;
  t_nbparam  **nbparam,**pair;
  t_block2   *block2;
  real       fudgeLJ=-1;    /* Multiplication factor to generate 1-4 from LJ */
  real       npow=12.0;     /* Default value for repulsion power */
  bool       bReadDefaults,bReadMolType,bGenPairs;
  double     qt=0,qBt=0; /* total charge */
  t_bond_atomtype *batype;
  
  /* open input and output file */
  if ((in = fopen(infile,"r")) == NULL)
    fatal_error(0,"Could not open %s",infile);

  /* some local variables */
  DS_Init(&DS);			/* directive stack			 */
  strcpy(curfile,infile);	/* filename    				 */
  nmol     = 0;			/* no molecules yet...			 */
  curline  = 0;              	/* line number 				 */
  d        = d_invalid;         /* first thing should be a directive 	 */
  nbparam  = NULL;              /* The temporary non-bonded matrix       */
  pair     = NULL;              /* The temporary pair interaction matrix */
  block2   = NULL;		/* the extra exclusions			 */
  nb_funct = F_LJ;
  
  snew(batype,1);
  init_bond_atomtype(batype);
  /* parse the actual file */
  bReadDefaults = FALSE;
  bGenPairs     = FALSE;
  bReadMolType  = FALSE;
  while (fgets2(line,STRLEN-2,in) != NULL) {
    curline++;
    assert (pline = strdup(line));
    
    /* build one long line from several fragments */
    while (continuing(line) && (fgets2(line,STRLEN-1,in) != NULL)) {
      curline++;
      srealloc(pline,strlen(pline)+strlen(line)+1);
      strcat(pline,line);
    }

    /* skip trailing and leading spaces and comment text */
    strip_comment (pline);
    trim (pline);

    /* if there is something left... */
    if ((int)strlen(pline) > 0) {
      if (pline[0] == CPPMARK) {
	/* A cpp-leftover on this line:
	 * the file and line number for debug info
	 */
	if (sscanf ((pline+1),"%d %s",&curline,curfile) == 2) {
	  /* Read file and linenumber ok, don't count the cpp mark  */
	  curline--;
	}
      }
      else if (pline[0] == OPENDIR) {
	/* A directive on this line: copy the directive 
	 * without the brackets into dirstr, then
	 * skip spaces and tabs on either side of directive
	 */
	dirstr = strdup((pline+1));
	if ((dummy2 = strchr (dirstr,CLOSEDIR)) != NULL)
	  (*dummy2) = 0;
	trim (dirstr);

	if ((newd = str2dir(dirstr)) == d_invalid) {
	  sprintf(errbuf,"Invalid directive %s",dirstr);
	  warning(errbuf);
	}
	else {
	  /* Directive found */
	  if (debug) 
	    fprintf(debug,"found directive '%s'\n",dir2str(newd));
	  if (DS_Check_Order (DS,newd)) {
	    DS_Push (&DS,newd);
	    d = newd;
	  }
	  else {
	    /* we should print here which directives should have
	       been present, and which actually are */
	    fatal_error(0,"Invalid order for directive %s, file \"%s\", line %d",dirstr,curfile,curline);
	    /* d = d_invalid; */
	  }
	}
	sfree(dirstr);
      }
      else if (d != d_invalid) {
	/* Not a directive, just a plain string 
	 * use a gigantic switch to decode,
         * if there is a valid directive!
	 */
	if (debug) {
	  fprintf(debug,"%s : %4d : %s\n",curfile,curline,pline);
	  fflush(debug);
	}
	set_warning_line(curfile,curline);
	
	switch (d) {
	case d_defaults:
	  if (bReadDefaults)
	    fatal_error(0,"Found a second defaults directive, file %s, line %d",curfile,curline);
	  bReadDefaults = TRUE;
	  nscan = sscanf(pline,"%s%s%s%lf%lf%lf",
			 nb_str,comb_str,genpairs,&fLJ,&fQQ,&fPOW);
	  if (nscan < 2)
	    too_few();
	  else {
	    bGenPairs = FALSE;
	    fudgeLJ   = 1.0;
	    *fudgeQQ  = 1.0;
	    
	    get_nbparm(nb_str,comb_str,&nb_funct,&comb);
	    *combination_rule = comb;
	    if (nscan >= 3) 
	      bGenPairs = (strncasecmp(genpairs,"Y",1) == 0);
	    if (nscan >= 4)
	      fudgeLJ   = fLJ;
	    if (nscan >= 5)
	      *fudgeQQ  = fQQ;
	    if (nscan >= 6)
	      npow      = fPOW;
	  }
	  nb_funct = ifunc_index(d_nonbond_params,nb_funct);
	  
	  break;
	case d_atomtypes:
	  push_at(symtab,atype,batype,pline,nb_funct,
		  &nbparam,bGenPairs ? &pair : NULL);
	  break;

#define PUSHBT(nral) push_bt(d,plist,nral,batype->atomname,batype->nr,pline)
	case d_bondtypes:
	  PUSHBT(2);
	  break;
	case d_constrainttypes:
	  PUSHBT(2);
	  break;
	case d_pairtypes:
	  if (bGenPairs)
	    push_nbt(d,pair,atype,pline,nb_funct);
	  else
	    push_bt(d,plist,2,atype->atomname,atype->nr,pline);
	  break;
	case d_angletypes:
	  PUSHBT(3);
	  break;
	case d_dihedraltypes:
	  /* Special routine that can read both 2 and 4 atom dihedral definitions. */
	  push_dihedraltype(d,plist,batype->atomname,batype->nr,pline);
	  break;
#undef PUSHBT

	case d_nonbond_params:
	  push_nbt(d,nbparam,atype,pline,nb_funct);
	  break;
	case d_blocktype:
	  nblock++;
	  srenew(block,nblock);
	  srenew(blockinfo,nblock);
	  blk0=&(block[nblock-1]);
	  bi0=&(blockinfo[nblock-1]);
	  init_top(blk0);
	  init_molinfo(bi0);
	  push_molt(symtab,bi0,pline);
	  break;
	case d_moleculetype: {
	  if (!bReadMolType) {
	    ncombs = atype->nr*(atype->nr+1)/2;
	    generate_nbparams(comb,nb_funct,&(plist[nb_funct]),atype,npow);
	    ncopy = copy_nbparams(nbparam,nb_funct,&(plist[nb_funct]),
				  atype->nr);
	    fprintf(stderr,"Generated %d of the %d non-bonded parameter combinations\n",ncombs-ncopy,ncombs);
	    free_nbparam(nbparam,atype->nr);
	    
	    if (bGenPairs) {
	      gen_pairs(&(plist[nb_funct]),&(plist[F_LJ14]),fudgeLJ,bVerbose);
	      ncopy = copy_nbparams(pair,nb_funct,&(plist[F_LJ14]),
				    atype->nr);
	      fprintf(stderr,"Generated %d of the %d 1-4 parameter combinations\n",ncombs-ncopy,ncombs);
	      free_nbparam(pair,atype->nr);
	    }
	    bReadMolType = TRUE;
	  }
	  
	  nmol++;
	  srenew((*molinfo),nmol);
	  srenew(block2,nmol);
	  mi0=*molinfo;
	  mi0=&(mi0[nmol-1]);
	  init_molinfo(mi0);
	  block2[nmol-1].nr=0;
	  push_molt(symtab,mi0,pline);
	  break;
	}
	case d_atoms: 
	  push_atom(symtab,&(mi0->cgs),&(mi0->atoms),atype,pline);
	  break;
	  
	case d_pairs: 
	  push_bond(d,plist,mi0->plist,&(mi0->atoms),atype,pline,FALSE,bGenPairs);
	  break;
	  
	case d_dum2:
	case d_dum3:
	case d_dum4:
        case d_bonds:
	case d_angles:
	case d_constraints:
	case d_settles:
	case d_position_restraints:
	case d_angle_restraints:
	case d_angle_restraints_z:
	case d_distance_restraints: 
	case d_orientation_restraints:
	case d_dihedrals:
	  push_bond(d,plist,mi0->plist,&(mi0->atoms),atype,pline,TRUE,bGenPairs);
	  break;

	case d_exclusions:
	  if (!block2[nmol-1].nr)
	    init_block2(&(block2[nmol-1]),mi0->atoms.nr);
	  push_excl(pline,&(block2[nmol-1]));
	  break;
	case d_system: 
	  trim(pline);
	  title=put_symtab(symtab,pline);
	  break;
	case d_molecules: {
	  int whichmol;

	  push_mol(nmol,*molinfo,pline,&whichmol,&nrcopies);
	  mi0=*molinfo;
	  mi0=&(mi0[whichmol]);
	  srenew(Sims,Nsim+1);
	  Sims[Nsim].whichmol=whichmol;
	  Sims[Nsim].nrcopies=nrcopies;
	  Nsim++;
	  if (mi0->atoms.nr == 0)
	    fatal_error(0,"Moleculetype %s contains no atoms",*mi0->name);
	  fprintf(stderr,"Excluding %d bonded neighbours for %s\n",
		    mi0->nrexcl,pline);
	  if (!mi0->bProcessed) {
	    sum_q(&mi0->atoms,nrcopies,&qt,&qBt);
	    generate_excl(mi0->nrexcl,
			  mi0->atoms.nr,
			  mi0->plist,
			  &(mi0->atoms.excl));
	    merge_excl(&(mi0->atoms.excl),&(block2[whichmol]));
	    done_block2(&(block2[whichmol]));
	    make_shake(mi0->plist,&mi0->atoms,atype,nshake); 
	    stupid_fill(&mi0->mols,mi0->atoms.nr,TRUE);
	    mi0->bProcessed=TRUE;
	  }
	  break;
	}
	default:
	  fprintf (stderr,"case: %d\n",d);
	  invalid_case();
	}
      }
    }
    sfree(pline);
    pline=NULL;
  }
  /* this is not very clean, but fixes core dump on empty system name */
  if(!title)
    title=put_symtab(symtab,"");
  if (fabs(qt) > 1e-5) {
    sprintf(errbuf,"System has non-zero total charge: %e\n",qt);
    warning(errbuf);
  }
  if (fabs(qBt) > 1e-5 && qBt != qt) {
    sprintf(errbuf,"State B has non-zero total charge: %e\n",qBt);
    warning(errbuf);
  }
  fclose (in);
  DS_Done (&DS);
  for(i=0; i<nmol; i++)
    done_block2(&(block2[i]));
  free(block2);
  *nrmols=nmol;
  
  *nsim=Nsim;
  *sims=Sims;
  
  return title;
}

char **do_top(bool         bVerbose,
	      char         *topfile,
	      char         *topppfile,
	      t_gromppopts *opts,
	      t_symtab     *symtab,
	      t_params     plist[],
	      t_atomtype   *atype,
	      int          *nrmols,
	      t_molinfo    **molinfo,
	      t_inputrec   *ir,
	      int          *nsim,
	      t_simsystem  **sims)
{
  /* Tmpfile might contain a long path */
  char tmpfile[32];
  char **title;
  int  combination_rule;
  
  init_atomtype(atype);

  if (bVerbose) printf("calling %s...\n",opts->cpp);
  if (topppfile)
    strcpy(tmpfile,topppfile);
  else {
    strcpy(tmpfile,"gromppXXXXXX");
    gmx_tmpnam(tmpfile);
    set_fatal_tmp_file(tmpfile);
  }
  preprocess(topfile,tmpfile,opts->cpp,opts->define,opts->include);

  if (bVerbose) printf("processing topology...\n");
  title=read_topol(tmpfile,symtab,atype,nrmols,molinfo,
		   plist,opts->nshake,&ir->fudgeQQ,nsim,sims,bVerbose,
		   &combination_rule);
  if ((combination_rule != eCOMB_ARITHMETIC) && 
      (ir->vdwtype == evdwUSER)) {
    warning("Using sigma/epsilon based combination rules with"
	    " user supplied potential function may produce unwanted"
	    " results");
  }
  if (!topppfile) {
    if (unlink(tmpfile) != 0)
      perror ("Unable to remove temporary file");
    unset_fatal_tmp_file(tmpfile);
  }
    
  return title;
}


void generate_qmexcl(t_topology *sys,t_inputrec *ir)
{
  /* generates the exclusions between the individual QM atoms, as
   * these interactions should be handled by the QM subroutines and
   * not by the gromacs routines 
   */
  int
    i,j,l,k=0,jmax,qm_max=0,qm_nr=0,nratoms=0,link_nr=0,link_max=0;
  atom_id 
    *qm_arr=NULL,*link_arr=NULL,a1,a2,a3,a4,ftype=0;
  t_block 
    qmexcl;
  t_block2
    qmexcl2;
  bool 
    *bQMMM,*blink,bexcl;

  /* First we search and select the QM atoms in an qm_arr array that
   * we use to create the exclusions.
   *
   * we take the possibility into account that a user has defined more
   * than one QM group:
   *
   * for that we also need to do this an ugly work-about just in case
   * the QM group contains the entire system...  
   */
  fprintf(stderr,"excluding classical QM-QM interactions...\n");

  jmax = ir->opts.ngQM;

  /* we first search for all the QM atoms and put them in an array 
   */
  for(j=0;j<jmax;j++){
    for(i=0;i<sys->atoms.nr;i++){
      if(qm_nr>=qm_max){
	qm_max += 100;
	srenew(qm_arr,qm_max);
      }
      if(sys->atoms.atom[i].grpnr[egcQMMM]==j){
	qm_arr[qm_nr++] = i;
      }
    }
  }  
  /* bQMMM[..] is an array containin TRUE/FALSE for atoms that are
   * QM/not QM. We first set all elements to false. Afterwards we use
   * the qm_arr to change the elements corresponding to the QM atoms
   * to TRUE.  
   */
  snew(bQMMM,sys->atoms.nr);
  for (i=0;i<sys->atoms.nr;i++)
    bQMMM[i]=FALSE;
  for (i=0;i<qm_nr;i++)
    bQMMM[qm_arr[i]]=TRUE;
  
  /* We remove all bonded interactions (i.e. bonds,
   * angles, dihedrals, 1-4's), involving the QM atoms. The way they
   * are removed is as follows: if the interaction invloves 2 atoms,
   * it is removed if both atoms are QMatoms. If it involves 3 atoms,
   * it is removed if at least two of the atoms are QM atoms, if the
   * interaction involves 4 atoms, it is removed if there are at least
   * 2 QM atoms.  Since this routine is called once before any forces
   * are computed, the top->idef.il[N].iatom[] array (see idef.h) can
   * be rewritten at this poitn without any problem. 25-9-2002 */
  
  /* first check weter we already have CONNBONDS: */
  if (sys->idef.il[F_CONNBONDS].nr != 0){
    fprintf(stderr,"nr. of CONNBONDS present already: %d\n",
	    sys->idef.il[F_CONNBONDS].nr/3);
    ftype = sys->idef.il[F_CONNBONDS].iatoms[0];
    k = sys->idef.il[F_CONNBONDS].nr;
  }
  /* now we delete all bonded interactions, except the ones describing
   * a chemical bond. These are converted to CONNBONDS
   */
  for (i=0;i<F_LJ;i++){
    if(i==F_CONNBONDS)
      continue;
    /* temporary hack: */
    if(i==F_IDIHS)
      continue;
    nratoms = interaction_function[i].nratoms;
    j = 0;
    while (j<sys->idef.il[i].nr){
      bexcl = FALSE;
      switch (nratoms){
      case 2:
	a1 = sys->idef.il[i].iatoms[j+1];
	a2 = sys->idef.il[i].iatoms[j+2];
	bexcl = (bQMMM[a1] && bQMMM[a2]);
	/* a bonded beteen two QM atoms will be copied to the
  	 * CONNBONDS list, for reasons mentioned above 
	 */
	if(bexcl && i<F_ANGLES){
	  srenew(sys->idef.il[F_CONNBONDS].iatoms,k+3);
	  sys->idef.il[F_CONNBONDS].nr         += 3; 
	  sys->idef.il[F_CONNBONDS].iatoms[k++] = ftype;
	  sys->idef.il[F_CONNBONDS].iatoms[k++] = a1;
	  sys->idef.il[F_CONNBONDS].iatoms[k++] = a2;
	}
	break;
      case 3:
	a1 = sys->idef.il[i].iatoms[j+1];
	a2 = sys->idef.il[i].iatoms[j+2];
	a3 = sys->idef.il[i].iatoms[j+3];
	bexcl = ((bQMMM[a1] && bQMMM[a2]) ||
		 (bQMMM[a1] && bQMMM[a3]) ||
		 (bQMMM[a2] && bQMMM[a3]));
	break;
      case 4:
	a1 = sys->idef.il[i].iatoms[j+1];
	a2 = sys->idef.il[i].iatoms[j+2];
	a3 = sys->idef.il[i].iatoms[j+3];
	a4 = sys->idef.il[i].iatoms[j+4];
	bexcl = ((bQMMM[a1] && bQMMM[a2] && bQMMM[a3]) ||
		 (bQMMM[a1] && bQMMM[a2] && bQMMM[a4]) ||
		 (bQMMM[a1] && bQMMM[a3] && bQMMM[a4]) ||
		 (bQMMM[a2] && bQMMM[a3] && bQMMM[a4]));
	break;
      default:
	fatal_error(0,"no such bonded interactions with %d atoms\n",nratoms);
      }
      if (bexcl){
	/* since the interaction involves QM atoms, these should be
         * removed from the MM ilist 
	 */
	sys->idef.il[i].nr -= (nratoms+1);
	for (l=j;l<sys->idef.il[i].nr;l++)
	  sys->idef.il[i].iatoms[l] = sys->idef.il[i].iatoms[l+(nratoms+1)];  
      } else {
	j += nratoms+1; /* the +1 is for the functype */
      }
    }
  }
  /* Now, we search for atoms bonded to a QM atom because we also want
   * to exclude their nonbonded interactions with the QM atoms. The
   * reason for this is that this interaction is accounted for in the
   * linkatoms interaction with the QMatoms and would be counted
   * twice.  */
 
  
    

  for(i=0;i<F_NRE;i++){
    if(IS_CHEMBOND(i)){
      j=0;
      while(j<sys->idef.il[i].nr){
	a1 = sys->idef.il[i].iatoms[j+1];
	
	a2 = sys->idef.il[i].iatoms[j+2];
	if((bQMMM[a1] && !bQMMM[a2])||(!bQMMM[a1] && bQMMM[a2])){
	  fprintf(stderr,"bingo! atoms %d and %d\n",a1,a2);
	  if(link_nr>=link_max){
	    link_max += 10;
	    srenew(link_arr,link_max);
	  }
	  if(bQMMM[a1]){
	    link_arr[link_nr++] = a2;
	  } else {
	    link_arr[link_nr++] = a1;
	  }
	}
	j+=3;
      }
    }
  }   
  snew(blink,sys->atoms.nr);   
  for (i=0;i<sys->atoms.nr;i++)
    blink[i]=FALSE;
  for (i=0;i<link_nr;i++)
    blink[link_arr[i]]=TRUE;
  /* creating the exclusion block for the QM atoms. Each QM atom has
   * as excluded elements all the other QMatoms (and itself).
   */
  qmexcl.nr = sys->atoms.nr;
  qmexcl.nra = qm_nr*(qm_nr+link_nr)+link_nr*qm_nr; 
  snew(qmexcl.index,qmexcl.nr+1);
  snew(qmexcl.a,qmexcl.nra);
  j=0;
  for(i=0;i<qmexcl.nr;i++){
    qmexcl.index[i]=j;
    if(bQMMM[i]){
      for(k=0;k<qm_nr;k++){
	qmexcl.a[k+j] = qm_arr[k];
      }
      for(k=0;k<link_nr;k++){
	qmexcl.a[qm_nr+k+j] = link_arr[k];
      }
      j+=(qm_nr+link_nr);
    }
    if(blink[i]){
      for(k=0;k<qm_nr;k++){
	qmexcl.a[k+j] = qm_arr[k];
      }
      j+=qm_nr;
    }
  }
  qmexcl.index[qmexcl.nr]=j;
  
  /* and merging with the exclusions already present in sys.
   */

  init_block2(&qmexcl2,sys->atoms.nr);
  b_to_b2(&qmexcl, &qmexcl2);
  merge_excl(&(sys->atoms.excl),&qmexcl2);
  done_block2(&qmexcl2);

  /* Finally, we also need to get rid of the pair interactions of the
   * classical atom bonded to the boundary QM atoms with the QMatoms,
   * as this interaction is already accounted for by the QM, so also
   * here we run the risk of double counting! We proceed in a similar
   * way as we did above for the other bonded interactions: */
  for (i=F_LJ14;i<F_COUL14;i++){
    nratoms = interaction_function[i].nratoms;
    j = 0;
    while (j<sys->idef.il[i].nr){
      bexcl = FALSE;
      a1 = sys->idef.il[i].iatoms[j+1];
      a2 = sys->idef.il[i].iatoms[j+2];
      bexcl = ((bQMMM[a1] && bQMMM[a2])||
	       (blink[a1] && bQMMM[a2])||
	       (bQMMM[a1] && blink[a2]));
      if (bexcl){
	/* since the interaction involves QM atoms, these should be
         * removed from the MM ilist 
	 */
	sys->idef.il[i].nr -= (nratoms+1);
	for (k=j;k<sys->idef.il[i].nr;k++)
	  sys->idef.il[i].iatoms[k] = sys->idef.il[i].iatoms[k+(nratoms+1)];  
      } else {
	j += nratoms+1; /* the +1 is for the functype */
      }
    }
  }  
  
  free(qm_arr);  
  free(bQMMM);
  free(link_arr);
  free(blink);
} /* generate_qmexcl */ 
