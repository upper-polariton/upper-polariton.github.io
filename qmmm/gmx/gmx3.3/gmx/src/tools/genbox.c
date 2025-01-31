/*
 * $Id: genbox.c,v 1.51 2002/02/28 11:00:27 spoel Exp $
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
static char *SRCID_genbox_c = "$Id: genbox.c,v 1.51 2002/02/28 11:00:27 spoel Exp $";
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "assert.h"
#include "string2.h"
#include "physics.h"
#include "confio.h"
#include "copyrite.h"
#include "txtdump.h"
#include "math.h"
#include "macros.h"
#include "random.h"
#include "futil.h"
#include "atomprop.h"
#include "names.h"
#include "vec.h"
#include "pbc.h"
#include "fatal.h"
#include "statutil.h"
#include "vec.h"
#include "gbutil.h"
#include "addconf.h"
#include "pdbio.h"

#ifdef DEBUG
void print_stat(rvec *x,int natoms,matrix box)
{
  int i,m;
  rvec xmin,xmax;
  for(m=0;(m<DIM);m++) {
    xmin[m]=x[0][m];
    xmax[m]=x[0][m];
  }
  for(i=0;(i<natoms);i++) {
    for (m=0;m<DIM;m++) {
      xmin[m]=min(xmin[m],x[i][m]);
      xmax[m]=max(xmax[m],x[i][m]);
    }   
  }
  for(m=0;(m<DIM);m++)
    fprintf(stderr,"DIM %d XMIN %8.3f XMAX %8.3f BOX %8.3f\n",
	    m,xmin[m],xmax[m],box[m][m]);
}
#endif

static bool in_box(matrix box,rvec x)
{
  return( x[XX]>=0.0 && x[XX]<=box[XX][XX] &&
	  x[YY]>=0.0 && x[YY]<=box[YY][YY] &&
	  x[ZZ]>=0.0 && x[ZZ]<=box[ZZ][ZZ] );
}

static void mk_vdw(t_atoms *a, real rvdw[], real r_distance)
{
  int i;
  
  /* initialise van der waals arrays of configuration */
  fprintf(stderr,"Initialising van der waals distances...\n");
  for(i=0; (i < a->nr); i++)
    rvdw[i]=get_vdw( 
		    *(a->resname[a->atom[i].resnr]), *(a->atomname[i]),
		    r_distance);
}

typedef struct {
  char *name;
  int  natoms;
  int  nmol;
  int  i,i0;
  int  res0;
} t_moltypes;

void sort_molecule(t_atoms **atoms_solvt,rvec *x,rvec *v,real *r)
{
  int atnr,i,j,moltp=0,nrmoltypes,resnr;
  t_moltypes *moltypes;
  int *tps;
  t_atoms *atoms,*newatoms;
  rvec *newx, *newv=NULL;
  real *newr;
  
  fprintf(stderr,"Sorting configuration\n");

  atoms = *atoms_solvt;

  /* copy each residue from *atoms to a molecule in *molecule */
  snew(tps,atoms->nr);
  moltypes=NULL;
  nrmoltypes=0;
  atnr=0;
  for (i=0; i<atoms->nr; i++) {
    if ( (i==0) || (atoms->atom[i].resnr != atoms->atom[i-1].resnr) ) {
      /* see if this was a molecule type we haven't had yet: */
      moltp=NOTSET;
      for (j=0; (j<nrmoltypes) && (moltp==NOTSET); j++)
	if (strcmp(*(atoms->resname[atoms->atom[i].resnr]),
		   moltypes[j].name)==0)
	  moltp=j;
      if (moltp==NOTSET) {
	moltp=nrmoltypes;
	nrmoltypes++;
	srenew(moltypes,nrmoltypes);
	moltypes[moltp].name=*(atoms->resname[atoms->atom[i].resnr]);
	atnr = 0;
	while ((i+atnr<atoms->nr) && 
	       (atoms->atom[i].resnr == atoms->atom[i+atnr].resnr))
	  atnr++;
	moltypes[moltp].natoms=atnr;
	moltypes[moltp].nmol=0;
      }
      moltypes[moltp].nmol++;
    }
    tps[i]=moltp;
  }
  
  fprintf(stderr,"Found %d%s molecule type%s:\n",
	  nrmoltypes,nrmoltypes==1?"":" different",nrmoltypes==1?"":"s");
  for(j=0; j<nrmoltypes; j++) {
    if (j==0)
      moltypes[j].res0 = 0;
    else
      moltypes[j].res0 = moltypes[j-1].res0+moltypes[j-1].nmol;
    fprintf(stderr,"%7s (%4d atoms): %5d residues\n",
	    moltypes[j].name,moltypes[j].natoms,moltypes[j].nmol);
  }
  
  /* if we have only 1 moleculetype, we don't have to sort */
  if (nrmoltypes>1) {
    /* find out which molecules should go where: */
    moltypes[0].i = moltypes[0].i0 = 0;
    for(j=1; j<nrmoltypes; j++) {
      moltypes[j].i =
	moltypes[j].i0 = 
	moltypes[j-1].i0+moltypes[j-1].natoms*moltypes[j-1].nmol;
    }
    
    /* now put them there: */
    snew(newatoms,1);
    init_t_atoms(newatoms,atoms->nr,FALSE);
    newatoms->nres=atoms->nres;
    snew(newatoms->resname,atoms->nres);
    snew(newx,atoms->nr);
    if (v) snew(newv,atoms->nr);
    snew(newr,atoms->nr);
    
    for (i=0; i<atoms->nr; i++) {
      resnr = moltypes[tps[i]].res0 +
	(moltypes[tps[i]].i-moltypes[tps[i]].i0) / moltypes[tps[i]].natoms;
      newatoms->resname[resnr] = atoms->resname[atoms->atom[i].resnr];
      newatoms->atomname[moltypes[tps[i]].i] = atoms->atomname[i];
      newatoms->atom[moltypes[tps[i]].i] = atoms->atom[i];
      newatoms->atom[moltypes[tps[i]].i].resnr = resnr;
      copy_rvec(x[i],newx[moltypes[tps[i]].i]);
      if (v) copy_rvec(v[i],newv[moltypes[tps[i]].i]);
      newr[moltypes[tps[i]].i] = r[i];
      moltypes[tps[i]].i++;
    }
    
    /* put them back into the original arrays and throw away temporary arrays */
    sfree(atoms->atomname);
    sfree(atoms->resname);
    sfree(atoms->atom);
    sfree(atoms);
    *atoms_solvt = newatoms;
    for (i=0; i<(*atoms_solvt)->nr; i++) {
      copy_rvec(newx[i],x[i]);
      if (v) copy_rvec(newv[i],v[i]);
      r[i]=newr[i];
    }
    sfree(newx);
    if (v) sfree(newv);
    sfree(newr);
  }
  sfree(moltypes);
}

void rm_res_pbc(t_atoms *atoms, rvec *x, matrix box)
{
  int i,start,n,d,nat;
  rvec xcg;

  start=0;  
  nat=0;
  clear_rvec(xcg);
  for(n=0; n<atoms->nr; n++) {
    if (!is_hydrogen(*(atoms->atomname[n]))) {
      nat++;
      rvec_inc(xcg,x[n]);
    }
    if ( (n+1 == atoms->nr) || 
	 (atoms->atom[n+1].resnr != atoms->atom[n].resnr) ) {
      /* if nat==0 we have only hydrogens in the solvent, 
	 we take last coordinate as cg */
      if (nat==0) {
	nat=1;
	copy_rvec(x[n],xcg);
      }
      svmul(1.0/nat,xcg,xcg);
      for(d=0; d<DIM; d++) {
	while (xcg[d]<0) {
	  for(i=start; i<=n; i++)
	    x[i][d]+=box[d][d];
	  xcg[d]+=box[d][d];
	}
	while (xcg[d]>=box[d][d]) {
	  for(i=start; i<=n; i++)
	    x[i][d]-=box[d][d];
	  xcg[d]-=box[d][d];
	}
      }
      start=n+1;
      nat=0;
      clear_rvec(xcg);
    }
  }
}

char *insert_mols(char *mol_insrt,int nmol_insrt,int ntry,int seed,
		  t_atoms *atoms,rvec **x,
		  real **r,matrix box, real r_distance,real rshell)
{
  static  char    *title_insrt;
  t_atoms atoms_insrt;
  rvec    *x_insrt,*x_n;
  real    *r_insrt;
  matrix  box_insrt;
  int     i,mol,onr;
  real    alfa,beta,gamma;
  rvec    offset_x;
  int     try;
  
  /* read number of atoms of insert molecules */
  get_stx_coordnum(mol_insrt,&atoms_insrt.nr);
  if (atoms_insrt.nr == 0)
    fatal_error(0,"No molecule in %s, please check your input\n",mol_insrt);
  /* allocate memory for atom coordinates of insert molecules */
  snew(x_insrt,atoms_insrt.nr);
  snew(r_insrt,atoms_insrt.nr);
  snew(atoms_insrt.resname,atoms_insrt.nr);
  snew(atoms_insrt.atomname,atoms_insrt.nr);
  snew(atoms_insrt.atom,atoms_insrt.nr);
  atoms_insrt.pdbinfo = NULL;
  snew(x_n,atoms_insrt.nr);
  snew(title_insrt,STRLEN);
  
  /* read residue number, residue names, atomnames, coordinates etc. */
  fprintf(stderr,"Reading molecule configuration \n");
  read_stx_conf(mol_insrt,title_insrt,&atoms_insrt,x_insrt,NULL,box_insrt);
  fprintf(stderr,"%s\nContaining %d atoms in %d residue\n",
	  title_insrt,atoms_insrt.nr,atoms_insrt.nres);
  srenew(atoms_insrt.resname,atoms_insrt.nres);  
    
  /* initialise van der waals arrays of insert molecules */
  mk_vdw(&atoms_insrt, r_insrt, r_distance);

  srenew(atoms->resname,(atoms->nres+nmol_insrt));
  srenew(atoms->atomname,(atoms->nr+atoms_insrt.nr*nmol_insrt));
  srenew(atoms->atom,(atoms->nr+atoms_insrt.nr*nmol_insrt));
  srenew(*x,(atoms->nr+atoms_insrt.nr*nmol_insrt));
  srenew(*r,(atoms->nr+atoms_insrt.nr*nmol_insrt));
  
  try=mol=0;
  while ((mol < nmol_insrt) && (try < ntry*nmol_insrt)) {
    fprintf(stderr,"\rTry %d",try++);
    for (i=0;(i<atoms_insrt.nr);i++) {
      if (atoms_insrt.atom[i].resnr!=0) 
	fatal_error(0,"more then one residue in insert molecules\n"
		    "program terminated\n");
      copy_rvec(x_insrt[i],x_n[i]);
    }
    alfa=2*M_PI*rando(&seed);
    beta=2*M_PI*rando(&seed);
    gamma=2*M_PI*rando(&seed);
    rotate_conf(atoms_insrt.nr,x_n,NULL,alfa,beta,gamma);
    offset_x[XX]=box[XX][XX]*rando(&seed);
    offset_x[YY]=box[YY][YY]*rando(&seed);
    offset_x[ZZ]=box[ZZ][ZZ]*rando(&seed);
    gen_box(0,atoms_insrt.nr,x_n,box_insrt,offset_x,TRUE);
    if (!in_box(box,x_n[0]) || !in_box(box,x_n[atoms_insrt.nr-1]))
      continue;
    onr=atoms->nr;
    
    add_conf(atoms,x,NULL,r,FALSE,box,TRUE,
	     &atoms_insrt,x_n,NULL,r_insrt,FALSE,rshell);
    
    if (atoms->nr==(atoms_insrt.nr+onr)) {
      mol++;
      fprintf(stderr," success (now %d atoms)!",atoms->nr);
    }
  }
  srenew(atoms->resname,  atoms->nres);
  srenew(atoms->atomname, atoms->nr);
  srenew(atoms->atom,     atoms->nr);
  srenew(*x,              atoms->nr);
  srenew(*r,              atoms->nr);
  
  fprintf(stderr,"\n");
  /* print number of molecules added */
  fprintf(stderr,"Added %d molecules (out of %d requested) of %s\n",
	  mol,nmol_insrt,*atoms_insrt.resname[0]); 
    
  return title_insrt;
}

void add_solv(char *fn,t_atoms *atoms,rvec **x,rvec **v,real **r,matrix box,
	      real r_distance,int *atoms_added,int *residues_added,
	      real rshell)
{
  int     i,nmol;
  ivec    n_box;
  char    filename[STRLEN];
  char    title_solvt[STRLEN];
  t_atoms *atoms_solvt;
  rvec    *x_solvt,*v_solvt=NULL;
  real    *r_solvt;
  matrix  box_solvt;
  int     onr,onres;

  strncpy(filename,libfn(fn),STRLEN);
  snew(atoms_solvt,1);
  get_stx_coordnum(filename,&(atoms_solvt->nr)); 
  if (atoms_solvt->nr == 0)
    fatal_error(0,"No solvent in %s, please check your input\n",filename);
  snew(x_solvt,atoms_solvt->nr);
  if (v) snew(v_solvt,atoms_solvt->nr);
  snew(r_solvt,atoms_solvt->nr);
  snew(atoms_solvt->resname,atoms_solvt->nr);
  snew(atoms_solvt->atomname,atoms_solvt->nr);
  snew(atoms_solvt->atom,atoms_solvt->nr);
  atoms_solvt->pdbinfo = NULL;
  fprintf(stderr,"Reading solvent configuration%s\n",
	  v_solvt?" and velocities":"");
  read_stx_conf(filename,title_solvt,atoms_solvt,x_solvt,v_solvt,box_solvt);
  fprintf(stderr,"\"%s\"\n",title_solvt);
  fprintf(stderr,"solvent configuration contains %d atoms in %d residues\n",
	  atoms_solvt->nr,atoms_solvt->nres);
  fprintf(stderr,"\n");
  
  /* apply pbc for solvent configuration for whole molecules */
  rm_res_pbc(atoms_solvt,x_solvt,box_solvt);
  
  /* initialise van der waals arrays of solvent configuration */
  mk_vdw(atoms_solvt,r_solvt,r_distance);
  
  /* calculate the box multiplication factors n_box[0...DIM] */
  nmol=1;
  for (i=0; (i < DIM);i++) {
    n_box[i] = 1;
    while (n_box[i]*box_solvt[i][i] < box[i][i])
      n_box[i]++;
    nmol*=n_box[i];
  }
  fprintf(stderr,"Will generate new solvent configuration of %dx%dx%d boxes\n",
	  n_box[XX],n_box[YY],n_box[ZZ]);
  
  /* realloc atoms_solvt for the new solvent configuration */
  srenew(atoms_solvt->resname,atoms_solvt->nres*nmol);
  srenew(atoms_solvt->atomname,atoms_solvt->nr*nmol);
  srenew(atoms_solvt->atom,atoms_solvt->nr*nmol);
  srenew(x_solvt,atoms_solvt->nr*nmol);
  if (v_solvt) srenew(v_solvt,atoms_solvt->nr*nmol);
  srenew(r_solvt,atoms_solvt->nr*nmol);
  
  /* generate a new solvent configuration */
  genconf(atoms_solvt,x_solvt,v_solvt,r_solvt,box_solvt,n_box);

#ifdef DEBUG
  print_stat(x_solvt,atoms_solvt->nr,box_solvt);
#endif
  
#ifdef DEBUG
  print_stat(x_solvt,atoms_solvt->nr,box_solvt);
#endif
  /* Sort the solvent mixture, not the protein... */
  sort_molecule(&atoms_solvt,x_solvt,v_solvt,r_solvt);
  
  /* add the two configurations */
  onr=atoms->nr;
  onres=atoms->nres;
  add_conf(atoms,x,v,r,TRUE,box,FALSE,
	   atoms_solvt,x_solvt,v_solvt,r_solvt,TRUE,rshell);
  *atoms_added=atoms->nr-onr;
  *residues_added=atoms->nres-onres;
  
  sfree(x_solvt);
  sfree(r_solvt);

  fprintf(stderr,"Generated solvent containing %d atoms in %d residues\n",
	  *atoms_added,*residues_added);
}

char *read_prot(char *confin,t_atoms *atoms,rvec **x,rvec **v,real **r,
		matrix box,real r_distance)
{
  char *title;
  int  natoms;
  
  snew(title,STRLEN);
  get_stx_coordnum(confin,&natoms);

  /* allocate memory for atom coordinates of configuration 1 */
  snew(*x,natoms);
  if (v) snew(*v,natoms);
  snew(*r,natoms);
  init_t_atoms(atoms,natoms,FALSE);

  /* read residue number, residue names, atomnames, coordinates etc. */
  fprintf(stderr,"Reading solute configuration%s\n",v?" and velocities":"");
  read_stx_conf(confin,title,atoms,*x,v?*v:NULL,box);
  fprintf(stderr,"%s\nContaining %d atoms in %d residues\n",
	  title,atoms->nr,atoms->nres);
  
  /* initialise van der waals arrays of configuration 1 */
  mk_vdw(atoms,*r,r_distance);
  
  return title;
}

static void update_top(t_atoms *atoms,matrix box,int NFILE,t_filenm fnm[])
{
#define TEMP_FILENM "temp.top"
  FILE *fpin,*fpout;
  char buf[STRLEN],buf2[STRLEN],*temp,*topinout;
  int  line;
  bool bSystem,bMolecules,bSkip;
  int  i,nsol=0;
  double mtot;
  real vol;
  
  for(i=0; (i<atoms->nres); i++) {
    /* calculate number of SOLvent molecules */
    if ( (strcmp(*atoms->resname[i],"SOL")==0) ||
	 (strcmp(*atoms->resname[i],"WAT")==0) ||
	 (strcmp(*atoms->resname[i],"HOH")==0) )
      nsol++;
  }
  mtot = 0;
  for(i=0; (i<atoms->nr); i++) 
    mtot += get_mass(*atoms->resname[atoms->atom[i].resnr],
		     *atoms->atomname[i]);
  
  vol=det(box);
  
  fprintf(stderr,"Volume                 :  %10g (nm^3)\n",vol);
  fprintf(stderr,"Density                :  %10g (g/l)\n",
	  (mtot*1e24)/(AVOGADRO*vol));
  fprintf(stderr,"Number of SOL molecules:  %5d   \n\n",nsol);
  
  /* open topology file and append sol molecules */
  topinout  = ftp2fn(efTOP,NFILE,fnm);
  if ( ftp2bSet(efTOP,NFILE,fnm) ) {
    fprintf(stderr,"Processing topology\n");
    fpin = ffopen(topinout,"r");
    fpout= ffopen(TEMP_FILENM,"w");
    line=0;
    bSystem = bMolecules = FALSE;
    while (fgets(buf, STRLEN, fpin)) {
      bSkip=FALSE;
      line++;
      strcpy(buf2,buf);
      if ((temp=strchr(buf2,'\n')) != NULL)
	temp[0]='\0';
      ltrim(buf2);
      if (buf2[0]=='[') {
	buf2[0]=' ';
	if ((temp=strchr(buf2,'\n')) != NULL)
	  temp[0]='\0';
	rtrim(buf2);
	if (buf2[strlen(buf2)-1]==']') {
	  buf2[strlen(buf2)-1]='\0';
	  ltrim(buf2);
	  rtrim(buf2);
	  bSystem=(strcasecmp(buf2,"system")==0);
	  bMolecules=(strcasecmp(buf2,"molecules")==0);
	}
      } else if (bSystem && nsol && (buf[0]!=';') ) {
	/* if sol present, append "in water" to system name */
	rtrim(buf2);
	if (buf2[0] && (!strstr(buf2," water")) ) {
	  sprintf(buf,"%s in water\n",buf2);
	  bSystem = FALSE;
	}
      } else if (bMolecules) {
	/* check if this is a line with solvent molecules */
	sscanf(buf,"%s",buf2);
	if (strcmp(buf2,"SOL")==0) {
	  sscanf(buf,"%*s %d",&i);
	  nsol-=i;
	  if (nsol<0) {
	    bSkip=TRUE;
	    nsol+=i;
	  }
	}
      }
      if (bSkip) {
	if ((temp=strchr(buf,'\n')) != NULL)
	  temp[0]='\0';
	fprintf(stdout,"Removing line #%d '%s' from topology file (%s)\n",
		line,buf,topinout);
      } else
	fprintf(fpout,"%s",buf);
    }
    fclose(fpin);
    if ( nsol ) {
      fprintf(stdout,"Adding line for %d solute molecules to "
	      "topology file (%s)\n",nsol,topinout);
      fprintf(fpout,"%-15s %5d\n","SOL",nsol);
    }
    fclose(fpout);
    /* use ffopen to generate backup of topinout */
    fpout=ffopen(topinout,"w");
    fclose(fpout);
    rename(TEMP_FILENM,topinout);
  }
#undef TEMP_FILENM
}

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "Genbox can do one of 3 things:[PAR]",
    
    "1) Generate a box of solvent. Specify -cs and -box. Or specify -cs and",
    "-cp with a structure file with a box, but without atoms.[PAR]",
    
    "2) Solvate a solute configuration, eg. a protein, in a bath of solvent ",
    "molecules. Specify [TT]-cp[tt] (solute) and [TT]-cs[tt] (solvent). ",
    "The box specified in the solute coordinate file ([TT]-cp[tt]) is used,",
    "unless [TT]-box[tt] is set, which also centers the solute.",
    "The program [TT]editconf[tt] has more sophisticated options to change",
    "the box and center the solute.",
    "Solvent molecules are removed from the box where the ",
    "distance between any atom of the solute molecule(s) and any atom of ",
    "the solvent molecule is less than the sum of the VanderWaals radii of ",
    "both atoms. A database ([TT]vdwradii.dat[tt]) of VanderWaals radii is ",
    "read by the program, atoms not in the database are ",
    "assigned a default distance [TT]-vdw[tt].[PAR]",
    
    "3) Insert a number ([TT]-nmol[tt]) of extra molecules ([TT]-ci[tt]) ",
    "at random positions.",
    "The program iterates until [TT]nmol[tt] molecules",
    "have been inserted in the box. To test whether an insertion is ",
    "successful the same VanderWaals criterium is used as for removal of ",
    "solvent molecules. When no appropriately ",
    "sized holes (holes that can hold an extra molecule) are available the ",
    "program tries for [TT]-nmol[tt] * [TT]-try[tt] times before giving up. ",
    "Increase -try if you have several small holes to fill.[PAR]",
    
    "The default solvent is Simple Point Charge water (SPC), with coordinates ",
    "from [TT]$GMXLIB/spc216.gro[tt]. Other",
    "solvents are also supported, as well as mixed solvents. The",
    "only restriction to solvent types is that a solvent molecule consists",
    "of exactly one residue. The residue information in the coordinate",
    "files is used, and should therefore be more or less consistent.",
    "In practice this means that two subsequent solvent molecules in the ",
    "solvent coordinate file should have different residue number.",
    "The box of solute is built by stacking the coordinates read from",
    "the coordinate file. This means that these coordinates should be ",
    "equlibrated in periodic boundary conditions to ensure a good",
    "alignment of molecules on the stacking interfaces.[PAR]",
    
    "The program can optionally rotate the solute molecule to align the",
    "longest molecule axis along a box edge. This way the amount of solvent",
    "molecules necessary is reduced.",
    "It should be kept in mind that this only works for",
    "short simulations, as eg. an alpha-helical peptide in solution can ",
    "rotate over 90 degrees, within 500 ps. In general it is therefore ",
    "better to make a more or less cubic box.[PAR]",
    
    "Setting -shell larger than zero will place a layer of water of",
    "the specified thickness (nm) around the solute. Hint: it is a good",
    "idea to put the protein in the center of a box first (using editconf).",
    "[PAR]",
    
    "Finally, genbox will optionally remove lines from your topology file in ",
    "which a number of solvent molecules is already added, and adds a ",
    "line with the total number of solvent molecules in your coordinate file."
  };

  static char *bugs[] = {
    "Molecules must be whole in the initial configurations.",
    "At the moment -ci only works when inserting one molecule."
  };
  
  /* parameter data */
  bool bSol,bProt,bBox;
  char *conf_prot,*confout;
  int  bInsert;
  real *r;
  char *title_ins;

  /* protein configuration data */
  char    *title=NULL;
  t_atoms atoms;
  rvec    *x,*v=NULL;
  matrix  box;
  
  /* other data types */
  int  atoms_added,residues_added;
  
  t_filenm fnm[] = {
    { efSTX, "-cp", "protein", ffOPTRD },
    { efSTX, "-cs", "spc216",  ffLIBOPTRD},
    { efSTX, "-ci", "insert",  ffOPTRD},
    { efSTO, NULL,  NULL,      ffWRITE},
    { efTOP, NULL,  NULL,      ffOPTRW},
  };
#define NFILE asize(fnm)
  
  static int nmol_ins=0,nmol_try=10,seed=1997;
  static real r_distance=0.105,r_shell=0;
  static rvec new_box={0.0,0.0,0.0};
  static bool bReadV=FALSE;
  t_pargs pa[] = {
    { "-box",    FALSE, etRVEC, {new_box},   
      "box size" },
    { "-nmol",   FALSE, etINT , {&nmol_ins},  
      "no of extra molecules to insert" },
    { "-try",    FALSE, etINT , {&nmol_try},  
      "try inserting -nmol*-try times" },
    { "-seed",   FALSE, etINT , {&seed},      
      "random generator seed"},
    { "-vdwd",   FALSE, etREAL, {&r_distance},
      "default vdwaals distance"},
    { "-shell",  FALSE, etREAL, {&r_shell},
      "thickness of optional water layer around solute" },
    { "-vel",    FALSE, etBOOL, {&bReadV},
      "HIDDENkeep velocities from input solute and solvent" }
  };

  CopyRight(stderr,argv[0]);
  parse_common_args(&argc,argv, PCA_BE_NICE,NFILE,fnm,asize(pa),pa,
		    asize(desc),desc,asize(bugs),bugs);
  
  bInsert   = opt2bSet("-ci",NFILE,fnm) && (nmol_ins > 0);
  bSol      = opt2bSet("-cs",NFILE,fnm);
  bProt     = opt2bSet("-cp",NFILE,fnm);
  bBox      = opt2parg_bSet("-box",asize(pa),pa);
  
  /* check input */
  if (bInsert && nmol_ins<=0)
    fatal_error(0,"When specifying inserted molecules (-ci), "
		"-nmol must be larger than 0");
  if (!bProt && !bBox)
    fatal_error(0,"When no solute (-cp) is specified, "
		"a box size (-box) must be specified");
  
  if (bProt) {
    /*generate a solute configuration */
    conf_prot = opt2fn("-cp",NFILE,fnm);
    title = read_prot(conf_prot,&atoms,&x,bReadV?&v:NULL,&r,box,r_distance);
    if (bReadV && !v)
      fprintf(stderr,"Note: no velocities found\n");
    if (atoms.nr == 0) {
      fprintf(stderr,"Note: no atoms in %s\n",conf_prot);
      bProt = FALSE;
    }
  } 
  if (!bProt) {
    atoms.nr=0;
    atoms.nres=0;
    atoms.resname=NULL;
    atoms.atomname=NULL;
    atoms.atom=NULL;
    atoms.pdbinfo=NULL;
    x=NULL;
    r=NULL;
  }
  if (bBox) {
    clear_mat(box);
    box[XX][XX]=new_box[XX];
    box[YY][YY]=new_box[YY];
    box[ZZ][ZZ]=new_box[ZZ];
  }
  if (det(box) == 0) 
    fatal_error(0,"Undefined solute box.\nCreate one with editconf "
		"or give explicit -box command line option");
  
  init_pbc(box);
  
  /* add nmol_ins molecules of atoms_ins 
     in random orientation at random place */
  if (bInsert) 
    title_ins = insert_mols(opt2fn("-ci",NFILE,fnm),nmol_ins,nmol_try,seed,
			    &atoms,&x,&r,box,r_distance,r_shell);
  else
    title_ins = strdup("Generated by genbox");
  
  /* add solvent */
  if (bSol)
    add_solv(opt2fn("-cs",NFILE,fnm),&atoms,&x,v?&v:NULL,&r,box,
	     r_distance,&atoms_added,&residues_added,r_shell);
	     
  /* write new configuration 1 to file confout */
  confout = ftp2fn(efSTO,NFILE,fnm);
  fprintf(stderr,"Writing generated configuration to %s\n",confout);
  if (bProt) {
    write_sto_conf(confout,title,&atoms,x,v,box);
    /* print box sizes and box type to stderr */
    fprintf(stderr,"%s\n",title);  
  } else 
    write_sto_conf(confout,title_ins,&atoms,x,v,box);
  
  /* print size of generated configuration */
  fprintf(stderr,"\nOutput configuration contains %d atoms in %d residues\n",
	  atoms.nr,atoms.nres);
  update_top(&atoms,box,NFILE,fnm);
	  
  thanx(stderr);
  
  return 0;
}



