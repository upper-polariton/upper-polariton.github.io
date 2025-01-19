#
#  $Author: wab $
#  $Date: 1999/08/13 07:33:40 $
#  $Locker:  $
#  $Revision: 1.104 $
#  $Source: /c/qcg/cvs/psh/GAMESS-UK/m4/Makefile.in,v $
#  $State: Exp $
#
#   makefile for GAMESS Version 6.0 direct from M4 
#
#startdoc
#
#   issue ./configure and then enter the m4 processing options required.
#
# machine keywords:  rs6000   IBM RS6K (workstation-power2)
#                    rs6000p1 IBM RS6K (workstation-power1)
#                    rs6000p3 IBM RS6K (workstation-power3)
#                    rs6000ppc IBM RS6K (workstation-powerPC)
#                    sp2      IBM SP2
#                    sgi      SGI (MIPS R4400 processor - mips2)
#                    sgitfp   SGI (MIPS R8000 processor)
#                    sgitfp10kSGI (MIPS R10000 processor)
#                    sgitfp5k SGI (MIPS R5000 processor)
#                    axpev5   DEC (alpha AXP EV5 processor)
#                    hp700    HP 9000 7xx
#                    hp800    HP 9000 Model C160, C200 (with blas)
#                    exemplar Hp/Convex Exemplar-S
#                    t3d      Cray T3D
#                    t3e      Cray T3E
#                    hitachi  Hitachi SR2201
#                    c90      Cray YMP-C90
#                    j90      Cray YMP-J90
#                    nec      NEC SX-4 (development version)
#                    vpp300   Fujitsu VPP300 (development version)
#                    ipsc     Intel iPSC/860
#                    linux    Intel 486 PC running Linux
#                    linuxppc Macintosh G3 running linux (Bigendian/g77)
#                    win95    Windows95 source code generation
#                    sspsol   SUN SuperSPARC / solaris
#                    sultra   SUN UltraSPARC / solaris
#
# build keywords:    debug
#                    profile
#                    protect-source   chmod o-w on fortran/C files
#                    ccpdft   include CCP1 DFT code
#                             [requires code in ../dft]
#                    drf      include developmental DRF code
#                             [requires code in ../drf]
#                    mopac    include mopac code
#                             [requires code in ../mopac]
#                    zora     include developmental ZORA code
#                    mrdci    include developmental MRDCI code
#                    newscf   include developmental SCF code
#                             [requires code in ../newscf]
#                    timings  Detailed timing breakdown
#
# parallel keywords: tcgmsg   parallel code (no GA)
#                    mpi      parallel code (no GA) using MPI
#                    ga       link GA tools and parallel direct MP2
#                    secd     as ga but including parallel hessian
#                    gascf    SCF-only with GA (rs6k,hp700 only)
#                    peigs    PeIGS Parallel diagonaliser (T3D,T3E,SP2 only)
#
# keywords determining the system size limits:
#
#                    large    2048 basis functions
#                    qmmm     3000 nuclear centres
#
# Additional options
#
#                    nbo      Activate NBO code
#                    zora     Activate ZORA code
#                    mrdci    Activate new MRDCI code
#                    timings  Detailed timing breakdown
#                    f90      Use F90 features
#
#enddoc
#
#   WARNING - make clean also deletes fortran and C source 
#             leaving only .m files
#
#  Most processing is controlled by the machine file in directory 
#  ../machines, supplemented by extra M4 control flags  (see PAR_M4_OPTS
#  PEIGS_M4_OPTS, DFT_M4_OPTS, DRF_M4_OPTS, MOPAC_M4_OPTS, NEWSCF_M4_OPTS
#  settings below) which  are passed to m4 on the command line
#
# ----------------------- PATHNAMES ------------------------------
#
# target names for executables
#
SCFEXE = ../bin/gamess_scf
EXE=../bin/gamess
#
# ----------------------- DEFAULT MAKE TARGET ---------------------
#
GAMESS_TARGET=lib
#
# ----------------------- DEFAULT DEFINITIONS ---------------------
#
.SUFFIXES:
.SUFFIXES:	.m .o .s .c .f
SHELL=/bin/sh
M4 = m4
# problems with M4 on DEC-alpha OSF may require
# m4 on remote workstation
# M4 = rsh -l psh tcsi /usr/5bin/m4
# M4 = /home/wab/columbus/m4-1.4/bin/m4
#
FC = f77
CC = cc
AS = as
LD = f77
CPP = cpp
FFLAGSV = -c
FFLAGSS = -c
FFLAGSN = -c
FFLAGS_CPP =
CFLAGS = -c
GMAKE  = gmake
ARCHIVE  =  ar rcv
RANLIB   =  ar ts
# double->single precision processing
SNGL=
# set to make_ga if GA tool need to be built
MAKE_GA=
#
# ---------------------------- F90 OPTIONS ---------------------------
#
# Use the f90 keyword to switch on the use of F90 features
#
#
# ---------------------- PARALLEL OPTIONS ------------------------------------
PAR_M4_OPTS=parallel,mpi,static
PAR_LIB=
# ----------------------- CCPDFT OPTIONS ------------------------------------
#
# ----------------------- DRF OPTIONS --------------------------------------
# ----------------------- MOPAC OPTIONS ------------------------------------
#
# ----------------------- ChemShell OPTIONS ------------------------------------
#
#
# ----------------------- NEWSCF OPTIONS ------------------------------------
#
#
# ----------------------- CHARMM INTERFACE OPTIONS -------------------------
#
# Add qmmm here to restrict memory use for large number of qm centres
# Add charmm to activate the gamess-uk/charmm interface
#
#
# ----------------------- SYSTEM SIZE OPTIONS --------------------------------
# defaults
#
#
#  TEST  
#
##MAXAT	=	10000
##MAXORB	=	500
MAXAT	=	5000
MAXORB	=	8192

MXSHEL	=	6000
MXPRIM	=	20000
MXGRPS	=	5600
MXGAUS	=	56000
# modifications requested by configure options
#MAXORB	=	2048
#MXSHEL	=	1024
#MXPRIM	=	4096
# modifications requested by configure options
#
# -------------------------- PEIGS definitions  ------------------
# ------------------ DEVELOPMENT OPTIONS --------------------------
DEV_M4_OPTS=$(D1)$(D2)$(D3)$(D4)
#
# ------------------ MACHINE-SPECIFIC DEFINITIONS -----------------
#
#
#
#
#
#
#
#
#
#
#
# ======================== linux PC 486 / Macintosh PPC ===================
#
FC = /work2/groenhof/Programs/lam-7.0.2/bin/mpif77
MPIINC=-I/work2/groenhof/Programs/lam-7.0.2/include
MPILIB=
LAMLIBS=-ltstdio -ltrillium -largs -lt
# lam61 builds without  -fno-second-underscore
FF2CFLAG= -w
CF2CFLAG=
#
FFLAGSV = -c -O -Wno-globals  -fno-globals -malign-double -fno-f90 -fugly-complex ${FF2CFLAG} ${MPIINC}
FFLAGSS = -c    -Wno-globals  -fno-globals -malign-double -fno-f90 -fugly-complex ${FF2CFLAG} ${MPIINC}
FFLAGSN = -c -O -Wno-globals -fno-globals -malign-double -fno-f90 -fugly-complex ${FF2CFLAG} ${MPIINC}
CFLAGS  = -c -O -DLINUX -DLINUX ${CF2CFLAG} 
LDFLAGS = ${MPILIB}  ${FF2CFLAG} ${CF2CFLAG}
LIB= ${LAMLIBS}
LD = /work2/groenhof/Programs/lam-7.0.2/bin/mpif77
CC = /work2/groenhof/Programs/lam-7.0.2/bin/mpicc
RANLIB=ranlib
MACH=linux.m
NAG=nag.o
BLAS=iblas.o 
EXTRA=dblas.o
MXM=mxm_noblas3.o
LIB=
CPP=/lib/cpp
#
#
#
#
#
#
#
#
# ======================= End of machine options =============================
#
# combine options together
#
M4OPTS	= 	-DM4_OPTIONS=$(PAR_M4_OPTS)$(MACH_M4_OPTS)$(PEIGS_M4_OPTS)$(DFT_M4_OPTS)$(DRF_M4_OPTS)$(MOPAC_M4_OPTS)$(CHM_M4_OPTS)$(CHEMSH_M4_OPTS)$(NEWSCF_M4_OPTS)$(DEV_M4_OPTS)$(F90_M4_OPTS) \
		-DMAXAT=$(MAXAT)   -DMAXORB=$(MAXORB) \
		-DMXSHEL=$(MXSHEL) -DMXPRIM=$(MXPRIM) \
		-DMXGRPS=$(MXGRPS) -DMXGAUS=$(MXGAUS)
LDLIBS	= 	$(DFT_LIB) $(DRF_LIB) $(MOPAC_LIB) $(NEWSCF_LIB) $(PEIGS_LIB) $(PAR_LIB) $(LIB)
MISC	=	$(BLAS) $(MXM) $(NAG) $(EXTRA) $(PEIGS_OBJ)
#
#
#
#--------------------------- library names  ----------------------------------
#
SCFLIB = libgamess.a
#
#--------------------------- scf-only target ----------------------------------
#
all:	notes.target $(MAKE_PEIGS) $(MAKE_DFT) $(MAKE_NEWSCF) $(MAKE_DRF) $(MAKE_MOPAC) $(MAKE_GA) $(GAMESS_TARGET)







#
# for charmm, chemshell work, additionally remove optef 
# delete common/small from gamess (scratch in util1 - but check)
#
SCFOBJ=gamess.o anala.o analb.o analc.o analf.o analg.o basis.o  dft.o   \
	drv1e.o   drv2e.o   drv80.o   guess.o   input.o   intega.o   integb.o   integc.o \
	integs.o   machscf.o master.o   model.o  omits.o   optef.o   \
	optim.o gmx.o ver_c.o scf.o server.o   util1.o   util2.o   util3.o morokuma.o nbo.o \
	parallel.o  $(MISC) mass.o c.o zora.o $(PAR_EXTRA) $(MODOBJ)
#
DRF=    drfctl.o drfsub.o neqpot.o radius.o drfsolv.o drfscf.o surfsub.o \
	hondext.o dppop.o fld.o missing.o mcsub.o drfextra.o rfin.o mscon.o \
        drfgrad.o extgrad.o drfder.o
#
scfobj:	$(SCFOBJ)
GOBJ = $(SCFOBJ)
#
#
scfexe:	$(SCFEXE)
#
$(SCFEXE):	$(GOBJ) $(MAKE_DFT) $(MAKE_DRF) $(MAKE_MOPAC)
	$(LD) $(LDFLAGS) -o $(SCFEXE) $(GOBJ) $(ASOBJ) $(LDLIBS) 
#


#
OBJF=   gamess.o mainci.o anala.o analb.o analc.o anald.o anale.o \
        analf.o analg.o basis.o cphf.o derdrv.o dft.o drv1e.o \
        drv2e.o drv80.o drvmp.o guess.o index4.o inpci.o input.o intega.o \
        integb.o integc.o integd.o intege.o integs.o machci.o machscf.o \
        master.o model.o mp2a.o mp2b.o mp2c.o mp2d.o mp3.o \
        omit.o optef.o optim.o gmx.o ver_c.o scf.o server.o tran4.o util1.o util2.o util3.o \
        util4.o util5.o util6.o util7.o util8.o \
        gff.o tdaf.o sec1e.o sec2e.o  secmp2.o mcscfa.o mcscfb.o mcscfc.o \
        mrdci1.o mrdci2.o mrdci3.o mrdci4.o mrdci5.o mrdci6.o mrdci7.o  fullci.o  \
	newmrd1.o $(MODOBJ) \
        dircta.o dirctb.o dirctc.o direct.o casa.o casb.o   \
        ccsd.o dirrpa.o mclr.o nvccsd.o rpa.o tsort.o \
        morokuma.o nbo.o parallel.o  mass.o zora.o lapack.o $(MISC) 

OBJC=c.o tsortc.o

TOBJ=$(OBJF) $(OBJC)
#
exe:	$(EXE)

$(EXE):	$(TOBJ)  $(MAKE_DFT) $(MAKE_MOPAC) $(MAKE_DRF) $(MAKE_NEWSCF)
	$(LD) -o $(EXE) $(LDFLAGS) $(TOBJ) $(ASOBJ) $(LDLIBS) 
	@echo ----- made $(EXE) -----

#
lib:		$(SCFLIB)
$(SCFLIB):	$(TOBJ)
		$(ARCHIVE) $(SCFLIB) *.o
		$(RANLIB) $(SCFLIB)
		@echo ----- made $(SCFLIB) -----

#
# ======================== Rules to build object code =========================
#
.m.o:
	rm -f $*.f
	cat ../machines/$(MACH) ../utilities/gener.m $*.m | $(M4) $(M4OPTS) $(SNGL) >> $*.f
#
	$(FC) $(FFLAGSV) $*.f
#
#
#  default rules
#
.f.o:
	$(FC) $(FFLAGSV) $*.f
#
#
#  explicit rules so that .m->.o conversion goes by .c (not .f)
#
c.o:	c.c
	$(CC) $(CFLAGS) c.c
#
tsortc.o:	tsortc.c
	$(CC) $(CFLAGS) tsortc.c
#
# ver_c
ver_c.o:	ver_c.f
		$(FC) -O2 -fno-second-underscore ver_c.f

#
#  parallel.m is the only source file that needs CPP processing
#  for GA tools etc
#
parallel.o:	parallel.m
	rm -f parallel.F parallel.f
	cat ../machines/$(MACH) ../utilities/gener.m parallel.m | $(M4) $(M4OPTS) $(SNGL) >> parallel.f
	$(FC) $(FFLAGSS) parallel.f
#
#
#
#
#
#  ======================  Exceptions for linux ==========================
#
mrdci5.o:	mrdci5.f
	$(FC) $(FFLAGSS) $*.f
#
#
#
#
#
#


#
# ======================== Rules to build source code =========================
#
.m.f:	
	rm -f $*.f
	cat ../machines/$(MACH) ../utilities/gener.m $*.m | $(M4) $(M4OPTS) $(SNGL) >> $*.f
#
# special rule to include CPP
#
parallel.f:	parallel.m
	rm -f parallel.F parallel.f
	cat ../machines/$(MACH) ../utilities/gener.m parallel.m | $(M4) $(M4OPTS) $(SNGL) >> parallel.f
#
.m.c:
	rm -f $*.c
	cat ../machines/$(MACH) ../utilities/gener.m $*.m | $(M4) $(M4OPTS) >> $*.c
.m.s:
	cat ../machines/$(MACH) ../utilities/gener.m $*.m | $(M4) $(M4OPTS)> $*.s
#
#
# ============================= Common Block Dependencies  ===========================
#
# After adding new code, this list can be regenerated using the 
# makedeps script in ../utilities
#
anala.o:		common/atmol3
anala.o:		common/cigrad
anala.o:		common/cndx41
anala.o:		common/discc
anala.o:		common/dm
anala.o:		common/dump3
anala.o:		common/gaanal
anala.o:		common/groups
anala.o:		common/harmon
anala.o:		common/infoa
anala.o:		common/iofile
anala.o:		common/limy
anala.o:		common/machin
anala.o:		common/mapper
anala.o:		common/nodeio
anala.o:		common/nshel
anala.o:		common/parcntl
anala.o:		common/prints
anala.o:		common/restar
anala.o:		common/restri
anala.o:		common/restrj
anala.o:		common/runlab
anala.o:		common/scfopt
anala.o:		common/scfwfn
anala.o:		common/scra7
anala.o:		common/sector
anala.o:		common/segm
anala.o:		common/sizes
anala.o:		common/statis
anala.o:		common/timez
anala.o:		common/tran
anala.o:		common/unocas
anala.o:		common/xfield
analb.o:		common/atmol3
analb.o:		common/datplt
analb.o:		common/datprp
analb.o:		common/discc
analb.o:		common/dump3
analb.o:		common/infoa
analb.o:		common/iofile
analb.o:		common/limy
analb.o:		common/linkan
analb.o:		common/machin
analb.o:		common/mapper
analb.o:		common/nshel
analb.o:		common/prints
analb.o:		common/restar
analb.o:		common/restrj
analb.o:		common/root
analb.o:		common/runlab
analb.o:		common/scra7
analb.o:		common/sector
analb.o:		common/sizes
analb.o:		common/timez
analb.o:		common/tran
analb.o:		common/vdwrad
analc.o:		common/atmol3
analc.o:		common/coval
analc.o:		common/direc
analc.o:		common/discc
analc.o:		common/hermit
analc.o:		common/infoa
analc.o:		common/iofile
analc.o:		common/limy
analc.o:		common/machin
analc.o:		common/mapper
analc.o:		common/nshel
analc.o:		common/restar
analc.o:		common/restrj
analc.o:		common/runlab
analc.o:		common/scfwfn
analc.o:		common/scra7
analc.o:		common/segm
analc.o:		common/sizes
analc.o:		common/symtry
analc.o:		common/timez
analc.o:		common/tran
analc.o:		common/transf
analc.o:		common/unocas
analc.o:		common/wermit
analc.o:		common/work
anald.o:		common/atmblk
anald.o:		common/cndx40
anald.o:		common/cndx41
anald.o:		common/common
anald.o:		common/ijlab
anald.o:		common/infoa
anald.o:		common/iofile
anald.o:		common/machin
anald.o:		common/mapper
anald.o:		common/nshel
anald.o:		common/prnprn
anald.o:		common/restar
anald.o:		common/restri
anald.o:		common/restrj
anald.o:		common/runlab
anald.o:		common/segm
anald.o:		common/sizes
anald.o:		common/statis
anald.o:		common/symtry
anald.o:		common/tdhf
anald.o:		common/tdhfx
anald.o:		common/timez
anald.o:		common/vectrn
anale.o:		common/cndx41
anale.o:		common/common
anale.o:		common/infoa
anale.o:		common/iofile
anale.o:		common/mapper
anale.o:		common/prnprn
anale.o:		common/runlab
anale.o:		common/sizes
analf.o:		common/angxyz
analf.o:		common/atmol3
analf.o:		common/cslosc
analf.o:		common/direc
analf.o:		common/dnfnw
analf.o:		common/dump3
analf.o:		common/field
analf.o:		common/g80nb
analf.o:		common/infoa
analf.o:		common/iofile
analf.o:		common/limy
analf.o:		common/machin
analf.o:		common/mapper
analf.o:		common/modj
analf.o:		common/nshel
analf.o:		common/parallel
analf.o:		common/phycon
analf.o:		common/prints
analf.o:		common/psscrf
analf.o:		common/radxxx
analf.o:		common/radyyy
analf.o:		common/restar
analf.o:		common/root
analf.o:		common/runlab
analf.o:		common/scfopt
analf.o:		common/scfwfn
analf.o:		common/scra7
analf.o:		common/segm
analf.o:		common/sizes
analf.o:		common/statis
analf.o:		common/symtry
analf.o:		common/timez
analf.o:		common/tran
analf.o:		common/vdwrad
analg.o:		common/atmol3
analg.o:		common/cndx41
analg.o:		common/datprp
analg.o:		common/discc
analg.o:		common/dm
analg.o:		common/dump3
analg.o:		common/gvalue
analg.o:		common/hermit
analg.o:		common/infoa
analg.o:		common/iofile
analg.o:		common/limy
analg.o:		common/mapper
analg.o:		common/natorb
analg.o:		common/nshel
analg.o:		common/restar
analg.o:		common/restrj
analg.o:		common/runlab
analg.o:		common/scfwfn
analg.o:		common/scra7
analg.o:		common/segm
analg.o:		common/sizes
analg.o:		common/timez
analg.o:		common/tran
analg.o:		common/unocas
analg.o:		common/wermit
basis.o:		common/atmol3
basis.o:		common/datbas
basis.o:		common/fsymas
basis.o:		common/harmon
basis.o:		common/infoa
basis.o:		common/infob
basis.o:		common/iofile
basis.o:		common/machin
basis.o:		common/nshel
basis.o:		common/prints
basis.o:		common/restar
basis.o:		common/restrj
basis.o:		common/runlab
basis.o:		common/scra7
basis.o:		common/sizes
basis.o:		common/symtry
basis.o:		common/tol
basis.o:		common/transf
basis.o:		common/work
basis.o:		common/zorac
casa.o:		common/avstat
casa.o:		common/caspar
casa.o:		common/ciconv
casa.o:		common/cndx40
casa.o:		common/ctrl
casa.o:		common/discc
casa.o:		common/dm
casa.o:		common/dump3
casa.o:		common/exc
casa.o:		common/filel
casa.o:		common/files
casa.o:		common/finish
casa.o:		common/gjs
casa.o:		common/harmon
casa.o:		common/iofile
casa.o:		common/machin
casa.o:		common/mapper
casa.o:		common/popnos
casa.o:		common/prints
casa.o:		common/qice
casa.o:		common/restar
casa.o:		common/runlab
casa.o:		common/segm
casa.o:		common/simul
casa.o:		common/sizes
casa.o:		common/stak
casa.o:		common/trntim
casb.o:		common/ciconv
casb.o:		common/cndx40
casb.o:		common/ctrl
casb.o:		common/dm
casb.o:		common/dump3
casb.o:		common/exc
casb.o:		common/filel
casb.o:		common/files
casb.o:		common/finish
casb.o:		common/gjs
casb.o:		common/harmon
casb.o:		common/infoa
casb.o:		common/iofile
casb.o:		common/machin
casb.o:		common/mapper
casb.o:		common/nshel
casb.o:		common/popnos
casb.o:		common/prints
casb.o:		common/qice
casb.o:		common/restar
casb.o:		common/runlab
casb.o:		common/scra7
casb.o:		common/simul
casb.o:		common/sizes
casb.o:		common/stak
casb.o:		common/symtry
casb.o:		common/timez
ccsd.o:		common/direc
ccsd.o:		common/discc
ccsd.o:		common/filel
ccsd.o:		common/iofile
ccsd.o:		common/machin
ccsd.o:		common/restar
ccsd.o:		common/runlab
ccsd.o:		common/savem
ccsd.o:		common/segm
ccsd.o:		common/sizes
ccsd.o:		common/statis
ccsd.o:		common/work
cphf.o:		common/atmblk
cphf.o:		common/cigrad
cphf.o:		common/cndx40
cphf.o:		common/cndx41
cphf.o:		common/common
cphf.o:		common/ghfblk
cphf.o:		common/infoa
cphf.o:		common/iofile
cphf.o:		common/mapper
cphf.o:		common/nshel
cphf.o:		common/prnprn
cphf.o:		common/sizes
cphf.o:		common/statis
cphf.o:		common/symtry
cphf.o:		common/tdhf
cphf.o:		common/tdhfx
cphf.o:		common/timez
dblas.o:		common/iofile
derdrv.o:		common/atmblk
derdrv.o:		common/cigrad
derdrv.o:		common/cndx40
derdrv.o:		common/cndx41
derdrv.o:		common/common
derdrv.o:		common/cslosc
derdrv.o:		common/dump3
derdrv.o:		common/funct
derdrv.o:		common/ghfblk
derdrv.o:		common/grad2
derdrv.o:		common/ijlab
derdrv.o:		common/infoa
derdrv.o:		common/iofile
derdrv.o:		common/machin
derdrv.o:		common/mapper
derdrv.o:		common/nshel
derdrv.o:		common/prnprn
derdrv.o:		common/restar
derdrv.o:		common/restrj
derdrv.o:		common/runlab
derdrv.o:		common/segm
derdrv.o:		common/sizes
derdrv.o:		common/statis
derdrv.o:		common/symtry
derdrv.o:		common/tdhf
derdrv.o:		common/tdhfx
derdrv.o:		common/timez
derdrv.o:		common/vectrn
dft.o:		common/angxyz
dft.o:		common/dnfnw
dft.o:		common/field
dft.o:		common/infoa
dft.o:		common/iofile
dft.o:		common/nshel
dft.o:		common/phycon
dft.o:		common/radxxx
dft.o:		common/scfwfn
dft.o:		common/sizes
dircta.o:		common/auxh
dircta.o:		common/ccepa
dircta.o:		common/ccntl
dircta.o:		common/cdcryi
dircta.o:		common/cdcryz
dircta.o:		common/cic
dircta.o:		common/cjdavid
dircta.o:		common/cntrl
dircta.o:		common/comrjb
dircta.o:		common/corctl
dircta.o:		common/diactl
dircta.o:		common/dir_erg
dircta.o:		common/direc
dircta.o:		common/discc
dircta.o:		common/disktl
dircta.o:		common/file12
dircta.o:		common/fpinfo
dircta.o:		common/helpr
dircta.o:		common/hold
dircta.o:		common/icon
dircta.o:		common/infoa
dircta.o:		common/iofile
dircta.o:		common/machin
dircta.o:		common/mapp
dircta.o:		common/modeci
dircta.o:		common/mr_cepctl
dircta.o:		common/mr_comci
dircta.o:		common/mr_mp2
dircta.o:		common/natorb
dircta.o:		common/orb
dircta.o:		common/presrt
dircta.o:		common/prints
dircta.o:		common/prnprn
dircta.o:		common/restar
dircta.o:		common/restri
dircta.o:		common/scfwfn
dircta.o:		common/segm
dircta.o:		common/sizes
dircta.o:		common/statis
dircta.o:		common/symchk
dircta.o:		common/symci
dircta.o:		common/table
dircta.o:		common/timanb
dircta.o:		common/timez
dircta.o:		common/trial
dircta.o:		common/work
dirctb.o:		../drf/comdrf/dafil
dirctb.o:		../drf/comdrf/darw
dirctb.o:		../drf/comdrf/drfpar
dirctb.o:		../drf/comdrf/sizesrf
dirctb.o:		common/auxh
dirctb.o:		common/ccepa
dirctb.o:		common/ccntl
dirctb.o:		common/cdcryi
dirctb.o:		common/cdcryz
dirctb.o:		common/cic
dirctb.o:		common/cjdavid
dirctb.o:		common/cntrl
dirctb.o:		common/comrjb
dirctb.o:		common/corctl
dirctb.o:		common/diactl
dirctb.o:		common/dir_erg
dirctb.o:		common/discc
dirctb.o:		common/disktl
dirctb.o:		common/drfopt
dirctb.o:		common/dump3
dirctb.o:		common/file12
dirctb.o:		common/helpr
dirctb.o:		common/hold
dirctb.o:		common/iofile
dirctb.o:		common/machin
dirctb.o:		common/mapp
dirctb.o:		common/modeci
dirctb.o:		common/mr_mp2
dirctb.o:		common/natorb
dirctb.o:		common/orb
dirctb.o:		common/outctl
dirctb.o:		common/parallel
dirctb.o:		common/prints
dirctb.o:		common/prnprn
dirctb.o:		common/restar
dirctb.o:		common/sizes
dirctb.o:		common/symci
dirctb.o:		common/table
dirctb.o:		common/timanb
dirctb.o:		common/timez
dirctb.o:		common/trial
dirctc.o:		common/auxh
dirctc.o:		common/auxh
dirctc.o:		common/ccepa
dirctc.o:		common/ccepa
dirctc.o:		common/ccntl
dirctc.o:		common/ccntl
dirctc.o:		common/cdcryi
dirctc.o:		common/cdcryz
dirctc.o:		common/cntrl
dirctc.o:		common/corctl
dirctc.o:		common/diactl
dirctc.o:		common/dir_erg
dirctc.o:		common/discc
dirctc.o:		common/disktl
dirctc.o:		common/disktl
dirctc.o:		common/file12
dirctc.o:		common/helpr
dirctc.o:		common/iofile
dirctc.o:		common/machin
dirctc.o:		common/machin
dirctc.o:		common/mapp
dirctc.o:		common/mr_cepctl
dirctc.o:		common/mr_comci
dirctc.o:		common/mr_mp2
dirctc.o:		common/mr_mp2con
dirctc.o:		common/mr_mp2l
dirctc.o:		common/mr_shifts
dirctc.o:		common/orb
dirctc.o:		common/presrt
dirctc.o:		common/prints
dirctc.o:		common/prnprn
dirctc.o:		common/sizes
dirctc.o:		common/symci
dirctc.o:		common/symci
dirctc.o:		common/table
dirctc.o:		common/trial
dirctc.o:		common/work
direct.o:		common/atmol3
direct.o:		common/cndx40
direct.o:		common/cndx41
direct.o:		common/dump3
direct.o:		common/funct
direct.o:		common/harmon
direct.o:		common/infoa
direct.o:		common/iofile
direct.o:		common/machin
direct.o:		common/mapper
direct.o:		common/nshel
direct.o:		common/parallel
direct.o:		common/prints
direct.o:		common/restar
direct.o:		common/runlab
direct.o:		common/scra7
direct.o:		common/sizes
direct.o:		common/statis
direct.o:		common/symtry
direct.o:		common/tran
direct.o:		common/wrtd
dirrpa.o:		common/iofile
dirrpa.o:		common/machin
dirrpa.o:		common/nshel
dirrpa.o:		common/parallel
dirrpa.o:		common/polars
dirrpa.o:		common/prints
dirrpa.o:		common/rpfile
dirrpa.o:		common/runlab
dirrpa.o:		common/sizes
dirrpa.o:		common/statis
dirrpa.o:		common/symtry
drv1e.o:		../drf/comdrf/drfbem
drv1e.o:		../drf/comdrf/drfpar
drv1e.o:		../drf/comdrf/rfene
drv1e.o:		../drf/comdrf/rfgrad
drv1e.o:		../drf/comdrf/runpar
drv1e.o:		../drf/comdrf/sizesrf
drv1e.o:		common/atmblk
drv1e.o:		common/blur
drv1e.o:		common/ccpdft.hf77
drv1e.o:		common/chgcc
drv1e.o:		common/chmgms
drv1e.o:		common/cigrad
drv1e.o:		common/cndx40
drv1e.o:		common/cndx41
drv1e.o:		common/common
drv1e.o:		common/cslosc
drv1e.o:		common/dm
drv1e.o:		common/drfopt
drv1e.o:		common/dump3
drv1e.o:		common/funct
drv1e.o:		common/g80nb
drv1e.o:		common/ghfblk
drv1e.o:		common/grad2
drv1e.o:		common/hermit
drv1e.o:		common/ijlab
drv1e.o:		common/infoa
drv1e.o:		common/iofile
drv1e.o:		common/machin
drv1e.o:		common/mapper
drv1e.o:		common/misc
drv1e.o:		common/modj
drv1e.o:		common/mp2grad_pointers
drv1e.o:		common/nodinf
drv1e.o:		common/nshel
drv1e.o:		common/prints
drv1e.o:		common/prnprn
drv1e.o:		common/repp
drv1e.o:		common/restar
drv1e.o:		common/restri
drv1e.o:		common/restrl
drv1e.o:		common/root
drv1e.o:		common/runlab
drv1e.o:		common/scfwfn
drv1e.o:		common/scra7
drv1e.o:		common/segm
drv1e.o:		common/sizes
drv1e.o:		common/specal
drv1e.o:		common/statis
drv1e.o:		common/symtry
drv1e.o:		common/timeperiods
drv1e.o:		common/timez
drv1e.o:		common/wermit
drv1e.o:		common/zorac
drv2e.o:		common/atmblk
drv2e.o:		common/ccpdft.hf77
drv2e.o:		common/cigrad
drv2e.o:		common/cndx41
drv2e.o:		common/cslosc
drv2e.o:		common/dmisc
drv2e.o:		common/dshlnf
drv2e.o:		common/dshlno
drv2e.o:		common/dump3
drv2e.o:		common/funct
drv2e.o:		common/ghfblk
drv2e.o:		common/grad2
drv2e.o:		common/ijlab
drv2e.o:		common/incrd
drv2e.o:		common/infoa
drv2e.o:		common/iofile
drv2e.o:		common/mapper
drv2e.o:		common/nshel
drv2e.o:		common/parallel
drv2e.o:		common/picon
drv2e.o:		common/prints
drv2e.o:		common/prnprn
drv2e.o:		common/restar
drv2e.o:		common/restri
drv2e.o:		common/root
drv2e.o:		common/scfwfn
drv2e.o:		common/segm
drv2e.o:		common/sizes
drv2e.o:		common/specal
drv2e.o:		common/symtry
drv2e.o:		common/timez
drv2e.o:		common/vcore
drv80.o:		common/auxvar
drv80.o:		common/ccpdft.hf77
drv80.o:		common/cslosc
drv80.o:		common/dindex
drv80.o:		common/dmisc
drv80.o:		common/ffq
drv80.o:		common/funct
drv80.o:		common/grad2
drv80.o:		common/ijlab
drv80.o:		common/infoa
drv80.o:		common/iofile
drv80.o:		common/mapper
drv80.o:		common/nshel
drv80.o:		common/parallel
drv80.o:		common/prints
drv80.o:		common/restar
drv80.o:		common/scfwfn
drv80.o:		common/sizes
drv80.o:		common/specal
drv80.o:		common/symtry
drv80.o:		common/timez
drvmp.o:		common/atmblk
drvmp.o:		common/cigrad
drvmp.o:		common/cndx41
drvmp.o:		common/common
drvmp.o:		common/infoa
drvmp.o:		common/iofile
drvmp.o:		common/mapper
drvmp.o:		common/nshel
drvmp.o:		common/sizes
drvmp.o:		common/stak
drvmp.o:		common/symtry
drvmp.o:		common/three
fullci.o:		common/cciab
fullci.o:		common/ci
fullci.o:		common/cone
fullci.o:		common/cstrng
fullci.o:		common/direc
fullci.o:		common/discc
fullci.o:		common/filel
fullci.o:		common/infoa
fullci.o:		common/iofile
fullci.o:		common/machin
fullci.o:		common/mapper
fullci.o:		common/prnprn
fullci.o:		common/restar
fullci.o:		common/restri
fullci.o:		common/savem
fullci.o:		common/scfwfn
fullci.o:		common/sizes
fullci.o:		common/statis
fullci.o:		common/timez
fullci.o:		common/work
ga.o:		common/disc
ga.o:		common/discc
ga.o:		common/errcodes
ga.o:		common/ga_file
ga.o:		common/gaanal
ga.o:		common/gadiis
ga.o:		common/harmon
ga.o:		common/infoa
ga.o:		common/iofile
ga.o:		common/mapper
ga.o:		common/nodeio
ga.o:		common/parcntl
ga.o:		common/prints
ga.o:		common/scfopt
ga.o:		common/scra7
ga.o:		common/sizes
ga.o:		common/timeperiods
ga.o:		common/vcore
ga.o:		common/vcore
gff.o:		common/discc
gff.o:		common/filel
gff.o:		common/iofile
gff.o:		common/machin
gff.o:		common/prints
gff.o:		common/restar
gff.o:		common/restri
gff.o:		common/runlab
gff.o:		common/sizes
gff.o:		common/statis
gff.o:		common/work
guess.o:		../drf/comdrf/dafil
guess.o:		../drf/comdrf/drfpar
guess.o:		common/atmol3
guess.o:		common/blockc
guess.o:		common/cslosc
guess.o:		common/datgue
guess.o:		common/discc
guess.o:		common/drfopt
guess.o:		common/dump3
guess.o:		common/gadiis
guess.o:		common/gjs
guess.o:		common/harmon
guess.o:		common/hermit
guess.o:		common/infoa
guess.o:		common/iofile
guess.o:		common/machin
guess.o:		common/mapper
guess.o:		common/nodeio
guess.o:		common/nshel
guess.o:		common/parcntl
guess.o:		common/phycon
guess.o:		common/prints
guess.o:		common/restar
guess.o:		common/runlab
guess.o:		common/scfopt
guess.o:		common/scfwfn
guess.o:		common/scra7
guess.o:		common/segm
guess.o:		common/sizes
guess.o:		common/statis
guess.o:		common/syminf
guess.o:		common/symtry
guess.o:		common/timeperiods
guess.o:		common/timez
guess.o:		common/tran
guess.o:		common/tranao
guess.o:		common/wermit
guess.o:		common/zorac
index4.o:		common/atmblk
index4.o:		common/cigrad
index4.o:		common/cndx40
index4.o:		common/cndx41
index4.o:		common/common
index4.o:		common/ghfblk
index4.o:		common/harmon
index4.o:		common/infoa
index4.o:		common/iofile
index4.o:		common/machin
index4.o:		common/mapper
index4.o:		common/nshel
index4.o:		common/prints
index4.o:		common/prnprn
index4.o:		common/restar
index4.o:		common/restrj
index4.o:		common/runlab
index4.o:		common/sizes
index4.o:		common/stak
index4.o:		common/symtry
index4.o:		common/three
index4.o:		common/timez
index4.o:		common/tran
index4.o:		common/uhfspn
index4.o:		common/vectrn
index4.o:		common/zorac
inpci.o:		common/avstat
inpci.o:		common/ciconv
inpci.o:		common/cigrad
inpci.o:		common/cjdavid
inpci.o:		common/cndx40
inpci.o:		common/ctrl
inpci.o:		common/direc
inpci.o:		common/discc
inpci.o:		common/disktl
inpci.o:		common/dm
inpci.o:		common/dump3
inpci.o:		common/exc
inpci.o:		common/files
inpci.o:		common/finish
inpci.o:		common/fpinfo
inpci.o:		common/gjs
inpci.o:		common/harmon
inpci.o:		common/infoa
inpci.o:		common/iofile
inpci.o:		common/jobopt
inpci.o:		common/limy
inpci.o:		common/machin
inpci.o:		common/mapper
inpci.o:		common/multic
inpci.o:		common/prints
inpci.o:		common/restar
inpci.o:		common/runlab
inpci.o:		common/scra7
inpci.o:		common/segm
inpci.o:		common/simul
inpci.o:		common/sizes
inpci.o:		common/statis
inpci.o:		common/syminf
inpci.o:		common/timez
inpci.o:		common/work
inpci.o:		common/workc
inpci.o:		common/zorac
input.o:		../drf/comdrf/trnsf
input.o:		common/atmol3
input.o:		common/blur
input.o:		common/cndx41
input.o:		common/cntl1
input.o:		common/common
input.o:		common/coval
input.o:		common/crnams
input.o:		common/crnamx
input.o:		common/csubch
input.o:		common/csubst
input.o:		common/czmat
input.o:		common/datbas
input.o:		common/datgue
input.o:		common/direc
input.o:		common/discc
input.o:		common/dump3
input.o:		common/errcodes
input.o:		common/foropt
input.o:		common/fpinfo
input.o:		common/frame
input.o:		common/fsymas
input.o:		common/ghfblk
input.o:		common/harmon
input.o:		common/infoa
input.o:		common/infob
input.o:		common/iofile
input.o:		common/machin
input.o:		common/mapper
input.o:		common/mod2d
input.o:		common/moda
input.o:		common/modf
input.o:		common/modfov
input.o:		common/modj
input.o:		common/modmd
input.o:		common/modmin
input.o:		common/molsym
input.o:		common/mp2grd
input.o:		common/nbterm
input.o:		common/nshel
input.o:		common/phycon
input.o:		common/prints
input.o:		common/prnprn
input.o:		common/qercmx
input.o:		common/qercom
input.o:		common/restar
input.o:		common/restri
input.o:		common/restrj
input.o:		common/restrl
input.o:		common/rtdata
input.o:		common/runhed
input.o:		common/runlab
input.o:		common/scfopt
input.o:		common/scfwfn
input.o:		common/scra7
input.o:		common/secd_pointers
input.o:		common/sector
input.o:		common/sizes
input.o:		common/symtry
input.o:		common/tdhf
input.o:		common/tdhfx
input.o:		common/tol
input.o:		common/transf
input.o:		common/work
input.o:		common/workc
input.o:		common/xfield
intega.o:		../drf/comdrf/dafil
intega.o:		../drf/comdrf/darw
intega.o:		../drf/comdrf/drfbem
intega.o:		../drf/comdrf/drfdaf
intega.o:		../drf/comdrf/drfexp
intega.o:		../drf/comdrf/drfint
intega.o:		../drf/comdrf/drfpar
intega.o:		../drf/comdrf/ijpair
intega.o:		../drf/comdrf/int
intega.o:		../drf/comdrf/iofil
intega.o:		../drf/comdrf/mem
intega.o:		../drf/comdrf/opt
intega.o:		../drf/comdrf/runpar
intega.o:		../drf/comdrf/scfopt
intega.o:		../drf/comdrf/scmprt
intega.o:		../drf/comdrf/sizesrf
intega.o:		common/atmblk
intega.o:		common/bshell
intega.o:		common/ccpdft.hf77
intega.o:		common/chgcc
intega.o:		common/cslosc
intega.o:		common/denss
intega.o:		common/discc
intega.o:		common/drfopt
intega.o:		common/dump3
intega.o:		common/files
intega.o:		common/flip70
intega.o:		common/flips
intega.o:		common/g80nb
intega.o:		common/hermit
intega.o:		common/ijlab
intega.o:		common/imisc
intega.o:		common/indez
intega.o:		common/infoa
intega.o:		common/inxblk
intega.o:		common/iofile
intega.o:		common/machin
intega.o:		common/mapper
intega.o:		common/maxlen
intega.o:		common/misc
intega.o:		common/modj
intega.o:		common/nshel
intega.o:		common/parallel
intega.o:		common/parcntl
intega.o:		common/picon
intega.o:		common/pkfil
intega.o:		common/prints
intega.o:		common/prnprn
intega.o:		common/restar
intega.o:		common/restri
intega.o:		common/restrl
intega.o:		common/root
intega.o:		common/rtdata
intega.o:		common/runlab
intega.o:		common/segm
intega.o:		common/setint
intega.o:		common/shlg70
intega.o:		common/shlinf
intega.o:		common/shlnos
intega.o:		common/shlt
intega.o:		common/sizes
intega.o:		common/sortp
intega.o:		common/statis
intega.o:		common/symtry
intega.o:		common/timez
intega.o:		common/utilc
intega.o:		common/wermit
intega.o:		common/xfield
intega.o:		common/zorac
integb.o:		common/dawfcm
integb.o:		common/derfcm
integb.o:		common/direc
integb.o:		common/discc
integb.o:		common/ftabcm
integb.o:		common/infoa
integb.o:		common/iofile
integb.o:		common/key2ec
integb.o:		common/machin
integb.o:		common/mapper
integb.o:		common/nshel
integb.o:		common/rcombd
integb.o:		common/repp
integb.o:		common/restar
integb.o:		common/restrl
integb.o:		common/runlab
integb.o:		common/sizes
integb.o:		common/symtry
integb.o:		common/timez
integb.o:		common/work
integb.o:		common/workc
integb.o:		common/ztabcm
integc.o:		common/atmblk
integc.o:		common/bshell
integc.o:		common/const
integc.o:		common/flip70
integc.o:		common/geom
integc.o:		common/ginf
integc.o:		common/ijlab
integc.o:		common/infoa
integc.o:		common/iofile
integc.o:		common/mapper
integc.o:		common/maxc
integc.o:		common/miscg
integc.o:		common/nshel
integc.o:		common/parallel
integc.o:		common/pgeom
integc.o:		common/picon
integc.o:		common/pkfil
integc.o:		common/qgeom
integc.o:		common/restar
integc.o:		common/restri
integc.o:		common/shlg70
integc.o:		common/shllfo
integc.o:		common/shlnos
integc.o:		common/shlt
integc.o:		common/sizes
integc.o:		common/sortp
integc.o:		common/symtry
integc.o:		common/timez
integd.o:		common/atmblk
integd.o:		common/cigrad
integd.o:		common/cndx41
integd.o:		common/common
integd.o:		common/infoa
integd.o:		common/iofile
integd.o:		common/mapper
integd.o:		common/nshel
integd.o:		common/prnprn
integd.o:		common/runlab
integd.o:		common/sizes
integd.o:		common/symtry
integd.o:		common/timez
intege.o:		common/atmblk
intege.o:		common/mapper
intege.o:		common/sizes
intege.o:		common/three
integs.o:		common/astore
integs.o:		common/auxvar
integs.o:		common/bshell
integs.o:		common/ccpdft.hf77
integs.o:		common/confus
integs.o:		common/const
integs.o:		common/cslosc
integs.o:		common/flip70
integs.o:		common/geom
integs.o:		common/ginf
integs.o:		common/ijlab
integs.o:		common/indez
integs.o:		common/infoa
integs.o:		common/iofile
integs.o:		common/machin
integs.o:		common/mapper
integs.o:		common/maxc
integs.o:		common/misc
integs.o:		common/miscg
integs.o:		common/nshel
integs.o:		common/parallel
integs.o:		common/pgeom
integs.o:		common/picon
integs.o:		common/pkfil
integs.o:		common/pqgeom
integs.o:		common/qgeom
integs.o:		common/restar
integs.o:		common/restri
integs.o:		common/runlab
integs.o:		common/scfopt
integs.o:		common/segm
integs.o:		common/shlg70
integs.o:		common/shllfo
integs.o:		common/shlnos
integs.o:		common/shlt
integs.o:		common/sizes
integs.o:		common/sortp
integs.o:		common/symtry
integs.o:		common/timez
integs.o:		common/utilc
machci.o:		common/cfsinfo
machci.o:		common/iofile
machci.o:		common/nodinf
machci.o:		common/sizes
machci.o:		common/stak
machci.o:		common/utilc
machscf.o:		common/atmblk
machscf.o:		common/blocks
machscf.o:		common/cfsinfo
machscf.o:		common/direc
machscf.o:		common/disc
machscf.o:		common/discc
machscf.o:		common/errcodes
machscf.o:		common/files
machscf.o:		common/ga_file
machscf.o:		common/gmemdata
machscf.o:		common/infoa
machscf.o:		common/iofile
machscf.o:		common/ipsc
machscf.o:		common/jinfo
machscf.o:		common/machin
machscf.o:		common/maxlen
machscf.o:		common/nodeio
machscf.o:		common/nodinf
machscf.o:		common/parcntl
machscf.o:		common/prints
machscf.o:		common/restar
machscf.o:		common/restrj
machscf.o:		common/segm
machscf.o:		common/sizes
machscf.o:		common/sizes
machscf.o:		common/symtry
machscf.o:		common/syscod
machscf.o:		common/timeperiods
machscf.o:		common/timez
machscf.o:		common/utilc
machscf.o:		common/work
machscf.o:		common/workc
mainci.o:		common/auxh
mainci.o:		common/avstat
mainci.o:		common/caspar
mainci.o:		common/ccepa
mainci.o:		common/cdcryi
mainci.o:		common/cic
mainci.o:		common/ciconv
mainci.o:		common/comrjb
mainci.o:		common/corctl
mainci.o:		common/ctrl
mainci.o:		common/diactl
mainci.o:		common/dm
mainci.o:		common/finish
mainci.o:		common/helpr
mainci.o:		common/hold
mainci.o:		common/jobopt
mainci.o:		common/mcff
mainci.o:		common/natorb
mainci.o:		common/outctl
mainci.o:		common/presrt
mainci.o:		common/qice
mainci.o:		common/simul
mainci.o:		common/sizes
mainci.o:		common/symci
mainci.o:		common/syminf
mainci.o:		common/trial
gamess.o:		common/astore
gamess.o:		common/atmblk
gamess.o:		common/atmol3
gamess.o:		common/auxvar
gamess.o:		common/blockc
gamess.o:		common/blocks
gamess.o:		common/bshell
gamess.o:		common/chemshell
gamess.o:		common/chgcc
gamess.o:		common/chmgms
gamess.o:		common/cndx40
gamess.o:		common/cndx41
gamess.o:		common/cntl1
gamess.o:		common/cntl2
gamess.o:		common/const
gamess.o:		common/copt
gamess.o:		common/coval
gamess.o:		common/crnams
gamess.o:		common/crnamx
gamess.o:		common/csubch
gamess.o:		common/csubst
gamess.o:		common/datbas
gamess.o:		common/datgue
gamess.o:		common/datplt
gamess.o:		common/datprp
gamess.o:		common/defunk
gamess.o:		common/denss
gamess.o:		common/disc
gamess.o:		common/discc
gamess.o:		common/dmisc
gamess.o:		common/drfopt
gamess.o:		common/errcodes
gamess.o:		common/extsav
gamess.o:		common/ffq
gamess.o:		common/field
gamess.o:		common/filel
gamess.o:		common/files
gamess.o:		common/foropt
gamess.o:		common/fpinfo
gamess.o:		common/frame
gamess.o:		common/fsymas
gamess.o:		common/funct
gamess.o:		common/ghfblk
gamess.o:		common/ginf
gamess.o:		common/gjs
gamess.o:		common/grad2
gamess.o:		common/gvalue
gamess.o:		common/harmon
gamess.o:		common/hermit
gamess.o:		common/ijlab
gamess.o:		common/imisc
gamess.o:		common/incrdd
gamess.o:		common/indez
gamess.o:		common/indsyx
gamess.o:		common/indxsv
gamess.o:		common/infoa
gamess.o:		common/infob
gamess.o:		common/inxblk
gamess.o:		common/iofile
gamess.o:		common/jinfo
gamess.o:		common/jspec
gamess.o:		common/junkc
gamess.o:		common/junkcc
gamess.o:		common/linkan
gamess.o:		common/machin
gamess.o:		common/mapper
gamess.o:		common/maxlen
gamess.o:		common/misc
gamess.o:		common/miscg
gamess.o:		common/modfov
gamess.o:		common/modj
gamess.o:		common/molsym
gamess.o:		common/nshel
gamess.o:		common/periodic
gamess.o:		common/phycon
gamess.o:		common/picon
gamess.o:		common/prints
gamess.o:		common/qercmx
gamess.o:		common/qercom
gamess.o:		common/rcombd
gamess.o:		common/restar
gamess.o:		common/restri
gamess.o:		common/restrj
gamess.o:		common/rtdata
gamess.o:		common/runlab
gamess.o:		common/scra7
gamess.o:		common/scrf
gamess.o:		common/sector
gamess.o:		common/seerch
gamess.o:		common/segm
gamess.o:		common/shlg70
gamess.o:		common/shlinf
gamess.o:		common/shlt
gamess.o:		common/sizes
gamess.o:		common/statis
gamess.o:		common/tdhf
gamess.o:		common/tdhfx
gamess.o:		common/timez
gamess.o:		common/tol
gamess.o:		common/tranao
gamess.o:		common/transf
gamess.o:		common/trntim
gamess.o:		common/unocas
gamess.o:		common/utilc
gamess.o:		common/vcore
gamess.o:		common/vdwrad
gamess.o:		common/vibrtn
gamess.o:		common/wermit
gamess.o:		common/work
gamess.o:		common/workc
gamess.o:		common/worksp
gamess.o:		common/zjorgs
gamess.o:		common/zlabs
gamess.o:		common/zorac
mass.o:		common/errcodes
mass.o:		common/infoa
mass.o:		common/infob
mass.o:		common/iofile
mass.o:		common/masses
mass.o:		common/periodic
mass.o:		common/runlab
mass.o:		common/sizes
mass.o:		common/work
master.o:		../drf/comdrf/ciopt
master.o:		../drf/comdrf/dafil
master.o:		../drf/comdrf/darw
master.o:		../drf/comdrf/drfbem
master.o:		../drf/comdrf/drfdaf
master.o:		../drf/comdrf/drfpar
master.o:		../drf/comdrf/drfzfa
master.o:		../drf/comdrf/enrgci
master.o:		../drf/comdrf/extinf
master.o:		../drf/comdrf/rfene
master.o:		../drf/comdrf/runpar
master.o:		../drf/comdrf/sizesrf
master.o:		../drf/comdrf/trnsf
master.o:		common/atmblk
master.o:		common/atmol3
master.o:		common/auxvar
master.o:		common/blocks
master.o:		common/blur
master.o:		common/ccpdft.hf77
master.o:		common/chgcc
master.o:		common/chmgms
master.o:		common/ciconv
master.o:		common/cigrad
master.o:		common/cndx40
master.o:		common/cndx41
master.o:		common/cntl1
master.o:		common/common
master.o:		common/copt
master.o:		common/crnams
master.o:		common/crnamx
master.o:		common/cslosc
master.o:		common/ctrl
master.o:		common/czmat
master.o:		common/datgue
master.o:		common/datprp
master.o:		common/direc
master.o:		common/disc
master.o:		common/discc
master.o:		common/dm
master.o:		common/dnfnw
master.o:		common/drfopt
master.o:		common/dump3
master.o:		common/errcodes
master.o:		common/field
master.o:		common/files
master.o:		common/finish
master.o:		common/foropt
master.o:		common/fpinfo
master.o:		common/fsymas
master.o:		common/funct
master.o:		common/g80nb
master.o:		common/gen3
master.o:		common/ghfblk
master.o:		common/gjs
master.o:		common/groups
master.o:		common/gvalue
master.o:		common/harmon
master.o:		common/harmon
master.o:		common/indsyx
master.o:		common/infoa
master.o:		common/infob
master.o:		common/iofile
master.o:		common/jinfo
master.o:		common/jspec
master.o:		common/limy
master.o:		common/machin
master.o:		common/mapper
master.o:		common/maxlen
master.o:		common/mod2d
master.o:		common/moda
master.o:		common/modf
master.o:		common/modfov
master.o:		common/modj
master.o:		common/modmd
master.o:		common/modmin
master.o:		common/molsym
master.o:		common/morokuma
master.o:		common/mp2grd
master.o:		common/mpcut
master.o:		common/natorb
master.o:		common/nbterm
master.o:		common/newmrd_sort
master.o:		common/newscf
master.o:		common/nodeio
master.o:		common/nshel
master.o:		common/orb
master.o:		common/parcntl
master.o:		common/periodic
master.o:		common/phycon
master.o:		common/picon
master.o:		common/prints
master.o:		common/prnprn
master.o:		common/psscrf
master.o:		common/qercmx
master.o:		common/qercom
master.o:		common/repp
master.o:		common/restar
master.o:		common/restrj
master.o:		common/root
master.o:		common/runhed
master.o:		common/runlab
master.o:		common/scfopt
master.o:		common/scfwfn
master.o:		common/scra7
master.o:		common/scrf
master.o:		common/sector
master.o:		common/seerch
master.o:		common/segm
master.o:		common/simul
master.o:		common/sizes
master.o:		common/sortp
master.o:		common/specal
master.o:		common/statis
master.o:		common/symtry
master.o:		common/tdhf
master.o:		common/tdhfx
master.o:		common/timeperiods
master.o:		common/timez
master.o:		common/tol
master.o:		common/tran
master.o:		common/trntim
master.o:		common/unocas
master.o:		common/utilc
master.o:		common/vectrn
master.o:		common/work
master.o:		common/workc
master.o:		common/wrtd
master.o:		common/xfield
master.o:		common/zjorgs
master.o:		common/zorac
mcdab_ga.o:		common/cigrad
mcdab_ga.o:		common/dmisc
mcdab_ga.o:		common/incrd
mcdab_ga.o:		common/nshel
mcdab_ga.o:		common/sizes
mclr.o:		common/block
mclr.o:		common/debug
mclr.o:		common/iofile
mclr.o:		common/lradrs
mclr.o:		common/lrfile
mclr.o:		common/machin
mclr.o:		common/mcff
mclr.o:		common/mclr_blocks
mclr.o:		common/mcrun
mclr.o:		common/optima
mclr.o:		common/polars
mclr.o:		common/restar
mclr.o:		common/sector
mclr.o:		common/sizes
mclr.o:		common/tran
mclr.o:		common/work
mcscfa.o:		common/count
mcscfa.o:		common/detcic
mcscfa.o:		common/direc
mcscfa.o:		common/discc
mcscfa.o:		common/gjs
mcscfa.o:		common/harmon
mcscfa.o:		common/iofile
mcscfa.o:		common/jinfo
mcscfa.o:		common/jobopt
mcscfa.o:		common/machin
mcscfa.o:		common/mapper
mcscfa.o:		common/mcff
mcscfa.o:		common/mcscra
mcscfa.o:		common/multic
mcscfa.o:		common/runlab
mcscfa.o:		common/sizes
mcscfa.o:		common/syminf
mcscfb.o:		common/atmblk
mcscfb.o:		common/count
mcscfb.o:		common/detcic
mcscfb.o:		common/harmon
mcscfb.o:		common/jinfo
mcscfb.o:		common/jobopt
mcscfb.o:		common/machin
mcscfb.o:		common/mapper
mcscfb.o:		common/mcaddr
mcscfb.o:		common/mcff
mcscfb.o:		common/mcscra
mcscfb.o:		common/multic
mcscfb.o:		common/runlab
mcscfb.o:		common/sizes
mcscfb.o:		common/syminf
mcscfc.o:		common/jobopt
mcscfc.o:		common/multic
mcscfc.o:		common/sizes
mcscfc.o:		common/syminf
model.o:		common/cntl1
model.o:		common/csubst
model.o:		common/finish
model.o:		common/funct
model.o:		common/g80nb
model.o:		common/infoa
model.o:		common/infob
model.o:		common/iofile
model.o:		common/mod2d
model.o:		common/moda
model.o:		common/modf
model.o:		common/modfov
model.o:		common/modj
model.o:		common/modmd
model.o:		common/modmin
model.o:		common/molsym
model.o:		common/nbterm
model.o:		common/phycon
model.o:		common/prints
model.o:		common/restar
model.o:		common/runhed
model.o:		common/runlab
model.o:		common/segm
model.o:		common/sizes
model.o:		common/statis
morokuma.o:		common/atmblk
morokuma.o:		common/atmol3
morokuma.o:		common/ccpdft.hf77
morokuma.o:		common/cslosc
morokuma.o:		common/dnfnw
morokuma.o:		common/dump3
morokuma.o:		common/extsav
morokuma.o:		common/field
morokuma.o:		common/filel
morokuma.o:		common/fsymas
morokuma.o:		common/funct
morokuma.o:		common/gvalue
morokuma.o:		common/infoa
morokuma.o:		common/iofile
morokuma.o:		common/mapper
morokuma.o:		common/maxlen
morokuma.o:		common/modj
morokuma.o:		common/morokuma
morokuma.o:		common/nodeio
morokuma.o:		common/phycon
morokuma.o:		common/prints
morokuma.o:		common/prnprn
morokuma.o:		common/psscrf
morokuma.o:		common/restar
morokuma.o:		common/restri
morokuma.o:		common/restrl
morokuma.o:		common/runlab
morokuma.o:		common/scfopt
morokuma.o:		common/scra7
morokuma.o:		common/scrf
morokuma.o:		common/segm
morokuma.o:		common/sizes
morokuma.o:		common/sortp
morokuma.o:		common/statis
morokuma.o:		common/symtry
morokuma.o:		common/timez
morokuma.o:		common/tran
morokuma.o:		common/vcore
morokuma.o:		common/vcore
morokuma.o:		common/xfield
mp2_parallel.o:		common/atmblk
mp2_parallel.o:		common/atmol3
mp2_parallel.o:		common/cigrad
mp2_parallel.o:		common/cndx40
mp2_parallel.o:		common/cndx41
mp2_parallel.o:		common/cntl1
mp2_parallel.o:		common/cntl2
mp2_parallel.o:		common/common
mp2_parallel.o:		common/cslosc
mp2_parallel.o:		common/dmisc
mp2_parallel.o:		common/dshlno
mp2_parallel.o:		common/dump3
mp2_parallel.o:		common/errcodes
mp2_parallel.o:		common/flips
mp2_parallel.o:		common/fsymas
mp2_parallel.o:		common/ghfblk
mp2_parallel.o:		common/global
mp2_parallel.o:		common/gmemdata
mp2_parallel.o:		common/ijlab
mp2_parallel.o:		common/incrd
mp2_parallel.o:		common/indez
mp2_parallel.o:		common/infoa
mp2_parallel.o:		common/iofile
mp2_parallel.o:		common/machin
mp2_parallel.o:		common/mapper
mp2_parallel.o:		common/misc
mp2_parallel.o:		common/modj
mp2_parallel.o:		common/mp2grad_pointers
mp2_parallel.o:		common/mp2grd
mp2_parallel.o:		common/nshel
mp2_parallel.o:		common/parallel
mp2_parallel.o:		common/parcntl
mp2_parallel.o:		common/picon
mp2_parallel.o:		common/pkfil
mp2_parallel.o:		common/prints
mp2_parallel.o:		common/prnprn
mp2_parallel.o:		common/psscrf
mp2_parallel.o:		common/restar
mp2_parallel.o:		common/restri
mp2_parallel.o:		common/restrj
mp2_parallel.o:		common/restrl
mp2_parallel.o:		common/root
mp2_parallel.o:		common/runlab
mp2_parallel.o:		common/scfopt
mp2_parallel.o:		common/scra7
mp2_parallel.o:		common/segm
mp2_parallel.o:		common/shlg70
mp2_parallel.o:		common/shlinf
mp2_parallel.o:		common/shlnos
mp2_parallel.o:		common/shlt
mp2_parallel.o:		common/sizes
mp2_parallel.o:		common/sortp
mp2_parallel.o:		common/specal
mp2_parallel.o:		common/statis
mp2_parallel.o:		common/symtry
mp2_parallel.o:		common/timeperiods
mp2_parallel.o:		common/timez
mp2_parallel.o:		common/tran
mp2_parallel.o:		common/utilc
mp2_parallel.o:		common/vcore
mp2_parallel.o:		common/vectrn
mp2a.o:		common/atmblk
mp2a.o:		common/cigrad
mp2a.o:		common/cndx40
mp2a.o:		common/cndx41
mp2a.o:		common/common
mp2a.o:		common/infoa
mp2a.o:		common/iofile
mp2a.o:		common/mapper
mp2a.o:		common/nshel
mp2a.o:		common/prnprn
mp2a.o:		common/restrj
mp2a.o:		common/sizes
mp2a.o:		common/stak
mp2a.o:		common/statis
mp2a.o:		common/three
mp2a.o:		common/timeperiods
mp2a.o:		common/vectrn
mp2b.o:		common/atmblk
mp2b.o:		common/cigrad
mp2b.o:		common/cndx40
mp2b.o:		common/cndx41
mp2b.o:		common/common
mp2b.o:		common/infoa
mp2b.o:		common/iofile
mp2b.o:		common/mapper
mp2b.o:		common/nshel
mp2b.o:		common/prnprn
mp2b.o:		common/sizes
mp2b.o:		common/symtry
mp2b.o:		common/uhfspn
mp2c.o:		common/iofile
mp2c.o:		common/prnprn
mp2c.o:		common/stak
mp2c.o:		common/three
mp2d.o:		common/atmblk
mp2d.o:		common/cigrad
mp2d.o:		common/cndx40
mp2d.o:		common/cndx41
mp2d.o:		common/common
mp2d.o:		common/infoa
mp2d.o:		common/iofile
mp2d.o:		common/mapper
mp2d.o:		common/nshel
mp2d.o:		common/sizes
mp2d.o:		common/symtry
mp2d.o:		common/tdhf
mp2d.o:		common/tdhfx
mp3.o:		common/atmblk
mp3.o:		common/cigrad
mp3.o:		common/cndx40
mp3.o:		common/cndx41
mp3.o:		common/common
mp3.o:		common/harmon
mp3.o:		common/infoa
mp3.o:		common/iofile
mp3.o:		common/mapper
mp3.o:		common/nshel
mp3.o:		common/prints
mp3.o:		common/sizes
mp3.o:		common/symtry
mp3.o:		common/uhfspn
mrdci1.o:		common/blockc
mrdci1.o:		common/discc
mrdci1.o:		common/dump3
mrdci1.o:		common/filel
mrdci1.o:		common/files
mrdci1.o:		common/ftape
mrdci1.o:		common/infoa
mrdci1.o:		common/iofile
mrdci1.o:		common/machin
mrdci1.o:		common/mapper
mrdci1.o:		common/prints
mrdci1.o:		common/restar
mrdci1.o:		common/sizes
mrdci1.o:		common/zorac
mrdci2.o:		common/atmol3
mrdci2.o:		common/blockc
mrdci2.o:		common/discc
mrdci2.o:		common/ftape
mrdci2.o:		common/infoa
mrdci2.o:		common/iofile
mrdci2.o:		common/machin
mrdci2.o:		common/mapper
mrdci2.o:		common/prints
mrdci2.o:		common/sizes
mrdci3.o:		common/iofile
mrdci3.o:		common/prints
mrdci3.o:		common/sizes
mrdci4.o:		common/discc
mrdci4.o:		common/infoa
mrdci4.o:		common/iofile
mrdci4.o:		common/jinfo
mrdci4.o:		common/machin
mrdci4.o:		common/mapper
mrdci4.o:		common/nshel
mrdci4.o:		common/prints
mrdci4.o:		common/runlab
mrdci4.o:		common/sizes
mrdci5.o:		common/iofile
mrdci5.o:		common/prints
mrdci6.o:		common/iofile
mrdci6.o:		common/prints
mrdci6.o:		common/sizes
mrdci7.o:		common/iofile
mrdci7.o:		common/prints
mrdci7.o:		common/runlab
mrdci7.o:		common/sizes
nag.o:		common/iofile
nbo.o:		common/atmol3
nbo.o:		common/dump3
nbo.o:		common/infoa
nbo.o:		common/iofile
nbo.o:		common/limy
nbo.o:		common/mapper
nbo.o:		common/nshel
nbo.o:		common/phycon
nbo.o:		common/restar
nbo.o:		common/restri
nbo.o:		common/runlab
nbo.o:		common/scfopt
nbo.o:		common/scra7
nbo.o:		common/sector
nbo.o:		common/sizes
nbo.o:		common/tran
newmrd1.o:		common/discc
newmrd1.o:		common/file12
newmrd1.o:		common/filel
newmrd1.o:		common/helpr
newmrd1.o:		common/iofile
newmrd1.o:		common/machin
newmrd1.o:		common/mrdcid
newmrd1.o:		common/newmrd_sort
newmrd1.o:		common/orb
newmrd1.o:		common/prints
newmrd1.o:		common/restar
newmrd1.o:		common/sizes
newmrd3.o:		common/mrdcid
newmrd3.o:		common/newmrd_parinc
newmrd3.o:		common/newmrd_sort
newmrd3.o:		common/restrj
newmrd3.o:		common/sizes
newmrd3.o:		common/work
newmrd3.o:		common/workc
newmrd4.o:		common/newmrd_rglob
newmrd5.o:		common/newmrd_adlinc
newmrd5.o:		common/newmrd_rglob
nvccsd.o:		common/prnprn
nvccsd.o:		common/t_adata
nvccsd.o:		common/t_files
nvccsd.o:		common/t_int
nvccsd.o:		common/t_iter
nvccsd.o:		common/t_parm
optef.o:		common/cntl1
optef.o:		common/cntl2
optef.o:		common/csubch
optef.o:		common/csubst
optef.o:		common/czmat
optef.o:		common/dump3
optef.o:		common/funct
optef.o:		common/infoa
optef.o:		common/infob
optef.o:		common/iofile
optef.o:		common/machin
optef.o:		common/prints
optef.o:		common/restar
optef.o:		common/restri
optef.o:		common/runlab
optef.o:		common/scfopt
optef.o:		common/sector
optef.o:		common/segm
optef.o:		common/sizes
optef.o:		common/statis
optef.o:		common/timez
optim.o:		../drf/comdrf/drfbem
optim.o:		../drf/comdrf/drfpar
optim.o:		../drf/comdrf/rfene
optim.o:		../drf/comdrf/runpar
optim.o:		../drf/comdrf/sizesrf
optim.o:		common/atmol3
optim.o:		common/cigrad
optim.o:		common/cndx41
optim.o:		common/cntl1
optim.o:		common/cntl2
optim.o:		common/common
optim.o:		common/copt
optim.o:		common/cslosc
optim.o:		common/csubch
optim.o:		common/csubst
optim.o:		common/czmat
optim.o:		common/discc
optim.o:		common/drfopt
optim.o:		common/dump3
optim.o:		common/foropt
optim.o:		common/fpinfo
optim.o:		common/funct
optim.o:		common/harmon
optim.o:		common/infoa
optim.o:		common/infob
optim.o:		common/iofile
optim.o:		common/machin
optim.o:		common/mapper
optim.o:		common/modj
optim.o:		common/molsym
optim.o:		common/natorb
optim.o:		common/phycon
optim.o:		common/prints
optim.o:		common/prnprn
optim.o:		common/restar
optim.o:		common/restri
optim.o:		common/restrj
optim.o:		common/runlab
optim.o:		common/scfwfn
optim.o:		common/seerch
optim.o:		common/segm
optim.o:		common/sizes
optim.o:		common/symtry
optim.o:		common/timez
optim.o:		common/tran
optim.o:		common/vibrtn
optim.o:		common/work
gmx.o:		../drf/comdrf/drfbem
gmx.o:		../drf/comdrf/drfpar
gmx.o:		../drf/comdrf/rfene
gmx.o:		../drf/comdrf/runpar
gmx.o:		../drf/comdrf/sizesrf
gmx.o:		common/atmol3
gmx.o:		common/cigrad
gmx.o:		common/cndx41
gmx.o:		common/cntl1
gmx.o:		common/cntl2
gmx.o:		common/common
gmx.o:		common/copt
gmx.o:		common/cslosc
gmx.o:		common/csubch
gmx.o:		common/csubst
gmx.o:		common/czmat
gmx.o:		common/discc
gmx.o:		common/drfopt
gmx.o:		common/dump3
gmx.o:		common/foropt
gmx.o:		common/fpinfo
gmx.o:		common/funct
gmx.o:		common/harmon
gmx.o:		common/infoa
gmx.o:		common/infob
gmx.o:		common/iofile
gmx.o:		common/machin
gmx.o:		common/mapper
gmx.o:		common/modj
gmx.o:		common/molsym
gmx.o:		common/natorb
gmx.o:		common/phycon
gmx.o:		common/prints
gmx.o:		common/prnprn
gmx.o:		common/restar
gmx.o:		common/restri
gmx.o:		common/restrj
gmx.o:		common/runlab
gmx.o:		common/scfwfn
gmx.o:		common/seerch
gmx.o:		common/segm
gmx.o:		common/sizes
gmx.o:		common/symtry
gmx.o:		common/timez
gmx.o:		common/tran
gmx.o:		common/vibrtn
gmx.o:		common/work
parallel.o:		common/gmemdata
parallel.o:		common/iofile
parallel.o:		common/mpidata
parallel.o:		common/nodinf
parallel.o:		common/parallel
parallel.o:		common/parcntl
parallel.o:		common/parcntl
parallel.o:		common/segm
parallel.o:		common/sizes
parallel.o:		common/taskinfo
parallel.o:		common/timeperiods
parallel.o:		common/vcore
parallel.o:		common/vcore
pdiag.o:		common/timeperiods
pdiag.o:		common/vcore
plot.o:		common/discc
plot.o:		common/infoa
plot.o:		common/machin
plot.o:		common/sector
plot.o:		common/sizes
ptest.o:		common/gadiis
ptest.o:		common/iofile
ptest.o:		common/parcntl
ptest.o:		common/vcore
rpa.o:		common/atmol3
rpa.o:		common/block
rpa.o:		common/direc
rpa.o:		common/dump3
rpa.o:		common/infoa
rpa.o:		common/iofile
rpa.o:		common/machin
rpa.o:		common/nshel
rpa.o:		common/polars
rpa.o:		common/restrj
rpa.o:		common/rpacom
rpa.o:		common/rpfile
rpa.o:		common/sizes
rpa.o:		common/tran
rpa.o:		common/workc
scf.o:		../drf/comdrf/drfbem
scf.o:		../drf/comdrf/drfdaf
scf.o:		../drf/comdrf/drfpar
scf.o:		../drf/comdrf/gsspar
scf.o:		../drf/comdrf/opt
scf.o:		../drf/comdrf/runpar
scf.o:		../drf/comdrf/sizesrf
scf.o:		common/atmol3
scf.o:		common/blur
scf.o:		common/ccpdft.hf77
scf.o:		common/chmgms
scf.o:		common/cslosc
scf.o:		common/disc
scf.o:		common/dm
scf.o:		common/dnfnw
scf.o:		common/drfopt
scf.o:		common/dump3
scf.o:		common/extsav
scf.o:		common/field
scf.o:		common/filel
scf.o:		common/fpinfo
scf.o:		common/fsymas
scf.o:		common/funct
scf.o:		common/gadiis
scf.o:		common/gjs
scf.o:		common/gvalue
scf.o:		common/harmon
scf.o:		common/infoa
scf.o:		common/iofile
scf.o:		common/machin
scf.o:		common/mapper
scf.o:		common/maxlen
scf.o:		common/modj
scf.o:		common/nodeio
scf.o:		common/nshel
scf.o:		common/parcntl
scf.o:		common/phycon
scf.o:		common/prints
scf.o:		common/prnprn
scf.o:		common/psscrf
scf.o:		common/restar
scf.o:		common/restrj
scf.o:		common/restrl
scf.o:		common/runlab
scf.o:		common/scfopt
scf.o:		common/scfwfn
scf.o:		common/scra7
scf.o:		common/scrf
scf.o:		common/segm
scf.o:		common/sizes
scf.o:		common/statis
scf.o:		common/symtry
scf.o:		common/timeperiods
scf.o:		common/timez
scf.o:		common/tran
scf.o:		common/vcore
scf.o:		common/xfield
scf.o:		common/zorac
sec1e.o:		common/cigrad
sec1e.o:		common/common
sec1e.o:		common/infoa
sec1e.o:		common/iofile
sec1e.o:		common/mapper
sec1e.o:		common/nshel
sec1e.o:		common/picon
sec1e.o:		common/prnprn
sec1e.o:		common/root
sec1e.o:		common/sizes
sec1e.o:		common/symtry
sec2e.o:		common/acc2dd
sec2e.o:		common/atmblk
sec2e.o:		common/cigrad
sec2e.o:		common/cndx41
sec2e.o:		common/common
sec2e.o:		common/ghfblk
sec2e.o:		common/grad2
sec2e.o:		common/infoa
sec2e.o:		common/iofile
sec2e.o:		common/mapper
sec2e.o:		common/nshel
sec2e.o:		common/parallel
sec2e.o:		common/prnprn
sec2e.o:		common/root
sec2e.o:		common/runlab
sec2e.o:		common/sizes
sec2e.o:		common/symtry
sec2e.o:		common/timez
secd_parallel.o:		common/atmblk
secd_parallel.o:		common/cigrad
secd_parallel.o:		common/cndx40
secd_parallel.o:		common/cndx41
secd_parallel.o:		common/common
secd_parallel.o:		common/ghfblk
secd_parallel.o:		common/global
secd_parallel.o:		common/infoa
secd_parallel.o:		common/iofile
secd_parallel.o:		common/mapper
secd_parallel.o:		common/mp2grad_pointers
secd_parallel.o:		common/mp2grd
secd_parallel.o:		common/nshel
secd_parallel.o:		common/prnprn
secd_parallel.o:		common/restrj
secd_parallel.o:		common/secd_pointers
secd_parallel.o:		common/segm
secd_parallel.o:		common/sizes
secd_parallel.o:		common/sortp
secd_parallel.o:		common/statis
secd_parallel.o:		common/symtry
secd_parallel.o:		common/timeperiods
secd_parallel.o:		common/timez
secd_parallel.o:		common/vcore
secd_parallel.o:		common/vectrn
secdchf.o:		common/cndx41
secdchf.o:		common/common
secdchf.o:		common/global
secdchf.o:		common/iofile
secdchf.o:		common/prnprn
secdchf.o:		common/sizes
secdchf.o:		common/timeperiods
secdchf.o:		common/timez
secdchf.o:		common/vcore
secmp2.o:		common/atmblk
secmp2.o:		common/cigrad
secmp2.o:		common/cndx41
secmp2.o:		common/common
secmp2.o:		common/infoa
secmp2.o:		common/iofile
secmp2.o:		common/mapper
secmp2.o:		common/nshel
secmp2.o:		common/prnprn
secmp2.o:		common/sizes
secmp2.o:		common/stak
secmp2.o:		common/symtry
secmp2.o:		common/three
server.o:		../drf/comdrf/drfpar
server.o:		../drf/comdrf/enrgci
server.o:		../drf/comdrf/extinf
server.o:		../drf/comdrf/rad
server.o:		../drf/comdrf/rfene
server.o:		../drf/comdrf/sizesrf
server.o:		common/aimpac
server.o:		common/atmblk
server.o:		common/atmol3
server.o:		common/blocks
server.o:		common/blur
server.o:		common/cndx40
server.o:		common/cndx41
server.o:		common/cntl1
server.o:		common/cntl2
server.o:		common/common
server.o:		common/coval
server.o:		common/csubch
server.o:		common/csubst
server.o:		common/czmat
server.o:		common/direc
server.o:		common/disc
server.o:		common/discc
server.o:		common/drfopt
server.o:		common/drfopt
server.o:		common/dump3
server.o:		common/errcodes
server.o:		common/files
server.o:		common/indxsv
server.o:		common/infoa
server.o:		common/infob
server.o:		common/iofile
server.o:		common/machin
server.o:		common/mapper
server.o:		common/maxlen
server.o:		common/molsym
server.o:		common/nodeio
server.o:		common/nodinf
server.o:		common/nshel
server.o:		common/periodic
server.o:		common/phycon
server.o:		common/prints
server.o:		common/restar
server.o:		common/restri
server.o:		common/restrl
server.o:		common/runlab
server.o:		common/scfopt
server.o:		common/scfwfn
server.o:		common/sector
server.o:		common/segm
server.o:		common/sizes
server.o:		common/statis
server.o:		common/symtry
server.o:		common/tran
server.o:		common/transf
server.o:		common/unocas
server.o:		common/work
server.o:		common/workc
tdaf.o:		common/discc
tdaf.o:		common/filel
tdaf.o:		common/iofile
tdaf.o:		common/machin
tdaf.o:		common/restar
tdaf.o:		common/restri
tdaf.o:		common/runlab
tdaf.o:		common/segm
tdaf.o:		common/sizes
tdaf.o:		common/statis
tran4.o:		common/block
tran4.o:		common/cndx40
tran4.o:		common/discc
tran4.o:		common/dump3
tran4.o:		common/field
tran4.o:		common/filel
tran4.o:		common/files
tran4.o:		common/harmon
tran4.o:		common/infoa
tran4.o:		common/iofile
tran4.o:		common/machin
tran4.o:		common/mlngth
tran4.o:		common/newmrd_sort
tran4.o:		common/prints
tran4.o:		common/prnprn
tran4.o:		common/restar
tran4.o:		common/runlab
tran4.o:		common/scra7
tran4.o:		common/segm
tran4.o:		common/sizes
tran4.o:		common/statis
tran4.o:		common/timez
tran4.o:		common/tran
tran4.o:		common/trntim
tran4.o:		common/zorac
tsort.o:		common/prnprn
tsort.o:		common/t_files
tsort.o:		common/t_flname
tsort.o:		common/t_iobuff
tsort.o:		common/t_pointr
tsort.o:		common/t_sect
tsort.o:		common/titan
tsort.o:		common/utilc
util1.o:		../drf/comdrf/clas
util1.o:		../drf/comdrf/drfamb
util1.o:		../drf/comdrf/drfbem
util1.o:		../drf/comdrf/drfin
util1.o:		../drf/comdrf/drfpar
util1.o:		../drf/comdrf/extinf
util1.o:		../drf/comdrf/mcener
util1.o:		../drf/comdrf/mcinp
util1.o:		../drf/comdrf/rfene
util1.o:		../drf/comdrf/rota
util1.o:		../drf/comdrf/runpar
util1.o:		../drf/comdrf/sizescon
util1.o:		../drf/comdrf/sizesrf
util1.o:		common/atmblk
util1.o:		common/chgcc
util1.o:		common/cigrad
util1.o:		common/cndx40
util1.o:		common/cndx41
util1.o:		common/common
util1.o:		common/connolly
util1.o:		common/disc
util1.o:		common/discc
util1.o:		common/dm
util1.o:		common/drfopt
util1.o:		common/dump3
util1.o:		common/errcodes
util1.o:		common/harmon
util1.o:		common/infoa
util1.o:		common/iofile
util1.o:		common/junkcc
util1.o:		common/machin
util1.o:		common/mapper
util1.o:		common/nshel
util1.o:		common/parcntl
util1.o:		common/phycon
util1.o:		common/prints
util1.o:		common/repp
util1.o:		common/restar
util1.o:		common/restri
util1.o:		common/restrj
util1.o:		common/restrl
util1.o:		common/runlab
util1.o:		common/scfopt
util1.o:		common/scfwfn
util1.o:		common/scra7
util1.o:		common/sector
util1.o:		common/segm
util1.o:		common/sizes
util1.o:		common/sizes
util1.o:		common/timeperiods
util1.o:		common/timez
util1.o:		common/tran
util1.o:		common/work
util1.o:		common/workc
util2.o:		common/atmblk
util2.o:		common/ccpdft.hf77
util2.o:		common/cslosc
util2.o:		common/disc
util2.o:		common/discc
util2.o:		common/dm
util2.o:		common/dmisc
util2.o:		common/filel
util2.o:		common/files
util2.o:		common/flips
util2.o:		common/grad2
util2.o:		common/ijlab
util2.o:		common/incrd
util2.o:		common/indez
util2.o:		common/infoa
util2.o:		common/iofile
util2.o:		common/junkcc
util2.o:		common/mapper
util2.o:		common/maxlen
util2.o:		common/misc
util2.o:		common/nshel
util2.o:		common/pkfil
util2.o:		common/restar
util2.o:		common/shlnos
util2.o:		common/shlt
util2.o:		common/sizes
util2.o:		common/sortp
util3.o:		common/atmblk
util3.o:		common/ccpdft.hf77
util3.o:		common/chmgms
util3.o:		common/czmat
util3.o:		common/discc
util3.o:		common/dump3
util3.o:		common/files
util3.o:		common/funct
util3.o:		common/infoa
util3.o:		common/iofile
util3.o:		common/iofile
util3.o:		common/mapper
util3.o:		common/nshel
util3.o:		common/parcntl
util3.o:		common/sector
util3.o:		common/sizes
util3.o:		common/statis
util3.o:		common/timeperiods
util3.o:		common/timez
util3.o:		common/work
util4.o:		common/cndx40
util4.o:		common/corctl
util4.o:		common/count
util4.o:		common/detcic
util4.o:		common/discc
util4.o:		common/disktl
util4.o:		common/dump3
util4.o:		common/filel
util4.o:		common/gjs
util4.o:		common/iofile
util4.o:		common/jobopt
util4.o:		common/machin
util4.o:		common/mapper
util4.o:		common/mcff
util4.o:		common/mcscra
util4.o:		common/multic
util4.o:		common/prints
util4.o:		common/prnprn
util4.o:		common/restar
util4.o:		common/sector
util4.o:		common/sizes
util4.o:		common/stak
util4.o:		common/syminf
util4.o:		common/work
util5.o:		common/cdcryi
util5.o:		common/cdcryz
util5.o:		common/cndx40
util5.o:		common/disktl
util5.o:		common/helpr
util5.o:		common/iofile
util5.o:		common/mapper
util5.o:		common/mlngth
util5.o:		common/presrt
util5.o:		common/prnprn
util5.o:		common/sizes
util5.o:		common/stak
util5.o:		common/symci
util6.o:		common/atmblk
util6.o:		common/cigrad
util6.o:		common/cndx41
util6.o:		common/common
util6.o:		common/ghfblk
util6.o:		common/infoa
util6.o:		common/iofile
util6.o:		common/prnprn
util6.o:		common/runlab
util6.o:		common/sizes
util7.o:		common/atmblk
util7.o:		common/cndx41
util7.o:		common/common
util7.o:		common/iofile
util7.o:		common/mapper
util7.o:		common/mpcut
util7.o:		common/sizes
util7.o:		common/stak
util7.o:		common/three
util7.o:		common/uhfspn
util8.o:		common/sizes
xcheck.o:		common/xcheck
xcheck.o:		common/xcheck
xerbla_lsame.o:		common/iofile
zora.o:		common/common
zora.o:		common/cslosc
zora.o:		common/czmat
zora.o:		common/dump3
zora.o:		common/infoa
zora.o:		common/infob
zora.o:		common/iofile
zora.o:		common/machin
zora.o:		common/mapper
zora.o:		common/nshel
zora.o:		common/restar
zora.o:		common/restri
zora.o:		common/restrl
zora.o:		common/runlab
zora.o:		common/scfopt
zora.o:		common/scra7
zora.o:		common/sizes
zora.o:		common/sortp
zora.o:		common/symtry
zora.o:		common/tran
zora.o:		common/transf
zora.o:		common/zorac
#
# ============================ NON-COMPILED TARGETS ===========================
#
clean:
	touch x.c x.o x~ x.f \#x
	rm -f *.c *.o *.f *~ \#* $(EXE) $(SCFEXE) 
	rm -f notes.stamp
	@(if test -d ../dft; then \
	cd ../dft; make clean; fi)
	@(if test -d ../drf; then \
	cd ../drf; make clean; fi)
	@(if test -d ../mopac; then \
	cd ../mopac; make clean; fi)
	@(if test -d ../newscf; then \
	cd ../newscf; make clean; fi)
	@(cd ../utilities; make clean)
#
realclean:
	make clean


depend:
	../utilities/makedeps
	@(if test -d ../dft; then \
	cd ../dft; make depend; fi)
	@(if test -d ../drf; then \
	cd ../drf; make depend; fi)
	@(if test -d ../mopac; then \
	cd ../mopac; make depend; fi)
	@(if test -d ../newscf; then \
	cd ../newscf; make depend; fi)
#
#  Machine-specific instructions will be printed only once as a reminder
#
notes:	
	@rm -f notes.stamp; make notes.stamp
notes.target:	notes.stamp
notes.stamp:
	@(if [ "X" != "X$(NOTES)" ] ; then \
	echo "Specific instructions for build with $(MACH) "; \
	echo $(NOTES); \
	echo "Type make notes to see this message again"; fi )
	@touch notes.stamp
#
scfsrc:	
	@make ` echo $(SCFOBJ) | sed 's/\.o/\.f/g' `
	@make c.c
#

src:
	@$(MAKE) ` echo $(OBJF) | sed 's/\.o/\.f/g' `
	@$(MAKE) ` echo $(OBJC) | sed 's/\.o/\.c/g' `
	@$(MAKE) makefile_f

makefile_f:
	cat ../machines/$(MACH) ../utilities/gener.m  makefile.m | $(M4) $(M4OPTS) > makefile_f
#
#
#
config:
	echo there is no config target - Use ./configure
