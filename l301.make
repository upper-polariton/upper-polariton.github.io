UTIL_NAME = $(GAUSS_DIR)/util.a
GAULIB = util.a
FFLAGS =  -i8 '-mcmodel=medium'  -mp  -O2 -tp k8-64 -Mreentrant -Mrecursive -Mnosave -Minfo -Mneginfo -time -fast -Munroll -Mvect=assoc,recog,cachesize:1048576
LFLAGS = -mp -tp k8-64 -time -Munroll -Mvect=cachesize:524288 
LIBS = $(GAUSS_DIR)/bsd/libf77blas-amd64.a $(GAUSS_DIR)/bsd/libatlas-amd64.a -lpthread -lm -lc

RUNF77 = pgf77
.SUFFIXES:
.SUFFIXES: .o .F

.F.o:
        make -f $(GAUSS_DIR)/bsd/g03.make FFLAGS='$(FFLAGS)' $*.o

all: l301.exe

MAIN301 = ml301.o
NEWOBJ301 = solcnr.o

OBJL301 = $(NEWOBJ301)

l301.exe: $(MAIN301) $(OBJL301)
        $(RUNF77) $(FFLAGS) $(LFLAGS) -o l301.exe $(MAIN301) \
        $(OBJL301) $(GAUSS_DIR)/l301.a \
        $(GAUSS_DIR)/$(GAULIB) $(LIBS)

ml301.F:
        $(GAUSS_DIR)/bsd/gau-get $(@:.F=) main

