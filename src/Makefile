FFLAGS = -O

#**** files common to several of the programs ****
SAF_ION_DET_OBJ =  dsubs.o inside.o vector.o dsetup.o

SAF_ION_OBJ = traj.o flshso.o hameq.o scat.o potential.o tsubs.o safio.o \
              dtect.o dtput.o corrinit.o corrspl.o corrcalc.o

SAF_ION_INL_OBJ = traj.o flshso.o hameqinel.o scat.o potential.o tsubs.o safioinel.o \
              dtect.o dtput.o corrinit.o corrspl.o corrcalc.o

ION_DET_OBJ = get.o

#**** lists of objects for each program ****
SAFARI_OBJECTS = safari.o safsubs.o $(SAF_ION_OBJ) $(SAF_ION_DET_OBJ)

SAFARI_NEUT_OBJECTS = safari_neut.o safsubs_neut.o $(SAF_ION_OBJ) $(SAF_ION_DET_OBJ)

SAFARINEL_OBJECTS = safari.o safsubs.o $(SAF_ION_INL_OBJ) $(SAF_ION_DET_OBJ)

ION_OBJECTS = ion.o $(SAF_ION_OBJ) $(ION_DET_OBJ) $(SAF_ION_DET_OBJ)

IONINEL_OBJECTS = ion.o $(SAF_ION_INL_OBJ) $(ION_DET_OBJ) $(SAF_ION_DET_OBJ)

DETECT_OBJECTS = analysis.o spct.o dchang.o rdata.o impact.o disp.o glean.o \
                 $(ION_DET_OBJ) $(SAF_ION_DET_OBJ)

#**** the actual rules ****
Safari+: $(SAFARI_OBJECTS); ifort $(SAFARI_OBJECTS) -o Safari+

Safari_neut+: $(SAFARI_NEUT_OBJECTS); f95 $(SAFARI_NEUT_OBJECTS) -o Safari_neut+

Safarinel+: $(SAFARINEL_OBJECTS); xlf $(SAFARINEL_OBJECTS) -o Safarinel+

Ion+: $(ION_OBJECTS); ifort $(ION_OBJECTS) -o Ion+

Ioninel+: $(IONINEL_OBJECTS); xlf $(IONINEL_OBJECTS) -o Ioninel+

Detect+: $(DETECT_OBJECTS); ifort $(DETECT_OBJECTS) -o Detect+

flshso.o: flshso.c; cc -c flshso.c

#**** rules for making a safari library ****
SAFLIB = libsafari.a
SAFLIBOBJS = $(SAF_ION_DET_OBJ) $(SAF_ION_OBJ) $(ION_DET_OBJ) \
		$(DETECT_OBJECTS) safsubs.o

lib: $(SAFLIB)

libsafari.a: $(SAFLIBOBJS)
	/bin/rm -f $(SAFLIB)
	ar qv $(SAFLIB) $(SAFLIBOBJS)

install: $(SAFLIB)
	/bin/cp $(SAFLIB) ../lib

