#
# Makefile to build the CGEM FishTank model
#
# Note:
# - These settings are for the *INTEL* compiler.
# - Ensure you load the appropriates modules before building (if applicable).

### =============== User Modifiable Section =============== ###

### Uncomment the next line to enable debugging
#DFLAGS = -warn -debug all -g -check all -ftrapuv  #-DDEBUG -mcmodel=medium -shared-intel 

### Build options for specific platforms. 
### LIBS has path to pnetCDF
SOL_INC	  = -I. -I/usr/local/apps/netcdf-4.3.3/intel-15.0/include/
SOL_LIBS  = -L/usr/local/apps/netcdf-4.3.3/intel-15.0/lib -lnetcdf -lnetcdff

IRIS_INC  = -I. -I/usr/local/apps/netcdf-4.4.0/gcc-4.4.7/include/
IRIS_LIBS = -L. -L/usr/local/apps/netcdf-4.4.0/gcc-4.4.7/lib/ -lnetcdf -lnetcdff

OTHER_INC   = -I. -I/usr/local/include
OTHER_LIBS  = -L/usr/local/lib -lnetcdff -L/usr/local/bin -lnetcdf 

### =============== End User Modifiable Section  =============== ####
include cgem_source

### These lines should seldom change! ###
EXE	= FishTank.exe
MY_HOST	  = $(shell hostname)
ifeq (iris,$(findstring iris,$(MY_HOST)))
  INC	   = $(IRIS_INC)
  LIBS	   = $(IRIS_LIBS)
  F90      = gfortran #ifort
  FFLAGS = #-Warn all -traceback #-check -debug -g       #-g -fbacktrace -Wall 
else 
ifeq (sol,$(findstring sol,$(MY_HOST)))
  INC	   = $(SOL_INC)
  LIBS	   = $(SOL_LIBS)
  F90     = gfortran 
  FFLAGS =  
else
  INC      = $(OTHER_INC)
  LIBS     = $(OTHER_LIBS)
  F90     = gfortran 
  FFLAGS = -lgfortran 
endif
endif

FishTank: ${OBJ} ${SDM_OBJS}
	$(F90) -o $(EXE) $(FFLAGS) $(DFLAGS) $(OBJ) ${SDM_OBJS} $(LIBS) $(INC)


#
# Pattern rules
#

# These items must have compiler optimization disabled
$(NO_OPT_OBJS): %.o: %.F90
	$(F90) -c -O0 $(FFLAGS) $(INC) $(DFLAGS) $<

# No Implicit None flag
$(SDM_OBJS): %.o: %.f
	$(F90) -c $(FFLAGS_SDM) $(DFLAGS)  $<

# Generic implicit rules
%.o: %.F90 
	$(F90) -c $(FFLAGS) $(DFLAGS)  $<	

#
# Miscellaneous targets
#

clean:
	rm -f *.o ${EXE} *.mod *genmod*

tags:
	ctags --language-force=Fortran *.F90

etags:
	etags -l fortran *.F90
