#
# Makefile to build the CGEM FishTank model
#
# Note:
# - These settings are for the *INTEL* compiler.
# - Ensure you load the appropriates modules before building (if applicable).

### =============== User Modifiable Section =============== ###

### Uncomment the next line to enable debugging
#DFLAGS = -DDEBUG
#DFLAGS = -g -warn -debug all -g -check all -ftrapuv#  -DDEBUG #-mcmodel=medium -shared-intel 
#DFLAGS = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -D_CGEM -DRDEBUG -DDEBUG 
#DFLAGS = -g

### Build options for specific platforms. 
### LIBS has path to netCDF
ATMOS_INC   = -I. -I/usr/local/apps/pnetcdf-1.9.0/intel-18.0/include -I/usr/local/apps/netcdf-4.6.3/intel-18.0/include/
ATMOS_LIBS  = -L. -L/usr/local/apps/pnetcdf-1.9.0/intel-18.0/lib -lpnetcdf -L/usr/local/apps/netcdf-4.6.3/intel-18.0/lib -lnetcdff -lnetcdf

IRIS_INC  = -I. -I/usr/local/apps/netcdf-4.4.0/gcc-4.4.7/include/
IRIS_LIBS = -L. -L/usr/local/apps/netcdf-4.4.0/gcc-4.4.7/lib/ -lnetcdff -lnetcdf -L/usr/local/apps/pnetcdf-1.6.1/intel-15.0/lib -lpnetcdf

OTHER_INC   = -I. -I/usr/local/include -I/usr/include/x86_64-linux-gnu/ -I/usr/include
OTHER_LIBS  = -L/usr/lib/x86_64-linux-gnu/ -lpnetcdf -L/usr/local/lib -lnetcdff -L/usr/local/bin -lnetcdf 

### =============== End User Modifiable Section  =============== ####
#include main_src/src_just_par
#include main_src/src_just_serial
include main_src/src_files_par
include moc_src/src_files
include sdm_src/src_files
include cgem_src/src_files
include wqem_src/src_files

maindir=main_src
mocdir=moc_src
sdmdir=sdm_src
cgemdir=cgem_src
wqemdir=wqem_src

### These lines should seldom change! ###
EXE	= CGEM 
MY_HOST	  = $(shell hostname)
ifeq (iris,$(findstring iris,$(MY_HOST)))
  INC	   = $(IRIS_INC)
  LIBS	   = $(IRIS_LIBS)
  F90      = mpiifort
  FFLAGS = -warn all -traceback -no-diag-error-limit -implicitnone -convert big_endian -ftz -D_MPI#-check -debug -g       #-g -fbacktrace -Wall 
else 
ifeq (atmos,$(findstring atmos,$(MY_HOST)))
  INC	   = $(ATMOS_INC)
  LIBS	   = $(ATMOS_LIBS)
  F90     = mpiifort 
  FFLAGS = -D_MPI
else
  INC      = $(OTHER_INC)
  LIBS     = $(OTHER_LIBS)
  F90    = mpif90 
  FFLAGS = -D_MPI
endif
endif

FishTank: ${PAR_OBJ} ${SER_OBJ} ${MAIN_OBJ} ${MOC_OBJ} ${CGEM_OBJ} ${SDM_OBJ} ${WQEM_OBJ}
	$(F90) -o $(EXE) $(FFLAGS) $(DFLAGS) $(INC) $(PAR_OBJ) $(SER_OBJ) $(MAIN_OBJ) ${MOC_OBJ} ${CGEM_OBJ} ${SDM_OBJ} ${WQEM_OBJ} $(LIBS)


#
# Pattern rules
#

# These items must have compiler optimization disabled
$(NO_OPT_OBJS): %.o: %.F90
	$(F90) -c -O0 $(FFLAGS) $(DFLAGS) $<

# No Implicit None flag
$(SDM_OBJ): %.o: $(sdmdir)/%.f
	$(F90) -c $(FFLAGS_SDM) $<

$(MAIN_OBJ):%.o: $(maindir)/%.F90
	$(F90) $(INC) -c $(FFLAGS) $(DFLAGS)  $<

$(MOC_OBJ):%.o: $(mocdir)/%.F90
	$(F90) -c $(FFLAGS) $(DFLAGS)  $<

$(OBJ):%.o: %.F90
	$(F90) -c $(FFLAGS) $(DFLAGS) $<

$(CGEM_OBJ):%.o: $(cgemdir)/%.F90
	$(F90) $(INC) -c $(FFLAGS) $(DFLAGS)   $<

$(WQEM_OBJ):%.o: $(wqemdir)/%.F90
	$(F90) -c $(FFLAGS) $(DFLAGS)  $<

$(PAR_OBJ):%.o: $(maindir)/%.F90
	$(F90) $(INC) -c  $<

$(SER_OBJ):%.o: $(maindir)/%.F90
	$(F90) $(INC) -c  $<

## Generic implicit rules
#%.o: %.F90 
#	$(F90) -c $(FFLAGS) $(DFLAGS)  $<	

#
# Miscellaneous targets
#

clean:
	rm -f *.o ${EXE} *.mod *genmod*

tags:
	ctags --language-force=Fortran *.F90

etags:
	etags -l fortran *.F90
