#
# Makefile to build the CGEM FishTank model
#
# Note:
# - These settings are for the *INTEL* compiler.
# - Ensure you load the appropriates modules before building (if applicable).

### =============== User Modifiable Section =============== ###

### Uncomment the next line to enable debugging
#DFLAGS = -warn -debug all -g -check all -ftrapuv -DDEBUG #-mcmodel=medium -shared-intel 
#DFLAGS = -Wall -Wextra -pedantic -fimplicit-none -fbacktrace -D_CGEM -DRDEBUG -DDEBUG 
#DFLAGS = -DDEBUG
#DFLAGS = -DDEBUGVARS -DDEBUG_Vars

### Build options for specific platforms. 
### LIBS has path to netCDF
SOL_INC	  = -I. -I/usr/local/apps/netcdf-4.3.3/intel-15.0/include/
SOL_LIBS  = -L/usr/local/apps/netcdf-4.3.3/intel-15.0/lib -lnetcdff -lnetcdf

IRIS_INC  = -I. -I/usr/local/apps/netcdf-4.4.0/gcc-4.4.7/include/
IRIS_LIBS = -L. -L/usr/local/apps/netcdf-4.4.0/gcc-4.4.7/lib/ -lnetcdff -lnetcdf

OTHER_INC   = -I. -I/usr/local/include
OTHER_LIBS  = -L/usr/local/lib -lnetcdff -L/usr/local/bin -lnetcdf 

### =============== End User Modifiable Section  =============== ####
include main_src/src_files
include moc_src/src_files
include sdm_src/src_files
include cgem_src/src_files
include gd_src/src_files

maindir=main_src
mocdir=moc_src
sdmdir=sdm_src
cgemdir=cgem_src
gddir=gd_src

### These lines should seldom change! ###
EXE	= FishTank.exe
MY_HOST	  = $(shell hostname)
ifeq (iris,$(findstring iris,$(MY_HOST)))
  INC	   = $(IRIS_INC)
  LIBS	   = $(IRIS_LIBS)
  F90      = gfortran #ifort
  FFLAGS = -ffree-line-length-none #-Warn all -traceback #-check -debug -g       #-g -fbacktrace -Wall -ffree-line-length-none
  FFLAGS_SDM = -O3 
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
  FFLAGS = -ffree-line-length-none 
endif
endif

FishTank: ${MAIN_OBJ} ${MOC_OBJ} ${CGEM_OBJ} ${SDM_OBJ} ${GD_OBJ}
	$(F90) -o $(EXE) $(FFLAGS) $(DFLAGS) $(INC) $(MAIN_OBJ) ${MOC_OBJ} ${CGEM_OBJ} ${SDM_OBJ} ${GD_OBJ} $(LIBS)


#
# Pattern rules
#

# These items must have compiler optimization disabled
$(NO_OPT_OBJS): %.o: %.F90
	$(F90) -c -O0 $(FFLAGS) $(INC) $(DFLAGS) $<

# No Implicit None flag
$(SDM_OBJ): %.o: $(sdmdir)/%.f
	$(F90) -c $(FFLAGS_SDM) $<

$(MAIN_OBJ):%.o: $(maindir)/%.F90
	$(F90) -c $(FFLAGS) $(DFLAGS)  $<

$(MOC_OBJ):%.o: $(mocdir)/%.F90
	$(F90) -c $(FFLAGS) $(DFLAGS)  $<

$(OBJ):%.o: %.F90
	$(F90) -c $(FFLAGS) $(DFLAGS) $<

$(CGEM_OBJ):%.o: $(cgemdir)/%.F90
	$(F90) -c $(FFLAGS) $(DFLAGS)   $<

$(GD_OBJ):%.o: $(gddir)/%.F90
	$(F90) -c $(FFLAGS) $(DFLAGS)  $<


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
