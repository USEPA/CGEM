#
# Makefile to build the CGEM FishTank model
#
# Note:
# - Ensure you load the appropriates modules before building (if applicable).

### =============== User Modifiable Section =============== ###

###    === Possible options to set for DFLAGS below for debugging ===   ###
# Intel compiler options related to debugging and compiler output
# -g -debug -warn all -check all -traceback -no-diag-error-limit
# gfortran compiler options related to debugging and compiler output
# -g -Wall -Wextra -pedantic -fimplicit-none
# Not sure for what compiler
#  -ftrapuv
# Conditional compilation macros that control the code's debug output
# -DDEBUG
# -DDEBUGVARS
# -DDEBUG_A
# -DDEBUG_ADDBLIP
# -DDEBUG_CWS
# -DDEBUG_GRID
# -DDEBUG_LIGHT
# -DDEBUG_TIME
# -DDEBUG_V
# -DDEBUG_VMIX
# -DCALIBRATE
# -DCAL_DK
# -DCAL_LT
# -DCAL_LTNT
# -DCAL_O2
# -DCAL_OUTPUT
# -DCAL_PHYTO
# -DCAL_ZOO
# -Dmap_code
#DFLAGS = -DDEBUG

#Compiler can be INTEL or GNU
COMPILER = GNU   

### Build options for specific platforms. 
### LIBS has path to netCDF

ifeq (INTEL,$(strip $(COMPILER)))
#INTEL
  ATMOS_INC	  = -I. -I/usr/local/apps/netcdf-4.6.3/intel-18.0/include/
  ATMOS_LIBS  = -L/usr/local/apps/netcdf-4.6.3/intel-18.0/lib -lnetcdff -lnetcdf

  IRIS_INC  = -I. -I/usr/local/apps/netcdf-4.4.1/intel-17.0/include/
  IRIS_LIBS = -L. -L/usr/local/apps/netcdf-4.4.1/intel-17.0/lib/ -lnetcdff -lnetcdf

  OTHER_INC   = -I. -I/usr/local/include -I/usr/include/x86_64-linux-gnu/ -I/usr/include
  #OTHER_LIBS  = -L. -L/usr/local/lib -lnetcdff -L/usr/local/bin -lnetcdf 
  OTHER_LIBS  = -L. -L/usr/lib/x86_64-linux-gnu -lnetcdff -lnetcdf

  F90    = ifort
  FFLAGS =
# Misc intel flags that have been previously used
# -mcmodel=medium -shared-intel -implicitnone -convert big_endian -ftz 
else
ifeq (GNU,$(strip $(COMPILER)))
#GNU
  ATMOS_INC	  = -I. -I/usr/local/apps/netcdf-4.6.3/gcc-6.1.0/include/
  ATMOS_LIBS  = -L/usr/local/apps/netcdf-4.6.3/gcc-6.1.0/lib -lnetcdff -lnetcdf

  IRIS_INC  = -I. -I/usr/local/apps/netcdf-4.4.1/gcc-4.4.7/include/
  IRIS_LIBS = -L. -L/usr/local/apps/netcdf-4.4.1/gcc-4.4.7/lib/ -lnetcdff -lnetcdf

  OTHER_INC   = -I. -I/usr/local/include -I/usr/include/x86_64-linux-gnu/ -I/usr/include
  #OTHER_LIBS  = -L. -L/usr/local/lib -lnetcdff -L/usr/local/bin -lnetcdf 
  OTHER_LIBS  = -L. -L/usr/lib/x86_64-linux-gnu -lnetcdff -lnetcdf

  F90    = gfortran
  FFLAGS = -ffree-line-length-none
# Misc gcc flags that have been previously used
# 
else
$(error INVALID COMPILER TYPE)
endif
endif

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
EXE	= CGEM 
MY_HOST	  = $(shell hostname)
ifeq (iris,$(findstring iris,$(MY_HOST)))
  INC	   = $(IRIS_INC)
  LIBS	   = $(IRIS_LIBS)
else 
  ifeq (atmos,$(findstring atmos,$(MY_HOST)))
    INC	= $(ATMOS_INC)
    LIBS = $(ATMOS_LIBS)
  else
    INC = $(OTHER_INC)
    LIBS = $(OTHER_LIBS)
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
	(cd $(maindir); ctags --language-force=Fortran *.F90)
	(cd $(mocdir);  ctags --language-force=Fortran *.F90)
	(cd $(sdmdir);  ctags --language-force=Fortran *.f)
	(cd $(cgemdir); ctags --language-force=Fortran *.F90)
	(cd $(gddir);   ctags --language-force=Fortran *.F90)

etags:
	(cd $(maindir); etags -l fortran *.F90)
	(cd $(mocdir);  etags -l fortran *.F90)
	(cd $(sdmdir);  etags -l fortran *.f)
	(cd $(cgemdir); etags -l fortran *.F90)
	(cd $(gddir);   etags -l fortran *.F90)

