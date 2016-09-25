!******************************************************************************
! PURPOSE: OUTPUT_NETCDF.F90 - Routines for outputting NCOM_GEM data to a
!          NetCDF file.
! NOTES:   Non-ADT module.
! HISTORY: 2010/04/26, Todd Plessel, plessel.todd@epa.gov, Created.
!******************************************************************************
Subroutine OUTPUT_NETCDF_GD_allocate

  USE Model_dim
  USE TEST_Mod_GD

  IMPLICIT NONE

  integer i

  ALLOCATE(VARIABLE_NAMES(nf))
  VARIABLE_NAMES(1) = "DOC"
  VARIABLE_NAMES(2) = "DIA"
  VARIABLE_NAMES(3) = "GRE"
  VARIABLE_NAMES(4) = "ZOO"
  VARIABLE_NAMES(5) = "LOC"
  VARIABLE_NAMES(6) = "ROC"
  VARIABLE_NAMES(7) = "SRP"
  VARIABLE_NAMES(8) = "DOP"
  VARIABLE_NAMES(9) = "LOP"
  VARIABLE_NAMES(10) = "ROP"
  VARIABLE_NAMES(11) = "NH4"
  VARIABLE_NAMES(12) = "NO3"
  VARIABLE_NAMES(13) = "DON"
  VARIABLE_NAMES(14) = "LON"
  VARIABLE_NAMES(15) = "RON"
  VARIABLE_NAMES(16) = "SA"
  VARIABLE_NAMES(17) = "SU"
  VARIABLE_NAMES(18) = "DO2"
  VARIABLE_NAMES(19) = "TR"

  ALLOCATE( WRITE_VARIABLE(nf) ) 
  do i=1,nf
   WRITE_VARIABLE(i) = .TRUE.
  enddo

  ALLOCATE(VARIABLE_DESCRIPTIONS(nf))
  VARIABLE_DESCRIPTIONS(1) = "Dissolved organic Carbon."
  VARIABLE_DESCRIPTIONS(2) = "Diatom."
  VARIABLE_DESCRIPTIONS(3) = "Algae excluding diatoms."
  VARIABLE_DESCRIPTIONS(4) = "Zooplankton."
  VARIABLE_DESCRIPTIONS(5) = "Labile particulate organic carbon."
  VARIABLE_DESCRIPTIONS(6) = "Refractory particulate organic carbon."
  VARIABLE_DESCRIPTIONS(7) = "Soluble reactive phosphorous."
  VARIABLE_DESCRIPTIONS(8) = "Dissolved organic phosphorous."
  VARIABLE_DESCRIPTIONS(9) = "Labile particulate organic phosphorous."
  VARIABLE_DESCRIPTIONS(10) = "Refractory particulate organic phosphorous."
  VARIABLE_DESCRIPTIONS(11) = "Ammonia."
  VARIABLE_DESCRIPTIONS(12) = "Nitrate plus nitrite nitrogen."
  VARIABLE_DESCRIPTIONS(13) = "Dissolved organic nitrogen."
  VARIABLE_DESCRIPTIONS(14) = "Labile particulate organic nitrogen."
  VARIABLE_DESCRIPTIONS(15) = "Refractory particulate organic nitrogen."
  VARIABLE_DESCRIPTIONS(16) = "Available silica."
  VARIABLE_DESCRIPTIONS(17) = "Unavailable silica."
  VARIABLE_DESCRIPTIONS(18) = "Dissolved oxygen."
  VARIABLE_DESCRIPTIONS(19) = "Tracer."

  ALLOCATE(VARIABLE_UNITS(nf))
  VARIABLE_UNITS(1) = "C kg/m3"
  VARIABLE_UNITS(2) = "C kg/m3"
  VARIABLE_UNITS(3) = "C kg/m3"
  VARIABLE_UNITS(4) = "C kg/m3"
  VARIABLE_UNITS(5) = "C kg/m3"
  VARIABLE_UNITS(6) = "C kg/m3"
  VARIABLE_UNITS(7) = "P kg/m3"
  VARIABLE_UNITS(8) = "P kg/m3"
  VARIABLE_UNITS(9) = "P kg/m3"
  VARIABLE_UNITS(10) = "P kg/m3"
  VARIABLE_UNITS(11) = "N kg/m3"
  VARIABLE_UNITS(12) = "N kg/m3"
  VARIABLE_UNITS(13) = "N kg/m3"
  VARIABLE_UNITS(14) = "N kg/m3"
  VARIABLE_UNITS(15) = "N kg/m3"
  VARIABLE_UNITS(16) = "Si kg/m3"
  VARIABLE_UNITS(17) = "Si kg/m3"
  VARIABLE_UNITS(18) = "O2 kg/m3"
  VARIABLE_UNITS(19) = "kg/m3"


  ALLOCATE(EXTRA_VARIABLE_NAMES(EXTRA_VARIABLES))
  EXTRA_VARIABLE_NAMES(1) = "SUM_DENITR"
  EXTRA_VARIABLE_NAMES(2) = "SUM_DENITR_C"
  EXTRA_VARIABLE_NAMES(3) = "SUM_DOCPRD"
  EXTRA_VARIABLE_NAMES(4) = "SUM_DOCMET"
  EXTRA_VARIABLE_NAMES(5) = "SUM_DOCZOO"

  ALLOCATE(WRITE_EXTRA_VARIABLE(EXTRA_VARIABLES)) 
  do i=1,EXTRA_VARIABLES
   WRITE_EXTRA_VARIABLE(i) = .TRUE.
  enddo

  ALLOCATE(EXTRA_VARIABLE_DESCRIPTIONS(EXTRA_VARIABLES))
  EXTRA_VARIABLE_DESCRIPTIONS(1) = "Denitrification N" 
  EXTRA_VARIABLE_DESCRIPTIONS(2) = "Denitrification C"
  EXTRA_VARIABLE_DESCRIPTIONS(3) = "Carbon loss due to predation"
  EXTRA_VARIABLE_DESCRIPTIONS(4) = "Carbon loss due to metabolism"
  EXTRA_VARIABLE_DESCRIPTIONS(5) = "Carbon loss due to zooplankton mortality"


  ALLOCATE(EXTRA_VARIABLE_UNITS(EXTRA_VARIABLES))
  EXTRA_VARIABLE_UNITS(1) = "kg/m3 N" 
  EXTRA_VARIABLE_UNITS(2) = "kg/m3 C"
  EXTRA_VARIABLE_UNITS(3) = "kg/m3 C"
  EXTRA_VARIABLE_UNITS(4) = "kg/m3 C"
  EXTRA_VARIABLE_UNITS(5) = "kg/m3 C"


  ALLOCATE(F_VAR(nf)) ! NetCDF IDs for each variable.
  ALLOCATE(EXTRA_VAR(EXTRA_VARIABLES))  ! NetCDF IDs for extra vars.

END Subroutine OUTPUT_NETCDF_GD_allocate
