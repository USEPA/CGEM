!******************************************************************************
! PURPOSE: OUTPUT_NETCDF.F90 - Routines for outputting NCOM_GEM data to a
!          NetCDF file.
! NOTES:   Non-ADT module.
! HISTORY: 2010/04/26, Todd Plessel, plessel.todd@epa.gov, Created.
!******************************************************************************

Subroutine OUTPUT_NETCDF_allocate

  USE Model_dim
  USE TEST_Mod 

  IMPLICIT NONE

  integer i,counter
  character*6 var
  character*19 var1
  character*100 var2

  ALLOCATE(VARIABLE_NAMES(nf))
   counter = 0
 if(nospA.le.9) then
  do i=1,nospA
    counter = counter + 1
    write(var,'(A1,i1)') 'A',i
    VARIABLE_NAMES(counter) = var
  enddo 
  do i=1,nospA
    counter = counter + 1
    write(var,'(A2,i1)') 'Qn',i
    VARIABLE_NAMES(counter) = var
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var,'(A2,i1)') 'Qp',i
    VARIABLE_NAMES(counter) = var
  enddo
 elseif(nospA.le.99) then
  do i=1,nospA
    counter = counter + 1
    write(var,'(A1,i2)') 'A',i
    VARIABLE_NAMES(counter) = var
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var,'(A2,i2)') 'Qn',i
    VARIABLE_NAMES(counter) = var
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var,'(A2,i2)') 'Qp',i
    VARIABLE_NAMES(counter) = var
  enddo
 else
  write(6,*) "No more than 99 phytoplankton species allowed in netCDF"
  stop
 endif
 if(nospZ.le.9) then
  do i=1,nospZ
    counter = counter + 1
    write(var,'(A1,i1)') 'Z',i
    VARIABLE_NAMES(counter) = var
  enddo
 elseif(nospZ.le.99) then
  do i=1,nospZ
    counter = counter + 1
    write(var,'(A1,i2)') 'Z',i
    VARIABLE_NAMES(counter) = var
  enddo
 else
  write(6,*) "No more than 99 phytoplankton species allowed in netCDF"
  stop
 endif
    VARIABLE_NAMES(counter+1) = 'NO3   '
    VARIABLE_NAMES(counter+2) = 'NH4   '
    VARIABLE_NAMES(counter+3) = 'PO4   '
    VARIABLE_NAMES(counter+4) = 'DIC   '
    VARIABLE_NAMES(counter+5) = 'O2    '
    VARIABLE_NAMES(counter+6) = 'OM1_A '
    VARIABLE_NAMES(counter+7) = 'OM2_A '
    VARIABLE_NAMES(counter+8) = 'OM1_Z '
    VARIABLE_NAMES(counter+9) = 'OM2_Z '
    VARIABLE_NAMES(counter+10) = 'OM1_R '
    VARIABLE_NAMES(counter+11) = 'OM2_R '
    VARIABLE_NAMES(counter+12) = 'CDOM  '
    VARIABLE_NAMES(counter+13) = 'Si    '
    VARIABLE_NAMES(counter+14) = 'OM1_BC'
    VARIABLE_NAMES(counter+15) = 'OM2_BC'
    VARIABLE_NAMES(counter+16) = 'ALK   '


  ALLOCATE( WRITE_VARIABLE(nf) ) 
  do i=1,nf
   WRITE_VARIABLE(i) = .TRUE.
  enddo

  ALLOCATE(VARIABLE_DESCRIPTIONS(nf))
   counter = 0
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A21,i3,A15)') 'Phytoplankton group ',i," number density"
    VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A22,i3,A20)') 'Phytoplankton group ',i," nitrogen quota."
    VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A22,i3,A20)') 'Phytoplankton group ',i," phosphorus quota." 
    VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
   do i=1,nospZ
    counter = counter + 1
    write(var2,'(A22,i3,A20)') 'Zooplankton group ',i," number density."
    VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
    VARIABLE_DESCRIPTIONS(counter+1) = 'Nitrate.  '
    VARIABLE_DESCRIPTIONS(counter+2) = 'Ammonium.                                                           '
    VARIABLE_DESCRIPTIONS(counter+3) = 'Phosphate.                                                          '
    VARIABLE_DESCRIPTIONS(counter+4) = 'Dissolved inorganic carbon.                                         '
    VARIABLE_DESCRIPTIONS(counter+5) = 'Molecular oxygen.                                                   '
    VARIABLE_DESCRIPTIONS(counter+6) = 'Particulate organic matter derived from dead algae.                 '
    VARIABLE_DESCRIPTIONS(counter+7) = 'Dissolved organic matter derived from dead algae.                   '
    VARIABLE_DESCRIPTIONS(counter+8) = 'Particulate organic matter derived from zooplankton fecal pellets.  '
    VARIABLE_DESCRIPTIONS(counter+9) = 'Dissolved organic matter derived from zooplankton fecal pellets.    '
    VARIABLE_DESCRIPTIONS(counter+10) = 'Particulate organic matter derived from river outflow.              '
    VARIABLE_DESCRIPTIONS(counter+11) = 'Dissolved organic matter derived from river outflow.                '
    VARIABLE_DESCRIPTIONS(counter+12) = 'Colored dissolved organic matter.                                   '
    VARIABLE_DESCRIPTIONS(counter+13) = 'Silica.                                                             '
    VARIABLE_DESCRIPTIONS(counter+14) = 'Particulate organic matter in the initial and boundary conditions '
    VARIABLE_DESCRIPTIONS(counter+15) = 'Dissolved organic matter in the initial and boundary conditions '
    VARIABLE_DESCRIPTIONS(counter+16) = 'Alkalinity.                                                         '

  ALLOCATE(VARIABLE_UNITS(nf))
  counter = 0
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A8)') "cells/m3" 
    VARIABLE_UNITS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A11)') "mmol-N/cell"
    VARIABLE_UNITS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A11)') "mmol-P/cell"
    VARIABLE_UNITS(counter) = var2
  enddo
  do i=1,nospZ
    counter = counter + 1
    write(var2,'(A12)') "organisms/m3"
    VARIABLE_UNITS(counter) = var2
  enddo
    VARIABLE_UNITS(counter+1) = 'mmol-N/m3                       '
    VARIABLE_UNITS(counter+2) = 'mmol-N/m3                       '
    VARIABLE_UNITS(counter+3) = 'mmol-P/m3                       '
    VARIABLE_UNITS(counter+4) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+5) = 'mmol-O2/m3                      '
    VARIABLE_UNITS(counter+6) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+7) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+8) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+9) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+10) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+11) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+12) = 'ppb                             '
    VARIABLE_UNITS(counter+13) = 'mmol-Si/m3                      '
    VARIABLE_UNITS(counter+14) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+15) = 'mmol-C/m3                       '
    VARIABLE_UNITS(counter+16) = 'mmol/m3                         '


  ALLOCATE(EXTRA_VARIABLE_NAMES(EXTRA_VARIABLES))
    EXTRA_VARIABLE_NAMES(1) = 'irradiance'
    EXTRA_VARIABLE_NAMES(2) = 'irradiance_fraction'
   counter = 2
 if(nospA.le.9) then
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i1)') 'uN',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i1)') 'uP',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i1)') 'uE',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i1)') 'uA',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
 elseif(nospA.le.99) then
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i2)') 'uN',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i2)') 'uP',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i2)') 'uE',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A2,i2)') 'uA',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
 endif
    EXTRA_VARIABLE_NAMES(counter+1) = 'Chla_mg_tot'
    EXTRA_VARIABLE_NAMES(counter+2) = 's_x1A' 
    EXTRA_VARIABLE_NAMES(counter+3) = 's_y1A' 
    EXTRA_VARIABLE_NAMES(counter+4) = 's_x2A' 
    EXTRA_VARIABLE_NAMES(counter+5) = 's_y2A' 
    EXTRA_VARIABLE_NAMES(counter+6) = 's_x1Z' 
    EXTRA_VARIABLE_NAMES(counter+7) = 's_y1Z' 
    EXTRA_VARIABLE_NAMES(counter+8) = 's_x2Z' 
    EXTRA_VARIABLE_NAMES(counter+9) = 's_y2Z'
    counter = counter + 9 
 if(nospA.le.9) then
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A3,i1)') 'uSi',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A5,i1)') 'Chl_C',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
 elseif(nospA.le.99) then
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A3,i2)') 'uSi',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var1,'(A5,i2)') 'Chl_C',i
    EXTRA_VARIABLE_NAMES(counter) = var1
  enddo
 endif 
    EXTRA_VARIABLE_NAMES(counter+1) = 'pH'  
    EXTRA_VARIABLE_NAMES(counter+2) = 'RNO3' 

  ALLOCATE(WRITE_EXTRA_VARIABLE(EXTRA_VARIABLES)) 
  do i=1,EXTRA_VARIABLES
   WRITE_EXTRA_VARIABLE(i) = .TRUE.
  enddo

  ALLOCATE(EXTRA_VARIABLE_DESCRIPTIONS(EXTRA_VARIABLES))
    EXTRA_VARIABLE_DESCRIPTIONS(1) = 'Irradiance at depth.  ' 
    EXTRA_VARIABLE_DESCRIPTIONS(2) = 'Fraction of surface irradiance                                      '
    counter = 2
 if(nospA.le.9) then
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A36,i1)') 'nitrogen-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A38,i1)') 'phosphorus-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A33,i1)') 'light-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A26,i1)') 'specific growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
 else
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A36,i2)') 'nitrogen-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A38,i2)') 'phosphorus-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A33,i2)') 'light-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A26,i2)') 'specific growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
endif
    EXTRA_VARIABLE_DESCRIPTIONS(counter+1) = 'Total Chla from all phytoplankton ' 
    EXTRA_VARIABLE_DESCRIPTIONS(counter+2) = 'Stoichiometry C:P for OM1_A                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+3) = 'Stoichiometry N:P for OM1_A                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+4) = 'Stoichiometry C:P for OM2_A                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+5) = 'Stoichiometry N:P for OM2_A                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+6) = 'Stoichiometry C:P for OM1_Z                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+7) = 'Stoichiometry N:P for OM1_Z                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+8) = 'Stoichiometry C:P for OM2_Z                                         '
    EXTRA_VARIABLE_DESCRIPTIONS(counter+9) = 'Stoichiometry N:P for OM2_Z                                         '
  counter = counter+9
 if(nospA.le.9) then
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A34,i1)') 'silica-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A11,i1)') 'Chl:c for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
 else
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A34,i2)') 'silica-dependent growth rate for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
  do i=1,nospA
    counter = counter + 1
    write(var2,'(A11,i2)') 'Chl:c for A',i
    EXTRA_VARIABLE_DESCRIPTIONS(counter) = var2
  enddo
endif
    EXTRA_VARIABLE_DESCRIPTIONS(counter+1) = 'pH                            '  
    EXTRA_VARIABLE_DESCRIPTIONS(counter+2) = 'RN2 Denitrification Term       '

  ALLOCATE(EXTRA_VARIABLE_UNITS(EXTRA_VARIABLES))
    EXTRA_VARIABLE_UNITS(1) = 'photons/cm2/s                   '
    EXTRA_VARIABLE_UNITS(2) = '%                               '
  counter = 2
  do i=1,nospA
    counter = counter + 1
    EXTRA_VARIABLE_UNITS(counter) = 'd-1' 
  enddo
  do i=1,nospA
    counter = counter + 1
    EXTRA_VARIABLE_UNITS(counter) = 'd-1'
  enddo
  do i=1,nospA
    counter = counter + 1
    EXTRA_VARIABLE_UNITS(counter) = 'd-1'
  enddo
  do i=1,nospA
    counter = counter + 1
    EXTRA_VARIABLE_UNITS(counter) = 'd-1'
  enddo
    EXTRA_VARIABLE_UNITS(counter+1) = 'mg-Chla/m3                      ' 
    EXTRA_VARIABLE_UNITS(counter+2) = 'C:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+3) = 'N:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+4) = 'C:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+5) = 'N:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+6) = 'C:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+7) = 'N:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+8) = 'C:P (mmol/mmol)                 '
    EXTRA_VARIABLE_UNITS(counter+9) = 'N:P (mmol/mmol)                 '
    counter = counter+9
  do i=1,nospA
    counter = counter + 1
    EXTRA_VARIABLE_UNITS(counter) = 'd-1'
  enddo
  do i=1,nospA
    counter = counter + 1
    EXTRA_VARIABLE_UNITS(counter) = 'mg-Chla/mg-C'
  enddo
    EXTRA_VARIABLE_UNITS(counter+1) = 's.u.                            '  
    EXTRA_VARIABLE_UNITS(counter+2) = 'mmol-N/m3                       '

  ALLOCATE(F_VAR(nf)) ! NetCDF IDs for each variable.
  ALLOCATE(EXTRA_VAR(EXTRA_VARIABLES))  ! NetCDF IDs for extra vars.

END Subroutine OUTPUT_NETCDF_allocate


