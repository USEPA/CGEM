Module MASS_BALANCE_CGEM

IMPLICIT NONE

SAVE

real, dimension(:,:,:), allocatable :: RN2_ijk 
real, dimension(:,:,:), allocatable :: RO2_A_ijk
real, dimension(:,:,:), allocatable :: RO2_Z_ijk
real, dimension(:,:,:), allocatable :: RO2_R_ijk
real, dimension(:,:,:), allocatable :: RO2_BC_ijk


contains

Subroutine MASS_BALANCE_CGEM_allocate

USE Fill_Value
USE Model_dim

IMPLICIT NONE

ALLOCATE(RN2_ijk(im,jm,km))
RN2_ijk = fill(0)

ALLOCATE(RO2_A_ijk(im,jm,km))
RO2_A_ijk = fill(0)
ALLOCATE(RO2_Z_ijk(im,jm,km))
RO2_Z_ijk = fill(0)
ALLOCATE(RO2_R_ijk(im,jm,km))
RO2_R_ijk = fill(0)
ALLOCATE(RO2_BC_ijk(im,jm,km))
RO2_BC_ijk = fill(0)

return

END Subroutine MASS_BALANCE_CGEM_allocate

END MODULE MASS_BALANCE_CGEM

