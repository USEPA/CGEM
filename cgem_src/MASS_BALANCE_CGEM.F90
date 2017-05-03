Module MASS_BALANCE_CGEM

IMPLICIT NONE

SAVE

real, dimension(:,:,:), allocatable :: RN2_ijk 

contains

Subroutine MASS_BALANCE_CGEM_allocate

USE Fill_Value
USE Model_dim

IMPLICIT NONE

ALLOCATE(RN2_ijk(im,jm,nsl))
RN2_ijk = fill(0)

return

END Subroutine MASS_BALANCE_CGEM_allocate

END MODULE MASS_BALANCE_CGEM

