Module TEMP_VARS 

IMPLICIT NONE

SAVE

integer :: Which_temperature
real, dimension(:), allocatable :: KTg1, KTg2, Tref, Ea, N

contains

Subroutine TEMP_VARS_allocate

USE Model_dim, ONLY:nospA, nospZ
USE Fill_Value

IMPLICIT NONE

ALLOCATE (KTg1(nospA+nospZ)) 
ALLOCATE (KTg2(nospA+nospZ))
ALLOCATE (Tref(nospA+nospZ))
ALLOCATE (Ea(nospA+nospZ))
ALLOCATE (N(nospA+nospZ))

!Fill values for netCDF
KTg1 = fill(0)
KTg2 = fill(0)
Tref = fill(0)
Ea = fill(0)
N = fill(0)

return
End Subroutine TEMP_VARS_allocate

End Module TEMP_VARS
