Subroutine TEMP_VARS_allocate

USE TEMP_VARS 
USE Model_dim, ONLY:nospA, nospZ

IMPLICIT NONE

ALLOCATE (KTg1(nospA+nospZ)) 
ALLOCATE (KTg2(nospA+nospZ))
ALLOCATE (Tref(nospA+nospZ))
ALLOCATE (Ea_R(nospA+nospZ))

End Subroutine TEMP_VARS_allocate
