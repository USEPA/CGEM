Subroutine MASS_BALANCE_GD_allocate

USE Model_dim
USE MASS_BALANCE_GD

IMPLICIT NONE

ALLOCATE(SUM_DENITR(im,jm,nsl))    ! N loss to denitrification 
ALLOCATE(SUM_DENITR_C(im,jm,nsl))  ! C loss to denitrification 
ALLOCATE(SUM_DOCPRD(im,jm,nsl))    ! C loss to predation 
ALLOCATE(SUM_DOCMET(im,jm,nsl))    ! C loss to metabolism 
ALLOCATE(SUM_DOCZOO(im,jm,nsl))    ! C loss to zooplankton mortality 

SUM_DENITR = 0.
SUM_DENITR_C = 0.
SUM_DOCPRD = 0.
SUM_DOCMET = 0.
SUM_DOCZOO = 0.

END Subroutine MASS_BALANCE_GD_allocate
