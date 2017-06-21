Module SDM 

IMPLICIT NONE

SAVE

real, allocatable :: sedflux(:,:,:)
 
contains

Subroutine SDM_allocate

USE Model_dim

IMPLICIT NONE

allocate (sedflux(im,jm,nf))
 
END Subroutine SDM_allocate

END MODULE SDM
