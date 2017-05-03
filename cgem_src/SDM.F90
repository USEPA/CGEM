Module SDM 

IMPLICIT NONE

SAVE

real, allocatable :: sedflux(:,:,:)
real*8, dimension(:,:,:), allocatable :: Y
 
contains

Subroutine SDM_allocate

USE Model_dim

IMPLICIT NONE

allocate (sedflux(im,jm,nf))
allocate (Y(im,jm,27000))
 
END Subroutine SDM_allocate

END MODULE SDM
