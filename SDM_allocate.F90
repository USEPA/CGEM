Subroutine SDM_allocate

USE Model_dim
USE SDM

IMPLICIT NONE

allocate (sedflux(im,jm,nf))
allocate (Y(im,jm,27000))
 
END Subroutine SDM_allocate
