      Module JWMod 

         save

         real, allocatable :: CBODS(:,:)

      contains

      Subroutine JWMod_allocate
 
      USE Model_dim
      
      IMPLICIT NONE

      allocate (CBODS(im,jm))

      END Subroutine JWMod_allocate

      END MODULE JWMod

