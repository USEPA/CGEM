      Subroutine JWMod_allocate
 
      USE JWMod 
      USE Model_dim
      
      IMPLICIT NONE

      allocate (CBODS(im,jm))

      END Subroutine JWMod_allocate
