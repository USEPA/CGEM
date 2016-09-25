Subroutine INPUT_VARS_GD_allocate

USE INPUT_VARS_GD 
USE Model_dim, ONLY : nf

IMPLICIT NONE

!----Sinking Terms----------------------------------
ALLOCATE( ws(nf) ) 

END Subroutine INPUT_VARS_GD_allocate
