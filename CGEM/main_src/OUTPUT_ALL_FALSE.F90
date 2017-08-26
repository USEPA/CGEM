Subroutine OUTPUT_ALL_FALSE

  USE Model_dim
  USE OUTPUT

  IMPLICIT NONE

  integer i

!True/False, do you want to write out the state variables?
  do i=1,nf
   WRITE_VARIABLE(i) = .FALSE.
  enddo

!True/False, do you want to write out the extra variable?
  do i=1,EXTRA_VARIABLES
   WRITE_EXTRA_VARIABLE(i) = .FALSE.
  enddo

  return

END Subroutine OUTPUT_ALL_FALSE

