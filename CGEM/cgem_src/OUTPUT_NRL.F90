Subroutine OUTPUT_NRL

  USE Model_dim
  USE OUTPUT 
  USE CGEM_vars

  IMPLICIT NONE

  integer i,counter

!True/False, do you want to write out the state variables?
  do i=1,nf
   WRITE_VARIABLE(i) = .FALSE.
  enddo
!For NRL, only write O2
   WRITE_VARIABLE(iO2) = .TRUE.

!True/False, do you want to write out the extra variable?
  do i=1,EXTRA_VARIABLES
   WRITE_EXTRA_VARIABLE(i) = .FALSE.
  enddo

!Counter, figure out where is ChlA:
! 2, for irradiance, and irradiance_fraction
   counter = 2
   do i=1, 4*nospA
    counter = counter + 1  !uN, uP, uE, uA
   enddo
   WRITE_EXTRA_VARIABLE(counter+1) = .TRUE. !Here is Chla_mg_tot

END Subroutine OUTPUT_NRL

