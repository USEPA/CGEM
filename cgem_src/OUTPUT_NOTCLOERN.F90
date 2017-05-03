Subroutine OUTPUT_NotCloern

  USE Model_dim
  USE OUTPUT 

  IMPLICIT NONE

  integer i,counter

  counter = 2 !irr, irr_frac
  counter = counter + 4*nospA !uN,uP,uE,uA,
  counter = counter + 9 !Chla + stoich
  counter = counter + nospA  !uSi

!True/False, do you want to write out Chla
  do i=1,nospA
   counter = counter + 1
   WRITE_EXTRA_VARIABLE(counter) = .FALSE.
  enddo

 return

END Subroutine OUTPUT_NotCloern
