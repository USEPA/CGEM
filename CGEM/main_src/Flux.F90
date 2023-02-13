       Subroutine Flux(Which_code, TC_8, istep, myid, numprocs)

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
       integer, intent(in)  :: istep, myid, numprocs
       character(6), intent(in) :: Which_code

       if(Which_code.eq."CGEM") then !CGEM
         call Flux_CGEM(TC_8, istep, myid, numprocs)
       else if(Which_code.eq."WQEM") then !WQEM
         call Flux_WQEM(TC_8, istep, myid, numprocs)
       else
         write(6,*) "Model ",Which_code," not found, Exiting."
         stop
       endif

       return

       End Subroutine Flux
