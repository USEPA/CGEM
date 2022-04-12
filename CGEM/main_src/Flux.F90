       Subroutine Flux(Which_code, istep, myid, numprocs)

       IMPLICIT NONE

       integer, intent(in)  :: istep, myid, numprocs
       character(6), intent(in) :: Which_code

       if(Which_code.eq."CGEM") then !CGEM
         call Flux_CGEM(istep, myid, numprocs)
       else if(Which_code.eq."GOMDOM") then !GOMDOM
         call Flux_GD()
       else
         write(6,*) "Model ",Which_code," not found, Exiting."
         stop
       endif

       return

       End Subroutine Flux
