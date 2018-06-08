       Subroutine Transport(Which_code,myid,numprocs)

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs
       character(6), intent(in) :: Which_code

       !Advection and Vmixing
       if(Which_code.eq."CGEM") then !CGEM

          call Transport_CGEM(myid,numprocs)

       else if(Which_code.eq."GOMDOM") then !GOMDOM

         call Transport_GD(myid,numprocs)

       else

         write(6,*) "Model ",Which_code," not found, Exiting."
         stop

       endif

       return

       End Subroutine Transport 
