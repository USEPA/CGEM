       Subroutine Model_Output(Which_code,istep_out,myid,numprocs)

       IMPLICIT NONE


       integer, intent(in)  :: istep_out,myid,numprocs
       character(6), intent(in) :: Which_code


       if(Which_code.eq."CGEM") then !CGEM

         call Model_Output_CGEM( istep_out,myid,numprocs )

       else if(Which_code.eq."WQEM") then !WQEM

         call Model_Output_WQEM( istep_out,myid,numprocs )

       else

         write(6,*) "Model ",Which_code," not found, Exiting."
         stop

       endif

       return

       End Subroutine Model_Output
