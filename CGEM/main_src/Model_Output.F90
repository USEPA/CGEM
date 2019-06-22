       Subroutine Model_Output(Which_code, istep_out)

       USE STATE_VARS

       IMPLICIT NONE


       integer, intent(in)  :: istep_out
       character(6), intent(in) :: Which_code


       if(Which_code.eq."CGEM") then !CGEM
         call Model_Output_CGEM( istep_out )
       else if(Which_code.eq."GOMDOM") then !GOMDOM
         call Model_Output_GD( istep_out )
       else
         write(6,*) "Model ",Which_code," not found, Exiting."
         stop
       endif

       return

       End Subroutine Model_Output
