       Subroutine WQ_Model(Which_code,TC_8,istep, istep_out)

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
       integer, intent(in)  :: istep, istep_out
       character(6), intent(in) :: Which_code

       if(Which_code.eq."CGEM") then !CGEM
         call CGEM( TC_8, istep, istep_out )
       else if(Which_code.eq."GOMDOM") then !GOMDOM
         call GOMDOM( TC_8 )
       else
         write(6,*) "Model ",Which_code," not found, Exiting."
         stop
       endif

       return

       End Subroutine WQ_Model
