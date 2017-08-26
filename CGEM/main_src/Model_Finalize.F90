       Subroutine Model_Finalize(Which_code)

       IMPLICIT NONE

       character(6), intent(in) :: Which_code

       if(Which_code.eq."CGEM") then !CGEM
         call Model_Finalize_CGEM()
       else if(Which_code.eq."GOMDOM") then !GOMDOM
         call Model_Finalize_GD()
       else
         write(6,*) "Model ",Which_code," not found, Exiting."
         stop
       endif

       return

       End Subroutine Model_Finalize
