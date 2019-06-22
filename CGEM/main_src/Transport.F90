       Subroutine Transport(Which_code)

       IMPLICIT NONE

       character(6), intent(in) :: Which_code

       !Advection and Vmixing
       if(Which_code.eq."CGEM") then !CGEM
          call Transport_CGEM()
       else if(Which_code.eq."GOMDOM") then !GOMDOM
         call Transport_GD()
       else
         write(6,*) "Model ",Which_code," not found, Exiting."
         stop
       endif

       return

       End Subroutine Transport 
