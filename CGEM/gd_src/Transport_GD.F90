       Subroutine Transport_GD()

       use Model_dim, ONLY:which_gridio
       use INPUT_VARS, ONLY:Which_VMix

       IMPLICIT NONE

       !Advection and Vmixing
       if(which_gridio.ne.0) then  

        call Adv3D()
      
        if(Which_VMix.ne.0) call VMixing()

       endif

       return

       End Subroutine Transport_GD 
