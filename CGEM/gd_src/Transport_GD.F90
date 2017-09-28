       Subroutine Transport_GD()

       use Model_dim, ONLY:which_gridio
       use INPUT_VARS, ONLY:Which_VMix

       IMPLICIT NONE

       !Advection and Vmixing
       if(which_gridio.ne.0) then  

       if(Which_gridio.eq.2) then
        call Adv3D_EFDC()
       else
        call Adv3D()
       endif
      
        if(Which_VMix.ne.0) call VMixing()
       endif

       return

       End Subroutine Transport_GD 
