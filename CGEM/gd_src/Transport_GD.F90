       Subroutine Transport_GD()

       IMPLICIT NONE

       !Advection and Vmixing

       call Adv3D()

       call VMixing()

       return

       End Subroutine Transport_GD 
