       Subroutine Transport_GD()
!***********************************************************************
! Purpose: HMixing.F90  This subroutine calls the transport routines.
!
! Revised: 04/29/2020 Wilson Melendez,  Added call to HMixing. 
!***********************************************************************
       use Model_dim, ONLY: which_gridio
       use INPUT_VARS, ONLY: Which_VMix

       IMPLICIT NONE

       ! Advection, horizontal mixing, and vertical mixing.
       if(which_gridio .ne. 0) then  

          call Adv3D()
      
          if(Which_VMix.ne.0) call VMixing()

          call HMixing()

       endif

       return

       End Subroutine Transport_GD 
