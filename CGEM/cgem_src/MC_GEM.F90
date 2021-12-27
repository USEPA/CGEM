!---------------------------------------------------------------------------
!  SUBROUTINE MC_GEM(f, fm, PrimProd, WC_O2, FPOM, FO2,  &
!     &     FNO3, FNH4, FPO4, outstep, istep, my_imstart, my_imend, my_im  ) 
  SUBROUTINE MC_GEM(f, fm, PrimProd, WC_O2, FPOM, FO2,  &
     &     FNO3, FNH4, FPO4, outstep, istep, my_im  )   
!---------------------------------------------------------------------------
!Assume this is only called in 3D

  USE Model_dim
!  USE Grid
!  USE MPI_dim
!  USE STATES
  USE INPUT_VARS, ONLY:dT  
  USE  CGEM_vars  
  USE Model_Compare

  !--------------------------------------------------------------------------
  ! model inter-comparison study through the Coastal Ocean Modeling Testbed
  ! (http://testbed.sura.org/) 
  !------------------------------------------------------------------------
    IMPLICIT NONE

    real, intent(in) :: f(myimp2,jm,nsl,nf)
    real, dimension (my_im,jm), intent(in) :: PrimProd, WC_O2, FO2
    real, dimension (my_im,jm), intent(in) :: FNO3, FNH4, FPO4, FPOM 
    integer, intent(in) :: fm(im,jm,km)
!    integer, intent(in) :: outstep, istep, my_imstart, my_imend, my_im
    integer, intent(in) :: outstep, istep
    real :: A_N(my_im,jm,km)
    integer :: i, j, k, myi, isp, my_im

    my_im = myi_end - myi_start + 1 
    A_N = 0.
 
    do j = 1,jm
     myi = 2 
!     do i = my_imstart,my_imend
     do i = myi_start, myi_end
       if(fm(i,j,1).eq.1) then
        do k = 1, km
         do isp = 1, nospA
            A_N(myi-1,j,k) = A_N(myi-1,j,k) + f(myi,j,k,iA(isp))*f(myi,j,k,iQn(isp))
         enddo
        enddo
       endif
       myi = myi + 1
      enddo 
    enddo

!    call WRITE_GEM_MC( my_imstart, my_im, 1, jm, 1, nz, outstep, istep, real(dT), &
    call WRITE_GEM_MC( myi_start, my_im, 1, jm, 1, km, outstep, istep, real(dT), &
     &             f(2:my_im+1,1:jm,1:km,iO2),     &
     &             f(2:my_im+1,1:jm,1:km,iNO3),    &
     &             f(2:my_im+1,1:jm,1:km,iNH4),    &
     &             f(2:my_im+1,1:jm,1:km,iPO4),    &
     &                  A_N(1:my_im,1:jm,1:km),    &
     &             PrimProd(1:my_im,1:jm),         &
     &                WC_O2(1:my_im,1:jm),         &
     &                 FPOM(1:my_im,1:jm),         &
     &                  FO2(1:my_im,1:jm),         &
     &                 FNO3(1:my_im,1:jm),         &
     &                 FNH4(1:my_im,1:jm),         &
     &                 FPO4(1:my_im,1:jm))         


 
    RETURN
  END SUBROUTINE MC_GEM 
