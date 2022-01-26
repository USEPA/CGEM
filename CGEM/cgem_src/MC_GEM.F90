!---------------------------------------------------------------------------
  SUBROUTINE MC_GEM(f, fm, PrimProd, WC_O2, FPOM, FO2,  &
     &     FNO3, FNH4, FPO4, outstep, istep, my_im  )   
!---------------------------------------------------------------------------
!Assume this is only called in 3D

  USE Model_dim
  USE INPUT_VARS, ONLY:dT  
  USE  CGEM_vars  
  USE Model_Compare

  !--------------------------------------------------------------------------
  ! model inter-comparison study through the Coastal Ocean Modeling Testbed
  ! (http://testbed.sura.org/) 
  !------------------------------------------------------------------------
    IMPLICIT NONE

    real, intent(in) :: f(myimp2,jm,nsl,nf)
    integer, intent(in) :: outstep, istep, my_im
    real, dimension (my_im,jm), intent(in) :: PrimProd, WC_O2, FO2
    real, dimension (my_im,jm), intent(in) :: FNO3, FNH4, FPO4, FPOM 
    real, intent(in) :: fm(im,jm,km)
    real :: A_N(my_im,jm,km)
    integer :: i, j, k, myi, isp, nz

    A_N = 0.

    do j = 1, jm
     myi = 1
     do i = myi_start, myi_end
       if (fm(i,j,1) > 1.0E-06) then
         nz = nza(i,j)
         do k = 1, nz
            do isp = 1, nospA
               A_N(myi,j,k) = A_N(myi,j,k) + f(myi,j,k,iA(isp))*f(myi,j,k,iQn(isp))
            enddo
        enddo
       endif
       myi = myi + 1
      enddo 
    enddo

    call WRITE_GEM_MC( myi_start, my_im, 1, jm, 1, km, outstep, istep, real(dT), &
     &             f(1:my_im,1:jm,1:km,iO2),     &
     &             f(1:my_im,1:jm,1:km,iNO3),    &
     &             f(1:my_im,1:jm,1:km,iNH4),    &
     &             f(1:my_im,1:jm,1:km,iPO4),    &
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
