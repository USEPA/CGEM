!---------------------------------------------------------------------------
!  SUBROUTINE MC_Flux(fm, O2_Flux, istep, my_imstart, my_imend, istep_wait, print_ave  )
SUBROUTINE MC_Flux(fm, O2_Flux, istep, istep_wait, print_ave  )   
!---------------------------------------------------------------------------
!Assume this is only called in 3D

  USE Model_dim
  USE Model_Compare

  !--------------------------------------------------------------------------
  ! model inter-comparison study through the Coastal Ocean Modeling Testbed
  ! (http://testbed.sura.org/) 
  !------------------------------------------------------------------------
    IMPLICIT NONE

    real, intent(in) :: O2_Flux(myim,jm)
    integer, intent(in) :: istep
    integer, intent(in) :: istep_wait, print_ave, fm(im,jm,km)
    real :: SUM_O2F(myim,jm)
    integer :: my_im, myi, i, j
    integer, save :: i_out, print_file

!Define Loop Bounds
    my_im = myi_end - myi_start + 1
    if(istep .le. istep_wait) then
       SUM_O2F = 0.
       i_out = 0
       print_file = istep_wait + print_ave
    endif

!Average Daily Flux
    do j = 1, jm
         myi = 1
         do i = myi_start, myi_end
             if(fm(i,j,1) > 1.0E-06) then
               SUM_O2F(myi,j) = SUM_O2F(myi,j) + O2_Flux(myi,j)/real(print_ave)
             endif
         myi = myi + 1
         enddo
     enddo

    if(istep.eq.print_file) then

     call WRITE_FLUX_MC( myi_start, my_im, 1, jm, i_out, SUM_O2F(1:myim,1:jm) )

     SUM_O2F = 0.
     i_out = i_out + 1
     print_file = print_file + print_ave

    endif

 
    RETURN
  END SUBROUTINE MC_Flux
