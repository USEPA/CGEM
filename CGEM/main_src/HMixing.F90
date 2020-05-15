!***********************************************************************
! Purpose: HMixing.F90  This subroutine calculates horizontal mixing.
!
! Revised:    
!***********************************************************************
     Subroutine  HMixing ()

     USE Model_dim
     USE INPUT_VARS, ONLY: dT
     USE Grid
     USE State_Vars
     USE Hydro

     IMPLICIT NONE

     integer :: i, j, k, ii
     integer :: im1, jm1
     real :: dispx_temp(im, jm, km, nf)
     real :: dispy_temp(im, jm, km, nf)
     real :: disp1, disp2, avg_disp
     real :: delta_conc, dispersion, ff_area

! ---- Initialize horizontal dispersion to zero
     dispx_temp = 0.0
     dispy_temp = 0.0
     
     im1 = im - 1
     jm1 = jm - 1

     do j = 1, jm
        do i = 1, im1
           do k = 1, nza(i,j)
              disp1 = Dispx(i,j,k)
              if (fm(i+1,j,k) == 1) then
                  disp2 = Dispx(i+1,j,k)
                  avg_disp = (disp1 + disp2) / 2.0
                  ff_area = dy(i+1,j) * ((dz(i+1,j,k) + dz(i,j,k)) / 2.0)
                  do ii = 1, nf
                     delta_conc = f(i+1,j,k,ii) - f(i,j,k,ii)
                     dispersion = avg_disp * ff_area * delta_conc / (dx(i+1,j) + dx(i,j))/2.0
                     dispx_temp(i+1,j,k,ii) = dispx_temp(i+1,j,k,ii) + dispersion
                     dispx_temp(i,j,k,ii) = dispx_temp(i,j,k,ii) - dispersion
                  enddo                      
              endif
           enddo
        enddo
     enddo

     do i = 1, im
        do j = 1, jm1
           do k = 1, nza(i,j)
              disp1 = Dispy(i,j,k)
              if (fm(i,j+1,k) == 1) then
                  disp2 = Dispy(i,j+1,k)
                  avg_disp = (disp1 + disp2) / 2.0
                  ff_area = dx(i,j+1) * ((dz(i,j+1,k) + dz(i,j,k)) / 2.0)
                  do ii = 1, nf
                     delta_conc = f(i,j+1,k,ii) - f(i,j,k,ii)
                     dispersion = avg_disp * ff_area * delta_conc / (dy(i,j+1) + dy(i,j))/2.0
                     dispy_temp(i,j+1,k,ii) = dispy_temp(i,j+1,k,ii) + dispersion
                     dispy_temp(i,j,k,ii) = dispy_temp(i,j,k,ii) - dispersion
                  enddo
              endif
           enddo
        enddo
     enddo

     f(i,j,k,:) = f(i,j,k,:) +  ( (dispx_temp(i,j,k,:) + dispy_temp(i,j,k,:)) / Vol(i,j,k) ) * dT 

     return
     end

