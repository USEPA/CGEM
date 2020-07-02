!*******************************************************************************
! Purpose: HMixing.F90  This subroutine calculates horizontal mixing.
!
! Revised: 05/27/2020 Wilson Melendez, Bug fix: reversed sign of "dispersion"
!                                      term.
!*******************************************************************************
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
     real :: delta_conc, ff_area, dispersion

! ---- Initialize temporary horizontal dispersion arrays to zero
     dispx_temp = 0.0
     dispy_temp = 0.0

! Set im1 and jm1 variables     
     im1 = im - 1
     jm1 = jm - 1

! Perform horizontal mixing along the x direction.
     do j = 1, jm
        do i = 1, im1
           do k = 1, nza(i,j)
              disp1 = Dispx(i,j,k)
              if (fm(i+1,j,k) > 0.0) then
                  disp2 = Dispx(i+1,j,k)
                  avg_disp = (disp1 + disp2) / 2.0
                  ff_area = dy(i+1,j) * ((dz(i+1,j,k) + dz(i,j,k)) / 2.0)
!                  if (i == 68 .and. j == 48) then
!                      write(6,*) k, disp1, disp2, avg_disp
!                      write(6,*) 'ff_area (x direction) = ', ff_area
!                  endif
                !  do ii = 1, nf
                     ii = 19
                     delta_conc = f(i+1,j,k,ii) - f(i,j,k,ii)
                     dispersion = avg_disp * ff_area * delta_conc / (dx(i+1,j) + dx(i,j))/2.0
                     dispx_temp(i+1,j,k,ii) = dispx_temp(i+1,j,k,ii) - dispersion
                     dispx_temp(i,j,k,ii) = dispx_temp(i,j,k,ii) + dispersion
!                     if (i == 68 .and. j == 48 .and. ii == 19) then
!                         write(6,*) k, f(i+1,j,k,ii), f(i,j,k,ii), delta_conc, (dx(i+1,j) + dx(i,j))/2.0
!                         write(6,*) 'dispersion (x direction) = ', dispersion
!                         write(6,*) dispx_temp(i+1,j,k,ii), dispx_temp(i,j,k,ii)
!                     endif
                !  enddo                      
              endif
           enddo
        enddo
     enddo

! Perform horizontal mixing along the y direction.
     do i = 1, im
        do j = 1, jm1
           do k = 1, nza(i,j)
              disp1 = Dispy(i,j,k)
              if (fm(i,j+1,k) > 0.0) then
                  disp2 = Dispy(i,j+1,k)
                  avg_disp = (disp1 + disp2) / 2.0
                  ff_area = dx(i,j+1) * ((dz(i,j+1,k) + dz(i,j,k)) / 2.0)
!                  if (i == 68 .and. j == 48) then
!                      write(6,*) k, disp1, disp2, avg_disp
!                      write(6,*) 'ff_area (y direction) = ', ff_area
!                  endif
                !  do ii = 1, nf
                     ii = 19
                     delta_conc = f(i,j+1,k,ii) - f(i,j,k,ii)
                     dispersion = avg_disp * ff_area * delta_conc / (dy(i,j+1) + dy(i,j))/2.0
                     dispy_temp(i,j+1,k,ii) = dispy_temp(i,j+1,k,ii) - dispersion
                     dispy_temp(i,j,k,ii) = dispy_temp(i,j,k,ii) + dispersion
!                    if (i == 68 .and. j == 48 .and. ii == 19) then
!                        write(6,*) k, f(i,j+1,k,ii), f(i,j,k,ii), delta_conc, (dy(i,j+1) + dy(i,j))/2.0
!                        write(6,*) 'dispersion (y direction) = ', dispersion
!                        write(6,*) dispy_temp(i,j+1,k,ii), dispy_temp(i,j,k,ii)
!                    endif
                !  enddo
              endif
           enddo
        enddo
     enddo

!     write(6,*) f(68,48,1,19), dispx_temp(68,48,1,19), dispy_temp(68,48,1,19), Vol(68,48,1) 
! Update concentrations
     do j = 1, jm
        do i = 1, im
           do k = 1, nza(i,j)
              f(i,j,k,19) = f(i,j,k,19) +  ( (dispx_temp(i,j,k,19) + dispy_temp(i,j,k,19)) / Vol(i,j,k) ) * dT 
           enddo
        enddo
     enddo

!     write(6,*) 'After update: ', f(68,48,1,19)
!     write(6,*) '------------------------------------------------------------------------------'

     return
     end
