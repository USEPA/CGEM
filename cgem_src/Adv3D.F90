!----------------------------------------------------------------------
      Subroutine  Adv3D (f)
!     Modified by Cody Simmons/EMVL
!     Originally written by D.S.Ko/NRL
! ----------------------------------------------------------------------
!     Do 3D advection for one time step
!   * u/v/w is transport m3/s

      USE Model_dim
      USE INPUT_VARS
      USE Grid
      USE Hydro

      IMPLICIT NONE

      integer :: i,j,k,ii,nz
      integer, dimension(im,jm) :: fm2
      real, dimension(im,jm,nsl,nf) :: f, ff


! --- transports
      real, dimension(im,jm,nsl) :: w_wsink
      real, dimension(im,jm) :: dxdy

! --- tmp
      integer :: km1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf

      nz = nsl-1

      ! Alternate Mask: Make lateral boundary cells "land" cells so
      ! they aren't modified/looped over in the code
      fm2(:,:) = fm(:,:)
      fm2(1,:) = 0
      fm2(im,:) = 0
      fm2(:,1) = 0
      fm2(:,jm) = 0

! --------loop over each variable
      do ii = 1,nf

! -------------------------------------------------------------------
        do j = 1,jm 
          do i = 1, im  ! over all calculation grids
            if(fm2(i,j).eq.1) then
! --------------------------------------------------------

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
              w_wsink (i,j,1) = Wx(i,j,1)

              do k = 2, nz
                w_wsink(i,j,k) = Wx(i,j,k)+ ws(ii)*dxdy(i,j)
              end do

!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
              w_wsink (i,j,nz+1) = Wx(i,j,nz+1)+ ws(ii)*wsm(i,j)*dxdy(i,j)


! -------------------------------------------------------------
              do k = 1, nz	! do layer by layer
                km1 = max0(k-1,1)

! ----- upwind advection
                ufm = amax1( Ux(i  ,j,k),0.)	! pp/n0
                ufp = amax1(-Ux(i+1,j,k),0.)	! p0/np

                vfm = amax1( Vx(i  ,j,k),0.)
                vfp = amax1(-Vx(i,j+1,k),0.)
                wfm = amax1(-w_wsink(i,j,k  ),0.)	! p0/np
                wfp = amax1( w_wsink(i,j,k+1),0.)	! pp/n0
!                       p0/nn             pn/n0
                cfh = ( (Ux(i,j,k)-ufm) - (Ux(i+1,j,k)+ufp) )               &
     &            +( (Vx(i,j,k)-vfm) - (Vx(i,j+1,k)+vfp) )              
                cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
                ff(i+1,j,k,ii) =                                        &
     &            ( f(i+1,j,k,ii)*Vol_prev(i,j,k)                            &
     &            +( cf*f(i+1,j,k,ii)                                  &
     &            +( ( (ufm*f(i,j,k,ii)+ufp*f(i+2,j,k,ii))       &
     &            +(vfm*f(i+1,j-1,k,ii)+vfp*f(i+1,j+1,k,ii)) )     &
     &            +(wfm*f(i+1,j,km1,ii)+wfp*f(i+1,j,k+1,ii)) ) )*dT  &
     &            ) /Vol(i,j,k)

! -------------------------------------------------------------
              end do	! k = 1, nz
            endif !fm2 = 1
          end do !i
        end do !j
      end do   ! ii = 1,nf

! update f for the current timestep
      do k = 1,nz
        do j = 1,jm
          do i = 1, im !my_imstart,my_imend
            if(fm2(i,j).eq.1) then
              f(i+1,j,k,:) = ff(i+1,j,k,:)
            endif
          enddo
        enddo
      enddo

      RETURN 
      END 
