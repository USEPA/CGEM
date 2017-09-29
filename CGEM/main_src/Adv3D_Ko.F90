!----------------------------------------------------------------------
      Subroutine  Adv3D ()
!     Modified by Cody Simmons/EMVL
!     Originally written by D.S.Ko/NRL
! ----------------------------------------------------------------------
!     Do 3D advection for one time step
!   * u/v/w is transport m3/s

      USE Model_dim
      USE INPUT_VARS
      USE Grid
      USE State_Vars
      USE Hydro

      IMPLICIT NONE

      integer :: i,j,k,ii,nz
      integer, dimension(im,jm) :: fm2
      real, dimension(im,jm,nsl,nf) :: ff !,f
      real, dimension (im,jm,nsl,nf) :: fmod


! --- transports
      real, dimension(im,jm,nsl) :: w_wsink
      !real, dimension(im,jm) :: dxdy

! --- tmp
      integer :: km1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf

      integer :: tmpi,tmpj,tmpk,tmpii
      real :: tmpVal1, tmpVal2, tmpVal3, tmpVal4
      real :: fmod_im, fmod_ip, fmod_jm, fmod_jp, fmod_wm, fmod_wp



! --------loop over each variable
     do ii = 1,nf

! -------------------------------------------------------------------
      do j = 1,jm 
      do i = 1,im ! over all calculation grids
               if(fm(i,j).gt.0) then
! --------------------------------------------------------

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
      w_wsink (i,j,1) = wx(i,j,1)

      do k = 2,nza(i,j) 
        w_wsink(i,j,k) = wx(i,j,k)+ ws(ii)*area(i,j)
      end do
       nz = nza(i,j)
!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
      w_wsink (i,j,nz+1) = wx(i,j,nz+1)+ ws(ii)*wsm(i,j)*area(i,j)


! -------------------------------------------------------------
      do k = 1, nz	! do layer by layer
      km1 = max0(k-1,1)

! ----- upwind advection
        ufm = amax1( ux(i  ,j,k),0.)	! pp/n0
        ufp = amax1(-ux(i+1,j,k),0.)	! p0/np

        vfm = amax1( vx(i  ,j,k),0.)
        vfp = amax1(-vx(i,j+1,k),0.)
        wfm = amax1(-w_wsink(i,j,k  ),0.)	! p0/np
        wfp = amax1( w_wsink(i,j,k+1),0.)	! pp/n0
!                   p0/nn             pn/n0
        cfh = ( (ux(i,j,k)-ufm) - (ux(i+1,j,k)+ufp) )               &
     &       +( (vx(i,j,k)-vfm) - (vx(i,j+1,k)+vfp) )              
        cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
          ff(i,j,k,ii) =                                        &
     &      ( f(i,j,k,ii)*Vol(i,j,k)                            &
     &       +( cf*f(i,j,k,ii)                                  &
     &         +( ( (ufm*f(i-1,j,k,ii)+ufp*f(i+1,j,k,ii))       &
     &             +(vfm*f(i,j-1,k,ii)+vfp*f(i,j+1,k,ii)) )     &
     &           +(wfm*f(i,j,km1,ii)+wfp*f(i,j,k+1,ii)) ) )*dT  &
     &      ) /Vol_prev(i,j,k)

! -------------------------------------------------------------
      end do	! k = 1, nz
       endif !fm = 1
      end do !i
      end do !j
      end do   ! ii = 1,nf

! update f for the current timestep
         do j = 1,jm
         do i = 1,im
          do k=1,nza(i,j) 
             if(fm(i,j).gt.0) then
             f(i,j,k,:) = ff(i,j,k,:)
             endif
         enddo
         enddo
         enddo

      RETURN 
      END 
