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
      real :: ff(0:im+1,0:jm+1,1:nz_max+1,nf) !,f
      real :: uxx(0:im+1,0:jm+1,1:nz_max+1)
      real :: vxx(0:im+1,0:jm+1,1:nz_max+1)
      real :: wxx(0:im+1,0:jm+1,1:nz_max+1)
      real :: f_n(im,jm,nz_max,nf)
      real :: fm_r(0:im+1,0:jm+1,1:nz_max+1)

! --- transports
      real, dimension(im,jm,nz_max+1) :: w_wsink

! --- tmp
      integer :: km1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf
   
      fm_r = real(fm,4)
      ff = sqrt(0d0) 
      uxx=0.
      vxx=0.
      wxx=0.

      do j = 1,jm
      do i = 1,im ! over all calculation grids
      nz = nza(i,j)
      do k = 1,nz
        ff(i,j,k,:) = f(i,j,k,:)
        uxx(i,j,k) = ux(i,j,k)
        vxx(i,j,k) = vx(i,j,k)
        wxx(i,j,k) = wx(i,j,k)
      enddo
      enddo
      enddo

! --------loop over each variable
     do ii = 1,nf

! -------------------------------------------------------------------
      do j = 1,jm 
      do i = 1,im ! over all calculation grids
! --------------------------------------------------------

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
      w_wsink (i,j,1) = wxx(i,j,1)

      do k = 2,nza(i,j) 
        w_wsink(i,j,k) = wxx(i,j,k)+ ws(ii)*area(i,j)
      end do
       nz = nza(i,j)
!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
      w_wsink (i,j,nz+1) = wxx(i,j,nz+1)+ ws(ii)*wsm(i,j)*area(i,j)


! -------------------------------------------------------------
      do k = 1, nz	! do layer by layer
      km1 = max0(k-1,1)

! ----- upwind advection
        ufm = amax1( uxx(i  ,j,k),0.)	! pp/n0
        ufp = amax1(-uxx(i+1,j,k),0.)	! p0/np


        vfm = amax1( vxx(i  ,j,k),0.)
        vfp = amax1(-vxx(i,j+1,k),0.)

        wfm = amax1(-w_wsink(i,j,k  ),0.)	! p0/np
        wfp = amax1( w_wsink(i,j,k+1),0.)	! pp/n0
!                   p0/nn             pn/n0
        cfh = ( (uxx(i,j,k)-ufm) - (uxx(i+1,j,k)+ufp) )               &
     &       +( (vxx(i,j,k)-vfm) - (vxx(i,j+1,k)+vfp) )              
        cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
          f_n(i,j,k,ii) =                                        &
     &      ( ff(i,j,k,ii)*Vol_prev(i,j,k)                            &
     &       +( cf*ff(i,j,k,ii)                                  &
     &         +( ( (ufm*ff(i-1,j,k,ii)*fm_r(i-1,j,k)+ufp*ff(i+1,j,k,ii)*fm_r(i+1,j,k))       &
     &             +(vfm*ff(i,j-1,k,ii)*fm_r(i,j-1,k)+vfp*ff(i,j+1,k,ii)*fm_r(i,j+1,k)) )     &
     &           +(wfm*ff(i,j,km1,ii)*fm_r(i,j,km1)+wfp*ff(i,j,k+1,ii)*fm_r(i,j,k+1))) ) *dT  &
     &      ) /Vol(i,j,k)
! -------------------------------------------------------------
      write(6,*) i,j,k
      write(6,*) Vol(i,j,k)
      write(6,*) Vol(i-1,j,k)
      write(6,*) Vol(i+1,j,k)
      write(6,*) Vol(i,j-1,k)
      write(6,*) Vol(i,j+1,k)
      write(6,*) Vol(i,j,km1)
      write(6,*) Vol(i,j,k+1)
      write(6,*) 
      end do	! k = 1, nz
      end do !i
      end do !j
      end do   ! ii = 1,nf

! update f for the current timestep
         do j = 1,jm
         do i = 1,im
            nz = nza(i,j)
          do k=1,nz
             f(i,j,k,:) = f_n(i,j,k,:)
         enddo
         enddo
         enddo

      RETURN 
      END 
