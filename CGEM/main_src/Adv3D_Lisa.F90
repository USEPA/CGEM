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

! --- transports
      real, dimension(im,jm,nz_max+1) :: w_wsink

! --- tmp
      integer :: km1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf


      if(Which_Adv.eq.0) then
       ux=0.
       vx=0.
       wx=0.
      endif

      ff = 0. !-9999. 
      uxx=0.
      vxx=0.
      wxx=0.

      do j = 1,jm
      do i = 1,im 
      nz = nz_max !nza(i,j)
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
      do i = 1,im 
! --------------------------------------------------------

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
      w_wsink (i,j,1) = wxx(i,j,1)

       nz = nza(i,j)

      do k = 2,nz 
        w_wsink(i,j,k) = wxx(i,j,k)+ ws(ii)*area(i,j)
      end do

!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
      w_wsink (i,j,nz+1) = wxx(i,j,nz+1)+ ws(ii)*real(wsm(i,j),4)*area(i,j)

! -------------------------------------------------------------
      do k = 1, nz       ! do layer by layer
      km1 = max0(k-1,1)
! ----- upwind advection
        ufm = amax1( ux(i  ,j,k),0.)*fm(i-1,j,k)    ! pp/n0
        ufp = amax1(-ux(i+1,j,k),0.)*fm(i+1,j,k)    ! p0/np

        vfm = amax1( vx(i  ,j,k),0.)*fm(i,j-1,k)
        vfp = amax1(-vx(i,j+1,k),0.)*fm(i,j+1,k)
        wfm = amax1(-w_wsink(i,j,k  ),0.)       ! p0/np
        wfp = amax1( w_wsink(i,j,k+1),0.)       ! pp/n0
!                   p0/nn             pn/n0
        cfh = ( (ux(i,j,k)-ufm)*fm(i-1,j,k) - (ux(i+1,j,k)+ufp)*fm(i+1,j,k) )               &
     &       +( (vx(i,j,k)-vfm)*fm(i,j-1,k) - (vx(i,j+1,k)+vfp)*fm(i,j+1,k) )
        cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
          f_n(i,j,k,ii) =                                        &
     &      ( ff(i,j,k,ii)*Vol_prev(i,j,k)                            &
     &       +( cf*ff(i,j,k,ii)                                  &
     &         +( ( (ufm*ff(i-1,j,k,ii)+ufp*ff(i+1,j,k,ii))       &
     &             +(vfm*ff(i,j-1,k,ii)+vfp*ff(i,j+1,k,ii)) )     &
     &           +(wfm*ff(i,j,km1,ii)+wfp*ff(i,j,k+1,ii))) ) *dT  &
     &      ) /Vol(i,j,k)
! -------------------------------------------------------------
      !if(ii.eq.1) write(6,*) Vol_prev(i,j,k)/Vol(i,j,k)
      !if(Vol(i,j,k).le.0) write(6,*) "Vol",i,j,k,Vol(i,j,k)
      !if(ff(i,j,k,ii).lt.0.and.cf.ne.0)    write(6,*) "cf",i,j,k,cf,ff(i,j,k,ii),fm(i,j,k)
      !if(ff(i-1,j,k,ii).lt.0.and.ufm.ne.0) write(6,*) "ufm",i,j,k,ufm,ff(i-1,j,k,ii),fm(i-1,j,k)
      !if(ff(i+1,j,k,ii).lt.0.and.ufp.ne.0) write(6,*) "ufp",i,j,k,ufp,ff(i+1,j,k,ii),fm(i+1,j,k)
      !if(ff(i,j-1,k,ii).lt.0.and.vfm.ne.0) write(6,*) "vfm",i,j,k,vfm,ff(i,j-1,k,ii),fm(i,j-1,k)
      !if(ff(i,j+1,k,ii).lt.0.and.vfp.ne.0) write(6,*) "vfp",i,j,k,vfp,ff(i,j+1,k,ii),fm(i,j+1,k)
      !if(ff(i,j,km1,ii).lt.0.and.wfm.ne.0) write(6,*) "wfm",i,j,k,wfm,ff(i,j,km1,ii),fm(i,j,km1)
      !if(ff(i,j,k+1,ii).lt.0.and.wfp.ne.0) write(6,*) "wfp",i,j,k,wfp,ff(i,j,k+1,ii),fm(i,j,k+1)
      end do ! k = 1, nz
      end do !i
      end do !j
      end do   ! ii = 1,nf
      !stop
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
