!----------------------------------------------------------------------
      Subroutine  Adv3D_EFDC ()
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
      real :: f_n(im,jm,km,nf) 

! --- transports
      real :: w_wsink(im,jm,km+1)

! --- tmp
      integer :: im1,ip1,jm1,jp1,km1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf

#ifdef DEBUG
write(6,*) "---Adv3D---"
write(6,*) "  Which_adv=",Which_adv
write(6,*) 
#endif

      if(Which_Adv.eq.0) then
       ux=0.
       vx=0.
       wx=0.
      endif


! --------loop over each variable
     do ii = 1,nf

! -------------------------------------------------------------------
      do j = 1,jm
      do i = 1,im
! --------------------------------------------------------
       im1 = max0(i-1,1)
       ip1 = min0(i+1,im)
       jm1 = max0(j-1,1)
       jp1 = min0(j+1,jm)

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
      w_wsink (i,j,1) = wx(i,j,1)

       nz = nza(i,j)

      do k = 2,nz 
        w_wsink(i,j,k) = wx(i,j,k) + ws(ii)*area(i,j)
      end do

!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
      w_wsink (i,j,nz+1) =  wx(i,j,k) + ws(ii)*real(wsm(i,j),4)*area(i,j)

#ifdef DEBUG
write(6,*) "nz=",nz
write(6,*)
write(6,*) "At the cell i,j,k=",icent,jcent,nz
write(6,*) "ux,vx,wx,w_wsink:"
write(6,*) ux(icent,jcent,nz),vx(icent,jcent,nz),wx(icent,jcent,nz),w_wsink(icent,jcent,nz)
write(6,*) 
#endif

! -------------------------------------------------------------
      do k = 1, nz       ! do layer by layer
      km1 = max0(k-1,1)
! ----- upwind advection
        ufm = amax1( ux(im1  ,j,k),0.)    ! pp/n0
        ufp = amax1(-ux(ip1  ,j,k),0.)    ! p0/np

        vfm = amax1( vx(i  ,jm1,k),0.)
        vfp = amax1(-vx(i  ,jp1,k),0.)
        wfm = amax1(-w_wsink(i,j,k  ),0.)       ! p0/np
        wfp = amax1( w_wsink(i,j,k+1),0.)       ! pp/n0
!                   p0/nn             pn/n0
        cfh = ( (ux(im1,j,k)-ufm) - (ux(ip1,j,k)+ufp) )               &
     &       +( (vx(i,jm1,k)-vfm) - (vx(i,jp1,k)+vfp) )
        cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
          f_n(i,j,k,ii) =                                        &
     &      ( f(i,j,k,ii)*Vol_prev(i,j,k)                            &
     &       +( cf*f(i,j,k,ii)                                  &
     &         +( ( (ufm*f(im1,j,k,ii)+ufp*f(ip1,j,k,ii))       &
     &             +(vfm*f(i,jm1,k,ii)+vfp*f(i,jp1,k,ii)) )     &
     &           +(wfm*f(i,j,km1,ii)+wfp*f(i,j,k+1,ii))) ) *dT  &
     &      ) /Vol(i,j,k)
! -------------------------------------------------------------
      !if(ii.eq.1) write(6,*) Vol_prev(i,j,k)/Vol(i,j,k)
      !if(Vol(i,j,k).le.0) write(6,*) "Vol",i,j,k,Vol(i,j,k)
      !if(f(i,j,k,ii).lt.0.and.cf.ne.0)    write(6,*) "cf",i,j,k,cf,f(i,j,k,ii),fm(i,j,k)
      !if(f(i-1,j,k,ii).lt.0.and.ufm.ne.0) write(6,*) "ufm",i,j,k,ufm,f(i-1,j,k,ii),fm(i-1,j,k)
      !if(f(i+1,j,k,ii).lt.0.and.ufp.ne.0) write(6,*) "ufp",i,j,k,ufp,f(i+1,j,k,ii),fm(i+1,j,k)
      !if(f(i,j-1,k,ii).lt.0.and.vfm.ne.0) write(6,*) "vfm",i,j,k,vfm,f(i,j-1,k,ii),fm(i,j-1,k)
      !if(f(i,j+1,k,ii).lt.0.and.vfp.ne.0) write(6,*) "vfp",i,j,k,vfp,f(i,j+1,k,ii),fm(i,j+1,k)
      !if(f(i,j,km1,ii).lt.0.and.wfm.ne.0) write(6,*) "wfm",i,j,k,wfm,f(i,j,km1,ii),fm(i,j,km1)
      !if(f(i,j,k+1,ii).lt.0.and.wfp.ne.0) write(6,*) "wfp",i,j,k,wfp,f(i,j,k+1,ii),fm(i,j,k+1)
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
