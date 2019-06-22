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

      if(Which_Adv.eq.0.or.Which_Adv.eq.2) then
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
      w_wsink (i,j,nz+1) = wx(i,j,nz+1) + ws(ii)*real(wsm(i,j),4)*area(i,j)

#ifdef DEBUG_A
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
        ufm = amax1( ux(i  ,j,k),0.)    ! pp/n0
        ufp = amax1(-ux(ip1,j,k),0.)    ! p0/np

        vfm = amax1( vx(i  ,j,k),0.)
        vfp = amax1(-vx(i,jp1,k),0.)

        wfm = amax1(-w_wsink(i,j,k  ),0.)       ! p0/np
        wfp = amax1( w_wsink(i,j,k+1),0.)       ! pp/n0
!                   p0/nn             pn/n0
        cfh = ( (ux(i,j,k)-ufm) - (ux(ip1,j,k)+ufp) )               &
     &       +( (vx(i,j,k)-vfm) - (vx(i,jp1,k)+vfp) )
        cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
          f_n(i,j,k,ii) =                                        &
     &      ( f(i,j,k,ii)*Vol_prev(i,j,k)                            &
     &       +( cf*f(i,j,k,ii)                                  &
     &         +( ( (ufm*f(im1,j,k,ii)+ufp*f(ip1,j,k,ii))       &
     &             +(vfm*f(i,jm1,k,ii)+vfp*f(i,jp1,k,ii)) )     &
     &           +(wfm*f(i,j,km1,ii)+wfp*f(i,j,min(k+1,nz),ii))) ) *dT  &
     &      ) /Vol(i,j,k)
! -------------------------------------------------------------
       if(f_n(i,j,k,ii).lt.0) then
           write(6,*) "f_n(ii,i,j,k) lt zero",ii,i,j,k,f_n(i,j,k,ii)
           write(6,*) "f,Vol_prev,V/V",f(i,j,k,ii),Vol_prev(i,j,k),Vol_prev(i,j,k)/Vol(i,j,k)
           write(6,*) "ufm,ufp,vfm,vfp,wfm,wfp",ufm,ufp,vfm,vfp,wfm,wfp
           write(6,*) "cf,cfh",cf,cfh
           write(6,*) (ux(i,j,k)-ufm),(ux(ip1,j,k)+ufp),(ux(i,j,k)-ufm) - (ux(ip1,j,k)+ufp)
           write(6,*) (vx(i,j,k)-vfm),(ux(i,jp1,k)+vfp),(vx(i,j,k)-vfm) - (vx(i,jp1,k)+vfp)
           write(6,*) "cf_wterms",w_wsink(i,j,k+1),w_wsink(i,j,k),(w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm)
           write(6,*) "u,v,w",ux(i,j,k),vx(i,j,k),w_wsink(i,j,k)
           write(6,*) "u,v,w:p1",ux(ip1,j,k),vx(i,jp1,k),w_wsink(i,j,k+1)
           write(6,*) "V,Vip1",Vol(i,j,k),Vol(ip1,j,k)
           write(6,*) "V,Vjp1",Vol(i,j,k),Vol(i,jp1,k)
           write(6,*) "ufm*fm1,ufp*fp1",ufm*f(im1,j,k,ii),ufp*f(ip1,j,k,ii)
           write(6,*) "vfm*fm1,vfp*fp1",vfm*f(i,jm1,k,ii),vfp*f(i,jp1,k,ii)
           write(6,*) "wfm*fm1,wfp*fp1",wfm*f(i,j,km1,ii),vfp*f(i,j,min(k+1,nz),ii)
           write(6,*)
           write(6,*) "u",ux(i-2,j,k),ux(im1,j,k),ux(i,j,k),ux(ip1,j,k),ux(i+2,j,k)
           write(6,*) "u_mps",ux(im1,j,k)*dx(im1,j)/Vol(im1,j,k),ux(i,j,k)*dx(i,j)/Vol(i,j,k),ux(ip1,j,k)*dx(ip1,j)/Vol(ip1,j,k)
           write(6,*) "v",vx(i,j-2,k),vx(i,jm1,k),vx(i,j,k),vx(i,jp1,k),vx(i,j+2,k)
           write(6,*) "v_mps",vx(i,jm1,k)*dy(i,jm1)/Vol(i,jm1,k),vx(i,j,k)*dy(i,j)/Vol(i,j,k),vx(i,jp1,k)*dy(i,jp1)/Vol(i,jp1,k)
           write(6,*) "w",wx(i,j,km1),wx(i,j,k),wx(i,j,k+1)
           write(6,*) "w_mps",wx(i,j,km1)*dz(i,j,km1)/Vol(i,j,km1),wx(i,j,k)*dz(i,j,k)/Vol(i,j,k),wx(i,j,k+1)*dz(i,j,min(k+1,nz))/Vol(i,j,min(k+1,nz))
           stop
       endif
      end do ! k = 1, nz
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
