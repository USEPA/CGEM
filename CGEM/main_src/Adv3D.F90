!----------------------------------------------------------------------
      Subroutine  Adv3D (myid,numprocs)
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
      use mpi

      IMPLICIT NONE

      integer, intent(in) :: myid,numprocs
      integer :: i,j,k,ii,nz,myi,mpierr
      real :: f_n(0:(myim+1),jm,km,nf) 

! --- transports
      real :: w_wsink(im,jm,km+1)

! --- tmp
      integer :: im1,ip1,jm1,jp1,km1
      integer :: myim1,myip1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf

      if(Which_Adv.eq.0.or.Which_Adv.eq.2) then
       ux=0.
       vx=0.
       wx=0.
      endif

      if(numprocs.gt.1) call AdvNeighbors(f,myim,jm,km,nsl,myid,numprocs)

! --------loop over each variable
     do ii = 1,nf
     
! -------------------------------------------------------------------
      do j = 1,jm
      myi = 1
      do i = myi_start,myi_end
! --------------------------------------------------------
       im1 = max0(i-1,1)
       ip1 = min0(i+1,im)
       jm1 = max0(j-1,1)
       jp1 = min0(j+1,jm)

       myim1 = max0(myi-1,1)
       myip1 = min0(myi+1,im)
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
          f_n(myi,j,k,ii) =                                        &
     &      ( f(myi,j,k,ii)*Vol_prev(i,j,k)                            &
     &       +( cf*f(myi,j,k,ii)                                  &
     &         +( ( (ufm*f(myim1,j,k,ii)+ufp*f(myip1,j,k,ii))       &
     &             +(vfm*f(myi,jm1,k,ii)+vfp*f(myi,jp1,k,ii)) )     &
     &           +(wfm*f(myi,j,km1,ii)+wfp*f(myi,j,min(k+1,nz),ii))) ) *dT  &
     &      ) /Vol(i,j,k)
! -------------------------------------------------------------
       if(f_n(myi,j,k,ii).lt.0) then
           write(6,*) "myid,numprocs,myid,start,end=",myid,numprocs,myid,myi_start,myi_end
           write(6,*) "f_n(ii,i,j,k) lt zero,myi",ii,i,j,k,f_n(myi,j,k,ii),myi
           write(6,*) "fim1,f,fip1",f(myim1,j,k,ii),f(myi,j,k,ii),f(myip1,j,k,ii)
           write(6,*) "ufm,ufp,vfm,vfp,wfm,wfp,cf",ufm,ufp,vfm,vfp,wfm,wfp,cf
           write(6,*) "f,Vol_prev,V/V",f(myi,j,k,ii),Vol_prev(i,j,k),Vol_prev(i,j,k)/Vol(i,j,k)
           write(6,*) "ufm,ufp,vfm,vfp,wfm,wfp",ufm,ufp,vfm,vfp,wfm,wfp
           write(6,*) "cf,cfh",cf,cfh
           write(6,*) (ux(i,j,k)-ufm),(ux(ip1,j,k)+ufp),(ux(i,j,k)-ufm) - (ux(ip1,j,k)+ufp)
           write(6,*) (vx(i,j,k)-vfm),(ux(i,jp1,k)+vfp),(vx(i,j,k)-vfm) - (vx(i,jp1,k)+vfp)
           write(6,*) "cf_wterms",w_wsink(i,j,k+1),w_wsink(i,j,k),(w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm)
           write(6,*) "u,v,w",ux(i,j,k),vx(i,j,k),w_wsink(i,j,k)
           write(6,*) "u,v,w:p1",ux(ip1,j,k),vx(i,jp1,k),w_wsink(i,j,k+1)
           write(6,*) "V,Vip1",Vol(i,j,k),Vol(ip1,j,k)
           write(6,*) "V,Vjp1",Vol(i,j,k),Vol(i,jp1,k)
           write(6,*) "ufm*fm1,ufp*fp1",ufm*f(myim1,j,k,ii),ufp*f(myip1,j,k,ii)
           write(6,*) "vfm*fm1,vfp*fp1",vfm*f(myi,jm1,k,ii),vfp*f(myi,jp1,k,ii)
           write(6,*) "wfm*fm1,wfp*fp1",wfm*f(myi,j,km1,ii),vfp*f(myi,j,min(k+1,nz),ii)
           write(6,*)
           write(6,*) "u",ux(i-2,j,k),ux(im1,j,k),ux(i,j,k),ux(ip1,j,k),ux(i+2,j,k)
           write(6,*) "v",vx(i,j-2,k),vx(i,jm1,k),vx(i,j,k),vx(i,jp1,k),vx(i,j+2,k)
           write(6,*) "w",wx(i,j,km1),wx(i,j,k),wx(i,j,k+1)
           call MPI_BARRIER(MPI_COMM_WORLD,mpierr)
           call MPI_FINALIZE(mpierr)
           stop
       endif
      end do ! k = 1, nz
       myi=myi+1
      end do !i
      end do !j
      end do   ! ii = 1,nf

! update f for the current timestep
         do j = 1,jm
          myi = 1
         do i = myi_start,myi_end
            nz = nza(i,j)
          do k=1,nz
             f(myi,j,k,:) = f_n(myi,j,k,:)
          enddo
          myi = myi + 1
         enddo
         enddo

#ifdef DEBUG
write(6,*) "---Adv3D---"
write(6,*) "  Which_adv=",Which_adv
if(numprocs.eq.1) write(6,*) "f(1)=",f(icent,jcent,1,1)
write(6,*)
#endif

      RETURN 
      END 
