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
   
      integer :: mv

! --- transports
      real, dimension(im,jm,nsl) :: w_wsink
      real, dimension(im,jm) :: dxdy

! --- tmp
      integer :: km1
      real :: ufm,ufp,vfm,vfp,wfm,wfp
      real :: cfh,cf

      integer :: tmpi,tmpj,tmpk,tmpii
      real :: tmpVal1, tmpVal2, tmpVal3, tmpVal4
      real :: fmod_im, fmod_ip, fmod_jm, fmod_jp, fmod_wm, fmod_wp
      mv = -9999


       fm2 = fm


!#ifdef DEBUG_CWS
!      !Vol = 1
!      !Vol_prev = 1
!      Ux = -1 
!      Vx = 0
!      Wx = 0   !-0.001
!      !Wx(:,:,1) = 0
!      !do j=1,jm
!      !  do i=1,im
!      !    Wx(i,j,fm2(i,j)) = 0
!      !  enddo
!      !enddo 
!#endif


! --------loop over each variable
      do ii = 1,nf

! -------------------------------------------------------------------
        do j = 1,jm 
          do i = 1, im  ! over all calculation grids
            !if(fm2(i,j).eq.1) then
            !write(6,'(a,i2,a,i2,a,i2)') "fm2(",i,",",j,")=",fm2(i,j)
            if(fm2(i,j).ne.0) then

! --------------------------------------------------------

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
              w_wsink (i,j,1) = Wx(i,j,1)

              !nz=nza(i,j)-1
              !do k = 2, nz 
              do k = 2, fm2(i,j)
                w_wsink(i,j,k) = Wx(i,j,k) + ws(ii)*dxdy(i,j)
              end do

!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
              w_wsink (i,j,nz+1) = Wx(i,j,nz+1)+ ws(ii)*wsm(i,j)*dxdy(i,j)


! -------------------------------------------------------------
              !do k = 1, nz	! do layer by layer
              do k = 1, fm2(i,j)	! do layer by layer
                km1 = max0(k-1,1)
!#ifdef DEBUG_CWS
!      write(6,'(a,i2,a,i2,a,i2,a)')"(i,j,k)=(",i,",",j,",",k,")"
!#endif


! ----- upwind advection
                !check that data is defined on i-1 cell

              
                ufm = amax1( Ux(i  ,j,k),0.)	! pp/n0
                ufp = amax1(-Ux(i+1,j,k),0.)	! p0/np
                vfm = amax1( Vx(i  ,j,k),0.)
                vfp = amax1(-Vx(i,j+1,k),0.)
                wfm = amax1(-w_wsink(i,j,k  ),0.)	! p0/np
                wfp = amax1( w_wsink(i,j,k+1),0.)	! pp/n0


                if (f(i-1,j,k,ii) .eq. mv) then
                  fmod_im = f(i,j,k,ii)
                else 
                  fmod_im = f(i-1,j,k,ii)
                endif

                if (f(i+1,j,k,ii) .eq. mv) then
                  fmod_ip = f(i,j,k,ii)
                else
                  fmod_ip = f(i+1,j,k,ii)
                endif

                if (f(i,j-1,k,ii) .eq. mv) then
                  fmod_jm = f(i,j,k,ii)
                else
                  fmod_jm = f(i,j-1,k,ii)
                endif

                if (f(i,j+1,k,ii) .eq. mv) then
                  fmod_jp = f(i,j,k,ii)
                else 
                  fmod_jp = f(i,j+1,k,ii)
                endif

                if (f(i,j,k+1,ii) .eq. mv) then
                  fmod_wp = f(i,j,k,ii)
                else
                  fmod_wp = f(i,j,k+1,ii)
                endif

!                       p0/nn             pn/n0               

                cfh = ( (Ux(i,j,k)-ufm) - (Ux(i+1,j,k)+ufp) )               &
     &               +( (Vx(i,j,k)-vfm) - (Vx(i,j+1,k)+vfp) )              
                cf  = cfh + ((w_wsink(i,j,k+1)-wfp)-(w_wsink(i,j,k)+wfm))
! -------------------------------------------------------------
     !           ff(i,j,k,ii) =                                        &
     !&            ( f(i,j,k,ii)*Vol_prev(i,j,k)                            &
     !&            +( cf*f(i,j,k,ii)                                  &
     !&            +( ( (ufm*f(i-1,j,k,ii)+ufp*f(i+1,j,k,ii))       &
     !&            +(vfm*f(i,j-1,k,ii)+vfp*f(i,j+1,k,ii)) )     &
     !&            +(wfm*f(i,j,km1,ii)+wfp*f(i,j,k+1,ii)) ) )*dT  &
     !&            ) /Vol(i,j,k)


!                ff(i,j,k,ii) =                                     &
!     &            ( f(i,j,k,ii)*Vol_prev(i,j,k)                            &
!     &            +(0.0                                             &
!     &            + cf*f(i,j,k,ii)                                  &
!     &            +( 0.0 +                                          &
!     &              ( (ufm*f(i-1,j,k,ii)+ufp*f(i+1,j,k,ii))       &
!     &            +(vfm*f(i,j-1,k,ii)+vfp*f(i,j+1,k,ii)) )     &
!     &            +(wfm*f(i,j,km1,ii)+wfp*f(i,j,k+1,ii))            &
!     &             )                                     &
!     &            )*dT  &
!     &            ) /Vol(i,j,k)


                tmpVal1 = f(i,j,k,ii)*Vol_prev(i,j,k)
                tmpVal2 = cf*f(i,j,k,ii)*dT
                tmpVal3 = ((ufm*fmod_im+ufp*fmod_ip)       &
     &            +(vfm*fmod_jm+vfp*fmod_jp))*dT
                tmpVal4 = (wfm*f(i,j,km1,ii)+wfp*fmod_wp)*dT

                 ff(i,j,k,ii) = (tmpVal1 + tmpVal2 + tmpVal3 +tmpVal4)/Vol(i,j,k)

!#ifdef DEBUG_CWS
!      !write(6,*)"Vol=", Vol(i,j,k)
!      !write(6,*)"Vol_prev=",Vol_prev(i,j,k)
!      !write(6,*)"ii=", ii
!      !write(6,*)"dT=",dT
!      !if(ii .eq. 25) then
!      tmpii = 25
!      tmpi = 12
!      tmpj = 25
!      tmpk = 1
!
!      if(ii .eq.tmpii .AND. i.eq.tmpi .AND. j.eq.tmpj .AND. k.eq.tmpk) then
!        !write(6,*)"  f =", f(i,j,k,ii)
!        !write(6,*)"  ff=", ff(i,j,k,ii)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  f(",i,",",j,",",k,") =", f(i,j,k,ii)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  ff(",i,",",j,",",k,")=", ff(i,j,k,ii)
!        write(6,*)"  fmod_im=",fmod_im
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"     f(",i-1,",",j,",",k,") =", f(i-1,j,k,ii)
!        write(6,*)"  fmod_ip=",fmod_ip
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"     f(",i+1,",",j,",",k,") =", f(i+1,j,k,ii)
!        write(6,*)"  fmod_jm=",fmod_jm
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"     f(",i,",",j-1,",",k,") =", f(i,j-1,k,ii)
!        write(6,*)"  fmod_jp=",fmod_jp
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"     f(",i,",",j+1,",",k,") =", f(i,j+1,k,ii)
!        !write(6,*)"  fmod_wm=",fmod_wm
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"     f(",i,",",j,",",km1,") =", f(i,j,km1,ii)
!        write(6,*)"  fmod_wp=",fmod_wp
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"     f(",i,",",j,",",k+1,") =", f(i,j,k+1,ii)
!        !write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  fmod(",i,",",j,",",km1,") =", fmod(i,j,km1,ii)
!        !write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  fmod(",i,",",j,",",k+1,") =", fmod(i,j,k+1,ii)
!
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  Ux(",i,",",j,",",k,") =", Ux(i,j,k)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  Ux(",i+1,",",j,",",k,") =", Ux(i+1,j,k)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  Vx(",i,",",j,",",k,") =", Vx(i,j,k)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  Vx(",i,",",j+1,",",k,") =", Vx(i,j+1,k)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  Wx(",i,",",j,",",k,") =", Wx(i,j,k)
!        write(6,'(a,i2,a,i2,a,i2,a,f17.7)')"  Wx(",i,",",j,",",k+1,") =", Wx(i,j,k+1)
!        write(6,'(a,f17.7)')"  ufm=", ufm
!        write(6,'(a,f17.7)')"  ufp=", ufp
!        write(6,'(a,f17.7)')"  vfm=", vfm
!        write(6,'(a,f17.7)')"  vfp=", vfp
!        write(6,'(a,f17.7)')"  wfm=", wfm
!        write(6,'(a,f17.7)')"  wfp=", wfp
!        write(6,'(a,f17.7)')"  cfh=", cfh
!        write(6,'(a,f17.7)')"  cf =", cf
!        write(6,*)"  tmpVal1=",tmpVal1
!        write(6,*)"  tmpVal2=",tmpVal2
!        write(6,*)"  tmpVal3=",tmpVal3
!        write(6,*)"  tmpVal4=",tmpVal4
!        write(6,*)"  dT = ", dT
!        write(6,*)"Vol=", Vol(i,j,k)
!        write(6,*)"Vol_prev=",Vol_prev(i,j,k)
!      endif
!#endif


! -------------------------------------------------------------
              end do	! k = 1, nz
            endif !fm = 1
          end do !i
        end do !j
      end do   ! ii = 1,nf


! update f for the current timestep
      !do k = 1,nz
        !do j = 2,jm-1
        !  do i = 2, im-1 !my_imstart,my_imend
        do j = 1,jm
          do i = 1, im !my_imstart,my_imend
            do k=1,fm2(i,j)   !Will not execute loop if fm2=0
              f(i,j,k,:) = ff(i,j,k,:)
            enddo
          enddo
        enddo
      !enddo

      RETURN 
      END 
