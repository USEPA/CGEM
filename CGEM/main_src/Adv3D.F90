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

      integer :: i,j,k,ii
      integer, dimension(im,jm) :: fm2
      real, dimension(im,jm,nsl,nf) :: ff !,f
      real, dimension (im,jm,nsl,nf) :: fmod 
   
      integer :: mv

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
      mv = -9999


       fm2 = fm


!#ifdef DEBUG_CWS
!      !Vol = 1
!      !Vol_prev = 1
!      Ux = 0 
!      Vx = 0 
!      Wx = -1   !-0.001
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
            if(fm2(i,j).ne.0) then

! --------------------------------------------------------

!     w_wsink means 'w' with sinking terms
!     at surface sink = 0.
              w_wsink (i,j,1) = Wx(i,j,1)

              !nz=nza(i,j)-1
              !do k = 2, nz 
              do k = 2, fm2(i,j)
                w_wsink(i,j,k) = Wx(i,j,k) + ws(ii)*area(i,j)  !dxdy(i,j)
              end do

!If wsm=0 (shelf), then set sinking velocity to zero as well...
!     at bottom (w=0) add settling or deep ocean (wsm=1)
              !w_wsink (i,j,nz+1) = Wx(i,j,nz+1)+ ws(ii)*wsm(i,j)*dxdy(i,j)
              w_wsink (i,j,fm2(i,j)+1) = Wx(i,j,fm2(i,j)+1)+ ws(ii)*wsm(i,j)*area(i,j)  !dxdy(i,j)


! -------------------------------------------------------------
              !do k = 1, nz	! do layer by layer
              do k = 1, fm2(i,j)	! do layer by layer
                km1 = max0(k-1,1)

! ----- upwind advection
                !check that data is defined on i-1 cell

              
                ufm = amax1( Ux(i  ,j,k),0.)	! pp/n0
                ufp = amax1(-Ux(i+1,j,k),0.)	! p0/np
                vfm = amax1( Vx(i  ,j,k),0.)
                vfp = amax1(-Vx(i,j+1,k),0.)
                wfm = amax1(-w_wsink(i,j,k  ),0.)	! p0/np
                wfp = amax1( w_wsink(i,j,k+1),0.)	! pp/n0


                if (fm2(i-1,j) .ge. k) then
                  fmod_im = f(i-1,j,k,ii)
                else 
                  fmod_im = f(i,j,k,ii)
                endif

                if (fm2(i+1,j) .ge. k) then
                  fmod_ip = f(i+1,j,k,ii)
                else
                  fmod_ip = f(i,j,k,ii)
                endif

                if (fm2(i,j-1) .ge. k) then
                  fmod_jm = f(i,j-1,k,ii)
                else
                  fmod_jm = f(i,j,k,ii)
                endif

                if (fm2(i,j+1) .ge. k) then
                  fmod_jp = f(i,j+1,k,ii)
                else 
                  fmod_jp = f(i,j,k,ii)
                endif

                if (fm2(i,j) .gt. k) then
                  fmod_wp = f(i,j,k+1,ii)
                else
                  fmod_wp = f(i,j,k,ii)
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


                tmpVal1 = f(i,j,k,ii)*Vol_prev(i,j,k)
                tmpVal2 = cf*f(i,j,k,ii)*dT
                tmpVal3 = ((ufm*fmod_im+ufp*fmod_ip)       &
     &            +(vfm*fmod_jm+vfp*fmod_jp))*dT
                tmpVal4 = (wfm*f(i,j,km1,ii)+wfp*fmod_wp)*dT

                 ff(i,j,k,ii) = (tmpVal1 + tmpVal2 + tmpVal3 +tmpVal4)/Vol(i,j,k)

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
