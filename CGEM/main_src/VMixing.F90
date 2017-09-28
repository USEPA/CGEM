! ----------------------------------------------------------------------
      Subroutine  VMixing ()
!     Modified by Cody Simmons/EMVL
!     Originally written by D.S.Ko/NRL
! ----------------------------------------------------------------------
!***********************************************************************
!     Solve Conservation Equation for Scalar
!     dF/dt = d(Kh*dF/dz)/dz
!***********************************************************************

      USE Model_dim
      USE INPUT_VARS, ONLY: dT
      USE Grid, ONLY: dz,d_sfc,d,dzz
      USE State_Vars
      USE Hydro, ONLY: Kh
#ifdef DEBUG
      USE INPUT_VARS, ONLY: icent,jcent,Which_VMix
#endif

      IMPLICIT NONE

      integer i,j,k,ii,nz

! --- Tmp:
      real  A(50),C(50),E(50),G(50)
      real  Gk(50)

#ifdef DEBUG
write(6,*) "---VMixing---"
write(6,*) "  Which_VMix=",Which_VMix
write(6,*) "  nz=",km
write(6,*) "  At the cell i,j,k=",icent,jcent,km
write(6,*) "    mixing coeff Kh is:",Kh(icent,jcent,km)
write(6,*)
#endif

       do j = 1, jm
         do i = 1, im
             nz = nza(i,j)
             do k = 2, nz
                 A(k-1) = -dT*Kh(i,j,k)                       &
            &                /(dz(i,j,k-1)*(d_sfc(i,j,k)-d_sfc(i,j,k-1)))           
       
                 C(k  ) = -dT*Kh(i,j,k)                       &
            &                /(dz(i,j,k)*(d_sfc(i,j,k)-d_sfc(i,j,k-1)))            
             end do
             E(1) = A(1)/(A(1)-1.)
             do k=2, nz-1
               Gk(k)= 1./((A(k)+C(k)*(1.-E(k-1)))-1.)
               E(k) = A(k)*Gk(k)
             end do
       
             do ii = 1, nf
       
               ! --- No flux at surface
               G(1) = -f(i,j,1,ii)/(A(1)-1.)
               do k=2,nz-1
                 G(k) = (C(k)*G(k-1)-f(i,j,k,ii))*Gk(k)
               end do
               ! --- No flux at bottom
               f(i,j,nz,ii) = (C(nz)*G(nz-1)-f(i,j,nz,ii)) &
              &              /(C(nz)*(1.-E(nz-1))-1.)
         
               do k=nz-1, 1, -1
                 f(i,j,k,ii) = E(k)*f(i,j,k+1,ii)+G(k)
               end do
       
             end do

        enddo
      enddo


      return
      end

