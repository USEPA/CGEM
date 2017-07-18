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
      USE Grid, ONLY: fm,dz,d_sfc
      !USE INPUT_VARS_CGEM
      USE State_Vars
      !USE CGEM_Vars
      USE Hydro, ONLY: Kh

      IMPLICIT NONE

      integer i,j,k,ii,nz  !,fm(im,jm)

! --- Tmp:
      real  A(50),C(50),E(50),G(50)
      real  Gk(50)
!     real  Kh0 		! Background
!     parameter (Kh0=1.e-6)	! m**2/s


!      nz = nsl -1

       do j = 1, jm
         do i = 1, im
       !    if(fm(i,j).eq.1) then
           !  do k=2,fm(i,j)
             do k = 2, fm(i,j)
!              A(k-1) = -dT*(Kh(i,j,k)+Kh0)
               A(k-1) = -dT*Kh(i,j,k)                       &
            &              /(dz(i,j,k-1)*(d_sfc(i,j,k)-d_sfc(i,j,k-1)))           
!           &              /(dz(k-1)*dzz(k-1)*d(i,j)*d(i,j))
       
!              C(k  ) = -dT*(Kh(i,j,k)+Kh0)
               C(k  ) = -dT*Kh(i,j,k)                       &
            &              /(dz(i,j,k)*(d_sfc(i,j,k)-d_sfc(i,j,k-1)))            
!           &              /(dz(k  )*dzz(k-1)*d(i,j)*d(i,j))
       
             end do
             E(1) = A(1)/(A(1)-1.)
             do k=2, fm(i,j)-1
             !do k=2, nz-1
               Gk(k)= 1./((A(k)+C(k)*(1.-E(k-1)))-1.)
               E(k) = A(k)*Gk(k)
             end do
       
             do ii = 1, nf
       
               ! --- No flux at surface
               G(1) = -f(i,j,1,ii)/(A(1)-1.)
               do k=2,fm(i,j)-1
               !do k=2,nz-1
                 G(k) = (C(k)*G(k-1)-f(i,j,k,ii))*Gk(k)
               end do
               ! --- No flux at bottom
               f(i,j,fm(i,j),ii) = (C(fm(i,j))*G(fm(i,j)-1)-f(i,j,fm(i,j),ii)) &
              &              /(C(fm(i,j))*(1.-E(fm(i,j)-1))-1.)
         
               do k=fm(i,j)-1, 1, -1
               !do k = nz-1, 1, -1
                 f(i,j,k,ii) = E(k)*f(i,j,k+1,ii)+G(k)
               end do
       
             end do

        !  endif
        enddo
      enddo


      return
      end

