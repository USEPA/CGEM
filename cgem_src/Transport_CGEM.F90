       Subroutine Transport_CGEM()

       USE Model_dim
       USE INPUT_VARS_CGEM, ONLY: KH_coeff
       USE State_Vars
       USE CGEM_Vars, ONLY: iQp, iQn, iA
       USE Hydro, ONLY: Kh

       IMPLICIT NONE

       integer :: i, j, k

       ! Before Advection and VMixing, combine A's and Q's
       do k=1, nsl
         do j=1, jm
           do i=1, im
             f(i,j,k,iQn(:)) = f(i,j,k,iQn(:)) * f(i,j,k,iA(:))
             f(i,j,k,iQp(:)) = f(i,j,k,iQp(:)) * f(i,j,k,iA(:))
           enddo
         enddo
       enddo

       !Advection and Vmixing

!       call Adv3D()

      !Multiply Kh by KH_coeff for h<30
      !Kh(:,:,1:nsl-1) = Kh(:,:,1:nsl-1)*KH_coeff(:,:,1:nsl-1)
       Kh(:,:,1:nsl-1) = Kh(:,:,1:nsl-1)*KH_coeff

       call VMixing()

       ! After Advection and VMixing, return to Q's
       do k=1, nsl
         do j=1, jm
           do i=1, im
             f(i,j,k,iQn(:)) = f(i,j,k,iQn(:)) / f(i,j,k,iA(:))
             f(i,j,k,iQp(:)) = f(i,j,k,iQp(:)) / f(i,j,k,iA(:))
           enddo
         enddo
       enddo

       return

       End Subroutine Transport_CGEM 
