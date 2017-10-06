       Subroutine Transport_CGEM()

       USE Model_dim
       USE INPUT_VARS_CGEM, ONLY: KH_coeff
       USE INPUT_VARS, ONLY: Which_VMix
       USE State_Vars
       USE CGEM_Vars, ONLY: iQp, iQn, iA
       USE Hydro, ONLY: Kh

       IMPLICIT NONE

       integer :: i, j, k, nz 
 
       if(which_gridio.ne.0) then

       ! Before Advection and VMixing, combine A's and Q's
         do j=1, jm
           do i=1, im
             nz = nza(i,j)
             do k=1,nz
             f(i,j,k,iQn(:)) = f(i,j,k,iQn(:)) * f(i,j,k,iA(:))
             f(i,j,k,iQp(:)) = f(i,j,k,iQp(:)) * f(i,j,k,iA(:))
           enddo
         enddo
       enddo

       !Advection and Vmixing

       !Needs to call advection because that is where sinking is:
       if(Which_gridio.eq.1) then
        call Adv3D_EFDC()
       else
        call Adv3D()
       endif

      !Multiply Kh by KH_coeff for h<30
        !L3...Fix this so it is only for h<30
         !!Kh(:,:,1:nz) = Kh(:,:,1:nz)*KH_coeff
       if(Which_VMix.ne.0) call VMixing()

       ! After Advection and VMixing, return to Q's
         do j=1, jm
           do i=1, im
             nz = nza(i,j)
             do k=1,nz
             f(i,j,k,iQn(:)) = f(i,j,k,iQn(:)) / f(i,j,k,iA(:))
             f(i,j,k,iQp(:)) = f(i,j,k,iQp(:)) / f(i,j,k,iA(:))
           enddo
         enddo
       enddo

       endif

       return

       End Subroutine Transport_CGEM 
