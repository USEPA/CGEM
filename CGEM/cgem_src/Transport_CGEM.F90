       Subroutine Transport_CGEM()

       USE Model_dim
       USE INPUT_VARS_CGEM, ONLY: KH_coeff,Which_VMix
       USE State_Vars
       USE CGEM_Vars, ONLY: iQp, iQn, iA
       USE Hydro, ONLY: Kh

       IMPLICIT NONE

       integer :: i, j, k
 
       if(which_gridio.ne.0) then

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

       !Needs to call advection because that is where sinking is:
       call Adv3D()

#ifdef DEBUG
write(6,*) "After Adv3D"
write(6,*) "CDOM",f(1,1,1:nsl,32)
#endif

      !Multiply Kh by KH_coeff for h<30
        !L3...Fix this so it is only for h<30
        Kh(:,:,1:nsl) = Kh(:,:,1:nsl)*KH_coeff

       if(Which_VMix.ne.0) call VMixing()

#ifdef DEBUG
write(6,*) "After VMixing"
write(6,*) "CDOM",f(1,1,1:nsl,32)
#endif


       ! After Advection and VMixing, return to Q's
       do k=1, nsl
         do j=1, jm
           do i=1, im
             f(i,j,k,iQn(:)) = f(i,j,k,iQn(:)) / f(i,j,k,iA(:))
             f(i,j,k,iQp(:)) = f(i,j,k,iQp(:)) / f(i,j,k,iA(:))
           enddo
         enddo
       enddo

       endif

       return

       End Subroutine Transport_CGEM 
