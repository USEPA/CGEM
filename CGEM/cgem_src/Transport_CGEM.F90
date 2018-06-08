       Subroutine Transport_CGEM(myid,numprocs)

       USE Model_dim
       USE INPUT_VARS_CGEM, ONLY: KH_coeff
       USE INPUT_VARS, ONLY: Which_VMix
       USE State_Vars
       USE CGEM_Vars, ONLY: iQp, iQn, iA
       USE Hydro, ONLY: Kh

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs 
       integer :: i, j, k, nz, myi
 
       if(which_gridio.ne.0) then

       ! Before Advection and VMixing, combine A's and Q's
         do j=1, jm
           myi = 1
           do i=myi_start, myi_end 
             nz = nza(i,j)
             do k=1,nz
             f(myi,j,k,iQn(:)) = f(myi,j,k,iQn(:)) * f(myi,j,k,iA(:))
             f(myi,j,k,iQp(:)) = f(myi,j,k,iQp(:)) * f(myi,j,k,iA(:))
             enddo
             myi = myi + 1
         enddo
       enddo

       !Advection and Vmixing

       !Needs to call advection because that is where sinking is:
        call Adv3D(myid,numprocs) 

      !Multiply Kh by KH_coeff for h<30
        !L3...Fix this so it is only for h<30
         !!Kh(:,:,1:nz) = Kh(:,:,1:nz)*KH_coeff
       if(Which_VMix.ne.0) call VMixing()

       ! After Advection and VMixing, return to Q's
         do j=1, jm
           myi = 1
           do i=myi_start, myi_end 
             nz = nza(i,j)
             do k=1,nz
             f(myi,j,k,iQn(:)) = f(myi,j,k,iQn(:)) / f(myi,j,k,iA(:))
             f(myi,j,k,iQp(:)) = f(myi,j,k,iQp(:)) / f(myi,j,k,iA(:))
             enddo
            myi = myi + 1
         enddo
       enddo

       endif

       return

       End Subroutine Transport_CGEM 
