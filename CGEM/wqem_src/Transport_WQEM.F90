       Subroutine Transport_WQEM(myid,numprocs)

       USE Model_dim
       USE INPUT_VARS, ONLY: Which_VMix
       USE State_Vars
       USE STATES
       USE BoundaryConcentration

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs
       integer :: i, k, nz, myi
       integer :: ibc, jbc        ! Indices of boundary grid cells.

       !Advection and Vmixing
       if (which_gridio.ne.0) then  

          call Adv3D(myid,numprocs)
      
          if(Which_VMix.ne.0) call VMixing()

          ! Reset concentrations of boundary cells
          do i = 1, nBC            ! Loop over boundary cells
             ibc = bcIJ(i,1)  ! Extract the i index of grid cell 
             jbc = bcIJ(i,2)  ! Extract the j index of grid cell 
             if ((ibc .ge. myi_start) .and. (ibc .le. myi_end)) then
                myi = ibc - myi_start + 1
                nz = nza(myi,jbc)
                do k = 1, nz       ! Loop over the sigma layers
                   f(myi,jbc,k,JTR) = BC1(i) 
                enddo
             endif
          enddo

       endif

       return

       End Subroutine Transport_WQEM 
