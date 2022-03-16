       Subroutine Transport_CGEM(myid,numprocs)

       USE Model_dim
       USE INPUT_VARS_CGEM, ONLY: KH_coeff
       USE INPUT_VARS, ONLY: Which_VMix
       USE State_Vars
       USE CGEM_Vars, ONLY: iQp, iQn, iA, iNO3, iNH4, iPO4, iO2
       USE Hydro, ONLY: Kh
       USE BoundaryConcentration

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs 
       integer :: i, j, k, nz, myi
       integer :: ibc, jbc        ! Indices of boundary grid cells.
 
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


       ! Reset concentrations of boundary cells
       do i = 1, nBC            ! Loop over boundary cells
          ibc = bcIJ(i,1)  
          jbc = bcIJ(i,2)  ! Extract the j index of grid cell
          if ((ibc .ge. myi_start) .and. (ibc .le. myi_end)) then
              myi = ibc - myi_start + 1 
              nz = nza(myi,jbc)
              do k = 1, nz          ! Loop over the sigma layers
                 f(myi,jbc,k,iNO3) = BC2(i) * 1.0e3 / 14.01
                 f(myi,jbc,k,iNH4) = BC3(i) * 1.0e3 / 14.01
                 f(myi,jbc,k,iPO4) = BC6(i) * 1.0e3 / 30.97
                 f(myi,jbc,k,iO2) =  BC9(i) * 1.0e3 / 32.0
              enddo
          endif
       enddo


       endif

       return

       End Subroutine Transport_CGEM 
