       Subroutine Transport_CGEM()

       USE Model_dim
       USE INPUT_VARS_CGEM, ONLY: KH_coeff
       USE INPUT_VARS, ONLY: Which_VMix
       USE State_Vars
       USE CGEM_Vars, ONLY: iQp, iQn, iA, iNO3, iNH4, iPO4, iO2
       USE Hydro, ONLY: Kh
       USE BoundaryConcentration

       IMPLICIT NONE

       integer :: i, j, k, nz 
       integer ::  ibc, jbc      ! Indices of boundary grid cells.
 
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
        call Adv3D() 

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

       ! Reset concentrations of boundary cells
       do i = 1, nBC            ! Loop over boundary cells
          ibc = bcIJ(i,1)  
          jbc = bcIJ(i,2)  ! Extract the j index of grid cell 
          nz = nza(ibc,jbc)
          do k = 1, nz          ! Loop over the sigma layers
             f(ibc,jbc,k,iNO3) = BCvar2(i) * 1.0e3 / 14.01
             f(ibc,jbc,k,iNH4) = BCvar3(i) * 1.0e3 / 14.01
             f(ibc,jbc,k,iPO4) = BCvar6(i) * 1.0e3 / 30.97
             f(ibc,jbc,k,iO2) =  BCvar9(i) * 1.0e3 / 32.0
          enddo
       enddo

       endif

       return

       End Subroutine Transport_CGEM 
