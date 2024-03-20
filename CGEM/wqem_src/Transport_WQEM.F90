       Subroutine Transport_WQEM(myid,numprocs)
       !
       ! This subroutine calls the advection and vertical mixing subroutines.
       ! It also resets the boundary concentrations (if using boundary
       ! conditions).
       !
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
             PRINT*, "Inside transport: i, ibc, jbc = ", i, ibc, jbc
             if ((ibc .ge. myi_start) .and. (ibc .le. myi_end)) then
                myi = ibc - myi_start + 1
                nz = nza(myi,jbc)
                PRINT*, "Inside transport: myi, nz = ", myi, nz
                PRINT*, "f(myi,jbc,1,JDOC) = ", f(myi,jbc,1,JDOC)
                do k = 1, nz       ! Loop over the sigma layers
                   f(myi,jbc,k,JNO3) = BC1(i) * 1.0E-03  ! Convert mg/L to kg/m3
                   f(myi,jbc,k,JNH4) = BC2(i) * 1.0E-03
                   f(myi,jbc,k,JDON) = BC3(i) * 1.0E-03
                   f(myi,jbc,k,JTR)  = BC4(i) * 1.0E-03
                   f(myi,jbc,k,JSRP) = BC5(i) * 1.0E-03
                   f(myi,jbc,k,JDOP) = BC6(i) * 1.0E-03
                   f(myi,jbc,k,JDO2) = BC7(i) * 1.0E-03
                   f(myi,jbc,k,JDOC) = BC8(i) * 1.0E-03
                   f(myi,jbc,k,JDIA) = BC9(i) * 1.0E-03
                   f(myi,jbc,k,JGRE) = BC10(i) * 1.0E-03
                   f(myi,jbc,k,JZOO) = BC11(i) * 1.0E-03
                   f(myi,jbc,k,JLOC) = BC12(i) * 1.0E-03
                   f(myi,jbc,k,JROC) = BC13(i) * 1.0E-03
                   f(myi,jbc,k,JLOP) = BC14(i) * 1.0E-03
                   f(myi,jbc,k,JROP) = BC15(i) * 1.0E-03
                   f(myi,jbc,k,JLON) = BC16(i) * 1.0E-03
                   f(myi,jbc,k,JRON) = BC17(i) * 1.0E-03
                   f(myi,jbc,k,JSA)  = BC18(i) * 1.0E-03
                   f(myi,jbc,k,JSU)  = BC19(i) * 1.0E-03
                   f(myi,jbc,k,JDIAN) = BC20(i) * 1.0E-03
                   f(myi,jbc,k,JDIAP) = BC21(i) * 1.0E-03
                   f(myi,jbc,k,JGREN) = BC22(i) * 1.0E-03
                   f(myi,jbc,k,JGREP) = BC23(i) * 1.0E-03   
                enddo
                PRINT*, "f(myi,jbc,k,JDOC) = ", f(myi,jbc,k,JDOC)
             endif
          enddo

       endif

       return

       End Subroutine Transport_WQEM 
