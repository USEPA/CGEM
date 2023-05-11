       Subroutine Set_Initial_Conditions_CGEM(init_filename,myid,numprocs) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow
       USE CGEM_Vars
       USE Grid 
       USE State_Vars
       USE BoundaryConcentration

       IMPLICIT NONE

       integer, intent(in) :: myid, numprocs
       character(200) :: filename
       character(120), intent(in) :: init_filename
       integer i,j,k,nz,myi
       integer :: ibc, jbc    ! Indices of boundary grid cells.

       if (InitializeHow == 0) then 
          write(filename,'(A,A,A)') trim(DATADIR),"/",trim(init_filename)
          call USER_Set_Initial_Conditions(filename,myid,numprocs) 
       elseif (InitializeHow == 1) then !Salinity Regression Equations
          call Salinity_Regression_Init_CGEM()
          do j = 1, jm
             myi = 1
             do i = myi_start, myi_end
                do k = 1, nza(i,j)
                   f(myi,j,k,iTr) = 1./Vol(i,j,k)
                enddo
                myi = myi + 1
             enddo
          enddo
       else
          write(6,*) "Unknown, InitializeHow = ", InitializeHow," exiting"
          stop
       endif

       ! Initialize concentrations of boundary cells
       do i = 1, nBC            ! Loop over boundary cells
          ibc = bcIJ(i,1)  ! Extract the i index of grid cell 
          jbc = bcIJ(i,2)  ! Extract the j index of grid cell 
          if ((ibc .ge. myi_start) .and. (ibc .le. myi_end)) then
             myi = ibc - myi_start + 1
             nz = nza(myi,jbc)
             do k = 1, nz       ! Loop over the sigma layers
                f(myi,jbc,k,iNO3) = BC2(i) * 1.0e3 / 14.01
                f(myi,jbc,k,iNH4) = BC3(i) * 1.0e3 / 14.01
                f(myi,jbc,k,iPO4) = BC6(i) * 1.0e3 / 30.97
                f(myi,jbc,k,iO2) =  BC9(i) * 1.0e3 / 32.0
             enddo
          endif
       enddo


       !call InitError_Check_CGEM()
#ifdef DEBUG
      write(6,*) "In Set Initial Conditions CGEM,myid=",myid
      write(6,*) "first state var at i,j,k=",f(1,1,1,1),1,1,1
      write(6,*)
#endif
       return

       End Subroutine Set_Initial_Conditions_CGEM 
