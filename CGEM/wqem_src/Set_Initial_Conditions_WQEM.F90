       Subroutine Set_Initial_Conditions_WQEM(init_filename,myid,numprocs) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow
       USE states
       USE Grid
       USE State_Vars
       USE BoundaryConcentration

       IMPLICIT NONE

       character(200) filename
       character(120), intent(in) :: init_filename
       integer, intent(in) :: myid, numprocs
       integer :: i, j, k, nz, myi
       integer :: ibc, jbc    ! Indices of boundary grid cells.

       if(InitializeHow.eq.0) then

        write(filename,'(A,A,A)') trim(DATADIR),"/",trim(init_filename)

        call USER_Set_Initial_Conditions(filename,myid,numprocs) 
!        do j=1,jm
!          myi=1
!        do i=myi_start,myi_end
!           do k=1,nza(i,j)
!             f(myi,j,k,JTR) = 1./Vol(i,j,k)
!           enddo
!           myi = myi + 1
!        enddo
!        enddo

       elseif(InitializeHow.eq.1) then !Salinity Regression Equations

        call Salinity_Regression_Init_WQEM()
        do j=1,jm
          myi=1
          do i=myi_start,myi_end
           do k=1,nza(i,j)
             f(myi,j,k,JTR) = 1./Vol(i,j,k)
           enddo
           myi = myi + 1
          enddo
        enddo

       endif

       ! Initialize concentrations of boundary cells
       do i = 1, nBC            ! Loop over boundary cells
          ibc = bcIJ(i,1)  ! Extract the i index of grid cell 
          jbc = bcIJ(i,2)  ! Extract the j index of grid cell 
          if ((ibc .ge. myi_start) .and. (ibc .le. myi_end)) then
             myi = ibc - myi_start + 1
             nz = nza(myi,jbc)
             do k = 1, nz       ! Loop over the sigma layers
                f(myi,jbc,k,JNO3) = BC1(i) * 1.0E-03  ! Convert mg/L to kg/m3
                f(myi,jbc,k,JNH4) = BC2(i) * 1.0E-03
                f(myi,jbc,k,JDON) = BC3(i) * 1.0E-03
                f(myi,jbc,k,JTR)  = BC4(i) * 1.0E-03  
                f(myi,jbc,k,JSRP) = BC5(i) * 1.0E-03
                f(myi,jbc,k,JDOP) = BC6(i) * 1.0E-03
                f(myi,jbc,k,JDO2) = BC7(i) * 1.0E-03
             enddo
          endif
       enddo

#ifdef DEBUG
      write(6,*) "In Set Initial Conditions WQEM,myid=",myid
      write(6,*)
#endif

       return

       End Subroutine Set_Initial_Conditions_WQEM
