       Subroutine Set_Initial_Conditions_CGEM(init_filename) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow
#ifdef DEBUG
       USE INPUT_VARS, ONLY:START_SECONDS, dT
#endif
       USE CGEM_Vars
       USE Grid 
       USE State_Vars
       USE BoundaryConcentration

       IMPLICIT NONE

       character(200) filename
       character(120), intent(in) :: init_filename
       integer i,j,k, nz
       integer ::  ibc, jbc      ! Indices of boundary grid cells.

#ifdef DEBUG
      write(6,*) "In Set Initial Conditions"
      write(6,*) "start,dT",START_SECONDS, dT
#endif


       if(InitializeHow.eq.0) then 

        write(filename,'(A,A,A)') trim(DATADIR),"/",trim(init_filename)

        call USER_Set_Initial_Conditions(filename) 
        do j=1,jm
        do i=1,im
        do k=1,nza(i,j)
        !f(i,j,k,:)= f(i,j,k,:)/Vol(i,j,k)*Vol(25,16,3)
        f(i,j,k,iTr) = 1./Vol(i,j,k)
!        write(6,*) "i,j,k,vol",i,j,k,Vol(i,j,k)
        enddo
        enddo
        enddo


       elseif(InitializeHow.eq.1) then !Salinity Regression Equations

        call Salinity_Regression_Init_CGEM()

        do j=1,jm
        do i=1,im
        do k=1,nza(i,j)
        f(i,j,k,iTr) = 1./Vol(i,j,k)
        enddo
        enddo
        enddo

       else
          write(6,*) "Unknown, InitializeHow=",InitializeHow," exiting"
          stop
       endif

       ! Initialize concentrations of boundary cells
       do i = 1, nBC            ! Loop over boundary cells
          ibc = bcIJ(i,1)  ! Extract the i index of grid cell 
          jbc = bcIJ(i,2)  ! Extract the j index of grid cell 
          nz = nza(ibc,jbc)
          do k = 1, nz       ! Loop over the sigma layers
             f(ibc,jbc,k,iNO3) = BCvar2(i) * 1.0e3 / 14.01
             f(ibc,jbc,k,iNH4) = BCvar3(i) * 1.0e3 / 14.01
             f(ibc,jbc,k,iPO4) = BCvar6(i) * 1.0e3 / 30.97
             f(ibc,jbc,k,iO2) =  BCvar9(i) * 1.0e3 / 32.0
          enddo
       enddo


       !call InitError_Check_CGEM()

       return

       End Subroutine Set_Initial_Conditions_CGEM 
