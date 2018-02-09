       Subroutine Set_Initial_Conditions_CGEM(init_filename) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow
#ifdef DEBUG
       USE INPUT_VARS, ONLY:START_SECONDS, dT
#endif
       USE CGEM_Vars
       USE Grid 
       USE State_Vars

       IMPLICIT NONE

       character(200) filename
       character(120), intent(in) :: init_filename
       integer i,j,k

       if(InitializeHow.eq.0) then 

        write(filename,'(A,A,A)') trim(DATADIR),"/",trim(init_filename)

        call USER_Set_Initial_Conditions(filename) 
        do j=1,jm
        do i=1,im
        do k=1,nza(i,j)
        f(i,j,k,iTr) = 1./Vol(i,j,k)
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

       !call InitError_Check_CGEM()
#ifdef DEBUG
      write(6,*) "In Set Initial Conditions"
      write(6,*) "start,dT",START_SECONDS, dT
#endif
       return

       End Subroutine Set_Initial_Conditions_CGEM 
