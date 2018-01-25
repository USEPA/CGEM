       Subroutine Set_Initial_Conditions_GD(init_filename) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow
       USE states
       USE Grid
       USE State_Vars
#ifdef DEBUG
       USE INPUT_VARS, ONLY: START_SECONDS, dT
#endif
       IMPLICIT NONE

       character(200) filename
       character(120), intent(in) :: init_filename
       integer :: i,j,k

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
         f(i,j,k,JTR) = 1./Vol(i,j,k)
        enddo
        enddo
        enddo

       elseif(InitializeHow.eq.1) then !Salinity Regression Equations

        call Salinity_Regression_Init_GD()
        do j=1,jm
        do i=1,im
        do k=1,nza(i,j)
         f(i,j,k,JTR) = 1./Vol(i,j,k)
        enddo
        enddo
        enddo

       endif

       return

       End Subroutine Set_Initial_Conditions_GD
