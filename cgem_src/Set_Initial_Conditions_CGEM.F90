       Subroutine Set_Initial_Conditions_CGEM(init_filename) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow
#ifdef DEBUG
       USE INPUT_VARS, ONLY:START_SECONDS, dT
#endif


       IMPLICIT NONE

       character(200) filename
       character(120), intent(in) :: init_filename


#ifdef DEBUG
      write(6,*) "In Set Initial Conditions"
      write(6,*) "start,dT",START_SECONDS, dT
#endif


       if(InitializeHow.eq.0) then 

        write(filename,'(A,A,A)') trim(DATADIR),"/",trim(init_filename)

        call USER_Set_Initial_Conditions(filename) 

       elseif(InitializeHow.eq.1) then !Salinity Regression Equations

        call Salinity_Regression_Init_CGEM()

       else
          write(6,*) "Unknown, InitializeHow=",InitializeHow," exiting"
          stop
       endif

       call InitError_Check_CGEM()

       return

       End Subroutine Set_Initial_Conditions_CGEM 
