       Subroutine Set_Initial_Conditions_GD(init_filename,myid,numprocs) 

       USE Model_dim
       USE INPUT_VARS, ONLY: InitializeHow, icent, jcent 
       USE states
       USE Grid
       USE State_Vars

       IMPLICIT NONE

       character(200) filename
       character(120), intent(in) :: init_filename
       integer, intent(in) :: myid, numprocs
       integer :: i,j,k,myi

       if(InitializeHow.eq.0) then

        write(filename,'(A,A,A)') trim(DATADIR),"/",trim(init_filename)

        call USER_Set_Initial_Conditions(filename,myid,numprocs) 
        do j=1,jm
          myi=1
        do i=myi_start,myi_end
           do k=1,nza(i,j)
             f(myi,j,k,JTR) = 1./Vol(i,j,k)
           enddo
           myi = myi + 1
        enddo
        enddo

       elseif(InitializeHow.eq.1) then !Salinity Regression Equations

        call Salinity_Regression_Init_GD()
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

#ifdef DEBUG
      write(6,*) "In Set Initial Conditions GD,myid=",myid
      write(6,*) "first state var at i,j,k=",f(icent,jcent,1,1),icent,jcent,1
      write(6,*)
#endif

       return

       End Subroutine Set_Initial_Conditions_GD
