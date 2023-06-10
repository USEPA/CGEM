      Subroutine Model_Output_WQEM(istep_out,myid,numprocs)

      !
      ! 02/21/2020  Wilson Melendez, Commented out loop that was
      !             recalcualting dumf for the tracer case for
      !             unknown reason

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_WQEM
      USE Grid
      USE states 
      USE OUTPUT

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter
      integer,intent(in) :: myid,numprocs
      real :: dumf(myim,jm,km,nf)

      IF (.FALSE.) WRITE(6,*) "myid, numprocs = ", myid, numprocs

      dumf = f(1:myim,:,:,:)

      CALL WRITE_DATA( myi_start,myim, 1, jm, 1, km, nf, istep_out, dumf)
      CALL WRITE_EXTRA_DATA( myi_start, myIM,1, JM, 1,KM, EXTRA_VARIABLES, istep_out) 

      return

      End Subroutine
