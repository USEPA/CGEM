      Subroutine Model_Output_CGEM(istep_out,myid,numprocs)

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_CGEM
      USE Grid
      USE CGEM_vars

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter
      integer, intent(in) :: myid, numprocs
      real :: dumf(myim,jm,km,nf)
      
      IF (.FALSE.) WRITE(6,*) "myid, numprocs = ", myid, numprocs

      dumf = f(1:myim,:,:,:)

      CALL WRITE_DATA( myi_start, myim, 1, jm, 1, km, istep_out, dumf)

      return

      End Subroutine
