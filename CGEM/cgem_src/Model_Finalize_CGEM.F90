       Subroutine Model_Finalize_CGEM(myid,numprocs)

       USE OUTPUT_NETCDF_CGEM
       USE INPUT_VARS_CGEM
       USE Model_Compare

       IMPLICIT NONE

       integer, intent(in) :: myid,numprocs

       IF (.FALSE.) write(6,*) "myid, numprocs = ", myid, numprocs

         CALL CLOSE_FILE()

!For a model inter-comparison study through the Coastal Ocean Modeling Testbed
       if (MC.eq.1) call CLOSE_FILE_MC()

       return

       End Subroutine Model_Finalize_CGEM
