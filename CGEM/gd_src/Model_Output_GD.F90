      Subroutine Model_Output_GD(istep_out)

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_GD

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter

        CALL WRITE_DATA( im, jm, nsl, nf, istep_out, f)
        CALL WRITE_EXTRA_DATA( IM, JM, NSL, EXTRA_VARIABLES, istep_out) 

      return

      End Subroutine
