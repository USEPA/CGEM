       Subroutine Model_Finalize(Which_code,Which_gridio)

       use Hydro, only:close_hydro_netcdf
       use Grid, only:close_grid_netcdf

       IMPLICIT NONE

       character(6), intent(in) :: Which_code
       integer, intent(in) :: Which_gridio

      !Close EFDC Hydro is the same for any WQM:
      if (Which_gridio.ne.0) then
        Call Close_Hydro_NetCDF()
        Call Close_Grid_NetCDF()
      endif

      !Right now these just close the output netCDF files
      if(Which_code.eq."CGEM") then !CGEM
        call Model_Finalize_CGEM()
      else if(Which_code.eq."GOMDOM") then !GOMDOM
        call Model_Finalize_GD()
      else
        write(6,*) "Model ",Which_code," not found, Exiting."
        stop
      endif

      return

      End Subroutine Model_Finalize
