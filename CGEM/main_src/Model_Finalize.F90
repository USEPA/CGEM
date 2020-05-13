       Subroutine Model_Finalize(Which_code,Which_gridio,myid,numprocs)

       use Hydro, only:close_hydro_netcdf
       use Grid, only:close_grid_netcdf
       USE Riverload, only:Close_RiverLoad_NetCDF
       USE BoundaryConcentration, only:Close_BoundaryConcentration_NetCDF

       IMPLICIT NONE

       character(6), intent(in) :: Which_code
       integer, intent(in) :: Which_gridio,myid,numprocs

      !Close EFDC Hydro is the same for any WQM:
      if (Which_gridio.ne.0.and.myid.eq.0) then
        Call Close_Hydro_NetCDF()
        Call Close_Grid_NetCDF()
        if (Which_gridio.eq.1) then
          Call Close_RiverLoad_NetCDF()
          Call Close_BoundaryConcentration_NetCDF()
        endif
      endif

      !Right now these just close the output netCDF files
      if(Which_code.eq."CGEM") then !CGEM
        call Model_Finalize_CGEM(myid,numprocs)
      else if(Which_code.eq."GOMDOM") then !GOMDOM
        call Model_Finalize_GD(myid,numprocs)
      else
        write(6,*) "Model ",Which_code," not found, Exiting."
        stop
      endif

      return

      End Subroutine Model_Finalize
