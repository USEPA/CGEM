       Subroutine Init_Output_GD(BASE_NETCDF_OUTPUT_FILE_NAME)

       USE Model_dim
       USE INPUT_VARS, ONLY : nstep,dT_out, IYRS, IMONS,&
     & IDAYS, IHRS, IMINS, ISECS, IYRE, IMONE, IDAYE, IHRE,&
     & IMINE, ISECE
       USE Grid
       USE State_Vars
       USE OUTPUT_NETCDF_GD

       IMPLICIT NONE

       character(100) :: BASE_NETCDF_OUTPUT_FILE_NAME
       character(256) :: NETCDF_OUTPUT_FILE_NAME

       ! Change True/False parameters for netCDF Write Variables
       !L3 add Which_Output to GD InputFile
       !if(Which_Output.eq.2) call OUTPUT_ALL_FALSE() 

       WRITE ( NETCDF_OUTPUT_FILE_NAME, '(A, I6.6, A)' )&
              trim(BASE_NETCDF_OUTPUT_FILE_NAME), 0, '.nc'

          CALL CREATE_FILE( trim(NETCDF_OUTPUT_FILE_NAME), &
                            im, jm, nsl, nstep, nf, EXTRA_VARIABLES, &
                            iYr0, &
                            IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS, &
                            IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE, &
                            DT_OUT, &
                            LON, LAT, d, FM, &
                            DZ, AREA )

          CALL CLOSE_FILE()

      ! Opens the output file for writing:
       CALL OPEN_FILE( trim(NETCDF_OUTPUT_FILE_NAME), nf, EXTRA_VARIABLES, 0 )

       CALL WRITE_DATA( im, jm, nsl, nf, 0, f)

       End Subroutine Init_Output_GD
