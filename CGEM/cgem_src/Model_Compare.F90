!******************************************************************************
! PURPOSE: AVE_NETCDF.F90 - Routines for outputing averaged CGEM data to a
!          NetCDF file.
! NOTES:   Non-ADT module.
! HISTORY: 2010/04/26, Todd Plessel, plessel.todd@epa.gov, Created.
!          2021/12/03, Wilson Melendez, melendez.wilson@epa.gov, Replaced
!                      module 'pnetcdf' with 'xnetcdf', commented out mpif.h, and
!                      replaced 'NFMPI' with 'NCDF' in the names of all 
!                      parallel NetCDF subroutines.
!******************************************************************************

MODULE Model_Compare 
!  USE pnetcdf ! For NFMPI_*
  USE xnetcdf
  USE NETCDF_UTILITIES ! For CHKERR, DEFDIM, DEFVI1, CONVERT_LONGITUDES, etc.
  USE DATE_TIME ! For TOTAL_SECONDS

  IMPLICIT NONE
!  INCLUDE 'mpif.h' ! For MPI_*

  ! Private

  INTEGER,PARAMETER:: VAR_3D = 5
  INTEGER,PARAMETER:: VAR_2D = 8
  INTEGER,PARAMETER:: VARS = VAR_3D + VAR_2D 
  CHARACTER(LEN=*),PARAMETER,DIMENSION(VARS):: VAR_NAMES = (/ &
    'O2                      ', &
    'NO3                     ', &
    'NH4                     ', &
    'PO4                     ', &
    'Total_Phytoplankton     ', &
    'Primary_Production      ', &
    'Water_Column_Respiration', &
    'Air_Sea_O2_Flux         ', &
    'FPOM                    ', &
    'FO2                     ', &
    'FNO3                    ', &
    'FNH4                    ', &
    'FPO4                    '  &
  /)

  CHARACTER(LEN=*),PARAMETER,DIMENSION(VARS):: VAR_DESCRIPTIONS = (/&
    'Molecular oxygen                              ', &
    'Nitrate                                       ', &
    'Ammonium                                      ', &
    'Phosphate                                     ', &
    'Total Phytoplankton                           ', &
    'Photosynthesis- Primary production            ', &
    'Water column respiration                      ', &
    'Air-Sea O2 flux                               ', &
    'Particulate Organic Matter Sediment-water Flux', &
    'O2 Sediment-water Flux                        ', &
    'NO3 Sediment-water Flux                       ', &
    'NH4 Sediment-water Flux                       ', &
    'PO4 Sediment-water Flux                       '  &
  /)

  CHARACTER(LEN=*),PARAMETER,DIMENSION(VARS):: VAR_UNITS = (/ &
    'mmol-O2/m3                     ', &
    'mmol-N/m3                      ', &
    'mmol-N/m3                      ', &
    'mmol-P/m3                      ', &
    'mmol-N/m3                      ', &
    'mmol-O2/m2/d                   ', &
    'mmol-O2/m2/d                   ', &
    'mmol-O2/m2/d                   ', &
    'mmol-N/m2/d                    ', &
    'mmol-O2/m2/d                   ', &
    'mmol-N/m2/d                    ', &
    'mmol-N/m2/d                    ', &
    'mmol-P/m2/d                    '  &
  /)

  INTEGER, DIMENSION(VARS):: VAR ! NetCDF IDs for each variable.
  INTEGER TIME_MC_VAR ! NetCDF ID for time array variable.
  INTEGER FILE_MC_ID ! NetCDF ID for file.

PUBLIC CREATE_FILE_MC, OPEN_FILE_MC, &
       WRITE_GEM_MC, WRITE_FLUX_MC, CLOSE_FILE_MC, FLUSH_FILE_MC

PRIVATE
CONTAINS

  ! Public

  ! Commands:

  ! CREATE_FILE_MC: Create output NetCDF file with given header data.
  ! In a concurrent program, only one process should call this routine
  ! then call CLOSE_FILE_MC then
  ! worker processes should call OPEN_FILE_MC, WRITE_DATA_MC, CLOSE_FILE_MC.
  !
  SUBROUTINE CREATE_FILE_MC( NAME, IM, JM, KM, IYR0,   &
                             RLON, RLAT, FM, CODE_ID )

    IMPLICIT NONE
    include 'netcdf.inc'
    CHARACTER(LEN=*),INTENT(IN):: NAME
    INTEGER,INTENT(IN):: IM, JM, KM
    INTEGER,INTENT(IN):: IYR0 ! Reference year (before start of model run).
    REAL,DIMENSION(IM):: RLON
    REAL,DIMENSION(JM):: RLAT
    REAL, DIMENSION(IM, JM, KM),INTENT(IN):: FM
    ! Locals:
    REAL, DIMENSION(IM, JM, KM):: TEMP_IM_JM ! VLA
    INTEGER IM_DIM, JM_DIM, KM_DIM, NSTEPP_MC_DIM
    INTEGER RLON_VAR, RLAT_VAR, FM_VAR
    INTEGER ERR, VARIABLE, DIM_IDS( 4 ), INFO
    REAL,DIMENSION(IM):: RLON_COPY
    CHARACTER(LEN=40):: TIME_UNITS
    CHARACTER(LEN=150):: CODE_ID 
    FILE_MC_ID = -2

    ! Create/overwrite NetCDF output file:

    CALL MPI_INFO_CREATE( INFO, ERR )
    CALL MPI_INFO_SET( INFO, 'ind_wr_buffer_size', '16777216', ERR )
    ERR = NCDF_CREATE( MPI_COMM_SELF, NAME, &
                        IOR( NF_CLOBBER, NF_64BIT_OFFSET ), INFO, FILE_MC_ID )
    CALL CHKERR( ERR, 'create NetCDF output file ' // NAME )
    CALL MPI_INFO_FREE( INFO, ERR ) 

    ! Create dimensions:

    CALL DEFDIM( FILE_MC_ID, IM_DIM, 'longitude', IM )
    CALL DEFDIM( FILE_MC_ID, JM_DIM, 'latitude', JM )
    CALL DEFDIM( FILE_MC_ID, KM_DIM, 'nz', KM )
!!  CALL DEFDIM( FILE_MC_ID, NSTEPP_MC_DIM, 'time', NSTEP )
    CALL DEFDIM( FILE_MC_ID, NSTEPP_MC_DIM, 'time', 0 ) ! 0 Means UNLIMITED size.

    ! Write global scalar attributes:

    CALL DEFTAT( FILE_MC_ID, 'Run_Identifier', TRIM(CODE_ID) )
    CALL DEFIAT( FILE_MC_ID, 'iYr0', IYR0 )
    CALL DEFTAT( FILE_MC_ID, 'description1', &
      'Output from CGEM ' // &
      '(Coastal General Ecology Model)' )
    CALL DEFTAT( FILE_MC_ID, 'description2', &
      'Dimension longitude: Number of cells in the longitudinal direction.' )
    CALL DEFTAT( FILE_MC_ID, 'description3', &
      'Dimension latitude: Number of cells in the latitudinal direction.' )
    CALL DEFTAT( FILE_MC_ID, 'description4', &
      'Dimension nsl: Number of depth sigma layer interfaces.' )
    CALL DEFTAT( FILE_MC_ID, 'description5', &
      'Dimension time: Number of timesteps' )
    CALL DEFTAT( FILE_MC_ID, 'description6', &
      'Run Identifier: Optional code run name specified in input file.' )
    CALL DEFTAT( FILE_MC_ID, 'description7', &
      'Attribute iYr0: Reference year (YYYY) all timestamps are relative to.' )

    ! Define non-time-varying array variables:

    CALL DEFVR1( FILE_MC_ID, IM_DIM, RLON_VAR, 'longitude', &
                 'Cell center longitude [-180, 180].', 'deg' )
    CALL DEFVR1( FILE_MC_ID, JM_DIM, RLAT_VAR, 'latitude', &
                 'Cell center latitude [-90, 90].', 'deg' )
    CALL DEFVR3( FILE_MC_ID, IM_DIM, JM_DIM, KM_DIM, FM_VAR, 'fm', &
                 'Mask: 0 = land, 1 = water.', 'none' )

    ! Define time array variable as each output data's seconds since IYR0:

    WRITE ( TIME_UNITS, '(A,I4.4,A)' ) &
            'Seconds since ', iYr0, '-01-01 00:00:00Z'

    CALL DEFVD1( FILE_MC_ID, NSTEPP_MC_DIM, TIME_MC_VAR, 'time', &
                 TRIM( TIME_UNITS ), TRIM( TIME_UNITS ) )

    ! Define time-varying array variables:

    DIM_IDS( 1 ) = IM_DIM
    DIM_IDS( 2 ) = JM_DIM
    DIM_IDS( 3 ) = KM_DIM
    DIM_IDS( 4 ) = NSTEPP_MC_DIM

    DO VARIABLE = 1, VAR_3D

        CALL DEFVR4( FILE_MC_ID, DIM_IDS, VAR( VARIABLE ), &
                   TRIM( VAR_NAMES( VARIABLE ) ), &
                   TRIM( VAR_DESCRIPTIONS( VARIABLE ) ), &
                   TRIM( VAR_UNITS( VARIABLE ) ), 1)
    END DO

    DO VARIABLE = VAR_3D+1, VARS

        CALL DEFVR3( FILE_MC_ID, IM_DIM, JM_DIM, NSTEPP_MC_DIM, VAR( VARIABLE ), &
                   TRIM( VAR_NAMES( VARIABLE ) ), &
                   TRIM( VAR_DESCRIPTIONS( VARIABLE ) ), &
                   TRIM( VAR_UNITS( VARIABLE ) ) )
    END DO



    ! End of NetCDF header:

    ERR = NCDF_ENDDEF( FILE_MC_ID )
    CALL CHKERR( ERR, 'create NetCDF output header' )
    ERR = NCDF_BEGIN_INDEP_DATA( FILE_MC_ID )
    CALL CHKERR( ERR, 'begin independent data access mode' )

    ! Write non-time-varying array variables:

    RLON_COPY = RLON
    CALL CONVERT_LONGITUDES( IM, RLON_COPY )
    ERR = NCDF_PUT_VAR_REAL( FILE_MC_ID, RLON_VAR, RLON_COPY )
    CALL CHKERR( ERR, 'write output variable rlon' )
    ERR = NCDF_PUT_VAR_REAL( FILE_MC_ID, RLAT_VAR, RLAT )
    CALL CHKERR( ERR, 'write output variable rlat' )
    TEMP_IM_JM = FM ! Must copy for NetCDF call.
    ERR = NCDF_PUT_VAR_REAL( FILE_MC_ID, FM_VAR, TEMP_IM_JM )
    CALL CHKERR( ERR, 'write output variable fm' )
    ERR = NCDF_SYNC( FILE_MC_ID )

    RETURN
  END SUBROUTINE CREATE_FILE_MC



  ! OPEN_FILE: Open existing output NetCDF file for shared writing.
  ! In a concurrent program, each worker process should call
  ! OPEN_FILE, WRITE_DATA, CLOSE_FILE.
  !
  SUBROUTINE OPEN_FILE_MC( NAME )
    IMPLICIT NONE
    include 'netcdf.inc'
    CHARACTER(LEN=*),INTENT(IN):: NAME
    ! Locals:
    INTEGER ERR, VARIABLE, INFO

    CALL MPI_INFO_CREATE( INFO, ERR )
    CALL MPI_INFO_SET( INFO, 'ind_wr_buffer_size', '16777216', ERR )

    ! Open existing shared 64-bit NetCDF output file for writing:

    ERR = NCDF_OPEN( MPI_COMM_WORLD, NAME, &
                      IOR( NF_WRITE, NF_64BIT_OFFSET ), INFO, FILE_MC_ID )
    CALL CHKERR( ERR, 'open existing shared writable output file ' // NAME )
    CALL MPI_INFO_FREE( INFO, ERR ) 

    ! Get time variable id:

    ERR = NCDF_INQ_VARID( FILE_MC_ID, 'time', TIME_MC_VAR )
    CALL CHKERR( ERR, 'inquire NetCDF variable ID ' )

    ! Get time-varying array variable ids:

    DO VARIABLE = 1, VARS
      ERR = NCDF_INQ_VARID( FILE_MC_ID, VAR_NAMES( VARIABLE ), VAR( VARIABLE ) )
      CALL CHKERR( ERR, 'inquire NetCDF variable ID ' )
    END DO

    RETURN
  END SUBROUTINE OPEN_FILE_MC



  ! CLOSE_FILE: Close output file.
  !
  SUBROUTINE CLOSE_FILE_MC()
    IMPLICIT NONE
    INTEGER ERR

    ERR = NCDF_CLOSE( FILE_MC_ID )
    CALL CHKERR( ERR, 'close NetCDF output file ' )
    FILE_MC_ID = -2

    RETURN
  END SUBROUTINE CLOSE_FILE_MC



  ! WRITE_GEM_MC: Write current timestep data to output file.
  ! Called by concurrent programs to write all data per timestep
  ! though only one process (PROCESS_ID 0) will write the extra data.
  !
  SUBROUTINE WRITE_GEM_MC( ISTART, IM, JSTART, JM, ZSTART, KM, OUTSTEP, ISTEP, &
                  SECONDS_PER_TIMESTEP, O2, NO3, NH4, PO4, A_N, PrimProd, WC_O2, &
                  FPOM, FO2, FNO3, FNH4, FPO4) 
    USE INPUT_VARS , ONLY:START_SECONDS
    IMPLICIT NONE
    INTEGER,INTENT(IN):: ISTART, JSTART, ZSTART
    INTEGER,INTENT(IN):: IM, JM, KM, OUTSTEP, ISTEP
    REAL, INTENT(IN) :: SECONDS_PER_TIMESTEP
    REAL, DIMENSION(IM, JM, KM) :: O2, NO3, NH4, PO4, A_N 
    REAL, DIMENSION(IM, JM) :: PrimProd, WC_O2
    REAL, DIMENSION(IM, JM) :: FPOM, FO2, FNO3, FNH4, FPO4 
    ! Locals:
    REAL(8):: SECONDS(1)
    INTEGER(MPI_OFFSET_KIND):: STARTS( 4 ), COUNTS( 4 )
    INTEGER ERR, FILE_TIMESTEP, MODEL_TIMESTEP
    INTEGER,DIMENSION(VARS+1):: REQUESTS, STATUSES
    INTEGER REQUEST_COUNT, REQUEST

    FILE_TIMESTEP = OUTSTEP
    MODEL_TIMESTEP = ISTEP

    ! Write time variable as an 8-byte real since NetCDF lacks 8-byte integer:

    STARTS( 1 ) = FILE_TIMESTEP + 1
    COUNTS( 1 ) = 1
    SECONDS( 1 ) = START_SECONDS + MODEL_TIMESTEP * SECONDS_PER_TIMESTEP
    ERR = NCDF_PUT_VARA_DOUBLE( FILE_MC_ID, TIME_MC_VAR, STARTS, COUNTS, &
                                  SECONDS, REQUESTS( 1 ) )
    CALL CHKERR( ERR, 'write output variable time' )

    REQUEST_COUNT = 1

    ! Write time-varying data variables:

    STARTS( 1 ) = ISTART
    STARTS( 2 ) = JSTART
    STARTS( 3 ) = ZSTART
    STARTS( 4 ) = FILE_TIMESTEP + 1 ! NetCDF follows FORTRAN 1-based convention.
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM
    COUNTS( 3 ) = KM
    COUNTS( 4 ) = 1

! O2 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 1 ), STARTS, COUNTS, &
                                   O2( 1, 1, 1 ), REQUESTS( REQUEST_COUNT ))
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(1))

! NO3 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 2 ), STARTS, COUNTS, &
                                   NO3( 1, 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(2))

! NH4 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 3 ), STARTS, COUNTS, &
                                   NH4( 1, 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(3))

! PO4 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 4 ), STARTS, COUNTS, &
                                   PO4( 1, 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(4))

! A_N 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 5 ), STARTS, COUNTS, &
                                   A_N( 1, 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(5))

    STARTS( 1 ) = ISTART
    STARTS( 2 ) = JSTART
    STARTS( 3 ) = FILE_TIMESTEP + 1 ! NetCDF follows FORTRAN 1-based convention.
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM
    COUNTS( 3 ) = 1

! PrimProd 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 6 ), STARTS, COUNTS, &
                          PrimProd( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(6))

! WC_O2 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 7 ), STARTS, COUNTS, &
                          WC_O2( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(7))

! FPOM
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 9 ), STARTS, COUNTS, &
                           FPOM( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(9))

! FO2
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 10 ), STARTS, COUNTS, &
                           FO2( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(10))

! FNO3
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 11 ), STARTS, COUNTS, &
                          FNO3( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(11))

! FNH4 
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 12 ), STARTS, COUNTS, &
                          FNH4( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(12))

! FPO4
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 13 ), STARTS, COUNTS, &
                          FPO4( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(13))


      ERR = NCDF_WAIT_ALL( FILE_MC_ID, REQUEST_COUNT, REQUESTS, STATUSES )
          CALL CHKERR( ERR, 'inplement non-blocking interface' )

          ! check status of each nonblocking call

          DO REQUEST = 1, REQUEST_COUNT
            CALL CHKERR (STATUSES( REQUEST ), 'nonblocking call ' )
          END DO

      ERR = NCDF_SYNC( FILE_MC_ID ) ! Flush buffers to disk in case of crash.

    RETURN
  END SUBROUTINE WRITE_GEM_MC

  ! WRITE_GEM_MC: Write current timestep data to output file.
  ! Called by concurrent programs to write all data per timestep
  ! though only one process (PROCESS_ID 0) will write the extra data.
  !
  SUBROUTINE WRITE_FLUX_MC( ISTART, IM, JSTART, JM, OUTSTEP, O2_Flux) 
    IMPLICIT NONE
    INTEGER,INTENT(IN):: ISTART, JSTART
    INTEGER,INTENT(IN):: IM, JM, OUTSTEP
    REAL,DIMENSION(IM, JM):: O2_Flux
    ! Locals:
    INTEGER(MPI_OFFSET_KIND):: STARTS( 4 ), COUNTS( 4 )
    INTEGER ERR, FILE_TIMESTEP
    INTEGER,DIMENSION(VARS+1):: REQUESTS, STATUSES
    INTEGER REQUEST_COUNT, REQUEST

    FILE_TIMESTEP = OUTSTEP

    REQUEST_COUNT = 0

    STARTS( 1 ) = ISTART
    STARTS( 2 ) = JSTART
    STARTS( 3 ) = FILE_TIMESTEP + 1 ! NetCDF follows FORTRAN 1-based convention.
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM
    COUNTS( 3 ) = 1

! O2_Flux
        REQUEST_COUNT = REQUEST_COUNT + 1
        ERR = NCDF_PUT_VARA_REAL( FILE_MC_ID, VAR( 8 ), STARTS, COUNTS, &
                          O2_Flux( 1, 1 ), REQUESTS( REQUEST_COUNT ) )
        CALL CHKERR( ERR, 'write output variable  ' // VAR_NAMES(8))

      ERR = NCDF_WAIT_ALL( FILE_MC_ID, REQUEST_COUNT, REQUESTS, STATUSES )
          CALL CHKERR( ERR, 'inplement non-blocking interface' )

          ! check status of each nonblocking call

          DO REQUEST = 1, REQUEST_COUNT
            CALL CHKERR (STATUSES( REQUEST ), 'nonblocking call ' )
          END DO

      ERR = NCDF_SYNC( FILE_MC_ID ) ! Flush buffers to disk in case of crash.

    RETURN
  END SUBROUTINE WRITE_FLUX_MC




  ! FLUSH_FILE: Flush buffers to disk in case of crash.
  !
  SUBROUTINE FLUSH_FILE_MC()
    IMPLICIT NONE
    INTEGER ERR

    ERR = NCDF_SYNC( FILE_MC_ID )
    CALL CHKERR( ERR, 'flush buffers to disk ' )

    RETURN
  END SUBROUTINE FLUSH_FILE_MC


  ! Private


END MODULE Model_Compare 
