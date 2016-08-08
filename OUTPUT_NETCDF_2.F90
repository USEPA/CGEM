!******************************************************************************
! PURPOSE: OUTPUT_NETCDF.F90 - Routines for outputting NCOM_GEM data to a
!          NetCDF file.
! NOTES:   Non-ADT module.
! HISTORY: 2010/04/26, Todd Plessel, plessel.todd@epa.gov, Created.
!******************************************************************************

MODULE OUTPUT_NETCDF

!  USE netcdf
  USE NETCDF_UTILITIES ! For CHKERR, DEFDIM, DEFVI1, CONVERT_LONGITUDES, etc.
  USE DATE_TIME ! For TOTAL_SECONDS

  USE Model_dim, ONLY: nospA,nospZ
  USE INPUT_VARS
  USE CGEM_vars
  USE LIGHT_VARS
  USE Which_Flux
  USE TEMP_VARS

  IMPLICIT NONE

  ! Private

  INTEGER TIME_VAR ! NetCDF ID for time array variable.
  INTEGER FILE_ID ! NetCDF ID for file.
  INTEGER FILE_FIRST_TIMESTEP ! 0-based model timestep number of current file.
  INTEGER SECONDS_PER_TIMESTEP ! Output timestep size in seconds.
  INTEGER(8) SECONDS0 ! Seconds since Model_dim::iYr0.


PUBLIC CREATE_FILE, OPEN_FILE, &
       WRITE_DATA, CLOSE_FILE, FLUSH_FILE, &
       WRITE_EXTRA_DATA

PRIVATE
CONTAINS

  ! Public

  ! Commands:

  ! CREATE_FILE: Create output NetCDF file with given header data.
  ! In a concurrent program, only one process should call this routine
  ! then call CLOSE_FILE then
  ! worker processes should call OPEN_FILE, WRITE_DATA, CLOSE_FILE.
  !
  SUBROUTINE CREATE_FILE( NAME, IM, JM, NSL, NSTEP, VARIABLES,    &
                          EXTRA_VARIABLES, IYR0, &
                          IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS, &
                          IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE, &
                          DT_OUT, RLON, RLAT, H, FM, DZ)
    USE Test_Mod
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN):: NAME
    INTEGER,INTENT(IN):: IM, JM, NSL
    INTEGER,INTENT(IN):: NSTEP
    INTEGER,INTENT(IN):: VARIABLES, EXTRA_VARIABLES
    INTEGER,INTENT(IN):: IYR0 ! Reference year (before start of model run).
    INTEGER,INTENT(IN):: IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS ! Run start.
    INTEGER,INTENT(IN):: IYRE, IMONE, IDAYE, IHRE, IMINE, ISECE ! Run end.
    INTEGER,INTENT(IN):: DT_OUT ! Model timestep size in seconds.
    REAL,DIMENSION(IM):: RLON
    REAL,DIMENSION(JM):: RLAT
    REAL,DIMENSION(IM, JM, NSL):: H,DZ
    INTEGER,DIMENSION(IM, JM),INTENT(IN):: FM
   ! External NetCDF routines:
    INTEGER NF_CREATE, NF_ENDDEF, NF_PUT_VAR_INT, NF_PUT_VAR_REAL, NF_SYNC
    EXTERNAL NF_CREATE, NF_ENDDEF, NF_PUT_VAR_INT, NF_PUT_VAR_REAL, NF_SYNC
   ! Locals:
    REAL,DIMENSION(IM, JM, NSL):: TEMP_3D_H ! VLA
    INTEGER,DIMENSION(IM, JM, NSL):: TEMP_3D_FM ! VLA
    INTEGER IM_DIM, JM_DIM, NSL_DIM, NSTEPP1_DIM
    INTEGER RLON_VAR, RLAT_VAR, H_VAR, FM_VAR
    INTEGER DZ_VAR
    INTEGER K, i
    INTEGER ERR, VARIABLE, DIM_IDS( 4 )
    REAL,DIMENSION(IM):: RLON_COPY
    CHARACTER(LEN=40):: TIME_UNITS
    CHARACTER(LEN=14):: var
    FILE_ID = -1

    ! Create/overwrite NetCDF output file:
    ERR = NF_CREATE( trim(NAME), 770, FILE_ID) 
    CALL CHKERR( ERR, 'create NetCDF output file ' // NAME )
    ! Create dimensions:
    CALL DEFDIM( FILE_ID, IM_DIM, 'longitude', IM )
    CALL DEFDIM( FILE_ID, JM_DIM, 'latitude', JM )
    CALL DEFDIM( FILE_ID, NSL_DIM, 'k', NSL )
!!  CALL DEFDIM( FILE_ID, NSTEPP1_DIM, 'time', NSTEP )
    CALL DEFDIM( FILE_ID, NSTEPP1_DIM, 'time', 0 ) ! 0 Means UNLIMITED size.
    ! Write global scalar attributes:

    CALL DEFTAT( FILE_ID, 'Run_Identifier', TRIM(CODE_ID) )
    CALL DEFIAT( FILE_ID, 'nstep', NSTEP )
    CALL DEFIAT( FILE_ID, 'iYr0', IYR0 )
    CALL DEFIAT( FILE_ID, 'iYrS', IYRS )
    CALL DEFIAT( FILE_ID, 'iMonS', IMONS )
    CALL DEFIAT( FILE_ID, 'iDayS', IDAYS )
    CALL DEFIAT( FILE_ID, 'iHrS', IHRS )
    CALL DEFIAT( FILE_ID, 'iMinS', IMINS )
    CALL DEFIAT( FILE_ID, 'iSecS', ISECS )
    CALL DEFIAT( FILE_ID, 'iYrE', IYRE )
    CALL DEFIAT( FILE_ID, 'iMonE', IMONE )
    CALL DEFIAT( FILE_ID, 'iDayE', IDAYE )
    CALL DEFIAT( FILE_ID, 'iHrE', IHRE )
    CALL DEFIAT( FILE_ID, 'iMinE', IMINE )
    CALL DEFIAT( FILE_ID, 'iSecE', ISECE )
    CALL DEFIAT( FILE_ID, 'dT_out', DT_OUT )
    CALL DEFTAT( FILE_ID, 'description1', &
      'Output from NCOM_GEM ' // &
      '(US Naval Coastal Ocean Model Gulf Ecology Model)' )
    CALL DEFTAT( FILE_ID, 'description2', &
      'Dimension longitude: Number of cells in the longitudinal direction.' )
    CALL DEFTAT( FILE_ID, 'description3', &
      'Dimension latitude: Number of cells in the latitudinal direction.' )
    CALL DEFTAT( FILE_ID, 'description4', &
      'Dimension time: Number of timesteps. ' )
    CALL DEFTAT( FILE_ID, 'description5', &
      'Attribute nstep: Number of model output timesteps excluding initial time.' )
    CALL DEFTAT( FILE_ID, 'description6', &
      'Attribute iYr0: Reference year (YYYY) all timestamps are relative to.' )
    CALL DEFTAT( FILE_ID, 'description7', &
      'Attribute iYrS: GMT year (YYYY) of start of simulation.' )
    CALL DEFTAT( FILE_ID, 'description8', &
      'Attribute iMonS: GMT month (1-12) of start of simulation.' )
    CALL DEFTAT( FILE_ID, 'description9', &
      'Attribute iDayS: GMT day of month (1-31) of start of simulation.' )
    CALL DEFTAT( FILE_ID, 'description10', &
      'Attribute iHrS: GMT hour (0-23) of start of simulation.' )
    CALL DEFTAT( FILE_ID, 'description11', &
      'Attribute iMinS: GMT minite (0-59) of start of simulation.' )
    CALL DEFTAT( FILE_ID, 'description12', &
      'Attribute iSecS: GMT second (0-59) of start of simulation.' )
    CALL DEFTAT( FILE_ID, 'description13', &
      'Attribute iYrE: GMT year (YYYY) of end of simulation.' )
    CALL DEFTAT( FILE_ID, 'description14', &
      'Attribute iMonE: GMT month (1-12) of end of simulation.' )
    CALL DEFTAT( FILE_ID, 'description15', &
      'Attribute iDayE: GMT day of month (1-31) of end of simulation.' )
    CALL DEFTAT( FILE_ID, 'description16', &
      'Attribute iHrE: GMT hour (0-23) of end of simulation.' )
    CALL DEFTAT( FILE_ID, 'description17', &
      'Attribute iMinE: GMT minite (0-59) of end of simulation.' )
    CALL DEFTAT( FILE_ID, 'description18', &
      'Attribute iSecE: GMT second (0-59) of end of simulation.' )
    CALL DEFTAT( FILE_ID, 'description19', &
      'Attribute dT_out: Number of seconds per output timestep.' )

    ! List Calibration Parameters
    CALL DEFTAT( FILE_ID, 'Calibration0', 'Calibration Parameters used for this run.')
!Switches in GEM
    CALL DEFTAT( FILE_ID, 'Calibration1', 'Switches in GEM.')
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iO2surf)', Which_fluxes(iO2surf) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iDICsurf)', Which_fluxes(iDICsurf) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iSOC)', Which_fluxes(iSOC) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iMPB)', Which_fluxes(iMPB) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iNutEx)', Which_fluxes(iNutEx) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iCMAQ)', Which_fluxes(iCMAQ) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iInRemin)', Which_fluxes(iInRemin) )
    CALL DEFIAT( FILE_ID, 'Which_fluxes(iSDM)', Which_fluxes(iSDM) )
    CALL DEFIAT( FILE_ID, 'Which_temperature', Which_temperature )
    CALL DEFIAT( FILE_ID, 'Which_uptake', Which_uptake  )
    CALL DEFIAT( FILE_ID, 'Which_quota', Which_quota  )
    CALL DEFIAT( FILE_ID, 'Calibration', Calibration  )
    CALL DEFIAT( FILE_ID, 'Out_1D', Out_1D  )
    CALL DEFIAT( FILE_ID, 'Which_irradiance', Which_irradiance  )
    CALL DEFIAT( FILE_ID, 'Which_chlaC', Which_chlaC  )
    CALL DEFIAT( FILE_ID, 'Which_photosynthesis', Which_photosynthesis  )
    CALL DEFIAT( FILE_ID, 'Which_growth', Which_growth  )
    CALL DEFIAT( FILE_ID, 'SolarRad', SolarRad  )
    CALL DEFIAT( FILE_ID, 'InitializeHow', InitializeHow  )
!Optics
    CALL DEFTAT( FILE_ID, 'Calibration2', 'Optics in GEM.')
    CALL DEFRAT( FILE_ID, 'Kw', Kw  )
    CALL DEFRAT( FILE_ID, 'Kcdom', Kcdom  )
    CALL DEFRAT( FILE_ID, 'Kspm', Kspm  )
    CALL DEFRAT( FILE_ID, 'Kchla', Kchla  )
    CALL DEFRAT( FILE_ID, 'astar490', astar490  )
    CALL DEFRAT( FILE_ID, 'aw490', aw490  )
    CALL DEFRAT( FILE_ID, 'astarOMA', astarOMA  )
    CALL DEFRAT( FILE_ID, 'astarOMZ', astarOMZ  )
    CALL DEFRAT( FILE_ID, 'astarOMR', astarOMR  )
    CALL DEFRAT( FILE_ID, 'astarOMBC', astarOMBC  )
    CALL DEFRAT( FILE_ID, 'sink CDOM', ws(iCDOM)  )
!Temperature
    CALL DEFTAT( FILE_ID, 'Calibration3', 'Temperature in GEM.')
    CALL DEFRATX( FILE_ID, 'Tref', Tref, nospA+nospZ )
    CALL DEFRATX( FILE_ID, 'KTg1', KTg1, nospA+nospZ )
    CALL DEFRATX( FILE_ID, 'KTg2', KTg2, nospA+nospZ )
    CALL DEFRATX( FILE_ID, 'Ea_R', Ea_R, nospA+nospZ )
!Phytoplankton
    CALL DEFTAT( FILE_ID, 'Calibration4', 'Phytoplankton in GEM.')
    do i=1,nospZ
       write(var,'(A12,i2)') 'ediblevector',i
       CALL DEFRATX( FILE_ID, var, ediblevector(i,:), nospA )
    enddo
    CALL DEFRATX( FILE_ID, 'umax', umax, nospA )
    CALL DEFRATX( FILE_ID, 'alpha', alpha, nospA )
    CALL DEFRATX( FILE_ID, 'beta', beta, nospA )
    CALL DEFRATX( FILE_ID, 'respg', respg, nospA )
    CALL DEFRATX( FILE_ID, 'respb', respb, nospA )
    CALL DEFRATX( FILE_ID, 'QminN', QminN, nospA  )
    CALL DEFRATX( FILE_ID, 'QminP', QminP, nospA )
    CALL DEFRATX( FILE_ID, 'QmaxN', QmaxN, nospA )
    CALL DEFRATX( FILE_ID, 'QmaxP', QmaxP, nospA  )
    CALL DEFRATX( FILE_ID, 'Kn', Kn, nospA  )
    CALL DEFRATX( FILE_ID, 'Kp', Kp, nospA  )
    CALL DEFRATX( FILE_ID, 'Ksi', Ksi, nospA  )
    CALL DEFRATX( FILE_ID, 'KQn', KQn, nospA )
    CALL DEFRATX( FILE_ID, 'KQp', KQp, nospA  )
    CALL DEFRATX( FILE_ID, 'nfQs', nfQs, nospA )
    CALL DEFRATX( FILE_ID, 'vmaxN', vmaxN, nospA  )
    CALL DEFRATX( FILE_ID, 'vmaxP', vmaxP, nospA  )
    CALL DEFRATX( FILE_ID, 'vmaxSi', vmaxSi, nospA  )
    CALL DEFRATX( FILE_ID, 'aN', aN, nospA )
    CALL DEFRATX( FILE_ID, 'volcell', volcell, nospA  )
    CALL DEFRATX( FILE_ID, 'Qc', Qc, nospA  )
    CALL DEFRATX( FILE_ID, 'Athresh', Athresh, nospA )
    CALL DEFRATX( FILE_ID, 'sink A', ws(iA(1):iA(nospA)), nospA  )
    CALL DEFRATX( FILE_ID, 'mA', mA, nospA )

!Zooplankton
    CALL DEFTAT( FILE_ID, 'Calibration5', 'Zooplankton in GEM.')
    CALL DEFRATX( FILE_ID, 'Zeffic', Zeffic, nospZ )
    CALL DEFRATX( FILE_ID, 'Zslop', Zslop, nospZ )
    CALL DEFRATX( FILE_ID, 'Zvolcell', Zvolcell, nospZ )
    CALL DEFRATX( FILE_ID, 'ZQc', ZQc, nospZ )
    CALL DEFRATX( FILE_ID, 'ZQn', ZQn, nospZ )
    CALL DEFRATX( FILE_ID, 'ZQp', ZQp, nospZ )
    CALL DEFRATX( FILE_ID, 'ZKa', ZKa, nospZ )
    CALL DEFRATX( FILE_ID, 'Zrespg', Zrespg, nospZ )
    CALL DEFRATX( FILE_ID, 'Zrespb', Zrespb, nospZ )
    CALL DEFRATX( FILE_ID, 'Zumax', Zumax, nospZ )
    CALL DEFRATX( FILE_ID, 'Zm', Zm, nospZ )

!Organic Matter
    CALL DEFTAT( FILE_ID, 'Calibration6', 'Optics in GEM.')
    CALL DEFRAT( FILE_ID, 'KG1', KG1  )
    CALL DEFRAT( FILE_ID, 'KG2', KG2 )
    CALL DEFRAT( FILE_ID, 'KG1_R', KG1_R  )
    CALL DEFRAT( FILE_ID, 'KG2_R', KG2_R  )
    CALL DEFRAT( FILE_ID, 'KG1_BC', KG1_BC  )
    CALL DEFRAT( FILE_ID, 'KG2_BC', KG2_BC  )
    CALL DEFRAT( FILE_ID, 'KNH4', KNH4 )
    CALL DEFRAT( FILE_ID, 'nitmax', nitmax )
    CALL DEFRAT( FILE_ID, 'KO2', KO2 )
    CALL DEFRAT( FILE_ID, 'KstarO2', KstarO2  )
    CALL DEFRAT( FILE_ID, 'KNO3', KNO3  )
    CALL DEFRAT( FILE_ID, 'pCO2', pCO2  )
    CALL DEFRAT( FILE_ID, 'stoich_x1R', stoich_x1R  )
    CALL DEFRAT( FILE_ID, 'stoich_y1R', stoich_y1R  )
    CALL DEFRAT( FILE_ID, 'stoich_x2R', stoich_x2R  )
    CALL DEFRAT( FILE_ID, 'stoich_y2R', stoich_y2R )
    CALL DEFRAT( FILE_ID, 'stoich_x1BC', stoich_x1BC  )
    CALL DEFRAT( FILE_ID, 'stoich_y1BC', stoich_y1BC  )
    CALL DEFRAT( FILE_ID, 'stoich_x2BC', stoich_x2BC  )
    CALL DEFRAT( FILE_ID, 'stoich_y2BC', stoich_y2BC )
    CALL DEFRAT( FILE_ID, 'sink OM1_A', ws(iOM1_A) )
    CALL DEFRAT( FILE_ID, 'sink OM2_A', ws(iOM2_A)  )
    CALL DEFRAT( FILE_ID, 'sink OM1_Z', ws(iOM1_Z)  )
    CALL DEFRAT( FILE_ID, 'sink OM2_Z', ws(iOM2_Z)  )
    CALL DEFRAT( FILE_ID, 'sink OM1_R', ws(iOM1_R) )
    CALL DEFRAT( FILE_ID, 'sink OM2_R', ws(iOM2_R) )
    CALL DEFRAT( FILE_ID, 'sink OM1_BC', ws(iOM1_BC) )
    CALL DEFRAT( FILE_ID, 'sink OM2_BC', ws(iOM2_BC) )
    CALL DEFRAT( FILE_ID, 'KGcdom', KGcdom  )
    CALL DEFRAT( FILE_ID, 'CF_SPM',CF_SPM )
!River Loads
    CALL DEFTAT( FILE_ID, 'Calibration7', 'River Loads in GEM.')
    CALL DEFRAT( FILE_ID, 'rcNO3', rcNO3 )
    CALL DEFRAT( FILE_ID, 'rcNH4', rcNH4 )
    CALL DEFRAT( FILE_ID, 'rcPO4', rcPO4 )
    CALL DEFRAT( FILE_ID, 'rcSi',  rcSi  )
!Other Including Boundary Conditions
    CALL DEFTAT( FILE_ID, 'Calibration8', 'Other Including Boundary Conditions in GEM.')
    CALL DEFIAT( FILE_ID, 'Which_VMix', Which_VMix  )
    CALL DEFRAT( FILE_ID, 'KH_coeff', KH_coeff )
    CALL DEFIAT( FILE_ID, 'Which_Outer_BC', Which_Outer_BC  )
    CALL DEFRAT( FILE_ID, 'm_OM_init', m_OM_init )
    CALL DEFRAT( FILE_ID, 'm_OM_BC', m_OM_BC )
    CALL DEFRAT( FILE_ID, 'm_OM_sh', m_OM_sh )
    CALL DEFRAT( FILE_ID, 'Stoich_x1A_init', Stoich_x1A_init )
    CALL DEFRAT( FILE_ID, 'Stoich_y1A_init', Stoich_y1A_init )
    CALL DEFRAT( FILE_ID, 'Stoich_x2A_init', Stoich_x2A_init )
    CALL DEFRAT( FILE_ID, 'Stoich_y2A_init', Stoich_y2A_init )
    CALL DEFRAT( FILE_ID, 'Stoich_x1Z_init', Stoich_x1Z_init  )
    CALL DEFRAT( FILE_ID, 'Stoich_y1Z_init', Stoich_y1Z_init  )
    CALL DEFRAT( FILE_ID, 'Stoich_x2Z_init', Stoich_x2Z_init  )
    CALL DEFRAT( FILE_ID, 'Stoich_y2Z_init', Stoich_y2Z_init  )
    CALL DEFRAT( FILE_ID, 'KG_bot', KG_bot  )

    ! Define non-time-varying array variables:

    CALL DEFVR1( FILE_ID, IM_DIM, RLON_VAR, 'longitude', &
                 'Cell center longitude [-180, 180].', 'deg' )
    CALL DEFVR1( FILE_ID, JM_DIM, RLAT_VAR, 'latitude', &
                 'Cell center latitude [-90, 90].', 'deg' )
    CALL DEFVR3( FILE_ID, IM_DIM, JM_DIM, NSL_DIM, H_VAR, 'h', &
                 'Cell bottom depth.', 'm' )
    CALL DEFVI3( FILE_ID, IM_DIM, JM_DIM, NSL_DIM, FM_VAR, 'fm', &
                 'Mask: 0 = land, 1 = water.', 'none' )
    CALL DEFVR3( FILE_ID, IM_DIM, JM_DIM, NSL_DIM, DZ_VAR, 'dz', &
                 'Thickness of cell.', 'none' )
    ! Define time array variable as each output data's seconds since IYR0:

    WRITE ( TIME_UNITS, '(A,I4.4,A)' ) &
            'Seconds since ', iYr0, '-01-01 00:00:00Z'

    ! Note: this is defined as an 8-byte real in the file
    ! because NetCDF does not support 8-byte integers.
    ! Its value is converted to/from interger during reads/writes.

    CALL DEFVD1( FILE_ID, NSTEPP1_DIM, TIME_VAR, 'time', &
                 TRIM( TIME_UNITS ), TRIM( TIME_UNITS ) )

    ! Define time-varying array variables:

    DIM_IDS( 1 ) = IM_DIM
    DIM_IDS( 2 ) = JM_DIM
    DIM_IDS( 3 ) = NSL_DIM
    DIM_IDS( 4 ) = NSTEPP1_DIM

    DO VARIABLE = 1, VARIABLES
      IF ( WRITE_VARIABLE( VARIABLE ) ) THEN
        CALL DEFVR4( FILE_ID, DIM_IDS, F_VAR( VARIABLE ), &
                     TRIM( VARIABLE_NAMES( VARIABLE ) ), &
                     TRIM( VARIABLE_DESCRIPTIONS( VARIABLE ) ), &
                     TRIM( VARIABLE_UNITS( VARIABLE ) ) )
      END IF
    END DO

    ! Define extra time-varying array variables:

    DIM_IDS( 1 ) = IM_DIM
    DIM_IDS( 2 ) = JM_DIM
    DIM_IDS( 3 ) = NSL_DIM
    DIM_IDS( 4 ) = NSTEPP1_DIM

    DO VARIABLE = 1, EXTRA_VARIABLES

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        CALL DEFVR4( FILE_ID, DIM_IDS, EXTRA_VAR( VARIABLE ), &
                   TRIM( EXTRA_VARIABLE_NAMES( VARIABLE ) ), &
                   TRIM( EXTRA_VARIABLE_DESCRIPTIONS( VARIABLE ) ), &
                   TRIM( EXTRA_VARIABLE_UNITS( VARIABLE ) ) )
      END IF
    END DO

    ! End of NetCDF header:

    ERR = NF_ENDDEF( FILE_ID )
    CALL CHKERR( ERR, 'create NetCDF output header' )

    ! Write non-time-varying array variables:

    RLON_COPY = RLON

    CALL CONVERT_LONGITUDES( IM, RLON_COPY )

    ERR = NF_PUT_VAR_REAL( FILE_ID, RLON_VAR, RLON_COPY )
    CALL CHKERR( ERR, 'write output variable rlon' )

    ERR = NF_PUT_VAR_REAL( FILE_ID, RLAT_VAR, RLAT )
    CALL CHKERR( ERR, 'write output variable rlat' )

    ! Write h in 3D

    TEMP_3D_H = H
    ERR = NF_PUT_VAR_REAL( FILE_ID, H_VAR, TEMP_3D_H )
    CALL CHKERR( ERR, 'write output variable h' )


    ! Write fm in 3D

    DO K = 1, NSL
      TEMP_3D_FM(:,:,K) = FM
    END DO
    ERR = NF_PUT_VAR_INT( FILE_ID, FM_VAR, TEMP_3D_FM )
    CALL CHKERR( ERR, 'write output variable fm' )

    ERR = NF_PUT_VAR_REAL( FILE_ID, DZ_VAR, DZ )
    CALL CHKERR( ERR, 'write output variable dz' )



    CALL FLUSH_FILE()


    RETURN
  END SUBROUTINE CREATE_FILE



  ! OPEN_FILE: Open existing output NetCDF file for shared writing.
  ! In a concurrent program, each worker process should call
  ! OPEN_FILE, WRITE_DATA, CLOSE_FILE.
  ! FIRST_TIMESTEP is 0-based timestep number to begin writing.
  !
  SUBROUTINE OPEN_FILE( NAME, VARIABLES, EXTRA_VARIABLES, FIRST_TIMESTEP )
    USE Test_Mod
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN):: NAME
    INTEGER,INTENT(IN):: FIRST_TIMESTEP
    INTEGER,INTENT(IN):: VARIABLES, EXTRA_VARIABLES
    ! External NetCDF routines:
    INTEGER NF__OPEN, NF_INQ_VARID, NF_GET_ATT_INT, NF_GET_ATT_REAL
    EXTERNAL NF__OPEN, NF_INQ_VARID, NF_GET_ATT_INT, NF_GET_ATT_REAL
    ! Locals:
    INTEGER ERR, VARIABLE
    INTEGER DT_OUT
    INTEGER IYR0 ! Reference year (before start of model run).
    INTEGER IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS ! Run start.
    INTEGER(8) SECONDS_FROM_YEAR0
    INTEGER BUFFER_SIZE

    ! Open existing shared 64-bit NetCDF output file for writing:
!!!!ERR = NF_OPEN( NAME, 2565, FILE_ID )
    BUFFER_SIZE = 256 * 1024
    ERR = NF__OPEN( NAME, 775, BUFFER_SIZE, FILE_ID ) ! 1+4+256+2+512 64-bit
    CALL CHKERR( ERR, 'open existing shared writable output file ' // NAME )

    ! Get time variable id:

    ERR = NF_INQ_VARID( FILE_ID, 'time', TIME_VAR )
    CALL CHKERR( ERR, 'inquire NetCDF variable ID ' )

    ! Read time attributes to compute SECONDS0:

    DT_OUT = READIAT( FILE_ID, 'dT_out' )
    IYR0   = READIAT( FILE_ID, 'iYr0' )
    IYRS   = READIAT( FILE_ID, 'iYrS' )
    IMONS  = READIAT( FILE_ID, 'iMonS' )
    IDAYS  = READIAT( FILE_ID, 'iDayS' )
    IHRS   = READIAT( FILE_ID, 'iHrS' )
    IMINS  = READIAT( FILE_ID, 'iMinS' )
    ISECS  = READIAT( FILE_ID, 'iSecS' )
    SECONDS_FROM_YEAR0 = &
      TOTAL_SECONDS( IYR0, IYRS, IMONS, IDAYS, IHRS, IMINS, ISECS )

    FILE_FIRST_TIMESTEP = FIRST_TIMESTEP ! 0-based model timestep of this file.
    SECONDS_PER_TIMESTEP = DT_OUT
    SECONDS0 = SECONDS_PER_TIMESTEP
    SECONDS0 = SECONDS0 * FILE_FIRST_TIMESTEP
    SECONDS0 = SECONDS0 + SECONDS_FROM_YEAR0

    ! Get time-varying array variable ids:

    DO VARIABLE = 1, VARIABLES

      IF ( WRITE_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_INQ_VARID( FILE_ID, TRIM(VARIABLE_NAMES( VARIABLE )), &
                               F_VAR( VARIABLE ) )
        CALL CHKERR( ERR, 'inquire NetCDF variable ID ' )
      END IF
    END DO

    ! Get time-varying extra array variable ids:

    DO VARIABLE = 1, EXTRA_VARIABLES

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_INQ_VARID( FILE_ID, TRIM(EXTRA_VARIABLE_NAMES( VARIABLE )), &
                               EXTRA_VAR( VARIABLE ) )
        CALL CHKERR( ERR, 'inquire NetCDF variable ID ' )
      END IF
    END DO

    RETURN
  END SUBROUTINE OPEN_FILE



  ! CLOSE_FILE: Close output file.
  !
  SUBROUTINE CLOSE_FILE()
    IMPLICIT NONE
    INTEGER NF_CLOSE 
    EXTERNAL NF_CLOSE
    INTEGER ERR

    ERR = NF_CLOSE( FILE_ID )
    CALL CHKERR( ERR, 'close NetCDF output file ' )
    FILE_ID = -1

    RETURN
  END SUBROUTINE CLOSE_FILE


  ! WRITE_DATA: Write current timestep data to output file.
  ! Called by non-concurrent programs to write all data per timestep.
  !

  SUBROUTINE WRITE_DATA( IM, JM, NSL, VARIABLES, TIMESTEP, F ) 
    USE Test_Mod
    IMPLICIT NONE
    INTEGER,INTENT(IN):: IM, JM, NSL, VARIABLES, TIMESTEP ! Model TIMESTEP is 0-based.
    REAL,DIMENSION(IM, JM, NSL, VARIABLES):: F
    ! External NetCDF routines:
    INTEGER NF_PUT_VARA_REAL, NF_PUT_VARA_DOUBLE, NF_SYNC
    EXTERNAL NF_PUT_VARA_REAL, NF_PUT_VARA_DOUBLE, NF_SYNC
    ! Locals:
    REAL(8):: SECONDS(1) ! TIME_VAR.
    INTEGER ERR, VARIABLE, FILE_TIMESTEP
    INTEGER STARTS(4),COUNTS(4)

    FILE_TIMESTEP = TIMESTEP - FILE_FIRST_TIMESTEP

    ! Write time variable as an 8-byte real since NetCDF lacks 8-byte integer:

    STARTS( 1 ) = FILE_TIMESTEP + 1
    COUNTS( 1 ) = 1
    SECONDS( 1 ) = SECONDS0 + FILE_TIMESTEP * SECONDS_PER_TIMESTEP
    ERR = NF_PUT_VARA_DOUBLE( FILE_ID, TIME_VAR, STARTS, COUNTS, SECONDS) 
    CALL CHKERR( ERR, 'write output variable time' )


    STARTS( 1 ) = 1 
    STARTS( 2 ) = 1
    STARTS( 3 ) = 1 
    STARTS( 4 ) = FILE_TIMESTEP + 1 ! NetCDF follows FORTRAN 1-based convention
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM
    COUNTS( 3 ) = NSL
    COUNTS( 4 ) = 1

    DO VARIABLE = 1, VARIABLES

      IF ( WRITE_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, F_VAR( VARIABLE ), &
                  STARTS, COUNTS, F( 1, 1, 1, VARIABLE ))
        CALL CHKERR( ERR, 'write output variable ' // VARIABLE_NAMES(VARIABLE))
      END IF
    END DO

    ! check status of each nonblocking call

    CALL FLUSH_FILE() ! Flush buffers to disk in case of crash.

    RETURN
  END SUBROUTINE WRITE_DATA



  ! WRITE_EXTRA_DATA: Write current timestep data to output file.
  ! Called by concurrent programs to write all data per timestep
  !
  SUBROUTINE WRITE_EXTRA_DATA( IM, JM, NSL, EXTRA_VARIABLES, SPECIES, &
                               TIMESTEP, IRRADIANCE, IRRADIANCE_FRACTION,    &
                               UN, UP, UE, UA, CHLA_MG_TOT,                  &
                               S_X1A, S_Y1A, S_X2A, S_Y2A,                   &
                               S_X1FP, S_Y1FP, S_X2FP, S_Y2FP, USI, ChlC, pH, R_11 )
    USE Test_Mod
    IMPLICIT NONE
    INTEGER,INTENT(IN):: IM, JM, NSL, EXTRA_VARIABLES, SPECIES, TIMESTEP
    REAL,DIMENSION(IM, JM, NSL):: IRRADIANCE
    REAL,DIMENSION(IM, JM, NSL):: IRRADIANCE_FRACTION
    REAL,DIMENSION(IM, JM, NSL, SPECIES):: UN
    REAL,DIMENSION(IM, JM, NSL, SPECIES):: UP
    REAL,DIMENSION(IM, JM, NSL, SPECIES):: UE
    REAL,DIMENSION(IM, JM, NSL, SPECIES):: UA
    REAL,DIMENSION(IM, JM, NSL, SPECIES):: USI
    REAL,DIMENSION(IM, JM, NSL, SPECIES):: ChlC 
    REAL,DIMENSION(IM, JM, NSL):: CHLA_MG_TOT
    REAL,DIMENSION(IM, JM, NSL):: S_X1A, S_Y1A, S_X2A, S_Y2A
    REAL,DIMENSION(IM, JM, NSL):: S_X1FP, S_Y1FP, S_X2FP, S_Y2FP
    REAL,DIMENSION(IM, JM, NSL):: pH
    REAL,DIMENSION(IM, JM, NSL):: R_11
    ! External NetCDF routines:
    INTEGER NF_PUT_VARA_REAL, NF_SYNC
    EXTERNAL NF_PUT_VARA_REAL, NF_SYNC
    ! Locals:
    INTEGER ERR, FILE_TIMESTEP, VARIABLE, INDEX
    INTEGER STARTS(4),COUNTS(4)

    FILE_TIMESTEP = TIMESTEP - FILE_FIRST_TIMESTEP

    ! Define write subset indices for variables:

    STARTS( 1 ) = 1 
    STARTS( 2 ) = 1 
    STARTS( 3 ) = 1 
    STARTS( 4 ) = FILE_TIMESTEP + 1 ! NetCDF follows FORTRAN 1-based convention.
    COUNTS( 1 ) = IM
    COUNTS( 2 ) = JM
    COUNTS( 3 ) = NSL
    COUNTS( 4 ) = 1

    ! Write each extra variable (if selected to be written):


    VARIABLE = 1 ! IRRADIANCE:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  IRRADIANCE( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = 2 ! IRRADIANCE_FRACTION:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  IRRADIANCE_FRACTION( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    DO INDEX = 1, SPECIES ! UN(:,:,:,SPECIES):
      VARIABLE = VARIABLE + 1

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    UN( 1, 1, 1, INDEX ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF
    END DO

    DO INDEX = 1, SPECIES ! UP(:,:,:,SPECIES):
      VARIABLE = VARIABLE + 1

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    UP( 1, 1, 1, INDEX ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF
    END DO

    DO INDEX = 1, SPECIES ! UE(:,:,:,SPECIES):
      VARIABLE = VARIABLE + 1

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    UE( 1, 1, 1, INDEX ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF
    END DO

    DO INDEX = 1, SPECIES ! UA(:,:,:,SPECIES):
      VARIABLE = VARIABLE + 1

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    UA( 1, 1, 1, INDEX ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF
    END DO

    VARIABLE = VARIABLE + 1 ! CHLA_MG_TOT:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  CHLA_MG_TOT( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_X1A:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_X1A( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_Y1A:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_Y1A( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_X2A:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_X2A( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_Y2A:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_Y2A( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_X1FP:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_X1FP( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_Y1FP:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_Y1FP( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_X2FP:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_X2FP( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! S_Y2FP:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  S_Y2FP( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    DO INDEX = 1, SPECIES ! USI(:,:,:,SPECIES):
      VARIABLE = VARIABLE + 1

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    USI( 1, 1, 1, INDEX ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF
    END DO

    DO INDEX = 1, SPECIES ! ChlC(:,:,:,SPECIES):
      VARIABLE = VARIABLE + 1

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    ChlC( 1, 1, 1, INDEX ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF
    END DO


    VARIABLE = VARIABLE + 1 ! pH:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                   pH( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = VARIABLE + 1 ! R_11:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                   R_11( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF


    CALL FLUSH_FILE() ! Flush buffers to disk in case of crash.

    IF ( VARIABLE .NE. EXTRA_VARIABLES ) THEN
      PRINT *, 'PROBLEM: Mismatched EXTRA_VARIABLES count: ', EXTRA_VARIABLES
      STOP
    END IF

    RETURN
  END SUBROUTINE WRITE_EXTRA_DATA



  ! FLUSH_FILE: Flush buffers to disk in case of crash.
  !
  SUBROUTINE FLUSH_FILE()
    IMPLICIT NONE
    ! External NetCDF routines:
    INTEGER NF_PUT_VARA_REAL, NF_SYNC
    EXTERNAL NF_PUT_VARA_REAL, NF_SYNC
    INTEGER ERR
    ERR = NF_SYNC( FILE_ID )
    CALL CHKERR( ERR, 'flush buffers to disk ' )

    RETURN
  END SUBROUTINE FLUSH_FILE


  ! Private



END MODULE OUTPUT_NETCDF


