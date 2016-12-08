!******************************************************************************
! PURPOSE: OUTPUT_NETCDF.F90 - Routines for outputting NCOM_GEM data to a
!          NetCDF file.
! NOTES:   Non-ADT module.
! HISTORY: 2010/04/26, Todd Plessel, plessel.todd@epa.gov, Created.
!******************************************************************************

MODULE OUTPUT_NETCDF_GD

!  USE netcdf
  USE NETCDF_UTILITIES ! For CHKERR, DEFDIM, DEFVI1, CONVERT_LONGITUDES, etc.
  USE DATE_TIME ! For TOTAL_SECONDS

  USE Model_dim, ONLY: nf 
  USE INPUT_VARS_GD
  USE STATES 
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
    USE Test_Mod_GD
    USE eut
    USE flags
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
    INTEGER K
    INTEGER ERR, VARIABLE, DIM_IDS( 4 )
    REAL,DIMENSION(IM):: RLON_COPY
    CHARACTER(LEN=40):: TIME_UNITS
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
    CALL DEFTAT( FILE_ID, 'Calibration1', 'Switches in GoMDOM.')
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iO2surf)', Which_Fluxes(iO2surf) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iDICsurf)', Which_Fluxes(iDICsurf) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iSOC)', Which_Fluxes(iSOC) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iMPB)', Which_Fluxes(iMPB) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iNutEx)', Which_Fluxes(iNutEx) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iCMAQ)', Which_Fluxes(iCMAQ) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iInRemin)', Which_Fluxes(iInRemin) )
    CALL DEFIAT( FILE_ID, 'Which_Fluxes(iSDM)', Which_Fluxes(iSDM) )
    CALL DEFIAT( FILE_ID, 'SolarRadKo', SolarRadKo  )
!River Loads
    CALL DEFTAT( FILE_ID, 'Calibration7', 'River Loads in GoMDOM.')
    CALL DEFRAT( FILE_ID, 'rcNO3', rcNO3 )
    CALL DEFRAT( FILE_ID, 'rcNH4', rcNH4 )
    CALL DEFRAT( FILE_ID, 'rcPO4', rcPO4 )
    CALL DEFRAT( FILE_ID, 'rcSi',  rcSi  )
!Other Including Boundary Conditions
    CALL DEFTAT( FILE_ID, 'Calibration8', 'Other Including Boundary Conditions in GoMDOM.')
    CALL DEFIAT( FILE_ID, 'Which_VMix', Which_VMix  )
    CALL DEFRAT( FILE_ID, 'KH_coeff', KH_coeff )
    CALL DEFIAT( FILE_ID, 'Which_Outer_BC', Which_Outer_BC  )
    CALL DEFRAT( FILE_ID, 'm_OM_init', m_OM_init )
    CALL DEFRAT( FILE_ID, 'm_OM_bc', m_OM_bc )
    CALL DEFRAT( FILE_ID, 'm_OM_sh', m_OM_sh )
!GoMDOM Parameters
    CALL DEFRAT( FILE_ID, 'ALPHA_DIA', ALPHA_DIA  )
    CALL DEFRAT( FILE_ID, 'ALPHA_GRE', ALPHA_GRE  )
    CALL DEFRAT( FILE_ID, 'ANCP',ANCP  )
    CALL DEFRAT( FILE_ID, 'APCP',APCP  )
    CALL DEFRAT( FILE_ID, 'ASCD',ASCD  )
    CALL DEFRAT( FILE_ID, 'AVFRAC',AVFRAC  )
    CALL DEFRAT( FILE_ID, 'AVFRACDON',AVFRACDON  )
    CALL DEFRAT( FILE_ID, 'BMRD',BMRD  )
    CALL DEFRAT( FILE_ID, 'BMRG',BMRG  )
    CALL DEFRAT( FILE_ID, 'CCHLD',CCHLD  )
    CALL DEFRAT( FILE_ID, 'CCHLG',CCHLG  )
    CALL DEFRAT( FILE_ID, 'CGZ',CGZ  )
    CALL DEFRAT( FILE_ID, 'DENIT_CN_RATIO',DENIT_CN_RATIO  )
    CALL DEFRAT( FILE_ID, 'FCDD', FCDD )
    CALL DEFRAT( FILE_ID, 'FCDG', FCDG )
    CALL DEFRAT( FILE_ID, 'FCDP', FCDP  )
    CALL DEFRAT( FILE_ID, 'FCDZ', FCDZ  )
    CALL DEFRAT( FILE_ID, 'FCLD', FCLD  )
    CALL DEFRAT( FILE_ID, 'FCLG', FCLG  )
    CALL DEFRAT( FILE_ID, 'FCLP', FCLP  )
    CALL DEFRAT( FILE_ID, 'FCLZ', FCLZ  )
    CALL DEFRAT( FILE_ID, 'FCRD', FCRD  )
    CALL DEFRAT( FILE_ID, 'FCRG', FCRG )
    CALL DEFRAT( FILE_ID, 'FCRP', FCRP  )
    CALL DEFRAT( FILE_ID, 'FCRZ', FCRZ  )
    CALL DEFRAT( FILE_ID, 'FNDD', FNDD  )
    CALL DEFRAT( FILE_ID, 'FNDG', FNDG  )
    CALL DEFRAT( FILE_ID, 'FNDP', FNDP  )
    CALL DEFRAT( FILE_ID, 'FNDZ', FNDZ  )
    CALL DEFRAT( FILE_ID, 'FNID', FNID  )
    CALL DEFRAT( FILE_ID, 'FNIG', FNIG  )
    CALL DEFRAT( FILE_ID, 'FNIP', FNIP  )
    CALL DEFRAT( FILE_ID, 'FNIZ', FNIZ  )
    CALL DEFRAT( FILE_ID, 'FNLD', FNLD  )
    CALL DEFRAT( FILE_ID, 'FNLG', FNLG  )
    CALL DEFRAT( FILE_ID, 'FNLP', FNLP  )
    CALL DEFRAT( FILE_ID, 'FNLZ', FNLZ  )
    CALL DEFRAT( FILE_ID, 'FNRD', FNRD  )
    CALL DEFRAT( FILE_ID, 'FNRG', FNRG  )
    CALL DEFRAT( FILE_ID, 'FNRP', FNRP  )
    CALL DEFRAT( FILE_ID, 'FNRZ', FNRZ  )
    CALL DEFRAT( FILE_ID, 'FPDD', FPDD  )
    CALL DEFRAT( FILE_ID, 'FPDG', FPDG  )
    CALL DEFRAT( FILE_ID, 'FPDP', FPDP  )
    CALL DEFRAT( FILE_ID, 'FPDZ', FPDZ  )
    CALL DEFRAT( FILE_ID, 'FPID', FPID  )
    CALL DEFRAT( FILE_ID, 'FPIG', FPIG  )
    CALL DEFRAT( FILE_ID, 'FPIP', FPIP  )
    CALL DEFRAT( FILE_ID, 'FPIZ', FPIZ  )
    CALL DEFRAT( FILE_ID, 'FPLD', FPLD  )
    CALL DEFRAT( FILE_ID, 'FPLG', FPLG  )
    CALL DEFRAT( FILE_ID, 'FPLP', FPLP  )
    CALL DEFRAT( FILE_ID, 'FPLZ', FPLZ  )
    CALL DEFRAT( FILE_ID, 'FPRD', FPRD  )
    CALL DEFRAT( FILE_ID, 'FPRG', FPRG  )
    CALL DEFRAT( FILE_ID, 'FPRP', FPRP  )
    CALL DEFRAT( FILE_ID, 'FPRZ', FPRZ  )
    CALL DEFRAT( FILE_ID, 'FSAP', FSAP  )
    CALL DEFRAT( FILE_ID, 'GREFF', GREFF  )
    CALL DEFRAT( FILE_ID, 'ILMUL', ILMUL  )
    CALL DEFRAT( FILE_ID, 'KDC', KDC  )
    CALL DEFRAT( FILE_ID, 'KDCALG', KDCALG  )
    CALL DEFRAT( FILE_ID, 'KDN', KDN  )
    CALL DEFRAT( FILE_ID, 'KDNALG', KDNALG  )
    CALL DEFRAT( FILE_ID, 'KDP', KDP  )
    CALL DEFRAT( FILE_ID, 'KDPALG', KDPALG  )
    CALL DEFIAT( FILE_ID, 'KDWD', KDWD  )
    CALL DEFRAT( FILE_ID, 'KHDONT_SED', KHDONT_SED  )
    CALL DEFRAT( FILE_ID, 'KHN', KHN  )
    CALL DEFRAT( FILE_ID, 'KHND', KHND  )
    CALL DEFRAT( FILE_ID, 'KHNG', KHNG  )
    CALL DEFRAT( FILE_ID, 'KHNNT', KHNNT  )
    CALL DEFRAT( FILE_ID, 'KHODOC_SED', KHODOC_SED  )
    CALL DEFRAT( FILE_ID, 'KHP', KHP  )
    CALL DEFRAT( FILE_ID, 'KHPD', KHPD  )
    CALL DEFRAT( FILE_ID, 'KHPG', KHPG )
    CALL DEFRAT( FILE_ID, 'KHSD', KHSD  )
    CALL DEFRAT( FILE_ID, 'KLC', KLC   )
    CALL DEFRAT( FILE_ID, 'KLCALG', KLCALG   )
    CALL DEFRAT( FILE_ID, 'KLN', KLN   )
    CALL DEFRAT( FILE_ID, 'KLNALG', KLNALG   )
    CALL DEFRAT( FILE_ID, 'KLP', KLP   )
    CALL DEFRAT( FILE_ID, 'KLPALG', KLPALG   )
    CALL DEFRAT( FILE_ID, 'KRC', KRC   )
    CALL DEFRAT( FILE_ID, 'KRCALG', KRCALG   )
    CALL DEFRAT( FILE_ID, 'KRN', KRN   )
    CALL DEFRAT( FILE_ID, 'KRNALG', KRNALG   )
    CALL DEFRAT( FILE_ID, 'KRP', KRP   )
    CALL DEFRAT( FILE_ID, 'KRPALG', KRPALG   )
    CALL DEFRAT( FILE_ID, 'KSUA', KSUA   )
    CALL DEFRAT( FILE_ID, 'KSZ', KSZ   )
    CALL DEFRAT( FILE_ID, 'KTBD', KTBD   )
    CALL DEFRAT( FILE_ID, 'KTBG', KTBG   )
    CALL DEFRAT( FILE_ID, 'KTGD1', KTGD1   )
    CALL DEFRAT( FILE_ID, 'KTGD2', KTGD2   )
    CALL DEFRAT( FILE_ID, 'KTGG1', KTGG1   )
    CALL DEFRAT( FILE_ID, 'KTGG2', KTGG2   )
    CALL DEFRAT( FILE_ID, 'KTHDR', KTHDR   )
    CALL DEFRAT( FILE_ID, 'KTMNL', KTMNL   )
    CALL DEFRAT( FILE_ID, 'KTNT1', KTNT1   )
    CALL DEFRAT( FILE_ID, 'KTNT2', KTNT2   )
    CALL DEFRAT( FILE_ID, 'KTSUA', KTSUA   )
    CALL DEFRAT( FILE_ID, 'NTM', NTM   )
    CALL DEFRAT( FILE_ID, 'PBMAX_DIA', PBMAX_DIA   )
    CALL DEFRAT( FILE_ID, 'PBMAX_GRE', PBMAX_GRE   )
    CALL DEFRAT( FILE_ID, 'PMD', PMD   )
    CALL DEFRAT( FILE_ID, 'PMG', PMG   )
    CALL DEFRAT( FILE_ID, 'RSODNTR', RSODNTR   )
    CALL DEFIAT( FILE_ID, 'SILIM', SILIM   )
    CALL DEFRAT( FILE_ID, 'TMD', TMD   )
    CALL DEFRAT( FILE_ID, 'TMG', TMG   )
    CALL DEFRAT( FILE_ID, 'TMNT', TMNT   )
    CALL DEFRAT( FILE_ID, 'TRD', TRD  )
    CALL DEFRAT( FILE_ID, 'TRG', TRG   )
    CALL DEFRAT( FILE_ID, 'TRHDR', TRHDR   )
    CALL DEFRAT( FILE_ID, 'TRMNL', TRMNL   )
    CALL DEFRAT( FILE_ID, 'TRSUA', TRSUA  )
    CALL DEFRAT( FILE_ID, 'TZREF',TZREF   )
    CALL DEFRAT( FILE_ID, 'TREF', TREF   )
    CALL DEFRAT( FILE_ID, 'ZDTH', ZDTH   )
    CALL DEFRAT( FILE_ID, 'ZTHET', ZTHET   )
    CALL DEFRAT( FILE_ID, 'KCOD', KCOD   )
    CALL DEFRAT( FILE_ID, 'KDENITR', KDENITR   )
    CALL DEFRAT( FILE_ID, 'KHDENITR', KHDENITR   )
    CALL DEFRAT( FILE_ID, 'KHDONT', KHDONT   )
    CALL DEFRAT( FILE_ID, 'KHOCOD', KHOCOD   )
    CALL DEFRAT( FILE_ID, 'KHODOC', KHODOC   )
    CALL DEFRAT( FILE_ID, 'FINTNID', FINTNID )
    CALL DEFRAT( FILE_ID, 'FINTNDD', FINTNDD )
    CALL DEFRAT( FILE_ID, 'FINTNLD', FINTNLD )
    CALL DEFRAT( FILE_ID, 'FINTNRD', FINTNRD )
    CALL DEFRAT( FILE_ID, 'FINTNIG', FINTNIG )
    CALL DEFRAT( FILE_ID, 'FINTNDG', FINTNDG )
    CALL DEFRAT( FILE_ID, 'FINTNLG', FINTNLG )
    CALL DEFRAT( FILE_ID, 'FINTNRG', FINTNRG )
    CALL DEFRAT( FILE_ID, 'FINTLUXNIP', FINTLUXNIP )
    CALL DEFRAT( FILE_ID, 'FINTSTRNIP', FINTSTRNIP )
    CALL DEFRAT( FILE_ID, 'FINTLUXNDP', FINTLUXNDP )
    CALL DEFRAT( FILE_ID, 'FINTSTRNDP', FINTSTRNDP )
    CALL DEFRAT( FILE_ID, 'FINTLUXNLP', FINTLUXNLP )
    CALL DEFRAT( FILE_ID, 'FINTSTRNLP', FINTSTRNLP )
    CALL DEFRAT( FILE_ID, 'FINTLUXNRP', FINTLUXNRP )
    CALL DEFRAT( FILE_ID, 'FINTSTRNRP', FINTSTRNRP )
    CALL DEFRAT( FILE_ID, 'FINTPID', FINTPID )
    CALL DEFRAT( FILE_ID, 'FINTPDD', FINTPDD )
    CALL DEFRAT( FILE_ID, 'FINTPLD', FINTPLD )
    CALL DEFRAT( FILE_ID, 'FINTPRD', FINTPRD )
    CALL DEFRAT( FILE_ID, 'FINTPIG', FINTPIG )
    CALL DEFRAT( FILE_ID, 'FINTPDG', FINTPDG )
    CALL DEFRAT( FILE_ID, 'FINTPLG', FINTPLG )
    CALL DEFRAT( FILE_ID, 'FINTPRG', FINTPRG )
    CALL DEFRAT( FILE_ID, 'FINTLUXPIP', FINTLUXPIP )
    CALL DEFRAT( FILE_ID, 'FINTSTRPIP', FINTSTRPIP )
    CALL DEFRAT( FILE_ID, 'FINTLUXPDP', FINTLUXPDP )
    CALL DEFRAT( FILE_ID, 'FINTSTRPDP', FINTSTRPDP )
    CALL DEFRAT( FILE_ID, 'FINTLUXPLP', FINTLUXPLP )
    CALL DEFRAT( FILE_ID, 'FINTSTRPLP', FINTSTRPLP )
    CALL DEFRAT( FILE_ID, 'FINTLUXPRP', FINTLUXPRP )
    CALL DEFRAT( FILE_ID, 'FINTSTRPRP', FINTSTRPRP )
    CALL DEFRAT( FILE_ID, 'KHINTND', KHINTND )
    CALL DEFRAT( FILE_ID, 'KHINTNG', KHINTNG )
    CALL DEFRAT( FILE_ID, 'KHINTPD', KHINTPD )
    CALL DEFRAT( FILE_ID, 'KHINTPG', KHINTPG )
    CALL DEFRAT( FILE_ID, 'QMINND', QMINND )
    CALL DEFRAT( FILE_ID, 'QMINNG', QMINNG )
    CALL DEFRAT( FILE_ID, 'QMINPD', QMINPD )
    CALL DEFRAT( FILE_ID, 'QMINPG', QMINPG )
    CALL DEFRAT( FILE_ID, 'QMAXND', QMAXND )
    CALL DEFRAT( FILE_ID, 'QMAXNG', QMAXNG )
    CALL DEFRAT( FILE_ID, 'QMAXPD', QMAXPD )
    CALL DEFRAT( FILE_ID, 'QMAXPG', QMAXPG )
    CALL DEFRAT( FILE_ID, 'UPNMAXD', UPNMAXD )
    CALL DEFRAT( FILE_ID, 'UPNMAX', UPNMAXG )
    CALL DEFRAT( FILE_ID, 'UPPMAXD', UPPMAXD )
    CALL DEFRAT( FILE_ID, 'UPPMAXG', UPPMAXG )
!    CALL DEFRAT( FILE_ID, 'KRDO: D.O. reaeration coefficient (m/s)',
!    KRDO(1,1,1)   )
    CALL DEFRAT( FILE_ID, 'RCDO', RCDO   )
    CALL DEFRAT( FILE_ID, 'RNTO', RNTO   )
    CALL DEFRAT( FILE_ID, 'VDIA', ws(JDIA)   )
    CALL DEFRAT( FILE_ID, 'VDIAN', ws(JDIAN)   )
    CALL DEFRAT( FILE_ID, 'VDIAP', ws(JDIAP)   )
    CALL DEFRAT( FILE_ID, 'VGRE', ws(JGRE)   )
    CALL DEFRAT( FILE_ID, 'VGREN', ws(JGREN)   )
    CALL DEFRAT( FILE_ID, 'VGREP', ws(JGREP)   )
    CALL DEFRAT( FILE_ID, 'VLOC', ws(JLOC)  )
    CALL DEFRAT( FILE_ID, 'VROC', ws(JROC)   )
    CALL DEFRAT( FILE_ID, 'VLON', ws(JLON)  )
    CALL DEFRAT( FILE_ID, 'VRON', ws(JRON)  )
    CALL DEFRAT( FILE_ID, 'VLOP', ws(JLOP)   )
    CALL DEFRAT( FILE_ID, 'VROP', ws(JROP)  )
    CALL DEFRAT( FILE_ID, 'VSU', ws(JSU)   )



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
    USE Test_Mod_GD
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
    USE Test_Mod_GD
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
  SUBROUTINE WRITE_EXTRA_DATA( IM, JM, NSL, EXTRA_VARIABLES,  &
                               TIMESTEP, SUM_DENITR, SUM_DENITR_C, &
                               SUM_DOCPRD, SUM_DOCMET, SUM_DOCZOO ) 
    USE Test_Mod_GD
    IMPLICIT NONE
    INTEGER,INTENT(IN):: IM, JM, NSL, EXTRA_VARIABLES, TIMESTEP
    REAL,DIMENSION(IM, JM, NSL):: SUM_DENITR 
    REAL,DIMENSION(IM, JM, NSL):: SUM_DENITR_C 
    REAL,DIMENSION(IM, JM, NSL):: SUM_DOCPRD 
    REAL,DIMENSION(IM, JM, NSL):: SUM_DOCMET 
    REAL,DIMENSION(IM, JM, NSL):: SUM_DOCZOO 
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


    VARIABLE = 1 ! SUM_DENITR:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  SUM_DENITR( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

    VARIABLE = 2 ! SUM_DENITR_C:

    IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
      ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                  STARTS, COUNTS, &
                                  SUM_DENITR_C( 1, 1, 1 ))
      CALL CHKERR( ERR, 'write output variable  ' &
                   // EXTRA_VARIABLE_NAMES( VARIABLE ) )
    END IF

      VARIABLE = 3 ! SUM_DOCPRD  

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                 SUM_DOCPRD( 1, 1, 1 ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF

      VARIABLE = 4 !SUM_DOCMET 

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    SUM_DOCMET( 1, 1, 1 ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
      END IF

      VARIABLE = 5 !SUM_DOCZOO

      IF ( WRITE_EXTRA_VARIABLE( VARIABLE ) ) THEN
        ERR = NF_PUT_VARA_REAL( FILE_ID, EXTRA_VAR( VARIABLE ), &
                                    STARTS, COUNTS, &
                                    SUM_DOCZOO( 1, 1, 1 ))
        CALL CHKERR( ERR, 'write output variable  ' &
                     // EXTRA_VARIABLE_NAMES( VARIABLE ) )
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



END MODULE OUTPUT_NETCDF_GD


