    MODULE MOD_EPACOM_GEM_UTILITIES    

    IMPLICIT NONE
  
    CONTAINS

!**************************************************************
  FUNCTION thermocons( S, T)     RESULT(RK)

  !------------------------------------------------------------
  ! Compute all the equilibrium constants (RK) for CO2 system.
  ! These are temperature and salinity dependent.
  ! from Whitfield and Turner, The Carbon Dioxide System
  ! in Estuarties--an Inorganic Perspective, The Science of the
  ! total Environment, 49(1986) 235-255, Elsevier.
  !------------------------------------------------------------
    IMPLICIT NONE

    REAL,  INTENT(IN) :: S  ! Salinity
    REAL,  INTENT(IN) :: T  ! Water Temperature(deg C)

!-----------------------------------------------------------

    REAL, DIMENSION(8), PARAMETER :: A0 = (/&
    & 290.9097, 207.6548, 148.0248, 0.0221, &
    & 0.5709  , 0.9805  , 1.4853  , 0.5998 /)

    REAL, DIMENSION(8), PARAMETER :: A1 = (/&
    & 14554.21, 11843.79, 8966.9 , 34.02,   &
    & -84.25  , -92.65  , -192.69, -75.25  /)

    REAL, DIMENSION(8), PARAMETER :: A2 =   &
    &  (/ 45.0575, 33.6485, 24.4344, 0.0,   &
    &     0.0    , 0.0    , 0.0    , 0.0   /)

    REAL,DIMENSION(8),PARAMETER ::  B0 =    &
    & (/ 0.0   , 0.0   , 0.0   , 0.0   ,    &
    &   -1.632 , -3.294, -5.058, -1.767    /)

    REAL, DIMENSION(9) :: RK

    REAL    :: SP
    REAL    :: TP
    REAL    :: TP_OVER_100
    REAL    :: RK1S
    INTEGER :: I
    INTEGER :: J
!-----------------------------------------------------------
    SP          = MAX( S, 0.0 )
    TP          = T + 273.15     ! Absolute temperature (deg K).
    TP_OVER_100 = TP * 0.01

    ! Temperature dependence of the thermodynamic stability:
    DO I = 1, 3
      RK(I) = EXP( A0(I) - A1(I) / TP - A2(I) * LOG(TP) ) ! %Ko(i)
    END DO

    ! Sal. and temp. dependence of the stability constant K1:
    I = 1
    DO J = 4, 5
      RK( J ) = EXP( LOG( RK( I ) ) + (A0( J ) + A1( J ) / TP +        &
      &         A2( J ) * LOG( TP ) ) * SQRT( SP )                     &
      &       + B0( J ) * SP / 100.0 )
    END DO

    I = 2
    DO J = 6, 7
      RK( J ) = EXP( LOG( RK( I ) ) + ( A0( J ) + A1( J ) / TP +       &
      &         A2( J ) * LOG( TP ) ) * SQRT( SP )                     &
      &       + B0( J ) * SP / 100.0 )
    END DO

    ! Salinity and temperature dependence of K0:
    RK( 8 ) = EXP( -58.0931 + 90.5069 * ( 100.0 / TP ) +               &
    &           22.2940 * LOG( TP_OVER_100 ) +                         &
    &          (0.027766 - 0.025888 * TP_OVER_100 +                    &
    &           0.0050578 * TP_OVER_100 * TP_OVER_100 ) * SP)

    ! Addition of RK(9) from Sediment Diagenesis Model:
      RK1S = 2.527+1359.96/TP -0.206*SP**(1.0/3.0)
      RK1S = 10.0**(-RK1S)
      RK(9)= RK1S

    RETURN
  END FUNCTION thermocons


!----------------------------------------------------------------------------
  FUNCTION salclosed ( S, TC, T, PH )       RESULT(SPC)
!--------------------------------------------------------------------
! Closed CO2 System Calculations. SPC in units concentration mmol/m3.
!--------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN)    :: S     ! Salinity 
    REAL, INTENT(IN)    :: TC
    REAL, INTENT(IN)    :: T     ! Temperature (Celsius)
    REAL, INTENT(IN)    :: PH    ! dimensionless
!-----------------------------------------------
    REAL                :: RK0        = 0.0
    REAL                :: RK1        = 0.0
    REAL                :: RK2        = 0.0
    REAL                :: RK1RK2     = 0.0
    REAL                :: H1         = 0.0
    REAL                :: H1_SQUARED = 0.0
    REAL                :: ALPHE0     = 0.0
    REAL                :: ALPHE1     = 0.0
    REAL                :: ALPHE2     = 0.0
    REAL                :: HCO3C      = 0.0
    REAL                :: CO3C       = 0.0
    REAL                :: H2CO3C     = 0.0

    REAL,DIMENSION(9) :: RK           = 0.0
    REAL,DIMENSION(6) :: SPC
!-----------------------------------------------

  !-----------------------------------------------------------
  ! Compute all the equilibrium constants (RK) for CO2 system.
  ! These are temperature and salinity dependent.
  ! from Whitfield and Turner (1986).
  !-----------------------------------------------------------
    RK     = thermocons( S, T)
    RK1    = RK( 4 )
    RK2    = RK( 6 )
    RK0    = RK( 8 )
    RK1RK2 = RK1 * RK2

    !-----------------------------------------------------------------
    ! pH  is given! The expressions below are from Cappellen and Wang,
    !               March 1996, American Journal of Science, "Cycling
    !               of iron and manganese in sfc sediments: A Genl.
    !               Theory for the Coupled Transport and Reaction of
    !               CO2, Oxygen, Nitrogen, Sulfur, Iron, and Manganese
    !-----------------------------------------------------------------
    H1         = 10.0 ** ( -PH )
    H1_SQUARED = H1 * H1
    ALPHE0     = 1.0/( 1.0               + RK1/H1 + RK1RK2/H1_SQUARED )
    ALPHE1     = 1.0/( H1/RK1            + 1.0    + RK2/H1 )
    ALPHE2     = 1.0/( H1_SQUARED/RK1RK2 + H1/RK2 + 1.0 )

    HCO3C      = TC * ALPHE1
    CO3C       = TC * ALPHE2
    H2CO3C     = TC * ALPHE0

! Each element of SPC has concentration units of mmol/m3
    SPC( 1 )   = H2CO3C
    SPC( 2 )   = HCO3C
    SPC( 3 )   = CO3C
    SPC( 4 )   = RK1
    SPC( 5 )   = RK2
    SPC( 6 )   = RK0

    RETURN
  END FUNCTION salclosed

  
  END MODULE MOD_EPACOM_GEM_UTILITIES
      
!****************************************************************      
