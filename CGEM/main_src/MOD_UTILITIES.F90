    MODULE MOD_UTILITIES    
!MOD_EPACOM_GEM_UTILITIES.F90

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
    REAL, INTENT(IN)    :: TC    ! TC       - Dissolved Inorganic Carbon (DIC) (mmol m-3) in upper
                                 !            model layer
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
!---------------------------------------------------------------------

      FUNCTION sw_smow( T ) RESULT( RES )

  !------------------------------------------------------------------
  ! Denisty of Standard Mean Ocean Water (Pure Water) using EOS 1980.
  ! Based on MATLAB code and the following references:
  ! SW_SMOW  $Revision: 1.3 $  $Date: 1994/10/10 05:51:46 $
  ! Copyright (C) CSIRO, Phil Morgan 1992.
  !
  ! INPUT:
  !   T = temperature [degree C (IPTS-68)]
  !
  ! OUTPUT:
  !   dens = density  [kg/m^3]
  !
  ! AUTHOR:  Phil Morgan 92-11-05  (morgan@ml.csiro.au)
  !
  ! DISCLAIMER:
  !   This software is provided "as is" without warranty of any kind.
  !   See the file sw_copy.m for conditions of use and licence.
  !
  ! REFERENCES:
  !     Unesco 1983. Algorithms for computation of fundamental properties of
  !     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
  !     UNESCO 1983 p17  Eqn(14)
  !
  !     Millero, F.J & Poisson, A.
  !     INternational one-atmosphere equation of state for seawater.
  !     Deep-Sea Research Vol28A No.6. 1981 625-629.    Eqn (6)
  !-------------------------------------------------------------------------
    IMPLICIT NONE

    REAL,INTENT(IN) :: T    ! Temperature (deg C)
    REAL            :: RES

    ! Locals:
    REAL, PARAMETER :: A0 = 999.842594
    REAL, PARAMETER :: A1 =   6.793952E-2
    REAL, PARAMETER :: A2 =  -9.095290E-3
    REAL, PARAMETER :: A3 =   1.001685E-4
    REAL, PARAMETER :: A4 =  -1.120083E-6
    REAL, PARAMETER :: A5 =   6.536332E-9

    RES = A0 + ( A1 + ( A2 + ( A3 + ( A4 + A5 * T ) * T ) * T ) * T ) * T

    RETURN
  END FUNCTION sw_smow

      FUNCTION sw_dens0( S, T ) RESULT( RES )

  !--------------------------------------------------------------------------
  ! Density of Sea Water at atmospheric pressure using UNESCO 1983 (EOS 1980).
  ! Based on MATLAB code and the following references:
  ! SW_DENS0  $Revision: 1.3 $  $Date: 1994/10/10 04:54:09 $
  !           Copyright (C) CSIRO, Phil Morgan 1992
  !
  ! INPUT:  (all must have same dimensions)
  !   S = salinity    [psu      (PSS-78)]
  !   T = temperature [degree C (IPTS-68)]
  !
  ! OUTPUT:
  !   dens0 = density  [kg/m^3] of salt water with properties S,T,
  !           P=0 (0 db gauge pressure)
  !
  ! AUTHOR:  Phil Morgan 92-11-05  (morgan@ml.csiro.au)
  !
  ! DISCLAIMER:
  !   This software is provided "as is" without warranty of any kind.
  !   See the file sw_copy.m for conditions of use and licence.
  !
  ! REFERENCES:
  !     Unesco 1983. Algorithms for computation of fundamental properties of
  !     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
  !
  !     Millero, F.J. and  Poisson, A.
  !     International one-atmosphere equation of state of seawater.
  !     Deep-Sea Res. 1981. Vol28A(6) pp625-629.
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: S    ! Salinity (psu)
    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL :: RES

    ! Locals from UNESCO 1983 eqn(13) p17.
    REAL, PARAMETER:: B0 =  8.24493E-1
    REAL, PARAMETER:: B1 = -4.0899E-3
    REAL, PARAMETER:: B2 =  7.6438E-5
    REAL, PARAMETER:: B3 = -8.2467E-7
    REAL, PARAMETER:: B4 =  5.3875E-9

    REAL, PARAMETER:: C0 = -5.72466E-3
    REAL, PARAMETER:: C1 = +1.0227E-4
    REAL, PARAMETER:: C2 = -1.6546E-6

    REAL, PARAMETER:: D0 =  4.8314E-4

    RES = sw_smow( T ) +                                                &
    &     ( B0 + ( B1 + ( B2 + ( B3 + B4 * T ) * T ) * T ) * T ) * S +  &
    &     ( C0 + ( C1 + C2 * T ) * T ) * S * SQRT( S ) + D0 * S * S

    RETURN
  END FUNCTION sw_dens0


!------------------------------------------------------------------
  FUNCTION gas_exchange( T, S, TC, H, PH, PCO2 ) RESULT( RES )
!------------------------------------------------------------------
! Gas exchange model from Eldridge and Cifuentes (2000).
! Provides and estimate of the loss or gain of CO2 from the atmosphere
! The atmosphere pCo2 concentration was from Whitfield and Turner (1986)
! and may have changes by now.  D/z is in cm s-1 and depth is in meters.
! Multiplication by 1000 and division by 100 provides units of mmol C m-2
! s-1.  Final units are mmol C m-2 d-1.
!------------------------------------------------------------------
! T        - Temperature (deg C) of upper model layer.
! S        - Salinity (psu) of upper model layer.
! TC       - Dissolved Inorganic Carbon (DIC) (mmol m-3) in upper
!            model layer
! H        -- Thickness of the upper model layer (m.) contacting the
!             atmosphere
! PH       -- pH concentration of seawater.
! PCO2     -- CO2 concentration in atmosphere in ppmv.
!             Table 1 in Huang et al. 2015 for LA shelf, average=380
!------------------------------------------------------------------

    IMPLICIT NONE

    REAL   ,INTENT(IN)  :: T
    REAL   ,INTENT(IN)  :: S
    REAL   ,INTENT(IN)  :: TC
    REAL   ,INTENT(IN)  :: H
    REAL   ,INTENT(IN)  :: PCO2
    REAL   ,INTENT(IN)  :: PH
!--------------------------------------------
    REAL, PARAMETER      :: DZ      = 0.005
                                        ! combines diffusion through
                                        ! water-atm layer and
                                        ! thickness(cm s-1).
    REAL                 :: ATM_CO2
    REAL                 :: H2CO3C
    REAL                 :: PW
    REAL                 :: RES
    REAL                 :: RK0
    REAL   ,DIMENSION(6) :: SPC

    ! Use an updated salclosed function to get the distribution of
    ! carbonate alkalinity species and H+.  Whitman and Turner (1986).
    SPC     = salclosed( S, TC, T, PH )  ! SPC units Concentration mmol/m3

    H2CO3C  = SPC( 1 )   ! Concentration of H2CO3 in units of mmol/m3
    RK0     = SPC( 6 )   ! Henry's constant (mol kg-1 atm-1)
    PW      = sw_dens0( S, T )              ! water density  [kg/m3]

    ATM_CO2  = PCO2 * RK0 * (PW * 1.0E-3)

    RES = -DZ  * ( ATM_CO2 - H2CO3C ) / ( H * 100.0) !mmol C m-2 s-1

    RETURN
  END FUNCTION gas_exchange

!***********************************************************************
      FUNCTION SchmidtNumber( S, T, which ) RESULT( RES )

  !-------------------------------------------------------------------------
  ! INPUT:  (all must have same dimensions)
  !   S = salinity    [psu      (PSS-78)]
  !   T = temperature [degree C (IPTS-68)]
  !
  ! OUTPUT:
  !   Schmidt Number for Oxygen (which = 0)
  !or Schmidt Number for CO2    (which = 1)
  !
  ! REFERENCES:
  !     Unesco 1983. Algorithms for computation of fundamental properties of
  !     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: S    ! Salinity (psu)
    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL :: RES
    REAL :: muSW,KT
    REAL :: D_02, D_C02
    INTEGER, INTENT(IN) :: which !0 for Oxygen
                                 !1 for CO2

    REAL, PARAMETER:: K0 = 1.7910
    REAL, PARAMETER:: K1 = -6.144e-2
    REAL, PARAMETER:: K2 = 1.4510e-3
    REAL, PARAMETER:: K3 = -1.6826e-5
    REAL, PARAMETER:: K4 = -1.5290e-4
    REAL, PARAMETER:: K5 = 8.3885e-8
    REAL, PARAMETER:: K6 = 2.4727e-3
    REAL, PARAMETER:: K7 = 6.0574e-6
    REAL, PARAMETER:: K8 = -2.6760e-9
    REAL, PARAMETER:: K9 = 4.8429e-5
    REAL, PARAMETER:: K10 = -4.7172e-6
    REAL, PARAMETER:: K11 = 7.5986e-8

    REAL, PARAMETER:: D1 = 0.2604e-5
    REAL, PARAMETER:: D2 = 0.006383e-5
    REAL, PARAMETER:: D3 = 0.1954e-5
    REAL, PARAMETER:: D4 = 0.005089e-5

    REAL, PARAMETER:: P = 1.01325

    KT = 273.15 + T

    muSW = K0 + K1*T + K2*T*T + K3*T*T*T + K4*P + K5*P*P + K6*S +  & 
  & T*(K7*P + K8*P*P) + S*(K9*T + K10*T*T + K11*T*T*T)

    if(which.eq.0) then
     D_02 = D1 + D2*KT/muSW
     RES = muSW/(sw_dens0(S,T)*D_02)
    endif

    if(which.eq.1) then
     D_C02 = D3 + D4*KT/muSW
     RES = muSW/(sw_dens0(S,T)*D_C02)
    endif


    RETURN
  END FUNCTION SchmidtNumber

!---------------------------------------------------------------------
   FUNCTION   o2sat(S, T)               RESULT(OSAT)

!---------------------------------------------------------------------
! Oxygen concentration at saturation at one atmosphere (umol/kg).
! Source: "The Solubility Of Nitrogen, Oxygen And Argon In Water And
!         Seawater" - Weiss (1970) _Deep Sea Research_ V17(4): 721-735.
! Based on MATLAB code by Edward T Peltzer, MBARI, revised: 1998-01-2
!---------------------------------------------------------------------

    IMPLICIT NONE

    REAL        , INTENT(IN) :: S  ! Salinity
    REAL        , INTENT(IN) :: T  ! Temperature (degrees C)

    ! Declare Locals:
!----------------------------------------------------------------------
    REAL, PARAMETER:: CONSTANT1  =  273.15
    REAL, PARAMETER:: CONSTANT2  =    0.01
    REAL, PARAMETER:: CONSTANT3  = -177.7888
    REAL, PARAMETER:: CONSTANT4  =  255.5907
    REAL, PARAMETER:: CONSTANT5  =  146.4813
    REAL, PARAMETER:: CONSTANT6  =   22.204
    REAL, PARAMETER:: CONSTANT7  =   -0.037362
    REAL, PARAMETER:: CONSTANT8  =    0.016504
    REAL, PARAMETER:: CONSTANT9  =    0.0020564
    REAL, PARAMETER:: ML_PER_UM  =   44.658
    REAL          :: T1
    REAL          :: OSAT ! Oxygen (umol/kg) --function RESULT
!----------------------------------------------------------------------

    T1 = ( T + CONSTANT1 ) * CONSTANT2

    OSAT = CONSTANT3 + CONSTANT4 /                                     &
    &      T1 + CONSTANT5 * LOG( T1 ) - CONSTANT6 * T1

    OSAT = OSAT + S * (CONSTANT7 + T1 * (CONSTANT8 - CONSTANT9 * T1))
    OSAT = EXP( OSAT )
    OSAT = OSAT * ML_PER_UM ! Oxygen (umol/kg)

    RETURN
  END FUNCTION o2sat

  
  END MODULE MOD_UTILITIES
      
!****************************************************************      
