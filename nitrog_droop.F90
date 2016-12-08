SUBROUTINE NITROG_DROOP(f,DTM,TEMP,i,j,k)
!------------------------------------------------------------------------------
!-
!-   Purpose and Methods : Kinetic sources and sinks of nitrogen are
!-                         computed in this subroutine.
!-
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:
!-
!-   Calls:      None
!-   Called by:  EUTRO
!-
!-   Created:    July 28, 1992
!-   Authors:    Carl F. Cerco      : Water quality scheme
!-
!- Revised: September 09, 2016  W. Melendez : Added missing parameters to OMP loop.
!- Revised: August 18, 2016  W. Melendez : Changed QminND and QminNG to QND and
!-                                         QNG in the respiration term.
!- Revised: July 19, 2016    W. Melendez : Changed QND and QNG to QminND and
!-                                         QminNG in the respiration term.
!-                                         Added QmaxND and QmaxNG.
!------------------------------------------------------------------------------
!

USE Model_dim
USE FLAGS, ONLY: DO_DO2
USE STATES
USE EUT
USE MASS_BALANCE_GD


IMPLICIT NONE

REAL, INTENT(IN) :: f(nf)
REAL, INTENT(IN) :: TEMP
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k

       
REAL :: MNDON        ! Dissolved organic N mineralization mass derivative
REAL :: HDRLPON      ! Labile detrital N hydrolysis mass mass derivative
REAL :: HDRRPON      ! Refractory detrital N hydrolysis mass mass derivative
       
REAL :: DON          ! Dissolved organic nitrogen
REAL :: NDIS         ! Dissolved nitrogen

REAL :: LPON         ! Contributions to labile detritus mass derivative
REAL :: RPON         ! Contributions to refractory detritus mass derivative

REAL :: NH4D         ! Diatom contribution to ammonia mass derivative
REAL :: NH4G         ! Green algae contribution to ammonia mass derivative

REAL :: NO3D         ! Diatom contribution to nitrate mass derivative
REAL :: NO3G         ! Green algae contribution to nitrate mass derivative
       
REAL :: NT           ! Nitrification rate
REAL :: FTN          ! Nitrification temperature dependence function

REAL :: KDON         ! Dissolved organic nitrogen  mineralization rate [s^-1]
REAL :: KLPON        ! Labile particulate nitrogen  hydrolysis rate [s^-1]
REAL :: KRPON        ! Refractory particulate nitrogen  hydrolysis rate [s^-1]



REAL :: ZDEATH              ! Zooplankton concentration from mortality
REAL :: NH4ZOO              ! Ammonia released by zooplankton mortality
REAL :: DONZOO              ! Dissolved organic N released by zoo. mortality
REAL :: LPONZOO             ! Labile organic N released by zoo. mortality
REAL :: RPONZOO             ! Refractory organic N released by zoo. mortality


REAL :: ALG                 ! Total phytoplankton concentration (dia+gre)
REAL :: TIN                 ! Total inorganic nitrogen (no3+nh4)

REAL :: FNH4                ! NH4 fraction consumed by production
REAL :: FNO3                ! NO3 fraction consumed by production
REAL :: FDON                ! DON fraction consumed by production


!------------------------------------------------------------------------------
! Dissolved Oxygen limitation variable
!------------------------------------------------------------------------------
REAL :: DO2_LIMITATION 

!------------------------------------------------------------------------------
! Denitrification rate in units of kg/(m^3 * s)
!------------------------------------------------------------------------------
REAL :: DENITRIFICATION_N

!------------------------------------------------------------------------------
! Nitrate and ammonia fluxes across the water-sediment interface.
!------------------------------------------------------------------------------
REAL :: UptakeND, UptakeNG
REAL :: InternalN_dia
REAL :: InternalN_gre
REAL :: LuxuryND
REAL :: LuxuryNG


!
!------------------------------------------------------------------------------
!


!------------------------------------------------------------------------------
!  Nitrification 
!------------------------------------------------------------------------------
  IF (TEMP < TMNT) THEN
      FTN = EXP(-KTNT1 * (TEMP - TMNT)**2)
  ELSE
      FTN = EXP(-KTNT2 * (TEMP - TMNT)**2)
  ENDIF

  IF (DO_DO2) THEN
      DO2_LIMITATION = f(JDO2) / ( KHDONT + f(JDO2) )
  ELSE
      DO2_LIMITATION = 1 
  ENDIF

   IF (KHNNT > 0.0) THEN
       NT = FTN * NTM * f(JNH4) / ( KHNNT + f(JNH4) ) &
          * DO2_LIMITATION 
   ELSEIF (f(JNH4) > 0.0) THEN
       NT = FTN * NTM * DO2_LIMITATION
   ELSE
       NT = 0.0
   ENDIF 

  DTM(JNH4) = DTM(JNH4) - NT 
  DTM(JNO3) = DTM(JNO3) + NT 

                                                                 
!------------------------------------------------------------------------------
! Algae sources/sinks
!------------------------------------------------------------------------------
!  FNO3 --> NO3 fraction
!  FNH4 --> NH4 fraction
!  NH4D --> Ammonia from diatoms
!  NH4G --> Ammonia from greens
!  NO3D --> Nitrate from diatoms
!  NO3G --> Nitrate from greens
!  DON  --> Dissolved organic nitrogen mass rate
!  LPON --> Labile particulate detrital nitrogen mass rate
!  RPON --> Refactory particulate detrital nitrogen mass rate
!  DTM(ISEG,JNH4) --> Ammonia mass derivative
!  DTM(ISEG,JNO3) --> Nitrate mass derivative
!  DTM(ISEG,JDON) --> Dissolved organic N mass derivative
!  DTM(ISEG,JLON) --> Labile detrital N mass derivative
!  DTM(ISEG,JRON)-->  Refractory detrital N mass derivative
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Diatoms  
!------------------------------------------------------------------------------
                        
   NDIS = f(JNH4) + f(JNO3) + AVFRACDON *  f(JDON)   
        
   IF (ABS(NDIS) <= TINY(f(JNH4))) THEN
      FNO3 = 0.0
      FNH4 = 0.0
      FDON = 0.0
   ELSE
      FNO3 = f(JNO3) / NDIS        
      FNH4 = f(JNH4) / NDIS
      FDON = (AVFRACDON *  f(JDON)) / NDIS        
   ENDIF

   UptakeND = 0.0
   IF ((f(JDIA) > 0.0) .AND. (QND(i,j,k) < QmaxND)) UptakeND = UpNMaxD * NDIS / (KhIntND + NDIS) 
   IF (IAVG(i,j,k) <= TINY(IAVG(i,j,k))) UptakeND = UptakeND * ILMUL

   LuxuryND = 0.0
   IF (QND(i,j,k) > QminND) LuxuryND = QND(i,j,k) - QminND

   NH4D = (FIntNID * BMD(i,j,k) * QND(i,j,k) - FNH4 * UptakeND) * f(JDIA) +  &         
           FIntLuxNIP * PRD(i,j,k) * LuxuryND + &
           FIntStrNIP * (1.0 - GREFF) * PRD(i,j,k) * QminND
          
   NO3D = -FNO3 * UptakeND * f(JDIA) 
                               
   DON = (FIntNDD * BMD(i,j,k) * QND(i,j,k) - FDON * UptakeND) * f(JDIA) +  &
          FIntLuxNDP * PRD(i,j,k) * LuxuryND + &
          FIntStrNDP * (1.0 - GREFF) * PRD(i,j,k) * QminND

   LPON = FIntNLD * BMD(i,j,k) * QND(i,j,k) * f(JDIA) + &
          FIntLuxNLP * PRD(i,j,k) * LuxuryND + &
          FIntStrNLP * (1.0 - GREFF) * PRD(i,j,k) * QminND

   RPON = FIntNRD * BMD(i,j,k) * QND(i,j,k) * f(JDIA) + &
          FIntLuxNRP * PRD(i,j,k) * LuxuryND + &
          FIntStrNRP * (1.0 - GREFF) * PRD(i,j,k) * QminND

   InternalN_dia = (UptakeND - BMD(i,j,k) * QND(i,j,k)) * f(JDIA) - &
                   PRD(i,j,k) * QND(i,j,k)

   DTM(JNH4) = DTM(JNH4) + NH4D              
   DTM(JNO3) = DTM(JNO3) + NO3D 
   DTM(JDON) = DTM(JDON) + DON 
   DTM(JLON) = DTM(JLON) + LPON
   DTM(JRON) = DTM(JRON) + RPON
   DTM(JDIAN) = DTM(JDIAN) + InternalN_dia 

!------------------------------------------------------------------------------
!  Greens
!------------------------------------------------------------------------------
   UptakeNG = 0.0
   IF ((f(JGRE) > 0.0) .AND. (QNG(i,j,k) < QmaxNG)) UptakeNG = UpNMaxG * NDIS / (KhIntNG + NDIS)
   IF (IAVG(i,j,k) <= TINY(IAVG(i,j,k))) UptakeNG = UptakeNG * ILMUL

   LuxuryNG = 0.0
   IF (QNG(i,j,k) > QminNG) LuxuryNG = QNG(i,j,k) - QminNG

   NH4G = (FIntNIG * BMG(i,j,k) * QNG(i,j,k) - FNH4 * UptakeNG) * f(JGRE) +  &
           FIntLuxNIP * PRG(i,j,k) * LuxuryNG + &
           FIntStrNIP * (1.0 - GREFF) * PRG(i,j,k) * QminNG

   NO3G = -FNO3 * UptakeNG * f(JGRE)

   DON = (FIntNDG * BMG(i,j,k) * QNG(i,j,k) - FDON * UptakeNG) * f(JGRE) +  &
          FIntLuxNDP * PRG(i,j,k) * LuxuryNG + &
          FIntStrNDP * (1.0 - GREFF) * PRG(i,j,k) * QminNG

   LPON = FIntNLG * BMG(i,j,k) * QNG(i,j,k) * f(JGRE) + &
          FIntLuxNLP * PRG(i,j,k) * LuxuryNG + &
          FIntStrNLP * (1.0 - GREFF) * PRG(i,j,k) * QminNG

   RPON = FIntNRG * BMG(i,j,k) * QNG(i,j,k) * f(JGRE) + &
          FIntLuxNRP * PRG(i,j,k) * LuxuryNG + &
          FIntStrNRP * (1.0 - GREFF) * PRG(i,j,k) * QminNG

   InternalN_gre = (UptakeNG - BMG(i,j,k) * QNG(i,j,k)) * f(JGRE) - &
                   PRG(i,j,k) * QNG(i,j,k)

   DTM(JNH4) = DTM(JNH4) + NH4G 
   DTM(JNO3) = DTM(JNO3) + NO3G
   DTM(JDON) = DTM(JDON) + DON
   DTM(JLON) = DTM(JLON) + LPON 
   DTM(JRON) = DTM(JRON) + RPON
   DTM(JGREN) = DTM(JGREN) + InternalN_gre

!------------------------------------------------------------------------------
!  Zooplankton
!------------------------------------------------------------------------------
! ZDEATH --> Zooplankton mortality rate: it was changed from a linear to a
!            quadratic dependence on the zooplankton concentration.  See
!            zoo.F90 subroutine for more details about this change.
!            [ZDTH] = 1/(kg/m^3 s)
! NH4ZOO --> NH4 component
! DONZOO --> DON component
! LPONZOO --> Labile particulate component
! RPONZOO --> Refractory particulate component
! DTM(ISEG,JNH4) --> NH4 time derivative
! DTM(ISEG,JDON) --> DON time derivative
! DTM(ISEG,JLON) --> Labile part. time derivative
! DTM(ISEG,JRON) --> Refractory part. time derivative
!------------------------------------------------------------------------------
   ZDEATH = ZDTH * f(JZOO) * f(JZOO)                                 
   NH4ZOO  = FNIZ * ANCP * ZDEATH                               
   DONZOO  = FNDZ * ANCP * ZDEATH                               
   LPONZOO = FNLZ * ANCP * ZDEATH                               
   RPONZOO = FNRZ * ANCP * ZDEATH                               
   DTM(JNH4) = DTM(JNH4) + NH4ZOO 
   DTM(JDON) = DTM(JDON) + DONZOO
   DTM(JLON) = DTM(JLON) + LPONZOO 
   DTM(JRON) = DTM(JRON) + RPONZOO 
   

!------------------------------------------------------------------------------
!  Mineralization and hydrolysis
!------------------------------------------------------------------------------
! ALG --> Total phytoplankton concentration (dia+gre)
! TIN --> Total inorganic nitrogen
! KDON --> Maximum DON mineralization rate
! KLPON --> Maximum LON hydrolysis rate
! KRPON --> Maximum RON hydrolysis rate
! MNDON --> N mineralization mass derivative
! HDRLPON --> Labile detrital N hydrolysis mass derivative
! HDRRPON --> Refractory detrital N hydrolysis mass derivative
! DTM(ISEG,JNH4) --> ammonia mass derivative
! DTM(ISEG,JDON) --> dissolved organic N mass derivative
! DTM(ISEG,JLON) --> particulate N mass derivative
! DTM(ISEG,JRON) --> particulate N mass derivative
!------------------------------------------------------------------------------

   ALG = f(JGRE) + f(JDIA)                          
   TIN = f(JNO3) + f(JNH4)                          

   KDON =  KDN + ALG * KDNALG * KHN / (KHN + TIN)                     
   KLPON = KLN + ALG * KLNALG * KHN / (KHN + TIN)                     
   KRPON = KRN + ALG * KRNALG * KHN / (KHN + TIN)                     

   MNDON   =  KDON * FTMNL(i,j,k) * f(JDON)                   
   HDRLPON = KLPON * FTHDR(i,j,k) * f(JLON)                   
   HDRRPON = KRPON * FTHDR(i,j,k) * f(JRON)                   

   DTM(JNH4) = DTM(JNH4) + MNDON 
   DTM(JDON) = DTM(JDON)      &      
                  + (HDRLPON + HDRRPON - MNDON) 
   DTM(JLON) = DTM(JLON) - HDRLPON          
   DTM(JRON) = DTM(JRON) - HDRRPON          


!------------------------------------------------------------------------------
!  Denitrificaiton
!  Denitrification is a microbially mediated process that converts
!  nitrate to molecular nitrogen (N2) under anaerobic conditions using DOC.
!  Assume denitrification happens when D.0. concentration is less than 
!  or equal to 2 mg/L.  A low D.O. concentration is considered anything 
!  less than 2 mg/L.  Denitrification reduces the amount of nitrate present
!  in the system.
!  Unit conversion: 2 mg/L is equal to 2 * 1.0E-03 kg/m^3
!------------------------------------------------------------------------------
IF (DO_DO2) THEN

       IF (f(JDO2) <= (2 * 1.0E-03)) THEN
           DENITRIFICATION_N = f(JNO3) / ( f(JNO3) + KHDENITR ) &
                             * ANCP * KDENITR * f(JDOC)   
       ELSE
           DENITRIFICATION_N = 0
       ENDIF

       SUM_DENITR(i,j,k) = SUM_DENITR(i,j,k) + DENITRIFICATION_N

       DTM(JNO3) = DTM(JNO3) - DENITRIFICATION_N 

ENDIF

!Sediment fluxes in Flux_GoMDOM.F90

!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE NITROG_DROOP
