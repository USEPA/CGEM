SUBROUTINE NITROG(f,DTM,TEMP,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: nitrog.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
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
!-               Raymond S. Chapman : Numerical solution scheme
!-               Thomas M. Cole     : Computer algorithms & coding
!-               Hydroqual          : Sediment compartment
!-
!-               Water Quality Modeling Group
!-               U.S. Army Corps of Engineers
!-               Waterways Experiment Station
!-               Vicksburg, Mississippi 39180
!-
!-
!- Revised: 09/02/14  W. Melendez   :   Added parallelization code.
!- Revised: 03/14/14  W. Melendez   :   Commented out the capping of the NO3
!-                                      sediment bed flux.  The NO3 capping
!-                                      was moved to the exchange.F90
!-                                      subroutine.
!- Revised: 02/17/14  W. Melendez   :   Capped the NO3 sediment flux by
!-                                      a value determined by the NO3 mass
!-                                      available in the overlying water.
!- Revised: 02/06/14  W. Melendez   :   Renamed some of the variables 
!-                                      related to the nitrate and ammonia
!-                                      sediment bed fluxes. Fixed bug:
!-                                      set ammonia flux to zero when
!-                                      using Murrell and Lehrter's 
!-                                      equations.
!- Revised: 02/03/14  W. Melendez   :   Added NO3SED_LEHRTER flag and a 
!-                                      water-sediment ammonia flux. Added
!-                                      a new water-sediment nitrate flux.
!- Revised: 01/10/14  W. Melendez   :   Zeroed out water-sediment nitrate 
!-                                      flux for bottom cells that do not
!-                                      have a sediment bed interface.
!- Revised: 04/26/13  W. Melendez   :   Added variable that stores the
!-                                      nitrate lost from the system due
!-                                      denitrification.  Added variable
!-                                      that stores the nitrate gained/lost
!-                                      to/from the system due the 
!-                                      water-sediment NO3 flux.
!- Revised: 04/11/13  W. Melendez   :   Bug fix --> changed the minus
!-                                      sign (-) to a plus (+) sign in
!-                                      the equation that has the 
!-                                      sediment-water nitrate flux 
!-                                      contribution to the nitrate mass
!-                                      derivative. Updated the reference
!-                                      of the equation used for the 
!-                                      sediment-water nitrate flux.
!- Revised: 02/28/13  W. Melendez   :   Added term for loss of DON due to
!-                                      consumption by phytoplankton 
!-                                      production --> see diatoms and
!-                                      greens sections below.  Added
!-                                      NDIS and FDON variables.
!- Revised: 08/30/12  W. Melendez   :   Redesigned the nitrification 
!-                                      conditional statement to properly
!-                                      account for zero ammonium (NH4)
!-                                      concentrations. Changed zooplankton
!-                                      higher order predation (mortality) term
!-                                      from a linear to a quadratic dependence
!-                                      on the zooplankton concentration.
!- Revised: 02/08/12  W. Melendez   :   Bug fix: added division by 8.64E+04
!-                                      (number of seconds in a day) to the
!-                                      equation that calculates a nitrate
!-                                      flux between bottom water layer and
!-                                      sediment bed.
!- Revised: 02/03/12  W. Melendez   :   Bug fix: allow calculation of nitrate
!-                                      flux between bottom water layer and
!-                                      sediment bed only when simulating 
!-                                      dissolved oxygen.
!- Revised: 08/23/11  W. Melendez   :   Added calculation for the flux of 
!-                                      nitrate going from the bottom water
!-                                      layer to the sediment bed.
!- Revised: 01/25/10  W. Melendez   :   Added contribution to the nitrification
!-                                      process from dissolved oxygen.
!-                                      Added denitrification process resulting
!-                                      from low D.O. concentrations. 
!-                                      Added DO_DO2 flag.
!- Revised: 06/22/09  W. Melendez   : ----> WQEM. This application is based
!-                                      on LM3-Eutro (LM3 version 3.2.13) and 
!-                                      its purpose is to serve as a dissolved
!-                                      oxygen model for the Gulf of Mexico
!-                                      hypoxia project. NetCDF is the library 
!-                                      used to handle input/output binary
!-                                      files.
!- Revised: 01/08/04  W. Melendez     : Replaced the hardwired value of
!-                                      2.0E-38 by the Fortran 90 intrinsic
!-                                      function named TINY.  Made cosmetic
!-                                      changes.
!- Revised: 09/11/00  W. Melendez     : Set fractions of consumed NH4 and
!-                                      NO3 to zero if the sum of their
!-                                      respective concentrations is zero.
!- Revised: 09/06/00  W. Melendez     : Modify fraction of consumed 
!-                                      NH4 and NO3 from production.
!- Revised: 09/01/00  W. Melendez     : Modify calculation of NT to account
!-                                      for zero values for KHNNT.
!- Revised: 12/16/99  Pauer & Settles : Restore Minod dependence to organic
!-                                      nitrogen decay
!- Revised: 08/25/99  Pauer & Settles : Change to zooplankton kinetics; 
!-                                      remove linear dependence of grazing
!-                                      on total phyto concentration; 
!-                                      dimension of grazing rate CGZ
!- Revised: 08/03/99  M. Settles  :  Add module DMASS
!- Revised: 06/28/99  M. Settles  :  Remove DOEUTRO cpp flag
!- Revised: 04/09/99  M. Settles  :  WQM_COM,WTRANS merger -> WTRBLK
!- Revised: 04/07/99  M. Settles  :  New state variable convention
!- Revised: 03/15/99  M. Settles  :  Remove debug code
!- Revised: 03/15/99  M. Settles  :  Remove unused module references
!- Revised: 01/15/99  M. Settles  :  Use the WTMP module for temperature
!- Revised: 12/15/98  M. Settles  :  Use dM/dt array DTM
!- Revised: 11/28/98  M. Settles  :  -> Fortran 90
!- Revised: 11/28/98  M. Settles  :  Fixed eutro. state variable indices
!- Revised: 03/05/98  M. Settles  :   Remove knetic.inc include file ref
!- Revised: 02/27/98  M. Settles  :  Drop ISE cell index
!- Revised: 02/27/98  M. Settles  :  Change C(),CD() array dimensions
!- Revised: 12/27/97  M. Settles  :  IPX state variables
!-
!------------------------------------------------------------------------------
!

USE Model_dim
USE FLAGS, ONLY: DO_DO2
USE STATES
USE EUT
USE MASS_BALANCE_WQEM

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
       
REAL :: ALGN         ! Algal nitrogen (??)

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
!  ALGN --> Algal nitrogen  
!  FNO3 --> NO3 fraction
!  FNH4 --> NH4 fraction
!  NH4D --> Ammonia from diatoms
!  NH4G --> Ammonia from greens
!  NO3D --> Nitrate from diatoms
!  NO3G --> Nitrate from greens
!  DON  --> Dissolved organic nitrogen mass rate
!  LPON --> Labile particulate detrital nitrogen mass rate
!  RPON --> Refactory particulate detrital nitrogen mass rate
!  DTM(i,j,k,JNH4) --> Ammonia mass derivative
!  DTM(i,j,k,JNO3) --> Nitrate mass derivative
!  DTM(i,j,k,JDON) --> Dissolved organic N mass derivative
!  DTM(i,j,k,JLON) --> Labile detrital N mass derivative
!  DTM(i,j,k,JRON)-->  Refractory detrital N mass derivative
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Diatoms  
!------------------------------------------------------------------------------

   ALGN = ANCP * f(JDIA)                         
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

   NH4D = (FNID * BMD(i,j,k) - FNH4 * PD(i,j,k)) * ALGN  &         
        +  FNIP * (1.0 - GREFF) * PRD(i,j,k) * ANCP          
   NO3D = -FNO3 * PD(i,j,k) * ALGN                                
   DON = (FNDD * BMD(i,j,k) - FDON * PD(i,j,k)) * ALGN +  &
         (FNDP * (1.0 - GREFF) * PRD(i,j,k) * ANCP)
   LPON = (FNLD * BMD(i,j,k) * ALGN) + (FNLP * (1.0 - GREFF) * PRD(i,j,k) * ANCP)
   RPON = (FNRD * BMD(i,j,k) * ALGN) + (FNRP * (1.0 - GREFF) * PRD(i,j,k) * ANCP)
   DTM(JNH4) = DTM(JNH4) + NH4D              
   DTM(JNO3) = DTM(JNO3) + NO3D              
   DTM(JDON) = DTM(JDON) + DON               
   DTM(JLON) = DTM(JLON) + LPON             
   DTM(JRON) = DTM(JRON) + RPON             

!------------------------------------------------------------------------------
!  Greens
!------------------------------------------------------------------------------
   ALGN = ANCP * f(JGRE)                                      
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

   NH4G = (FNIG * BMG(i,j,k) - FNH4 * PG(i,j,k)) * ALGN        &           
        +  FNIP * (1.0 - GREFF) * PRG(i,j,k) * ANCP          
   NO3G = -FNO3 * PG(i,j,k) * ALGN                                     
   DON = (FNDG * BMG(i,j,k) - FDON * PG(i,j,k)) * ALGN + &
         FNDP * (1.0 - GREFF) * PRG(i,j,k) * ANCP   
   LPON = FNLG * BMG(i,j,k) * ALGN + FNLP * (1.0 - GREFF) * PRG(i,j,k) * ANCP  
   RPON = FNRG * BMG(i,j,k) * ALGN + FNRP * (1.0 - GREFF) * PRG(i,j,k) * ANCP  
   DTM(JNH4) = DTM(JNH4) + NH4G              
   DTM(JNO3) = DTM(JNO3) + NO3G             
   DTM(JDON) = DTM(JDON) + DON               
   DTM(JLON) = DTM(JLON) + LPON              
   DTM(JRON) = DTM(JRON) + RPON              

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
! DTM(i,j,k,JNH4) --> NH4 time derivative
! DTM(i,j,k,JDON) --> DON time derivative
! DTM(i,j,k,JLON) --> Labile part. time derivative
! DTM(i,j,k,JRON) --> Refractory part. time derivative
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
! DTM(i,j,k,JNH4) --> ammonia mass derivative
! DTM(i,j,k,JDON) --> dissolved organic N mass derivative
! DTM(i,j,k,JLON) --> particulate N mass derivative
! DTM(i,j,k,JRON) --> particulate N mass derivative
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

   DTM(JDON) = DTM(JDON) + HDRLPON + HDRRPON - MNDON 
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

!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE NITROG
