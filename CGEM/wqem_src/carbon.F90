SUBROUTINE CARBON(f,DTM,TEMP,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: carbon.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
!-
!-   Purpose and Methods : Kinetic sources and sinks of carbon are
!-                         computed in this subroutine.
!-
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:   None
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
!- Revised: 09/02/14  W. Melendez :  Added parallelization code.
!- Revised: 12/14/13  W. Melendez :  Bug fix: added DO_PHYTO_PROCESSES
!-                                   flag to plankton/D.O. related
!-                                   processes that get written to output.
!- Revised: 07/08/13  W. Melendez :  Turned DOCMINRL into a positive
!-                                   number.
!- Revised: 06/26/13  W. Melendez :  Added arrays that store the DOC
!-                                   generated at D.O. concentrations
!-                                   that are <= 2mg/L. 
!- Revised: 06/25/13  W. Melendez :  Changed "1" to "1.0".
!- Revised: 06/24/13  W. Melendez :  Changed the conditional statement
!-                                   C(ISEG,JDO2) < (2.0 * 1.0E-03)) to
!-                                   C(ISEG,JDO2) <= (2.0 * 1.0E-03))
!-                                   for the zooplankton mortality 
!-                                   process.
!- Revised: 05/06/13  W. Melendez :  Added term to DOC mineralization 
!-                                   that contains a dissolved oxygen  
!-                                   dependence that was previously 
!-                                   missing.
!- Revised: 04/30/13  W. Melendez :  Added DOCMINRL array to store
!-                                   DOC mineralization mass balance 
!-                                   information.
!- Revised: 04/11/13  W. Melendez :  Changed the conditional statement
!-                                   C(ISEG,JDO2) < (2.0 * 1.0E-03)) to
!-                                   C(ISEG,JDO2) <= (2.0 * 1.0E-03)). 
!- Revised: 12/19/12  W. Melendez :  Added algal predation and zooplankton
!-                                   mortality contributions to DOC
!-                                   production for DO concentrations of
!-                                   less than 2 mg/L.
!- Revised: 09/06/12  W. Melendez :  Changed zooplankton higher order 
!-                                   predation (mortality) term from a linear
!-                                   to a quadratic dependence on the
!-                                   zooplankton concentration.
!- Revised: 01/25/10  W. Melendez :  Added effect of low D.O. concentration to
!-                                   DOC concentration resulting from diatoms' 
!-                                   and greens' metabolism.  Added  
!-                                   de-nitrification process resulting from
!-                                   low D.O. concentrations.
!-                                   Added DO_DO2 flag.
!- Revised: 06/22/09  W. Melendez : ----> WQEM. This application is based
!-                                   on LM3-Eutro (LM3 version 3.2.13) and 
!-                                   its purpose is to serve as a dissolved
!-                                   oxygen model for the Gulf of Mexico
!-                                   hypoxia project. NetCDF is the library 
!-                                   used to handle input/output binary
!-                                   files.
!- Revised: 03/13/01  W. Melendez :  Added LOC and ROC production from 
!-                                   diatoms and greens metabolism and/or
!-                                   mortality rate.  
!- Revised: 12/16/99  Pauer & Settles : Restore Minod dependence to carbon
!-                                      mineralization and hydrolysis
!- Revised: 08/25/99  Pauer & Settles : Change to zooplankton kinetics; 
!-                                      remove linear dependence of grazing
!-                                      on total phyto concentration; 
!-                                      dimension of grazing rate CGZ
!- Revised: 08/03/99  M. Settles  :  Add module DMASS
!- Revised: 06/28/99  M. Settles  :  Remove DOEUTRO cpp test
!- Revised: 04/09/99  M. Settles  :  WQM_COM,WTRANS merger -> WTRBLK
!- Revised: 04/07/99  M. Settles  :  New state variable index convention
!- Revised: 03/31/99  M. Settles  :  *BUG FIX* to POC from zoo. mortality
!- Revised: 03/15/99  M. Settles  :  Remove debug code
!- Revised: 03/15/99  M. Settles  :  Remove unused module references
!- Revised: 03/01/99  M. Settles  :  Misc. bug fixes
!- Revised: 01/15/99  M. Settles  :  Temperature is WTMP module
!- Revised: 12/15/98  M. Settles  :  Array dM/dt is DTM
!- Revised: 11/23/98  M. Settles  :  -> Fortran 90
!- Revised: 03/05/98  M. Settles  :  Remove knetic.inc include file ref
!- Revised: 02/27/98  M. Settles  :  New C(),CD() dimensions
!- Revised: 01/05/98  M. Settles  :  IPX state variables
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
REAL, INTENT(INOUT) :: DTM(nf)
REAL, INTENT(IN) :: TEMP
INTEGER, INTENT(IN) :: i,j,k

REAL :: DOCMET            ! DOC production by algal metabolism
REAL :: LPOCMET           ! LPOC production by algal metabolism
REAL :: RPOCMET           ! RPOC production by algal metabolism
REAL :: DOCPRD            ! DOC production by algal predation
REAL :: RPDCPRD           ! Refractory detritus production by algal predation
REAL :: LPDCPRD           ! Labile detritus production by algal predation


REAL :: KDOC              ! Maximum mineralization rate of DOC [sec^-1]
REAL :: KRPOC             ! Maximum hydrolysis rate of refractory POC [sec^-1]
REAL :: KLPOC             ! Maximum hydrolysis rate of labile POC [sec^-1]

REAL :: MNLDOC            ! DOC mineralization mass derivative
REAL :: HDRRPOC           ! Refractory PDC hydrolysis mass derivative
REAL :: HDRLPOC           ! Labile PDC hydrolysis mass derivative

REAL :: DOCZOO            ! DOC produced by zooplankton mortality
REAL :: LPOCZOO           ! Labile detritus produced by zooplankton mortality
REAL :: RPOCZOO           ! Refractory detritus produced by zooplankton 
                          ! mortality

REAL :: ZDEATH            ! Zooplankton mortality concentration

REAL :: DIAT              ! Diatom carbon concentration
REAL :: GRE               ! Green algae carbon concentration
REAL :: ALG               ! Phytoplankton concentration (dia+gre)

!------------------------------------------------------------------------------
! Denitrification rate in units of kg/(m^3 * s)
!------------------------------------------------------------------------------
REAL :: DENITRIFICATION_C   

!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
!  Diatoms
!------------------------------------------------------------------------------
    
!------------------------------------------------------------------------------
! Diatom carbon concentration
!------------------------------------------------------------------------------
   DIAT = f(JDIA)                         

!------------------------------------------------------------------------------
! DOC from diatom metabolism
!------------------------------------------------------------------------------
   DOCMET  = FCDD * BMD(i,j,k) * DIAT           

!------------------------------------------------------------------------------
! LPOC from diatom metabolism
!------------------------------------------------------------------------------
   LPOCMET = FCLD * BMD(i,j,k) * DIAT           

!------------------------------------------------------------------------------
! RPOC from diatom metabolism
!------------------------------------------------------------------------------
   RPOCMET = FCRD * BMD(i,j,k) * DIAT           

!------------------------------------------------------------------------------
! DOC from diatom predation
!------------------------------------------------------------------------------
   DOCPRD  = FCDP * (1.0 - GREFF) * PRD(i,j,k)  

!------------------------------------------------------------------------------
! Refractory particulate from diatom predation
!------------------------------------------------------------------------------
   RPDCPRD = FCRP * (1.0 - GREFF) * PRD(i,j,k)  
                                               
!------------------------------------------------------------------------------
! Labile particulate from diatom predation 
!------------------------------------------------------------------------------
   LPDCPRD = FCLP * (1.0 - GREFF) * PRD(i,j,k)  
                                               
!------------------------------------------------------------------------------
!  A low Dissolved Oxygen (DO) concentration results in no release of CO2 
!  during diatoms metabolism or predation, but higher DOC concentration. A 
!  low DO concentration is considered anything less than 2 mg/L.
!  Unit conversion: 2 mg/L is equal to 2 * 1.0E-03 kg/m^3
!------------------------------------------------------------------------------
   IF (DO_DO2) THEN

       IF (f(JDO2) <= (2.0 * 1.0E-03)) THEN

           DOCMET = DOCMET + (1.0 - (FCDD + FCLD + FCRD)) * BMD(i,j,k) * DIAT

           DOCPRD = DOCPRD + (1.0 - GREFF) * (1.0 - (FCDP + FCLP + FCRP)) * &
                   & PRD(i,j,k)

       ENDIF 

   ENDIF

           SUM_DOCMET(i,j,k) = SUM_DOCMET(i,j,k) + DOCMET
           SUM_DOCPRD(i,j,k) = SUM_DOCPRD(i,j,k) + DOCPRD


!------------------------------------------------------------------------------
!         DOC mass derivative
!------------------------------------------------------------------------------
   DTM(JDOC) = DTM(JDOC) + DOCMET + DOCPRD 

!------------------------------------------------------------------------------
!         Refractory detritus mass derivative
!------------------------------------------------------------------------------
   DTM(JROC) = DTM(JROC) + RPOCMET + RPDCPRD 

!------------------------------------------------------------------------------
!         Labile detritus mass derivative 
!------------------------------------------------------------------------------
   DTM(JLOC) = DTM(JLOC) + LPOCMET + LPDCPRD 

! END DO                                                               


!------------------------------------------------------------------------------
!  Greens
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Green algae carbon concentation
!------------------------------------------------------------------------------
   GRE = f(JGRE)    

!------------------------------------------------------------------------------
!  DOC from green algae metabolism
!------------------------------------------------------------------------------
   DOCMET = FCDG * BMG(i,j,k) * GRE           

!------------------------------------------------------------------------------
! LPOC from green algae metabolism
!------------------------------------------------------------------------------
   LPOCMET = FCLG * BMG(i,j,k) * GRE          

!------------------------------------------------------------------------------
! RPOC from green algae metabolism
!------------------------------------------------------------------------------
   RPOCMET = FCRG * BMG(i,j,k) * GRE          

!------------------------------------------------------------------------------
! DOC from green algae predation
!------------------------------------------------------------------------------
   DOCPRD = FCDP * (1.0 - GREFF) * PRG(i,j,k)   

!------------------------------------------------------------------------------
! Refractory particulate from green algae predation
!------------------------------------------------------------------------------
   RPDCPRD = FCRP * (1.0 - GREFF) * PRG(i,j,k) 

!------------------------------------------------------------------------------
! Labile particulate from green algae predation 
!------------------------------------------------------------------------------
   LPDCPRD = FCLP * (1.0 - GREFF) * PRG(i,j,k)  

!------------------------------------------------------------------------------
!  A low Dissolved Oxygen (DO) concentration results in no release of CO2 
!  during greens metabolism or predation, but higher DOC concentration. A 
!  low DO concentration is considered anything less than 2 mg/L.
!  Unit conversion: 2 mg/L is equal to 2 * 1.0E-03 kg/m^3
!------------------------------------------------------------------------------
   IF (DO_DO2) THEN

       IF (f(JDO2) <= (2.0 * 1.0E-03)) THEN

           DOCMET = DOCMET + (1.0 - (FCDG + FCLG + FCRG)) * BMG(i,j,k) * GRE

           DOCPRD = DOCPRD + (1.0 - GREFF) * (1.0 - (FCDP + FCLP + FCRP)) * &
                  & PRG(i,j,k)

       ENDIF 

   ENDIF 

           SUM_DOCMET(i,j,k) = SUM_DOCMET(i,j,k) + DOCMET
           SUM_DOCPRD(i,j,k) = SUM_DOCPRD(i,j,k) + DOCPRD


!------------------------------------------------------------------------------
!         DOC mass derivative
!------------------------------------------------------------------------------
   DTM(JDOC) = DTM(JDOC) + DOCMET + DOCPRD      

!------------------------------------------------------------------------------
!         Refractory detritus mass derivative
!------------------------------------------------------------------------------
   DTM(JROC) = DTM(JROC) + RPOCMET + RPDCPRD 

!------------------------------------------------------------------------------
!         Labile detritus mass derivative 
!------------------------------------------------------------------------------
   DTM(JLOC) = DTM(JLOC) + LPOCMET + LPDCPRD  
         

!------------------------------------------------------------------------------
! Zooplankton mortality
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! ZDEATH --> Zooplankton mortality rate; it was changed from a linear to a
!            quadratic dependence on the zooplankton concentration.  See
!            zoo.F90 subroutine for more details about this change.
!            [ZDTH] = 1/(kg/m^3 s)
!------------------------------------------------------------------------------
   ZDEATH = ZDTH * f(JZOO) * f(JZOO) 

!------------------------------------------------------------------------------
!  DOC component
!------------------------------------------------------------------------------
   DOCZOO  = FCDZ * ZDEATH               

   IF (DO_DO2) THEN

       IF (f(JDO2) <= (2.0 * 1.0E-03)) THEN

           DOCZOO = DOCZOO + (1.0 - (FCDZ + FCLZ + FCRZ)) * ZDEATH

       ENDIF 

   ENDIF

           SUM_DOCZOO(i,j,k) = SUM_DOCZOO(i,j,k) + DOCZOO

!------------------------------------------------------------------------------
!  Labile particulate component
!------------------------------------------------------------------------------
   LPOCZOO = FCLZ * ZDEATH               

!------------------------------------------------------------------------------
!  Refractory particulate component
!------------------------------------------------------------------------------
   RPOCZOO = FCRZ * ZDEATH               

!------------------------------------------------------------------------------
! DOC time derivative
!------------------------------------------------------------------------------
   DTM(JDOC) = DTM(JDOC) + DOCZOO   

!------------------------------------------------------------------------------
! Labile particulate time derivative
!------------------------------------------------------------------------------
   DTM(JLOC) = DTM(JLOC) + LPOCZOO

!------------------------------------------------------------------------------
! Refractory particulate time derivative
!------------------------------------------------------------------------------
   DTM(JROC) = DTM(JROC) + RPOCZOO  
 

!------------------------------------------------------------------------------
!  Mineralization
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Total phytoplankton concentration
!------------------------------------------------------------------------------
   ALG = f(JGRE) + f(JDIA)

!------------------------------------------------------------------------------
!  Mineralization temperature dependence
!------------------------------------------------------------------------------
   FTMNL(i,j,k) = EXP( KTMNL * (TEMP - TRMNL) )   

!------------------------------------------------------------------------------
!  Hydrolysis temperature dependence
!------------------------------------------------------------------------------
   FTHDR(i,j,k) = EXP( KTHDR * (TEMP - TRHDR) )   

!------------------------------------------------------------------------------
!  Maximum mineralization rate
!------------------------------------------------------------------------------
   KDOC = KDC + KDCALG * ALG                  

!------------------------------------------------------------------------------
!  Maximum refractory hydrolysis rate
!------------------------------------------------------------------------------
   KRPOC = KRC + KRCALG * ALG           

!------------------------------------------------------------------------------
!  Maximum labile hydrolysis rate 
!------------------------------------------------------------------------------
   KLPOC = KLC + KLCALG * ALG           

!------------------------------------------------------------------------------
!  Mineralization time derivative
!------------------------------------------------------------------------------
   IF (DO_DO2) THEN 
       MNLDOC = KDOC * FTMNL(i,j,k) * f(JDOC) *     &
              & ( f(JDO2) / (KHODOC + f(JDO2)) )
   ELSE
       MNLDOC = KDOC * FTMNL(i,j,k) * f(JDOC)   
   ENDIF

!------------------------------------------------------------------------------
!  Refractory hydrolysis time derivative
!------------------------------------------------------------------------------
   HDRRPOC = KRPOC * FTHDR(i,j,k) * f(JROC)  

!------------------------------------------------------------------------------
!  Labile hydrolysis time derivative 
!------------------------------------------------------------------------------
   HDRLPOC = KLPOC * FTHDR(i,j,k) * f(JLOC)  
                      
!------------------------------------------------------------------------------
!  DOC time derivative 
!------------------------------------------------------------------------------
   DTM(JDOC)  = DTM(JDOC) + HDRRPOC + HDRLPOC - MNLDOC

!------------------------------------------------------------------------------
!  Refractory particulate mass derivative
!------------------------------------------------------------------------------
   DTM(JROC) = DTM(JROC) - HDRRPOC   

!------------------------------------------------------------------------------
!  Labile particulate mass derivative
!------------------------------------------------------------------------------
   DTM(JLOC) = DTM(JLOC) - HDRLPOC 

! END DO


!------------------------------------------------------------------------------
!  Denitrification is a microbially mediated process that converts
!  nitrate to molecular nitrogen (N2) under anaerobic conditions using DOC.
!  Assume denitrification happens when D.0. concentration is less than 
!  or equal to 2 mg/L.  A low D.O. concentration is considered anything 
!  less than 2 mg/L.  Denitrification reduces the amount of DOC present in 
!  the system.
!  Unit conversion: 2 mg/L is equal to 2 * 1.0E-03 kg/m^3
!------------------------------------------------------------------------------
 IF (DO_DO2) THEN

       IF (f(JDO2) <= (2.0 * 1.0E-03)) THEN
           DENITRIFICATION_C = ( f(JNO3) / (f(JNO3) + KHDENITR) )   &
                             * KDENITR * f(JDOC)   
       ELSE
           DENITRIFICATION_C = 0.0
       ENDIF

       SUM_DENITR_C(i,j,k) = SUM_DENITR_C(i,j,k) + DENITRIFICATION_C

       DTM(JDOC) =  DTM(JDOC) - DENITRIFICATION_C 

 ENDIF



!
!------------------------------------------------------------------------------
!


RETURN

END SUBROUTINE CARBON

