SUBROUTINE ZOO(f,DTM,TEMP,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: zoo.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
!-
!-   Description :  Perform zooplankton kinetics calculation.  Account
!-                  for zooplankton growth from consumption of phyto-
!-                  plankton, and zooplankton mortality.
!-
!-   Inputs  :  None 
!-   Outputs :  None
!-
!-   Calls:      REPORT
!-   Called by:  EUTRO
!-
!-   Created: 03/05/99  J. Pauer & M. Settles
!-
!- Revised:  09/02/14  W. Melendez :    Added parallelization code.
!- Revised:  07/08/13  W. Melendez :    Turned ZOOMORT into a positive number.
!- Revised:  04/30/13  W. Melendez :    Added ZOOMORT array to store the amount
!-                                      of carbon lost from the zooplankton
!-                                      due to mortality.
!- Revised:  08/30/12  W. Melendez :    Changed the zooplankton mortality term
!-                                      from a linear to a quadratic dependence
!-                                      on the zooplankton concentration. Added
!-                                      documentation that explains the
!-                                      rationale behind this change.
!- Revised:  06/22/09  W. Melendez : ----> GOMDOM. This application is based
!-                                      on LM3-Eutro (LM3 version 3.2.13) and 
!-                                      its purpose is to serve as a dissolved
!-                                      oxygen model for the Gulf of Mexico
!-                                      hypoxia project. NetCDF is the library 
!-                                      used to handle input/output binary
!-                                      files.
!- Revised: 02/25/05  W. Melendez     : Redimensioned the variable CNAME and
!-                                      added the module DIMS.
!- Revised: 02/10/05  W. Melendez     : Made cosmetic changes.
!- Revised: 12/21/99  M. Settles      : Remove class dependent predation
!-                                      calculation
!- Revised: 10/15/99  M. Settles      : Fix grazing for case of zero
!-                                      phytoplankton concentration.
!- Revised: 08/25/99  Pauer & Settles : Change to zooplankton kinetics; 
!-                                      remove linear dependence of grazing
!-                                      on total phyto concentration; 
!-                                      dimension of grazing rate CGZ
!- Revised: 08/03/99  M. Settles      :  Add module DMASS
!- Revised: 04/09/99  M. Settles      :  WQM_COM,WTRANS merger -> WTRBLK
!-
!------------------------------------------------------------------------------
USE Model_dim, ONLY:nf
USE STATES
USE EUT

IMPLICIT NONE

!
!------------------------------------------------------------------------------
!
REAL, INTENT(IN) :: f(nf)
REAL, INTENT(IN) :: TEMP
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k
REAL  :: ZPLANK     ! Zooplankton concentration
REAL  :: PHYTO      ! Phytoplankton concentration 
REAL  :: ZGRAZ      ! Factor in zoo grazing rate 
REAL  :: ZGRTH      ! Growth rate of zooplankton


!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
! Zoo concentration
!------------------------------------------------------------------------------
    ZPLANK = f(JZOO)                                  
!------------------------------------------------------------------------------
! Total phytoplankton concentration
!------------------------------------------------------------------------------
    PHYTO  = f(JDIA) + f(JGRE)    
               
    IF (PHYTO > 1.0E-20) THEN

!------------------------------------------------------------------------------
! Zooplankton grazing factor
!------------------------------------------------------------------------------
        ZGRAZ = PHYTO / (KSZ +  PHYTO) * CGZ * ZTHET**(TEMP - TZREF) &
              * ZPLANK

!------------------------------------------------------------------------------
! Diatom predation rate
!------------------------------------------------------------------------------
        PRD(i,j,k) = ZGRAZ * f(JDIA) / PHYTO                

!------------------------------------------------------------------------------
! Greens predation rate
!------------------------------------------------------------------------------
        PRG(i,j,k) = ZGRAZ * f(JGRE) / PHYTO  
              
    ELSE    

        ZGRAZ     = 0.0
        PRD(i,j,k) = 0.0
        PRG(i,j,k) = 0.0

    ENDIF

!------------------------------------------------------------------------------
! Zooplankton "growth"
! Higher predation (mortality) term in the zooplankton kinetics was 
! changed from a linear to a quadratic dependence on the concentration.
! Phil DePetro proposed the following equation for higher order predation
!
!     ZOOGRAZ = (ZDTH * (ZPLANK * 10^6)^2) / 10^6
!
! where ZDTH has units of 1/(mg/m^3 s).
! 
! The factor of 10^6 that is multipliying the zooplankton concentration 
! converts the units from kg/m^3 to mg/m^3. The factor of 10^6 in the 
! denominator converts the units back to kg/m^3. All this unit conversion 
! scheme involving the 10^6 conversion factor looks too arbitrary and from
! a mathematical standpoint it is simply acting as a type of coefficient 
! that scales up the mortality rate.  It's kind of hard to justify a unit
! conversion to mg/m^3 in the equation above and not some other units such 
! as ug/m^3. The main drive behind the use of this particular unit converstion 
! is that it makes the zooplankton population more stable and that it prevents 
! the phytoplankton from undergoing boom and bust cycles. With this in mind, 
! we decided to take a closer look at the equation above to see whether it 
! could be rewritten in a way that could be seen as being less dependent on the
! choice of unit conversions, and we found a way of doing it. The basic idea
! was to make the 10^6 conversion factor "disappear" from the equation in
! order to give it a cleaner look.  This was accomplished by noticing the 
! following:
!
!     (ZPLANK * 10^6)^2)/10^6 = ZPLANK * (10^6)^2/10^6 = ZPLANK * 10^6
!          
! Grouping 10^6 with ZDTH we get that
!                   
!     ZOOGRAZ = (ZDTH * 10^6) * ZPLANK^2
!
! This is equivalent to 
!
!     ZOOGRAZ = ZDTH * ZPLANK^2
!
! where ZDTH now has units of 1/(kg/m^3 s).  This means that in the input 
! deck users will have to specify ZDTH values that are much larger than 
! the ones that they used to input before.  For example: instead of setting 
! ZDTH to 1.0E-07, it should now be set to 1.0E-01 to be consistent with the 
! units of 1/(kg/m^3 s).
!
! See Cerco and Noel (2002) for more on details in using a quadratic form
! for zooplankton higher predation.
!
!------------------------------------------------------------------------------
    ZGRTH = (GREFF * ZGRAZ) - (ZDTH * ZPLANK * ZPLANK)                      

!------------------------------------------------------------------------------
! Change in zooplankton mass
!------------------------------------------------------------------------------
    DTM(JZOO) = DTM(JZOO) + ZGRTH  

!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE ZOO
