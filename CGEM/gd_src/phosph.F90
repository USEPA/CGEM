SUBROUTINE PHOSPH(f,DTM,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: phosph.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
!-
!-   Purpose and Methods : Kinetic sources and sinks of phosphorus are
!-                         computed in this subroutine.
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:   None
!-
!-   Calls:      REPORT
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
!------------------------------------------------------------------------------
!

USE Model_dim
USE STATES
USE EUT

IMPLICIT NONE

REAL, INTENT(IN) :: f(nf)
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k

REAL :: ALGP              ! Algal phosphate                         
REAL :: DOP               ! Dissolved organic P mass derivative
REAL :: POPL              ! Labile detrital P produced by
                          ! algal metabolism and predation
REAL :: POPR              ! Refractory detrital P produced by
                          ! algal metabolism and predation

REAL :: PO4D              ! Change in phosphate from diatom source/sink
REAL :: PO4G              ! Change in phosphate from greens source/sink
REAL :: MNLDOP            ! Dissolved P mineralization mass derivative
REAL :: HDRLPOP           ! Labile detrital P hydrolysis mass derivative
REAL :: HDRRPOP           ! Refractory detrital P hydrolysis mass derivative

REAL :: ZDEATH            ! Zooplankton mortality concentration
REAL :: PO4ZOO            ! Phosphate released by zoo. mortality
REAL :: DOPZOO            ! Dissolved organic P released by zoo. mortality
REAL :: LPOPZOO           ! Labile organic P released by zoo. mortality
REAL :: RPOPZOO           ! Refractory organic P released by zoo. mortality


REAL :: KDOP              ! Dissolved organic phosphorous mineralization rate [s^-1]
REAL :: KPOPL             ! Labile organic phosphorous hydrolysis rate [s^-1]
REAL :: KPOPR             ! Labile organic phosphorous hydrolysis rate [s^-1]

REAL :: FRAC_SRP          ! Fraction of phosphorous that is in the silica
REAL :: FRAC_DOP          ! Fraction of phosphorous that is dissolved

REAL :: ALG               ! Total phytoplankton concentration (dia+gre)


!
!------------------------------------------------------------------------------
!

 
!
!  Contribution from diatoms
!
   ALGP = APCP * f(JDIA)
   IF (f(JSRP) > 0.0) THEN
       IF (AVFRAC > 0.0) THEN
          FRAC_SRP = f(JSRP)/(f(JSRP) + AVFRAC*f(JDOP))
       ELSE
          FRAC_SRP = 1.0
       ENDIF
   ELSE
       FRAC_SRP = 0.0
   ENDIF
   PO4D = (FPID * BMD(i,j,k) - PD(i,j,k) * FRAC_SRP) * ALGP + &
           FPIP * (1.0 - GREFF) * PRD(i,j,k) * APCP
   IF (AVFRAC*f(JDOP) > 0.0)THEN
      FRAC_DOP = AVFRAC*f(JDOP)/(f(JSRP) + AVFRAC*f(JDOP))
   ELSE
      FRAC_DOP = 0.0
   ENDIF
   DOP = (FPDD*BMD(i,j,k) - PD(i,j,k)*FRAC_DOP)*ALGP + & 
         FPDP*(1.0-GREFF)*PRD(i,j,k)*APCP
   POPL = FPLD*BMD(i,j,k)*ALGP + FPLP*(1.0-GREFF)*PRD(i,j,k)*APCP
   POPR = FPRD*BMD(i,j,k)*ALGP + FPRP*(1.0-GREFF)*PRD(i,j,k)*APCP

   DTM(JSRP) = DTM(JSRP) + PO4D 
   DTM(JDOP) = DTM(JDOP) + DOP 
   DTM(JLOP) = DTM(JLOP) + POPL 
   DTM(JROP) = DTM(JROP) + POPR

!
!  Contribution from greens
!
   ALGP = APCP * f(JGRE)
   IF (f(JSRP) > 0.0) THEN
      IF (AVFRAC > 0.0) THEN
         FRAC_SRP = f(JSRP)/(f(JSRP) + AVFRAC*f(JDOP))
      ELSE
         FRAC_SRP = 1.0
      ENDIF
   ELSE
      FRAC_SRP = 0.0
   ENDIF
   PO4G = (FPIG * BMG(i,j,k) - PG(i,j,k) * FRAC_SRP) * ALGP + & 
           FPIP * (1.0 - GREFF) * PRG(i,j,k) * APCP

   IF (AVFRAC*f(JDOP) > 0.0) THEN
      FRAC_DOP = AVFRAC*f(JDOP)/(f(JSRP) + AVFRAC*f(JDOP))
   ELSE
      FRAC_DOP = 0.0
   ENDIF
   DOP  = (FPDG*BMG(i,j,k) - PG(i,j,k)*FRAC_DOP)*ALGP + &
           FPDP*(1.0-GREFF)*PRG(i,j,k)*APCP
   POPL = FPLG*BMG(i,j,k)*ALGP + FPLP*(1.0-GREFF)*PRG(i,j,k)*APCP
   POPR = FPRG*BMG(i,j,k)*ALGP + FPRP*(1.0-GREFF)*PRG(i,j,k)*APCP

   DTM(JSRP) = DTM(JSRP) + PO4G
   DTM(JDOP) = DTM(JDOP) + DOP 
   DTM(JLOP) = DTM(JLOP) + POPL
   DTM(JROP) = DTM(JROP) + POPR
                                                            
!------------------------------------------------------------------------------
!  Zooplankton 
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Zooplankton mortality: it was changed from a linear to quadratic dependence
! on the zooplankton concentration. See the zoo.F90 subroutine for more
! details about this change.
! [ZDTH] = 1/(kg/m^3 s)
!------------------------------------------------------------------------------
   ZDEATH = ZDTH * f(JZOO) * f(JZOO)  

!------------------------------------------------------------------------------
! PO4 component
!------------------------------------------------------------------------------
   PO4ZOO  = FPIZ*APCP*ZDEATH   

!------------------------------------------------------------------------------
! DOP component
!------------------------------------------------------------------------------
   DOPZOO  = FPDZ*APCP*ZDEATH                                    

!------------------------------------------------------------------------------
! Labile particulate component
!------------------------------------------------------------------------------
   LPOPZOO = FPLZ*APCP*ZDEATH                                    

!------------------------------------------------------------------------------
!  Refractory particulate component
!------------------------------------------------------------------------------
   RPOPZOO = FPRZ*APCP*ZDEATH                               

!------------------------------------------------------------------------------
!  PO4 time derivative
!------------------------------------------------------------------------------
   DTM(JSRP) = DTM(JSRP) + PO4ZOO   

!------------------------------------------------------------------------------
!  DOP time derivative
!------------------------------------------------------------------------------
   DTM(JDOP) = DTM(JDOP) + DOPZOO             

!------------------------------------------------------------------------------
!  Labile particulate time derivative
!------------------------------------------------------------------------------
   DTM(JLOP) = DTM(JLOP) + LPOPZOO            

!------------------------------------------------------------------------------
!  Refractory particulate time derivative
!------------------------------------------------------------------------------
   DTM(JROP) = DTM(JROP) + RPOPZOO         

!------------------------------------------------------------------------------
!  Mineralization and hydrolysis
!------------------------------------------------------------------------------

   ALG = f(JGRE)+f(JDIA)
   
   KDOP  = KDP + KDPALG*ALG*KHP/(KHP+f(JSRP))
   KPOPL = KLP + KLPALG*ALG*KHP/(KHP+f(JSRP))
   KPOPR = KRP + KRPALG*ALG*KHP/(KHP+f(JSRP))
   
   MNLDOP  = KDOP*FTMNL(i,j,k)*f(JDOP)
   HDRLPOP = KPOPL*FTHDR(i,j,k)*f(JLOP)
   HDRRPOP = KPOPR*FTHDR(i,j,k)*f(JROP)

   DTM(JSRP) = DTM(JSRP) + MNLDOP 
   DTM(JDOP) = DTM(JDOP) + HDRRPOP + HDRLPOP - MNLDOP
   DTM(JLOP) = DTM(JLOP) - HDRLPOP 
   DTM(JROP) = DTM(JROP) - HDRRPOP 

!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE PHOSPH




