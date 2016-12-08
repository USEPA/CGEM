SUBROUTINE PHOSPH_DROOP(f,DTM,i,j,k)
!------------------------------------------------------------------------------
!-
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
!-   Created:    May 13, 2016
!-   Authors:    Wilson Melendez
!-
!- Revised: September 22, 2016  W. Melendez : Removed ALGP. Added PO4AVL to OMP
!-                                            private loop.
!- Revised: August 18, 2016  W. Melendez : Changed QminPD and QminPG to QPD and
!-                                         QPG in the respiration term.
!- Revised: July 19, 2016    W. Melendez : Changed QPD and QPG to QminPD and
!-                                         QminPG in the respiration term.
!-                                         Added QmaxPD and QmaxPG. 
!------------------------------------------------------------------------------
!
USE Model_dim
USE STATES
USE EUT

IMPLICIT NONE

REAL, INTENT(IN) :: f(nf)
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k

REAL :: DOP               ! Dissolved organic P mass derivative
REAL :: POPL              ! Labile detrital P produced by
                          ! algal metabolism and predation
REAL :: POPR              ! Refractory detrital P produced by
                          ! algal metabolism and predation

REAL :: PO4AVL            ! Dissolved phosphate concentration

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
REAL :: UptakePD          ! Phosphorus uptake by diatoms
REAL :: UptakePG          ! Phosphorus uptake by greens
REAL ::  LuxuryPD         ! Luxury P in diatoms 
REAL ::  LuxuryPG         ! Luxury P in greens
REAL :: InternalP_dia     ! Change in diatom's internal P 
REAL :: InternalP_gre     ! Chaknge in green's internal P


!
!------------------------------------------------------------------------------
!

 
!
!  Contribution from diatoms
!
   FRAC_DOP = 0.0
   IF (f(JSRP) > 0.0) THEN
       FRAC_SRP = f(JSRP)/(f(JSRP) + AVFRAC * f(JDOP))
       FRAC_DOP = AVFRAC * f(JDOP)/(f(JSRP) + AVFRAC * f(JDOP))
   ELSE
       FRAC_SRP = 0.0
       IF (AVFRAC * f(JDOP) > 0.0) FRAC_DOP = 1.0
   ENDIF

   PO4AVL = f(JSRP) + AVFRAC * f(JDOP)

   UptakePD = 0.0
   IF ((f(JDIA) > 0.0) .AND. (QPD(i,j,k) < QmaxPD)) UptakePD = UpPMaxD * PO4AVL / (KhIntPD + PO4AVL)
   IF (IAVG(i,j,k) <= TINY(IAVG(i,j,k))) UptakePD = UptakePD * ILMUL

   LuxuryPD = 0.0
   IF (QPD(i,j,k) > QminPD) LuxuryPD = QPD(i,j,k) - QminPD

   PO4D = (FIntPID * BMD(i,j,k) * QPD(i,j,k) - UptakePD * FRAC_SRP) * f(JDIA) + &
           FIntLuxPIP * PRD(i,j,k) * LuxuryPD + &
           FIntStrPIP * (1.0 - GREFF) * PRD(i,j,k) * QminPD

   DOP = (FIntPDD * BMD(i,j,k) * QPD(i,j,k) - UptakePD * FRAC_DOP) * f(JDIA) + &
          FIntLuxPDP * PRD(i,j,k) * LuxuryPD + &
          FIntStrPDP * (1.0 - GREFF) * PRD(i,j,k) * QminPD

   POPL = FIntPLD * BMD(i,j,k) * QPD(i,j,k) * f(JDIA) + &
          FIntLuxPLP * PRD(i,j,k) * LuxuryPD + &
          FIntStrPLP * (1.0 - GREFF) * PRD(i,j,k) * QminPD

   POPR = FIntPRD * BMD(i,j,k) * QPD(i,j,k) * f(JDIA) + &
          FIntLuxPRP * PRD(i,j,k) * LuxuryPD + &
          FIntStrPRP * (1.0 - GREFF) * PRD(i,j,k) * QminPD

   InternalP_dia = (UptakePD - BMD(i,j,k) * QPD(i,j,k)) * f(JDIA) - &
                   PRD(i,j,k) * QPD(i,j,k)

   DTM(JSRP) = DTM(JSRP) + PO4D 
   DTM(JDOP) = DTM(JDOP) + DOP 
   DTM(JLOP) = DTM(JLOP) + POPL  
   DTM(JROP) = DTM(JROP) + POPR 
   DTM(JDIAP) = DTM(JDIAP) + InternalP_dia 

!
!  Contribution from greens
!
   UptakePG = 0.0
   IF ((f(JGRE) > 0.0) .AND. (QPG(i,j,k) < QmaxPG)) UptakePG = UpPMaxG * PO4AVL / (KhIntPG + PO4AVL)
   IF (IAVG(i,j,k) <= TINY(IAVG(i,j,k))) UptakePG = UptakePG * ILMUL

   LuxuryPG = 0.0
   IF (QPG(i,j,k) > QminPG) LuxuryPG = QPG(i,j,k) - QminPG

   PO4G = (FIntPIG * BMG(i,j,k) * QPG(i,j,k) - UptakePG * FRAC_SRP) * f(JGRE) + & 
           FIntLuxPIP * PRG(i,j,k) *  LuxuryPG + &
           FIntStrPIP * (1.0 - GREFF) * PRG(i,j,k) * QminPG

   DOP  = (FIntPDG * BMG(i,j,k) * QPG(i,j,k) - UptakePG * FRAC_DOP) * f(JGRE) + &
           FIntLuxPDP * PRG(i,j,k) * LuxuryPG + &
           FIntStrPDP * (1.0 - GREFF) * PRG(i,j,k) * QminPG

   POPL = FIntPLG * BMG(i,j,k) * QPG(i,j,k) * f(JGRE) + &
          FIntLuxPLP * PRG(i,j,k) * LuxuryPG + &
          FIntStrPLP * (1.0 - GREFF) * PRG(i,j,k) * QminPG

   POPR = FIntPRG * BMG(i,j,k) * QPG(i,j,k) * f(JGRE) + &
          FIntLuxPRP * PRG(i,j,k) * LuxuryPG + &
          FIntStrPRP * (1.0 - GREFF) * PRG(i,j,k) * QminPG

   InternalP_gre = (UptakePG - BMG(i,j,k) * QPG(i,j,k)) * f(JGRE) - &
                   PRG(i,j,k) * QPG(i,j,k)

   DTM(JSRP) = DTM(JSRP) + PO4G 
   DTM(JDOP) = DTM(JDOP) + DOP 
   DTM(JLOP) = DTM(JLOP) + POPL
   DTM(JROP) = DTM(JROP) + POPR 
   DTM(JGREP) = DTM(JGREP) + InternalP_gre
                                                            
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
   PO4ZOO  = FPIZ * APCP * ZDEATH   

!------------------------------------------------------------------------------
! DOP component
!------------------------------------------------------------------------------
   DOPZOO  = FPDZ * APCP * ZDEATH                                    

!------------------------------------------------------------------------------
! Labile particulate component
!------------------------------------------------------------------------------
   LPOPZOO = FPLZ * APCP * ZDEATH                                    

!------------------------------------------------------------------------------
!  Refractory particulate component
!------------------------------------------------------------------------------
   RPOPZOO = FPRZ * APCP * ZDEATH                               

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

   ALG = f(JGRE) + f(JDIA)
   
   KDOP  = KDP + KDPALG*ALG*KHP/(KHP+f(JSRP))
   KPOPL = KLP + KLPALG*ALG*KHP/(KHP+f(JSRP))
   KPOPR = KRP + KRPALG*ALG*KHP/(KHP+f(JSRP))
   
   MNLDOP  = KDOP*FTMNL(i,j,k)*f(JDOP)
   HDRLPOP = KPOPL*FTHDR(i,j,k)*f(JLOP)
   HDRRPOP = KPOPR*FTHDR(i,j,k)*f(JROP)
   DTM(JSRP) = DTM(JSRP) + MNLDOP 
   DTM(JDOP) = DTM(JDOP) + (HDRRPOP + HDRLPOP - MNLDOP) 
   DTM(JLOP) = DTM(JLOP) - HDRLPOP 
   DTM(JROP) = DTM(JROP) - HDRRPOP 


!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE PHOSPH_DROOP

