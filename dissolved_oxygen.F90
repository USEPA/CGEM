SUBROUTINE DISSOLVED_OXYGEN(f,DTM,TEMP,i,j,k)
!------------------------------------------------------------------------------
!-
!-  $Id: dissolved_oxygen.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-  $Locker: wmelende $
!-
!-  Description : This subroutine performs kinetics calculations of 
!-                dissolved oxygen. 
!-
!-  Inputs  :   None
!-  Outputs :   None
!-
!-  Calls: None     
!-  Called by:  EUTRO
!-
!-  Created: 09/03/2009  James Pauer
!-
!------------------------------------------------------------------------------

USE Model_dim
USE EUT
USE FLAGS
USE STATES
 
IMPLICIT NONE

REAL, INTENT(IN) :: f(nf)
REAL, INTENT(IN) :: TEMP
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k


REAL :: ALG               ! Phytoplankton concentration (dia+gre)
REAL :: DOPRODD
REAL :: DOMETD
REAL :: DOPRODG
REAL :: DOMETG
REAL :: DOPREDD           ! DO consumption due to diatoms predation
REAL :: DOPREDG           ! DO consumption due to greens predation
REAL :: DOZOO             ! DO consumption due to zooplankton mortality
REAL :: DOMNLDOC
REAL :: DOCOD
REAL :: FTN
REAL :: KDOC              ! Mineralization rate of DOC [sec^-1]
REAL :: NT
REAL :: ZDEATH            ! Zooplankton mortality rate



!------------------------------------------------------------------------------
! Skip if not performing dissolved oxygen kinetics.
!------------------------------------------------------------------------------
IF (.NOT. DO_DO2) RETURN

!------------------------------------------------------------------------------
!- Nitrification based on LM3-Eutro/CE-Qual-ICM models
!------------------------------------------------------------------------------
   IF (TEMP < TMNT) THEN
       FTN = EXP(-KTNT1 * (TEMP - TMNT)**2)
   ELSE
       FTN = EXP(-KTNT2 * (TEMP - TMNT)**2)
   ENDIF


   IF (KHNNT > 0.0) THEN
       NT = FTN * NTM *                                    &
            ( f(JNH4) / (KHNNT  + f(JNH4)) ) *   &
            ( f(JDO2) / (KHDONT + f(JDO2)) )
   ELSEIF (f(JNH4) > 0.0) THEN
       NT = FTN * NTM * f(JDO2)/(KHDONT + f(JDO2))
   ELSE
       NT = 0.0
   ENDIF

   DTM(JDO2) = DTM(JDO2) - RNTO * NT 


!------------------------------------------------------------------------------
!- Oxygen sources and sinks from diatoms and greens. We
!- assume a threshold oxygen value below which all carbon 
!- is excreted as DOC, thus no DO consumed and CO2 generated
!- however, changes are necessary in other subroutines to 
!- change DOC fraction, FCDD when DO is below threshold value.
!- Also a oxygen:carbon conversion factor must be defined and
!- a value assigned. 
!------------------------------------------------------------------------------

    DOPRODD = PD(i,j,k) * f(JDIA) * RCDO
    DOPRODG = PG(i,j,k) * f(JGRE) * RCDO

    IF (f(JDO2) > (2.0 * 1.0E-03)) THEN
        DOMETD = (1.0 - (FCDD + FCLD + FCRD)) * BMD(i,j,k) * f(JDIA) * RCDO
        DOMETG = (1.0 - (FCDG + FCLG + FCRG)) * BMG(i,j,k) * f(JGRE) * RCDO
        DOPREDD = (1.0 - GREFF) * (1. - (FCDP + FCLP + FCRP)) * &
                 &  PRD(i,j,k) * RCDO
        DOPREDG = (1.0 - GREFF) * (1. - (FCDP + FCLP + FCRP)) * &
                 &  PRG(i,j,k) * RCDO
        ZDEATH = ZDTH * f(JZOO) * f(JZOO)
        DOZOO   =  (1.0 - (FCDZ + FCLZ + FCRZ)) * ZDEATH * RCDO
    ELSE
        DOMETD  = 0.0
        DOMETG  = 0.0
        DOPREDD = 0.0
        DOPREDG = 0.0
        DOZOO   = 0.0
    ENDIF

    DTM(JDO2) = DTM(JDO2) + DOPRODD - DOMETD + DOPRODG - DOMETG -  & 
                   &  DOPREDD - DOPREDG - DOZOO


!------------------------------------------------------------------------------
!- DOC and COD subroutine
!------------------------------------------------------------------------------

    ALG      = f(JGRE) + f(JDIA)
    KDOC     = (KDC + (KDCALG * ALG)) * FTMNL(i,j,k) 
    DOMNLDOC = ( f(JDO2) / (KHODOC + f(JDO2)) )      & 
             *  KDOC * f(JDOC) * RCDO
    
    !(WOD undefined)  IF (DO_WOD) THEN
    !    DOCOD  = ( f(JDO2) / (KHOCOD + f(JDO2)) )      &
    !           *  KCOD * WOD(i,j,k) * RCDO 
    !ELSE
        DOCOD  = 0.0
    !ENDIF

    DTM(JDO2) = DTM(JDO2) -(DOMNLDOC + DOCOD) 


!------------------------------------------------------------------------------
END SUBROUTINE DISSOLVED_OXYGEN
