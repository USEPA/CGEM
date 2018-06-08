SUBROUTINE SILICA(f,DTM,TEMP,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: silica.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
!-
!-   Purpose and Methods : Kinetic sources and sinks of silica are
!-                         computed in this subroutine.
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
!------------------------------------------------------------------------------
!

USE Model_dim
USE STATES
USE EUT

IMPLICIT NONE

REAL, INTENT(IN) :: f(nf)
REAL, INTENT(IN) :: TEMP
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k


REAL :: SU                         ! Unavailable silica
REAL :: SAD                        ! Silica in diatoms

REAL :: PSD                        ! Change in particulate silica dissolution
REAL :: DAS                        ! Change in available silica
REAL :: PUS                        ! Change in particulate unavailable silica
                                   ! metabolism and predation



!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
! Particulate biogenic silica
!------------------------------------------------------------------------------
   SU = f(JSU)   

!------------------------------------------------------------------------------
! Diatom silica
!------------------------------------------------------------------------------
   SAD = ASCD * f(JDIA)                            

!------------------------------------------------------------------------------
! Particulate biogenic silica dissolution
!------------------------------------------------------------------------------
   PSD = KSUA * SU * EXP(KTSUA * (TEMP - TRSUA))        

!------------------------------------------------------------------------------
! Available silica: predation/production
!------------------------------------------------------------------------------
   DAS = FSAP * PRD(i,j,k) * ASCD - (PD(i,j,k) * SAD)             

!------------------------------------------------------------------------------
! Unavailable silica: metabolism/predation
!------------------------------------------------------------------------------
   PUS = BMD(i,j,k) * SAD + (1.0 - FSAP) * PRD(i,j,k) * ASCD      

!------------------------------------------------------------------------------
! Dissolved available silica mass derivative
!------------------------------------------------------------------------------
   DTM(JSA)  = DTM(JSA) + PSD+DAS  

!------------------------------------------------------------------------------
! Particulate biogenic silica mass derivative
!------------------------------------------------------------------------------
   DTM(JSU) = DTM(JSU) + PUS - PSD   

   
!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE SILICA

