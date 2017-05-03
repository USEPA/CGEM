SUBROUTINE GD_Light_Model(f,S,Rad,PAR,fm,dz)
!------------------------------------------------------------------------------
!
!Input: Rad in Watts...actually, just convert it to Watts for this one
!so Rad is in photons/cm2/s or whatever.

USE Model_dim
USE STATES
USE EUT
USE INPUT_VARS, ONLY:Read_Solar
USE LIGHT_VARS, ONLY:PARfac

IMPLICIT NONE

REAL, INTENT(INOUT) :: f(im,jm,nsl,nf)
REAL, INTENT(IN)    :: S(im,jm,nsl),Rad(im,jm)
INTEGER, INTENT(IN) :: fm(im,jm)
REAL, INTENT(IN) :: dz(im,jm,nsl)
REAL, INTENT(OUT) :: PAR(im,jm,nsl)
REAL :: KESS(nsl),SAL_TERM,CHL_TERM,POC_TERM
REAL :: IATTOP, IATBOT(im,jm,nsl),OPTDEPTH,Rad_Watts(im,jm)
INTEGER :: i,j,k,nz

!
!------------------------------------------------------------------------------
!

Rad_Watts = Rad/2.77e14*PARfac

if(Read_Solar.ne.2) then
 Rad_Watts = Rad_Watts*0.48
endif

if(Read_Solar.eq.2) Rad_Watts = Rad/4.57

!GoMDOM LIGHT MODEL, No Wind Speed
 do j = 1,jm
     do i = 1,im 
       if(fm(i,j).eq.1) then
        nz = nza(i,j)
      do k = 1, nz
         SAL_TERM = 1.084E-06 * (S(i,j,k)**4)

          IF ((f(i,j,k,JDIA) + f(i,j,k,JGRE)) < 1.0E-07) THEN
               CHL_TERM = 0.0
          ELSE
               CHL_TERM = 0.2085 * LOG( (f(i,j,k,JDIA) * 1.0E6 / CCHLD) + &
                        & (f(i,j,k,JGRE) * 1.0E6 / CCHLG) )
          ENDIF

          POC_TERM = 0.7640 * SQRT( (f(i,j,k,JLOC) * 1.0E3) +  &
                   & (f(i,j,k,JROC) * 1.0E3) + (f(i,j,k,JZOO) * 1.0E3) )

          KESS(k) = ( ( -0.10 * (-0.5606 - SAL_TERM + CHL_TERM + POC_TERM) ) &
                  &  + 1 ) ** (1.0/(-0.10))
      enddo

      DO k = 1,1
         IATTOP    =  Rad_Watts(i,j)
         OPTDEPTH  =  KESS(k) * dz(i,j,k)
         IATBOT(i,j,k) =  IATTOP  * EXP(-OPTDEPTH)
         PAR(i,j,k)   =  (IATTOP - IATBOT(i,j,k)) / OPTDEPTH
      END DO
      IF(k>=2) THEN
       DO k = 2,nz
         IATTOP    =  IATBOT(i,j,k-1)
         OPTDEPTH  =  KESS(k) * dz(i,j,k) 
         IATBOT(i,j,k) =  IATTOP * EXP(-OPTDEPTH)
         PAR(i,j,k) =  (IATTOP - IATBOT(i,j,k)) / OPTDEPTH
       END DO
      ENDIF
       endif !End of if(fm(ij) statement
   enddo      ! end of do i block do loop
 enddo      ! end of do j block do loop


 PAR = PAR*4.57

!
!-----------------------------------------------------------------------------


RETURN

END SUBROUTINE GD_Light_Model 
