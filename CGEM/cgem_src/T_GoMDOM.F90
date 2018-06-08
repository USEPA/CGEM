!---------------------------------------------------------------------------
  SUBROUTINE T_GoMDOM( T, Tadj )   
!---------------------------------------------------------------------------

  USE Model_dim
  USE TEMP_VARS     
  USE INPUT_VARS_CGEM, ONLY: is_diatom      
  !--------------------------------------------------------------------------
  ! INPUT:  
  !   T = temperature [degree C]
  !
  ! OUTPUT:
  !   Temperature Adjustment 
  ! 
  ! REFERENCES:
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL, INTENT(OUT), DIMENSION(nospA+nospZ) :: Tadj 
    REAL :: TMD, TMG, TZREF(nospZ), ZTHET
    REAL :: KTGD1, KTGD2, KTGG1, KTGG2
    integer :: isp, isz

!!!Parameters that should be in the input file:
    TMD = 32            ! TMD: opt temp dia
    TMG = 32            ! TMG: opt temp gre
    TZREF = 25          ! TZREF: opt pred temp
    ZTHET = 1.05        ! ZTHET: temp coeff pred
    KTGD1 = 0.0030      ! KTGD1: temp coeff < (dia)
    KTGD2 = 0.00        ! KTGD2: temp coeff > (dia)
    KTGG1 = 0.0030      ! KTGG1: temp coeff < (gre)
    KTGG2 = 0.00        ! KTGG2: temp coeff > (gre)
!!!End input file parameters
!Try CGEM parameters
    TMD = 17 !32            ! TMD: opt temp dia
    TMG = 17 !32            ! TMG: opt temp gre
    TZREF(1) = 25          ! TZREF: opt pred temp
    TZREF(2) = 26
    ZTHET = 1.05        ! ZTHET: temp coeff pred
    KTGD1 = 0.0035 !0.0030      ! KTGD1: temp coeff < (dia)
    KTGD2 = 0.001 !0.00        ! KTGD2: temp coeff > (dia)
    KTGG1 = 0.0035 !0.0030      ! KTGG1: temp coeff < (gre)
    KTGG2 = 0.001 !0.00        ! KTGG2: temp coeff > (gre)

    do isp=1,nospA

     if(is_diatom(isp).eq.1) then
       if(T.lt.TMD) then !For "diatoms"
        Tadj(isp) = exp(-KTGD1 * (T - TMD)**2)
       else
        Tadj(isp) = exp(-KTGD2 * (TMD - T)**2)
       endif
     else
       if(T.lt.TMG) then !For "greens"
        Tadj(isp) = exp(-KTGG1 * (T - TMG)**2)
       else
        Tadj(isp) = exp(-KTGG2 * (TMG - T)**2)
       endif
     endif

    enddo

    !For Zooplankton
    do isz=1,nospZ
      Tadj(nospA+isz) = ZTHET**(T - TZREF(isz))
    enddo

    RETURN
  END SUBROUTINE T_GoMDOM  
