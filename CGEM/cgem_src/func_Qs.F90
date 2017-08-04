!------------------------------------------------------------
  SUBROUTINE func_Qs( Qn, Qp, f_Qn, f_Qp )   
!-- func_Qs is for a function of substrate 'S' --------------

  USE Model_dim
  USE INPUT_VARS_CGEM
      
  !--------------------------------------------------------------------------
  ! INPUT:  
  !   Qn - Phytoplankton Nitrogen Quota (mmol-N/cell) 
  !   Qp - Phytoplankton Nitrogen Quota (mmol-P/cell)
  !
  ! OUTPUT:
  !   f_Qn  - Nitrogen dependent growth function  
  !   f_Qp  - Phosphorus dependent growth function
  !
  !   Qmin/max_X - minimum and maximum nutrient cell quota (mmol/cell)
  ! 
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN), DIMENSION(nospA)  :: Qn    ! Phytoplankton Nitrogen Quota (mmol-N/cell)
    REAL, INTENT(IN), DIMENSION(nospA)  :: Qp    ! Phytoplankton Phosporus Quota (mmol-P/cell) 
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_Qn  ! Function based on N
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_Qp  ! Function based on P
 
    if (Which_uptake.eq.1) then !Michaelis-Menten
        f_Qn(:) = 1. 
        f_Qp(:) = 1. 
    else if (Which_uptake.eq.2) then !Geider(1998), Lehman(1975) is nfQs=1
        f_Qn(:) = ( (QmaxN(:) - Qn(:))/(QmaxN(:) - QminN(:)) ) ** nfQs(:)
        f_Qp(:) = ( (QmaxP(:) - Qp(:))/(QmaxP(:) - QminP(:)) ) ** nfQs(:)
    else if (Which_uptake.eq.3) then !Flynn(2003)
        f_Qn(:) = QmaxN(:)/Qn(:) 
        f_Qp(:) = QmaxP(:)/Qp(:)
    else  
        write(6,*) "Error in func_Qs"
        stop
    endif


    RETURN
  END SUBROUTINE func_Qs  
