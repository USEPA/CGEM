!------------------------------------------------------------
  SUBROUTINE func_S( Qn, Qp, Si, f_N, f_P, f_Si )   
!-- func_S is for a function of substrate 'S' --------------- 

  USE Model_dim
  USE INPUT_VARS
      
  !--------------------------------------------------------------------------
  ! INPUT:  
  !   Qn - Phytoplankton Nitrogen Quota (mmol-N/cell) 
  !   Qp - Phytoplankton Nitrogen Quota (mmol-P/cell)
  !   Si - Silica concentration in seawater (mmol-Si/m3)
  !
  ! OUTPUT:
  !   f_N  - Nitrogen dependent growth function  
  !   f_P  - Phosphorus dependent growth function
  !   f_Si - Silica dependent growth function
  !
  !   K_X - half saturation constants for phytoplankton group  (X mmol/m3)
  !   Qmin/max_X - minimum and maximum nutrient cell quota (mmol/cell)
  ! 
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN), DIMENSION(nospA)  :: Qn    ! Phytoplankton Nitrogen Quota (mmol-N/cell)
    REAL, INTENT(IN), DIMENSION(nospA)  :: Qp    ! Phytoplankton Phosporus Quota (mmol-P/cell) 
    REAL, INTENT(IN)                    :: Si    ! Silica concentration in seawater (mmol-Si/m3)
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_N    ! Function based on N
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_P    ! Function based on P
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_Si   ! Function based on Si
 
    if (Which_quota.eq.1) then !Droop(1968)
        f_N(:) = ( Qn(:) - QminN(:) ) / Qn(:)
        f_P(:) = ( Qp(:) - QminP(:) ) / Qp(:)       
    else if (Which_quota.eq.2) then !Nyholm(1978)
        f_N(:) = ( Qn(:) - QminN(:) ) / ( QmaxN(:) - QminN(:) )
        f_P(:) = ( Qp(:) - QminP(:) ) / ( QmaxP(:) - QminP(:) )
    else if (Which_quota.eq.3) then !Flynn(2003)
        f_N(:) = ( 1. + KQn(:) ) * ( Qn(:) - QminN(:) ) /        &
     &           ( Qn(:) - QminN(:) + KQn(:)*( QmaxN(:) - QminN(:) ) )
        f_P(:) = ( 1. + KQp(:) ) * ( Qp(:) - QminP(:) ) /        &
     &           ( Qp(:) - QminP(:) + KQp(:)*( QmaxP(:) - QminP(:) ) )
    else  !Default is Droop
        f_N(:) = ( Qn(:) - QminN(:) ) / Qn(:)
        f_P(:) = ( Qp(:) - QminP(:) ) / Qp(:)
    endif

    f_Si(:) = Si / ( Si + Ksi(:) ) !Monod 

    RETURN
  END SUBROUTINE func_S  
