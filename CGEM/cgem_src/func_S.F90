!------------------------------------------------------------
  SUBROUTINE func_S( Qn, Qp, N, P, Si, f_N, f_P, f_Si )   
!-- func_S is for a function of substrate 'S' --------------- 

  USE Model_dim
  USE INPUT_VARS_CGEM
      
  !--------------------------------------------------------------------------
  ! INPUT:  
  !   Qn - Phytoplankton Nitrogen Quota (mmol-N/cell) 
  !   Qp - Phytoplankton Nitrogen Quota (mmol-P/cell)
  !   Si - Silica concentration in seawater (mmol-Si/m3)
  !   N  - NO3+NH4 concentration in seawater (mmol-N/m3)
  !   P  - PO4 concentration in seawater (mmol-P/m3)
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
    REAL, INTENT(IN)                    :: N     ! NO3+NH4 concentration in seawater (mmol-N/m3)
    REAL, INTENT(IN)                    :: P     ! PO4 concentration in seawater (mmol-P/m3)
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_N   ! Function based on N
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_P   ! Function based on P
    REAL, INTENT(OUT), DIMENSION(nospA) :: f_Si  ! Function based on Si
    REAL :: KHNG(nospA), KHPG(nospA), KHND(nospA), KHPD(nospA), KHSD(nospA)
    integer :: isp

!!!Parameters that should go in the input file:
    KHNG = 2.50E-05 * 7.1e4        ! KHNG: mean N half sat  (gre)
    KHPG = 2.50E-06 * 3.2e4        ! KHPG: mean P half sat  (gre)
    KHND = 2.50E-05 * 7.1e4        ! KHND: mean N half sat  (dia)
    KHPD = 2.50E-06 * 3.2e4        ! KHPD: mean P half sat  (dia)
    KHSD = 2.50E-05 * 3.6e4        ! KHSD: mean Si half sat (dia)
!Try with CGEM parameters:
    KHNG = 1.13 !2.50E-05 * 7.1e4        ! KHNG: mean N half sat  (gre)
    KHPG = 0.51 !2.50E-06 * 3.2e4        ! KHPG: mean P half sat  (gre)
    KHND = 1.13 !2.50E-05 * 7.1e4        ! KHND: mean N half sat  (dia)
    KHPD = 0.51 !2.50E-06 * 3.2e4        ! KHPD: mean P half sat  (dia)
    KHSD = 1.13 !2.50E-05 * 3.6e4        ! KHSD: mean Si half sat (dia)


!WQEM is 2.50E-05, ours is 1.13         ! KHSD: mean Si half sat (dia)
!WQEM is kg/m3, ours is mmol/m3
! Conversions:
! Si kg/m3 * mmol/28mg * 1e6 mg/kg = 3.6e4 
! N  kg/m3 * mmol/14mg * 1e6 mg/kg = 7.1e4
! P  kg/m3 * mmol/31mg * 1e6 mg/kg = 3.2e4 
! KHNG --> 1.8 
! KHPG --> 0.08 
! KHSD --> .9 
! In CGEM, we have Kn=1.13, Kp=0.51, Ksi=1.13
 
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
    else if (Which_quota.eq.4) then !WQEM
        do isp=1,nospA
           if(is_diatom(isp).eq.1) then !Diatoms
             f_N(isp) = N / ( N + KHND(isp) ) !Monod
             f_P(isp) = P / ( P + KHPD(isp) ) !Monod
           else                    !Non-diatoms
             f_N(isp) = N / ( N + KHNG(isp) ) !Monod
             f_P(isp) = P / ( P + KHPG(isp) ) !Monod
           endif
        enddo
    else
        write(6,*) "Error in func_S"
        stop
    endif

    if (Which_quota.eq.4) then
        do isp=1,nospA
           if(is_diatom(isp).eq.1) then !Diatoms
             f_Si(isp) = Si / ( Si + KHSD(isp) ) !Monod
           else                    !Non-diatoms
             f_Si(isp) = 9999.     !Greens have no Si
           endif
        enddo
    else
     f_Si(:) = Si / ( Si + Ksi(:) ) !Monod 
    endif

    RETURN
  END SUBROUTINE func_S  
