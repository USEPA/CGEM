! This file contains code for calculating mg chlorophyll-a from phytoplankton
! abundance. There are currently two methods:
! 1. Regression
! 2. Cloern Chl:C ratio

MODULE Calc_Chla

CONTAINS

  FUNCTION Chla_Regression(A_k) RESULT(Chla_tot)

    use Model_dim
    use Conversions 
    implicit none

    ! Input parameters
    real, intent(in) :: A_k(nospA,nsl)  ! A's number density, cells/m3

    ! Function return value
    real :: Chla_tot(nsl)

    ! Local variables
    integer :: k, isp

    DO k = 1, nz
       Chla_tot(k) = 0.0
       DO isp = 1, nospA
          Chla_tot(k) =  Chla_tot(k) + A_k(isp,k) * C2_chla_mg !C2_chla_mg defined in Conversions module
       ENDDO ! isp = 1, nospA
    ENDDO ! k = 1, nz
    RETURN
  END FUNCTION Chla_Regression


  ! This routine uses the Chl:C ratio from Cloern et al. 1995 to calculate
  ! total Chlorophyll-a (in mg m-3) for each sigma layer.
  ! The resulting ratio is multiplied by the fixed carbon per cell and the
  ! abundance (A) to get the chlorophyll quantity.

  FUNCTION Chla_Cloern (A_k, Qn_k, Qp_k, N_k, P_k, Si_k, T_k, aRad, Chl_C) RESULT(Chla_tot)

    use Model_dim
    use INPUT_VARS
    implicit none

    ! Input parameters
    real, intent(in) :: A_k(nospA,nsl)  ! A's number density, cells/m3
    real, intent(in) :: Qn_k(nospA,nsl) ! A's Nitrogen Quota (mmol-N/cell)
    real, intent(in) :: Qp_k(nospA,nsl) ! A's Phosphorus Quota (mmol-P/cell)
    real, intent(in) :: N_k(nsl)        ! Nitrogen, mmol/m3
    real, intent(in) :: P_k(nsl)        ! Phosphorus, mmol/m3
    real, intent(in) :: Si_k(nsl)       ! Silica, mmol/m3 
    real, intent(in) :: T_k(nsl)  ! Temperature in Celsius
    real, intent(in) :: aRad(nsl) ! Daily total irradiance per layer
    real, intent(out) :: Chl_C(nospA,nsl)

    ! Function return value
    real :: Chla_tot(nsl)

    ! Local variables
    integer :: k, isp
    real :: A_carbon ! Total Carbon, mg
    real, dimension(nospA) :: f_N, f_P, f_Si  ! Nutrient growth functions 
    real :: mu  ! Growth factor from most limited nutrient
    real :: f_E ! Light factor

    DO k = 1, nz
       ! Get nutrient limited growth rates. Copied from calc_Agrow()
       call func_S( Qn_k(:,k), Qp_k(:,k), N_k(k), P_k(k), Si_k(k), f_N, f_P, f_Si )

       Chla_tot(k) = 0.0

       DO isp = 1, nospA
          ! Nutrient dependence, limiting nutrient
          mu = AMIN1( f_N(isp), f_P(isp), f_Si(isp) ) 
          ! Light dependence
          f_E = exp(-0.059*aRad(k))
          ! This is the Cloern expression
          Chl_C(isp,k) = 0.003 + 0.0154*exp(0.050*T_k(k)) * mu * f_E
         ! Total Carbon = cells/m3 * mmol C/cell * 12mg C/mmol C
          A_carbon = A_k(isp,k) * Qc(isp) * 12.
          ! mg chl-a = Chl:C * mg carbon
          Chla_tot(k) =  Chla_tot(k) + (Chl_C(isp,k) * A_carbon)
       ENDDO ! isp = 1, nospA

    ENDDO ! k = 1, nz


    RETURN
  END FUNCTION Chla_Cloern

END MODULE Calc_Chla
