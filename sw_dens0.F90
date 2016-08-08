!***********************************************************************
      FUNCTION sw_dens0( S, T ) RESULT( RES )  
      
  !--------------------------------------------------------------------------
  ! Density of Sea Water at atmospheric pressure using UNESCO 1983 (EOS 1980).
  ! Based on MATLAB code and the following references:
  ! SW_DENS0  $Revision: 1.3 $  $Date: 1994/10/10 04:54:09 $
  !           Copyright (C) CSIRO, Phil Morgan 1992
  !
  ! INPUT:  (all must have same dimensions)
  !   S = salinity    [psu      (PSS-78)]
  !   T = temperature [degree C (IPTS-68)]
  !
  ! OUTPUT:
  !   dens0 = density  [kg/m^3] of salt water with properties S,T,
  !           P=0 (0 db gauge pressure)
  ! 
  ! AUTHOR:  Phil Morgan 92-11-05  (morgan@ml.csiro.au)
  !
  ! DISCLAIMER:
  !   This software is provided "as is" without warranty of any kind.  
  !   See the file sw_copy.m for conditions of use and licence.
  !
  ! REFERENCES:
  !     Unesco 1983. Algorithms for computation of fundamental properties of 
  !     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
  !
  !     Millero, F.J. and  Poisson, A.
  !     International one-atmosphere equation of state of seawater.
  !     Deep-Sea Res. 1981. Vol28A(6) pp625-629.
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: S    ! Salinity (psu)
    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL :: RES
    REAL :: sw_smow
    
    ! Locals from UNESCO 1983 eqn(13) p17.
    REAL, PARAMETER:: B0 =  8.24493E-1
    REAL, PARAMETER:: B1 = -4.0899E-3
    REAL, PARAMETER:: B2 =  7.6438E-5
    REAL, PARAMETER:: B3 = -8.2467E-7
    REAL, PARAMETER:: B4 =  5.3875E-9
   
    REAL, PARAMETER:: C0 = -5.72466E-3
    REAL, PARAMETER:: C1 = +1.0227E-4
    REAL, PARAMETER:: C2 = -1.6546E-6
    
    REAL, PARAMETER:: D0 =  4.8314E-4

    RES = sw_smow( T ) +                                                &   
    &     ( B0 + ( B1 + ( B2 + ( B3 + B4 * T ) * T ) * T ) * T ) * S +  &
    &     ( C0 + ( C1 + C2 * T ) * T ) * S * SQRT( S ) + D0 * S * S       
               
    RETURN
  END FUNCTION sw_dens0  
