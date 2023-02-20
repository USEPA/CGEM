!---------------------------------------------------------------------------
  SUBROUTINE Light_WQEM(PARsurf, S, A, Z, OM1A, OM1Z, OM1R, OM1BC, dz, PAR_percent, PARbot, PARdepth, nz  )   
!---------------------------------------------------------------------------

  USE Model_dim
  USE INPUT_VARS_CGEM, ONLY:Qc,ZQc
    
  !--------------------------------------------------------------------------
  ! INPUT:  
  !
  ! OUTPUT:
  !   PAR 
  ! 
  ! REFERENCES:
  !   WQEM model code
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN)  :: PARsurf      ! Irradiance just below sea surface
    REAL, INTENT(IN)  :: S(km)    ! Salinity 
    REAL, INTENT(IN)  :: A(nospA,km),Z(nospZ,km)    ! Phytoplankton, Zooplankton 
    REAL, INTENT(IN)  :: OM1A(km),OM1Z(km),OM1R(km),OM1BC(km) ! Particulate OM in gC/m3
    REAL, INTENT(IN)  :: dz(km) !dz = thickness
    REAL, INTENT(OUT) :: PAR_percent(km),PARbot,PARdepth(km) 
    REAL :: SAL_TERM, CHL_TERM, POC_TERM, KESS(km) 
    REAL :: IATTOP, OPTDEPTH, IATBOT(km)
    INTEGER :: k
    INTEGER, INTENT(IN) :: nz

      do k = 1, nz
         SAL_TERM = 1.084E-06 * (S(k)**4)

         CHL_TERM = 0.2085 * LOG( SUM(A(:,k)*Qc(:)*12.) ) 

         POC_TERM = 0.7640 * SQRT(                & 
                 &  OM1A(k)        +  &
                 &  OM1Z(k)        +  &
                 &  OM1R(k)        +  &
                 &  OM1BC(k)       +  &
                 &  SUM(Z(:,k) * ZQc(:) * 12.e3 )   &
                 & )  

         KESS(k) = ( ( -0.10 * (-0.5606 - SAL_TERM + CHL_TERM + POC_TERM) ) &
                  &  + 1 ) ** (1.0/(-0.10))
      enddo

      DO k = 1,1
         IATTOP    =  PARsurf 
         OPTDEPTH  =  KESS(k) * dz(k)
         IATBOT(k) =  IATTOP  * EXP(-OPTDEPTH)
         PARdepth(k)   =  (IATTOP - IATBOT(k)) / OPTDEPTH
         PAR_percent(k) = 100.*PARdepth(k)/PARsurf
      END DO
      if(nz>=2) then
      DO k = 2,nz
         IATTOP    =  IATBOT(k-1)
         OPTDEPTH  =  KESS(k) * dz(k)
         IATBOT(k) =  IATTOP * EXP(-OPTDEPTH)
         PARdepth(k) =  (IATTOP - IATBOT(k)) / OPTDEPTH
         PAR_percent(k) = 100.*PARdepth(k)/PARsurf
      END DO
      endif
         PARbot = PARdepth(nz)

    RETURN

  END SUBROUTINE Light_WQEM  
