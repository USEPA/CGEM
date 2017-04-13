!---------------------------------------------------------------------------
  SUBROUTINE Light_GoMDOM(PARsurf, S, A, Z, OM1A, OM1Z, OM1R, OM1BC, dz, PAR_percent, PARbot, PARdepth, nz  )   
!---------------------------------------------------------------------------

  USE Model_dim
  USE INPUT_VARS, ONLY:Qc,ZQc
    
  !--------------------------------------------------------------------------
  ! INPUT:  
  !
  ! OUTPUT:
  !   PAR 
  ! 
  ! REFERENCES:
  !   GoMDOM model code
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN)  :: PARsurf      ! Irradiance just below sea surface
    REAL, INTENT(IN)  :: S(nsl)    ! Salinity 
    REAL, INTENT(IN)  :: A(nospA,nsl),Z(nospZ,nsl)    ! Phytoplankton, Zooplankton 
    REAL, INTENT(IN)  :: OM1A(nsl),OM1Z(nsl),OM1R(nsl),OM1BC(nsl) ! Particulate OM in gC/m3
    REAL, INTENT(IN)  :: dz(nsl) !dz = thickness
    REAL, INTENT(OUT) :: PAR_percent(nsl),PARbot,PARdepth(nsl) 
    REAL :: SAL_TERM, CHL_TERM, POC_TERM, KESS(nsl) 
    REAL :: IATTOP, OPTDEPTH, IATBOT(nsl)
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

  END SUBROUTINE Light_GoMDOM  
