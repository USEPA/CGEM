       Subroutine Flux_GD(TC_8, istep, myid, numprocs)

       USE Model_dim
       USE Grid
       USE Hydro
       USE State_Vars
       USE Which_Flux
       USE STATES
       USE INPUT_VARS
       USE INPUT_VARS_GD
       USE InRemin
       USE MOD_UTILITIES

       IMPLICIT NONE

       integer(kind=8), intent(in) :: TC_8 ! Current time in seconds since Model_dim::iYr0.
       integer, intent(in) :: istep, myid, numprocs
       REAL :: T_sfc, Sal_sfc, O2_sfc, Sc, Op_umole, rhow, Op, OsDOp
       REAL :: Vtrans, alpha_O2, O2_atF, zs, DIC_sfc, CO2_atF
       REAL :: NO3_Ex, NH4_Ex, PO4_Ex
       REAL :: NO3_CMAQ(im,jm),NH4_CMAQ(im,jm)
       REAL :: Si_Ex(im,jm,2) !1==SA 2==SRP
       INTEGER :: i, j, mpierr
       INTEGER, SAVE :: init=1
       REAL, PARAMETER :: SDay = 86400.0  ! # of sec in 24 hr day
       !Esed is quanta/cm2/s
       !Need mol photons/m2/d, N_Av=6.0221413E+23
       !quanta/cm2/s * 1 mol/N_av quanta * 10,000cm2/m2 * 86400s/d = mol/m2/d
       ! 1e-23 * 1e4 * 1e4 = 1e-15
       REAL, PARAMETER :: cnvt_O2 = 32.e-6
       REAL, PARAMETER :: cnvt_DIC = 12.e-6
       REAL, PARAMETER :: cnvt_N = 14.e-6
       REAL, PARAMETER :: cnvt_P = 31.e-6
       REAL, PARAMETER :: pH = 8.25
       REAL, PARAMETER :: pco2 = 395

       INTEGER nz !layers
       integer myi

! -- SURFACE FLUXES -------------------------------------------------------------

if(init.eq.1) then

  if(Which_Fluxes(iCMAQ).eq.1) then !CMAQ
      call Read_CMAQ_NH4_SVflux_bin(TC_8,NH4_CMAQ)
      call Read_CMAQ_NO3_SVflux_bin(TC_8,NO3_CMAQ)
      if(numprocs .gt. 1) call MPI_BCAST(NO3_CMAQ, im*jm, MPI_real, 0, MPI_COMM_WORLD, mpierr)
      if(numprocs .gt. 1) call MPI_BCAST(NH4_CMAQ, im*jm, MPI_real, 0, MPI_COMM_WORLD, mpierr)
      ! write(6,*) "Which_Fluxes(iCMAQ) not supported, exiting"
      ! stop
  endif

  if(Which_Fluxes(i_Si).eq.1) then !Fluxes for SA and SRP in kg/s
       write(6,*) "Which_Fluxes(i_Si) not supported, exiting"
       stop 
  endif

  init=0

endif



! -- Loop over i,j; k will be 1 (surface)
         do j = 1,jm
         myi = 1
         do i = myi_start,myi_end
             if(nza(i,j).gt.0) then


if(Which_Fluxes(iO2surf).eq.1) then
!--------------------------------------------------------------
! Calc  O2_atF, the sea surface vertical flux of O2
!--------------------------------------------------------------
               T_sfc    = T(i,j,1)     ! temp(deg C)      in sfc layer, k=1
               Sal_sfc  = S(i,j,1)     ! sal (psu)        in sfc layer, k=1
               O2_sfc   = f(myi,j,1,JDO2)/cnvt_O2 ! O2 (mmolE-O2/m3) in sfc layer, k=1

               Sc       = SchmidtNumber(Sal_sfc,T_sfc,0)  ! Schmidt number,
                                                          !   0 (zero) for O2

               Op_umole = o2sat(Sal_sfc,T_sfc)     ! O2 saturation,
                                                   !    (umolE-O2/kg)

               rhow     = sw_dens0(Sal_sfc,T_sfc)  ! water density [kg/m3]

               Op       = rhow * Op_umole * 1.0E-3 ! O2 saturation,
                                                   !    (mmolE-O2/m3)
               OsDOp    = O2_sfc/Op

            !--------------------------------------------------------------
            !  Vtrans below is the O2 transfer vel (m/s)
            !
            !  Vtrans   = (5.9*(kw)*(OsDOp*OsDOp))*(Sc)**X
            !    where kw and Sc are dependent on Wind Speed.
            !  Values kw and X are from Liss and Merlivat, 1986.
            !  Factor of OsDOp**2 is from Justic, et. al 2002 
            !   for when saturation levels are above 125%.
            !---------------------------------------------------------------
             if(Wind(i,j).lt.3.6) then
               Vtrans        = AMAX1((5.9 * (0.17*Wind(i,j))         &
               &              * Sc**(-2./3.) / SDay), 0.)
             else if(Wind(i,j).le.13.) then
               Vtrans        = AMAX1((5.9 *(2.85*Wind(i,j) - 9.65 )    &
               &              / SQRT(Sc) / SDay), 0.)
             else
               Vtrans        = AMAX1((5.9 *(5.9*Wind(i,j) - 49.3 )    &
               &              / SQRT(Sc) / SDay), 0.)
             endif
             if(OsDOp.gt.1.25) Vtrans = Vtrans * (OsDOp*OsDOp)

               alpha_O2       = 1.025

               O2_atF         = Vtrans*(O2_sfc - alpha_O2*Op)
                                                       ! flux of O2 thru the
                                                       ! sea sfc
                                                       ! ((mmolE-O2/m2/sec)
                                                       ! negative means into
               O2_atF = O2_atF*cnvt_O2
               f(myi,j,1,JDO2) = AMAX1(f(myi,j,1,JDO2) - O2_atF/dz(i,j,1)*dT,0.)
endif 


if(Which_Fluxes(iDICsurf).eq.1) then
  write(6,*) "DO NOT USE DIC surface flux in GoMDOM, no CO2 flux"
  stop
endif

if (Which_Fluxes(iCMAQ) .eq. 1) then !CMAQ
!!NO3 Exchange
      NO3_CMAQ(i,j) = NO3_CMAQ(i,j) * 14.e-6
      f(myi,j,1,JNO3) = AMAX1(f(myi,j,1,JNO3) - NO3_CMAQ(i,j)/ dz(i,j,1)*dT, 0.)
!!NH4 Exchange
      NH4_CMAQ(i,j) = NH4_CMAQ(i,j) * 14.e-6
      f(myi,j,1,JNH4) = AMAX1(f(myi,j,1,JNH4) - NH4_CMAQ(i,j)/ dz(i,j,1)*dT, 0.)
!!           write(6,*) "Which_Fluxes(iCMAQ) is not implemented yet, stopping."
!!           stop
endif

   endif !End of if(nza(i,j) statement
   myi = myi + 1
   END DO      ! end of do i block do loop
   END DO      ! end of do j block do loop

!-- BOTTOM FLUXES -------------------------------------------------------------------------
         do j = 1,jm
         myi = 1
         do i = myi_start,myi_end
                nz = nza(i,j)
             if(nz.gt.0) then  !water cell
              if(wsm(i,j).eq.0) then !If we are on the shelf

if(Which_Fluxes(iSOC).eq.1) then
!Murrell and Lehrter sediment oxygen consumption
               f(myi,j,nz,JDO2) = AMAX1(f(myi,j,nz,JDO2) - 0.0235*2.**(.1*T(i,j,nz))*  &
     & f(myi,j,nz,JDO2)/dz(i,j,nz)*dT/SDay,0.)
endif

if(Which_Fluxes(iNutEx).eq.1) then
!NO3 Exchange
       NO3_Ex = 0.0057*f(myi,j,nz,JDO2)/cnvt_O2 - 0.52
               f(myi,j,nz,JNO3) = AMAX1(f(myi,j,nz,JNO3) + cnvt_N*NO3_Ex/ &
     & dz(i,j,nz)*dT/SDay,0.)

!NH4 Exchange
       NH4_Ex = -1.55*NO3_Ex + 0.69
               f(myi,j,nz,JNH4) = AMAX1(f(myi,j,nz,JNH4) + cnvt_N*NH4_Ex/ &
     & dz(i,j,nz)*dT/SDay,0.)

!PO4 Exchange
      PO4_Ex = 0.094*NH4_Ex - 0.0125
               f(myi,j,nz,JSRP) = AMAX1(f(myi,j,nz,JSRP) + cnvt_P*PO4_Ex/ &
     & dz(i,j,nz)*dT/SDay,0.)
endif

if(Which_Fluxes(iInRemin).eq.1) then
      f(myi,j,nz,JDO2) = AMAX1(f(myi,j,nz,JDO2) - TSOD(i,j)*dT,0.)
      f(myi,j,nz,JNO3) = AMAX1(f(myi,j,nz,JNO3) + SED_NO3_RATE(i,j)*dT,0.)
      f(myi,j,nz,JNH4) = AMAX1(f(myi,j,nz,JNH4) + SED_NH3_RATE(i,j)*dT,0.)
endif


if(Which_Fluxes(iMPB).eq.1) then
!MPB O2 Production
           write(6,*) "Which_Fluxes(iMPB) is not implemented yet, stopping."
           stop
endif

if(Which_Fluxes(iSDM).eq.1) then
!Sediment Diagenesis Model
           write(6,*) "Which_Fluxes(iSDM) is not implemented yet, stopping."
           stop
endif

if(Which_Fluxes(i_Si).eq.1) then
      f(myi,j,nz,JSA)  = AMAX1(f(myi,j,nz,JSA)  + Si_Ex(i,j,1)/   &
        Vol(i,j,nz)*dT,0.)
      f(myi,j,nz,JSRP) = AMAX1(f(myi,j,nz,JSRP) + Si_Ex(i,j,2)/   &
        Vol(i,j,nz)*dT,0.)
endif

   endif !end shelf
   endif !End of if(nza(i,j) statement
   myi = myi+1
   END DO      ! end of do i block do loop
   END DO      ! end of do j block do loop


       RETURN

       END Subroutine Flux_GD
