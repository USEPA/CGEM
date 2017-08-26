      Subroutine Flux_CGEM(istep)

      USE Model_dim
      USE Grid
      USE Hydro
      USE State_Vars
      USE CGEM_Flux
      USE INPUT_VARS
      USE INPUT_VARS_CGEM
      USE CGEM_vars 
      USE Which_Flux
      USE gasx
      USE STOICH_VARS
      USE SDM, ONLY: sedflux
      USE MOD_UTILITIES

      IMPLICIT NONE


      integer, intent(in) :: istep
      real :: T_sfc, Sal_sfc, O2_sfc, Sc, Op_umole, rhow, Op, OsDOp
      real :: Vtrans, alpha_O2, O2_atF, zs, DIC_sfc, CO2_atF
      real :: SOC, O2Flux, NO3Flux, NH4Flux, PO4Flux, SiFlux, DICFlux, ALKFlux
      integer :: i, j
      integer, save :: init=1
      real, parameter :: SDay = 86400.0  ! # of sec in 24 hr day
      !Esed is quanta/cm2/s
      !Need mol photons/m2/d, N_Av=6.0221413E+23
      !quanta/cm2/s * 1 mol/N_av quanta * 10,000cm2/m2 * 86400s/d = mol/m2/d
      ! 1e-23 * 1e4 * 1e4 = 1e-15
      real, parameter :: convert = 1. / 6.0221413 * 8.64 * 1.e-15
      real*8, dimension(100), save :: A
      integer, parameter :: mrow=64
!------------------------------------------------------------------
!Output vars for mocsy subroutine:
      real :: kw660(1), co2flux(1), co2ex(1), dpco2(1)
      real :: ph_calc(1), pco2_calc(1), fco2(1), co2(1), hco3(1), co3(1), omegaa(1) 
      real :: omegac(1), betad_calc(1), rhosw(1), p(1), tempis(1)
      real :: patm(1) = 1., pco2_in(1)
      real :: m_alk(1), m_dic(1), m_si(1), m_po4(1)

!Justic and Wang
      real :: tau(im,jm) !Set Shear to zero in FishTank
!SDM
      integer :: NPOINTS, NEQ
      double precision, save :: ppH_init(2000)
      double precision, save :: Y(27000)
!Layers
      integer nz
!temp.
      real sedflux_iOM1_bc

! -- Read in "A" for SDM --------------------------------------
      if(Which_fluxes(iSDM).eq.1.and.init.eq.1) then
        ! Reads in SDM/hypox_input.csv
         call datain(A,mrow)
         NPOINTS = A(44)
         NEQ = NPOINTS*17 + 2
         OPEN(11,STATUS='UNKNOWN', file='SDM/ph2bprofile.dat')
         READ(11,*) (ppH_init(i),i=1,NPOINTS)
         CLOSE(11)

         OPEN(11,STATUS='UNKNOWN', file='SDM/normoxia.dat')
         READ(11,*) (Y(i),i=1,NEQ)
         CLOSE(11)
      endif

! -- Read in CMAQ -------------------------------------------------------------
if(Which_fluxes(iCMAQ).eq.1) then !CMAQ
   write(6,*) "CMAQ option not supported, will run without CMAQ flux"
endif


! -- SURFACE FLUXES -------------------------------------------------------------
! -- Loop over i,j; k will be 1 (surface)
         do j = 1,jm
         do i = 1,im
             if(nza(i,j).gt.0) then 

if(Which_fluxes(iO2surf).eq.1) then
!--------------------------------------------------------------
! Calc  O2_atF, the sea surface vertical flux of O2
!--------------------------------------------------------------
               T_sfc    = T(i,j,1)       ! Temperature (C)   in sfc layer, k=1
               Sal_sfc  = S(i,j,1)       ! Salinity          in sfc layer, k=1
               O2_sfc   = f(i,j,1,iO2) ! O2 (mmol-O2/m3) in sfc layer, k=1

               Sc       = SchmidtNumber(Sal_sfc,T_sfc,0)  ! Schmidt number,
                                                          !   0 (zero) for O2

               Op_umole = o2sat(Sal_sfc,T_sfc)     ! O2 saturation,
                                                   !    (umol-O2/kg)

               rhow     = sw_dens0(Sal_sfc,T_sfc)  ! water density [kg/m3]

               Op       = rhow * Op_umole * 1.0E-3 ! O2 saturation,
                                                   !    (mmol-O2/m3)
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
                                                       ! ((mmol-O2/m2/sec)
                                                       ! negative means into
               f(i,j,1,iO2) = AMAX1(f(i,j,1,iO2) - O2_atF/dz(i,j,1)*dT,0.)
endif 


if(Which_fluxes(iDICsurf).eq.1) then
!--------------------------------------------------------------
! Calc  SFLUX_CO2, the sea surface vertical flux of CO2
!--------------------------------------------------------------
               zs      = dz(i,j,1)       ! Thickness (m.) of the water column

               T_sfc   = T(i,j,1)        ! Temperature (C) in sfc layer, k=1
               Sal_sfc = S(i,j,1)        ! Salinity        in sfc layer, k=1
               DIC_sfc = f(i,j,1,iDIC) ! Dissolved Inorganic Carbon
                                         !    (mmol m-3) in sfc layer, k=1

             !----------------------------------------------------------
             ! Units of gas_exchange are mmol CO2 m-2 s-1 
             !----------------------------------------------------------
!use mocsy instead but calculate to compare
             CO2_atF = gas_exchange(T_sfc,Sal_sfc,DIC_sfc,zs,pH(i,j,1),pCO2)
                          f(i,j,1,iDIC) = AMAX1(f(i,j,1,iDIC) - CO2_atF/dz(i,j,1)*dT,0.)

elseif(Which_fluxes(iDICsurf).eq.2) then
!---------------using mocsy:------------------------------------
  kw660(1) = 0.01/3600.*0.31*Wind(i,j)*Wind(i,j) !*schmidtnumberterm
  pCO2_in(1) = pCO2
            !!! MOCSY alkalinity expressions:
            m_alk = f(i,j,1,iALK)/1000.
            m_dic = f(i,j,1,iDIC)/1000.
            m_si  = f(i,j,1,iSi)/1000.
            m_po4 = f(i,j,1,iPO4)/1000.

  call flxco2(co2flux, co2ex, dpco2, &
 &            ph_calc, pco2_calc, fco2, co2, hco3, co3, omegaa, omegac, betad_calc, rhosw, p, tempis,  &
 &            T(i,j,1), S(i,j,1), m_alk, m_dic, m_si, m_po4, kw660, pCO2_in, patm, zs, 1, &
 &            'mol/m3', 'Tinsitu', 'm ', 'u74', 'l  ', 'pf ', 'Pzero  ')

              f(i,j,1,iDIC) = AMAX1(f(i,j,1,iDIC) + 1000.*co2flux(1)/dz(i,j,1)*dT,0.)

endif


   endif !End of if(nza(i,j) statement
   END DO      ! end of do i block do loop
   END DO      ! end of do j block do loop


!-- BOTTOM FLUXES -------------------------------------------------------------------------
         do j = 1,jm
         do i = 1,im
              if(nza(i,j).gt.0) then
              if(wsm(i,j).eq.0) then !If we are on the shelf
               nz = nza(i,j)
if(Which_fluxes(iSOC).eq.1) then
!Murrell and Lehrter sediment oxygen consumption
       SOC = - 0.0235*2.**(.1*T(i,j,nz))*f(i,j,nz,iO2)
               f(i,j,nz,iO2) = AMAX1(f(i,j,nz,iO2)  + SOC/  &
     & dz(i,j,nz)*dT/SDay,0.)
       DICFlux = (-3.7*log(AMAX1(f(i,j,nz,iO2),1.e-8)) + 19.4)*SOC
               f(i,j,nz,iDIC) = AMAX1(f(i,j,nz,iDIC) + DICFlux/  &
     & dz(i,j,nz)*dT/SDay,0.)
elseif(Which_fluxes(iSOC).eq.2.or.Which_fluxes(iSOC).eq.3) then
!Justic and Wang sediment oxygen consumption
     tau=0.
     call JW_SOC(O2Flux,NH4Flux,PO4Flux,CBODW(i,j),f(i,j,nz,iA(1):iA(nospA)),Esed(i,j),f(i,j,nz,iO2),T(i,j,nz),tau(i,j),i,j)
!O2
               f(i,j,nz,iO2) = AMAX1(f(i,j,nz,iO2)    + O2Flux/  &
     & dz(i,j,nz)*dT/SDay,0.)
!NH4
               f(i,j,nz,iNH4) = AMAX1(f(i,j,nz,iNH4)  + NH4Flux/  &
     & dz(i,j,nz)*dT/SDay,0.)
!PO4
               f(i,j,nz,iPO4) = AMAX1(f(i,j,nz,iPO4)  + PO4Flux/  &
     & dz(i,j,nz)*dT/SDay,0.)
elseif(Which_fluxes(iSOC).eq.4) then
!Meta Model
     call Meta_SOC(f(i,j,nz,:),T(i,j,nz),S(i,j,nz),dz(i,j,nz),s_x1A(i,j,nz),s_y1A(i,j,nz),s_x1Z(i,j,nz),s_y1Z(i,j,nz))
endif


if(Which_fluxes(iNutEx).eq.1) then
!NO3 Exchange
       NO3Flux = 0.0057*f(i,j,nz,iO2) - 0.52
               f(i,j,nz,iNO3) = AMAX1(f(i,j,nz,iNO3) + NO3Flux/ &
     & dz(i,j,nz)*dT/SDay,0.)

!NH4 Exchange
       NH4Flux = -1.55*NO3Flux + 0.69
               f(i,j,nz,iNH4) = AMAX1(f(i,j,nz,iNH4) + NH4Flux/ &
     & dz(i,j,nz)*dT/SDay,0.)

!PO4 Exchange
      PO4Flux = 0.094*NH4Flux - 0.0125
               f(i,j,nz,iPO4) = AMAX1(f(i,j,nz,iPO4) + PO4Flux/ &
     & dz(i,j,nz)*dT/SDay,0.)

!Si Exchange
      SiFlux = 1.68 
               f(i,j,nz,iSi)  = AMAX1(f(i,j,nz,iSi)  + SiFlux/ &
     & dz(i,j,nz)*dT/SDay,0.)

!ALK Exchange
      ALKFlux = NO3Flux - NH4Flux + PO4Flux
               f(i,j,nz,iALK)  = AMAX1(f(i,j,nz,iALK)  + ALKFlux/ &
     & dz(i,j,nz)*dT/SDay,0.)
endif


!MPB O2 Production
if(Which_fluxes(iMPB).eq.1) then
! Gatusso et al. 2006
               f(i,j,nz,iO2) = f(i,j,nz,iO2) + 120.82*(1.-exp(-convert*Esed(i,j)/2.09))/ &
     & dz(i,j,nz)*dT/SDay
elseif(Which_fluxes(iMPB).eq.2) then
! Jahnke et al. 2008
               f(i,j,nz,iO2) = f(i,j,nz,iO2) + 132./12.*convert*Esed(i,j)**(1.45)/ &
     & dz(i,j,nz)*dT/SDay
elseif(Which_fluxes(iMPB).eq.3) then
! Lehrter et al. (2014)
               f(i,j,nz,iO2) = f(i,j,nz,iO2) + 0.33*convert*Esed(i,j)**(2.93)/ &
     & dz(i,j,nz)*dT/SDay
endif


if(Which_Fluxes(iSDM).eq.1) then

!Sediment Diagenesis Model
        if(init.eq.1.or.mod(istep,288).eq.0) then  !Call every day, 288 timestep, assumes timestep = 5 min
           call Sediment_Diagenesis_Flux(A,f(i,j,nz,:),T(i,j,nz),S(i,j,nz),pH(i,j,nz),sedflux(i,j,:),i,j,Y,ppH_init)
           !write(6,*) ppH_init(1:3)
           !write(6,*) sedflux(i,j,iO2),sedflux(i,j,iOM1_BC),sedflux(i,j,iNO3),sedflux(i,j,iNH4)
        endif


!DIC Exchange
               f(i,j,nz,iDIC) = AMAX1(f(i,j,nz,iDIC) + sedflux(i,j,iDIC)/ &
     & dz(i,j,nz)*dT/SDay,0.)

!NH4 Exchange
               f(i,j,nz,iNH4) = AMAX1(f(i,j,nz,iNH4) + sedflux(i,j,iNH4)/ &
     & dz(i,j,nz)*dT/SDay,0.)
!NO3 Exchange
               f(i,j,nz,iNO3) = AMAX1(f(i,j,nz,iNO3) + sedflux(i,j,iNO3)/ &
     & dz(i,j,nz)*dT/SDay,0.)
!O2 Exchange
               f(i,j,nz,iO2) = AMAX1(f(i,j,nz,iO2) + sedflux(i,j,iO2)/ &
     & dz(i,j,nz)*dT/SDay,0.)
!OM1 Exchange
                sedflux_iOM1_bc = sedflux(i,j,iOM1_bc)/dz(i,j,nz)*dT/SDay
                !!write(6,*) "f,sf",f(i,j,nz,iOM1_bc),sedflux_iOM1_bc
               f(i,j,nz,iOM1_bc) = AMAX1(f(i,j,nz,iOM1_bc)+sedflux_iOM1_bc,0.)
                !write(6,*) "after f",f(i,j,nz,iOM1_bc)
!OM2 Exchange
               f(i,j,nz,iOM2_bc) = AMAX1(f(i,j,nz,iOM2_bc) + sedflux(i,j,iOM2_bc)/ &
     & dz(i,j,nz)*dT/SDay,0.)
!ALK Exchange
               f(i,j,nz,iALK) = AMAX1(f(i,j,nz,iALK) + sedflux(i,j,iALK)/ &
     & dz(i,j,nz)*dT/SDay,0.)
endif


   endif !end shelf
   endif !End of if(nza(i,j) statement
   END DO      ! end of do i block do loop
   END DO      ! end of do j block do loop

   init=0

       RETURN

       END Subroutine Flux_CGEM
