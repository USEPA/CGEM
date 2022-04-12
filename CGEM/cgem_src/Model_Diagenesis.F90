Module Model_Diagenesis
      Use Sediment_Diagenesis_Routines

      IMPLICIT NONE
      SAVE

      REAL(kind=8) ::  tir_O2, tir_NO, tir_HN, tir_SO, tir_TC, tir_ALK, tir_DOM

      contains
      
      SUBROUTINE model(Ainp, dT, YY_ij, ppH_ij, sedflux_ij)

      USE SDM, ONLY: NPOINTS, NEQ, nsed,sO2,sNO3,sNH4,sDIC,sOM1,sOM2,sALK,sALK,sDOC

      IMPLICIT NONE
      
      INTEGER, PARAMETER :: MAXNEQ = 27000
      INTEGER, INTENT(IN) :: dT
      REAL(kind=4), INTENT(OUT) :: sedflux_ij(nsed)
      REAL(kind=8) :: MN2(1550), NH4(1500), NO3(1550)
      REAL(kind=8), INTENT(INOUT) :: YY_ij(NEQ)
      REAL(kind=8), INTENT(INOUT) :: ppH_ij(NPOINTS)
      REAL(kind=8) :: val1(400), val2(400)
      REAL(kind=8) :: G1(1550), G2(1550), O2(1550), Os(1500), Ob(1500)
      REAL(kind=8) :: FE2(1550), FE3(1550), SO4(1550), HS(1550)
      REAL(kind=8) :: FES(1550), TC(1550), ALK(1550), DOM(1550)
      REAL(kind=8) :: rootG1(400), rootG2(400), rootO2(400),rootNO3(400)
      REAL(kind=8) :: rootNH4(400), rootMN2(400), rootFE2(400), rootSO4(400)
      REAL(kind=8) :: rootHS(400), rootFES(400), rootTC(400), rootALK(400)
      REAL(kind=8) :: rootDOM(400), rootOs(400), rootOb(400)
      REAL(kind=8) :: Ainp(100)
      REAL(kind=8) :: sedO2(400), sedNO3(400), sedNH4(400)
      REAL(kind=8) :: sedMno(400), sedMN2(400), sedFe3(400), sedFe2(400)
      REAL(kind=8) :: sedSO4(400), sedHS(400), sedFeS(400), sedDIC(400)
      REAL(kind=8) :: sedALK(400), sedDOC(400), sedOM1(400), sedOM2(400)
      REAL(kind=8) :: pycoO2(400)

      REAL(kind=8), PARAMETER :: ZERO = 0.0D+00
      INTEGER :: n_its, nss, np, IC
      INTEGER :: IFLAG, ISTATE
      REAL(kind=8) ::  step_in, step

!
! --------    Notify user of input mode
!

!      WRITE(*,600)
 600  FORMAT(' ','DATA IS ENTERED WITH FILE hypox_inp.csv ')

!
! --------     Change some parameters to input variables
!

      nss = 17
      np = 1500

!
! --------     Set IFLAG for reading plume.dat
!

      IFLAG = 1  ! set to null Y vector

!
! --------     Run a series of CASES with ALtered inputs
!

!L3 Y starts off as read in from normoxia.dat, and is updated in CASES
!and FILL_Y

      temp = ZERO
!
      n_its = 1                        !Ainp(mrow+1)
      step_in = real(dT,8)/86400.D0    !From seconds to days !Ainp(mrow+2)

      step = step_in/365.D0            !From days to years

      DO IC = 1, n_its

!         write(6,*) "Step:years,days,secs",IC*step,IC*step_in,IC*dT
         val1(IC)= temp
         val2(IC)= temp + step
         temp    = val2(IC)
!         write(6,*) "before CASES",IC
      CALL CASES(val1(IC), val2(IC), YY_ij, ppH_ij, ISTATE, IFLAG, Ainp)

!         write(6,*) "after CASES",YY_ij(1),ppH_ij(1)

      CALL FILL_Y(NEQ, np, nss, YY_ij, G1, G2, O2, NO3, NH4, MN2, &
                  FE3, FE2, SO4, HS, FES, TC, ALK, DOM, Os, Ob)
!         write(6,*) "after fill_y",YY_ij(1)

!  SUM up the irrigation O2 flux (*1000)is puts it in
!  the same units as the sed flux.  The matlab routine
!  divides by 1000 TO get the needed units. ALL this is
!  done so we can USE the same fill_y and Rootint routines as
!  is used for the concentration stuff.


       tir_O2  = SUM(rIRRO2)
       tir_NO  = SUM(rIRRNO)
       tir_HN  = SUM(rIRRNH)
       tir_SO  = SUM(rIRRSO)
       tir_TC  = SUM(rIRRTC)
       tir_ALK = SUM(rIRRALK)
       tir_DOM = SUM(rIRRDOM)


       sedO2(IC)  = FLUXES(1) + tir_O2
       sedNO3(IC) = FLUXES(2) + tir_NO
       sedNH4(IC) = FLUXES(3) + tir_HN
       sedSO4(IC) = FLUXES(4) + tir_SO
       sedDIC(IC) = FLUXES(5) + tir_TC
       sedOM1(IC) = FLUXES(6)
       sedOM2(IC) = FLUXES(7)
       pycoO2(IC) = FLUXES(8)
       sedALK(IC) = FLUXES(9) + tir_ALK
       sedDOC(IC) = FLUXES(10) + tir_DOM

!       write(*,'(A6,2X,f12.2)') 'DOflux',sedO2(IC)/365*10   ! check Surface O2 concentrations
!
! ------- Output the sediment geochemical profiles to compare w/ data
!
!       Iflagout = mod(IC,10) ! Output full profile every 50 years
!       IF ((Iflagout == 0) .AND. (IC >= 20)) CALL MATLAB(NEQ,Y,IC)
!       CALL MATLAB(NEQ,Y,IC)

!       WRITE(*,260) ISTATE

       ENDDO  !End loop over 'ndays'


!      CALL OUTFLUX(sedO2,sedNO3,sedNH4,sedSO4,sedDIC,sedDOC,
!     * sedOM1,sedOM2, pycoO2,n_its)
      sedflux_ij = 0.
      IC = n_its
      sedflux_ij(sO2) = sedO2(IC)
      sedflux_ij(sNO3) = sedNO3(IC)
      sedflux_ij(sNH4) = sedNH4(IC)
      sedflux_ij(sDIC) = sedDIC(IC)
      sedflux_ij(sOM1) = sedOM1(IC)
      sedflux_ij(sOM2) = sedOM2(IC)
      sedflux_ij(sALK) = sedALK(IC)
      sedflux_ij(sDOC) = sedDOC(IC)
      sedflux_ij = sedflux_ij/365.
!      write(6,*) "fluxes",FLUXES
!      write(6,*) "tir",tir_O2,tir_ALK,tir_DOM
!      write(6,*) "sedO2(IC)years,days",IC,sO2,sedO2(IC),sedflux_ij(sO2)
!      write(6,*) "NO3,NH4,DIC",sedflux_ij(sNO3),sedflux_ij(sNH4),sedflux_ij(sDIC)
!       write(6,*) sedflux_ij

 260  FORMAT(///' POS # IS A GOOD THING, NEG # IS SUSPECT! ISTATE =',I3)
      END


END MODULE Model_Diagenesis
