      SUBROUTINE model(Ainp,ndays,sedflux,maxres,Y,ppH_init)

      USE Model_dim
      USE CGEM_vars 
      USE nparray

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 MN2(npts2),NH4(npts2),NO3(npts2)
      REAL, INTENT(OUT) :: sedflux(nf)
      REAL, INTENT(IN)  :: maxres  !Maximum residual
      REAL*8, INTENT(IN)  :: ppH_init(1500)
      INTEGER, INTENT(IN) :: ndays
      PARAMETER (MAXNEQ=27000)
      DIMENSION Y(MAXNEQ),val1(400),val2(400)
      DIMENSION G1(npts2),G2(npts2),O2(npts2),Os(npts),Ob(npts)
      DIMENSION FE2(npts2),FE3(npts),SO4(npts2),HS(npts2)
      DIMENSION FES(npts2),TC(npts2),ALK(npts2),DOM(npts2)
      DIMENSION Ainp(100)
      DIMENSION FLUXES(9),sedO2(400),sedNO3(400),sedNH4(400)
      DIMENSION sedMno(400),sedMN2(400),sedFe3(400),sedFe2(400)
      DIMENSION sedSO4(400),sedHS(400),sedFeS(400),sedDIC(400)
      DIMENSION sedALK(400),sedDOC(400),sedOM1(400),sedOM2(400)
      DIMENSION rIRRO2(npts), rIRRTC(npts)
      DIMENSION rIRRNO(npts), rIRRNH(npts), rIRRSO(npts)
      DIMENSION pycoO2(400)
      COMMON /FIRRG/ rIRRO2,rIRRTC,rIRRNO,rIRRNH,rIRRSO
      COMMON /GRIDPOINT/ NEQ
!      COMMON /IRRIG/ ALPHA0,XIRRIG
      COMMON /TIMES/ T0,TL
      COMMON /SPECIES/ NS,NPOINTS
      COMMON /SEDFLUX/ FLUXES
      COMMON /SEDIRR/ tir_O2,tir_NO,tir_HN,tir_SO,tir_TC

      DATA ZERO/0.0D+00/

      REAL,save :: sedO2_0,sedNO3_0,sedNH4_0 = 0.
      REAL,save :: sedDIC_0,sedOM1_0,sedOM2_0 = 0.
      REAL,save :: sedALK_0 = 0.
      REAL :: residual(7)
      integer,save :: init = 1
C
C --------     Change some parameters to input variables
C
      nss=17
      np = npts

C

C
C --------     Run a series of CASES with ALtered inputs
C
      !step= 730.0/365.0           ! Put in units of model (y)
      step = 1.d0/365.d0
 
      temp   = ZERO


      DO IC=1,ndays
         !write(6,*) "IC",IC
         val1(IC)= temp
         val2(IC)= temp + step
         temp    = val2(IC)
         date= temp*365.
         idate = mod(date,365.0d00)

!      write(6,*) "b ca pph_init",ppH_init(1:3)



      CALL CASES(val1(IC),val2(IC),Y,ISTATE,Ainp,ppH_init)


!      write(6,*) "b ca pph_init",ppH_init(1:3)

      CALL FILL_Y(NEQ,np,nss,Y,G1,G2,O2,NO3,NH4,MN2,
     *                  FE3,FE2,SO4,HS,FES,TC,ALK,DOM,Os,Ob)

!  SUM up the irrigation O2 flux (*1000)is puts it in
!  the same units as the sed flux.  The matlab routine
!  divides by 1000 TO get the needed units. ALL this is
!  done so we can USE the same fill_y and Rootint routines as
!  is used for the concentration stuff.
       tir_O2=SUM(rIRRO2)
       tir_NO=SUM(rIRRNO)
       tir_HN=SUM(rIRRNH)
       tir_SO=SUM(rIRRSO)
       tir_TC=SUM(rIRRTC)

       sedO2(IC) = FLUXES(1) + tir_O2
       sedNO3(IC)= FLUXES(2) + tir_NO
       sedNH4(IC)= FLUXES(3) + tir_HN
       sedSO4(IC)= FLUXES(4) + tir_SO
       sedDIC(IC)= FLUXES(5) + tir_TC
       sedOM1(IC)= FLUXES(6)
       sedOM2(IC)= FLUXES(7)
       pycoO2(IC)= FLUXES(8)
       sedALK(IC)= FLUXES(9)

       residual(1) =   ABS((sedO2(IC) - sedO2_0)/sedO2(IC))
       residual(2) =   ABS((sedNO3(IC) - sedNO3_0)/sedNO3(IC))
       residual(3) =   ABS((sedNH4(IC) - sedNH4_0)/sedNH4(IC))
       residual(4) =   ABS((sedDIC(IC) - sedDIC_0)/sedDIC(IC))
       residual(5) = 0.!  ABS((sedOM1(IC) - sedOM1_0)/sedOM1(IC))
       residual(6) = 0.!  ABS((sedOM2(IC) - sedOM2_0)/sedOM2(IC))
       residual(7) = 0.!  ABS((sedALK(IC) - sedALK_0)/sedALK(IC))

       sedO2_0  = sedO2(IC)
       sedNO3_0 = sedNO3(IC)
       sedNH4_0 = sedNH4(IC)
       sedDIC_0 = sedDIC(IC)
       sedOM1_0 = sedOM1(IC)
       sedOM2_0 = sedOM2(IC)
       sedALK_0 = sedALK(IC)


!
! ------- Output the sediment geochemical profiles to compare w/ data
!
       !Iflagout = mod(IC,10) ! Output full profile every 50 years
       !IF ((Iflagout == 0) .AND. (IC >= 20)) CALL MATLAB(NEQ,Y,IC)
!       WRITE(*,260) ISTATE

       if(maxval(residual).le.maxres.or.IC.eq.ndays) then
        EXIT 
       endif

      ENDDO  !ENDDO over ndays


 260  FORMAT(///' POS # IS A GOOD THING, NEG # IS SUSPECT! ISTATE =',I3)
      sedflux(iO2) = sedO2(IC)
      sedflux(iNO3) = sedNO3(IC)
      sedflux(iNH4) = sedNH4(IC)
      sedflux(iDIC) = sedDIC(IC)
      sedflux(iOM1_bc) = sedOM1(IC)
      sedflux(iOM2_bc) = sedOM2(IC)
      sedflux(iALK) = sedALK(IC)
      sedflux = sedflux/365.


      END

