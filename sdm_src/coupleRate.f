!     Last change:  PME  17 Jan 2008    6:30 pm
!
!   PROGRAMMED by Dr. PETER M. ELDRIDGE, The initial archtecture of this
!   program was developed by Dr. Bernard P. Boudreau for OM and O2.
!   The program has been revised to include the other constituents below
!   variable porosity, and various input option. The reaction scheme
!   was revised from Dr. P. Vancapellen with help from Dr. John Morse.
!   CO2 system is from Whitfield and Turner.
!
!   New formulation from CANDI (Boudreau, 1997) for Tortuousty
!   relating it to porosity is now included in this model. To reduce
!   the number of free parameters in the model, we are assuming
!   'steady state' compaction.
! 
!
!	Solves  DIAGENETIC Model with a finite 
!	differencing using the depth variable x.
!       includes:
!            OM1    - organic matter 1
!            OM2    - organic matter 2
!            DOM    - dissolved organic matter
!            O2     - oxygen
!            NO3-   - nitrate
!            NH3    - ammonia
!            MnO2   - Manganese solid
!            Mn+2   - Manganese porewater
!            Fe+3   - ferric
!            Fe+2   - ferrous
!            SO4-2  - sulfate
!            HS-    - sulfide
!            FeS    - pyrite
!            TC     - Total inorganoc carbon
!            ALK    - Carbonate alkalinity
!
!	- irrigation can occur and is modelled as a nonlocal
!	  sink/source as per Boudreau (1984, JMR)
!	- transport processes for solutes are molecular diffusion, 
!	  bioturbation, advection (burial) and irrigation
!	- transport processes for solids are bioturbation and burial
!
!	UNITS
!
!		- LENGTHS in CM
!		- TIME in YEARS 
!		- CONC. of porewater in Micro-MOLES/CC of porewater
!		- CONC. of Organic Matter in % OF SOLIDS
!		- FLUX of Organic Matter in Micro-Mole/(cm^2)/yr


!
!-----------------------------------------------------------------------
!  SUBROUTINE CASES: provides initial conditions from a steady state
!  model than at each designated output time provides a new input vector
!-----------------------------------------------------------------------
!
!

      SUBROUTINE CASES(value1,value2,Y,ISTATE,IFLAG,Ainp,myid,numprocs,   &
     & YY_init,ppH_init)
      IMPLICIT REAL*8 (A-H,O-Z)
      EXTERNAL FEX2,JEX
      INTEGER IWORK,LRW,LIW,MU,ML,ITASK,ISTATE,IOPT,ITOL,IPAR
      INTEGER myid, numprocs
      PARAMETER (MAXNEQ=27000)
      PARAMETER (NSPECIES=17)
      PARAMETER (NCOMPART=17)
      PARAMETER (MXSTEP =30000)
      PARAMETER (MU=NCOMPART,ML=MU)
      PARAMETER (LRW=22+11*MAXNEQ+(3*ML+2*MU)*MAXNEQ)
C     PARAMETER (LRW=22+9*MAXNEQ+2*MAXNEQ**2)
      PARAMETER (LIW=30+MAXNEQ)
      DIMENSION Y(MAXNEQ),RWORK(LRW),IWORK(LIW),RPAR(MAXNEQ)
      DIMENSION YY(MAXNEQ),pH(2000),ppH(2000)
      DIMENSION Ainp(100)
      COMMON /SPECIES/ NS,NPOINTS
      COMMON /SROOT/ ZROOT, WROOT
      COMMON /TEMPERATURE/ TEMP, SAL, PRESS,pH0
      COMMON /GETY/ YY
      COMMON /pHes/ pH
      COMMON /GRIDPOINT/ NEQ
      COMMON /GETPH/ ppH


      NS = NSPECIES
      ITOL = 1
      RTOL = 1.0D-03
      ATOL = 1.0D-08
      ITASK = 1
      ISTATE = 1
      IOPT = 1
      IWORK(1) = ML
      IWORK(2) = MU
      MF = 25
      IWORK(6) = MXSTEP

      IF (IFLAG .EQ. 1) THEN
        CALL FILEDATA2(RPAR,Ainp)
        DO I=1, NEQ
           Y(I)= YY(I)
        ENDDO
        IFLAG = 0
      ENDIF


!
! -------- provide new T and tout
!

      T= value1
      TOUT= value2

!
! --------  Provide pH profile 
!

        DO I=1,NPOINTS
           pH(I) = ppH(I)
        ENDDO

!----- Get DATA forthe water-column model


      CALL DVODE(FEX2,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,
     #    ISTATE,IOPT,RWORK,LRW,IWORK,LIW,JEX,MF,RPAR,IPAR)


      RETURN
      END

!
!  FEX2	CONTAINS THE EQUATIONS TO BE SOLVED AT EACH POINT
!		FOR THE "PRESCRIBED FLUX" BOUNDARY OM1 AND OM2
!		CONDITION (X=0) FOR THE ORGANIC MATTER TYPES
      

      SUBROUTINE FEX2(NEQ,T,Y,YDOT,RPAR,IPAR)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 NO30,NH30,IRRIG,NH3I,MnO0,Mn20,O20,KANH4, KADS_p 
      PARAMETER (MAXNEQ=27000)
      DIMENSION Y(NEQ),YDOT(NEQ),RATE(MAXNEQ)
      DIMENSION POROVEC(1500),rIRRO2(1500),rIRRTC(1500)
      DIMENSION rIRRNO(1500),rIRRNH(1500),rIRRSO(1500)
      DIMENSION RPAR(*)
      DIMENSION Rc(25)
      DIMENSION pH(2000),dy(2),FLUXES(9)

      COMMON /SPECIES/ NS,NPOINTS
      COMMON /DEPTH/ XL,DH
      COMMON /TEMPERATURE/ TEMP, SAL, PRESS, pH0
      COMMON /MIX/ X1,X2
      COMMON /BIOT/ Db0
      COMMON /ADVEC/ W00
      COMMON /CONC/ DO20,NO30,NH30,SO40,HS0,NH3I
      COMMON /DESORB/ KANH4
      COMMON /CONC2/ MNO0,MN20,FE30,FE20,FES0,TC0,ALK0,DOM0,DOMI
      COMMON /DIFF/ DO2,DNO3,DNH3,DMN2,DFE2,DSO4,DHS,DTC,DALK
      COMMON /FLUXCOMMON/   FG1,FG2
      COMMON /SEDFLUX/ FLUXES
      COMMON /DISSOL/ a, PER_DIS
      COMMON /IRRIG/ ALPHAA,XIRRIG
      COMMON /FIRRG/ rIRRO2,rIRRTC,rIRRNO,rIRRNH,rIRRSO

      COMMON /RATES/ RATE
      COMMON /pHes/ pH


      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/
      DATA FOUR/4.0D+00/,FIVE/5.0D+00/


      X = ZERO
      NPM1 = NPOINTS - 1

      Os =  Y(16)    ! gets the above ground biomass from vector
      Ob =  Y(17)    ! gets the above ground biomass from vector

      tt = t*365.+ 1           ! gets relative time + 1 -> dplant hates 0
      iday  = int(tt)

      O20 = DO20


!     write(*,'(A4,2X,f12.2)') 'O2O',O20  ! check HS concentration
! Apply Q10 relationship to the irrigation parameter
!
!L3  Set dtemp as it is undefined
      dtemp = 25.
!      dtemp = 0.
!L3  end change

      Tq1 = TEMP
      Tq2 = dtemp
      ALPHA0 = ALPHAA*exp(0.06*(Tq2-Tq1))

!
!  Provide values and derivatives for porosity and tortuosity
!
       CALL SED(X,P,DPDX,U,W)
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F=   PS/P         ! conversion dissolved to solids
       SW = SIG(X,U)
       SS = SIG(X,W)
       AGST = ONE/PS*(-DPDX*DB0+PS*DDB(X)-PS*W)
       DDOM = DHS/TWO    ! Estimate of DOM molecular diffusion 
!    
!   Need C0 concentration from calculated from flux information
!
      GM1 = Y(18) + TWO*DH*(FG1-PS*W*Y(1))/PS/DB0
      GM2 = Y(19) + TWO*DH*(FG2-PS*W*Y(2))/PS/DB0

        TESTOM1   = GM1
        TESTOM2   = GM2	
        TESTO2    = DO20
        TESTNIT   = NO30
        TESTNH3   = NH30
        TESTMNO   = MnO0
        TESTMN2   = Mn20
        TESTFE3   = FE30
        TESTFE2   = FE20
        TESTSO4   = SO40
        TESTTS    = HS0
        TESTFES   = FES0
        TESTTC    = TC0 
        TESTALK   = ALK0
        TESTDOM   = DOM0

      CALL REACTION_SDM(TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3,
     *   TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES,
     *   TESTTC,TESTALK,Rc,pH(1),F,dtemp)

        RCH2O1= Rc(1)
        RCH2O2= Rc(2)
        RDOM=   Rc(3)
        RO2=    Rc(4)
        RNO3=   Rc(5)
        RNH4=   Rc(6)
        RMNO=   Rc(7)
        RMN2=   Rc(8)
        RFEOH3= Rc(9)
        RFE2=   Rc(10)
        RSO4=   Rc(12)
        RTS=    Rc(13)
        RFES=   Rc(11)
        RTC=    Rc(14)
        RALK=   Rc(15)


C.......................................................................
         totOM   = TESTOM1 + TESTOM2
         disOM1  = TESTOM1*PER_DIS*(ONE-DEXP(-DB0*a)) 
         disOM2  = TESTOM2*PER_DIS*(ONE-DEXP(-DB0*a))
         disOM   = disOM1+disOM2
C
C OM1
C
      DIFF1 = DB0*(Y(18)-TWO*Y(1)+GM1)/DH/DH
      DGDX = (FG1-PS*W*Y(1))/PS/DB0
      ADV1 = AGST*DGDX
      YDOT(1) = DIFF1 + ADV1 + RCH2O1-disOM1
      RATE(1) = RCH2O1
C
C OM2
      DIFF1 = DB0*(Y(19)-TWO*Y(2)+GM2)/DH/DH
      DGDX = (FG2-PS*W*Y(2))/PS/DB0
      ADV1 = AGST*DGDX
      YDOT(2) = DIFF1 + ADV1 + RCH2O2-disOM2
      RATE(2) = RCH2O2
C      FOM= FOM1 + FOM2
C
C  Transport equations Grid-Point 1
C
       X = RPAR(1)
C
C  Provide values and derivatives for porosity and tortuosity
C
       CALL SED(X,P,DPDX,U,W)
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F=   PS/P          ! conversion dissolved to solids
       SW = SIG(X,U)
       SS = SIG(X,W)
C
C  Provides advective coefficents for solid and porewater species that
C  is general for all species at grid-point X.
C
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**TWO)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)
C
C  Provide input for the reaction for the second grid point
C
       TESTOM1   = Y(1)
       TESTOM2   = Y(2)
       TESTO2    = Y(3)
       TESTNIT   = Y(4)
       TESTNH3   = Y(5)
       TESTMNO   = Y(6)
       TESTMN2   = Y(7)
       TESTFE3   = Y(8)
       TESTFE2   = Y(9)
       TESTSO4   = Y(10)
       TESTTS    = Y(11)
       TESTFES   = Y(12)
       TESTTC    = Y(13)
       TESTALK   = Y(14)
       TESTDOM   = Y(15)
C  711  format(5 f9.3)
C.......................................................................
!L3 "I" undefined, I will set it to one:
!      I = 1
       I = 0
!L3 end modification
!Sed pH to 1
      CALL REACTION_SDM (TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3,
     *   TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES,
     *   TESTTC,TESTALK,Rc,pH(1),F,dtemp)
        RCH2O1= Rc(1)
        RCH2O2= Rc(2)
        RDOM=   Rc(3)
        RO2=    Rc(4)
        RNO3=   Rc(5)
        RNH4=   Rc(6)
        RMNO=   Rc(7)
        RMN2=   Rc(8)
        RFEOH3= Rc(9)
        RFE2=   Rc(10)
        RSO4=   Rc(11)
        RTS=    Rc(12)
        RFES=   Rc(13)
        RTC=    Rc(14)
        RALK=   Rc(15)
        totOM   = TESTOM1 + TESTOM1
C
C O2
C
      FO2= DO2/T2*P*(Y(3) - Y(37))/TWO/DH + U*P*Y(20)
      DIFF2 = (DB(X)+DO2/T2)*(Y(20)-TWO*Y(3)+O20)/DH/DH
      DGDX = ((ONE-SW)*Y(20)+TWO*SW*Y(3)-(ONE+SW)*O20)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF (X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(O20-Y(3))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(3) = DIFF2+ADV2+IRRIG+RO2
      RATE(3) = RO2
      rIRRO2(1) =IRRIG*DH
C 
C NO3
      FNO3= DNO3/T2*P*(Y(4) - Y(38))/TWO/DH + W*P*Y(21)
      DIFF2 = (DB(X)+DNO3/T2)*(Y(21)-TWO*Y(4)+NO30)/DH/DH
      DGDX = ((ONE-SW)*Y(21)+TWO*SW*Y(4)-(ONE+SW)*NO30)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(NO30-Y(4))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(4) = DIFF2 + ADV2 + IRRIG + RNO3
      RATE(4) = RNO3
      rIRRNO(1) =IRRIG*DH
C
C NH3
!     FNH3= DNH3/T2*(Y(5) - Y(39))/TWO/DH + U*P*Y(22)
!     DIFF2 = (DB(X)+DNH3/T2)*(Y(22)-TWO*Y(5)+NH30)/DH/DH
!     DGDX = ((ONE-SW)*Y(22)+TWO*SW*Y(5)-(ONE+SW)*NH30)/TWO/DH
!     ADV2 = (AGTG + ADVC)/P*DGDX
!     IF(X.LE.XIRRIG) THEN 
!        IRRIG = ALPHA0*(NH3I-Y(5))
!     ELSE
!        IRRIG = ZERO
!     ENDIF
!     YDOT(5) = DIFF2 + ADV2 + IRRIG + RNH4
!     RATE(5) = RNH4
!     rIRRNH(1) =IRRIG*DH

C NH3
      FNH3= DNH3/T2*(Y(5) - Y(39))/TWO/DH + U*P*Y(22)
      KADS_p = PS/P*KANH4
      DIFF2 = ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2)*
     *       (Y(22)-TWO*Y(5)+NH30)/DH/DH
      DGDX = ((ONE-SW)*Y(22)+TWO*SW*Y(5)-(ONE+SW)*NH30)/TWO/DH
      AGAD = DPDX*DB(X) + P*DDB(X) - (P*U+ KADS_p*W)
      ADV2 = (AGAD + ADVC)/P*DGDX
      IRRIG = ALPHA0*(NH3I-Y(5))
      YDOT(5) = DIFF2 + ADV2 + IRRIG + RNH4
      RATE(5) = RNH4
      rIRRNH(1) =IRRIG*DH

C
C MnO2 (SOLID PHASE)
      FMnO= DB0*PS*(Y(6) -Y(40))/TWO/DH + W*PS*Y(23)
      DIFF1 = DB(X)*(Y(23)-TWO*Y(6)+MNO0)/DH/DH
      DGDX = ((ONE-SS)*Y(23)+TWO*SS*Y(6)-(ONE+SS)*MNO0)/TWO/DH
      ADV1 = AGST/PS*DGDX    
      YDOT(6) =  DIFF1 + ADV1 + RMNO
      RATE(6) =  RFEOH3
C
C.......................................................................
C MN+2 (DISSOLVED PHASE)
      FMN2= DMN2*P*(Y(7) - Y(41))/TWO/DH + U*P*Y(24)
      DIFF2 = (DB(X)+DMN2/T2)*(Y(24)-TWO*Y(7)+MN20)/DH/DH
      DGDX = ((ONE-SW)*Y(24)+TWO*SW*Y(7)-(ONE+SW)*MN20)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(MN20-Y(7))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(7) = DIFF2+ADV2+IRRIG+RMN2
      RATE(7) = RFE2

C FE+3 (SOLID PHASE)
      FFE3= DB0*PS*(Y(8) -Y(42))/TWO/DH + W*PS*Y(25)
      DIFF1 = DB(X)*(Y(25)-TWO*Y(8)+FE30)/DH/DH
      DGDX = ((ONE-SS)*Y(25)+TWO*SS*Y(8)-(ONE+SS)*FE30)/TWO/DH
      ADV1 = AGST/PS*DGDX    
      YDOT(8) =  DIFF1 + ADV1 + RFEOH3
      RATE(8) =  RFEOH3
C
C.......................................................................
C FE+2 (DISSOLVED PHASE) 
      FFE2= DFE2*P*(Y(9) - Y(43))/TWO/DH + U*P*Y(26)
      DIFF2 = (DB(X)+DFE2/T2)*(Y(26)-TWO*Y(9)+FE20)/DH/DH
      DGDX = ((ONE-SW)*Y(26)+TWO*SW*Y(9)-(ONE+SW)*FE20)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(FE20-Y(9))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(9) = DIFF2+ADV2+IRRIG+RFE2
      RATE(9) = RFE2

C
C SO4--
      FSO4= DSO4/T2*P*(Y(10) - Y(44))/TWO/DH + U*P*Y(27)
      DIFF2 = (DB(X)+DSO4/T2)*(Y(27)-TWO*Y(10)+SO40)/DH/DH
      DGDX = ((ONE-SW)*Y(27)+TWO*SW*Y(10)-(ONE+SW)*SO40)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(SO40-Y(10))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(10) = DIFF2+ADV2+IRRIG+RSO4
      RATE(10) = RSO4
      rIRRSO(1) =IRRIG*DH
C
C TS 
      FHS= DHS/T2*P*(Y(11) - Y(45))/TWO/DH + U*P*Y(28)
      DIFF2 = (DB(X)+DHS/T2)*(Y(28)-TWO*Y(11)+HS0)/DH/DH
      DGDX = ((ONE-SW)*Y(28)+TWO*SW*Y(11)-(ONE+SW)*HS0)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
          IRRIG = ALPHA0*(HS0-Y(11))
      ELSE
          IRRIG = ZERO
      ENDIF
      YDOT(11)=  DIFF2+ADV2+IRRIG+RTS
      RATE(11) = RTS
C

C
C FES  (SOLID PHASE)
      FFES= DB0*PS*(Y(12) -Y(46))/TWO/DH + W*PS*Y(29)
      DIFF1 = DB(X)*(Y(29)-TWO*Y(12)+FES0)/DH/DH
      DGDX = ((ONE-SS)*Y(29)+TWO*SS*Y(12)-(ONE+SS)*FES0)/TWO/DH
       ADV1 = AGST/PS*DGDX    
      YDOT(12) = DIFF1 + ADV1 + RFES
      RATE(12) = RFES
C
C TC
C
      FDIC= DTC/T2*P*(Y(13) - Y(47))/TWO/DH + U*P*Y(30)
      DIFF2 = (DB(X)+DTC/T2)*(Y(30)-TWO*Y(13)+TC0)/DH/DH
      DGDX = ((ONE-SW)*Y(30)+TWO*SW*Y(13)-(ONE+SW)*TC0)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(TC0-Y(13))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(13) = DIFF2+ADV2+IRRIG+RTC
      RATE(13) = RTC
      rIRRTC(1) =IRRIG*DH
C
C ALKALINITY
C 
      FALK= DALK/T2*P*(Y(14) - Y(48))/TWO/DH + U*P*Y(31)
      DIFF2 = (DB(X)+DALK/T2)*(Y(31)-TWO*Y(14)+ALK0)/DH/DH
      DGDX = ((ONE-SW)*Y(31)+TWO*SW*Y(14)-(ONE+SW)*ALK0)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
          IRRIG = ALPHA0*(ALK0-Y(14))
      ELSE
          IRRIG = ZERO
      ENDIF
      YDOT(14)=  DIFF2+ADV2+IRRIG+RALK
      RATE(14) = pH(1)
!
!  DOC (DISSOLVED PHASE) 
!......................................................................
      FDOM= DDOM*P/T2*(Y(15) - Y(49))/TWO/DH + U*P*Y(32)
      DIFF2 = (DB(X)+DDOM/T2)*(Y(32)-TWO*Y(15)+DOM0)/DH/DH
      DGDX = ((ONE-SW)*Y(32)+TWO*SW*Y(15)-(ONE+SW)*DOM0)/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(DOMI-Y(15))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(15) = DIFF2+ADV2+IRRIG+RDOM+disOM
      RATE(15) = RDOM

!
!   ABV (above pycnocline water)
!
      YDOT(16) = dy(1)
      RATE(16) = -9.
!
!   BEL (subpycnocline water)
!
      YDOT(17) = dy(2)
      RATE(17) = -9.

!
! OM
!
      FOM1= DB0*PS*(Y(1) -Y(35))/TWO/DH + W*PS*Y(18)
      DIFF1 = DB(X)*(Y(35)-TWO*Y(18)+Y(1))/DH/DH
      DGDX=((ONE-SS)*Y(35)+TWO*SS*Y(18)-(ONE+SS)*Y(1))/TWO/DH
      ADV1 = AGST/PS*DGDX    
      YDOT(18) = DIFF1 + ADV1 + RCH2O1 - disOM1
      RATE(18) = RCH2O1
!
! OM
      FOM2= DB0*PS*(Y(2) -Y(36))/TWO/DH + W*PS*Y(19)
      DIFF1 = DB(X)*(Y(36)-TWO*Y(19)+Y(2))/DH/DH
      DGDX = ((ONE-SS)*Y(36)+TWO*SS*Y(19)-(ONE+SS)*Y(2))/TWO/DH
      ADV1 = AGST/PS*DGDX    
      YDOT(19) = DIFF1 + ADV1 + RCH2O2 - disOM2
      RATE(19) = RCH2O2

! Save surficial fluxes for output

        FLUXES(1) = FO2
        FLUXES(2) = FNO3
        FLUXES(3) = FNH3
        FLUXES(4) = FSO4
        FLUXES(5) = FDIC
        FLUXES(6) = FOM1
        FLUXES(7) = FOM2
        FLUXES(9) = FALK
  
!      write(*,'(A4,2X,f12.2)') 'FO2',FO2  ! check HS concentrations
!  FOR THE INTERVENING POINTS IN THE GRID TO X2:
!  Following statements gives the # of midpoints NPOINTS -1
      DO 10 I=2,NPM1
       II = I
       X = RPAR(I)
       IF(X.GE.X2) GO TO 20
       M = I*NS              ! starts at 18 for 9 species
!
!  Provide values and derivatives for porosity and tortuosity
!
       CALL SED(X,P,DPDX,U,W)
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       SS = SIG(X,W)
       SW = SIG(X,U)
       PS = ONE - P
       F=   PS/P          ! conversion dissolved to solids
C
C  Provides advective coefficents for solid and porewater species that 
C  is general for all species at grid-point X.
C
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**2)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)


C
C  Provide index for the the finite difference scheme
C
       MAX1  = M + 18        ! G1max
       MAX2  = M + 19        ! G2max
       MAX3  = M + 3         ! Omax
       MAX4  = M + 4         ! NO3max
       MAX5  = M + 5         ! NH3max
       MAX6  = M + 6         ! MNOmax
       MAX7  = M + 7         ! MN2max
       MAX8  = M + 8         ! FE3max
       MAX9  = M + 9         ! FE2max
       MAX10 = M + 10         ! SO4max
       MAX11 = M + 11        ! TSmax
       MAX12 = M + 12        ! FESmax
       MAX13 = M + 13        ! TCmax
       MAX14 = M + 14        ! ALKmax
       MAX15 = M + 15        ! DOMmax
       MAX16 = M + 16        ! Oamax
       MAX17 = M + 17        ! Obmax
       MID1  = M + 1         ! G1mid
       MID2  = M + 2         ! G2mid
       MID3  = M - 14        ! Omid
       MID4  = M - 13        ! NO3mid
       MID5  = M - 12        ! NH3mid
       MID6  = M - 11        ! MNOmid
       MID7  = M - 10        ! MN2mid
       MID8  = M - 9         ! FE3mid
       MID9  = M - 8         ! FE2mid
       MID10 = M - 7         ! SO4mid
       MID11 = M - 6         ! TSmid
       MID12 = M - 5         ! FESmid
       MID13 = M - 4         ! TCmid
       MID14 = M - 3         ! ALKmid
       MID15 = M - 2         ! DOMmid
       MID16 = M - 1         ! Oamid
       MID17 = M             ! Obmid
       MIN1 =  M - 16        ! G1min
       MIN2 =  M - 15        ! G2min
       MIN3 =  M - 31        ! Omin
       MIN4 =  M - 30        ! NO3min
       MIN5 =  M - 29        ! NH3min
       MIN6 =  M - 28        ! MNOmin
       MIN7 =  M - 27        ! MN2min
       MIN8 =  M - 26        ! FE3min
       MIN9 =  M - 25        ! FE2min
       MIN10=  M - 24        ! SO4min
       MIN11 = M - 23        ! TSmin
       MIN12 = M - 22        ! FESmin
       MIN13 = M - 21        ! TCmin
       MIN14 = M - 20        ! ALKmin
       MIN15 = M - 19        ! DOMmin
       MIN16 = M - 18        ! Oamin
       MIN17 = M - 17        ! Obmin
C
C
C    Species w/ corrisponding gridpoints
C
       TESTOM1   = Y(MID1)
       TESTOM2   = Y(MID2)
       TESTO2    = Y(MID3)
       TESTNIT   = Y(MID4)
       TESTNH3   = Y(MID5)
       TESTMNO   = Y(MID6)
       TESTMN2   = Y(MID7)
       TESTFE3   = Y(MID8)
       TESTFE2   = Y(MID9)
       TESTSO4   = Y(MID10)
       TESTTS    = Y(MID11)
       TESTFES   = Y(MID12)
       TESTTC    = Y(MID13)
       TESTALK   = Y(MID14)
       TESTDOM   = Y(MID15)
C
C      IF (TESTOM1 .GT. 3.0D+03) write(*,*) ' 1 OM1 ',TESTOM1
C      IF (TESTOM2 .GT. 3.0D+03) write(*,*) ' 1 OM2 ',TESTOM2
C      IF (TESTO2  .GT. 3.0D+02) write(*,*) ' 1 O2 ',TESTO2
C      IF (TESTNIT .GT. 3.0D+02) write(*,*) ' 1 No3 ',TESTNIT
C      IF (TESTNH3 .GT. 3.0D+02) write(*,*) ' 1 NH3 ',TESTNH3
C      IF (TESTSO4 .GT. 3.0D+02) write(*,*) ' 1 SO4 ',TESTSO4
C      IF (TESTTS  .GT. 3.0D+02) write(*,*) ' 1 TS ',TESTTS
C      IF (TESTFE3 .GT. 3.0D+02) write(*,*) ' 1 FE3 ',TESTFE3
C      IF (TESTFE2 .GT. 3.0D+02) write(*,*) ' 1 FE2 ',TESTFE2
C      IF (TESTFES .GT. 3.0D+02) write(*,*) ' 1 FES ',TESTFES
C      IF (TESTFEC .GT. 3.0D+03) write(*,*) ' 1 FEC ',TESTFEC
C      IF (TESTTC  .GT. 8.0D+01) write(*,*) ' 1 TC ',TESTTC
C      IF (TESTALK .GT. 8.0D+01) write(*,*) ' 1 ALK ',TESTALK
C.......................................................................
      CALL REACTION_SDM (TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3,
     *   TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES,
     *   TESTTC,TESTALK,Rc,pH(I),F,dtemp)
        RCH2O1= Rc(1)
        RCH2O2= Rc(2)
        RDOM=   Rc(3)
        RO2=    Rc(4)
        RNO3=   Rc(5)
        RNH4=   Rc(6)
        RMNO=   Rc(7)
        RMN2=   Rc(8)
        RFEOH3= Rc(9)
        RFE2=   Rc(10)
        RSO4=   Rc(11)
        RTS=    Rc(12)
        RFES=   Rc(13)
        RTC=    Rc(14)
        RALK=   Rc(15)
        totOM   = TESTOM1 + TESTOM1
C

        totOM   = TESTOM1 + TESTOM2
        disOM1  = TESTOM1*PER_DIS*(ONE-DEXP(-DB(X)*a)) 
        disOM2  = TESTOM2*PER_DIS*(ONE-DEXP(-DB(X)*a))
        disOM   = disOM1+disOM2
 125    FORMAT(4 F8.3)
C
C O2  Transport equations
C
      DIFF2 = (DB(X)+DO2/T2)*(Y(MAX3)-TWO*Y(MID3)+Y(MIN3))/DH/DH 
      DGDX=((ONE-SW)*Y(MAX3)+TWO*SW*Y(MID3)-(ONE+SW)*Y(MIN3))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(O20-Y(MID3))
      ELSE
        IRRIG = ZERO
      ENDIF
      YDOT(MID3) = DIFF2+ADV2+IRRIG+RO2
      RATE(MID3) = RO2
      rIRRO2(I) =IRRIG*DH
C
C NO3
C	
      DIFF2 = (DB(X)+DNO3/T2)*(Y(MAX4)-TWO*Y(MID4)+Y(MIN4))/DH/DH 
      DGDX=((ONE-SW)*Y(MAX4)+TWO*SW*Y(MID4)-(ONE+SW)*Y(MIN4))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(NO30-Y(MID4))
      ELSE
        IRRIG = ZERO
      ENDIF
      YDOT(MID4) = DIFF2 + ADV2 + IRRIG + RNO3
      RATE(MID4) = RNO3
      rIRRNO(I) =IRRIG*DH
C
C NH3
C	
!     DIFF2 = (DB(X)+DNH3/T2)*(Y(MAX5)-TWO*Y(MID5)+Y(MIN5))/DH/DH 
!     DGDX=((ONE-SW)*Y(MAX5)+TWO*SW*Y(MID5)-(ONE+SW)*Y(MIN5))/TWO/DH
!     ADV2 = (AGTG + ADVC)/P*DGDX
!     IF(X.LE.XIRRIG) THEN 
!        IRRIG = ALPHA0*(NH3I-Y(MID5))
!     ELSE
!        IRRIG = ZERO
!     ENDIF
!     YDOT(MID5) = DIFF2 + ADV2+ IRRIG + RNH4
!     RATE(MID5) = RNH4
!     rIRRNH(I) =IRRIG*DH

C
C NH3
C
      KADS_p = PS/P*KANH4
      DIFF2 =  ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2)*
     *         (Y(MAX5)-TWO*Y(MID5)+Y(MIN5))/DH/DH
      DGDX=((ONE-SW)*Y(MAX5)+TWO*SW*Y(MID5)-(ONE+SW)*Y(MIN5))/TWO/DH
      AGAD = DPDX*DB(X) + P*DDB(X) - (P*U+ KADS_p*W)
      ADV2 = (AGAD + ADVC)/P*DGDX
      IRRIG = ALPHA0*(NH3I-Y(MID5))
      YDOT(MID5) = DIFF2 + ADV2+ IRRIG + RNH4
      RATE(MID5) = RNH4
      rIRRNH(I) =IRRIG*DH

C
C MNO2 (SOLID PHASE)
C......................................................................
      DIFF1 = DB(X)*(Y(MAX6)-TWO*Y(MID6)+Y(MIN6))/DH/DH
      DGDX = ((ONE-SS)*Y(MAX6)+TWO*SS*Y(MID6)-(ONE+SS)*Y(MIN6))/TWO/DH
      ADV1= AGST/PS*DGDX
      YDOT(MID6) = DIFF1 + ADV1 + RMNO
      RATE(MID6) = RMNO
C
C MN+2 (DISSOLVED PHASE)
C
      DIFF2 = (DB(X)+DMN2/T2)*(Y(MAX7)-TWO*Y(MID7)+Y(MIN7))/DH/DH
      DGDX = ((ONE-SW)*Y(MAX7)+TWO*SW*Y(MID7)-(ONE+SW)*Y(MIN7))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(MN20-Y(MID7))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID7) = DIFF2 + ADV2 + IRRIG + RMN2
      RATE(MID7) = RFE2
C
C FE+3 (SOLID PHASE)
C......................................................................
      DIFF1 = DB(X)*(Y(MAX8)-TWO*Y(MID8)+Y(MIN8))/DH/DH
      DGDX = ((ONE-SS)*Y(MAX8)+TWO*SS*Y(MID8)-(ONE+SS)*Y(MIN8))/TWO/DH
      ADV1= AGST/PS*DGDX
      YDOT(MID8) = DIFF1 + ADV1 + RFEOH3
      RATE(MID8) = RFEOH3
C
C FE+2 (DISSOLVED PHASE) 
C
      DIFF2 = (DB(X)+DFE2/T2)*(Y(MAX9)-TWO*Y(MID9)+Y(MIN9))/DH/DH 
      DGDX = ((ONE-SW)*Y(MAX9)+TWO*SW*Y(MID9)-(ONE+SW)*Y(MIN9))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(FE20-Y(MID9))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID9) = DIFF2 + ADV2 + IRRIG + RFE2
      RATE(MID9) = RFE2
C
C SO4--
C	
      DIFF2 = (DB(X)+DSO4/T2)*(Y(MAX10)-TWO*Y(MID10)+Y(MIN10))/DH/DH
      DGDX=((ONE-SW)*Y(MAX10)+TWO*SW*Y(MID10)-(ONE+SW)*Y(MIN10))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(SO40-Y(MID10))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID10) = DIFF2 + ADV2 + IRRIG + RSO4
      RATE(MID10) = RSO4
      rIRRSO(I) =IRRIG*DH
C
C TS 
C	
      DIFF2 = (DB(X)+DHS/T2)*(Y(MAX11)-TWO*Y(MID11)+Y(MIN11))/DH/DH
      DGDX=((ONE-SW)*Y(MAX11)+TWO*SW*Y(MID11)-(ONE+SW)*Y(MIN11))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
        IRRIG = ALPHA0*(HS0-Y(MID11))
      ELSE
        IRRIG = ZERO
      ENDIF
      YDOT(MID11)=DIFF2+ADV2+IRRIG + RTS
      RATE(MID11) = RTS
C
C
C FES  (SOLID PHASE)
      DIFF1 = DB(X)*(Y(MAX12)-TWO*Y(MID12)+Y(MIN12))/DH/DH
      DGDX=((ONE-SS)*Y(MAX12)+TWO*SS*Y(MID12)-(ONE+SS)*Y(MIN12))/TWO/DH
      ADV1= AGST/PS*DGDX
      YDOT(MID12) = DIFF1 + ADV1 + RFES
      RATE(MID12) = RFES
C
C TC (DISSOLVED PHASE) 
C
      DIFF2=(DB(X)+DTC)*(Y(MAX13)-TWO*Y(MID13)+Y(MIN13))/DH/DH
      DGDX=((ONE-SW)*Y(MAX13)+TWO*SW*Y(MID13)-(ONE+SW)*Y(MIN13))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(TC0-Y(MID13))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID13) = DIFF2+ADV2+IRRIG+RTC
      RATE(MID13) = RTC
      rIRRTC(I) =IRRIG*DH
C
C ALKALINITY (DISSOLVED PHASE) 
C
      DIFF2=(DB(X)+DALK/T2)*(Y(MAX14)-TWO*Y(MID14)+Y(MIN14))/DH/DH
      DGDX=((ONE-SW)*Y(MAX14)+TWO*SW*Y(MID14)-(ONE+SW)*Y(MIN14))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(ALK0-Y(MID14))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID14) = DIFF2+ADV2+IRRIG+RALK
      RATE(MID14) = pH(I)
C
C DOC (DISSOLVED PHASE) 
C......................................................................
C
      DIFF2=(DB(X)+DDOM/T2)*(Y(MAX15)-TWO*Y(MID15)+Y(MIN15))/DH/DH
      DGDX=((ONE-SW)*Y(MAX15)+TWO*SW*Y(MID15)-(ONE+SW)*Y(MIN15))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(DOMI-Y(MID15))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID15) = DIFF2+ADV2+IRRIG+RDOM +disOM
      RATE(MID15) = RDOM

C
C   ABV (above pycnocline water)
C
      YDOT(MID16) = dy(1)
      RATE(MID16) = -9.

C
C   BEL (subpycnocline water)
C
      YDOT(MID17) = dy(2)
      RATE(MID17) = -9.


C
C OM1
C
       DIFF1 = DB(X)*(Y(MAX1)-TWO*Y(MID1)+Y(MIN1))/DH/DH
       DGDX=((ONE-SS)*Y(MAX1)+TWO*SS*Y(MID1)-(ONE+SS)*Y(MIN1))/TWO/DH
       ADV1= AGST/PS*DGDX
       YDOT(MID1) = DIFF1 + ADV1 + RCH2O1 - disOM1
       RATE(MID1) = RCH2O1
       POROVEC(I)= P
C OM2
       DIFF1 = DB(X)*(Y(MAX2)-TWO*Y(MID2)+Y(MIN2))/DH/DH
       DGDX=((ONE-SS)*Y(MAX2)+TWO*SS*Y(MID2)-(ONE+SS)*Y(MIN2))/TWO/DH
       ADV1= AGST/PS*DGDX
       YDOT(MID2) = DIFF1 + ADV1 + RCH2O2 - disOM2
       RATE(MID2) = RCH2O2
   10 CONTINUE
C
C  FOR THE INTERVENING POINTS IN THE GRID FROM X2 TO XL-DH:
C
   20 CONTINUE
      DO 30 I=II,NPM1
       X = RPAR(I)
       M = I*NS
       CALL SED(X,P,DPDX,U,W) 
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F=   PS/P          ! conversion dissolved to solids
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**2)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)

       MAX1  = M + 18        ! G1max
       MAX2  = M + 19        ! G2max
       MAX3  = M + 3         ! Omax
       MAX4  = M + 4         ! NO3max
       MAX5  = M + 5         ! NH3max
       MAX6  = M + 6         ! MNOmax
       MAX7  = M + 7         ! MN2max
       MAX8  = M + 8         ! FE3max
       MAX9  = M + 9         ! FE2max
       MAX10 = M + 10         ! SO4max
       MAX11 = M + 11        ! TSmax
       MAX12 = M + 12        ! FESmax
       MAX13 = M + 13        ! TCmax
       MAX14 = M + 14        ! ALKmax
       MAX15 = M + 15        ! DOMmax
       MAX16 = M + 16        ! Oamax
       MAX17 = M + 17        ! Obmax
       MID1  = M + 1         ! G1mid
       MID2  = M + 2         ! G2mid
       MID3  = M - 14        ! Omid
       MID4  = M - 13        ! NO3mid
       MID5  = M - 12        ! NH3mid
       MID6  = M - 11        ! MNOmid
       MID7  = M - 10        ! MN2mid
       MID8  = M - 9         ! FE3mid
       MID9  = M - 8         ! FE2mid
       MID10 = M - 7         ! SO4mid
       MID11 = M - 6         ! TSmid
       MID12 = M - 5         ! FESmid
       MID13 = M - 4         ! TCmid
       MID14 = M - 3         ! ALKmid
       MID15 = M - 2         ! DOMmid
       MID16 = M - 1         ! Oamid
       MID17 = M             ! Obmid
       MIN1 =  M - 16        ! G1min
       MIN2 =  M - 15        ! G2min
       MIN3 =  M - 31        ! Omin
       MIN4 =  M - 30        ! NO3min
       MIN5 =  M - 29        ! NH3min
       MIN6 =  M - 28        ! MNOmin
       MIN7 =  M - 27        ! MN2min
       MIN8 =  M - 26        ! FE3min
       MIN9 =  M - 25        ! FE2min
       MIN10=  M - 24        ! SO4min
       MIN11 = M - 23        ! TSmin
       MIN12 = M - 22        ! FESmin
       MIN13 = M - 21        ! TCmin
       MIN14 = M - 20        ! ALKmin
       MIN15 = M - 19        ! DOMmin
       MIN16 = M - 18        ! Oamin
       MIN17 = M - 17        ! Obmin
C
C
C    Species w/ corrisponding gridpoints
       TESTOM1   = Y(MID1)	
       TESTOM2   = Y(MID2)	
       TESTO2    = Y(MID3)
       TESTNIT   = Y(MID4)
       TESTNH3   = Y(MID5)
       TESTMNO   = Y(MID6)
       TESTMN2   = Y(MID7)
       TESTFE3   = Y(MID8)
       TESTFE2   = Y(MID9)
       TESTSO4   = Y(MID10)
       TESTTS    = Y(MID11)
       TESTFES   = Y(MID12)
       TESTTC    = Y(MID13)
       TESTALK   = Y(MID14)
       TESTDOM   = Y(MID15)
C
C
C.......................................................................
C

      CALL REACTION_SDM (TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3,
     *   TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES,
     *   TESTTC,TESTALK,Rc,pH(I),F,dtemp)
        RCH2O1= Rc(1)
        RCH2O2= Rc(2)
        RDOM=   Rc(3)
        RO2=    Rc(4)
        RNO3=   Rc(5)
        RNH4=   Rc(6)
        RMNO=   Rc(7)
        RMN2=   Rc(8)
        RFEOH3= Rc(9)
        RFE2=   Rc(10)
        RSO4=   Rc(11)
        RTS=    Rc(12)
        RFES=   Rc(13)
        RTC=    Rc(14)
        RALK=   Rc(15)

        totOM   = TESTOM1 + TESTOM2
        disOM1  = TESTOM1*PER_DIS*(ONE-DEXP(-DB(X)*a)) 
        disOM2  = TESTOM2*PER_DIS*(ONE-DEXP(-DB(X)*a))
        disOM   = disOM1+disOM2
C
C  O2
C
      DIFF2 = DO2/T2*(Y(MAX3)-TWO*Y(MID3)+Y(MIN3))/DH/DH 
      DGDX =  (Y(MAX3)-Y(MIN3))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(O20-Y(MID3))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID3) = DIFF2 + ADV2+IRRIG + RO2
      RATE(MID3) = RO2
      rIRRO2(I) =IRRIG*DH
C
C NO3
      DIFF2 = DNO3/T2*(Y(MAX4)-TWO*Y(MID4)+Y(MIN4))/DH/DH 
      DGDX =  (Y(MAX4)-Y(MIN4))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
          IRRIG = ALPHA0*(NO30-Y(MID4))
      ELSE
          IRRIG = ZERO
      ENDIF
      YDOT(MID4) = DIFF2 + ADV2 + IRRIG + RNO3
      RATE(MID4) = RNO3
      rIRRNO(I) =IRRIG*DH
C
C NH4+
C
!     DIFF2 = DNH3/T2*(Y(MAX5)-TWO*Y(MID5)+Y(MIN5))/DH/DH 
!     DGDX =  (Y(MAX5)-Y(MIN5))/TWO/DH
!     ADV2 = (AGTG + ADVC)/P*DGDX
!     IF(X.LE.XIRRIG) THEN 
!         IRRIG = ALPHA0*(NH3I-Y(MID5))
!     ELSE
!         IRRIG = ZERO
!     ENDIF
!     YDOT(MID5) = DIFF2 + ADV2+IRRIG + RNH4
!     RATE(MID5) = RNH4
!     rIRRNH(I) =IRRIG*DH

C
C NH4+
C
      KADS_p = PS/P*KANH4
      DIFF2 =  ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2)*
     *         (Y(MAX5)-TWO*Y(MID5)+Y(MIN5))/DH/DH
      DGDX =  (Y(MAX5)-Y(MIN5))/TWO/DH
      AGAD = DPDX*DB(X) + P*DDB(X) - (P*U+ KADS_p*W)
      ADV2 = (AGAD + ADVC)/P*DGDX
      IRRIG = ALPHA0*(NH3I-Y(MID5))
      YDOT(MID5) = DIFF2 + ADV2+IRRIG + RNH4
      RATE(MID5) = RNH4
      rIRRNH(I) =IRRIG*DH


C
C MNO2 (SOLID PHASE)
C
      DGDX = (Y(MID6)-Y(MIN6))/DH
      ADV1 = - W*DGDX
      YDOT(MID6) = ADV1 + RMNO
      RATE(MID6) = RMNO
C
C MN+2 (DISSOLVED PHASE)
C
      DIFF2 = DMN2/T2*(Y(MAX7)-TWO*Y(MID7)+Y(MIN7))/DH/DH
      DGDX =  (Y(MAX7)-Y(MIN7))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
        IRRIG = ALPHA0*(MN20-Y(MID7))
      ELSE
        IRRIG = ZERO
      ENDIF
      YDOT(MID7) = DIFF2 + ADV2 + IRRIG + RMN2
      RATE(MID7) = RMN2
C
C FE+3 (SOLID PHASE)
C
      DGDX = (Y(MID8)-Y(MIN8))/DH
      ADV1 = - W*DGDX
      YDOT(MID8) = ADV1 + RFEOH3
      RATE(MID8) = RFEOH3
C
C FE+2 (DISSOLVED PHASE) 
C
      DIFF2 = DFE2/T2*(Y(MAX9)-TWO*Y(MID9)+Y(MIN9))/DH/DH
      DGDX =  (Y(MAX9)-Y(MIN9))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
        IRRIG = ALPHA0*(FE20-Y(MID9))
      ELSE
        IRRIG = ZERO
      ENDIF
      YDOT(MID9) = DIFF2 + ADV2 + IRRIG + RFE2
      RATE(MID9) = RFE2
C
C SO4--
C
      DIFF2 = DSO4/T2*(Y(MAX10)-TWO*Y(MID10)+Y(MIN10))/DH/DH
      DGDX =  (Y(MAX10)-Y(MIN10))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(SO40-Y(MID10))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID10) = DIFF2 + ADV2 + IRRIG + RSO4
      RATE(MID10) = RSO4
      rIRRSO(I) =IRRIG*DH
C
C TS 
C
      DIFF2 = DHS/T2*(Y(MAX11)-TWO*Y(MID11)+Y(MIN11))/DH/DH
      DGDX =  (Y(MAX11)-Y(MIN11))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
          IRRIG = ALPHA0*(HS0-Y(MID11))
      ELSE
          IRRIG = ZERO
      ENDIF
      YDOT(MID11)=DIFF2 + ADV2+IRRIG + RTS
      RATE(MID11) = RTS

C
C FES  (SOLID PHASE) 
C
      DGDX = (Y(MID12)-Y(MIN12))/DH
      ADV1 = - W*DGDX
      YDOT(MID12) = ADV1 + RFES
      RATE(MID12) = RFES
C
C TC  (DISSOLVED PHASE) 
C
      DIFF2 = DTC/T2*(Y(MAX13)-TWO*Y(MID13)+Y(MIN13))/DH/DH
      DGDX =  (Y(MAX13)-Y(MIN13))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(TC0-Y(MID13))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID13) = DIFF2 + ADV2+IRRIG + RTC
      RATE(MID13) = RTC
      rIRRTC(I) =IRRIG*DH
C
C  ALKALINITY (DISSOLVED PHASE) 
C
      DIFF2 = DALK/T2*(Y(MAX14)-TWO*Y(MID14)+Y(MIN14))/DH/DH
      DGDX =  (Y(MAX14)-Y(MIN14))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(ALK0-Y(MID14))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID14) = DIFF2 + ADV2+IRRIG + RALK
      RATE(MID14) = pH(I)
C
C DOM (DISSOLVED PHASE) 
C
      DIFF2 = DDOM/T2*(Y(MAX15)-TWO*Y(MID15)+Y(MIN15))/DH/DH
      DGDX =  (Y(MAX15)-Y(MIN15))/TWO/DH
      ADV2 = (AGTG + ADVC)/P*DGDX
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(DOMI-Y(MID15))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID15) = DIFF2+ADV2+IRRIG+RDOM+disOM
      RATE(MID15) = RDOM

C
C   ABV (above pycnocline water)
C
      YDOT(MID16) = dy(1)

      RATE(MID16) = -9.
C
C   BEL (subpycnocline water)
C
      YDOT(MID17) = dy(2)
      RATE(MID17) = -9.

C
C OM1
C
C......................................................................
      DGDX = (Y(MID1)-Y(MIN1))/DH
      ADV1 = - W*DGDX
      YDOT(MID1) = ADV1 + RCH2O1 - disOM1
      RATE(MID1) = RCH2O2
      POROVEC(I) = P

C
C OM2
C
      DGDX = (Y(MID2)-Y(MIN2))/DH
      ADV1 = - W*DGDX
      YDOT(MID2) = ADV1 + RCH2O2 - disOM2
      RATE(MID2) = RCH2O2
   30 CONTINUE
C
C  FOR THE LAST POINT IN THE GRID:
C
       I = NPOINTS
       X = RPAR(I)
       M = I*NS
       CALL SED(X,P,DPDX,U,W) 
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F=   PS/P          ! conversion dissolved to solids
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**2)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)

C
       MID1  = M + 1         ! G1mid
       MID2  = M + 2         ! G2mid
       MID3  = M - 14        ! Omid
       MID4  = M - 13        ! NO3mid
       MID5  = M - 12        ! NH3mid
       MID6  = M - 11        ! MNOmid
       MID7  = M - 10        ! MN2mid
       MID8  = M - 9         ! FE3mid
       MID9  = M - 8         ! FE2mid
       MID10 = M - 7         ! SO4mid
       MID11 = M - 6         ! TSmid
       MID12 = M - 5         ! FESmid
       MID13 = M - 4         ! TCmid
       MID14 = M - 3         ! ALKmid
       MID15 = M - 2         ! DOMmid
       MID16 = M - 1         ! Oamid
       MID17 = M             ! Obmid
       MIN1 =  M - 16        ! G1min
       MIN2 =  M - 15        ! G2min
       MIN3 =  M - 31        ! Omin
       MIN4 =  M - 30        ! NO3min
       MIN5 =  M - 29        ! NH3min
       MIN6 =  M - 28        ! MNOmin
       MIN7 =  M - 27        ! MN2min
       MIN8 =  M - 26        ! FE3min
       MIN9 =  M - 25        ! FE2min
       MIN10=  M - 24        ! SO4min
       MIN11 = M - 23        ! TSmin
       MIN12 = M - 22        ! FESmin
       MIN13 = M - 21        ! TCmin
       MIN14 = M - 20        ! ALKmin
       MIN15 = M - 19        ! DOMmin
       MIN16 = M - 18        ! Oamin
       MIN17 = M - 17        ! Obmin
C
C    Species w/ corrisponding gridpoints
C
       TESTOM1   = Y(MID1)
       TESTOM2   = Y(MID2)	
       TESTO2    = Y(MID3)
       TESTNIT   = Y(MID4)
       TESTNH3   = Y(MID5)
       TESTMNO   = Y(MID6)
       TESTMN2   = Y(MID7)
       TESTFE3   = Y(MID8)
       TESTFE2   = Y(MID9)
       TESTSO4   = Y(MID10)
       TESTTS    = Y(MID11)
       TESTFES   = Y(MID12)
       TESTTC    = Y(MID13)
       TESTALK   = Y(MID14)
       TESTDOM   = Y(MID15)

C
C      IF (TESTOM1 .GT. 3.0D+03) write(*,*) ' 1 OM1 ',TESTOM1
C      IF (TESTOM2 .GT. 3.0D+03) write(*,*) ' 1 OM2 ',TESTOM2
C      IF (TESTO2  .GT. 3.0D+02) write(*,*) ' 1 O2 ',TESTO2
C      IF (TESTNIT .GT. 3.0D+02) write(*,*) ' 1 No3 ',TESTNIT
C      IF (TESTNH3 .GT. 3.0D+02) write(*,*) ' 1 NH3 ',TESTNH3
C      IF (TESTSO4 .GT. 3.0D+02) write(*,*) ' 1 SO4 ',TESTSO4
C      IF (TESTTS  .GT. 3.0D+02) write(*,*) ' 1 TS ',TESTTS
C      IF (TESTFE3 .GT. 3.0D+02) write(*,*) ' 1 FE3 ',TESTFE3
C      IF (TESTFE2 .GT. 3.0D+02) write(*,*) ' 1 FE2 ',TESTFE2
C      IF (TESTFES .GT. 3.0D+02) write(*,*) ' 1 FES ',TESTFES
C      IF (TESTTC .GT. 8.0D+01)  write(*,*) ' 1 TC ',TESTTC
C      IF (TESTALK .GT. 8.0D+01) write(*,*) ' 1 ALK ',TESTALK
C
C.......................................................................
       CALL REACTION_SDM(TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3,
     *   TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES,
     *   TESTTC,TESTALK,Rc,pH(I),F,dtemp)
        RCH2O1= Rc(1)
        RCH2O2= Rc(2)
        RDOM=   Rc(3)
        RO2=    Rc(4)
        RNO3=   Rc(5)
        RNH4=   Rc(6)
        RMNO=   Rc(7)
        RMN2=   Rc(8)
        RFEOH3= Rc(9)
        RFE2=   Rc(10)
        RSO4=   Rc(11)
        RTS=    Rc(12)
        RFES=   Rc(13)
        RTC=    Rc(14)
        RALK=   Rc(15)

        totOM   = TESTOM1 + TESTOM2
        disOM1  = TESTOM1*PER_DIS*(ONE-DEXP(-DB(X)*a)) 
        disOM2  = TESTOM2*PER_DIS*(ONE-DEXP(-DB(X)*a))
        disOM   = disOM1+disOM2
C
C O2
C
      FO2l= DO2*P/T2*(Y(MIN3-NS)-Y(MID3))/TWO/DH+U*P*Y(MID3)
      DIFF2 = DO2/T2*TWO*(-Y(MID3)+Y(MIN3))/DH/DH 
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(O20-Y(MID3))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID3) =DIFF2+IRRIG + RO2
      RATE(MID3) = RO2
      rIRRO2(I) =IRRIG*DH

C NO3
       FNO3l= DNO3*P/T2*(Y(MIN4-NS)-Y(MID4))/TWO/DH+U*P*Y(MID4)
       DIFF2 = DNO3/T2*TWO*(-Y(MID4)+Y(MIN4))/DH/DH 
       IF(X.LE.XIRRIG) THEN 
           IRRIG = ALPHA0*(NO30-Y(MID4))
       ELSE
           IRRIG = ZERO
       ENDIF
       YDOT(MID4) = DIFF2 + IRRIG + RNO3
       RATE(MID4) = RNO3
       rIRRNO(I) =IRRIG*DH

C
C NH3
C
!      FNH3l= DNH3*P/T2*(Y(MIN5-NS)-Y(MID5))/TWO/DH+U*P*Y(MID5)
!      DIFF2 = DNH3/T2*TWO*(-Y(MID5)+Y(MIN5))/DH/DH 
!      IF(X.LE.XIRRIG) THEN 
!        IRRIG = ALPHA0*(NH3I-Y(MID5))
!      ELSE
!        IRRIG = ZERO
!      ENDIF
!      YDOT(MID5) = DIFF2 + IRRIG + RNH4
!      RATE(MID5) = RNH4
!      rIRRNH(I) =IRRIG*DH

C
C NH3
C
      FNH3l= DNH3*P/T2*(Y(MIN5-NS)-Y(MID5))/TWO/DH+U*P*Y(MID5)

      KADS_p = PS/P*KANH4
      DIFF2 = ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2)*
     *        TWO*(-Y(MID5)+Y(MIN5))/DH/DH
      IRRIG = ALPHA0*(NH3I-Y(MID5))
      YDOT(MID5) = DIFF2 + IRRIG + RNH4
      RATE(MID5) = RNH4
      rIRRNH(I) =IRRIG*DH


C
C  MNO2 (SOLID PHASE)
C
      FFE3l= W*PS*Y(MID6)
      DGDX = (Y(MID6)-Y(MIN6))/DH
      ADV1 = - W*DGDX
      YDOT(MID6) = ADV1 + RMNO
      RATE(MID6) = RMNO
C
C MN+2  (DISSOLVED PHASE)
C
      FFE2l= DMN2*P/T2*(Y(MIN7-NS)-Y(MID7))/TWO/DH+U*P*Y(MID7)
      DIFF2 = DMN2/T2*TWO*(-Y(MID7)+Y(MIN7))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(MN20-Y(MID7))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID7) = DIFF2+IRRIG+RMN2
      RATE(MID7) = RMN2
C
C FE+3 (SOLID PHASE)
C
      FFE3l= W*PS*Y(MID8)
      DGDX = (Y(MID8)-Y(MIN8))/DH
      ADV1 = - W*DGDX
      YDOT(MID8) = ADV1 + RFEOH3
      RATE(MID8) = RFEOH3
C
C FE+2  (DISSOLVED PHASE)
C
      FFE2l= DFE2*P/T2*(Y(MIN9-NS)-Y(MID9))/TWO/DH+U*P*Y(MID9)
      DIFF2 = DFE2/T2*TWO*(-Y(MID9)+Y(MIN9))/DH/DH 
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(FE20-Y(MID9))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID9) = DIFF2+IRRIG+RFE2
      RATE(MID9) = RFE2
C
C SO4--
C
       FSO4l= DSO4*P/T2*(Y(MIN10-NS)-Y(MID10))/TWO/DH+U*P*Y(MID10)
       DIFF2 = DSO4/T2*TWO*(-Y(MID10)+Y(MIN10))/DH/DH
       IF(X.LE.XIRRIG) THEN 
           IRRIG = ALPHA0*(SO40-Y(MID10))
       ELSE
           IRRIG = ZERO
       ENDIF
       YDOT(MID10) = DIFF2 + IRRIG  + RSO4
       RATE(MID10) = RSO4
       rIRRSO(I) =IRRIG*DH
C
C TS (PORE WATER)
C
      FHSl= DHS*P/T2*(Y(MIN11-NS)-Y(MID11))/TWO/DH+U*P*Y(MID11)
      DIFF2 = DHS/T2*TWO*(-Y(MID11)+Y(MIN11))/DH/DH
      IF(X.LE.XIRRIG) THEN 
          IRRIG = ALPHA0*(HS0-Y(MID11))
      ELSE
          IRRIG = ZERO
      ENDIF
      YDOT(MID11) =DIFF2+IRRIG+RTS
      RATE(MID11) = RTS
C
C FES  (SOLID PHASE) 
C
      FFESl= W*PS*Y(MID12)
      DGDX = (Y(MID12)-Y(MIN12))/DH
      ADV1 = - W*DGDX
      YDOT(MID12) = ADV1 + RFES
      RATE(MID12) = RFES
C
C TC  (DISSOLVED PHASE) 
C 
      FDICl= DTC*P/T2*(Y(MIN13-NS)-Y(MID13))/TWO/DH+U*P*Y(MID13)
      DIFF2 = DTC/T2*TWO*(-Y(MID13)+Y(MIN13))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(TC0-Y(MID13))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID13) = DIFF2+IRRIG + RTC
      RATE(MID13) = RTC
      rIRRTC(I) =IRRIG*DH
C
C ALKALINITY (DISSOLVED PHASE) 
C
      FALKl= DALK*P/T2*(Y(MIN14-NS)-Y(MID14))/TWO/DH+U*P*Y(MID14)
      DIFF2 = DALK/T2*TWO*(-Y(MID14)+Y(MIN14))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(ALK0-Y(MID14))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID14) = DIFF2+IRRIG + RALK
      RATE(MID14) = pH(I)
C
C DOM (DISSOLVED PHASE) 
C
C.......................................................................
      FDOMl= DDOM*P/T2*(Y(MIN15-NS)-Y(MID15))/TWO/DH+U*P*Y(MID15)
      DIFF2 = DDOM/T2*TWO*(-Y(MID15)+Y(MIN15))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(DOMI-Y(MID15))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID15) = DIFF2+IRRIG+RDOM+disOM
      RATE(MID15) = RDOM

C
C   ABV (above pycnocline water)
C
      YDOT(MID16) = dy(1)
      RATE(MID16) = -9.

C
C   BEL (subpycnocline water)
C
      YDOT(MID17) = dy(2)
      RATE(MID17) = -9.

C
C OM1
C
       FOM1l= W*PS*Y(MID1)
       DGDX = (Y(MID1)-Y(MIN1))/DH
       ADV1 = - W*DGDX
       YDOT(MID1) = ADV1 + RCH2O2-disOM1
       RATE(MID1) = RCH2O2
       POROVEC(I)=P
C
C OM2
C
       FOM2l= W*PS*Y(MID2)
       DGDX = (Y(MID2)-Y(MIN2))/DH
       ADV1 = - W*DGDX
       YDOT(MID2) = ADV1 + RCH2O2-disOM2
       RATE(MID2) = RCH2O2
       RETURN
       END
C
C
C
C   JEX	A DUMMY ROUTINE FOR THE JACOBIAN CALLED BY VODE.f
C
C
      SUBROUTINE JEX (NEQ, T, Y, ML, MU, PD, NRPD, RPAR, IPAR)
      DOUBLE PRECISION PD, RPAR, T, Y
      DIMENSION Y(NEQ), PD(NRPD,NEQ)
      RETURN
      END
	  
C ----------------------------------------------------------------
C REACTIONS
C PURPOSE:  Provides all reaction rates for the model
C ----------------------------------------------------------------
C
      SUBROUTINE REACTION_SDM(TESTOM1,TESTOM2,TESTOM3,TESTO2,TESTNIT,
     *TESTNH3,TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES,
     *TESTTC,TESTALK,Rc,pH,F,dtemp)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 KG1,KG2,KDOM
      REAL*8 KO2, KNO3, KMNO,  KFE3,  KSO4
      REAL*8 k8,k10,k11,k12,k13,k14
      REAL*8 k15,k23,k_23
      REAL*8 KpFES
      DIMENSION R(6),R1(6),R2(6),R3(6)
      DIMENSION spc(9),Rc(25)
      COMMON /CONSTANT2/ KpFES
      COMMON /CONSTANT3/ k8,k10,k11,k12,k13,k14,k15,k23,k_23
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      COMMON /STOIC/ SC1,SN1,SP1,SC2,SN2,SP2,SC3,SN3,SP3
      COMMON /TEMPERATURE/ TEMP, SAL, PRESS,pH0
C
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/
      DATA THREE/3.0D+00/,FOUR/4.0D+00/,FIVE/5.0D+00/,EIGHT/8.0D+00/
C
C   STOICHIOMETRY (Cappellen and Wang 1996)
C          (Primary reactants)
C
      x1=SC1
      y1=SN1
      z1=SP1
      x2=SC2
      y2=SN2
      z2=SP2
      x3=SC3
      y3=SN3
      z3=SP3
C
C......................................................................
      IF (TESTO2  .LE. ZERO) TESTO2 = ZERO
      IF (TESTSO4 .LE. ZERO) TESTSO4 = ZERO
      IF (TESTMNO .LE. ZERO) TESTMNO= ZERO
      IF (TESTMN2 .LE. ZERO) TESTMN2= ZERO
      IF (TESTFE3 .LE. ZERO) TESTFE3= ZERO
      IF (TESTFE2 .LE. ZERO) TESTFE2= ZERO
      IF (TESTTS  .LE. ZERO) TESTTS= ZERO

C
C Use the Q10 relationship to determine the rates
C Assume that TEMP is the maximum temperature
C
      rq1=  KG1
      rq2=  KG2
      rq3=  KDOM
      Tq1 = TEMP
      Tq2 = dtemp 
      rq21= LOG10(rq1)-LOG10(2.)*((Tq1-Tq2)/10.)
      rq21= 10.0**rq21
      rq22= LOG10(rq2)-LOG10(2.)*((Tq1-Tq2)/10.)
      rq22= 10.0**rq22
      rq23= LOG10(rq3)-LOG10(2.)*((Tq1-Tq2)/10.)
      rq23= 10.0**rq23

C
C  ------ Now for the chemolithoautotrophic processes
C
      rq1= k11
      rq2= k11
      rq11= LOG10(rq1)-LOG10(2.)*((Tq1-Tq2)/10.)
      rq11= 10.0**rq11
      rq12= LOG10(rq2)-LOG10(2.)*((Tq1-Tq2)/10.)
      rq12= 10.0**rq12

C
C lets oxidants determine the rate of organic
C          degradation using the full Monod relationship
C
C Calculate the concentration of OMs from Flux
C
        RCt1= rq21*TESTOM1
        RCt2= rq22*TESTOM2
        RCt3= rq23*TESTOM3
        R(1) = TESTO2/(KO2+TESTO2)
        R(2) = TESTNIT/(KNO3+TESTNIT)*RNITRATE_SDM(TESTO2)
        R(3) = TESTMNO/(KMNO+TESTMNO)*
     &         RMANGANESE(TESTO2,TESTNIT)
        R(4) = TESTFE3/(KFE3+TESTFE3)*
     &         RFERRIC(TESTO2,TESTNIT,TESTFE3)
        R(5) = TESTSO4/(KSO4+TESTSO4)*
     &         RSULFATE(TESTO2,TESTNIT,TESTMN0,TESTFE3)
        R(1) = DMAX1(ZERO,R(1))
        R(2) = DMAX1(ZERO,R(2))
        R(3) = DMAX1(ZERO,R(3))
        R(4) = DMAX1(ZERO,R(4))
        R(5) = DMAX1(ZERO,R(5))
        Rt  = R(1)+R(2)+R(3)+R(4)+ R(5)
        IF (RT .LT. ZERO) write(*,*) 'RT', Rt, TESTO2, TESTNIT,
     &  TESTSO4
C        write(*,811) R(1),R(2),R(3),R(4),R(5)
        DO 10 j=1,5
          R1(j)=  R(j)*RCt1
          R2(j)=  R(j)*RCt2
          R3(j)=  R(j)*RCt3
   10   CONTINUE
C
C Call an updated salclosed to get the distribution of carbonate
C alkalinity species and H+. This formulation is from Whitman and 
C Turner and is not appropriate for a sediment diagenetic model.
C Van Cappellen and Wang, (1995) has a more appropriate formulation.
C
      CALL salclosed_SDM(spc,SAL,TESTALK,TESTTC,TESTTS,TEMP,pH)
      CO2aq = spc(1)
      HCO3c = spc(2)
      CO3c  = spc(3)
      H2S   = spc(4)
      HS    = spc(5)
      H1    = spc(6)
      alphe1= spc(7)
      pH = -Dlog10(H1)
      IF (HS .LE. ZERO) THEN
         HS= ZERO
      ENDIF
      IF (HS .GE. TESTTS) HS = TESTTS
      rKCO3 = 3.65D-01
      rKH1o = 3.0D-08
      fbCO3= CO3c/(CO3c + rKCO3)
      fbH1 = H1/(H1 + rKH1o)
      omegaFES= (TESTFE2*TWO*HS)/fbH1/KpFES  ! 2x HS test
      IF (omegaFES .GT. ONE) THEN
        delta23  = ONE
        delta_23 = ZERO
      ELSE
        delta23  = ZERO    
        delta_23 = ONE
      ENDIF
C
C Rate laws from Van Cappellen and Wang (table 3)
C
      R8   = k8*TESTFE2*TESTO2
      R10  = k10*TESTMNO*TESTFE2
      R11  = rq11*TESTNH3*TESTO2* rMandy(TESTTS)
      R12  = rq12*TESTTS*TESTO2
      R13  = k13*TESTTS*TESTMNO      ! R13 in Van Cappellan
      R14  = k14*TESTTS*TESTFE3
      R15  = k15*TESTFES*TESTO2
      R23  = k23*delta23*(omegaFES-1.0)
      R_23 = k_23*delta_23*TESTFES*(1.0-omegaFES)
C
C
C  This section deals with solubility controlled reaction of the form
C  H+ + FeSx -> fe2+ +  HS-; giving Kspx= aFe2+ aHS-/aH+
C  In rearrangement and substitution of gammaC mC for aC where
C  aC is the activity of a species, mC is the molar concentration,
C  and gammaC is the activity coefficient. We assume that gamma HS-
C  is the same as that for gammaCl-
C  For the moment we assume all HS-
C
C     rKspx=1.14e-3
C     gammaFe= 0.17
C     gammaHS= 0.63
C     rmHS= TESTTS/1.0D3          ! convert to M
C     rmFE = rKspx/gammaFe/gammaHS * 10 **(-pH) /rmHS
C     rmFE =  rmFE *1.0D3       ! this is the max allowed by eq kintics
C
  811  format(5 f9.3)
C        rKmax=10.0D2             ! was 10.0D2
C        k23_1= rKmax*(TESTFE2-rmFE)   ! note change in K units
C        IF (k23_1 .LE. ZERO) k23_1=ZERO
C        R23  = k23_1*delta23*(omegaFES-1.0)
C
C  Reaction rates from TABLE 5 of Van Cappellen and Wang (1996)
C
C......................................................................
      GAM13  = (x1+TWO*y1)/x1    ! AEROBIC RESP O2/CH2O
      GAM23  = (x2+TWO*y2)/x2    ! AEROBIC RESP O2/CH2O
      GAM33  = (x3+TWO*y3)/x3    ! AEROBIC RESP O2/CH2O
      GAM14 =(FOUR*x1+THREE*y1)/FIVE/x1 ! DENITRIFICATION NO3-/ CH20
      GAM24 =(FOUR*x2+THREE*y2)/FIVE/x2 ! DENITRIFICATION NO3-/ CH20
      GAM34 =(FOUR*x3+THREE*y3)/FIVE/x3 ! DENITRIFICATION NO3-/ CH20
      TMP1= -(GAM13*R1(1)+GAM23*R2(1)+ONE/F*GAM33*R3(1))-(TWO*R15)
      RO2    = F*(TMP1)-(R8/FOUR+TWO*R11+TWO*R12)
      TMP1= F*((y1/x1*R1(1)+y2/x2*R2(1)+ONE/F*y3/x3*R3(1))
     *      -(GAM14*R1(2)+GAM24*R2(2)+ONE/F*GAM34*R3(2)))
      RNO3  = TMP1+R11
      RMN2  = F*(TWO*(R1(3)+R2(3)+R10))+TWO*R3(3)
      RFE2  = F*(FOUR*(R1(4)+R2(4))+TWO*R14+R15
     *        -R23+R_23)-R8 + FOUR*R3(4)
      RSO4  = F*(-(R1(5)+R2(5))/TWO+R15)-R3(5)/TWO+R12
      TMP1  = (y1/x1*(R1(4)+R1(5))+y2/x2*(R2(4)+R2(5)))
      RNH4  = F*TMP1+y3/x3*(R3(4)+R3(5)+R3(5))-R11
      RCH2O1= - (R1(1)+R1(2)+R1(3)+R1(4)+R1(5))
      RCH2O2 = - (R2(1)+R2(2)+R2(3)+R2(4)+R2(5))
      RDOM   = - (R3(1)+R3(2)+R3(3)+R3(4)+R2(5))
      RMNO   = - TWO*(R1(3)+R2(3)+(R3(3)/F))-R10-R13
      RFEOH3 = - FOUR*(R1(4)+R2(4)+R3(4)/F)+R8/F-TWO*R14
      RFES   = -R15+R23-R_23
      RTC    = F*(R1(1)+R1(2)+R1(3)+R1(4)+R1(5)+R2(1)+R2(2)
     *         +R1(3)+R2(4)+R2(5))
     *         +R3(1)+R3(2)+R3(3)+R3(4)+R3(5)
C      RTS    = F*((R1(5)+R2(5))/TWO-R14-(TWO*(R23+R_23))+
C     *         R3(5)/TWO-R12
      RTS    = F*((R1(5)+R2(5))/TWO-R13-R14-R23+R_23)+
     *         R3(5)/TWO-R12
      IF (TESTALK .LE. ZERO) THEN
       RALK = ZERO
      ELSE
      TMP1=-(Y1+TWO*Z1)/X1*R1(1)+(FOUR*X1+THREE*Y1-10.*Z1)/5./X1*R1(2)
     *      +(FOUR*X1+Y1-TWO*Z1)/X1*R1(3)+(EIGHT*X1+Y1-TWO*Z1)/X1*R1(4)
     *      +(X1+Y1-TWO*Z1)/X1*R1(5)
      TMP2=-(Y2+TWO*Z2)/X2*R2(1)+(FOUR*X2+THREE*Y2-10.*Z2)/5./X2*R2(2)
     *      +(FOUR*X2+Y2-TWO*Z2)/X2*R2(3)+(EIGHT*X2+Y2-TWO*Z2)/X2*R2(4)
     *      +(X2+Y2-TWO*Z2)/X2*R2(5)
      TMP3=-(Y3+TWO*Z3)/X3*R3(1)+(FOUR*X3+THREE*Y3-10.*Z3)/5./X3*R3(2)
     *       +(FOUR*X3+Y3-TWO*Z3)/X3*R3(3)+(EIGHT*X3+Y3-TWO*Z3)/X3*R3(4)
     *       +(X3+Y3-TWO*Z3)/X3*R3(5)
      TMP4   = TMP1+TMP2+TMP3/F+TWO*R13+FOUR*R14
     *       -TWO*R23+TWO*R_23
      RALK   = F*TMP4-TWO*R8-TWO*R11-TWO*R12
      IF (TESTALK .LE. ZERO) RALK = ZERO
      ENDIF
C    Set the reaction rates in a vector 
C
C       Rc(1)=  ZERO
C       Rc(2)=  ZERO
C        Rc(3)=  ZERO
C       Rc(4)=  ZERO
C       Rc(5)=  ZERO
C       Rc(6)=  ZERO
C       Rc(7)=  ZERO
C       Rc(8)=  ZERO
C       Rc(9)=  ZERO
C       Rc(10)= ZERO
C       Rc(11)= ZERO
C       Rc(12)= ZERO
C       Rc(13)= ZERO
C       Rc(14)= ZERO
C       Rc(15)= ZERO
        Rc(1)=   RCH2O1
        Rc(2)=   RCH2O2
        Rc(3)=   RDOM
        Rc(4)=   RO2
        Rc(5)=   RNO3
        Rc(6)=   RNH4
        Rc(7)=   RMNO
        Rc(8)=   RMN2
        Rc(9)=   RFEOH3
        Rc(10)=  RFE2
        Rc(11)=  RSO4
        Rc(12)=  RTS
        Rc(13)=  RFES
        Rc(14)=  RTC
        Rc(15)=  RALK
      RETURN
      END
C ----------------------------------------------------------------
C CARBON
C PURPOSE: CLOSED SYSTEM CALCULATIONS
C ----------------------------------------------------------------
C
      SUBROUTINE salclosed_SDM(spc,s,ALK,TC,TS,t,pH)
      IMPLICIT REAL*8 (A-Z)
      DIMENSION rk(9),spc(9)
      CALL thermowater(rK,s,t)
      rK1=rK(4)
      rK2=rK(6)
      rK1s=rK(9)
C pH       
      H1 = 10.**(-pH)
      alphe0= 1.0/(1+rK1/H1 + rK1*rK2/H1**2)
      alphe1= 1.0/(H1/rK1 + 1 +rK2/H1)
      alphe2= 1.0/(H1**2/(rK1*rK2) + H1/rK2 +1)
      alpheS1 = 1.0/(1.0 + H1/rK1s)
      beta1=alphe1*TC/(alphe1*TC+2.0*alphe2*TC + alpheS1*TS)
      beta2=alphe2*TC/((alphe1*TC+2.0*alphe2*TC) + alpheS1*TS)
      beta1S=alpheS1*TS/(alphe1*TC+2.0*alphe2*TC+alpheS1*TS)
C      
      hco3c  = TC * alphe1
      co3c   = TC * alphe2
      h2co3c = TC * alphe0
      ALKc = ALK*(1.0 - beta1S)
      IF (TS .LE. 0.0D+00) THEN
           h2s = 0.0D+00
           hs  = 0.0D+00 
      ELSE
           h2s   = TS/(1.0 + rk1s/H1)
           hs    = TS/(1.0 + H1/rk1s)
      ENDIF

C
      spc(1) = h2co3c 
      spc(2) = hco3c
      spc(3) = co3c
      spc(4) = h2s
      spc(5) = hs
      spc(6) = h1
      spc(7) = alphe1
      RETURN
      END

!
! ------------------------------------------------------------------
! PURPOSE: Provides all the equilibrium constants for CO2 sytem
!          These are temperature and salinity dependent from
!          Whitfield and Turner (1986)
! -----------------------------------------------------------------
      SUBROUTINE thermowater(rk,s,tz)
      IMPLICIT none
      INTEGER i,j
      REAL*8 a0, a1, a2, b0,s,rk,rk1s,tz,t
      DIMENSION a0(8), a1(8), a2(8), b0(8),rk(9)
      DATA (a0(i),i=1,8) /290.9097,207.6548,148.0248,0.0221,
     * .5709, 0.9805, 1.4853, 0.5998/
      DATA (a1(i),i=1,8) /14554.21, 11843.79, 8966.9, 34.02,
     * -84.25,-92.65,-192.69,-75.25/
      DATA (a2(i),i=1,8) / 45.0575, 33.6485, 24.4344,5*0.0/
      DATA (b0(i),i=1,8) /0,0,0,0,-1.632,-3.294,-5.058,-1.767/
      IF (s .LE. 0.0) THEN
         s=0.0
      ENDIF

      t = tz+ 273.16     ! absolute temperature

! Temperature dependence of the thermodynamic stability
      DO i=1,3
         rk(i) = exp(a0(i) - a1(i)/t - a2(i)*log(t))  !Ko(i)
      ENDDO
! Sal. and temp. dependence of the stability constant K1
      i=1
      DO j=4,5
         rk(j)= exp(log(rk(i)) + (a0(j) + a1(j)/t +
     *   a2(j)*log(t))*SQRT(s) + b0(j)*s/100.0)
      ENDDO
      i=2  
      DO j=6,7
         rk(j)= exp(log(rk(i)) + (a0(j) + a1(j)/t +
     *   a2(j)*log(t))*SQRT(s) + b0(j)*s/100.0)
      ENDDO
! Salinity and temperature dependence of K0
       rk(8) = exp(-58.0931+90.5069*(100/t)+22.2940*log(t/100.0) +
     *(0.027766 - 0.025888*(t/100.0) + 0.0050578*(t/100.0)**2)*s)
 
	rk1s=2.527+1359.96/t-0.206*s**(1.0/3.0)
	rk1s=10.0**(-rk1s)
       rk(9)= rk1s
       RETURN
       END

!   NITRATE    Calculates the Monod type function and feedbacks for
!   NO3- as an electron acceptor.


      DOUBLE PRECISION FUNCTION RNITRATE_SDM(O20)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 KG1,KG2,KDOM, KPO2
      REAL*8 KO2, KNO3, KMNO,  KFE3,  KSO4
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/
      DATA TEN/1.0D+01/
C
       KPO2= KO2/HUN
       PO2=O20
       IF (PO2 .LT. ZERO)  PO2 = ZERO
       RNITRATE_SDM=KPO2/(KPO2 + PO2)
C
      RETURN
      END
C
C   RMANGANESE    Calculates the Monod type function and feedbacks for
C   FE+3 as an electron acceptor.
C 
C
C
      DOUBLE PRECISION FUNCTION RMANGANESE(O20,NO30)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 NO30
      REAL*8 KG1,KG2,KDOM
      REAL*8 KPO2,KPNO3
      REAL*8 KO2, KNO3, KMNO,  KFE3,  KSO4
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/
      DATA TEN/1.0D+01/,TWENTY/2.0D+01/,FIFTY/3.0D+02/
C
      KPO2= KO2/FIFTY
      KPNO3= KNO3/FIFTY
      PO2=O20
      IF (PO2 .LT. ZERO)  PO2 = ZERO
      PNO3=NO30
      IF (PNO3 .LT. ZERO)  PNO3 = ZERO
      RMANGANESE=(KPO2/(KPO2+PO2))*(KPNO3/(KPNO3+PNO3))
C
      RETURN
      END
C
C
C
C   RFERRIC    Calculates the Monod type function and feedbacks for
C   FE+3 as an electron acceptor.
C 
C
C
      DOUBLE PRECISION FUNCTION RFERRIC(O20,NO30,MNO)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 NO30,MNO
      REAL*8 KG1,KG2,KDOM
      REAL*8 KO2, KNO3, KMNO,  KFE3,  KSO4
      REAL*8 KPO2,KPNO3,KPMNO
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.5D+02/
      DATA TEN/1.0D+01/,FIFTY/5.0D+01/,SEVENTY/7.0D+1/
      DATA FIVEHUN/5.0D+02/
C     was fifty
      KPO2= KO2/HUN
      KPNO3= KNO3/HUN
      KPMNO= KMNO/HUN

      PO2=O20
      IF (PO2 .LT. ZERO)  PO2 = ZERO
      PNO3=NO30
      IF (PNO3 .LT. ZERO)  PNO3 = ZERO
      PMNO=MNO
      IF (PMNO .LT. ZERO)  PMNO = ZERO
      RFERRIC=(KPO2/(KPO2+PO2))*(KPNO3/(KPNO3+PNO3))*
     # (KPMNO/(KPMNO+PMNO))
C
      RETURN
      END
C
C
C
C   SULFATE    Calculates the Monod type function and feedbacks for
C   SO4-- as an electron acceptor.
C
C
C
      DOUBLE PRECISION FUNCTION RSULFATE(O20,NO30,MNO,FE30)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 KG1,KG2,KDOM,MNO
      REAL*8 KPO2,KPNO3,KPMNO,KPFE3, NO30
      REAL*8 KO2, KNO3, KMNO, KFE3, KSO4
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/
      DATA TEN/1.0D+01/,TWENTY/2.0D+01/,THIRTY/3.0D+01/
C      was twenty
      KPO2= KO2/THIRTY
      KPNO3= KNO3/THIRTY
      KPMNO= KMNO/THIRTY
      KPFE3= KFE3/THIRTY
C     KPO2= KO2
C     KPNO3= KNO3
C     KPFE3= KFE3
      PO2=O20
      IF (PO2 .LT. ZERO)  PO2 = ZERO
      PNO3=NO30
      IF (PNO3 .LT. ZERO) PNO3 = ZERO
      PMNO=MNO
      IF (PMNO .LT. ZERO)  PMNO = ZERO
      PFE3= FE30
      IF (PFE3 .LT. ZERO) PFE3 = ZERO
      RSULFATE=(KPO2/(KPO2+PO2))*(KPNO3/(KPNO3+PNO3))*
     # (KPMNO/(KPMNO+PMNO)) * (KPFE3/(KPFE3+PFE3))
C
      RETURN
      END
C
C   FUNCTION: MANDY after guess who
C   Sulfide inhibition TO nitrification. The result is a reduction
C   in the amount of both NO3 and subsequently denitrification IN the
C   presence of high Sulfide concentrations. Calculates the Monod type
C   feedback function.
C
C
      DOUBLE PRECISION FUNCTION rMandy(HS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      REAL*8 KPSO4
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      DATA ZERO/0.0D+00/,HUN/1.0D+02/
C
       KPSO4= KSO4/HUN
       PHS=HS
       IF (PHS .LT. ZERO)  PHS = ZERO
       rMandy=KPSO4/(KPSO4 +  PHS)
C
      RETURN
      END
C
C
C
C
C
C
C
C  SED  A subroutine for the sediment properties.
C       Calculates the value of the depth-dependent porosity (P),
C       derivative of the porosity with depth (DPDX), and the
C       burial velocity (W) at a supplied depth.
C
C     P = porosity at depth X
C     DPDX = spatial derivative of porosity at depth X
C     W = solid burial velocity at depth X
C     U = porewater advection velocity at depth X
C
C
      SUBROUTINE SED(X,P,DPDX,U,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /POROS/ P0,P00,BP
      COMMON /ADVEC/ W00
      DATA ZERO/0.0D+00/,ONE/1.0D+00/
C
      IF(BP.EQ.ZERO.OR.(P0-P00).EQ.ZERO) THEN
           P = P0
           DPDX = ZERO
      ELSE
           P = (P0-P00)*DEXP(-BP*X) + P00
           DPDX = -BP*(P0-P00)*DEXP(-BP*X)
      ENDIF
C
      PS = ONE - P
      PS00 = ONE - P00
      W = W00*PS00/PS
      U = W00*P00/P
      RETURN
      END


!   TORT2 Calculates the square of the tortuosity at an input depth X


!         Currently it uses the "universal" correlation from
!         Boudreau (1996, GCA, V. 60).  This can be replaced with any
!         desired functionality.
!
!         T2 = the square of the tortuosity at depth X (as it appears
!              in Berner's(1980) version of the diagenetic equations.
!         DT2DX = spatial derivative of T2 at depth X.


      SUBROUTINE TORT2(T2,DT2DX,P,DPDX,X)
      IMPLICIT REAL*8 (A-H,O-Z)
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,
     #     FOUR/4.0D+00/

      T2 = ONE - TWO*DLOG(P)
      DT2DX = - FOUR/P*DPDX

      RETURN
      END



!   DB	Contains the expressions for the depth-dependent
!		mixing coefficient


        DOUBLE PRECISION FUNCTION DB(X)
        IMPLICIT REAL*8 (A-H,O-Z)
        COMMON /MIX/ X1,X2
        COMMON / BIOT/ Db0

      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/
C
      IF(X.LE.X1) DB = DB0
      IF(X.GT.X1.AND.X.LT.X2) DB = DB0*(X2-X)/(X2-X1)
      IF(X.GE.X2) DB = ZERO
C
      RETURN
      END
C
C
C
C
C   DDB	Contains the depth derivatives of DB(X)
C
C
      DOUBLE PRECISION FUNCTION DDB(X)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /MIX/ X1,X2
        COMMON /BIOT/ Db0

      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/
C
      IF(X.LE.X1) DDB = ZERO
      IF(X.GT.X1.AND.X.LT.X2) DDB = -DB0/(X2-X1)
      IF(X.GE.X2) DDB = ZERO
C
      RETURN
      END




!   SIG	Calculates the weighting for the finite difference
!		approximation for the advective term (see Boudreau,
!		1986, Amer. J. Sci., v. 286, p.192)

      DOUBLE PRECISION FUNCTION SIG(X,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /DEPTH/ XL,DH
      DATA ZERO/0.0D+00/,ONE/1.0D+00/,TWO/2.0D+00/,HUN/1.0D+02/

      D = DB(X)
      IF(D.NE.ZERO) THEN
      E = W*DH/D/TWO
      IF(E.NE.ZERO) SIG = ONE/DTANH(E) - ONE/E
      IF(E.EQ.ZERO) SIG = ZERO
      RETURN
      ENDIF
      IF(D.EQ.ZERO) SIG = ONE
      RETURN
      END



!   FILL_Y Parses output vector for each species
!
!***********************************************************************

      SUBROUTINE FILL_Y(NEQ,np,nss,Y,G1,G2,O2,rNO3,rNH4,rMN2, 
     &                  FE3,FE2,SO4,HS,FES,TC,ALK,DOM,Os,Ob)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MAXNEQ=27000)
      DIMENSION Y(NEQ+2)
      DIMENSION G1(*),G2(*),O2(*),rNO3(*),rNH4(*),rMN2(*)
      DIMENSION SO4(*),HS(*),FE3(*),FE2(*),FES(*),TC(*),ALK(*)
      DIMENSION DOM(*), Os(*), Ob(*)
      COMMON /SPECIES/ NS,NPOINTS
C
      NPM1 = NPOINTS - 1
C
C  Following statements gives the # of midpoints NPOINTS -1
C     
      DO  I=2,NPM1
       M = I*NS              ! starts at 18 for 9 species

C
C  Provide index for the the finite difference scheme
C
       MID1  = M + 1         ! G1mid
       MID2  = M + 2         ! G2mid
       MID3  = M - 14        ! Omid
       MID4  = M - 13        ! NO3mid
       MID5  = M - 12        ! NH3mid
       MID6  = M - 11        ! MNOmid
       MID7  = M - 10        ! MN2mid
       MID8  = M - 9         ! FE3mid
       MID9  = M - 8         ! FE2mid
       MID10 = M - 7         ! SO4mid
       MID11 = M - 6         ! TSmid
       MID12 = M - 5         ! FESmid
       MID13 = M - 4         ! TCmid
       MID14 = M - 3         ! ALKmid
       MID15 = M - 2         ! DOMmid
       MID16 = M - 1         ! Oamid
       MID17 = M             ! Obmid


       G1(I) =   Y(MID1) * 1000.
       G2(I) =   Y(MID2) * 1000.
       O2(I) =   Y(MID3) * 1000.
       rNO3(I)=  Y(MID4) * 1000.
       rNH4(I) = Y(MID5) * 1000.
       rMN2(I)=  Y(MID7) * 1000.
       FE2(I) =  Y(MID9) * 1000.
       SO4(I) =  Y(MID10) * 1000.
       HS(I) =   Y(MID11) * 1000.
       FES(I) =  Y(MID12) * 1000.
       TC(I) =   Y(MID13) * 1000.
       ALK(I) =  Y(MID14) * 1000.
       DOM(I) =  Y(MID15) * 1000.
       Os(I)  =  Y(MID16)
       Ob(I)  =  Y(MID17)
      END DO
      RETURN
      END
C
C
C
C   ROOTINT Integrates and averages results over rootzone
C
      SUBROUTINE ROOTINT(NPOINTS,G1,G2,O2,rNO3,rNH4,
     *       rMN2,FE2,SO4,HS,FES,TC,ALK,DOM,Os,Ob,
     * tempG1,tempG2,tempO2,tempNO3,tempNH4,tempMN2,
     * tempFE2,tempSO4,tempHS,tempFES,tempTC,tempALK,tempDOM,
     * tempOs,tempOb)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION G1(*),G2(*),O2(*),rNO3(*),rNH4(*),rMN2(*)
      DIMENSION FE2(*),SO4(*),HS(*),FES(*),TC(*),ALK(*),DOM(*)
      DIMENSION Os(*),Ob(*)
      COMMON /SROOT/ ZROOT, WROOT
      COMMON /DEPTH/ XL,DH
      DATA ZERO/0.0D+00/
C
C
      NROOT  =  INT(ZROOT/DH)
      NHALFR =  INT(WROOT/2./DH)
      NLOW   =  NROOT - NHALFR
      NHI    =  NROOT + NHALFR
      NDIF   =  NHI   - NLOW
      tempG1 =  ZERO
      tempG2 =  ZERO
      tempO2 =  ZERO
      tempNO3 = ZERO
      tempNH4 = ZERO
      tempMN2 = ZERO
      tempFE2 = ZERO
      tempFES = ZERO
      tempSO4 = ZERO
      tempHS =  ZERO
      tempTC =  ZERO
      tempALK = ZERO
      tempDOM = ZERO
      tempOs  = ZERO
      tempOb  = ZERO
      DO I=NLOW,NHI
          tempG1 =  tempG1+G1(I)
          tempG2 =  tempG2+G2(I)
          tempO2 =  tempO2+O2(I)
          tempNO3 = tempNO3+rNO3(I)
          tempNH4 = tempNH4+rNH4(I)
          tempMN2 = tempMN2+rMN2(I)
          tempSO4 = tempSO4+SO4(I)
          tempHS =  tempHS+HS(I)
          tempFE2 = tempFE2+FE2(I)
          tempFES = tempFES+FES(I)
          tempTC =  tempTC+TC(I)
          tempALK = tempALK+ALK(I)
          tempDOM = tempDOM+DOM(I)
          tempOs  = tempOs + Os(I)
          tempOb  = tempOb + Ob(I)
       END DO
       tempG1  = tempG1/FLOAT(NDIF)
       tempG2  = tempG2/FLOAT(NDIF)
       tempO2  = tempO2/FLOAT(NDIF)
       tempNO3 = tempNO3/FLOAT(NDIF)
       tempNH4 = tempNH4/FLOAT(NDIF)
       tempMN2 = tempMN2/FLOAT(NDIF)
       tempFE2 = tempFE2/FLOAT(NDIF)
       tempSO4 = tempSO4/FLOAT(NDIF)
       tempHS  = tempHS/FLOAT(NDIF)
       tempFES = tempFES/FLOAT(NDIF)
       tempTC  = tempTC/FLOAT(NDIF)
       tempALK = tempALK/FLOAT(NDIF)
       tempDOM = tempDOM/FLOAT(NDIF)
       tempOs  = tempOs/FLOAT(NDIF)
       tempOb  = tempOb/FLOAT(NDIF)
       RETURN
       END


C
C  OUTVEC Produces output for the the MATLAB program "rootzone"
C
C     -----------------------------------------------------------------
C     FORMAT FOR MATLAB FILE
C     -----------------------------------------------------------------
      SUBROUTINE OUTVEC(rootG1,rootG2,rootO2,rootNO3,rootNH4,rootMN2,
     * rootFE2,rootSO4,rootHS,rootFES,rootTC,rootALK,rootDOM,
     * rootOs,rootOb,mm)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION rootG1(*),rootG2(*),rootO2(*),rootNO3(*),rootNH4(*),
     * rootMN2(*),rootFE2(*),rootSO4(*),rootHS(*),rootFES(*),rootTC(*),
     * rootALK(*),rootDOM(*),rootOs(*),rootOb(*)

C
      OPEN (12,FILE='rootG1.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootG1(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootG2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootG2(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootO2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootO2(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootNO3.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootNO3(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootNH4.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootNH4(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootMN2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootMN2(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootFE2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootFE2(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootSO4.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootSO4(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootHS.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootHS(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootFES.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootFES(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootTC.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootTC(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootALK.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootALK(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootDOM.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootDOM(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootOs.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootOs(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='rootOb.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (rootOb(i),i=1,mm)
      CLOSE(12)
      END



!  OUTFLUX  Produces output of interfacial fluxes for  MATLAB program "rootzone"
!
!     -----------------------------------------------------------------
!     FORMAT FOR MATLAB FILE
!     -----------------------------------------------------------------
      SUBROUTINE OUTFLUX(sedO2,sedNO3,sedNH4,sedSO4,sedDIC,sedDOC,
     * sedOM1,sedOM2, pycoO2,mm)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION sedO2(*),sedNO3(*),sedNH4(*),sedSO4(*),sedDIC(*),
     * sedDOC(*),sedOM1(*),sedOM2(*),pycoO2(*)

C
      OPEN (12,FILE='sedO2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedO2(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='sedNO3.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedNO3(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='sedNH4.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedNH4(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='sedSO4.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedSO4(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='sedDIC.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedDIC(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='pyco2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (pycoO2(i),i=1,mm)
      CLOSE(12)
!L3 Modification: Not outputting OMs...why?  Add here:  (1/12/2015)
      OPEN (12,FILE='sedOM1.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedOM1(i),i=1,mm)
      CLOSE(12)
      OPEN (12,FILE='sedOM2.dat',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(12,"(E12.4)") (sedOM2(i),i=1,mm)
      CLOSE(12)
!L3 End modification
      END

!    ----- PROCEDURE provides input for diagenetic model

        subroutine datain(A,mrow)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION A(100)
        character*20 cdum1
        character*35 cdum2
        open(11,file='SDM/hypox_input.csv')
!       first, skip over first five lines
        read(11,*)
        read(11,*)
        read(11,*)
        read(11,*)
        read(11,*)
        write(*,*)'mrow=',mrow
        do ilin=1,mrow
         read(11,*)dum1,cdum1,cdum2,cdum1, A(ilin)
          write(*,*)'i=',ilin,cdum2,'A=',A(ilin)
        enddo
        close(11)
      return
      END



!***************************************************************
!
!     SUBROUTINE FOR ENTERING DATA FROM A FILE.
!     (ABB 16th April 1996)
!
!***************************************************************

      SUBROUTINE FILEDATA2(RPAR,Ainp)

      IMPLICIT REAL*8 (A-H,O-Z)

C
      PARAMETER (MAXNEQ=27000)
      DIMENSION RPAR(MAXNEQ)
C
      REAL*8 KG1,KG2,KDOM,NO30,NH30,KANH4
      REAL*8 KO2, KNO3, KMNO,KFE3,KSO4
      REAL*8 MNO0,MN20,NH3I
      REAL*8 k8,k10,k11,k12,k13,k14
      REAL*8 k15,k23,k_23,KpFES

      DIMENSION Ainp(100)
      DIMENSION :: DF(24),rk(9), ppH(2000),YY(MAXNEQ)
      DIMENSION day(1100),gulfdo(1100)
      COMMON /SPECIES/ NS,NPOINTS
      COMMON /GRIDPOINT/ NEQ
      COMMON /ADVEC/ W00
      COMMON /DEPTH/ XL,DH
      COMMON /TIMING/ T,TOUT,TOUT1,TOUT2,TOUT3,TOUT4,TOUT5,TOUT6
      COMMON /MIX/ X1,X2
      COMMON /BIOT/ Db0
      COMMON /CONC/ DO20,NO30,NH30,SO40,HS0,NH3I
      COMMON /CONC2/ MNO0,MN20,FE30,FE20,FES0,TC0,ALK0,DOM0,DOMI
      COMMON /GULF/ gulfdo,day
      COMMON /DIFF/ DO2,DNO3,DNH3,DMN2,DFE2,DSO4,DHS,DTC,DALK
      COMMON /FLUXCOMMON/ FG1,FG2
      COMMON /POROS/ P0,P00,BP
      COMMON /SROOT/ ZROOT, WROOT
      COMMON /IRRIG/ ALPHA0,XIRRIG
      COMMON /KINETICS/ KG1,KG2,KDOM,KO2,KNO3,KMNO,KFE3,KSO4
      COMMON /CONSTANT2/ KpFES
      COMMON /CONSTANT3/ k8,k10,k11,k12,k13,k14,k15,k23,k_23
      COMMON /DISSOL/ a, PER_DIS
      COMMON /STOIC/ SC1,SN1,SP1,SC2,SN2,SP2,SC3,SN3,SP3
      COMMON /TEMPERATURE/ TEMP, SAL, PRESS,pH0
      COMMON /GETY/ YY
      COMMON /GETPH/ ppH
      COMMON /DESORB/ KANH4
      COMMON /hydro/ aa, bb, gg, zz

!       COMMON /TIMES/ T0,TL
!       COMMON /DIN/ rKn, NH3I, NO3I
!       COMMON /DEPDAY/ Nday, dep_mud


C
      DATA ZERO/0.0D+00/,HUN/1.0D+02/
      DATA YEAR/3.156D+07/,THOU/1.0D+03/
      NSPECIES = NS
      yr_sec   = 1/(3600*24*365)
      rho = 2.65

C

!     INPUT TEMP (oC), SALINITY AND PRESSURE (atm)

       TEMP= Ainp(1)
       SAL= Ainp(2)
       PRESS= Ainp(3)
       pH0= Ainp(4)

      CALL DDIFCOEF(V,DF,SAL,TEMP,PRESS)
      DO2  = DF(2)*YEAR
      DNO3 = DF(12)*YEAR
      DMN2= DF(22)*YEAR
      DNH3 = DF(10)*YEAR
      DSO4 = DF(23)*YEAR
      DHS  = DF(5)*YEAR
      DFE2 = DF(21)*YEAR
      DTC  = DF(8)*YEAR*1.5D00
      DALK = DF(8)*YEAR*1.5D00

!  Surface value of Bioturbation

       DB0= Ainp(5)

!  DEPTH FOR START OF DB DECREASE
       X1 = Ainp(6)

!  DEPTH FOR END OF DB DECREASE TO ZERO
       X2 = Ainp(7)

!  RATE CONSTANT FOR ORGANIC MATTER 1) DECAY
       KG1 = Ainp(8)
       KG2 = Ainp(9)
       KDOM= Ainp(10)

!  Conditional Equilibrium constant (unitless)
       a = Ainp(11)
       PER_DIS = Ainp(12)

!  Conditional Equilibrium constant (unitless)
        KpFES = Ainp(13)

!  Rate constants (T-6, Van Cap & Wang 1996 7-13)
!  (M-1 yr-1)
      k8  = Ainp(14)
      k10 = Ainp(15)
      k11 = Ainp(16)
      k12 = Ainp(17)
      k13 = Ainp(18)

! Rate constants (T-6, Van Cap & Wang 1996 14-_21)
! (M-1 yr-1 conv to (umolcm-3)-1 yr-1)
      k14 = Ainp(19)
      k15 = Ainp(20)


! Rate constants (T-6, Van Cap & Wang 1996 22-13)
      k23  = Ainp(21)
      k_23 = Ainp(22)

! STOICHIOMETRY OF THE ORGANIC MATTER OM1:
! NUMBER OF CARBONS, NITROGEN PHOSPHORUS ATOMS
      SC1 = Ainp(23)
      SN1 = Ainp(24)
      SP1 = Ainp(25)

! STOICHIOMETRY OF THE ORGANIC MATTER OM2:
! NUMBER OF CARBONS, NITROGEN, PHOSPHORUS ATOMS
      SC2 = Ainp(26)
      SN2 = Ainp(27)
      SP2 = Ainp(28)

! STOICHIOMETRY OF THE ORGANIC MATTER DOM:
! NUMBER OF CARBONS, NITROGEN, PHOSPHORUS ATOMS
      SC3 = Ainp(29)
      SN3 = Ainp(30)
      SP3 = Ainp(31)

!     MONOD CONSTANT FOR O2, NO3-, KMnO, S042-, FE3, MN4+
      KO2  = Ainp(32)
      KNO3 = Ainp(33)
      KMNO = Ainp(34)
      KFE3 = Ainp(35)
      KSO4 = Ainp(36)

!     SET UP IRRIGATION: DEFAULT IS NO IRRIGATION
      IRRG   = 0
!     IS THERE IRRIGATION (INTEGER):YES = 1  NO=0
      IRRG = INT(Ainp(37))
      ALPHA0 = Ainp(38)
      XIRRIG = Ainp(39)

!     ADVECTIVE VELOCITY
      W00 = Ainp(40)

!     POROSITY AND TORTUOSITY CORRECTION
      P0 = Ainp(41)
      P00 = Ainp(42)
      BP = Ainp(43)


!     PARAMETERS NEEDED FOR THE NUMERICS
!     NUMBER OF POINTS NEEDED (UP TO 400) INTEGER
      NPOINTS = Ainp(44)
      NEQ = NPOINTS*NSPECIES+2

!     MAXIMUM DEPTH FOR CALCULATIONS
      XL = Ainp(45)
      DH = XL/FLOAT(NPOINTS)
      DO 86 I=1,NPOINTS
         RPAR(I) = FLOAT(I)*DH
 86   CONTINUE

!     END TIME FOR THE CALCULATIONS (4000.)
      T0 = ZERO
      TL = Ainp(46)

!   CONCENTRATION O2 NO3 NH3 S04 HS- FE3+ FE2+ FES MN4+ MN2+
      DO20  = Ainp(47)/THOU
      NO30 = Ainp(48)/THOU
      NH30 = Ainp(49)/THOU
      SO40 = Ainp(50)/THOU
      HS0  = Ainp(51)/THOU
      MNO0 = Ainp(52)/THOU
      MN20 = Ainp(53)/THOU

      FE30 = Ainp(54)/THOU
      FE20 = Ainp(55)/THOU
      FES0 = Ainp(56)/THOU
      DOM0 = Ainp(57)/THOU
      TC0  = Ainp(58)/THOU

!     OVER LYING WATER CONCENTRATION SEEN BY IRRIGATORS
      DOMI = Ainp(59)/THOU
      NH3I = Ainp(60)/THOU
!     NO3I = Ainp(56)/THOU

!     TYPE OF BOUNDARY CONDITION AT X=0
!       KNOWN CONCENTRATION (1)
!       PRESCRIBED FLUX     (2)
      IFG = INT(Ainp(61))

!     FOR FLUX BOUNDARY CONDITIONS ON THE ORGANIC MATTER
      IF(IFG.EQ.2) THEN
!     INTERFACIAL FLUX OM1, OM2
       FG1 = Ainp(62)*365./10000.*1000./12 ! from mg C m-2 d >umol C cm-2 y-1
       FG2 = Ainp(63)*365./10000.*1000./12 ! from mg C m-2 d >umol C cm-2 y-1 
      ENDIF

!    SWICH options: USE initial guess 1
      KANH4 = (Ainp(64))

      YY = YY_init     !YY_init and ppH_init read in Flux.F90
      ppH = ppH_init

      CALL thermowater(rK,SAL,TEMP)
      rK1=rK(4)
      rK2=rK(6)
      phl = 10.**(-pH0)
      alphe0= 1.0/(1+rK1/phl + rK1*rK2/phl**2)
      alphe1= 1.0/(phl/rK1 + 1 +rK2/phl)
      alphe2= 1.0/(phl**2/(rK1*rK2) + phl/rK2 +1)
C
      hco3c  = TC0 * alphe1
      co3c   = TC0 * alphe2
      h2co3c = TC0 * alphe0
C
      ALK0 = hco3c + 2.*co3c
C

      RETURN
      END


