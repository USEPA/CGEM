Module Sediment_Diagenesis_Routines
    USE SDM, ONLY: NS, NEQ, NPOINTS

    IMPLICIT NONE
    SAVE

    REAL(kind=8) :: ZROOT, WROOT
    REAL(kind=8) :: TEMP, SAL, PRESS, pH0
    REAL(kind=8), DIMENSION(27000) :: RATE
    REAL(kind=8), DIMENSION(2000) :: pH
    REAL(kind=8), DIMENSION(1100) :: gulfdo, day
    REAL(kind=8), DIMENSION(10) :: FLUXES
    REAL(kind=8), DIMENSION(1500) :: rIRRO2, rIRRTC, rIRRNO, rIRRNH
    REAL(kind=8), DIMENSION(1500) :: rIRRSO, rIRRALK, rIRRDOM

    REAL(kind=8) :: XL, DH
    REAL(kind=8) :: X1, X2
    REAL(kind=8) :: Db0, W00
    REAL(kind=8) :: DO20, NO30, NH30, SO40, HS0, NH3I
    REAL(kind=8) :: KANH4
    REAL(kind=8) :: MNO0, MN20, FE30, FE20, FES0, TC0, ALK0, DOM0, DOMI
    REAL(kind=8) :: DO2, DNO3, DNH3, DMN2, DFE2, DSO4, DHS, DTC, DALK
    REAL(kind=8) :: FG1, FG2  
    REAL(kind=8) :: a, PER_DIS
    REAL(kind=8) :: ALPHAA, XIRRIG
    REAL(kind=8) :: ALPHA0

    REAL(kind=8) :: KpFES
    REAL(kind=8) :: k8, k10, k11, k12, k13, k14, k15, k23, k_23
    REAL(kind=8) :: KG1, KG2, KDOM, KO2, KNO3, KMNO, KFE3, KSO4
    REAL(kind=8) :: SC1, SN1, SP1, SC2, SN2, SP2, SC3, SN3, SP3

    REAL(kind=8) :: P0, P00, BP

    INTEGER :: ICF, INIT, IPUP, JCUR, JSTART, JSV, KFLAG, KUTH
    INTEGER :: L, LMAX, LYH, LEWT, LACOR, LSAVF, LWM, LIWM
    INTEGER :: LOCJS, MAXORD, METH, MITER, MSBJ, MXHNIL, MXSTEP
    INTEGER :: N, NEWH, NEWQ, NHNIL, NQ, NQNYH, NQWAIT, NSLJ
    INTEGER :: NSLP, NYH
    INTEGER :: IVOD1(33), IVOD2(8)
    INTEGER :: NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST

    REAL(kind=8), PARAMETER, PRIVATE :: ZERO = 0.0D+00, HALF = 0.5D0 
    REAL(kind=8), PARAMETER, PRIVATE :: ONE = 1.0D+00, TWO = 2.0D+00
    REAL(kind=8), PARAMETER, PRIVATE :: THREE = 3.0D+00, FOUR = 4.0D+00
    REAL(kind=8), PARAMETER, PRIVATE :: FIVE = 5.0D+00, SIX = 6.0D0
    REAL(kind=8), PARAMETER, PRIVATE :: EIGHT = 8.0D+00, TEN = 1.0D+01 
    REAL(kind=8), PARAMETER, PRIVATE :: TWENTY = 2.0D+01, THIRTY = 3.0D+02
    REAL(kind=8), PARAMETER, PRIVATE :: FIFTY = 5.0D+01, SEVENTY = 7.0D+1
    REAL(kind=8), PARAMETER, PRIVATE :: HUN = 1.0D+02, FIVEHUN = 5.0D+02
    REAL(kind=8), PARAMETER, PRIVATE :: THOU = 1000.0D0
    REAL(kind=8), PARAMETER, PRIVATE  :: PT1 = 0.1D0, PT2 = 0.2D0
    REAL(kind=8), PRIVATE :: HU

    REAL(kind=8) :: ACNRM, CCMXJ, CONP, CRATE, DRC, EL(13)
    REAL(kind=8) :: ETA, ETAMAX, H, HMIN, HMXI, HNEW, HSCAL, PRL1
    REAL(kind=8) :: RC, RL1, TAU(13), TQ(5), TN, UROUND
    REAL(kind=8) :: RVOD1(48), RVOD2(1)


contains

!
!-----------------------------------------------------------------------
!  SUBROUTINE CASES: provides initial conditions from a steady state
!  model than at each designated output time provides a new input vector
!-----------------------------------------------------------------------
!
!
      SUBROUTINE CASES(value1,value2,Y,ppH,ISTATE,IFLAG,Ainp)
      USE SDM, ONLY: NS, NEQ, NPOINTS
      IMPLICIT NONE
      INTEGER :: ITASK, ISTATE, IOPT, ITOL, IPAR
      INTEGER :: MF, I, IFLAG
      INTEGER, PARAMETER :: MAXNEQ = 27000
      INTEGER, PARAMETER :: NSPECIES = 17
      INTEGER, PARAMETER :: NCOMPART = 17
      INTEGER, PARAMETER :: MXSTEP = 30000
      INTEGER, PARAMETER :: MU = NCOMPART
      INTEGER, PARAMETER :: ML = MU
      INTEGER, PARAMETER :: LRW = 22 + 11*MAXNEQ + (3*ML + 2*MU)*MAXNEQ
      INTEGER, PARAMETER :: LIW = 30 + MAXNEQ
      INTEGER, DIMENSION(LIW) :: IWORK
      REAL(kind=8) :: Y(NEQ), RWORK(LRW), RPAR(MAXNEQ)
      REAL(kind=8) :: RTOL(MAXNEQ), ATOL(MAXNEQ)
      REAL(kind=8) :: ppH(NPOINTS)
      REAL(kind=8) :: Ainp(100)
      REAL(kind=8) :: value1, value2, T, TOUT

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

      !write(6,*) "In CASES, Y1=",Y(1)

!      IF (IFLAG .EQ. 1) THEN

!       This sets some values according to Ainp, so always do this
        CALL FILEDATA2(RPAR, Ainp)


!   Y comes into the subroutine initialized at t=0 in Flux_CGEM, and Y goes out

!        DO I=1, NEQ
!           Y(I)= YY(I)
!        ENDDO
!        IFLAG = 0
!        write(*,*) 'NPOINTS',NPOINTS
!        write(*,*) 'NEQ', NEQ
!      ENDIF


!
! -------- provide new T and tout
!

      T = value1
      TOUT = value2

!
! --------  Provide pH profile 
!

!L3 ppH goes into the subroutine, cannot pass it as 'pH' because it is
!in a common block, If it gets reset, we'll reset it at the end
        DO I = 1, NPOINTS
           pH(I) = ppH(I)
        ENDDO

!----- Get DATA forthe water-column model
!      write(*,*) "In CASES,before DVODE, Y1=",Y(1)
!      write(*,*) "In CASES,before DVODE, pH1=",pH(1)
!      write(*,*) 'NPOINTS',NPOINTS
!      write(*,*) 'NEQ', NEQ
!      write(*,*) "Value1,Value2",value1,value2
!      write(*,*) "Y1,T,TOUT",Y(1),T,TOUT

      CALL DVODE(NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK, &
                 ISTATE, IOPT, RWORK, LRW, IWORK, LIW, MF, RPAR, IPAR)

!      write(6,*) "In CASES,after DVODE, Y1=",Y(1)
!      write(6,*) "In CASES,after DVODE, pH1=",pH(1)

!L3 I don't even think that pH gets updated, but in case, then here:
        DO I = 1, NPOINTS
           ppH(I) = pH(I)
        ENDDO


      RETURN
      END SUBROUTINE CASES


      SUBROUTINE DVODE (NEQ1, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK, &
                        ISTATE, IOPT, RWORK, LRW, IWORK, LIW, MF,  &
                        RPAR, IPAR)
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXNEQ = 27000
      INTEGER :: LIW, LRW
      INTEGER :: NEQ1, ITOL, ITASK, ISTATE, IOPT, IWORK(LIW)
      INTEGER :: MF, IPAR
      REAL(kind=8) :: T, TOUT
      REAL(kind=8) :: Y(NEQ1), RTOL(MAXNEQ), ATOL(MAXNEQ), RWORK(LRW), RPAR(MAXNEQ)

!
! Type declarations for local variables --------------------------------
!
      LOGICAL :: IHIT
      REAL(kind=8) :: ATOLI, BIG, EWTI, H0, HMAX, HMX
      REAL(kind=8) :: RH, RTOLI, SIZE, TCRIT, TNEXT, TOLSF, TP
      INTEGER :: I, IER, IFLAG, IMXER, JCO, KGO, LENIW, LENJ, LENP, LENRW
      INTEGER :: LENWM, LF0, MBAND, ML, MU, MXHNL0, MXSTP0, NITER, NSLAST
      CHARACTER(LEN=80) :: MSG
!
      INTEGER, DIMENSION(2) :: MORD = (/ 12, 5 /)

!
      MXSTP0 = 500 
      MXHNL0 = 10

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
      IF (ISTATE .EQ. 1) GO TO 10
      IF (INIT .NE. 1) GO TO 603
      IF (ISTATE .EQ. 2) GO TO 200
      GO TO 20
 10   INIT = 0
      IF (TOUT .EQ. T) RETURN
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 20   IF (NEQ1 .LE. 0) GO TO 604
      IF (ISTATE .EQ. 1) GO TO 25
      IF (NEQ1 .GT. N) GO TO 605
 25   N = NEQ1
      IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606
      IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607
      JSV = ISIGN(1,MF)
      MF = IABS(MF)
      METH = MF/10
      MITER = MF - 10*METH
      IF (METH .LT. 1 .OR. METH .GT. 2) GO TO 608
      IF (MITER .LT. 0 .OR. MITER .GT. 5) GO TO 608
      IF (MITER .LE. 3) GO TO 30
      ML = IWORK(1)
      MU = IWORK(2)
      IF (ML .LT. 0 .OR. ML .GE. N) GO TO 609
      IF (MU .LT. 0 .OR. MU .GE. N) GO TO 610
 30   CONTINUE
! Next process and check the optional input. ---------------------------
      IF (IOPT .EQ. 1) GO TO 40
      MAXORD = MORD(METH)
      MXSTEP = MXSTP0
      MXHNIL = MXHNL0
      IF (ISTATE .EQ. 1) H0 = ZERO
      HMXI = ZERO
      HMIN = ZERO
      GO TO 60
 40   MAXORD = IWORK(5)
      IF (MAXORD .LT. 0) GO TO 611
      IF (MAXORD .EQ. 0) MAXORD = 100
      MAXORD = MIN0(MAXORD,MORD(METH))
      MXSTEP = IWORK(6)
      IF (MXSTEP .LT. 0) GO TO 612
      IF (MXSTEP .EQ. 0) MXSTEP = MXSTP0
      MXHNIL = IWORK(7)
      IF (MXHNIL .LT. 0) GO TO 613
      IF (MXHNIL .EQ. 0) MXHNIL = MXHNL0
      IF (ISTATE .NE. 1) GO TO 50
      H0 = RWORK(5)
      IF ((TOUT - T)*H0 .LT. ZERO) GO TO 614
 50   HMAX = RWORK(6)
      IF (HMAX .LT. ZERO) GO TO 615
      HMXI = ZERO
      IF (HMAX .GT. ZERO) HMXI = ONE/HMAX
      HMIN = RWORK(7)
      IF (HMIN .LT. ZERO) GO TO 616
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 60   LYH = 21
      IF (ISTATE .EQ. 1) NYH = N
      LWM = LYH + (MAXORD + 1)*NYH
      JCO = MAX0(0,JSV)
      IF (MITER .EQ. 0) LENWM = 0
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        LENWM = 2 + (1 + JCO)*N*N
        LOCJS = N*N + 3
      ENDIF
      IF (MITER .EQ. 3) LENWM = 2 + N
      IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        MBAND = ML + MU + 1
        LENP = (MBAND + ML)*N
        LENJ = MBAND*N
        LENWM = 2 + LENP + JCO*LENJ
        LOCJS = LENP + 3
      ENDIF
      LEWT = LWM + LENWM
      LSAVF = LEWT + N
      LACOR = LSAVF + N
      LENRW = LACOR + N - 1
      IWORK(17) = LENRW
      LIWM = 1
      LENIW = 30 + N
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) LENIW = 30
      IWORK(18) = LENIW
      IF (LENRW .GT. LRW) GO TO 617
      IF (LENIW .GT. LIW) GO TO 618
! Check RTOL and ATOL for legality. ------------------------------------
      RTOLI = RTOL(1)
      ATOLI = ATOL(1)
      DO 70 I = 1,N
        IF (ITOL .GE. 3) RTOLI = RTOL(I)
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        IF (RTOLI .LT. ZERO) GO TO 619
        IF (ATOLI .LT. ZERO) GO TO 620
 70     CONTINUE
      IF (ISTATE .EQ. 1) GO TO 100
! If ISTATE = 3, set flag to signal parameter changes to VSTEP. --------
      JSTART = -1
      IF (NQ .LE. MAXORD) GO TO 90

! MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into SAVF. ---------
      CALL DCOPY (N, RWORK(LWM:LWM+N-1), 1, RWORK(LSAVF:LSAVF+N-1), 1)

! Reload WM(1) = RWORK(LWM), since LWM may have changed. ---------------
 90   IF (MITER .GT. 0) RWORK(LWM) = DSQRT(UROUND)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 100  UROUND = D1MACH(4)
      TN = T
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 110
      TCRIT = RWORK(1)
      IF ((TCRIT - TOUT)*(TOUT - T) .LT. ZERO) GO TO 625
      IF (H0 .NE. ZERO .AND. (T + H0 - TCRIT)*H0 .GT. ZERO) H0 = TCRIT - T
 110  JSTART = 0
      IF (MITER .GT. 0) RWORK(LWM) = DSQRT(UROUND)
      CCMXJ = PT2
      MSBJ = 50
      NHNIL = 0
      NST = 0
      NJE = 0
      NNI = 0
      NCFN = 0
      NETF = 0
      NLU = 0
      NSLJ = 0
      NSLAST = 0
      HU = ZERO
      NQU = 0
! Initial call to F.  (LF0 points to YH(*,2).) -------------------------
      LF0 = LYH + NYH
      CALL FEX2(N, T, Y, RWORK(LF0:LF0+N-1), RPAR, IPAR)
      NFE = 1
! Load the initial value vector in YH. ---------------------------------
      CALL DCOPY (N, Y, 1, RWORK(LYH:LYH+N-1), 1)
! Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
      NQ = 1
      H = ONE
      CALL EWSET (N, ITOL, RTOL, ATOL, RWORK(LYH:LYH+N-1), RWORK(LEWT:LEWT+N-1))
      DO 120 I = 1, N
        IF (RWORK(I+LEWT-1) .LE. ZERO) GO TO 621
 120    RWORK(I+LEWT-1) = ONE/RWORK(I+LEWT-1)
      IF (H0 .NE. ZERO) GO TO 180
! Call VHIN to set initial step size H0 to be attempted. ---------------
      CALL VHIN (N, T, RWORK(LYH:LYH+N-1), RWORK(LF0:LF0+N-1), RPAR, IPAR, TOUT, &
                 UROUND, RWORK(LEWT:LEWT+N-1), ITOL, ATOL, Y, RWORK(LACOR:LACOR+N-1), H0, &
                 NITER, IER)
      NFE = NFE + NITER
      IF (IER .NE. 0) GO TO 622
! Adjust H0 if necessary to meet HMAX bound. ---------------------------
 180  RH = DABS(H0)*HMXI
      IF (RH .GT. ONE) H0 = H0/RH
! Load H with H0 and scale YH(*,2) by H0. ------------------------------
      H = H0
      CALL DSCAL (N, H0, RWORK(LF0:LF0+N-1), 1)
      GO TO 270
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 200  NSLAST = NST
      KUTH = 0
      GO TO (210, 250, 220, 230, 240), ITASK
 210  IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      CALL VINDY (TOUT, 0, RWORK(LYH:LYH+NYH-1), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 220  TP = TN - HU*(ONE + HUN*UROUND)
      IF ((TP - TOUT)*H .GT. ZERO) GO TO 623
      IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      GO TO 400
 230  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. ZERO) GO TO 624
      IF ((TCRIT - TOUT)*H .LT. ZERO) GO TO 625
      IF ((TN - TOUT)*H .LT. ZERO) GO TO 245
      CALL VINDY (TOUT, 0, RWORK(LYH:LYH+NYH-1), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 240  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. ZERO) GO TO 624
 245  HMX = DABS(TN) + DABS(H)
      IHIT = DABS(TN - TCRIT) .LE. HUN*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + HNEW*(ONE + FOUR*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. ZERO) GO TO 250
      H = (TCRIT - TN)*(ONE - FOUR*UROUND)
      KUTH = 1
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 250  CONTINUE
      IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500
      CALL EWSET (N, ITOL, RTOL, ATOL, RWORK(LYH:LYH+N-1), RWORK(LEWT:LEWT+N-1))
      DO 260 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. ZERO) GO TO 510
 260    RWORK(I+LEWT-1) = ONE/RWORK(I+LEWT-1)
 270  TOLSF = UROUND*VNORM (N, RWORK(LYH), RWORK(LEWT))
      IF (TOLSF .LE. ONE) GO TO 280
      TOLSF = TOLSF*TWO
      IF (NST .EQ. 0) GO TO 626
      GO TO 520
 280  IF ((TN + H) .NE. TN) GO TO 290
      NHNIL = NHNIL + 1
      IF (NHNIL .GT. MXHNIL) GO TO 290
      MSG = 'VODE--   Warning..internal T (=R1) and H (=R2) are'
      CALL XERRWV (MSG, 50, 101, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      such that in the machine, T + H = T on the next step  '
      CALL XERRWV (MSG, 60, 101, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      (H = step size). solver will continue anyway'
      CALL XERRWV (MSG, 50, 101, 1, 0, 0, 0, 2, TN, H)
      IF (NHNIL .LT. MXHNIL) GO TO 290
      MSG = 'VODE--   Above warning has been issued I1 times.  '
      CALL XERRWV (MSG, 50, 102, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      it will not be issued again for this problem'
      CALL XERRWV (MSG, 50, 102, 1, 1, MXHNIL, 0, 0, ZERO, ZERO)
 290  CONTINUE

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      CALL VSTEP (Y, RWORK(LYH:LRW), NYH, RWORK(LYH:LRW),  &
                  RWORK(LEWT:LEWT+NYH-1), RWORK(LSAVF:LSAVF+NYH-1), Y, &
                  RWORK(LACOR:LACOR+NYH-1), RWORK(LWM:LRW),      &
                  IWORK(LIWM:LIWM+NYH-1), RPAR, IPAR)

      KGO = 1 - KFLAG
! Branch on KFLAG.  Note..In this version, KFLAG can not be set to -3.
!  KFLAG .eq. 0,   -1,  -2
      GO TO (300, 530, 540), KGO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 300  INIT = 1
      KUTH = 0
      GO TO (310, 400, 330, 340, 350), ITASK
! ITASK = 1.  If TOUT has been reached, interpolate. -------------------
 310  IF ((TN - TOUT)*H .LT. ZERO) GO TO 250
      CALL VINDY (TOUT, 0, RWORK(LYH:LYH+NYH-1), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
! ITASK = 3.  Jump to exit if TOUT was reached. ------------------------
 330  IF ((TN - TOUT)*H .GE. ZERO) GO TO 400
      GO TO 250
! ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary.
 340  IF ((TN - TOUT)*H .LT. ZERO) GO TO 345
      CALL VINDY (TOUT, 0, RWORK(LYH:LYH+NYH-1), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
 345  HMX = DABS(TN) + DABS(H)
      IHIT = DABS(TN - TCRIT) .LE. HUN*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + HNEW*(ONE + FOUR*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. ZERO) GO TO 250
      H = (TCRIT - TN)*(ONE - FOUR*UROUND)
      KUTH = 1
      GO TO 250
! ITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
 350  HMX = DABS(TN) + DABS(H)
      IHIT = DABS(TN - TCRIT) .LE. HUN*UROUND*HMX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 400  CONTINUE
      CALL DCOPY (N, RWORK(LYH:LYH+N-1), 1, Y, 1)
      T = TN
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420
      IF (IHIT) T = TCRIT
 420  ISTATE = 2
      RWORK(11) = HU
      RWORK(12) = HNEW
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NEWQ
      IWORK(19) = NLU
      IWORK(20) = NNI
      IWORK(21) = NCFN
      IWORK(22) = NETF
      RETURN
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! The maximum number of steps was taken before reaching TOUT. ----------
 500  MSG = 'VODE--   At current T (=R1), MXSTEP (=I1) steps   '
      CALL XERRWV (MSG, 50, 201, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      taken on this call before reaching TOUT     '
      CALL XERRWV (MSG, 50, 201, 1, 1, MXSTEP, 0, 1, TN, ZERO)
      ISTATE = -1
      GO TO 580
! EWT(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  EWTI = RWORK(LEWT+I-1)
      MSG = 'VODE--   At T (=R1), EWT(I1) has become R2 .le. 0.'
      CALL XERRWV (MSG, 50, 202, 1, 1, I, 0, 2, TN, EWTI)
      ISTATE = -6
      GO TO 580
! Too much accuracy requested for machine precision. -------------------
 520  MSG = 'VODE--   At T (=R1), too much accuracy requested  '
      CALL XERRWV (MSG, 50, 203, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      for precision of machine..  see TOLSF (=R2) '
      CALL XERRWV (MSG, 50, 203, 1, 0, 0, 0, 2, TN, TOLSF)
      RWORK(14) = TOLSF
      ISTATE = -2
      GO TO 580
! KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
 530  MSG = 'VODE--   At T(=R1) and step size H(=R2), the error'
      CALL XERRWV (MSG, 50, 204, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      test failed repeatedly or with abs(H) = HMIN'
      CALL XERRWV (MSG, 50, 204, 1, 0, 0, 0, 2, TN, H)
      ISTATE = -4
      GO TO 560
! KFLAG = -2.  Convergence failed repeatedly or with abs(H) = HMIN. ----
 540  MSG = 'VODE--   At T (=R1) and step size H (=R2), the    '
      CALL XERRWV (MSG, 50, 205, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      corrector convergence failed repeatedly     '
      CALL XERRWV (MSG, 50, 205, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG = '      or with abs(H) = HMIN   '
      CALL XERRWV (MSG, 30, 205, 1, 0, 0, 0, 2, TN, H)
      ISTATE = -5
! Compute IMXER if relevant. -------------------------------------------
 560  BIG = ZERO
      IMXER = 1
      DO 570 I = 1,N
        SIZE = DABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
        IF (BIG .GE. SIZE) GO TO 570
        BIG = SIZE
        IMXER = I
 570    CONTINUE
      IWORK(16) = IMXER
! Set Y vector, T, and optional output. --------------------------------
 580  CONTINUE
      CALL DCOPY (N, RWORK(LYH:LYH+N-1), 1, Y, 1)
      T = TN
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      IWORK(19) = NLU
      IWORK(20) = NNI
      IWORK(21) = NCFN
      IWORK(22) = NETF
      RETURN
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 601  MSG = 'VODE--   ISTATE (=I1) illegal '
      CALL XERRWV (MSG, 30, 1, 1, 1, ISTATE, 0, 0, ZERO, ZERO)
      IF (ISTATE .LT. 0) GO TO 800
      GO TO 700
 602  MSG = 'VODE--   ITASK (=I1) illegal  '
      CALL XERRWV (MSG, 30, 2, 1, 1, ITASK, 0, 0, ZERO, ZERO)
      GO TO 700
 603  MSG = 'VODE--   ISTATE .gt. 1 but VODE- not initialized  '
      CALL XERRWV (MSG, 50, 3, 1, 0, 0, 0, 0, ZERO, ZERO)
      GO TO 700
 604  MSG = 'VODE--   NEQ (=I1) .lt. 1     '
      CALL XERRWV (MSG, 30, 4, 1, 1, NEQ1, 0, 0, ZERO, ZERO)
      GO TO 700
 605  MSG = 'VODE--   ISTATE = 3 and NEQ increased (I1 to I2)  '
      CALL XERRWV (MSG, 50, 5, 1, 2, N, NEQ1, 0, ZERO, ZERO)
      GO TO 700
 606  MSG = 'VODE--   ITOL (=I1) illegal   '
      CALL XERRWV (MSG, 30, 6, 1, 1, ITOL, 0, 0, ZERO, ZERO)
      GO TO 700
 607  MSG = 'VODE--   IOPT (=I1) illegal   '
      CALL XERRWV (MSG, 30, 7, 1, 1, IOPT, 0, 0, ZERO, ZERO)
      GO TO 700
 608  MSG = 'VODE--   MF (=I1) illegal     '
      CALL XERRWV (MSG, 30, 8, 1, 1, MF, 0, 0, ZERO, ZERO)
      GO TO 700
 609  MSG = 'VODE--   ML (=I1) illegal.. .lt.0 or .ge.NEQ (=I2)'
      CALL XERRWV (MSG, 50, 9, 1, 2, ML, NEQ1, 0, ZERO, ZERO)
      GO TO 700
 610  MSG = 'VODE--   MU (=I1) illegal.. .lt.0 or .ge.NEQ (=I2)'
      CALL XERRWV (MSG, 50, 10, 1, 2, MU, NEQ1, 0, ZERO, ZERO)
      GO TO 700
 611  MSG = 'VODE--   MAXORD (=I1) .lt. 0  '
      CALL XERRWV (MSG, 30, 11, 1, 1, MAXORD, 0, 0, ZERO, ZERO)
      GO TO 700
 612  MSG = 'VODE--   MXSTEP (=I1) .lt. 0  '
      CALL XERRWV (MSG, 30, 12, 1, 1, MXSTEP, 0, 0, ZERO, ZERO)
      GO TO 700
 613  MSG = 'VODE--   MXHNIL (=I1) .lt. 0  '
      CALL XERRWV (MSG, 30, 13, 1, 1, MXHNIL, 0, 0, ZERO, ZERO)
      GO TO 700
 614  MSG = 'VODE--   TOUT (=R1) behind T (=R2)      '
      CALL XERRWV (MSG, 40, 14, 1, 0, 0, 0, 2, TOUT, T)
      MSG = '      integration direction is given by H0 (=R1)  '
      CALL XERRWV (MSG, 50, 14, 1, 0, 0, 0, 1, H0, ZERO)
      GO TO 700
 615  MSG = 'VODE--   HMAX (=R1) .lt. 0.0  '
      CALL XERRWV (MSG, 30, 15, 1, 0, 0, 0, 1, HMAX, ZERO)
      GO TO 700
 616  MSG = 'VODE--   HMIN (=R1) .lt. 0.0  '
      CALL XERRWV (MSG, 30, 16, 1, 0, 0, 0, 1, HMIN, ZERO)
      GO TO 700
 617  CONTINUE
      MSG='VODE--   RWORK length needed, LENRW (=I1), exceeds LRW (=I2)'
      CALL XERRWV (MSG, 60, 17, 1, 2, LENRW, LRW, 0, ZERO, ZERO)
      GO TO 700
 618  CONTINUE
      MSG='VODE--   IWORK length needed, LENIW (=I1), exceeds LIW (=I2)'
      CALL XERRWV (MSG, 60, 18, 1, 2, LENIW, LIW, 0, ZERO, ZERO)
      GO TO 700
 619  MSG = 'VODE--   RTOL(I1) is R1 .lt. 0.0        '
      CALL XERRWV (MSG, 40, 19, 1, 1, I, 0, 1, RTOLI, ZERO)
      GO TO 700
 620  MSG = 'VODE--   ATOL(I1) is R1 .lt. 0.0        '
      CALL XERRWV (MSG, 40, 20, 1, 1, I, 0, 1, ATOLI, ZERO)
      GO TO 700
 621  EWTI = RWORK(LEWT+I-1)
      MSG = 'VODE--   EWT(I1) is R1 .le. 0.0         '
      CALL XERRWV (MSG, 40, 21, 1, 1, I, 0, 1, EWTI, ZERO)
      GO TO 700
 622  CONTINUE
      MSG='VODE--   TOUT (=R1) too close to T(=R2) to start integration'
      CALL XERRWV (MSG, 60, 22, 1, 0, 0, 0, 2, TOUT, T)
      GO TO 700
 623  CONTINUE
      MSG='VODE--   ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  '
      CALL XERRWV (MSG, 60, 23, 1, 1, ITASK, 0, 2, TOUT, TP)
      GO TO 700
 624  CONTINUE
      MSG='VODE--   ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   '
      CALL XERRWV (MSG, 60, 24, 1, 0, 0, 0, 2, TCRIT, TN)
      GO TO 700
 625  CONTINUE
      MSG='VODE--   ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   '
      CALL XERRWV (MSG, 60, 25, 1, 0, 0, 0, 2, TCRIT, TOUT)
      GO TO 700
 626  MSG = 'VODE--   At start of problem, too much accuracy   '
      CALL XERRWV (MSG, 50, 26, 1, 0, 0, 0, 0, ZERO, ZERO)
      MSG='      requested for precision of machine..  see TOLSF (=R1) '
      CALL XERRWV (MSG, 60, 26, 1, 0, 0, 0, 1, TOLSF, ZERO)
      RWORK(14) = TOLSF
      GO TO 700
 627  MSG = 'VODE--   Trouble from VINDY. ITASK = I1, TOUT = R1'
      CALL XERRWV (MSG, 50, 27, 1, 1, ITASK, 0, 1, TOUT, ZERO)
!
 700  CONTINUE
      ISTATE = -3
      RETURN
!
 800  MSG = 'VODE--   Run aborted.. apparent infinite loop     '
      CALL XERRWV (MSG, 50, 303, 2, 0, 0, 0, 0, ZERO, ZERO)
      RETURN
!----------------------- End of Subroutine DVODE ------------------------
      END SUBROUTINE DVODE


!------------------------------------------------------------------------

      SUBROUTINE VHIN (N, T0, Y0, YDOT, RPAR, IPAR, TOUT, UROUND, &
                       EWT, ITOL, ATOL, Y, TEMP, H0, NITER, IER)
      IMPLICIT NONE
      INTEGER :: N, IPAR, ITOL, NITER, IER
      INTEGER :: I, ITER
      REAL(kind=8) :: T0, TOUT, UROUND, H0
      REAL(kind=8) :: Y0(:), YDOT(:), EWT(:), ATOL(:), Y(:)
      REAL(kind=8) :: TEMP(:), RPAR(:)
      REAL(kind=8) :: AFI, ATOLI, DELYI, HG, HLB, HNEW, HRAT
      REAL(kind=8) :: HUB, T1, TDIST, TROUND, YDDNRM

      NITER = 0
      TDIST = DABS(TOUT - T0)
      TROUND = UROUND*DMAX1(DABS(T0),DABS(TOUT))
      IF (TDIST .LT. TWO*TROUND) GO TO 100
!
! Set a lower bound on h based on the roundoff level in T0 and TOUT. ---
      HLB = HUN*TROUND
! Set an upper bound on h based on TOUT-T0 and the initial Y and YDOT. -
      HUB = PT1*TDIST
      ATOLI = ATOL(1)
      DO 10 I = 1, N
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        DELYI = PT1*DABS(Y0(I)) + ATOLI
        AFI = DABS(YDOT(I))
        IF (AFI*HUB .GT. DELYI) HUB = DELYI/AFI
 10     CONTINUE
!
! Set initial guess for h as geometric mean of upper and lower bounds. -
      ITER = 0
      HG = DSQRT(HLB*HUB)
! If the bounds have crossed, exit with the mean value. ----------------
      IF (HUB .LT. HLB) THEN
        H0 = HG
        GO TO 90
      ENDIF
!
! Looping point for iteration. -----------------------------------------
 50   CONTINUE
!  Estimate the second derivative as a difference quotient in f. --------
      T1 = T0 + HG
      DO 60 I = 1, N
 60     Y(I) = Y0(I) + HG*YDOT(I)

      CALL FEX2(N, T1, Y, TEMP, RPAR, IPAR)

      DO 70 I = 1,N
 70     TEMP(I) = (TEMP(I) - YDOT(I))/HG
      YDDNRM = VNORM (N, TEMP, EWT)
!  Get the corresponding new value of h. --------------------------------
      IF (YDDNRM*HUB*HUB .GT. TWO) THEN
        HNEW = DSQRT(TWO/YDDNRM)
      ELSE
        HNEW = DSQRT(HG*HUB)
      ENDIF
      ITER = ITER + 1
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
      IF (ITER .GE. 4) GO TO 80
      HRAT = HNEW/HG
      IF ( (HRAT .GT. HALF) .AND. (HRAT .LT. TWO) ) GO TO 80
      IF ( (ITER .GE. 2) .AND. (HNEW .GT. TWO*HG) ) THEN
        HNEW = HG
        GO TO 80
      ENDIF
      HG = HNEW
      GO TO 50
! 
!  Iteration done.  Apply bounds, bias factor, and sign.  Then exit. ----
 80   H0 = HNEW*HALF
      IF (H0 .LT. HLB) H0 = HLB
      IF (H0 .GT. HUB) H0 = HUB
 90   H0 = DSIGN(H0, TOUT - T0)
      NITER = ITER
      IER = 0
      RETURN
!  Error return for TOUT - T0 too small. --------------------------------
 100  IER = -1
      RETURN
! ----------------------- End of Subroutine VHIN ------------------------
      END SUBROUTINE VHIN


      SUBROUTINE VINDY (T, K, YH, LDYH, DKY, IFLAG)
      IMPLICIT NONE
      INTEGER :: K, LDYH, IFLAG
      REAL(kind=8) :: T, YH(LDYH,*), DKY(:)      
!
!  Type declarations for local variables --------------------------------
! 
      REAL(kind=8) :: C, R, S, TFUZZ, TN1, TP
      INTEGER :: I, IC, J, JB, JB2, JJ, JJ1, JP1
      CHARACTER(LEN = 80) :: MSG
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------

      IFLAG = 0
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80
      TFUZZ = HUN * UROUND * (TN + HU)
      TP = TN - HU - TFUZZ
      TN1 = TN + TFUZZ
      IF ((T-TP)*(T-TN1) .GT. ZERO) GO TO 90
! 
      S = (T - TN)/H
      IC = 1
      IF (K .EQ. 0) GO TO 15
      JJ1 = L - K
      DO 10 JJ = JJ1,NQ
 10     IC = IC*JJ
 15   C = DFLOAT(IC)
      DO 20 I = 1, N
 20     DKY(I) = C*YH(I,L)
      IF (K .EQ. NQ) GO TO 55
      JB2 = NQ - K
      DO 50 JB = 1,JB2
        J = NQ - JB
        JP1 = J + 1
        IC = 1
        IF (K .EQ. 0) GO TO 35
        JJ1 = JP1 - K
        DO 30 JJ = JJ1,J
 30       IC = IC*JJ
 35     C = DFLOAT(IC)
        DO 40 I = 1, N
 40       DKY(I) = C*YH(I,JP1) + S*DKY(I)
 50     CONTINUE
      IF (K .EQ. 0) RETURN
 55   R = H**(-K)
      CALL DSCAL (N, R, DKY, 1)
      RETURN
! 
 80   MSG = 'VINDY--  K (=I1) illegal      '
      CALL XERRWV (MSG, 30, 51, 1, 1, K, 0, 0, ZERO, ZERO)
      IFLAG = -1
      RETURN
 90   MSG = 'VINDY--  T (=R1) illegal      '
      CALL XERRWV (MSG, 30, 52, 1, 0, 0, 0, 1, T, ZERO)
      MSG='      T not in interval TCUR - HU (= R1) to TCUR (=R2)      '
      CALL XERRWV (MSG, 60, 52, 1, 0, 0, 0, 2, TP, TN)
      IFLAG = -2
      RETURN
! ----------------------- End of Subroutine VINDY -----------------------
      END SUBROUTINE VINDY


      SUBROUTINE VSTEP (Y, YH, LDYH, YH1, EWT, SAVF, VSAV, ACOR, &
                        WM, IWM, RPAR, IPAR)
      IMPLICIT NONE
      INTEGER :: LDYH, IWM(:), IPAR
      REAL(kind=8) :: Y(:), YH(LDYH,*), YH1(:), EWT(:), SAVF(:), VSAV(:)
      REAL(kind=8) :: ACOR(:), WM(:), RPAR(:)

! 
!  Type declarations for local variables --------------------------------
! 
      REAL(kind=8) :: CNQUOT, DDN, DSM, DUP, ETAQ, ETAQM1, ETAQP1, FLOTL
      REAL(kind=8) :: R, TOLD
      INTEGER :: I, I1, I2, IBACK, J, JB, NCF, NFLAG
      INTEGER :: KFC = -3, KFH = -7, MXNCF = 10
! -----------------------------------------------------------------------
      REAL(kind=8) :: ADDON   = 1.0D-6,    BIAS1   = 6.0D0,     BIAS2   = 6.0D0
      REAL(kind=8) :: BIAS3   = 10.0D0,    ETACF   = 0.25D0,    ETAMIN  = 0.1D0
      REAL(kind=8) :: ETAMXF  = 0.2D0,     ETAMX1  = 1.0D4,     ETAMX2  = 10.0D0
      REAL(kind=8) :: ETAMX3  = 10.0D0,    ONEPSM  = 1.00001D0, THRESH  = 1.5D0
 
! 
      KFLAG = 0
      TOLD = TN
      NCF = 0
      JCUR = 0
      NFLAG = 0
      IF (JSTART .GT. 0) GO TO 20
      IF (JSTART .EQ. -1) GO TO 100
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
      LMAX = MAXORD + 1
      NQ = 1
      L = 2
      NQNYH = NQ*LDYH
      TAU(1) = H
      PRL1 = ONE
      RC = ZERO
      ETAMAX = ETAMX1
      NQWAIT = 2
      HSCAL = H
      GO TO 200
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 20   CONTINUE
      IF (KUTH .EQ. 1) THEN
        ETA = DMIN1(ETA,H/HSCAL)
        NEWH = 1
        ENDIF
 50   IF (NEWH .EQ. 0) GO TO 200
      IF (NEWQ .EQ. NQ) GO TO 150
      IF (NEWQ .LT. NQ) THEN
        CALL VJUST (YH, LDYH, -1)
        NQ = NEWQ
        L = NQ + 1
        NQWAIT = L
        GO TO 150
        ENDIF
      IF (NEWQ .GT. NQ) THEN
        CALL VJUST (YH, LDYH, 1)
        NQ = NEWQ
        L = NQ + 1
        NQWAIT = L
        GO TO 150
      ENDIF
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 100  CONTINUE
      LMAX = MAXORD + 1
      IF (N .EQ. LDYH) GO TO 120
      I1 = 1 + (NEWQ + 1)*LDYH
      I2 = (MAXORD + 1)*LDYH
      IF (I1 .GT. I2) GO TO 120
      DO 110 I = I1, I2
 110    YH1(I) = ZERO
 120  IF (NEWQ .LE. MAXORD) GO TO 140
      FLOTL = DFLOAT(LMAX)
      IF (MAXORD .LT. NQ-1) THEN
        DDN = VNORM (N, SAVF, EWT)/TQ(1)
        ETA = ONE/((BIAS1*DDN)**(ONE/FLOTL) + ADDON)
        ENDIF
      IF (MAXORD .EQ. NQ .AND. NEWQ .EQ. NQ+1) THEN
        write(6,*) "etaq",ETAQ
        ETA = ETAQ
      ENDIF

      IF (MAXORD .EQ. NQ-1 .AND. NEWQ .EQ. NQ+1) THEN
        write(6,*) "etaqm1",ETAQM1
        ETA = ETAQM1
        CALL VJUST (YH, LDYH, -1)
        ENDIF
      IF (MAXORD .EQ. NQ-1 .AND. NEWQ .EQ. NQ) THEN
        DDN = VNORM (N, SAVF, EWT)/TQ(1)
        ETA = ONE/((BIAS1*DDN)**(ONE/FLOTL) + ADDON)
        CALL VJUST (YH, LDYH, -1)
        ENDIF
      ETA = DMIN1(ETA,ONE)
      NQ = MAXORD
      L = LMAX
 140  IF (KUTH .EQ. 1) ETA = DMIN1(ETA,DABS(H/HSCAL))
      IF (KUTH .EQ. 0) ETA = DMAX1(ETA,HMIN/DABS(HSCAL))
      ETA = ETA/DMAX1(ONE,DABS(HSCAL)*HMXI*ETA)
      NEWH = 1
      NQWAIT = L
      IF (NEWQ .LE. MAXORD) GO TO 50
!  Rescale the history array for a change in H by a factor of ETA. ------
 150  R = ONE
      DO 180 J = 2, L
        R = R*ETA
        CALL DSCAL (N, R, YH(1:N,J), 1 )
 180    CONTINUE
      H = HSCAL*ETA
      HSCAL = H
      RC = RC*ETA
      NQNYH = NQ*LDYH
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 200  TN = TN + H
      I1 = NQNYH + 1
      DO 220 JB = 1, NQ
        I1 = I1 - LDYH
        DO 210 I = I1, NQNYH
 210      YH1(I) = YH1(I) + YH1(I+LDYH)
 220  CONTINUE
      CALL VSET
      RL1 = ONE/EL(2)
      RC = RC*(RL1/PRL1)
      PRL1 = RL1
! 
!  Call the nonlinear system solver. ------------------------------------
! 
      CALL VNLSD(Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM, &
                 NFLAG, RPAR, IPAR)
! 
      IF (NFLAG .EQ. 0) GO TO 450
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
        NCF = NCF + 1
        NCFN = NCFN + 1
        ETAMAX = ONE
        TN = TOLD
        I1 = NQNYH + 1
        DO 430 JB = 1,NQ
          I1 = I1 - LDYH
          DO 420 I = I1,NQNYH
 420        YH1(I) = YH1(I) - YH1(I+LDYH)
 430      CONTINUE
        IF (NFLAG .LT. -1) GO TO 680
        IF (DABS(H) .LE. HMIN*ONEPSM) GO TO 670
        IF (NCF .EQ. MXNCF) GO TO 670
        ETA = ETACF
        ETA = DMAX1(ETA,HMIN/DABS(H))
        NFLAG = -1
        GO TO 150
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 450  CONTINUE
      DSM = ACNRM/TQ(2)
      IF (DSM .GT. ONE) GO TO 500
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
      KFLAG = 0
      NST = NST + 1
      HU = H
      NQU = NQ
      DO 470 IBACK = 1,NQ
        I = L - IBACK
 470    TAU(I+1) = TAU(I)
      TAU(1) = H
      DO 480 J = 1,L
        CALL DAXPY (N, EL(J), ACOR, 1, YH(1:N,J), 1 )
 480    CONTINUE
      NQWAIT = NQWAIT - 1
      IF ((L .EQ. LMAX) .OR. (NQWAIT .NE. 1)) GO TO 490
      CALL DCOPY (N, ACOR, 1, YH(1:N,LMAX), 1 )
      CONP = TQ(5)
 490  IF (ETAMAX .NE. ONE) GO TO 560
      IF (NQWAIT .LT. 2) NQWAIT = 2
      NEWQ = NQ
      NEWH = 0
      ETA = ONE
      HNEW = H
      GO TO 690
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 500  KFLAG = KFLAG - 1
      NETF = NETF + 1
      NFLAG = -2
      TN = TOLD
      I1 = NQNYH + 1
      DO 520 JB = 1,NQ
        I1 = I1 - LDYH
        DO 510 I = I1,NQNYH
 510      YH1(I) = YH1(I) - YH1(I+LDYH)
 520  CONTINUE
      IF (DABS(H) .LE. HMIN*ONEPSM) GO TO 660
      ETAMAX = ONE
      IF (KFLAG .LE. KFC) GO TO 530
!  Compute ratio of new H to current H at the current order. ------------
      FLOTL = DFLOAT(L)
      ETA = ONE/((BIAS2*DSM)**(ONE/FLOTL) + ADDON)
      ETA = DMAX1(ETA,HMIN/DABS(H),ETAMIN)
      IF ((KFLAG .LE. -2) .AND. (ETA .GT. ETAMXF)) ETA = ETAMXF
      GO TO 150
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 530  IF (KFLAG .EQ. KFH) GO TO 660
      IF (NQ .EQ. 1) GO TO 540
      ETA = DMAX1(ETAMIN,HMIN/DABS(H))
      CALL VJUST (YH, LDYH, -1)
      L = NQ
      NQ = NQ - 1
      NQWAIT = L
      GO TO 150
 540  ETA = DMAX1(ETAMIN,HMIN/DABS(H))
      H = H*ETA
      HSCAL = H
      TAU(1) = H
      CALL FEX2(N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      DO 550 I = 1, N
 550    YH(I,2) = H*SAVF(I)
      NQWAIT = 10
      GO TO 200
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!  Compute ratio of new H to current H at the current order. ------------
 560  FLOTL = DFLOAT(L)
      ETAQ = ONE/((BIAS2*DSM)**(ONE/FLOTL) + ADDON)
      IF (NQWAIT .NE. 0) GO TO 600
      NQWAIT = 2
      ETAQM1 = ZERO
      IF (NQ .EQ. 1) GO TO 570
!  Compute ratio of new H to current H at the current order less one. ---
      DDN = VNORM (N, YH(1,L), EWT)/TQ(1)
      ETAQM1 = ONE/((BIAS1*DDN)**(ONE/(FLOTL - ONE)) + ADDON)
 570  ETAQP1 = ZERO
      IF (L .EQ. LMAX) GO TO 580
!  Compute ratio of new H to current H at current order plus one. -------
      CNQUOT = (TQ(5)/CONP)*(H/TAU(2))**L
      DO 575 I = 1,N
 575    SAVF(I) = ACOR(I) - CNQUOT*YH(I,LMAX)
      DUP = VNORM (N, SAVF, EWT)/TQ(3)
      ETAQP1 = ONE/((BIAS3*DUP)**(ONE/(FLOTL + ONE)) + ADDON)
 580  IF (ETAQ .GE. ETAQP1) GO TO 590
      IF (ETAQP1 .GT. ETAQM1) GO TO 620
      GO TO 610
 590  IF (ETAQ .LT. ETAQM1) GO TO 610
 600  ETA = ETAQ
      NEWQ = NQ
      GO TO 630
 610  ETA = ETAQM1
      NEWQ = NQ - 1
      GO TO 630
 620  ETA = ETAQP1
      NEWQ = NQ + 1
      CALL DCOPY (N, ACOR, 1, YH(1:N,LMAX), 1)
!  Test tentative new H against THRESH, ETAMAX, and HMXI, then exit. ----
 630  IF (ETA .LT. THRESH .OR. ETAMAX .EQ. ONE) GO TO 640
      ETA = DMIN1(ETA,ETAMAX)
      ETA = ETA/DMAX1(ONE,DABS(H)*HMXI*ETA)
      NEWH = 1
      HNEW = H*ETA
      GO TO 690
 640  NEWQ = NQ
      NEWH = 0
      ETA = ONE
      HNEW = H
      GO TO 690
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 660  KFLAG = -1
      GO TO 720
 670  KFLAG = -2
      GO TO 720
 680  IF (NFLAG .EQ. -2) KFLAG = -3
      IF (NFLAG .EQ. -3) KFLAG = -4
      GO TO 720
 690  ETAMAX = ETAMX3
      IF (NST .LE. 10) ETAMAX = ETAMX2
 700  R = ONE/TQ(2)
      CALL DSCAL (N, R, ACOR, 1)
 720  JSTART = 1
      RETURN
! ----------------------- End of Subroutine VSTEP -----------------------
      END SUBROUTINE VSTEP

! -----------------------------------------------------------------------

      SUBROUTINE VSET
! 
!  Type declarations for local variables --------------------------------
! 
      IMPLICIT NONE
      REAL(kind=8) :: AHATN0, ALPH0, CNQM1, CORTES, CSUM, ELP, EM(13)
      REAL(kind=8) :: EM0, FLOTI, FLOTL, FLOTNQ, HSUM, RXI, RXIS, S 
      REAL(kind=8) :: T1, T2, T3, T4, T5, T6, XI
      INTEGER :: I, IBACK, J, JP1, NQM1, NQM2 

! -----------------------------------------------------------------------

      CORTES = 0.1D0
      FLOTL = DFLOAT(L)
      NQM1 = NQ - 1
      NQM2 = NQ - 2
      GO TO (100, 200), METH
! 
!  Set coefficients for Adams methods. ----------------------------------
 100  IF (NQ .NE. 1) GO TO 110
      EL(1) = ONE
      EL(2) = ONE
      TQ(1) = ONE
      TQ(2) = TWO
      TQ(3) = SIX*TQ(2)
      TQ(5) = ONE
      GO TO 300
 110  HSUM = H
      EM(1) = ONE
      FLOTNQ = FLOTL - ONE
      DO 115 I = 2,L
 115    EM(I) = ZERO
      DO 150 J = 1,NQM1
        IF ((J .NE. NQM1) .OR. (NQWAIT .NE. 1)) GO TO 130
        S = ONE
        CSUM = ZERO
        DO 120 I = 1,NQM1
          CSUM = CSUM + S*EM(I)/DFLOAT(I+1)
 120      S = -S
        TQ(1) = EM(NQM1)/(FLOTNQ*CSUM)
 130    RXI = H/HSUM
        DO 140 IBACK = 1,J
          I = (J + 2) - IBACK
 140      EM(I) = EM(I) + EM(I-1)*RXI
        HSUM = HSUM + TAU(J)
 150    CONTINUE
!  Compute integral from -1 to 0 of polynomial and of x times it. -------
      S = ONE
      EM0 = ZERO
      CSUM = ZERO
      DO 160 I = 1,NQ
        FLOTI = DFLOAT(I)
        EM0 = EM0 + S*EM(I)/FLOTI
        CSUM = CSUM + S*EM(I)/(FLOTI+ONE)
 160    S = -S
!  In EL, form coefficients of normalized integrated polynomial. --------
      S = ONE/EM0
      EL(1) = ONE
      DO 170 I = 1,NQ
 170    EL(I+1) = S*EM(I)/DFLOAT(I)
      XI = HSUM/H
      TQ(2) = XI*EM0/CSUM
      TQ(5) = XI/EL(L)
      IF (NQWAIT .NE. 1) GO TO 300
!  For higher order control constant, multiply polynomial by 1+x/xi(q). -
      RXI = ONE/XI
      DO 180 IBACK = 1, NQ
        I = (L + 1) - IBACK
 180    EM(I) = EM(I) + EM(I-1)*RXI
!  Compute integral of polynomial. --------------------------------------
      S = ONE
      CSUM = ZERO
      DO 190 I = 1, L
        CSUM = CSUM + S*EM(I)/DFLOAT(I+1)
 190    S = -S
      TQ(3) = FLOTL*EM0/CSUM
      GO TO 300
! 
!  Set coefficients for BDF methods. ------------------------------------
 200  DO 210 I = 3, L
 210    EL(I) = ZERO
      EL(1) = ONE
      EL(2) = ONE
      ALPH0 = -ONE
      AHATN0 = -ONE
      HSUM = H
      RXI = ONE
      RXIS = ONE
      IF (NQ .EQ. 1) GO TO 240
      DO 230 J = 1, NQM2
!  In EL, construct coefficients of (1+x/xi(1))*...*(1+x/xi(j+1)). ------
        HSUM = HSUM + TAU(J)
        RXI = H/HSUM
        JP1 = J + 1
        ALPH0 = ALPH0 - ONE/DFLOAT(JP1)
        DO 220 IBACK = 1,JP1
          I = (J + 3) - IBACK
 220      EL(I) = EL(I) + EL(I-1)*RXI
 230    CONTINUE
      ALPH0 = ALPH0 - ONE/DFLOAT(NQ)
      RXIS = -EL(2) - ALPH0
      HSUM = HSUM + TAU(NQM1)
      RXI = H/HSUM
      AHATN0 = -EL(2) - RXI
      DO 235 IBACK = 1,NQ
        I = (NQ + 2) - IBACK
 235    EL(I) = EL(I) + EL(I-1)*RXIS
 240  T1 = ONE - AHATN0 + ALPH0
      T2 = ONE + DFLOAT(NQ)*T1
      TQ(2) = DABS(ALPH0*T2/T1)
      TQ(5) = DABS(T2/(EL(L)*RXI/RXIS))
      IF (NQWAIT .NE. 1) GO TO 300
      CNQM1 = RXIS/EL(L)
      T3 = ALPH0 + ONE/DFLOAT(NQ)
      T4 = AHATN0 + RXI
      ELP = T3/(ONE - T4 + T3)
      TQ(1) = DABS(ELP/CNQM1)
      HSUM = HSUM + TAU(NQ)
      RXI = H/HSUM
      T5 = ALPH0 - ONE/DFLOAT(NQ+1)
      T6 = AHATN0 - RXI
      ELP = T2/(ONE - T6 + T5)
      TQ(3) = DABS(ELP*RXI*(FLOTL + ONE)*T5)
 300  TQ(4) = CORTES*TQ(2)
      RETURN
! ----------------------- End of Subroutine VSET ------------------------
      END SUBROUTINE VSET


      SUBROUTINE VJUST (YH, LDYH, IORD)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDYH, IORD
      REAL(kind=8), INTENT(OUT) :: YH(LDYH,*)

! 
!  Type declarations for local variables --------------------------------
! 
      REAL(kind=8) :: ALPH0, ALPH1, HSUM, PROD, T1, XI, XIOLD
      INTEGER :: I, IBACK, J, JP1, LP1, NQM1, NQM2, NQP1

! -----------------------------------------------------------------------
   
      IF ((NQ .EQ. 2) .AND. (IORD .NE. 1)) RETURN
      NQM1 = NQ - 1
      NQM2 = NQ - 2
      GO TO (100, 200), METH
! -----------------------------------------------------------------------

 100  CONTINUE
      IF (IORD .EQ. 1) GO TO 180
!  Order decrease. ------------------------------------------------------
      DO 110 J = 1,LMAX
 110    EL(J) = ZERO
      EL(2) = ONE
      HSUM = ZERO
      DO 130 J = 1,NQM2
!  Construct coefficients of x*(x+xi(1))*...*(x+xi(j)). -----------------
        HSUM = HSUM + TAU(J)
        XI = HSUM/HSCAL
        JP1 = J + 1
        DO 120 IBACK = 1,JP1
          I = (J + 3) - IBACK
 120      EL(I) = EL(I)*XI + EL(I-1)
 130    CONTINUE
!  Construct coefficients of integrated polynomial. ---------------------
      DO 140 J = 2,NQM1
 140    EL(J+1) = DFLOAT(NQ)*EL(J)/DFLOAT(J)
!  Subtract correction terms from YH array. -----------------------------
      DO 170 J = 3,NQ
        DO 160 I = 1,N
 160      YH(I,J) = YH(I,J) - YH(I,L)*EL(J)
 170    CONTINUE
      RETURN
!  Order increase. ------------------------------------------------------
!  Zero out next column in YH array. ------------------------------------
 180  CONTINUE
      LP1 = L + 1
      DO 190 I = 1,N
 190    YH(I,LP1) = ZERO
      RETURN
! -----------------------------------------------------------------------
!  Stiff option...
!  Check to see if the order is being increased or decreased.
! -----------------------------------------------------------------------
 200  CONTINUE
      IF (IORD .EQ. 1) GO TO 300
!  Order decrease. ------------------------------------------------------
      DO 210 J = 1,LMAX
 210    EL(J) = ZERO
      EL(3) = ONE
      HSUM = ZERO
      DO 230 J = 1,NQM2
!  Construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). ---------------
        HSUM = HSUM + TAU(J)
        XI = HSUM/HSCAL
        JP1 = J + 1
        DO 220 IBACK = 1,JP1
          I = (J + 4) - IBACK
 220      EL(I) = EL(I)*XI + EL(I-1)
 230    CONTINUE
!  Subtract correction terms from YH array. -----------------------------
      DO 250 J = 3,NQ
        DO 240 I = 1,N
 240      YH(I,J) = YH(I,J) - YH(I,L)*EL(J)
 250    CONTINUE
      RETURN
!  Order increase. ------------------------------------------------------
 300  DO 310 J = 1,LMAX
 310    EL(J) = ZERO
      EL(3) = ONE
      ALPH0 = -ONE
      ALPH1 = ONE
      PROD = ONE
      XIOLD = ONE
      HSUM = HSCAL
      IF (NQ .EQ. 1) GO TO 340
      DO 330 J = 1,NQM1
!  Construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). ---------------
        JP1 = J + 1
        HSUM = HSUM + TAU(JP1)
        XI = HSUM/HSCAL
        PROD = PROD*XI
        ALPH0 = ALPH0 - ONE/DFLOAT(JP1)
        ALPH1 = ALPH1 + ONE/XI
        DO 320 IBACK = 1,JP1
          I = (J + 4) - IBACK
 320      EL(I) = EL(I)*XIOLD + EL(I-1)
        XIOLD = XI
 330    CONTINUE
 340  CONTINUE
      T1 = (-ALPH0 - ALPH1)/PROD
!  Load column L + 1 in YH array. ---------------------------------------
      LP1 = L + 1
      DO 350 I = 1,N
 350    YH(I,LP1) = T1*YH(I,LMAX)
!  Add correction terms to YH array. ------------------------------------
      NQP1 = NQ + 1
      DO 370 J = 3,NQP1
        CALL DAXPY (N, EL(J), YH(1:N,LP1), 1, YH(1:N,J), 1 )
 370  CONTINUE
      RETURN
! ----------------------- End of Subroutine VJUST -----------------------
      END SUBROUTINE VJUST


      SUBROUTINE VNLSD (Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM, &
                        NFLAG, RPAR, IPAR)
      IMPLICIT NONE
      INTEGER :: LDYH, IWM(:), NFLAG, IPAR
      REAL(kind=8) :: Y(:), YH(LDYH,*), VSAV(:), SAVF(:), EWT(:), ACOR(:)
      REAL(kind=8) :: WM(:), RPAR(:)

! 
! 
!  Type declarations for local variables --------------------------------
! 
      REAL(kind=8) :: CCMAX = 0.3D0, CRDOWN = 0.3D0
      REAL(kind=8) :: CSCALE, DCON, DEL, DELP
      REAL(kind=8) :: RDIV = 2.0D0
      INTEGER :: I, IERPJ, IERSL, M, MAXCOR = 3, MSBP = 20
! -----------------------------------------------------------------------

      IF (JSTART .EQ. 0) NSLP = 0
      IF (NFLAG .EQ. 0) ICF = 0
      IF (NFLAG .EQ. -2) IPUP = MITER
      IF ( (JSTART .EQ. 0) .OR. (JSTART .EQ. -1) ) IPUP = MITER
!  If this is functional iteration, set CRATE .eq. 1 and drop to 220
      IF (MITER .EQ. 0) THEN
        CRATE = ONE
        GO TO 220
      ENDIF
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
      DRC = DABS(RC-ONE)
      IF (DRC .GT. CCMAX .OR. NST .GE. NSLP+MSBP) IPUP = MITER
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 220  M = 0
      DELP = ZERO
      CALL DCOPY (N, YH(1:N,1), 1, Y, 1 )
      CALL FEX2(N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      IF (IPUP .LE. 0) GO TO 250
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
      CALL VJAC(Y, YH, LDYH, EWT, ACOR, SAVF, WM, IWM, IERPJ, RPAR, IPAR)
      IPUP = 0
      RC = ONE
      DRC = ZERO
      CRATE = ONE
      NSLP = NST
!  If matrix is singular, take error return to force cut in step size. --
      IF (IERPJ .NE. 0) GO TO 430
 250  DO 260 I = 1,N
 260    ACOR(I) = ZERO
!  This is a looping point for the corrector iteration. -----------------
 270  IF (MITER .NE. 0) GO TO 350
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
      DO 280 I = 1,N
 280    SAVF(I) = RL1*(H*SAVF(I) - YH(I,2))
      DO 290 I = 1,N
 290    Y(I) = SAVF(I) - ACOR(I)
      DEL = VNORM (N, Y, EWT)
      DO 300 I = 1,N
 300    Y(I) = YH(I,1) + SAVF(I)
      CALL DCOPY (N, SAVF, 1, ACOR, 1)
      GO TO 400
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 350  DO 360 I = 1,N
 360    Y(I) = (RL1*H)*SAVF(I) - (RL1*YH(I,2) + ACOR(I))
      CALL VSOL (WM, IWM, Y, IERSL)
      NNI = NNI + 1
      IF (IERSL .GT. 0) GO TO 410
      IF (METH .EQ. 2 .AND. RC .NE. ONE) THEN
        CSCALE = TWO/(ONE + RC)
        CALL DSCAL (N, CSCALE, Y, 1)
      ENDIF
      DEL = VNORM (N, Y, EWT)
      CALL DAXPY (N, ONE, Y, 1, ACOR, 1)
      DO 380 I = 1,N
 380    Y(I) = YH(I,1) + ACOR(I)
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
 400  IF (M .NE. 0) CRATE = DMAX1(CRDOWN*CRATE,DEL/DELP)
      DCON = DEL*DMIN1(ONE,CRATE)/TQ(4)
      IF (DCON .LE. ONE) GO TO 450
      M = M + 1
      IF (M .EQ. MAXCOR) GO TO 410
      IF (M .GE. 2 .AND. DEL .GT. RDIV*DELP) GO TO 410
      DELP = DEL
      CALL FEX2(N, TN, Y, SAVF, RPAR, IPAR)
      NFE = NFE + 1
      GO TO 270
! 
 410  IF (MITER .EQ. 0 .OR. JCUR .EQ. 1) GO TO 430
      ICF = 1
      IPUP = MITER
      GO TO 220
! 
 430  CONTINUE
      NFLAG = -1
      ICF = 2
      IPUP = MITER
      RETURN
! 
!  Return for successful step. ------------------------------------------
 450  NFLAG = 0
      JCUR = 0
      ICF = 0
      IF (M .EQ. 0) ACNRM = DEL
      IF (M .GT. 0) ACNRM = VNORM (N, ACOR, EWT)
      RETURN
! ----------------------- End of Subroutine VNLSD -----------------------
      END SUBROUTINE VNLSD


      SUBROUTINE VJAC (Y, YH, LDYH, EWT, FTEM, SAVF, WM, IWM, &
                       IERPJ, RPAR, IPAR)
      IMPLICIT NONE
      INTEGER :: LDYH, IWM(:), IERPJ, IPAR
      REAL(kind=8) :: Y(:), YH(LDYH,*), EWT(:), FTEM(:), SAVF(:), WM(:), RPAR(:)

! 
!  Type declarations for local variables --------------------------------
! 
      REAL(kind=8) :: CON, DI, FAC, HRL1, R, R0, SRUR
      REAL(kind=8) :: YI, YJ, YJJ
      INTEGER :: I, I1, I2, IER, II, J, J1, JJ, JOK, LENP, MBA, MBAND
      INTEGER :: MEB1, MEBAND, ML, ML3, MU, NP1
! -----------------------------------------------------------------------

      IERPJ = 0
      HRL1 = H*RL1
!  See whether J should be evaluated (JOK = -1) or not (JOK = 1). -------
      JOK = JSV
      IF (JSV .EQ. 1) THEN
        IF (NST .EQ. 0 .OR. NST .GT. NSLJ+MSBJ) JOK = -1
        IF (ICF .EQ. 1 .AND. DRC .LT. CCMXJ) JOK = -1
        IF (ICF .EQ. 2) JOK = -1
      ENDIF
!  End of setting JOK. --------------------------------------------------
! 
      IF (JOK .EQ. -1 .AND. MITER .EQ. 1) THEN
!  If JOK = -1 and MITER = 1, call JEX to evaluate Jacobian. ------------
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      LENP = N*N
      DO 110 I = 1,LENP
 110    WM(I+2) = ZERO
      CALL JEX(N, TN, Y, 0, 0, WM(3:3+N*N-1), N, RPAR, IPAR)
      IF (JSV .EQ. 1) CALL DCOPY (LENP, WM(3:3+LENP-1), 1, WM(LOCJS:LOCJS+LENP-1), 1)
      ENDIF
! 
      IF (JOK .EQ. -1 .AND. MITER .EQ. 2) THEN
!  If MITER = 2, make N calls to FEX2 to approximate the Jacobian. ---------
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      FAC = VNORM (N, SAVF, EWT)
      R0 = THOU*DABS(H)*UROUND*DFLOAT(N)*FAC
      IF (R0 .EQ. ZERO) R0 = ONE
      SRUR = WM(1)
      J1 = 2
      DO 230 J = 1,N
        YJ = Y(J)
        R = DMAX1(SRUR*DABS(YJ),R0/EWT(J))
        Y(J) = Y(J) + R
        FAC = ONE/R
        CALL FEX2(N, TN, Y, FTEM, RPAR, IPAR)
        DO 220 I = 1,N
 220      WM(I+J1) = (FTEM(I) - SAVF(I))*FAC
        Y(J) = YJ
        J1 = J1 + N
 230    CONTINUE
      NFE = NFE + N
      LENP = N*N
      IF (JSV .EQ. 1) CALL DCOPY (LENP, WM(3:3+LENP-1), 1, WM(LOCJS:LOCJS+LENP-1), 1)
      ENDIF
! 
      IF (JOK .EQ. 1 .AND. (MITER .EQ. 1 .OR. MITER .EQ. 2)) THEN
      JCUR = 0
      LENP = N*N
      CALL DCOPY (LENP, WM(LOCJS:LOCJS+LENP-1), 1, WM(3:3+LENP-1), 1)
      ENDIF
! 
      IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
!  Multiply Jacobian by scalar, add identity, and do LU decomposition. --
      CON = -HRL1
      CALL DSCAL (LENP, CON, WM(3:3+LENP-1), 1)
      J = 3
      NP1 = N + 1
      DO 250 I = 1,N
        WM(J) = WM(J) + ONE
 250    J = J + NP1
      NLU = NLU + 1
      CALL DGEFA (WM(3:3+N-1), N, N, IWM(31:31+N-1), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
      ENDIF
!  End of code block for MITER = 1 or 2. --------------------------------
! 
      IF (MITER .EQ. 3) THEN
!  If MITER = 3, construct a diagonal approximation to J and P. ---------
      NJE = NJE + 1
      JCUR = 1
      WM(2) = HRL1
      R = RL1*PT1
      DO 310 I = 1,N
 310    Y(I) = Y(I) + R*(H*SAVF(I) - YH(I,2))
      CALL FEX2(N, TN, Y, WM(3:3+N-1), RPAR, IPAR)
      NFE = NFE + 1
      DO 320 I = 1,N
        R0 = H*SAVF(I) - YH(I,2)
        DI = PT1*R0 - H*(WM(I+2) - SAVF(I))
        WM(I+2) = ONE
        IF (DABS(R0) .LT. UROUND/EWT(I)) GO TO 320
        IF (DABS(DI) .EQ. ZERO) GO TO 330
        WM(I+2) = PT1*R0/DI
 320    CONTINUE
      RETURN
 330  IERPJ = 1
      RETURN
      ENDIF
!  End of code block for MITER = 3. -------------------------------------
! 
!  Set constants for MITER = 4 or 5. ------------------------------------
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      LENP = MEBAND*N
! 
      IF (JOK .EQ. -1 .AND. MITER .EQ. 4) THEN
!  If JOK = -1 and MITER = 4, call JEX to evaluate Jacobian. ------------
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      DO 410 I = 1,LENP
 410    WM(I+2) = ZERO
      CALL JEX(N, TN, Y, ML, MU, WM(ML3:ML3+N*MEBAND-1), MEBAND, RPAR, IPAR)
      IF (JSV .EQ. 1) THEN
          CALL DACOPY (MBAND, N, WM(ML3:ML3 + MEBAND*N - 1), MEBAND, WM(LOCJS:LOCJS + MBAND*N -1), MBAND)
      ENDIF

      ENDIF
! 
      IF (JOK .EQ. -1 .AND. MITER .EQ. 5) THEN
!  If MITER = 5, make N calls to F to approximate the Jacobian. ---------
      NJE = NJE + 1
      NSLJ = NST
      JCUR = 1
      MBA = MIN0(MBAND,N)
      MEB1 = MEBAND - 1
      SRUR = WM(1)
      FAC = VNORM (N, SAVF, EWT)
      R0 = THOU*DABS(H)*UROUND*DFLOAT(N)*FAC
      IF (R0 .EQ. ZERO) R0 = ONE
      DO 560 J = 1,MBA
        DO 530 I = J,N,MBAND
          YI = Y(I)
          R = DMAX1(SRUR*DABS(YI),R0/EWT(I))
 530      Y(I) = Y(I) + R
        CALL FEX2(N, TN, Y, FTEM, RPAR, IPAR)
        DO 550 JJ = J,N,MBAND
          Y(JJ) = YH(JJ,1)
          YJJ = Y(JJ)
          R = DMAX1(SRUR*DABS(YJJ),R0/EWT(JJ))
          FAC = ONE/R
          I1 = MAX0(JJ-MU,1)
          I2 = MIN0(JJ+ML,N)
          II = JJ*MEB1 - ML + 2
          DO 540 I = I1, I2
 540        WM(II+I) = (FTEM(I) - SAVF(I))*FAC
 550      CONTINUE
 560    CONTINUE
      NFE = NFE + MBA
      IF (JSV .EQ. 1) THEN
          CALL DACOPY (MBAND, N, WM(ML3:ML3 + MEBAND*N - 1), MEBAND, WM(LOCJS:LOCJS + MBAND*N -1), MBAND)
      ENDIF

      ENDIF
! 
      IF (JOK .EQ. 1) THEN
      JCUR = 0
      CALL DACOPY (MBAND, N, WM(LOCJS:LOCJS + MBAND*N -1), MBAND, WM(ML3:ML3 + MEBAND*N - 1), MEBAND)
      ENDIF
! 
!  Multiply Jacobian by scalar, add identity, and do LU decomposition.
      CON = -HRL1
      CALL DSCAL (LENP, CON, WM(3:3+LENP-1), 1 )
      II = MBAND + 2
      DO 580 I = 1,N
        WM(II) = WM(II) + ONE
 580    II = II + MEBAND
      NLU = NLU + 1
      CALL DGBFA (WM(3:3+MEBAND*N-1), MEBAND, N, ML, MU, IWM(31:31+N-1), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
!  End of code block for MITER = 4 or 5. --------------------------------
! 
! ----------------------- End of Subroutine VJAC ------------------------
      END SUBROUTINE VJAC


      SUBROUTINE DACOPY (NROW, NCOL, A, NROWA, B, NROWB)
      IMPLICIT NONE
      INTEGER :: NROW, NCOL, NROWA, NROWB
      REAL(kind=8) :: A(NROWA,NCOL), B(NROWB,NCOL)
      INTEGER :: IC
! 
      DO 20 IC = 1,NCOL
        CALL DCOPY (NROW, A(1:NROW,IC), 1, B(1:NROW,IC), 1)
 20     CONTINUE
! 
      RETURN
! ----------------------- End of Subroutine DACOPY ----------------------
      END SUBROUTINE DACOPY


      SUBROUTINE VSOL (WM, IWM, X, IERSL)
      IMPLICIT NONE
      INTEGER :: IWM(:), IERSL
      REAL(kind=8) :: WM(:), X(:)

! 
!  Type declarations for local variables --------------------------------
! 
      INTEGER :: I, MEBAND, ML, MU
      REAL(kind=8) :: DI, HRL1, PHRL1, R

! -----------------------------------------------------------------------
! 
      IERSL = 0
      GO TO (100, 100, 300, 400, 400), MITER
 100  CALL DGESL (WM(3:3+N-1), N, N, IWM(31:31+N-1), X, 0)
      RETURN
! 
 300  PHRL1 = WM(2)
      HRL1 = H*RL1
      WM(2) = HRL1
      IF (HRL1 .EQ. PHRL1) GO TO 330
      R = HRL1/PHRL1
      DO 320 I = 1, N
        DI = ONE - R*(ONE - ONE/WM(I+2))
        IF (DABS(DI) .EQ. ZERO) GO TO 390
 320    WM(I+2) = ONE/DI
! 
 330  DO 340 I = 1, N
 340    X(I) = WM(I+2)*X(I)
      RETURN
 390  IERSL = 1
      RETURN
! 
 400  ML = IWM(1)
      MU = IWM(2)
      MEBAND = 2*ML + MU + 1
      CALL DGBSL (WM(3:3+MEBAND*N-1), MEBAND, N, ML, MU, IWM(31:31+N-1), X, 0)
      RETURN
! ----------------------- End of Subroutine VSOL ------------------------
      END SUBROUTINE VSOL


      SUBROUTINE VSRCO (RSAV, ISAV, JOB)
      IMPLICIT NONE
      INTEGER :: ISAV(:), JOB
      REAL(kind=8) :: RSAV(:)
      INTEGER :: I, LENIV1 = 33, LENIV2 = 8, LENRV1 = 48, LENRV2 = 1
 
      IF (JOB .EQ. 2) GO TO 100
      DO 10 I = 1,LENRV1
 10     RSAV(I) = RVOD1(I)
      DO 15 I = 1,LENRV2
 15     RSAV(LENRV1+I) = RVOD2(I)
 
      DO 20 I = 1,LENIV1
 20     ISAV(I) = IVOD1(I)
      DO 25 I = 1,LENIV2
 25     ISAV(LENIV1+I) = IVOD2(I)
 
      RETURN
 
 100  CONTINUE
      DO 110 I = 1,LENRV1
 110     RVOD1(I) = RSAV(I)
      DO 115 I = 1,LENRV2
 115     RVOD2(I) = RSAV(LENRV1+I)
 
      DO 120 I = 1,LENIV1
 120     IVOD1(I) = ISAV(I)
      DO 125 I = 1,LENIV2
 125     IVOD2(I) = ISAV(LENIV1+I)
 
      RETURN
! ----------------------- End of Subroutine VSRCO -----------------------
      END SUBROUTINE VSRCO

      SUBROUTINE EWSET (N, ITOL, RTOL, ATOL, YCUR, EWT)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N, ITOL
      REAL(kind=8), INTENT(IN) :: RTOL(N), ATOL(N), YCUR(N)
      REAL(kind=8), INTENT(OUT) :: EWT(N)
      INTEGER :: I
! 
      GO TO (10, 20, 30, 40), ITOL
 10   CONTINUE
      DO 15 I = 1, N
 15     EWT(I) = RTOL(1)*DABS(YCUR(I)) + ATOL(1)
      RETURN
 20   CONTINUE
      DO 25 I = 1, N
 25     EWT(I) = RTOL(1)*DABS(YCUR(I)) + ATOL(I)
      RETURN
 30   CONTINUE
      DO 35 I = 1, N
 35     EWT(I) = RTOL(I)*DABS(YCUR(I)) + ATOL(1)
      RETURN
 40   CONTINUE
      DO 45 I = 1, N
 45     EWT(I) = RTOL(I)*DABS(YCUR(I)) + ATOL(I)
      RETURN
! ----------------------- End of Subroutine EWSET -----------------------
      END SUBROUTINE EWSET


      REAL(kind=8) FUNCTION VNORM (N, V, W)
      IMPLICIT NONE
      INTEGER :: N
      REAL(kind=8) :: V(N), W(N)
      INTEGER :: I
      REAL(kind=8) :: SUM
! 
      SUM = 0.0D0
      DO 10 I = 1, N
 10     SUM = SUM + (V(I)*W(I))**2
      VNORM = DSQRT(SUM/DFLOAT(N))
      RETURN
! ----------------------- End of Function VNORM -------------------------
      END FUNCTION VNORM


      REAL(kind=8) FUNCTION D1MACH (IDUM)
      IMPLICIT NONE
      INTEGER :: IDUM
      REAL(kind=8) :: U, COMP

      U = 1.0D0
 10   U = U*0.5D0
      COMP = 1.0D0 + U
      IF (COMP .NE. 1.0D0) GO TO 10
      D1MACH = U*2.0D0
      RETURN
! ----------------------- End of Function D1MACH ------------------------
      END FUNCTION D1MACH 


      SUBROUTINE XERRWV (MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2)
      IMPLICIT NONE
      INTEGER :: NMES, NERR, LEVEL, NI, I1, I2, NR
      REAL(kind=8) :: R1, R2
      CHARACTER(LEN=1) :: MSG(NMES)
      INTEGER :: I, LUNIT, MESFLG
! 
!  Get message print flag and logical unit number. ----------------------
      MESFLG = MFLGSV (0,.FALSE.)
      LUNIT = LUNSAV (0,.FALSE.)
      IF (MESFLG .EQ. 0) GO TO 100
!  Write the message. ---------------------------------------------------
      WRITE (LUNIT,10) (MSG(I),I=1,NMES)
 10   FORMAT(1X,80A1)
      IF (NI .EQ. 1) WRITE (LUNIT, 20) I1
 20   FORMAT(6X,'In above message,  I1 =',I10)
      IF (NI .EQ. 2) WRITE (LUNIT, 30) I1,I2
 30   FORMAT(6X,'In above message,  I1 =',I10,3X,'I2 =',I10)
      IF (NR .EQ. 1) WRITE (LUNIT, 40) R1
 40   FORMAT(6X,'In above message,  R1 =',D21.13)
      IF (NR .EQ. 2) WRITE (LUNIT, 50) R1,R2
 50   FORMAT(6X,'In above,  R1 =',D21.13,3X,'R2 =',D21.13)
!  Abort the run if LEVEL = 2. ------------------------------------------
 100  IF (LEVEL .NE. 2) RETURN
      STOP
! ----------------------- End of Subroutine XERRWV ----------------------
      END SUBROUTINE XERRWV


      SUBROUTINE XSETF (MFLAG)
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: MFLAG
      INTEGER :: JUNK
      IF (MFLAG .EQ. 0 .OR. MFLAG .EQ. 1) JUNK = MFLGSV (MFLAG,.TRUE.)
      RETURN
      END SUBROUTINE XSETF 


      SUBROUTINE XSETUN (LUN)
      IMPLICIT NONE
      INTEGER :: LUN, JUNK
      IF (LUN .GT. 0) JUNK = LUNSAV (LUN,.TRUE.)
      RETURN
      END SUBROUTINE XSETUN


      INTEGER FUNCTION MFLGSV (IVALUE, ISET)
      IMPLICIT NONE
      LOGICAL :: ISET
      INTEGER :: IVALUE
      INTEGER, SAVE :: MESFLG = 1
      MFLGSV = MESFLG
      IF (ISET) MESFLG = IVALUE
      RETURN
      END FUNCTION MFLGSV


      INTEGER FUNCTION LUNSAV (IVALUE, ISET)
      IMPLICIT NONE
      LOGICAL :: ISET
      INTEGER :: IVALUE
      INTEGER, SAVE :: LUNIT = 9
      LUNSAV = LUNIT
      IF (ISET) LUNIT = IVALUE
      RETURN
      END FUNCTION LUNSAV

! 
! 
! 
      subroutine dgefa(a,lda,n,ipvt,info)
      IMPLICIT NONE
      integer :: lda, n, ipvt(n), info
      REAL(kind=8), DIMENSION(lda,n) :: a
!
!     dgefa factors a double precision matrix by gaussian elimination.
!
      REAL(kind=8) :: t
      integer :: j, k, kp1, l, nm1
!
!
!     gaussian elimination with partial pivoting
!
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
!
!        find l = pivot index
!
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
!
!        zero pivot implies this column already triangularized
!
         if (a(l,k) .eq. 0.0d0) go to 40
!
!           interchange if necessary
!
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
!
!           compute multipliers
!
            t = -1.0d0 / a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
!
!           row elimination with column indexing
!
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0d0) info = n
      return
      end subroutine dgefa


      subroutine dgesl(a,lda,n,ipvt,b,job)
      IMPLICIT NONE
      integer :: lda, n, ipvt(n), job
      REAL(kind=8) :: a(lda,n), b(n)
!
!     dgesl solves the double precision system
!     a * x = b  or  trans(a) * x = b
!     using the factors computed by dgeco or dgefa.
!
!
      REAL(kind=8) :: t
      integer :: k, kb, l, nm1
!
      nm1 = n - 1
      if (job .ne. 0) go to 50
!
!        job = 0 , solve  a * x = b
!        first solve  l*y = b
!
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
!
!        now solve  u*x = y
!
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
!
!        job = nonzero, solve  trans(a) * x = b
!        first solve  trans(u)*y = b
!
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
!
!        now solve trans(l)*x = y
!
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end subroutine dgesl


      SUBROUTINE DGBFA (ABD, LDA, N, ML, MU, IPVT, INFO)
!-----------------------------------------------------------------------------
! L3 updated linpack 1990
! DECK DGBFA
!
! BEGIN PROLOGUE  DGBFA
! PURPOSE  Factor a band matrix using Gaussian elimination.
! CATEGORY  D2A2
! TYPE      REAL(kind=8) (SGBFA-S, DGBFA-D, CGBFA-C)
! KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION
! AUTHOR  Moler, C. B., (U. of New Mexico)
! DESCRIPTION
!
!     DGBFA factors a double precision band matrix by elimination.
!
!     DGBFA is usually called by DGBCO, but it can be called
!     directly with a saving in time if  RCOND  is not needed.
!
!     On Entry
!
!        ABD     REAL(kind=8)(LDA, N)
!                contains the matrix in band storage.  The columns
!                of the matrix are stored in the columns of  ABD  and
!                the diagonals of the matrix are stored in rows
!                ML+1 through 2*ML+MU+1 of  ABD .
!                See the comments below for details.
!
!        LDA     INTEGER
!                the leading dimension of the array  ABD .
!                LDA must be .GE. 2*ML + MU + 1 .
!
!        N       INTEGER
!                the order of the original matrix.
!
!        ML      INTEGER
!                number of diagonals below the main diagonal.
!                0 .LE. ML .LT.  N .
!
!        MU      INTEGER
!                number of diagonals above the main diagonal.
!                0 .LE. MU .LT.  N .
!                More efficient if  ML .LE. MU .
!     On Return
!
!        ABD     an upper triangular matrix in band storage and
!                the multipliers which were used to obtain it.
!                The factorization can be written  A = L*U  where
!                L  is a product of permutation and unit lower
!                triangular matrices and  U  is upper triangular.
!
!        IPVT    INTEGER(N)
!                an integer vector of pivot indices.
!
!        INFO    INTEGER
!                = 0  normal value.
!                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
!                     condition for this subroutine, but it does
!                     indicate that DGBSL will divide by zero if
!                     called.  Use  RCOND  in DGBCO for a reliable
!                     indication of singularity.
!
!     Band Storage
!
!           If  A  is a band matrix, the following program segment
!           will set up the input.
!
!                   ML = (band width below the diagonal)
!                   MU = (band width above the diagonal)
!                   M = ML + MU + 1
!                   DO 20 J = 1, N
!                      I1 = MAX(1, J-MU)
!                      I2 = MIN(N, J+ML)
!                      DO 10 I = I1, I2
!                         K = I - J + M
!                         ABD(K,J) = A(I,J)
!                10    CONTINUE
!                20 CONTINUE
!
!           This uses rows  ML+1  through  2*ML+MU+1  of  ABD .
!           In addition, the first  ML  rows in  ABD  are used for
!           elements generated during the triangularization.
!           The total number of rows needed in  ABD  is  2*ML+MU+1 .
!           The  ML+MU by ML+MU  upper left triangle and the
!           ML by ML  lower right triangle are not referenced.
!
! REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
!                 Stewart, LINPACK Users' Guide, SIAM, 1979.
! ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
! REVISION HISTORY  (YYMMDD)
!   780814  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   890831  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
! END PROLOGUE  DGBFA
!-----------------------------------------------------------------------------
      IMPLICIT NONE
 
      INTEGER :: LDA, N, ML, MU, INFO
      INTEGER, DIMENSION(N) :: IPVT
      REAL(kind=8), DIMENSION(LDA,N) :: ABD
      REAL(kind=8) :: T
      INTEGER :: I, I0, J, JU, JZ, J0, J1, K, KP1, L, LM, M, MM, NM1

      m = ml + mu + 1
      info = 0
! 
!      ZERO INITIAL FILL-IN COLUMNS
! 
      j0 = mu + 2
      j1 = min(n,m) - 1
      IF (j1 .LT. j0) GO TO 30
      DO 20 jz = j0, j1
         i0 = m + 1 - jz
         DO 10 i = i0, ml
            abd(i,jz) = 0.0d0
   10    CONTINUE
   20 CONTINUE
   30 CONTINUE
      jz = j1
      ju = 0
! 
!      GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
! 
      nm1 = n - 1
      IF (nm1 .LT. 1) GO TO 130
      DO 120 k = 1, nm1
         kp1 = k + 1
! 
!         ZERO NEXT FILL-IN COLUMN
! 
         jz = jz + 1
         IF (jz .GT. n) GO TO 50
         IF (ml .LT. 1) GO TO 50
            DO 40 i = 1, ml
               abd(i,jz) = 0.0d0
   40       CONTINUE
   50    CONTINUE
! 
!         FIND L = PIVOT INDEX
! 
         lm = min(ml,n-k)
         l = idamax(lm+1,abd(m,k),1) + m - 1
         ipvt(k) = l + k - m
! 
!         ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
! 
         IF (abd(l,k) .EQ. 0.0d0) GO TO 100
! 
!            INTERCHANGE IF NECESSARY
! 
            IF (l .EQ. m) GO TO 60
               t = abd(l,k)
               abd(l,k) = abd(m,k)
               abd(m,k) = t
   60       CONTINUE
! 
!            COMPUTE MULTIPLIERS
! 
            t = -1.0d0/abd(m,k)
            CALL dscal(lm,t,abd(m+1,k),1)
! 
!            ROW ELIMINATION WITH COLUMN INDEXING
! 
            ju = min(max(ju,mu+ipvt(k)),n)
            mm = m
            IF (ju .LT. kp1) GO TO 90
            DO 80 j = kp1, ju
               l = l - 1
               mm = mm - 1
               t = abd(l,j)
               IF (l .EQ. mm) GO TO 70
                  abd(l,j) = abd(mm,j)
                  abd(mm,j) = t
   70          CONTINUE
               CALL daxpy(lm,t,abd(m+1,k),1,abd(mm+1,j),1)
   80       CONTINUE
   90       CONTINUE
         GO TO 110
  100    CONTINUE
            info = k
  110    CONTINUE
  120 CONTINUE
  130 CONTINUE
      ipvt(n) = n
      IF (abd(m,n) .EQ. 0.0d0) info = n
      RETURN
      END SUBROUTINE DGBFA


! L3 updated BLAS 1993 
! 
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       REAL(kind=8) DX(*),DY(*)
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    DCOPY copies a vector, x, to a vector, y.
!>    uses unrolled loops for increments equal to 1.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in] DX
!> \verbatim
!>          DX is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>         storage spacing between elements of DX
!> \endverbatim
!>
!> \param[out] DY
!> \verbatim
!>          DY is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>         storage spacing between elements of DY
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \date December 2016
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
!
!  -- Reference BLAS level1 routine (version 3.7.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
      IMPLICIT NONE

!     .. Scalar Arguments ..
      INTEGER :: INCX, INCY, N
!     ..
!     .. Array Arguments ..
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCX)) :: DX
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCY)) :: DY
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER :: I, IX, IY, M, MP1
     
!     ..
      IF (n.LE.0) RETURN
      IF (incx.EQ.1 .AND. incy.EQ.1) THEN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
         m = mod(n,7)
         IF (m.NE.0) THEN
            DO i = 1,m
               dy(i) = dx(i)
            END DO
            IF (n.LT.7) RETURN
         END IF
         mp1 = m + 1
         DO i = mp1,n,7
            dy(i) = dx(i)
            dy(i+1) = dx(i+1)
            dy(i+2) = dx(i+2)
            dy(i+3) = dx(i+3)
            dy(i+4) = dx(i+4)
            dy(i+5) = dx(i+5)
            dy(i+6) = dx(i+6)
         END DO
      ELSE
!
!        code for unequal increments or equal increments
!          not equal to 1
!
         ix = 1
         iy = 1
         IF (incx.LT.0) ix = (-n+1)*incx + 1
         IF (incy.LT.0) iy = (-n+1)*incy + 1
         DO i = 1, n
            dy(iy) = dx(ix)
            ix = ix + incx
            iy = iy + incy
         END DO
      END IF
      RETURN
      END SUBROUTINE DCOPY


! L3 updated BLAS 1993
! 
! 
!   =========== DOCUMENTATION ===========
! 
!  Online html documentation available at
!             http://www.netlib.org/lapack/explore-html/
! 
!   Definition:
!   ===========
! 
!        REAL(kind=8) FUNCTION DDOT(N,DX,INCX,DY,INCY)
! 
!        .. Scalar Arguments ..
!        INTEGER INCX,INCY,N
!        ..
!        .. Array Arguments ..
!        REAL(kind=8) :: DX(*),DY(*)
!        ..
! 
! 
! > \par Purpose:
!   =============
! >
! > \verbatim
! >
! >    DDOT forms the dot product of two vectors.
! >    uses unrolled loops for increments equal to one.
! > \endverbatim
! 
!   Arguments:
!   ==========
! 
! > \param[in] N
! > \verbatim
! >          N is INTEGER
! >         number of elements in input vector(s)
! > \endverbatim
! >
! > \param[in] DX
! > \verbatim
! >          DX is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
! > \endverbatim
! >
! > \param[in] INCX
! > \verbatim
! >          INCX is INTEGER
! >         storage spacing between elements of DX
! > \endverbatim
! >
! > \param[in] DY
! > \verbatim
! >          DY is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
! > \endverbatim
! >
! > \param[in] INCY
! > \verbatim
! >          INCY is INTEGER
! >         storage spacing between elements of DY
! > \endverbatim
! 
!   Authors:
!   ========
! 
! > \author Univ. of Tennessee
! > \author Univ. of California Berkeley
! > \author Univ. of Colorado Denver
! > \author NAG Ltd.
! 
! > \date December 2016
! 
! > \ingroup double_blas_level1
! 
! > \par Further Details:
!   =====================
! >
! > \verbatim
! >
! >     jack dongarra, linpack, 3/11/78.
! >     modified 12/3/93, array(1) declarations changed to array(*)
! > \endverbatim
! >
!   =====================================================================
      REAL(kind=8) FUNCTION ddot(N,DX,INCX,DY,INCY)
! 
!   -- Reference BLAS level1 routine (version 3.7.0) --
!   -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!   -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!      December 2016
! 
      IMPLICIT NONE

!      .. Scalar Arguments ..
      INTEGER :: INCX, INCY, N
!      ..
!      .. Array Arguments ..
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCX)) :: DX
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCY)) :: DY
!      ..
! 
!   =====================================================================
! 
!      .. Local Scalars ..
      REAL(kind=8) :: DTEMP
      INTEGER :: I, IX, IY, M, MP1

      ddot = 0.0d0
      dtemp = 0.0d0
      IF (n.LE.0) RETURN
      IF (incx.EQ.1 .AND. incy.EQ.1) THEN
! 
!         code for both increments equal to 1
! 
! 
!         clean-up loop
! 
         m = mod(n,5)
         IF (m.NE.0) THEN
            DO i = 1,m
               dtemp = dtemp + dx(i)*dy(i)
            END DO
            IF (n.LT.5) THEN
               ddot=dtemp
            RETURN
            END IF
         END IF
         mp1 = m + 1
         DO i = mp1,n,5
          dtemp = dtemp + dx(i)*dy(i) + dx(i+1)*dy(i+1) + &
                  dx(i+2)*dy(i+2) + dx(i+3)*dy(i+3) + dx(i+4)*dy(i+4)
         END DO
      ELSE
!
!        code for unequal increments or equal increments
!          not equal to 1
!
         ix = 1
         iy = 1
         IF (incx.LT.0) ix = (-n+1)*incx + 1
         IF (incy.LT.0) iy = (-n+1)*incy + 1
         DO i = 1, n
            dtemp = dtemp + dx(ix)*dy(iy)
            ix = ix + incx
            iy = iy + incy
         END DO
      END IF
      ddot = dtemp
      RETURN
      END FUNCTION ddot


      SUBROUTINE DGBSL(ABD, LDA, N, ML, MU, IPVT, B, JOB)
!-----------------------------------------------------------------------------
! L3 updated linpack 1992
! DECK DGBSL
! BEGIN PROLOGUE  DGBSL
! PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using
!          the factors computed by DGBCO or DGBFA.
! CATEGORY  D2A2
! TYPE      REAL(kind=8) (SGBSL-S, DGBSL-D, CGBSL-C)
! KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
! AUTHOR  Moler, C. B., (U. of New Mexico)
! DESCRIPTION
! 
!      DGBSL solves the double precision band system
!      A * X = B  or  TRANS(A) * X = B
!      using the factors computed by DGBCO or DGBFA.
! 
!      On Entry
! 
!        ABD     REAL(kind=8)(LDA, N)
!                 the output from DGBCO or DGBFA.
! 
!         LDA     INTEGER
!                 the leading dimension of the array  ABD .
! 
!         N       INTEGER
!                 the order of the original matrix.
! 
!         ML      INTEGER
!                 number of diagonals below the main diagonal.
! 
!         MU      INTEGER
!                 number of diagonals above the main diagonal.
! 
!         IPVT    INTEGER(N)
!                 the pivot vector from DGBCO or DGBFA.
! 
!         B       REAL(kind=8)(N)
!                 the right hand side vector.
! 
!         JOB     INTEGER
!                 = 0         to solve  A*X = B ,
!                 = nonzero   to solve  TRANS(A)*X = B , where
!                             TRANS(A)  is the transpose.
! 
!      On Return
! 
!         B       the solution vector  X .
! 
!      Error Condition
! 
!         A division by zero will occur if the input factor contains a
!         zero on the diagonal.  Technically this indicates singularity
!         but it is often caused by improper arguments or improper
!         setting of LDA .  It will not occur if the subroutines are
!         called correctly and if DGBCO has set RCOND .GT. 0.0
!         or DGBFA has set INFO .EQ. 0 .
! 
!      To compute  INVERSE(A) * C  where  C  is a matrix
!      with  P  columns
!            CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
!            IF (RCOND is too small) GO TO ...
!            DO 10 J = 1, P
!               CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
!         10 CONTINUE
! 
! ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
!                  Stewart, LINPACK Users' Guide, SIAM, 1979.
! ***ROUTINES CALLED  DAXPY, DDOT
! ***REVISION HISTORY  (YYMMDD)
!    780814  DATE WRITTEN
!    890531  Changed all specific intrinsics to generic.  (WRB)
!    890831  Modified array declarations.  (WRB)
!    890831  REVISION DATE from Version 3.2
!    891214  Prologue converted to Version 4.0 format.  (BAB)
!    900326  Removed duplicate information from DESCRIPTION section.
!            (WRB)
!    920501  Reformatted the REFERENCES section.  (WRB)
! ***END PROLOGUE  DGBSL
!-----------------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER :: LDA, N, ML, MU, IPVT(N), JOB
      REAL(kind=8) :: ABD(LDA,N), B(N)
      REAL(kind=8) :: T
      INTEGER :: K, KB, L, LA, LB, LM, M, NM1

      m = mu + ml + 1
      nm1 = n - 1
      IF (job .NE. 0) GO TO 50
! 
!         JOB = 0 , SOLVE  A * X = B
!         FIRST SOLVE L*Y = B
! 
         IF (ml .EQ. 0) GO TO 30
         IF (nm1 .LT. 1) GO TO 30
            DO 20 k = 1, nm1
               lm = min(ml,n-k)
               l = ipvt(k)
               t = b(l)
               IF (l .EQ. k) GO TO 10
                  b(l) = b(k)
                  b(k) = t
   10          CONTINUE
               CALL daxpy(lm,t,abd(m+1,k),1,b(k+1),1)
   20       CONTINUE
   30    CONTINUE
! 
!         NOW SOLVE  U*X = Y
! 
         DO 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/abd(m,k)
            lm = min(k,m) - 1
            la = m - lm
            lb = k - lm
            t = -b(k)
            CALL daxpy(lm,t,abd(la,k),1,b(lb),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
! 
!         JOB = NONZERO, SOLVE  TRANS(A) * X = B
!         FIRST SOLVE  TRANS(U)*Y = B
! 
         DO 60 k = 1, n
            lm = min(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
   60    CONTINUE
! 
!         NOW SOLVE TRANS(L)*X = Y
! 
         IF (ml .EQ. 0) GO TO 90
         IF (nm1 .LT. 1) GO TO 90
            DO 80 kb = 1, nm1
               k = n - kb
               lm = min(ml,n-k)
               b(k) = b(k) + ddot(lm,abd(m+1,k),1,b(k+1),1)
               l = ipvt(k)
               IF (l .EQ. k) GO TO 70
                  t = b(l)
                  b(l) = b(k)
                  b(k) = t
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END SUBROUTINE dgbsl


!L3 swap updated BLAS 12/3/93
!> \brief \b DAXPY
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!
!       .. Scalar Arguments ..
!       REAL(kind=8) DA
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       REAL(kind=8) DX(*),DY(*)
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    DAXPY constant times a vector plus a vector.
!>    uses unrolled loops for increments equal to one.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in] DA
!> \verbatim
!>          DA is REAL(kind=8)
!>           On entry, DA specifies the scalar alpha.
!> \endverbatim
!>
!> \param[in] DX
!> \verbatim
!>          DX is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>         storage spacing between elements of DX
!> \endverbatim
!>
!> \param[in,out] DY
!> \verbatim
!>          DY is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>         storage spacing between elements of DY
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \date December 2016
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
       SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!*
!  -- Reference BLAS level1 routine (version 3.7.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
!     .. Scalar Arguments ..
      INTEGER :: incx, incy, n
      REAL(kind=8) :: DA
!     ..
!     .. Array Arguments ..
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCX)) :: DX
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCY)) :: DY
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER :: i, ix, iy, m, mp1
!     ..
      IF (n.LE.0) RETURN
      IF (da.EQ.0.0d0) RETURN
      IF (incx.EQ.1 .AND. incy.EQ.1) THEN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
         m = mod(n,4)
         IF (m.NE.0) THEN
            DO i = 1,m
               dy(i) = dy(i) + da*dx(i)
            END DO
         END IF
         IF (n.LT.4) RETURN
         mp1 = m + 1
         DO i = mp1,n,4
            dy(i) = dy(i) + da*dx(i)
            dy(i+1) = dy(i+1) + da*dx(i+1)
            dy(i+2) = dy(i+2) + da*dx(i+2)
            dy(i+3) = dy(i+3) + da*dx(i+3)
         END DO
      ELSE
!
!        code for unequal increments or equal increments
!          not equal to 1
!
         ix = 1
         iy = 1
         IF (incx.LT.0) ix = (-n+1)*incx + 1
         IF (incy.LT.0) iy = (-n+1)*incy + 1
         DO i = 1,n
          dy(iy) = dy(iy) + da*dx(ix)
          ix = ix + incx
          iy = iy + incy
         END DO
      END IF
      RETURN
      END SUBROUTINE DAXPY


!L3 swap updated BLAS 12/3/93
!> \brief \b DSCAL
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DSCAL(N,DA,DX,INCX)
!
!       .. Scalar Arguments ..
!       REAL(kind=8) DA
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       REAL(kind=8) DX(*)
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    DSCAL scales a vector by a constant.
!>    uses unrolled loops for increment equal to 1.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in] DA
!> \verbatim
!>          DA is REAL(kind=8)
!>           On entry, DA specifies the scalar alpha.
!> \endverbatim
!>
!> \param[in,out] DX
!> \verbatim
!>          DX is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>         storage spacing between elements of DX
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \date December 2016
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE DSCAL(N,DA,DX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.7.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
      IMPLICIT NONE 

!     .. Scalar Arguments ..
      REAL(kind=8), INTENT(IN) :: DA
      INTEGER, INTENT(IN) :: INCX, N
!     ..
!     .. Array Arguments ..
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCX)), INTENT(INOUT) :: DX
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER :: i, m, mp1, nincx
!     ..
       
      IF (n.LE.0 .OR. incx.LE.0) RETURN
      IF (incx.EQ.1) THEN
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
         m = mod(n,5)
         IF (m.NE.0) THEN
            DO i = 1,m
               dx(i) = da*dx(i)
            END DO
            IF (n.LT.5) RETURN
         END IF
         mp1 = m + 1
         DO i = mp1,n,5
            dx(i) = da*dx(i)
            dx(i+1) = da*dx(i+1)
            dx(i+2) = da*dx(i+2)
            dx(i+3) = da*dx(i+3)
            dx(i+4) = da*dx(i+4)
         END DO
      ELSE
!
!        code for increment not equal to 1
!
         nincx = n*incx
         DO i = 1,nincx,incx
            dx(i) = da*dx(i)
         END DO
      END IF
      RETURN
      END SUBROUTINE DSCAL


!L3 swap updated BLAS 12/3/93
!> \brief \b IDAMAX
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       INTEGER FUNCTION IDAMAX(N,DX,INCX)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       REAL(kind=8) DX(*)
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    IDAMAX finds the index of the first element having maximum absolute value.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in] DX
!> \verbatim
!>          DX is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>         storage spacing between elements of SX
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \date December 2016
!
!> \ingroup aux_blas
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      INTEGER FUNCTION idamax(N,DX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.7.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!

      IMPLICIT NONE

!     .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: incx, n
!     ..
!     .. Array Arguments ..
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCX)), INTENT(IN) :: dx
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      REAL(kind=8) :: dmax
      INTEGER :: i, ix
!     ..
      idamax = 0
      IF (n.LT.1 .OR. incx.LE.0) RETURN
      idamax = 1
      IF (n.EQ.1) RETURN
      IF (incx.EQ.1) THEN
!
!        code for increment equal to 1
!
         dmax = DABS(dx(1))
         DO i = 2, n
            IF (dabs(dx(i)).GT.dmax) THEN
               idamax = i
               dmax = dabs(dx(i))
            END IF
         END DO
      ELSE
!
!        code for increment not equal to 1
!
         ix = 1
         dmax = dabs(dx(1))
         ix = ix + incx
         DO i = 2, n
            IF (dabs(dx(ix)).GT.dmax) THEN
               idamax = i
               dmax = dabs(dx(ix))
            END IF
            ix = ix + incx
         END DO
      END IF
      RETURN
      END FUNCTION idamax


!L3 swap updated BLAS 12/3/93
!> \brief \b DSWAP
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DSWAP(N,DX,INCX,DY,INCY)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       REAL(kind=8) DX(*),DY(*)
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    DSWAP interchanges two vectors.
!>    uses unrolled loops for increments equal to 1.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in,out] DX
!> \verbatim
!>          DX is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>         storage spacing between elements of DX
!> \endverbatim
!>
!> \param[in,out] DY
!> \verbatim
!>          DY is REAL(kind=8) array, dimension ( 1 + ( N - 1 )*abs( INCY ) )
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>         storage spacing between elements of DY
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \date December 2016
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE dswap(N,DX,INCX,DY,INCY)
!
!
!  -- Reference BLAS level1 routine (version 3.7.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     December 2016
!
      IMPLICIT NONE

!     .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: incx, incy, n
!     ..
!     .. Array Arguments ..
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCX)), INTENT(IN) :: DX
      REAL(kind=8), DIMENSION(1 + (N - 1)*abs(INCY)), INTENT(OUT) :: DY
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER :: i, ix, iy, m, mp1

!     ..
      IF (n.LE.0) RETURN
      IF (incx.EQ.1 .AND. incy.EQ.1) THEN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
         m = mod(n,7)
         IF (m.NE.0) THEN
            DO i = 1,m
               dy(i) = dx(i)
            END DO
            IF (n.LT.7) RETURN
         END IF
         mp1 = m + 1
         DO i = mp1,n,7
            dy(i) = dx(i)
            dy(i+1) = dx(i+1)
            dy(i+2) = dx(i+2)
            dy(i+3) = dx(i+3)
            dy(i+4) = dx(i+4)
            dy(i+5) = dx(i+5)
            dy(i+6) = dx(i+6)
         END DO
      ELSE
!
!        code for unequal increments or equal increments
!          not equal to 1
!
         ix = 1
         iy = 1
         IF (incx.LT.0) ix = (-n+1)*incx + 1
         IF (incy.LT.0) iy = (-n+1)*incy + 1
         DO i = 1,n
            dy(iy) = dx(ix)
            ix = ix + incx
            iy = iy + incy
         END DO
      END IF
      RETURN
      END SUBROUTINE dswap

! 
! 
! 
!  DDIFCOEF    Calculates molecular and ionic diffusion coefficients,
!              D(i), for a set of species listed below at given
!              conditions of salinity, S, temperature, T, and pressure, P
!              Also calculated is the shear viscosity of this solution.
! 
!              (Double Precision Version)
! 
!              Diffusion coefficients are:
! 
!                   D(1) = H2O
!                   D(2) = O2
!                   D(3) = CO2
!                   D(4) = NH3
!                   D(5) = H2S
!                   D(6) = H3PO4
!                   D(7) = B(OH)3
!                   D(8) = HCO3-
!                   D(9) = CO3=
!                   D(10) = NH4+
!                   D(11) = HS-
!                   D(12) = NO3-
!                   D(13) = H2PO4-
!                   D(14) = HPO4=
!                   D(15) = PO4(---)
!                   D(16) = B(0H)4-
!                   D(17) = H+
!                   D(18) = OH-
!                   D(19) = Ca++
!                   D(20) = Mg++
!                   D(21) = Fe++
!                   D(22) = Mn++
!                   D(23) = SO4=
!                   D(24) = H4SiO4
! 
! 
!              Note: 1) enter S in ppt, T in deg. C and P in atm.
!                    2) diffusion coefficients are in units of cm**2/s
!                    3) H2O viscosity is in unit of centipoise
! 
! 
      SUBROUTINE DDIFCOEF(V,D,S,T,P)
      IMPLICIT NONE
      INTEGER, PARAMETER :: N = 24
      INTEGER :: I
      REAL(kind=8) :: MW, L01, L02, L03, L00
      REAL(kind=8) :: V, S, T, P
      REAL(kind=8) :: D(N)
      REAL(kind=8) :: PP, TK, TS, R25, RHO, V0
      REAL(kind=8) :: A, B, T0, PHI, VM, FAC, R, F2
      REAL(kind=8) :: B1, B2, B3, B4, RH1, RH2, RH3, SS
      REAL(kind=8) :: VSTP, V29, V36

      PP = P * 1.013
      TK = T + 273.15
! 
!   Calculate density of pure water at 25 deg C and sample temperature
! 
      TS = 25.0
      CALL WATER(TS,R25)
      CALL WATER(T,RHO)
! 
!   Calculate the viscosity for the true sample conditions.
! 
      CALL VISCO(V,S,T,P)
! 
!   Start calculations of diffusion coefficients in pure water
!   at sample temperature.
! 
      CALL VISCO(V0,ZERO,T,ONE)
! 
!   Water : from Cohen and Turnbull (1959) and Krynicki et al. (1978)
! 
      A = 12.5D-09*DEXP(-5.22D-04*P)
      B = 925.0*DEXP(-2.6D-04*P)
      T0 = 95.0 + 2.61D-02*P
      D(1) = A*DSQRT(TK)*DEXP(-B/(TK-T0))*1.0D+04
! 
! 
!   Dissolved gases : from Wilke and Chang (1955)
!     note: 1) MW = molecular weight of water
!           2) VM = molar volumes (cm**3/mol) (Sherwood et al., 1975)
! 
!   The factor PHI is reduced to 2.26 as suggested by Hayduk and
!   Laudie (1974).
! 
! 
      PHI = 2.26
      MW = 18.0
      A = DSQRT(PHI*MW)*TK/V0
! 
!   Oxygen
! 
      VM = 25.6
      D(2) = 7.4D-08*(A/(VM**0.6))
! 
!   CO2
! 
      VM = 34.0
      D(3) = 7.4D-08*(A/(VM**0.6))
! 
!   NH3
! 
      VM = 25.8
      D(4) = 7.4D-08*(A/(VM**0.6))
! 
!   H2S
! 
      VM = 32.9
      D(5) = 7.4D-08*(A/(VM**0.6))
! 
! 
!   The coefficients in pure water for the following species are
!   calculated by assuming a linear function of temperature (deg C)
!   between 0 and 25 deg C, and using the values given by Li and
!   Gregory (1974) to obtain the parameter values.
! 
!   i.e. NO3-,HS-,H2PO4-,CO3=,SO4=,Ca++,Mg++,Mn++,Fe++,NH4+,H+ & OH-
! 
! 
      FAC = 1.0D-05
      D(9) = (0.439 + 0.0206*T)*FAC
      D(10) = (0.98 + 0.04*T)*FAC
      D(11) = (0.975 + 0.03*T)*FAC
      D(12) = (0.978 + 0.0369*T)*FAC
      D(13) = (0.378 + 0.0187*T)*FAC
      D(17) = (5.61 + 0.148*T)*FAC
      D(18) = (2.56 + 0.1084*T)*FAC
      D(19) = (0.373 + 0.0168*T)*FAC
      D(20) = (0.356 + 0.014*T)*FAC
      D(21) = (0.341 + 0.0151*T)*FAC
      D(22) = (0.305 + 0.0153*T)*FAC
      D(23) = (0.5 + 0.0228*T)*FAC
! 
! 
!   HCO3-, HPO4= and PO4(---):  don't have limiting
!   conductivity at temperatures other than 25 deg C.  Use the
!   the reduced state relationship of Marshall (1987) to calculate
!   limiting conductivities at other temperatures and the Nernst equation
!   to calculate diffusion coefficients.
! 
!   (note: assumes the RHOh parameter is a constant independent of T.)
! 
! 
      R = 8.3143
      F2 = 96490.0**2
      TS = 25.0
      A = ONE/850.0
      B1 = 1.33669D-06
      B2 = 1.68178D-07
      B3 = - 1.52321D-09
      B4 = 5.61864D-12
      L00 = TS*(B1 + TS*(B2 + TS*(B3 + TS*B4)))
      L00 = 950.0 - ONE/(A + L00)
      L01 = 44.5
      L02 = 55.14
      L03 = 68.96
      RH1 = R25*L00/(L00 - L01)
      RH2 = R25*L00/(L00 - L02)
      RH3 = R25*L00/(L00 - L03)
      L00 = T*(B1 + T*(B2 + T*(B3 + T*B4)))
      L00 = 950.0 - ONE/(A + L00)
      L01 = L00*(ONE - RHO/RH1)
      L02 = L00*(ONE - RHO/RH2)
      L03 = L00*(ONE - RHO/RH3)
      D(8) = R*TK*L01/F2
      D(14) = R*TK*L02/F2/TWO
      D(15) = R*TK*L03/F2/THREE
! 
!  H3PO4 : Least (1984) determined D(H3PO4) at 25 deg C and 0 ppt S.
!          Assume that this value can be scaled by the Stokes-Einstein
!          relationship to any other temperature and salinity.
! 
      D(6) = 0.87D-05
      SS = 25.0
      CALL VISCO(VSTP,ZERO,SS,ONE)
      D(6) = D(6)*VSTP/298.15*TK/V0
! 
!   B(OH)3 : Mackin (1986) determined D(B(OH)3) at 25 deg C and
!            about 29.2 ppt S.
!            Assume that this value can be scaled by the Stokes-Einstein
!            relationship to any other temperature and salinity.
! 
      D(7) = 1.12D-05
      TS = 29.2
      SS = 25.0
      CALL VISCO(V29,TS,SS,ONE)
      D(7) = D(7)*V29/298.15*TK/V0
! 
!   B(OH)4 : No information on this species whatsoever! Boudreau and
!            Canfield (1988) assume it is 12.5% smaller than B(OH)3.
! 
      D(16) = 0.875*D(7)
! 
!   H4SiO4 : Wollast and Garrels (1971) found D(H4SiO4) at 25 deg C
!            and 36.1 ppt S.
!            Assume that this value can be scaled by the Stokes-Einstein
!            relationship to any other temperature and salinity.
! 
      D(24) = 1.0E-05
      TS = 36.1
      SS = 25.0
      CALL VISCO(V36,TS,SS,ONE)
      D(24) = D(24)*V36/298.15*TK/V0
! 
!   To correct for salinity, the Stokes-Einstein relationship is used.
!   (This is not quite accurate, but is at least consistant.)
! 
      FAC = V0/V
      DO 10 I=1,N
   10 D(I) = D(I)*FAC
! 
      RETURN
      END SUBROUTINE DDIFCOEF


! 
! 
! 
!   VISCO      Calculates the shear viscosity of water using the equation
!              given by Kukulka et al. (1987).
!              Calculated viscosity is in centipoise.
! 
!              Valid for 0<T<30 and 0<S<36.
! 
      SUBROUTINE VISCO(V,S,T,P)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: S, T, P
      REAL(kind=8), INTENT(OUT) :: V
      V =  1.7910 - T*(6.144D-02 - T*(1.4510D-03 - T*1.6826D-05)) - &
           1.5290D-04*P + 8.3885D-08*P*P + 2.4727D-03*S + &
           (6.0574D-06*P - 2.6760D-09*P*P)*T + (T*(4.8429D-05 - & 
           T*(4.7172D-06 - T*7.5986D-08)))*S
      RETURN
      END SUBROUTINE VISCO
! 
! 
! 
!   WATER      Calculates the density of pure water using the equation
!              given by Bigg (1967).  (see Millero and Poisson, 1981)
! 
      SUBROUTINE WATER(T,RHO)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: T
      REAL(kind=8), INTENT(OUT) :: RHO
      RHO =  999.842594 + T*(6.793952D-02 + &
             T*(-9.09529D-03 + T*(1.001685D-04 + T*(-1.120083D-06 + T*6.536336D-09))))
      RHO = RHO/1000.0
      RETURN
      END SUBROUTINE WATER

      SUBROUTINE FEX2(NEQ1,T,Y,YDOT,RPAR,IPAR)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NEQ1
      INTEGER, PARAMETER :: MAXNEQ = 27000
      INTEGER :: IPAR
      REAL(kind=8), INTENT(IN) :: T
      REAL(kind=8) :: IRRIG, KADS_p
      REAL(kind=8) :: Y(NEQ1), YDOT(NEQ1)
      REAL(kind=8) :: POROVEC(1500)
      REAL(kind=8) :: RPAR(MAXNEQ)
      REAL(kind=8) :: Rc(25)
      REAL(kind=8) :: dy(2)

      REAL(kind=8) :: X, tt, Os, Ob, O20
      REAL(kind=8) :: dTemp, Tq1, Tq2
      REAL(kind=8) :: P, PS, F, SW, SS, AGTG, AGST, ADVC, DDOM, U, W
      REAL(kind=8) :: DPDX, DT2DX, T2, GM1, GM2
      REAL(kind=8) :: TESTOM1, TESTOM2, TESTO2, TESTNIT, TESTNH3, TESTMNO
      REAL(kind=8) :: TESTMN2, TESTFE3, TESTFE2, TESTSO4, TESTTS   
      REAL(kind=8) :: TESTFES, TESTTC, TESTALK, TESTDOM 
      REAL(kind=8) :: RCH2O1, RCH2O2, RDOM, RO2, RNO3, RNH4
      REAL(kind=8) :: RMNO, RMN2, RFEOH3, RFE2, RSO4, RTS, RFES, RTC, RALK
      REAL(kind=8) :: totOM, disOM1, disOM2, disOM
      REAL(kind=8) :: DIFF1, DGDX, ADV1, AGAD
      REAL(kind=8) :: DIFF2, FO2, ADV2, FNO3, FNH3, FMnO, FMN2, FFE3, FFE2
      REAL(kind=8) :: FSO4, FHS, FFES, FDIC, FALK, FDOM, FOM1, FOM2
      REAL(kind=8) :: FO2l, FNO3l, FNH3l, FFE3l, FFE2l, FSO4l, FHSl, FDICl, FFESl
      REAL(kind=8) :: FALKl, FDOMl, FOM1l, FOM2l

      INTEGER :: NPM1, iday, I, II, M
      INTEGER :: MAX1, MAX2, MAX3, MAX4,MAX5, MAX6, MAX7, MAX8, MAX9
      INTEGER :: MAX10, MAX11, MAX12, MAX13, MAX14, MAX15, MAX16, MAX17
      INTEGER :: MID1, MID2, MID3, MID4, MID5, MID6, MID7, MID8
      INTEGER :: MID9, MID10, MID11, MID12, MID13, MID14, MID15, MID16, MID17
      INTEGER :: MIN1, MIN2, MIN3, MIN4, MIN5, MIN6, MIN7, MIN8
      INTEGER :: MIN9, MIN10, MIN11, MIN12, MIN13, MIN14, MIN15, MIN16, MIN17

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
! JCL commented out      ALPHA0 = ALPHAA*exp(0.06*(Tq1-Tq2))
      ALPHA0 = ALPHAA
!
!  Provide values and derivatives for porosity and tortuosity
!
       CALL SED(X,P,DPDX,U,W)
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F =   PS/P         ! conversion dissolved to solids
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

      CALL REACTION_SDM(TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3, &
                        TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES, &
                        TESTTC,TESTALK,Rc,pH(1),F,dtemp)

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


! .......................................................................
         totOM   = TESTOM1 + TESTOM2
         disOM1  = TESTOM1*PER_DIS*(ONE-DEXP(-DB0*a)) 
         disOM2  = TESTOM2*PER_DIS*(ONE-DEXP(-DB0*a))
         disOM   = disOM1+disOM2
! 
!  OM1
! 
      DIFF1 = DB0*(Y(18)-TWO*Y(1)+GM1)/DH/DH
      DGDX = (FG1-PS*W*Y(1))/PS/DB0
      ADV1 = AGST*DGDX
      YDOT(1) = DIFF1 + ADV1 + RCH2O1-disOM1
      RATE(1) = RCH2O1
! 
!  OM2
      DIFF1 = DB0*(Y(19)-TWO*Y(2)+GM2)/DH/DH
      DGDX = (FG2-PS*W*Y(2))/PS/DB0
      ADV1 = AGST*DGDX
      YDOT(2) = DIFF1 + ADV1 + RCH2O2-disOM2
      RATE(2) = RCH2O2
!       FOM= FOM1 + FOM2
! 
!   Transport equations Grid-Point 1
! 
       X = RPAR(1)
! 
!   Provide values and derivatives for porosity and tortuosity
! 
       CALL SED(X,P,DPDX,U,W)
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F =   PS/P          ! conversion dissolved to solids
       SW = SIG(X,U)
       SS = SIG(X,W)
! 
!   Provides advective coefficents for solid and porewater species that
!   is general for all species at grid-point X.
! 
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**TWO)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)
! 
!   Provide input for the reaction for the second grid point
! 
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
!   711  format(5 f9.3)
! .......................................................................
!L3 "I" undefined, I will set it to one:
      I = 1
!       I = 0
!L3 end modification

      CALL REACTION_SDM (TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3, &
                         TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES, &
                         TESTTC,TESTALK,Rc,pH(I),F,dtemp)
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
! 
!  O2
! 
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
!  
!  NO3, JCL changed advective flux from W to U
!     FNO3= DNO3/T2*P*(Y(4) - Y(38))/TWO/DH + W*P*Y(21)
      FNO3= DNO3/T2*P*(Y(4) - Y(38))/TWO/DH + U*P*Y(21)
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
! 
!  NH3
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

!  NH3, JCL added variable 'P' to calculation of FNH3
!      FNH3= DNH3/T2*(Y(5) - Y(39))/TWO/DH + U*P*Y(22)
      FNH3= DNH3/T2*P*(Y(5) - Y(39))/TWO/DH + U*P*Y(22)
      KADS_p = PS/P*KANH4
      DIFF2 = ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2) * &
             (Y(22)-TWO*Y(5)+NH30)/DH/DH
      DGDX = ((ONE-SW)*Y(22)+TWO*SW*Y(5)-(ONE+SW)*NH30)/TWO/DH
      AGAD = DPDX*DB(X) + P*DDB(X) - (P*U+ KADS_p*W)
      ADV2 = (AGAD + ADVC)/P*DGDX
      IRRIG = ALPHA0*(NH3I-Y(5))
      YDOT(5) = DIFF2 + ADV2 + IRRIG + RNH4
      RATE(5) = RNH4
      rIRRNH(1) =IRRIG*DH

! 
!  MnO2 (SOLID PHASE)
      FMnO= DB0*PS*(Y(6) -Y(40))/TWO/DH + W*PS*Y(23)
      DIFF1 = DB(X)*(Y(23)-TWO*Y(6)+MNO0)/DH/DH
      DGDX = ((ONE-SS)*Y(23)+TWO*SS*Y(6)-(ONE+SS)*MNO0)/TWO/DH
      ADV1 = AGST/PS*DGDX    
      YDOT(6) =  DIFF1 + ADV1 + RMNO
      RATE(6) =  RFEOH3
! 
! .......................................................................
!  MN+2 (DISSOLVED PHASE)
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

!  FE+3 (SOLID PHASE)
      FFE3= DB0*PS*(Y(8) -Y(42))/TWO/DH + W*PS*Y(25)
      DIFF1 = DB(X)*(Y(25)-TWO*Y(8)+FE30)/DH/DH
      DGDX = ((ONE-SS)*Y(25)+TWO*SS*Y(8)-(ONE+SS)*FE30)/TWO/DH
      ADV1 = AGST/PS*DGDX    
      YDOT(8) =  DIFF1 + ADV1 + RFEOH3
      RATE(8) =  RFEOH3
! 
! .......................................................................
!  FE+2 (DISSOLVED PHASE) 
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

! 
!  SO4--
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
! 
!  TS 
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
! 

! 
!  FES  (SOLID PHASE)
      FFES= DB0*PS*(Y(12) -Y(46))/TWO/DH + W*PS*Y(29)
      DIFF1 = DB(X)*(Y(29)-TWO*Y(12)+FES0)/DH/DH
      DGDX = ((ONE-SS)*Y(29)+TWO*SS*Y(12)-(ONE+SS)*FES0)/TWO/DH
       ADV1 = AGST/PS*DGDX    
      YDOT(12) = DIFF1 + ADV1 + RFES
      RATE(12) = RFES
! 
!  TC
! 
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
! 
!  ALKALINITY
!  
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
!L3 adding irrigation terms for ALK
      rIRRALK(1) = IRRIG*DH

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
!L3 Adding irrigation terms for DOM
      rIRRDOM(I) = IRRIG*DH

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
        FLUXES(10) = FDOM
  
!      write(*,'(A4,2X,f12.2)') 'FO2',FO2  ! check HS concentrations
!  FOR THE INTERVENING POINTS IN THE GRID TO X2:
!  Following statements gives the # of midpoints NPOINTS -1
!L3      DO 10 I=2,NPM1
      DO I=2,NPM1
       II = I
       X = RPAR(I)
!L3       IF(X.GE.X2) GO TO 20
       IF(X.GE.X2) EXIT 
       M = I*NS              ! starts at 18 for 9 species
!
!  Provide values and derivatives for porosity and tortuosity
!
       CALL SED(X,P,DPDX,U,W)
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       SS = SIG(X,W)
       SW = SIG(X,U)
       PS = ONE - P
       F =   PS/P          ! conversion dissolved to solids
! 
!   Provides advective coefficents for solid and porewater species that 
!   is general for all species at grid-point X.
! 
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**2)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)


! 
!   Provide index for the the finite difference scheme
! 
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
! 
! 
!     Species w/ corrisponding gridpoints
! 
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
! 
!       IF (TESTOM1 .GT. 3.0D+03) write(*,*) ' 1 OM1 ',TESTOM1
!       IF (TESTOM2 .GT. 3.0D+03) write(*,*) ' 1 OM2 ',TESTOM2
!       IF (TESTO2  .GT. 3.0D+02) write(*,*) ' 1 O2 ',TESTO2
!       IF (TESTNIT .GT. 3.0D+02) write(*,*) ' 1 No3 ',TESTNIT
!       IF (TESTNH3 .GT. 3.0D+02) write(*,*) ' 1 NH3 ',TESTNH3
!       IF (TESTSO4 .GT. 3.0D+02) write(*,*) ' 1 SO4 ',TESTSO4
!       IF (TESTTS  .GT. 3.0D+02) write(*,*) ' 1 TS ',TESTTS
!       IF (TESTFE3 .GT. 3.0D+02) write(*,*) ' 1 FE3 ',TESTFE3
!       IF (TESTFE2 .GT. 3.0D+02) write(*,*) ' 1 FE2 ',TESTFE2
!       IF (TESTFES .GT. 3.0D+02) write(*,*) ' 1 FES ',TESTFES
!       IF (TESTFEC .GT. 3.0D+03) write(*,*) ' 1 FEC ',TESTFEC
!       IF (TESTTC  .GT. 8.0D+01) write(*,*) ' 1 TC ',TESTTC
!       IF (TESTALK .GT. 8.0D+01) write(*,*) ' 1 ALK ',TESTALK
! .......................................................................
      CALL REACTION_SDM (TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3, &
                         TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES, &
                         TESTTC,TESTALK,Rc,pH(I),F,dtemp)
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
! 

        totOM   = TESTOM1 + TESTOM2
        disOM1  = TESTOM1*PER_DIS*(ONE-DEXP(-DB(X)*a)) 
        disOM2  = TESTOM2*PER_DIS*(ONE-DEXP(-DB(X)*a))
        disOM   = disOM1+disOM2
 125    FORMAT(4 F8.3)
! 
!  O2  Transport equations
! 
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
! 
!  NO3
! 	
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
! 
!  NH3
! 	
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

! 
!  NH3
! 
      KADS_p = PS/P*KANH4
      DIFF2 =  ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2) * &
               (Y(MAX5)-TWO*Y(MID5)+Y(MIN5))/DH/DH
      DGDX=((ONE-SW)*Y(MAX5)+TWO*SW*Y(MID5)-(ONE+SW)*Y(MIN5))/TWO/DH
      AGAD = DPDX*DB(X) + P*DDB(X) - (P*U+ KADS_p*W)
      ADV2 = (AGAD + ADVC)/P*DGDX
      IRRIG = ALPHA0*(NH3I-Y(MID5))
      YDOT(MID5) = DIFF2 + ADV2+ IRRIG + RNH4
      RATE(MID5) = RNH4
      rIRRNH(I) =IRRIG*DH

! 
!  MNO2 (SOLID PHASE)
! ......................................................................
      DIFF1 = DB(X)*(Y(MAX6)-TWO*Y(MID6)+Y(MIN6))/DH/DH
      DGDX = ((ONE-SS)*Y(MAX6)+TWO*SS*Y(MID6)-(ONE+SS)*Y(MIN6))/TWO/DH
      ADV1= AGST/PS*DGDX
      YDOT(MID6) = DIFF1 + ADV1 + RMNO
      RATE(MID6) = RMNO
! 
!  MN+2 (DISSOLVED PHASE)
! 
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
! 
!  FE+3 (SOLID PHASE)
! ......................................................................
      DIFF1 = DB(X)*(Y(MAX8)-TWO*Y(MID8)+Y(MIN8))/DH/DH
      DGDX = ((ONE-SS)*Y(MAX8)+TWO*SS*Y(MID8)-(ONE+SS)*Y(MIN8))/TWO/DH
      ADV1= AGST/PS*DGDX
      YDOT(MID8) = DIFF1 + ADV1 + RFEOH3
      RATE(MID8) = RFEOH3
! 
!  FE+2 (DISSOLVED PHASE) 
! 
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
! 
!  SO4--
! 	
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
! 
!  TS 
! 	
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
! 
! 
!  FES  (SOLID PHASE)
      DIFF1 = DB(X)*(Y(MAX12)-TWO*Y(MID12)+Y(MIN12))/DH/DH
      DGDX=((ONE-SS)*Y(MAX12)+TWO*SS*Y(MID12)-(ONE+SS)*Y(MIN12))/TWO/DH
      ADV1= AGST/PS*DGDX
      YDOT(MID12) = DIFF1 + ADV1 + RFES
      RATE(MID12) = RFES
! 
!  TC (DISSOLVED PHASE) 
! 
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
! 
!  ALKALINITY (DISSOLVED PHASE) 
! 
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
! 
!  DOC (DISSOLVED PHASE) 
! ......................................................................
! 
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

! 
!    ABV (above pycnocline water)
! 
      YDOT(MID16) = dy(1)
      RATE(MID16) = -9.

! 
!    BEL (subpycnocline water)
! 
      YDOT(MID17) = dy(2)
      RATE(MID17) = -9.


! 
!  OM1
! 
       DIFF1 = DB(X)*(Y(MAX1)-TWO*Y(MID1)+Y(MIN1))/DH/DH
       DGDX=((ONE-SS)*Y(MAX1)+TWO*SS*Y(MID1)-(ONE+SS)*Y(MIN1))/TWO/DH
       ADV1= AGST/PS*DGDX
       YDOT(MID1) = DIFF1 + ADV1 + RCH2O1 - disOM1
       RATE(MID1) = RCH2O1
       POROVEC(I)= P
!  OM2
       DIFF1 = DB(X)*(Y(MAX2)-TWO*Y(MID2)+Y(MIN2))/DH/DH
       DGDX=((ONE-SS)*Y(MAX2)+TWO*SS*Y(MID2)-(ONE+SS)*Y(MIN2))/TWO/DH
       ADV1= AGST/PS*DGDX
       YDOT(MID2) = DIFF1 + ADV1 + RCH2O2 - disOM2
       RATE(MID2) = RCH2O2
!L3   10 CONTINUE
       ENDDO

! 
!   FOR THE INTERVENING POINTS IN THE GRID FROM X2 TO XL-DH:
! 
!L3   20 CONTINUE

!L3      DO 30 I=II,NPM1
       DO I = II, NPM1
       X = RPAR(I)
       M = I*NS
       CALL SED(X,P,DPDX,U,W) 
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F =   PS/P          ! conversion dissolved to solids
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
! 
! 
!     Species w/ corrisponding gridpoints
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
! 
! 
! .......................................................................
! 

       CALL REACTION_SDM(TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3, &
                         TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES, &
                         TESTTC,TESTALK,Rc,pH(I),F,dtemp)
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
! 
!   O2
! 
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
! 
!  NO3
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
! 
!  NH4+
! 
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

! 
!  NH4+
! 
      KADS_p = PS/P*KANH4
      DIFF2 =  ONE/(ONE+KADS_p)*(KADS_p*DB(X)+DNH3/T2) * &
               (Y(MAX5)-TWO*Y(MID5)+Y(MIN5))/DH/DH
      DGDX =  (Y(MAX5)-Y(MIN5))/TWO/DH
      AGAD = DPDX*DB(X) + P*DDB(X) - (P*U+ KADS_p*W)
      ADV2 = (AGAD + ADVC)/P*DGDX
      IRRIG = ALPHA0*(NH3I-Y(MID5))
      YDOT(MID5) = DIFF2 + ADV2+IRRIG + RNH4
      RATE(MID5) = RNH4
      rIRRNH(I) =IRRIG*DH


! 
!  MNO2 (SOLID PHASE)
! 
      DGDX = (Y(MID6)-Y(MIN6))/DH
      ADV1 = - W*DGDX
      YDOT(MID6) = ADV1 + RMNO
      RATE(MID6) = RMNO
! 
!  MN+2 (DISSOLVED PHASE)
! 
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
! 
!  FE+3 (SOLID PHASE)
! 
      DGDX = (Y(MID8)-Y(MIN8))/DH
      ADV1 = - W*DGDX
      YDOT(MID8) = ADV1 + RFEOH3
      RATE(MID8) = RFEOH3
! 
!  FE+2 (DISSOLVED PHASE) 
! 
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
! 
!  SO4--
! 
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
! 
!  TS 
! 
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

! 
!  FES  (SOLID PHASE) 
! 
      DGDX = (Y(MID12)-Y(MIN12))/DH
      ADV1 = - W*DGDX
      YDOT(MID12) = ADV1 + RFES
      RATE(MID12) = RFES
! 
!  TC  (DISSOLVED PHASE) 
! 
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
! 
!   ALKALINITY (DISSOLVED PHASE) 
! 
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
! 
!  DOM (DISSOLVED PHASE) 
! 
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

! 
!    ABV (above pycnocline water)
! 
      YDOT(MID16) = dy(1)

      RATE(MID16) = -9.
! 
!    BEL (subpycnocline water)
! 
      YDOT(MID17) = dy(2)
      RATE(MID17) = -9.

! 
!  OM1
! 
! ......................................................................
      DGDX = (Y(MID1)-Y(MIN1))/DH
      ADV1 = - W*DGDX
      YDOT(MID1) = ADV1 + RCH2O1 - disOM1
      RATE(MID1) = RCH2O2
      POROVEC(I) = P

! 
!  OM2
! 
      DGDX = (Y(MID2)-Y(MIN2))/DH
      ADV1 = - W*DGDX
      YDOT(MID2) = ADV1 + RCH2O2 - disOM2
      RATE(MID2) = RCH2O2
!L3   30 CONTINUE
      ENDDO
! 
!   FOR THE LAST POINT IN THE GRID:
! 
       I = NPOINTS
       X = RPAR(I)
       M = I*NS
       CALL SED(X,P,DPDX,U,W) 
       CALL TORT2(T2,DT2DX,P,DPDX,X)
       PS = ONE - P
       F =   PS/P          ! conversion dissolved to solids
       AGTG = DPDX*DB(X) + P*DDB(X) - P*U
       AGST = -DPDX*DB(X)+PS*DDB(X)-PS*W
       ADVC = ONE/(T2**2)*(T2*DPDX-P*DT2DX)  ! Expansion of diffusion terms (PWater)

! 
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
! 
!     Species w/ corrisponding gridpoints
! 
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

! 
!       IF (TESTOM1 .GT. 3.0D+03) write(*,*) ' 1 OM1 ',TESTOM1
!       IF (TESTOM2 .GT. 3.0D+03) write(*,*) ' 1 OM2 ',TESTOM2
!       IF (TESTO2  .GT. 3.0D+02) write(*,*) ' 1 O2 ',TESTO2
!       IF (TESTNIT .GT. 3.0D+02) write(*,*) ' 1 No3 ',TESTNIT
!       IF (TESTNH3 .GT. 3.0D+02) write(*,*) ' 1 NH3 ',TESTNH3
!       IF (TESTSO4 .GT. 3.0D+02) write(*,*) ' 1 SO4 ',TESTSO4
!       IF (TESTTS  .GT. 3.0D+02) write(*,*) ' 1 TS ',TESTTS
!       IF (TESTFE3 .GT. 3.0D+02) write(*,*) ' 1 FE3 ',TESTFE3
!       IF (TESTFE2 .GT. 3.0D+02) write(*,*) ' 1 FE2 ',TESTFE2
!       IF (TESTFES .GT. 3.0D+02) write(*,*) ' 1 FES ',TESTFES
!       IF (TESTTC .GT. 8.0D+01)  write(*,*) ' 1 TC ',TESTTC
!       IF (TESTALK .GT. 8.0D+01) write(*,*) ' 1 ALK ',TESTALK
! 
! .......................................................................
       CALL REACTION_SDM(TESTOM1,TESTOM2,TESTDOM,TESTO2,TESTNIT,TESTNH3, &
                         TESTSO4,TESTTS,TESTFE3,TESTFE2,TESTMNO,TESTMN2,TESTFES, &
                         TESTTC,TESTALK,Rc,pH(I),F,dtemp)
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
! 
!  O2
! 
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

!  NO3
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

! 
!  NH3
! 
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

! 
!  NH3
! 
      FNH3l= DNH3*P/T2*(Y(MIN5-NS)-Y(MID5))/TWO/DH+U*P*Y(MID5)

      KADS_p = PS/P*KANH4
      DIFF2 = ONE/(ONE+KADS_p) * (KADS_p*DB(X)+DNH3/T2) * &
              TWO * (-Y(MID5)+Y(MIN5))/DH/DH
      IRRIG = ALPHA0*(NH3I-Y(MID5))
      YDOT(MID5) = DIFF2 + IRRIG + RNH4
      RATE(MID5) = RNH4
      rIRRNH(I) =IRRIG*DH


! 
!   MNO2 (SOLID PHASE)
! 
      FFE3l= W*PS*Y(MID6)
      DGDX = (Y(MID6)-Y(MIN6))/DH
      ADV1 = - W*DGDX
      YDOT(MID6) = ADV1 + RMNO
      RATE(MID6) = RMNO
! 
!  MN+2  (DISSOLVED PHASE)
! 
      FFE2l= DMN2*P/T2*(Y(MIN7-NS)-Y(MID7))/TWO/DH+U*P*Y(MID7)
      DIFF2 = DMN2/T2*TWO*(-Y(MID7)+Y(MIN7))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(MN20-Y(MID7))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID7) = DIFF2+IRRIG+RMN2
      RATE(MID7) = RMN2
! 
!  FE+3 (SOLID PHASE)
! 
      FFE3l= W*PS*Y(MID8)
      DGDX = (Y(MID8)-Y(MIN8))/DH
      ADV1 = - W*DGDX
      YDOT(MID8) = ADV1 + RFEOH3
      RATE(MID8) = RFEOH3
! 
!  FE+2  (DISSOLVED PHASE)
! 
      FFE2l= DFE2*P/T2*(Y(MIN9-NS)-Y(MID9))/TWO/DH+U*P*Y(MID9)
      DIFF2 = DFE2/T2*TWO*(-Y(MID9)+Y(MIN9))/DH/DH 
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(FE20-Y(MID9))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID9) = DIFF2+IRRIG+RFE2
      RATE(MID9) = RFE2
! 
!  SO4--
! 
       FSO4l = DSO4*P/T2*(Y(MIN10-NS)-Y(MID10))/TWO/DH+U*P*Y(MID10)
       DIFF2 = DSO4/T2*TWO*(-Y(MID10)+Y(MIN10))/DH/DH
       IF(X.LE.XIRRIG) THEN 
           IRRIG = ALPHA0*(SO40-Y(MID10))
       ELSE
           IRRIG = ZERO
       ENDIF
       YDOT(MID10) = DIFF2 + IRRIG  + RSO4
       RATE(MID10) = RSO4
       rIRRSO(I) =IRRIG*DH
! 
!  TS (PORE WATER)
! 
      FHSl = DHS*P/T2*(Y(MIN11-NS)-Y(MID11))/TWO/DH+U*P*Y(MID11)
      DIFF2 = DHS/T2*TWO*(-Y(MID11)+Y(MIN11))/DH/DH
      IF(X.LE.XIRRIG) THEN 
          IRRIG = ALPHA0*(HS0-Y(MID11))
      ELSE
          IRRIG = ZERO
      ENDIF
      YDOT(MID11) =DIFF2+IRRIG+RTS
      RATE(MID11) = RTS
! 
!  FES  (SOLID PHASE) 
! 
      FFESl = W*PS*Y(MID12)
      DGDX = (Y(MID12)-Y(MIN12))/DH
      ADV1 = - W*DGDX
      YDOT(MID12) = ADV1 + RFES
      RATE(MID12) = RFES
! 
!  TC  (DISSOLVED PHASE) 
!  
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
! 
!  ALKALINITY (DISSOLVED PHASE) 
! 
      FALKl= DALK*P/T2*(Y(MIN14-NS)-Y(MID14))/TWO/DH+U*P*Y(MID14)
      DIFF2 = DALK/T2*TWO*(-Y(MID14)+Y(MIN14))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(ALK0-Y(MID14))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID14) = DIFF2+IRRIG + RALK
      RATE(MID14) = pH(I)
!L3 adding irrigation terms for ALK
      rIRRALK(I) = IRRIG*DH


! 
!  DOM (DISSOLVED PHASE) 
! 
! .......................................................................
      FDOMl= DDOM*P/T2*(Y(MIN15-NS)-Y(MID15))/TWO/DH+U*P*Y(MID15)
      DIFF2 = DDOM/T2*TWO*(-Y(MID15)+Y(MIN15))/DH/DH
      IF(X.LE.XIRRIG) THEN 
         IRRIG = ALPHA0*(DOMI-Y(MID15))
      ELSE
         IRRIG = ZERO
      ENDIF
      YDOT(MID15) = DIFF2+IRRIG+RDOM+disOM
      RATE(MID15) = RDOM
!L3 Adding irrigation terms for DOM
      rIRRDOM(I) = IRRIG*DH
! 
!    ABV (above pycnocline water)
! 
      YDOT(MID16) = dy(1)
      RATE(MID16) = -9.

! 
!    BEL (subpycnocline water)
! 
      YDOT(MID17) = dy(2)
      RATE(MID17) = -9.

! 
!  OM1
! 
       FOM1l= W*PS*Y(MID1)
       DGDX = (Y(MID1)-Y(MIN1))/DH
       ADV1 = - W*DGDX
       YDOT(MID1) = ADV1 + RCH2O2-disOM1
       RATE(MID1) = RCH2O2
       POROVEC(I)=P
! 
!  OM2
! 
       FOM2l= W*PS*Y(MID2)
       DGDX = (Y(MID2)-Y(MIN2))/DH
       ADV1 = - W*DGDX
       YDOT(MID2) = ADV1 + RCH2O2-disOM2
       RATE(MID2) = RCH2O2
       RETURN
       END SUBROUTINE FEX2

! 
! 
! 
!    JEX	A DUMMY ROUTINE FOR THE JACOBIAN CALLED BY VODE.f
! 
! 
      SUBROUTINE JEX (NEQ1, T, Y, ML, MU, PD, NRPD, RPAR, IPAR)
      IMPLICIT NONE
      INTEGER :: ML, MU, NEQ1, NRPD
      INTEGER :: IPAR
      REAL(kind=8) :: T
      REAL(kind=8) :: Y(NEQ1), PD(NRPD,NEQ1), RPAR(NRPD)
      RETURN
      END SUBROUTINE JEX

!  ----------------------------------------------------------------
!  REACTION_SDMS
!  PURPOSE:  Provides all reaction rates for the model
!  ----------------------------------------------------------------
! 
      SUBROUTINE REACTION_SDM(TESTOM1,TESTOM2,TESTOM3,TESTO2,TESTNIT,  &
                              TESTNH3,TESTSO4,TESTTS,TESTFE3,TESTFE2,  &
                              TESTMNO,TESTMN2,TESTFES, TESTTC,TESTALK, &
                              Rc,pH,F,dtemp)
      IMPLICIT NONE

      INTEGER :: j

      REAL(kind=8), INTENT(INOUT) :: TESTOM1,TESTOM2,TESTOM3,TESTO2,TESTNIT
      REAL(kind=8), INTENT(INOUT) :: TESTNH3,TESTSO4,TESTTS,TESTFE3,TESTFE2
      REAL(kind=8), INTENT(INOUT) :: TESTMNO,TESTMN2,TESTFES
      REAL(kind=8), INTENT(IN) :: TESTTC,TESTALK, F, dtemp
      REAL(kind=8), INTENT(OUT) :: Rc(25), pH

      REAL(kind=8) :: R(6),R1(6),R2(6),R3(6)
      REAL(kind=8) :: spc(9)
      REAL(kind=8) :: x1, y1, z1, x2, y2, z2, x3, y3, z3
      REAL(kind=8) :: rq1, rq11, rq12, rq2, rq3, Tq1, Tq2, rq21, rq22, rq23
      REAL(kind=8) :: RCt1, RCt2, RCt3, Rt
      REAL(kind=8) :: CO2aq, HCO3c, CO3c, H2S, HS, H1, alphe1
      REAL(kind=8) :: rKCO3, rKH1o, fbCO3, fbH1, omegaFES, delta23, delta_23
      REAL(kind=8) :: R8, R10, R11, R12, R13, R14, R15, R23, R_23 
      REAL(kind=8) :: GAM13, GAM23, GAM33, GAM14, GAM24, GAM34
      REAL(kind=8) :: TMP1, TMP2, TMP3, TMP4, RNO3, RMN2, RFE2, RSO4, RNH4, RO2
      REAL(kind=8) :: RCH2O1, RCH2O2, RDOM, RMNO, RFEOH3, RFES, RTC, RTS, RALK 


! 
!    STOICHIOMETRY (Cappellen and Wang 1996)
!           (Primary reactants)
! 
      x1=SC1
      y1=SN1
      z1=SP1
      x2=SC2
      y2=SN2
      z2=SP2
      x3=SC3
      y3=SN3
      z3=SP3
! 
! ......................................................................
      IF (TESTO2  .LE. ZERO) TESTO2 = ZERO
      IF (TESTSO4 .LE. ZERO) TESTSO4 = ZERO
      IF (TESTMNO .LE. ZERO) TESTMNO = ZERO
      IF (TESTMN2 .LE. ZERO) TESTMN2 = ZERO
      IF (TESTFE3 .LE. ZERO) TESTFE3 = ZERO
      IF (TESTFE2 .LE. ZERO) TESTFE2 = ZERO
      IF (TESTTS  .LE. ZERO) TESTTS = ZERO

! 
!  Use the Q10 relationship to determine the rates
!  Assume that TEMP is the maximum temperature
! 
      rq1 =  KG1
      rq2 =  KG2
      rq3 =  KDOM
      Tq1 = TEMP
      Tq2 = dtemp
      rq21 = LOG10(rq1)-LOG10(2.)*((Tq2-Tq1)/10.)
      rq21 = 10.0**rq21
      rq22 = LOG10(rq2)-LOG10(2.)*((Tq2-Tq1)/10.)
      rq22 = 10.0**rq22
      rq23 = LOG10(rq3)-LOG10(2.)*((Tq2-Tq1)/10.)
      rq23 = 10.0**rq23

! 
!   ------ Now for the chemolithoautotrophic processes
! 
      rq1 = k11
      rq2 = k11
      rq11 = LOG10(rq1)-LOG10(2.)*((Tq2-Tq1)/10.)
      rq11 = 10.0**rq11
      rq12 = LOG10(rq2)-LOG10(2.)*((Tq2-Tq1)/10.)
      rq12 = 10.0**rq12

! 
!  lets oxidants determine the rate of organic
!           degradation using the full Monod relationship
! 
!  Calculate the concentration of OMs from Flux
! 
        RCt1 = rq21*TESTOM1
        RCt2 = rq22*TESTOM2
        RCt3 = rq23*TESTOM3
        R(1) = TESTO2/(KO2+TESTO2)
        R(2) = TESTNIT/(KNO3+TESTNIT) * RNITRATE_SDM(TESTO2)
        R(3) = TESTMNO/(KMNO+TESTMNO) * RMANGANESE(TESTO2,TESTNIT)
        R(4) = TESTFE3/(KFE3+TESTFE3) * RFERRIC(TESTO2,TESTNIT,TESTFE3)
        R(5) = TESTSO4/(KSO4+TESTSO4) * RSULFATE(TESTO2,TESTNIT,TESTMNO,TESTFE3)  ! WM: TESTMN0 --> TESTMNO
        R(1) = DMAX1(ZERO,R(1))
        R(2) = DMAX1(ZERO,R(2))
        R(3) = DMAX1(ZERO,R(3))
        R(4) = DMAX1(ZERO,R(4))
        R(5) = DMAX1(ZERO,R(5))
        Rt  = R(1)+R(2)+R(3)+R(4)+ R(5)
        IF (RT .LT. ZERO) write(*,*) 'RT', Rt, TESTO2, TESTNIT, TESTSO4
!         write(*,811) R(1),R(2),R(3),R(4),R(5)
        DO 10 j = 1, 5
          R1(j)=  R(j)*RCt1
          R2(j)=  R(j)*RCt2
          R3(j)=  R(j)*RCt3
   10   CONTINUE
! 
!  Call an updated salclosed to get the distribution of carbonate
!  alkalinity species and H+. This formulation is from Whitman and 
!  Turner and is not appropriate for a sediment diagenetic model.
!  Van Cappellen and Wang, (1995) has a more appropriate formulation.
! 
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
         HS = ZERO
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
! 
!  Rate laws from Van Cappellen and Wang (table 3)
! 
      R8   = k8*TESTFE2*TESTO2
      R10  = k10*TESTMNO*TESTFE2
      R11  = rq11*TESTNH3*TESTO2* rMandy(TESTTS)
      R12  = rq12*TESTTS*TESTO2
      R13  = k13*TESTTS*TESTMNO      ! R13 in Van Cappellan
      R14  = k14*TESTTS*TESTFE3
      R15  = k15*TESTFES*TESTO2
      R23  = k23*delta23*(omegaFES-1.0)
      R_23 = k_23*delta_23*TESTFES*(1.0-omegaFES)
! 
! 
!   This section deals with solubility controlled reaction of the form
!   H+ + FeSx -> fe2+ +  HS-; giving Kspx= aFe2+ aHS-/aH+
!   In rearrangement and substitution of gammaC mC for aC where
!   aC is the activity of a species, mC is the molar concentration,
!   and gammaC is the activity coefficient. We assume that gamma HS-
!   is the same as that for gammaCl-
!   For the moment we assume all HS-
! 
!      rKspx=1.14e-3
!      gammaFe= 0.17
!      gammaHS= 0.63
!      rmHS= TESTTS/1.0D3          ! convert to M
!      rmFE = rKspx/gammaFe/gammaHS * 10 **(-pH) /rmHS
!      rmFE =  rmFE *1.0D3       ! this is the max allowed by eq kintics
! 
  811  format(5 f9.3)
!         rKmax=10.0D2             ! was 10.0D2
!         k23_1= rKmax*(TESTFE2-rmFE)   ! note change in K units
!         IF (k23_1 .LE. ZERO) k23_1=ZERO
!         R23  = k23_1*delta23*(omegaFES-1.0)
! 
!   Reaction rates from TABLE 5 of Van Cappellen and Wang (1996)
! 
! ......................................................................
      GAM13  = (x1+TWO*y1)/x1    ! AEROBIC RESP O2/CH2O
      GAM23  = (x2+TWO*y2)/x2    ! AEROBIC RESP O2/CH2O
      GAM33  = (x3+TWO*y3)/x3    ! AEROBIC RESP O2/CH2O
      GAM14 =(FOUR*x1+THREE*y1)/FIVE/x1 ! DENITRIFICATION NO3-/ CH20
      GAM24 =(FOUR*x2+THREE*y2)/FIVE/x2 ! DENITRIFICATION NO3-/ CH20
      GAM34 =(FOUR*x3+THREE*y3)/FIVE/x3 ! DENITRIFICATION NO3-/ CH20
      TMP1 = -(GAM13*R1(1)+GAM23*R2(1)+ONE/F*GAM33*R3(1))-(TWO*R15)
      RO2    = F*(TMP1)-(R8/FOUR+TWO*R11+TWO*R12)
      TMP1 = F*((y1/x1*R1(1)+y2/x2*R2(1)+ONE/F*y3/x3*R3(1)) &
            -(GAM14*R1(2)+GAM24*R2(2)+ONE/F*GAM34*R3(2)))
      RNO3  = TMP1+R11
      RMN2  = F*(TWO*(R1(3)+R2(3)+R10))+TWO*R3(3)
      RFE2  = F*(FOUR*(R1(4)+R2(4))+TWO*R14+R15 &
              -R23+R_23)-R8 + FOUR*R3(4)
      RSO4  = F*(-(R1(5)+R2(5))/TWO+R15)-R3(5)/TWO+R12
      TMP1  = (y1/x1*(R1(4)+R1(5))+y2/x2*(R2(4)+R2(5)))
      RNH4  = F*TMP1+y3/x3*(R3(4)+R3(5)+R3(5))-R11
      RCH2O1= - (R1(1)+R1(2)+R1(3)+R1(4)+R1(5))
      RCH2O2 = - (R2(1)+R2(2)+R2(3)+R2(4)+R2(5))
      RDOM   = - (R3(1)+R3(2)+R3(3)+R3(4)+R2(5))
      RMNO   = - TWO*(R1(3)+R2(3)+(R3(3)/F))-R10-R13
      RFEOH3 = - FOUR*(R1(4)+R2(4)+R3(4)/F)+R8/F-TWO*R14
      RFES   = -R15+R23-R_23
      RTC    = F*(R1(1)+R1(2)+R1(3)+R1(4)+R1(5)+R2(1)+R2(2) + &
               R2(3)+R2(4)+R2(5)) + R3(1)+R3(2)+R3(3)+R3(4)+R3(5)
!       RTS    = F*((R1(5)+R2(5))/TWO-R14-(TWO*(R23+R_23))+
!      *         R3(5)/TWO-R12
      RTS    = F*((R1(5)+R2(5))/TWO-R13-R14-R23+R_23) + R3(5)/TWO-R12
      IF (TESTALK .LE. ZERO) THEN
       RALK = ZERO
      ELSE
      TMP1=-(Y1+TWO*Z1)/X1*R1(1)+(FOUR*X1+THREE*Y1-10.*Z1)/5./X1*R1(2) &
            +(FOUR*X1+Y1-TWO*Z1)/X1*R1(3)+(EIGHT*X1+Y1-TWO*Z1)/X1*R1(4) &
            +(X1+Y1-TWO*Z1)/X1*R1(5)
      TMP2=-(Y2+TWO*Z2)/X2*R2(1)+(FOUR*X2+THREE*Y2-10.*Z2)/5./X2*R2(2) &
            +(FOUR*X2+Y2-TWO*Z2)/X2*R2(3)+(EIGHT*X2+Y2-TWO*Z2)/X2*R2(4) &
            +(X2+Y2-TWO*Z2)/X2*R2(5)
      TMP3=-(Y3+TWO*Z3)/X3*R3(1)+(FOUR*X3+THREE*Y3-10.*Z3)/5./X3*R3(2) &
             +(FOUR*X3+Y3-TWO*Z3)/X3*R3(3)+(EIGHT*X3+Y3-TWO*Z3)/X3*R3(4) &
             +(X3+Y3-TWO*Z3)/X3*R3(5)
      TMP4   = TMP1+TMP2+TMP3/F+TWO*R13+FOUR*R14 &
             -TWO*R23+TWO*R_23
      RALK   = F*TMP4-TWO*R8-TWO*R11-TWO*R12
      IF (TESTALK .LE. ZERO) RALK = ZERO
      ENDIF
!     Set the reaction rates in a vector 
! 
!        Rc(1)=  ZERO
!        Rc(2)=  ZERO
!         Rc(3)=  ZERO
!        Rc(4)=  ZERO
!        Rc(5)=  ZERO
!        Rc(6)=  ZERO
!        Rc(7)=  ZERO
!        Rc(8)=  ZERO
!        Rc(9)=  ZERO
!        Rc(10)= ZERO
!        Rc(11)= ZERO
!        Rc(12)= ZERO
!        Rc(13)= ZERO
!        Rc(14)= ZERO
!        Rc(15)= ZERO
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
      END SUBROUTINE REACTION_SDM

!  ----------------------------------------------------------------
!  CARBON
!  PURPOSE: CLOSED SYSTEM CALCULATIONS
!  ----------------------------------------------------------------
! 
      SUBROUTINE salclosed_SDM(spc,s,ALK,TC,TS,t,pH)
      IMPLICIT NONE

      REAL(kind=8), INTENT(IN) :: ALK, TC, TS, t, pH
      REAL(kind=8), INTENT(OUT)  :: spc(9)
      REAL(kind=8) :: H1, alphe0, alphe1, alphe2, alpheS1
      REAL(kind=8) :: beta1, beta2, beta1S
      REAL(kind=8) :: hco3c, co3c, h2co3c, ALKc, hs, h2s 
      REAL(kind=8) :: rK1, rK2, rK1s 
      REAL(kind=8) :: rK(9), s

      CALL thermowater(rK,s,t)

      rK1 = rK(4)
      rK2 = rK(6)
      rK1s = rK(9)
!  pH       
      H1 = 10.**(-pH)
      alphe0 = 1.0/(1+rK1/H1 + rK1*rK2/H1**2)
      alphe1 = 1.0/(H1/rK1 + 1 +rK2/H1)
      alphe2 = 1.0/(H1**2/(rK1*rK2) + H1/rK2 +1)
      alpheS1 = 1.0/(1.0 + H1/rK1s)
      beta1 = alphe1*TC/(alphe1*TC + 2.0*alphe2*TC + alpheS1*TS)
      beta2 = alphe2*TC/((alphe1*TC + 2.0*alphe2*TC) + alpheS1*TS)
      beta1S = alpheS1*TS/(alphe1*TC + 2.0*alphe2*TC + alpheS1*TS)
!       
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

! 
      spc(1) = h2co3c 
      spc(2) = hco3c
      spc(3) = co3c
      spc(4) = h2s
      spc(5) = hs
      spc(6) = h1
      spc(7) = alphe1
      RETURN
      END SUBROUTINE salclosed_SDM

!
! ------------------------------------------------------------------
! PURPOSE: Provides all the equilibrium constants for CO2 sytem
!          These are temperature and salinity dependent from
!          Whitfield and Turner (1986)
! -----------------------------------------------------------------
      SUBROUTINE thermowater(rk,s,tz)
      IMPLICIT NONE
      INTEGER :: i, j
      REAL(kind=8), INTENT(INOUT) :: s 
      REAL(kind=8), INTENT(IN) :: tz 
      REAL(kind=8), INTENT(OUT) :: rk(9) 
      REAL(kind=8) :: a0(8), a1(8), a2(8), b0(8)
      REAL(kind=8) :: rk1s, t

      a0 = (/ 290.9097, 207.6548, 148.0248, 0.0221, 0.5709, 0.9805, 1.4853, 0.5998 /)
      a1 = (/ 14554.21, 11843.79, 8966.9, 34.02, -84.25, -92.65, -192.69, -75.25 /)
      a2 = (/ 45.0575, 33.6485, 24.4344, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      b0 = (/ 0.0, 0.0, 0.0, 0.0, -1.632, -3.294, -5.058, -1.767 /)
      
      IF (s .LE. 0.0) THEN
         s = 0.0
      ENDIF

      t = tz + 273.16     ! absolute temperature

! Temperature dependence of the thermodynamic stability
      DO i=1,3
         rk(i) = exp(a0(i) - a1(i)/t - a2(i)*log(t))  !Ko(i)
      ENDDO
! Sal. and temp. dependence of the stability constant K1
      i=1
      DO j=4,5
         rk(j)= exp(log(rk(i)) + (a0(j) + a1(j)/t + &
         a2(j)*log(t))*SQRT(s) + b0(j)*s/100.0)
      ENDDO
      i=2  
      DO j=6,7
         rk(j)= exp(log(rk(i)) + (a0(j) + a1(j)/t + &
         a2(j)*log(t))*SQRT(s) + b0(j)*s/100.0)
      ENDDO
! Salinity and temperature dependence of K0
       rk(8) = exp(-58.0931+90.5069*(100/t)+22.2940*log(t/100.0) +  &
               (0.027766 - 0.025888*(t/100.0) + 0.0050578*(t/100.0)**2)*s)
 
       rk1s=2.527+1359.96/t-0.206*s**(1.0/3.0)
       rk1s=10.0**(-rk1s)
       rk(9)= rk1s
       RETURN
       END SUBROUTINE thermowater

!   NITRATE    Calculates the Monod type function and feedbacks for
!   NO3- as an electron acceptor.


      REAL(KIND=8) FUNCTION RNITRATE_SDM(O20)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: O20
      REAL(kind=8) :: KPO2, PO2
! JCL, note that KO2 from hypox_input is modified, I removed /HUN
       KPO2 = KO2
       PO2 = O20
       IF (PO2 .LT. ZERO)  PO2 = ZERO
       RNITRATE_SDM = KPO2/(KPO2 + PO2)

      RETURN
      END FUNCTION RNITRATE_SDM
! 
!    RMANGANESE    Calculates the Monod type function and feedbacks for
!    FE+3 as an electron acceptor.
!  
! 
! 
      REAL(KIND=8) FUNCTION RMANGANESE(O20, NO30)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: O20, NO30
      REAL(kind=8) :: KPO2, KPNO3, PO2, PNO3 
! JCL, note that KO2 and KNO3 from hypox_input are modified here, removed/FIFTY
      KPO2 = KO2
      KPNO3 = KNO3
      PO2 = O20
      IF (PO2 .LT. ZERO)  PO2 = ZERO
      PNO3 = NO30
      IF (PNO3 .LT. ZERO)  PNO3 = ZERO
      RMANGANESE = (KPO2/(KPO2+PO2))*(KPNO3/(KPNO3+PNO3))
!
      RETURN
      END FUNCTION RMANGANESE
!
!
!
!   RFERRIC    Calculates the Monod type function and feedbacks for
!   FE+3 as an electron acceptor.
! 
!
!
      REAL(KIND=8) FUNCTION RFERRIC(O20,NO30,MNO)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: O20, NO30, MNO
      REAL(kind=8) :: KPO2, KPNO3, KPMNO, PO2, PNO3, PMNO   
! JCL, note that KO2,KNO3, KMNO from hypox_input are modified here, removed /HUN
      KPO2 = KO2
      KPNO3 = KNO3
      KPMNO = KMNO

      PO2 = O20
      IF (PO2 .LT. ZERO)  PO2 = ZERO
      PNO3 = NO30
      IF (PNO3 .LT. ZERO)  PNO3 = ZERO
      PMNO = MNO
      IF (PMNO .LT. ZERO)  PMNO = ZERO
      RFERRIC = (KPO2/(KPO2+PO2)) * (KPNO3/(KPNO3+PNO3)) * (KPMNO/(KPMNO+PMNO))

      RETURN
      END FUNCTION RFERRIC
!
!
!
!   SULFATE    Calculates the Monod type function and feedbacks for
!   SO4-- as an electron acceptor.
!
!
!
      REAL(KIND=8) FUNCTION RSULFATE(O20,NO30,MNO,FE30)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: O20, NO30, MNO, FE30 
      REAL(kind=8) :: KPO2, KPNO3, KPMNO, KPFE3
      REAL(kind=8) :: PO2, PNO3, PMNO, PFE3 

! JCL, note that KO2,KNO3,KMNO, KFE3 from hypox_input are modified here, removed /THIRTY
      KPO2 = KO2
      KPNO3 = KNO3
      KPMNO = KMNO
      KPFE3 = KFE3
!     KPO2 = KO2
!     KPNO3 = KNO3
!     KPFE3 = KFE3
      PO2 = O20
      IF (PO2 .LT. ZERO)  PO2 = ZERO
      PNO3 = NO30
      IF (PNO3 .LT. ZERO) PNO3 = ZERO
      PMNO = MNO
      IF (PMNO .LT. ZERO)  PMNO = ZERO
      PFE3 = FE30
      IF (PFE3 .LT. ZERO) PFE3 = ZERO
      RSULFATE = (KPO2/(KPO2+PO2)) * (KPNO3/(KPNO3+PNO3)) * (KPMNO/(KPMNO+PMNO)) * (KPFE3/(KPFE3+PFE3))
!
      RETURN
      END FUNCTION RSULFATE

! 
!    FUNCTION: MANDY after guess who
!    Sulfide inhibition TO nitrification. The result is a reduction
!    in the amount of both NO3 and subsequently denitrification IN the
!    presence of high Sulfide concentrations. Calculates the Monod type
!    feedback function.
! 
! 
      REAL(KIND=8) FUNCTION rMandy(HS)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: HS
      REAL(kind=8) :: KPSO4, PHS
! JCL, note that KS04 is modified here, removed /HUN
      KPSO4 = KSO4
      PHS = HS
      IF (PHS .LT. ZERO)  PHS = ZERO
      rMandy = KPSO4 / (KPSO4 +  PHS)
!
      RETURN
      END FUNCTION rMandy


! 
!   SED  A subroutine for the sediment properties.
!        Calculates the value of the depth-dependent porosity (P),
!        derivative of the porosity with depth (DPDX), and the
!        burial velocity (W) at a supplied depth.
! 
!      P = porosity at depth X
!      DPDX = spatial derivative of porosity at depth X
!      W = solid burial velocity at depth X
!      U = porewater advection velocity at depth X
! 
! 
      SUBROUTINE SED(X,P,DPDX,U,W)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: X
      REAL(kind=8), INTENT(OUT) :: P, DPDX, U, W
      REAL(kind=8) :: PS, PS00
! 
      IF(BP.EQ.ZERO.OR.(P0-P00).EQ.ZERO) THEN
           P = P0
           DPDX = ZERO
      ELSE
           P = (P0-P00)*DEXP(-BP*X) + P00
           DPDX = -BP*(P0-P00)*DEXP(-BP*X)
      ENDIF
! 
      PS = ONE - P
      PS00 = ONE - P00
      W = W00*PS00/PS
      U = W00*P00/P
      RETURN
      END SUBROUTINE SED


!   TORT2 Calculates the square of the tortuosity at an input depth X


!         Currently it uses the "universal" correlation from
!         Boudreau (1996, GCA, V. 60).  This can be replaced with any
!         desired functionality.
!
!         T2 = the square of the tortuosity at depth X (as it appears
!              in Berner's(1980) version of the diagenetic equations.
!         DT2DX = spatial derivative of T2 at depth X.


      SUBROUTINE TORT2(T2,DT2DX,P,DPDX,X)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: P, DPDX, X
      REAL(kind=8), INTENT(OUT) :: T2, DT2DX

      T2 = ONE - TWO*DLOG(P)
      DT2DX = -FOUR/P*DPDX

      RETURN
      END SUBROUTINE TORT2



!   DB	Contains the expressions for the depth-dependent
!		mixing coefficient


      REAL(KIND=8) FUNCTION DB(X)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: X
! 
      IF(X.LE.X1) DB = DB0
      IF(X.GT.X1.AND.X.LT.X2) DB = DB0*(X2-X)/(X2-X1)
      IF(X.GE.X2) DB = ZERO
! 
      RETURN
      END FUNCTION DB
! 
! 
! 
! 
!    DDB	Contains the depth derivatives of DB(X)
! 
! 
      REAL(KIND=8) FUNCTION DDB(X)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: X
! 
      IF(X.LE.X1) DDB = ZERO
      IF(X.GT.X1.AND.X.LT.X2) DDB = -DB0/(X2-X1)
      IF(X.GE.X2) DDB = ZERO
! 
      RETURN
      END FUNCTION DDB




!   SIG	Calculates the weighting for the finite difference
!		approximation for the advective term (see Boudreau,
!		1986, Amer. J. Sci., v. 286, p.192)

      REAL(KIND=8) FUNCTION SIG(X,W)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: X, W
      REAL(kind=8) :: D, E

      D = DB(X)
      IF (D.NE.ZERO) THEN
          E = W * DH / D / TWO
          IF(E.NE.ZERO) SIG = ONE/DTANH(E) - ONE/E
          IF(E.EQ.ZERO) SIG = ZERO
          RETURN
      ENDIF
      IF(D.EQ.ZERO) SIG = ONE
      RETURN
      END FUNCTION SIG


!***********************************************************************
!   FILL_Y Parses output vector for each species
!
!***********************************************************************

      SUBROUTINE FILL_Y(NEQ1,np,nss,Y,G1,G2,O2,rNO3,rNH4,rMN2, &
                        FE3,FE2,SO4,HS,FES,TC,ALK,DOM,Os,Ob)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NEQ1, np, nss
      INTEGER, PARAMETER :: MAXNEQ = 27000
      REAL(kind=8), INTENT(IN) :: Y(NEQ1)
      REAL(kind=8), INTENT(OUT) :: G1(:),G2(:),O2(:),rNO3(:),rNH4(:),rMN2(:)
      REAL(kind=8), INTENT(OUT) :: SO4(:),HS(:),FE2(:),FES(:),TC(:),ALK(:)
      REAL(kind=8), INTENT(OUT) :: DOM(:), Os(:), Ob(:)
      REAL(kind=8) :: FE3(:)
      INTEGER :: NPM1, I, M
      INTEGER :: MID1, MID2, MID3, MID4, MID5, MID6, MID7, MID8
      INTEGER :: MID9, MID10, MID11, MID12, MID13, MID14, MID15, MID16, MID17
! 
      NPM1 = NPOINTS - 1
! 
!   Following statements gives the # of midpoints NPOINTS -1
!      
      DO  I = 2, NPM1
       M = I * NS              ! starts at 18 for 9 species

! 
!   Provide index for the the finite difference scheme
! 
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
      END SUBROUTINE FILL_Y

! 
! 
! 
!    ROOTINT Integrates and averages results over rootzone
! 
      SUBROUTINE ROOTINT(NPOINTS,G1,G2,O2,rNO3,rNH4, &
                         rMN2,FE2,SO4,HS,FES,TC,ALK,DOM,Os,Ob, &
                         tempG1,tempG2,tempO2,tempNO3,tempNH4,tempMN2, &
                         tempFE2,tempSO4,tempHS,tempFES,tempTC,tempALK,tempDOM, &
                         tempOs,tempOb)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOINTS
      REAL(kind=8), INTENT(IN) :: G1(:),G2(:),O2(:),rNO3(:),rNH4(:),rMN2(:)
      REAL(kind=8), INTENT(IN) :: FE2(:),SO4(:),HS(:),FES(:),TC(:),ALK(:),DOM(:)
      REAL(kind=8), INTENT(IN) :: Os(:),Ob(:)
      REAL(kind=8), INTENT(OUT) :: tempG1,tempG2,tempO2,tempNO3,tempNH4,tempMN2
      REAL(kind=8), INTENT(OUT) :: tempFE2,tempSO4,tempHS,tempFES,tempTC,tempALK
      REAL(kind=8), INTENT(OUT) :: tempDOM, tempOs,tempOb
      INTEGER :: I, NROOT, NHALFR, NLOW, NHI, NDIF 
! 
! 
      NROOT  =  INT(ZROOT/DH)
      NHALFR =  INT(WROOT/2./DH)
      NLOW   =  NROOT - NHALFR
      NHI    =  NROOT + NHALFR
      NDIF   =  NHI - NLOW
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
      DO I = NLOW, NHI
          tempG1 =  tempG1 + G1(I)
          tempG2 =  tempG2 + G2(I)
          tempO2 =  tempO2 + O2(I)
          tempNO3 = tempNO3 + rNO3(I)
          tempNH4 = tempNH4 + rNH4(I)
          tempMN2 = tempMN2 + rMN2(I)
          tempSO4 = tempSO4 + SO4(I)
          tempHS =  tempHS + HS(I)
          tempFE2 = tempFE2 + FE2(I)
          tempFES = tempFES + FES(I)
          tempTC =  tempTC + TC(I)
          tempALK = tempALK + ALK(I)
          tempDOM = tempDOM + DOM(I)
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
       END SUBROUTINE ROOTINT


! 
!   OUTVEC Produces output for the the MATLAB program "rootzone"
! 
!      -----------------------------------------------------------------
!      FORMAT FOR MATLAB FILE
!      -----------------------------------------------------------------
      SUBROUTINE OUTVEC(rootG1,rootG2,rootO2,rootNO3,rootNH4,rootMN2, &
                        rootFE2,rootSO4,rootHS,rootFES,rootTC,rootALK,rootDOM, &
                        rootOs,rootOb,mm)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: rootG1(:),rootG2(:),rootO2(:)
      REAL(kind=8), INTENT(IN) :: rootNO3(:),rootNH4(:), rootMN2(:)
      REAL(kind=8), INTENT(IN) :: rootFE2(:),rootSO4(:),rootHS(:)
      REAL(kind=8), INTENT(IN) :: rootFES(:),rootTC(:), rootALK(:)
      REAL(kind=8), INTENT(IN) :: rootDOM(:),rootOs(:),rootOb(:)
      INTEGER, INTENT(IN) :: mm
      INTEGER :: i

! 
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
      END SUBROUTINE OUTVEC



!  OUTFLUX  Produces output of interfacial fluxes for  MATLAB program "rootzone"
!
!     -----------------------------------------------------------------
!     FORMAT FOR MATLAB FILE
!     -----------------------------------------------------------------
      SUBROUTINE OUTFLUX(sedO2,sedNO3,sedNH4,sedSO4,sedDIC,sedDOC, &
                         sedOM1,sedOM2, pycoO2,mm)
      IMPLICIT NONE
      REAL(kind=8), INTENT(IN) :: sedO2(:),sedNO3(:),sedNH4(:)
      REAL(kind=8), INTENT(IN) :: sedSO4(:),sedDIC(:),sedDOC(:)
      REAL(kind=8), INTENT(IN) :: sedOM1(:),sedOM2(:),pycoO2(:)
      INTEGER, INTENT(IN) :: mm
      INTEGER :: i
! 
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
      END SUBROUTINE OUTFLUX

!    ----- PROCEDURE provides input for diagenetic model
! WM: removed parameter "ncol".
! WM: A(100,6) -->  A(100)

        SUBROUTINE datain(A,mrow)
        IMPLICIT NONE
        REAL(kind=8), INTENT(OUT) :: A(100)
        CHARACTER(LEN=25) :: cdum1
        INTEGER, INTENT(IN) :: mrow
        INTEGER :: ilin, nits, dum1
        REAL(kind=8) :: steps

        open(11, file = 'SDM/hypox_input.csv')
!L3 Modify to read in nits and steps
        read(11,*) nits 
        read(11,*) steps
!       first, skip over next five lines
        read(11,*)
        read(11,*)
        read(11,*)
        read(11,*)
        read(11,*)
        write(*,*)'mrow=',mrow
        do ilin = 1, mrow
!         read(11,*)dum1,cdum1,cdum1,cdum1,(a(ilin,j),j=1,ncol)
         read(11,*) dum1, cdum1, cdum1, cdum1, a(ilin)
         write(*,*) 'i=', ilin, 'A=', a(ilin)
        enddo
        close(11)
!L3 Put nits and steps at the end of "A"
        A(mrow+1) = nits
        A(mrow+2) = steps
        !write(6,*) "n_its",nits
        !write(6,*) "step_in",steps
      return
      END SUBROUTINE datain



!***************************************************************
!
!     SUBROUTINE FOR ENTERING DATA FROM A FILE.
!     (ABB 16th April 1996)
!
!***************************************************************

      SUBROUTINE FILEDATA2(RPAR,Ainp)

      IMPLICIT NONE

      INTEGER, PARAMETER :: MAXNEQ = 27000
      INTEGER :: I, IRRG, NSPECIES, IFG

      REAL(kind=8), DIMENSION(MAXNEQ), INTENT(OUT) :: RPAR
      REAL(kind=8), DIMENSION(100), INTENT(IN) :: Ainp
      REAL(kind=8), DIMENSION(24) :: DF
      REAL(kind=8), DIMENSION(9) :: rk
      REAL(kind=8), DIMENSION(2000) :: ppH

      REAL(kind=8) :: YEAR = 3.156D+07
      REAL(kind=8) :: yr_sec, rho, V
      REAL(kind=8) :: T0, TL, rK1, rK2, phl 
      REAL(kind=8) :: alphe0, alphe1, alphe2
      REAL(kind=8) :: hco3c, co3c, h2co3c

      NSPECIES = NS
      yr_sec   = 1/(3600*24*365)
      rho = 2.65

!     INPUT TEMP (oC), SALINITY AND PRESSURE (atm)

      TEMP = Ainp(1)
      SAL = Ainp(2)
      PRESS = Ainp(3)
      pH0 = Ainp(4)

      CALL DDIFCOEF(V,DF,SAL,TEMP,PRESS)
      DO2  = DF(2)*YEAR
      DNO3 = DF(12)*YEAR
      DMN2 = DF(22)*YEAR
      DNH3 = DF(10)*YEAR
      DSO4 = DF(23)*YEAR
      DHS  = DF(5)*YEAR
      DFE2 = DF(21)*YEAR
      DTC  = DF(8)*YEAR*1.5D00
      DALK = DF(8)*YEAR*1.5D00

!  Surface value of Bioturbation

       DB0 = Ainp(5)

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
!L3   These are set in Flux_CGEM.  Need to be set here too to
!L3   define the common block.  Do not change Ainp(44) outside
!L3   of Flux_CGEM.
      NPOINTS = Ainp(44)
      NEQ = NPOINTS * NSPECIES + 2

!     MAXIMUM DEPTH FOR CALCULATIONS
      XL = Ainp(45)
      DH = XL/FLOAT(NPOINTS)
      DO 86 I = 1, NPOINTS
         RPAR(I) = FLOAT(I) * DH
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


!These are read in in Flux_CGEM
!L3      OPEN(11,STATUS='UNKNOWN', file='ph2bprofile.dat')
!L3        READ(11,*) (ppH(i),i=1,NPOINTS)
!L3     CLOSE(11)

!        OPEN(11,STATUS='UNKNOWN', file='standard2b.dat')
!L3        OPEN(11,STATUS='UNKNOWN', file='normoxia.dat')
!L3          READ(11,*) (YY(i),i=1,NEQ)
!L3        CLOSE(11)



      CALL thermowater(rK,SAL,TEMP)
      rK1 = rK(4)
      rK2 = rK(6)
      phl = 10.**(-pH0)
      alphe0= 1.0/(1+rK1/phl + rK1*rK2/phl**2)
      alphe1= 1.0/(phl/rK1 + 1 +rK2/phl)
      alphe2= 1.0/(phl**2/(rK1*rK2) + phl/rK2 +1)
! 
      hco3c  = TC0 * alphe1
      co3c   = TC0 * alphe2
      h2co3c = TC0 * alphe0
! 
      ALK0 = hco3c + 2.*co3c
! 

      RETURN
      END SUBROUTINE FILEDATA2

END Module Sediment_Diagenesis_Routines
