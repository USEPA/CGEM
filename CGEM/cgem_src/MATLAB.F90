!
!
!  MATLAB Produces output for the the MATLAB program "fourout"
!
!     ------------------------------------------------------------
!     FORMAT FOR MATLAB  FILE
!     ------------------------------------------------------------
      SUBROUTINE MATLAB(NEQ1, A1,IC)

      USE Sediment_Diagenesis_Routines
      USE Model_Diagenesis, ONLY: tir_O2,tir_NO,tir_HN,tir_SO,tir_TC
      USE SDM, ONLY: NS, NPOINTS

      IMPLICIT NONE
      INTEGER, intent(in) :: IC, NEQ1
      CHARACTER(LEN=8) :: fmt
      INTEGER :: i, j, NEM1
      INTEGER, PARAMETER :: MAXNEQ = 27000

      CHARACTER (LEN = 2) :: CHR
      CHARACTER (LEN = 4) :: filenameSmall
      CHARACTER (LEN = 10) :: filename
      CHARACTER (LEN = 3) :: CHR2
      CHARACTER (LEN = 5) :: filenameSmall2
      CHARACTER (LEN = 11) :: filename2

      REAL(kind=8) :: A1(NEQ1)
      REAL(kind=8) :: SPECIEN
      REAL(kind=8) :: sedO2, sedNO3, sedNH4, sedSO4, sedDIC, sedOM1, sedOM2

!  TAKE OUT THE VERY SMALL VALUES FROM THE MODEL OUTPUT
!
      DO j = 1, NEQ1
           IF (ABS(A1(j)) .LE. 1.0E-9) THEN
               A1(j) = 0.0
           ENDIF
      ENDDO
      DO i = 1,NEQ1
            IF (ABS(RATE(i)) .LE. 1.0E-9) THEN
               RATE(i) = 0.0
            ENDIF
      ENDDO
      NEM1 = NEQ1

!   Convert "IC" FROM INTEGER TO character and add it TO FILE name for
!   profile

       IF (IC .LT. 10) then
         fmt = '(I1)'
         write(CHR,fmt) IC
         filenameSmall= 'Pz0'//CHR       ! concatanate w/ file name
         filename= filenameSmall//'.dat'           ! continuous w/ .dat;has some blanks??
         OPEN (12,FILE=filename,STATUS='UNKNOWN',FORM='FORMATTED')
            WRITE(12,"(E12.4)") (A1(i),i=1,NEM1)
         CLOSE(12)

         filenameSmall= 'Rz0'//CHR       ! concatanate w/ file name
         filename= filenameSmall//'.dat'           ! continuous w/ .dat;has some blanks??
         OPEN (12,FILE=filename,STATUS='UNKNOWN',FORM='FORMATTED')
            WRITE(12,"(E12.4)") (RATE(i),i=1,NEM1)
         CLOSE(12)

         filenameSmall= 'LB0'//CHR       ! concatanate w/ file name
         filename= filenameSmall//'.dat'           ! continuous w/ .dat;has some blanks??


      ElSE IF (IC .LT. 100) THEN
         fmt = '(I2)'
         write(CHR,fmt) IC
         filenameSmall= 'Pz'//CHR       ! concatanate w/ file name
         filename= filenameSmall//'.dat'           ! continuous w/ .dat;has some blanks??
         OPEN (12,FILE=filename,STATUS='UNKNOWN',FORM='FORMATTED')
            WRITE(12,"(E12.4)") (A1(i),i=1,NEM1)
         CLOSE(12)

         filenameSmall= 'Rz'//CHR       ! concatanate w/ file name
         filename= filenameSmall//'.dat'           ! continuous w/ .dat;has some blanks??
         OPEN (12,FILE=filename,STATUS='UNKNOWN',FORM='FORMATTED')
            WRITE(12,"(E12.4)") (RATE(i),i=1,NEM1)
         CLOSE(12)

        filenameSmall= 'LB'//CHR       ! concatanate w/ file name
        filename= filenameSmall//'.dat'           ! continuous w/ .dat;has some blanks??


      ELSE
         fmt = '(I3)'
         write(CHR2,fmt) IC
         filenameSmall2= 'Pz'//CHR2       ! concatanate w/ file name
         filename2= filenameSmall2//'.dat'           ! continuous w/ .dat;has some blanks??
         OPEN (12,FILE=filename2,STATUS='UNKNOWN',FORM='FORMATTED')
            WRITE(12,"(E12.4)") (A1(i),i=1,NEM1)
         CLOSE(12)

         filenameSmall2= 'Rz'//CHR2       ! concatanate w/ file name
         filename2= filenameSmall2//'.dat'           ! continuous w/ .dat;has some blanks??
         OPEN (12,FILE=filename2,STATUS='UNKNOWN',FORM='FORMATTED')
            WRITE(12,"(E12.4)") (RATE(i),i=1,NEM1)
         CLOSE(12)

        filenameSmall2= 'LB'//CHR2       ! concatanate w/ file name
        filename2= filenameSmall2//'.dat'           ! continuous w/ .dat;has some blanks??

      ENDIF

!  Now do the same for the rlab FILE

       sedO2  = FLUXES(1)
       sedNO3 = FLUXES(2)
       sedNH4 = FLUXES(3)
       sedSO4 = FLUXES(4)
       sedDIC = FLUXES(5)
       sedOM1 = FLUXES(6)
       sedOM2 = FLUXES(7)

!
       SPECIEN = FLOAT(NS)
      IF (IC .LT. 100) then
       OPEN (12,FILE=filename,STATUS='UNKNOWN',FORM='FORMATTED')
      ELSE
       OPEN (12,FILE=filename2,STATUS='UNKNOWN',FORM='FORMATTED')
      ENDIF
         WRITE(12,"(E16.6)") W00      !1
         WRITE(12,"(E16.6)") SPECIEN  !2
         WRITE(12,"(E16.6)") DH       !3
         WRITE(12,"(E16.6)") sedOM1   !4
         WRITE(12,"(E16.6)") sedOM2   !5
         WRITE(12,"(E16.6)") sedO2    !6
         WRITE(12,"(E16.6)") sedNO3   !7
         WRITE(12,"(E16.6)") sedNH4   !8
         WRITE(12,"(E16.6)") sedSO4   !9
         WRITE(12,"(E16.6)") sedDIC   !10
         WRITE(12,"(E16.6)") tir_O2   !11
         WRITE(12,"(E16.6)") tir_TC   !12
         WRITE(12,"(E16.6)") tir_NO   !13
         WRITE(12,"(E16.6)") tir_HN   !14
         WRITE(12,"(E16.6)") tir_SO   !15

       CLOSE(12)
      RETURN
      END SUBROUTINE MATLAB
