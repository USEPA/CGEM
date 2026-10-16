subroutine USER_Read_Nutrient_Concs(TC_8)
  
  USE Model_dim
  USE DATE_TIME
  USE RiverLoad

  IMPLICIT NONE

  INTEGER(KIND=8), INTENT (IN) :: TC_8
  CHARACTER(100) :: ConcFile
  INTEGER(KIND=8), SAVE :: t1, t2
  REAL, DIMENSION(4), SAVE :: Conc1, Conc2
  REAL :: fac
  CHARACTER(LEN=100) :: filename
  CHARACTER(LEN=3) :: strFNum
  INTEGER :: ifile0, ifnum, iriv
  ! Specify variables for dates and times
  INTEGER :: iYr, iMon, iDay, iHour, iMin, iSec
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: ifile
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: init

  IF (.NOT. ALLOCATED(init)) THEN
       ALLOCATE(init(nRiv))
       init = 1
  END IF
    
  IF (.NOT. ALLOCATED(ifile)) THEN
       ALLOCATE(ifile(nRiv))
       ifile0 = 3000     
       DO iriv = 1, nRiv
          ifile(iriv) = ifile0 + iriv
       END DO
  END IF    

  ConcFile = "Nutrient_Concs_River"
  DO iriv = 1, nRiv
      IF (init(iriv) == 1) THEN

          ifnum = iriv
          IF (iriv < 10) THEN
             WRITE(strFNum, '(I1)') ifnum
          ELSEIF (iriv < 100) THEN
             WRITE(strFNum, '(I2)') ifnum
          ELSE
             WRITE(strFNum, '(I3)') ifnum
          ENDIF
          filename = trim(DATADIR) // '/INPUT/' // trim(ConcFile) // trim(strFNum) // '.dat'
          OPEN(unit = ifile(iriv), file = filename, status = "old")

          !First line: header comment
          READ(ifile(iriv),*)

          !First data line
          READ(ifile(iriv),*) iYr, iMon, iDay, iHour, iMin, iSec, Conc1(1), Conc1(2), Conc1(3), Conc1(4)
          t1 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

          if (t1 > TC_8) then
              write(6,*) "Nutrient concentration data does not start early enough, exiting"!"
              STOP
          endif

          !Second line
          READ(ifile(iriv),*) iYr, iMon, iDay, iHour, iMin, iSec, Conc2(1), Conc2(2), Conc2(3), Conc2(4)
          t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

          do
            if (t2 < TC_8) then
                t1 = t2
                Conc1 = Conc2
                READ(ifile(iriv),*) iYr, iMon, iDay, iHour, iMin, iSec, Conc2(1), Conc2(2), Conc2(3), Conc2(4)
                t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
            else
                exit
            endif
          enddo

          init(iriv) = 0

      ENDIF

      if (t2 <= TC_8) then
          t1 = t2
          Conc1 = Conc2
          READ(ifile(iriv),*) iYr, iMon, iDay, iHour, iMin, iSec, Conc2(1), Conc2(2), Conc2(3), Conc2(4)
          t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
      endif        
  
      fac = real(TC_8 - t1)
      fac = real(fac,4) / real(( t2 - t1 ),4)
      River_Conc(iriv,:) = Conc1 + ( Conc2 - Conc1) * fac

  END DO
  

  RETURN
END SUBROUTINE USER_Read_Nutrient_Concs
