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
  CHARACTER(LEN=2) :: strFNum
  INTEGER :: ifile, ifnum
  ! Specify variables for dates and times
  INTEGER :: iYr, iMon, iDay, iHour, iMin, iSec
  INTEGER, SAVE :: init = 1

  ifile = 2001
  ConcFile = "Nutrient_Concs_River"
  IF (init == 1) THEN

      ifnum = 1
      WRITE(strFNum, '(I1)') ifnum
      filename = trim(DATADIR) // '/INPUT/' // trim(ConcFile) // trim(strFNum) // '.dat'
      OPEN(unit=ifile, file=filename, status="old")

      !First line
      READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, Conc1(1), Conc1(2), Conc1(3), Conc1(4)
      t1 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

      if (t1 > TC_8) then
          write(6,*) "Nutrient concentration data does not start early enough, exiting"!"
          STOP
      endif

      !Second line
      READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, Conc2(1), Conc2(2), Conc2(3), Conc2(4)
      t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

      do
        if (t2 < TC_8) then
            t1 = t2
            Conc1 = Conc2
            READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, Conc2(1), Conc2(2), Conc2(3), Conc2(4)
            t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
        else
            exit
        endif
      enddo

      init = 0

  ENDIF

  if (t2 <= TC_8) then
      t1 = t2
      Conc1 = Conc2
      READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, Conc2(1), Conc2(2), Conc2(3), Conc2(4)
      t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
  endif        
  
  fac = real(TC_8 - t1)
  fac = real(fac,4) / real(( t2 - t1 ),4)
  River_Conc = Conc1 + ( Conc2 - Conc1) * fac


  RETURN
END SUBROUTINE USER_Read_Nutrient_Concs
