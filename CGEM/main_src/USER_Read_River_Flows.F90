subroutine USER_Read_River_Flows(TC_8)
  
  USE Model_dim
  USE DATE_TIME
  USE RiverLoad

  IMPLICIT NONE

  INTEGER(KIND=8), INTENT (IN) :: TC_8
  CHARACTER(100) :: FlowFile
  INTEGER(KIND=8), SAVE :: t1, t2
  REAL, SAVE :: River_InFlow1, River_InFlow2
  REAL, SAVE :: River_OutFlow1, River_OutFlow2
  REAL :: fac
  CHARACTER(LEN=100) :: filename
  CHARACTER(LEN=2) :: strFNum
  INTEGER :: ifile, ifnum
  ! Specify variables for dates and times
  INTEGER :: iYr, iMon, iDay, iHour, iMin, iSec
  INTEGER, SAVE :: init = 1

  ifile = 2002
  FlowFile = "River_Flows"
  IF (init == 1) THEN

      ifnum = 1
      WRITE(strFNum, '(I1)') ifnum
      filename = trim(DATADIR) // '/INPUT/' // trim(FlowFile) // trim(strFNum) // '.dat'
      OPEN(unit=ifile, file=filename, status="old")

      !First line: header comment
      READ(ifile,*)

      !First data line
      READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, River_InFlow1, River_OutFlow1
      t1 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

      if (t1 > TC_8) then
          write(6,*) "River flow data does not start early enough, exiting"!"
          STOP
      endif

      !Second line
      READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, River_InFlow2, River_OutFlow2
      t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

      do
        if (t2 < TC_8) then
            t1 = t2
            River_InFlow1 = River_InFlow2
            River_OutFlow1 = River_OutFlow2
            READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, River_InFlow2, River_OutFlow2
            t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
        else
            exit
        endif
      enddo

      init = 0

  ENDIF

  if (t2 <= TC_8) then
      t1 = t2
      River_InFlow1 = River_InFlow2
      READ(ifile,*) iYr, iMon, iDay, iHour, iMin, iSec, River_InFlow2, River_OutFlow2 
      t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
  endif        
  
  fac = real(TC_8 - t1)
  fac = real(fac,4) / real(( t2 - t1 ),4)
  River_InFlow(1) = River_InFlow1 + ( River_InFlow2 - River_InFlow1) * fac
  River_OutFlow(1) = River_OutFlow1 + ( River_OutFlow2 - River_OutFlow1) * fac


  RETURN
END SUBROUTINE USER_Read_River_Flows
