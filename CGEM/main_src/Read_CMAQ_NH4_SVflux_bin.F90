!#define DEBUG

! file: Read_CMAQ_NH4_SVflux_bin.F90

      Subroutine Read_CMAQ_NH4_SVflux_bin(TC_8,NH4_SVflux)      
      
!----------------------------------------------------------------------      
! Read CMAQ NH4 dry + wet deposition vertical flux and time interpolate

! Original Version for wind, based on code from Ko, written by Lisa 
! Lowe: Jan 2014
! 
! Modifications by Barry Herchenroder: Feb 2014 and May-July 2014
!----------------------------------------------------------------------

       USE Model_dim ! For iYr0, etc.
       USE DATE_TIME ! For SECONDS_PER_DAY, TOTAL_SECONDS, DATE_TIMESTAMP

       IMPLICIT NONE
       
     !--------------------------  
     ! Declare/define parameters
     !--------------------------           
       
!       INTEGER, PARAMETER :: Num_sfc_cells = no !42898 ! sfc cells with sea-water on them   

       INTEGER :: Num_sfc_cells 					   

       CHARACTER(LEN=48), parameter :: headNH4 = 'Dry_NH4_+_Wet_NH4_deposition(mmole-NH4/m**2/sec)' 

     !---------------------------------------       
     ! Declare subroutine interface variables 
     !--------------------------------------- 
     
       integer*8, intent(in)  :: TC_8    ! Model time (in seconds after
                                         ! Jan 1, 2002, 00:00:00 GMT)
					 ! at center of timestep istep. 
					 
       real, intent(out)   :: NH4_SVflux(im,jm) 
                                         ! Vertical flux of NH4 at the 
					 ! sea surface at middle of timestep   
					 ! istep. Units of NH4_SVflux are: 
					 !    mmol-NO3/m**2/sec 
					 
!    Specify the title from the data file header
       character(LEN=48), SAVE :: srTitle       

!    Specify variables for dates and times
       integer       :: iYr , iMon, iDay, iHour 
       integer, SAVE :: iMin, iSec        

!    Looping variables
       integer i, j, istat, noc, step
       
       INTEGER  :: iter

!    Time
       INTEGER(8):: SECONDS
       real*8 DTC_8 ! TC_8 converted to fractional days.  					  	    
       real*8, save       :: DTC_max_8     ! maximum value of DTC_8 that is
                                           ! correct at the present time.
					   ! DTC_8 must be < DTC_max_8 DTC_max_8
					   ! corresponds to beginning of 
					   ! Jan 1 2008.
          					   
       real*8, save       :: Day1_8,Day2_8 ! Read in File date in decimal days
       real*8, save       :: DayDif_fact_8       
       
!    Specify NH4 and NH4 vertical flux variables 

!       REAL(KIND=8)          :: NH4_SVflux_dum(no)                              
!       REAL(KIND=8)   , SAVE :: NH4_1_a_dum(no)  
!       REAL(KIND=8)   , SAVE :: NH4_2_a_dum(no) 

       REAL(KIND=8), DIMENSION(IM,JM) :: NH4_SVflux_dum 
       REAL(KIND=8), DIMENSION(IM,JM) :: NH4_1_a_dum 
       REAL(KIND=8), DIMENSION(IM,JM) :: NH4_2_a_dum 
                                                                                 
       INTEGER, SAVE :: init = 1
              
       INTEGER       :: iYr_max
       INTEGER       :: iMon_max  
       INTEGER       :: iDay_max 
       INTEGER       :: iHr_max 
       INTEGER       :: iMin_max 
       INTEGER       :: iSec_max 
       
       INTEGER       :: LOC       
       
       INTEGER, SAVE :: unit9
       
!---------------------
! Character variables 
!---------------------

      CHARACTER blank1_dum_1*1
      CHARACTER blank1_dum_2*1   
      CHARACTER blank1_dum_3*1        
      
      CHARACTER blank3_dum_1*3
      
      CHARACTER blank22_dum_1*22 

      character(100) :: filename

!---------------------------------------------------------------------
!----------------       
! Begin main code
!---------------- 
   
       DTC_8 = TC_8
       DTC_8 = DTC_8 / SECONDS_PER_DAY
       unit9 = 9000
       Num_sfc_cells = im * jm

       NH4_SVflux_dum = 0.0
       NH4_1_a_dum = 0.0
       NH4_2_a_dum = 0.0

IF(init .eq. 0 ) THEN

!---------------------------------------------------------------------------
! If we get here, this is not the first time the present subroutine has been
! called. DTC_max_8 has already been calculated 
!---------------------------------------------------------------------------

      IF(DTC_8 >= DTC_max_8 ) THEN
         ! If we get here, DTC_8 is too large. Abort run.
         WRITE(6, "(' ')")
         WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")    
         WRITE(6, "('DTC_8 = ',F35.20,' is >= DTC_max_8 = ',F35.20)")    & 
	 &           DTC_8,                   DTC_max_8
         WRITE(6, "('Abort  ')")
	 STOP	   
      ENDIF

ELSE

!------------------------------------------------------------------------
! If we get here, this is the first time the present subroutine has been
! called
!------------------------------------------------------------------------


iMin = 0
iSec = 0

!---------------------------------------------------------------------- 
!    Calculate DTC_max_8, max value of DTC_8--DTC_8 must be < DTC_max_8
!    DTC_max_8--corresponds to 12:00 midnight at beginning of 1 Jan 2008
!----------------------------------------------------------------------

      iYr_max  = 2008
      iMon_max = 1
      iDay_max = 1
      iHr_max  = 0
      iMin_max = 0
      iSec_max = 0 

      SECONDS = TOTAL_SECONDS( iYr0, iYr_max, iMon_max, iDay_max, &
                               iHr_max, iMin_max, iSec_max )
      DTC_max_8 = SECONDS
      DTC_max_8 = DTC_max_8 / SECONDS_PER_DAY

#ifdef DEBUG
      WRITE(6, "(' ')")
      WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")
      WRITE(6, "(' ')")      
      WRITE(6, "('   DTC_max_8 = ', F35.20)")  DTC_max_8 
      WRITE(6, "(' ')")         
      WRITE(6, "('which corresponds to 12:00 midnight at the beginning')")
      WRITE(6, "('of 1 Jan 2008 ')")       
      WRITE(6, "(' ')")            
#endif      

!    Open the CMAQ NH4 deposition file with binary data in it
        !---------------------------------------------------------------
        ! Replace RHS of file below with the actual path to combined
        ! years (2001-2008) NH4 file.The data for 2004 is dummy data for
        ! a leap year since 2004 was a leap year. Robin Dennis CMAQ
        ! generated NH4 data for 2004 is not available yet....
        ! Note years 2001, 2004, and 2008 have fake data
        !---------------------------------------------------------------
       
       write(filename,'(A, A)') trim(DATADIR),'/INPUT/NH4_sfc_vertflx_bin_some_fake.2001-2008'
       open (unit9, file = filename, status = 'old', form = 'UNFORMATTED')  
!       open (unit9, file=NONHYDRODIR//"NH4_sfc_vertflx_bin_some_fake.2001-2008",status='old', form='UNFORMATTED')       
      
      rewind unit9

!----------------------------------------------------------------
!   Read the header - number of surface cells(noc), title srTitle
!----------------------------------------------------------------

      !  header of file for iYr at beginning of file for NH4 in iYr
         READ(unit9) blank3_dum_1, noc, blank22_dum_1, srTitle       
 
       if(noc .ne. Num_sfc_cells) then
	 WRITE(6, "(' ')")  
	 WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")	      
         write(6,*) "noc is .ne. Num_sfc_cells"
	 WRITE(6,*) "Abort Run."
	 WRITE(6, "(' ')") 
         STOP
        endif
	
       if(srTitle /= headNH4) THEN
	 WRITE(6, "(' ')")  
	 WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")	      
         write(6,*) "srTitle is .ne. headNH4 the header title"
	 WRITE(6,*) "Abort Run."
	 WRITE(6, "(' ')") 
         STOP
        endif       
       
       step = 0
       iter = 0
!-----------------------------------------------------------------------       
! Read in dates and NH4 dry + NH4 wet deposition data . NH4 deposition 
! data is for each month of 2002,2003,2004,2005,2006,2007. Fake data 
! for Dec 2001 and Jan 2008. 
!-----------------------------------------------------------------------

!----------------------------------------------------------------------- 
! At present, only the CMAQ vertical NH4 flux input data for 2002,
! 2003, 2004,2005, 2006,2007 have been generated for the present 
! subroutine. There is also fake data for 2001,
! and 2008. 2001 is the same as 2002 and 2008 is the same
! as 2007. Only Dec of 2001 and Jan of 2008 are used
!----------------------------------------------------------------------- 
       DO
       
       iter = iter + 1
!-------------------------------------------------------------------------    
!   Read in dates & times of data--first one is on line 2 of file attached
!   to Fortran unit unit9 = 9000. 
!-------------------------------------------------------------------------

#ifdef DEBUG
       ! Read to end of line
	   WRITE(6, "(' ')")  
	   WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")	
	   WRITE(6, "('at beginning of outer block DO loop')")  	   
	   WRITE(6, "('for iter = ', I6)") iter
	   WRITE(6, "(' ')")  	      	    	 
#endif       
              
       ! line with date/time for Dec of iYr = 2001
	 READ(unit9, IOSTAT = istat) iYr, blank1_dum_1, iMon, blank1_dum_2, iDay, blank1_dum_3, iHour
        ! write(6,*) iYr, iMon, iDay, iHour 

         if(iHour.ge.24) then
             iHour = iHour - 24 
             iDay = iDay + 1
         endif

!------------------------
!   Test for end of file.
!------------------------

         if (ISTAT < 0) then
	   LOC = 1
	   WRITE(6, "(' ')")  
	   WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin,')")
	   WRITE(6, "('at LOC = ', I2, ' an EOF encountered   ')") LOC	   
           write(6, "('in NH4 vertical flux input data.       ')")        
	   WRITE(6, "('Exit sub. Read_CMAQ_NH4_SVflux_bin.    ')")
	   WRITE(6, "(' ')") 	   
           EXIT
         endif
	 
! Print iYr, iMon, iDay, iHour just read.

#ifdef DEBUG
   WRITE(6, "(' ')")  
   WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")	
   WRITE(6, "('Data read from line 2 of unit=unit9=9000 is:')") 
   WRITE(6, "(' ')") 
   WRITE(6, "('iYr  = ',I4,' iMon = ',I2,' iDay = ',I2,' iHour = ',I2 )")  &
   &           iYr,          iMon,         iDay,         iHour
   WRITE(6, "('init = ',I2,' iter = ', I6)") init, iter	
   WRITE(6, "(' ')")
#endif	           	 

!------------------------------------------------------------------
!   If we get here, an eof has not been encountered. Therefore read 
!   CMAQ NH4 deposition data.
!------------------------------------------------------------------

#ifdef DEBUG
   WRITE(6, "(' ')")  
   WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")	
   WRITE(6, "('begin read of CMAQ NH4 vertical flux deposition data ')")
   WRITE(6, "(' ')")  
   WRITE(6, "('At this point,  ')") 
   WRITE(6, "('iYr  = ',I4,' iMon = ',I2,' iDay = ',I2,' iHour = ',I2 )")  &
   &           iYr,          iMon,         iDay,         iHour
   WRITE(6, "('init = ',I2,' iter = ', I6)") init, iter
   WRITE(6, "(' ')")
#endif 	   	

      !-----------Read in data to NH4_2_a SVflux array------------------- 
      
         ! Note Well:  NH4_2_a_dum should be KIND=8           

         read (unit9,IOSTAT = ISTAT)  NH4_2_a_dum 
         ! write(6,*) NH4_2_a_dum(1), NH4_2_a_dum(no)

!------------------------
!   Test for end of file.
!------------------------

         IF (ISTAT < 0) THEN
	   LOC = 2
	   WRITE(6, "(' ')")  
	   WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin,')")
	   WRITE(6, "('at LOC = ', I2, ' an EOF encountered   ')") LOC	   
           write(6, "('in NH4 vertical flux input data.       ')")        
	   WRITE(6, "('Exit sub. Read_CMAQ_NH4_SVflux_bin.    ')")
	   WRITE(6, "(' ')") 	   
           EXIT
         ENDIF		    
		    		    
#ifdef DEBUG		     
   WRITE(6, "(' ')") 
   WRITE(6, "('NH4_2_a_dum values just read for i = 1, IM and j = 1, JM are: ')")
!   DO ij = 1, no		 
!      WRITE(6,"('ij = ',I6,' NH4_2_a_dum(ij) = ',F35.20)")  &
!      &          ij,         NH4_2_a_dum(ij)
!   ENDDO
   DO j = 1, JM
      DO i = 1, IM
         WRITE(6,"('I = ',I3,' J = ', I3,' NH4_2_a_dum(i,j) = ',F35.20)") I, J, NH4_2_a_dum(i,j)
      ENDDO
   ENDDO
   WRITE(6, "(' ')")
#endif		    

#ifdef DEBUG
   WRITE(6,"(' ')")  
   WRITE(6,"('In subroutine Read_CMAQ_NH4_SVflux_bin,      ')")	
   WRITE(6,"('end read of CMAQ NH4 surface flux input data.')")
   WRITE(6,"('for, at this point,  ')") 
   WRITE(6,"('iYr  = ',I4,' iMon = ',I2,' iDay = ',I2,' iHour = ',I2 )") &
   &           iYr,         iMon,         iDay,         iHour
   WRITE(6,"('init = ',I2,' iter = ', I6)") init, iter
   WRITE(6,"(' ')")
#endif 	   	   
      !--End Read in data to NH4_2_a_dum array------------------------
      
!---------------------------------------------------------------
!  For read in data associated with iYr,iMon, iDay, iHour, 
!  get associated Model Time Day2_8 in decimal days.
!  IF iYr = 2001, set Day2_8 to
!  -15.5_8 (that is a minus sign)  where 15.5 is the # of decimal 
!  days in last half of Dec 2001
!---------------------------------------------------------------
  
     IF( iYr > 2001) THEN 
        SECONDS = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
        Day2_8 = SECONDS
        Day2_8 = Day2_8 / SECONDS_PER_DAY
	   ELSEIF ( iYr  == 2001) THEN  
	      Day2_8 = -15.5_8 
	   ELSE
	      WRITE(6, "(' ')")  
	      WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")
	      WRITE(6, "('for iYr <= ', I4)") iYr
	      WRITE(6, "('Data for a date-time other than')")  
	      WRITE(6, "('iMon = 12--iDay = 15--iHour=12 ')") 
	      WRITE(6, "('was encountered when it should ')")  
	      WRITE(6, "('not have been. Abort Run ')") 
	      WRITE(6, "(' ')") 
	      STOP 	         	   	      
	   ENDIF 
	  
           if(step.eq.0) then
              Day1_8 = Day2_8
           else
              if(DTC_8.ge.Day1_8.and.DTC_8.le.Day2_8) exit
           endif
 
           Day1_8  = Day2_8
           NH4_1_a_dum = NH4_2_a_dum   ! array op
           step    = 1 

#ifdef DEBUG
	  WRITE(6, "(' ')")  
	  WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")
	  WRITE(6, "('at end of outer block DO loop ')") 
	  WRITE(6, "('for iter = ', I6)")  iter 		  
	  WRITE(6, "(' ')")		  
#endif
       enddo   ! End of "DO" block DO
       
       init=0

ENDIF  ! End section executed only on initial subroutine call


      if(DTC_8.le.Day2_8) then  ! Date-Time DTC_8 is between or at input 
                               ! data dates/times Day1_8 and Day2_8
			       
        NH4_SVflux    = 0.0  ! array op
	DayDif_fact_8 = (DTC_8 - Day1_8)/(Day2_8-Day1_8)
 
       DO j = 1, JM
           DO i = 1, IM
              NH4_SVflux_dum(i,j)  = NH4_1_a_dum(i,j) + (NH4_2_a_dum(i,j) - NH4_1_a_dum(i,j))*DayDif_fact_8
              NH4_SVflux(i,j) = REAL(NH4_SVflux_dum(i,j),KIND=4)
           ENDDO
        ENDDO 

!	 DO ij = 1, no
!            NH4_SVflux_dum(ij)  = NH4_1_a_dum(ij) +                    &
!	    &  (NH4_2_a_dum(ij) - NH4_1_a_dum(ij))*DayDif_fact_8
!	    i                   = io(ij)
!	    j                   = jo(ij)	 
!	    NH4_SVflux(i,j)     = REAL(NH4_SVflux_dum(ij),KIND=4)	    	 
!	 ENDDO
		 
      else  ! else part of if(DTC_8.le.Day2_8) then-else

!------------------------------------------------------------------------      
!Reset Time and NH4 using array ops and look at next input data date-time:
!------------------------------------------------------------------------ 
        Day1_8       = Day2_8
        NH4_1_a_dum  = NH4_2_a_dum    ! both of these are KIND=8

!---------------------------------------
!Read in new CMAQ NH4 deposition Data:
!   Read in next input data date-times .
!---------------------------------------

       ! read (unit9,IOSTAT = istat) iYr, iMon, iDay, iHour
	 READ(unit9, IOSTAT = istat) iYr, blank1_dum_1, iMon, blank1_dum_2, iDay, blank1_dum_3, iHour 
         !write(6,*) iYr, iMon, iDay, iHour

         if(iHour.ge.24) then
             iHour = iHour - 24
             iDay = iDay + 1
         endif

!------------------------
!   Test for end of file.
!------------------------

         if (ISTAT < 0) then
	   LOC = 3
           WRITE(6, "(' ')")	
           WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin,    ')")
           WRITE(6, "('at LOC = ', I2                              )") 
           write(6, "('NH4 surface flux input data EOF encountered')")
           WRITE(6, "('unexpectedly. Find problem and fix it.     ')")	   
	   WRITE(6, "('Abort Run.                                 ')")
           WRITE(6, "(' ')")	
	   STOP  
         endif

!----------------------------------------------------------------------	  
!   If not end of data file, read CMAQ NH4 deposition surface flux data
!----------------------------------------------------------------------

      !-----------Read in CMAQ NH4 deposition data to NH4_2_a_dum array--------
	   
	  read (unit9, IOSTAT = ISTAT) NH4_2_a_dum
          !write(6,*) NH4_2_a_dum(1),NH4_2_a_dum(no)
!------------------------
!   Test for end of file.
!------------------------

         if (ISTAT < 0) then
	   LOC = 4
	   WRITE(6, "(' ')")  
	   WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")
	   WRITE(6, "('at LOC = ', I2, ' an EOF was encountered')") LOC	   
           write(6, "('in NH4 vertical flux input data.        ')")  
	   WRITE(6, "('on unit9=9000.                          ')")  	       
	   WRITE(6, "('Exit sub. Read_CMAQ_NH4_SVflux_bin.     ')")
	   WRITE(6, "('and abort run ')") 	   
	   WRITE(6, "(' ')") 	   
           STOP
         endif		    	  
	  
	   	   
      !--End Read in data to NH4_2_a_dum array------------------------

!---------------------------------------------------------------
!  For read in data associated with iYr,iMon, iDay, iHour, 
!  get associated Model Time Day2_8 in decimal days.
!---------------------------------------------------------------

        SECONDS = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
        Day2_8 = SECONDS
        Day2_8 = Day2_8 / SECONDS_PER_DAY

!----------------------------	
!-----------end read new data
!----------------------------
	
! Note Bene: NH4_SVflux(i,j) is KIND=4 and NH4_SVflux_dum(ij),
!            NH4_2_a_dum, and NH4_1_a_dum are KIND=8.	

        NH4_SVflux    = 0.0  ! array op
	DayDif_fact_8 = (DTC_8 - Day1_8)/(Day2_8-Day1_8)

        DO j = 1, JM
           DO i = 1, IM
              NH4_SVflux_dum(i,j)  = NH4_1_a_dum(i,j) + (NH4_2_a_dum(i,j) - NH4_1_a_dum(i,j))*DayDif_fact_8
              NH4_SVflux(i,j)     = REAL(NH4_SVflux_dum(i,j),KIND=4)
           ENDDO
        ENDDO

!	 DO ij = 1, no
!            NH4_SVflux_dum(ij)  = NH4_1_a_dum(ij) +                    &
!	    &  (NH4_2_a_dum(ij) - NH4_1_a_dum(ij))*DayDif_fact_8
!	    i                   = io(ij)
!	    j                   = jo(ij)	 
!	    NH4_SVflux(i,j)     = REAL(NH4_SVflux_dum(ij),KIND=4)	    	 
!	 ENDDO
	 
     endif ! End of if(DTC_8.le.Day2_8) block IF-THEN-ELSE


#ifdef DEBUG
      WRITE(6, "(' ')")	 
      WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")	            
      write(6, "('For NH4 vertical flux a sea surface:')")
      write(6, "('Day1_8          = ', F35.15)") Day1_8      
      write(6, "('DTC_8            = ', F35.15)") DTC_8
      write(6, "('Day2_8          = ', F35.15)") Day2_8   
      WRITE(6, "(' ')")	
      WRITE(6, "('Examples of raw and time-interpolated values ')")
      WRITE(6, "('around the grid are the sea and land sfce are: ')") 
      WRITE(6, "(' ')")	              
      WRITE(6, "('NH4_1_a_dum(IM/2,JM/2)             = ', F35.15)")         &
      &           NH4_1_a_dum(IM/2,JM/2)      
!      WRITE(6, "('io(no/2)                      = ', I3)") io(no/2)  
!      WRITE(6, "('jo(no/2)                      = ', I3)") jo(no/2)          
      WRITE(6, "('NH4_SVflux(IM/2,JM/2) = ', F35.15)")         &
      &           NH4_SVflux(IM/2,JM/2)         
      WRITE(6, "('NH4_2_a_dum(IM/2,JM/2)             = ', F35.15)")         &
      &           NH4_2_a_dum(IM/2,JM/2)  
      WRITE(6, "(' ')") 
      WRITE(6, "(' ')") 
      WRITE(6, "('In subroutine Read_CMAQ_NH4_SVflux_bin, ')")  
      write(6, "('For NH4 vertical flux at sea surface:')") 
      WRITE(6, "(' ')")              

    DO j = 1, JM, JM/4
      DO i = 1, IM, IM/8
         WRITE(6, "('NH4_1_a_dum(i,j)      = ', F35.15)") NH4_1_a_dum(i,j) 
         WRITE(6, "('NH4_SVflux_dum(i,j)   = ', F35.15)") NH4_SVflux_dum(i,j)   
         WRITE(6, "('NH4_2_a_dum(i,j)      = ', F35.15)") NH4_2_a_dum(i,j)
         WRITE(6, "(' ')")  
      ENDDO	  	    
    ENDDO
 
    DO j = 1, JM, JM/4  
       DO i = 1, IM, IM/8  
          WRITE(6, "('NH4_SVflux(i,j)   = ', F35.15)") NH4_SVflux(i,j)   
          WRITE(6, "(' ')")           
       ENDDO   
    ENDDO            
#endif

       RETURN

       END SUBROUTINE Read_CMAQ_NH4_SVflux_bin  

