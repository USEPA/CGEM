      subroutine USER_Read(TC_8,Var,which,init)

      USE Model_dim
      USE DATE_TIME
      USE INPUT_VARS, ONLY:Read_Solar,START_SECONDS
      IMPLICIT NONE

      integer*8, intent (inout) :: TC_8
      integer, intent(in) :: init ! Zero is regular already called, 
                                  ! 1 is regular first call, 
                                  ! 2 is need to rewind and close 
      real, intent (out) :: Var(IM,JM)
      integer*8,save :: t1,t2
      real,save :: Var1,Var2
      real :: fac
      character, intent(in) :: which
      character(100) :: filename
      integer :: ifile
!    Specify variables for dates and times
      integer iYr, iMon, iDay, iHour, iMin, iSec

      ifile = 2000 + ichar(which)

      if(init.gt.0) then

        select case(which)
         case("p")  !Solar Radiation
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Solar.dat'
#ifdef LT
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Solar.lt.dat'
#endif
#ifdef DK
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Solar.dk.dat'
#endif
#ifdef LTNT
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Solar.ltnt.dat'
#endif
         case("w")  !Wind Speed
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Wind.dat'
         case("t")  !Temperature
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Temp.dat'
#ifdef LT
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Temp.lt.dat'
#endif
#ifdef DK
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Temp.dk.dat'
#endif
#ifdef LTNT
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Temp.ltnt.dat'
#endif
         case("s")  !Temperature
          write(filename,'(A, A)') trim(DATADIR),'/INPUT/Sal.dat'
         case default
           write(6,*) "Error in USER_Read, input=",which
        end select 

        open(unit=ifile,file=filename,status="old")

        !First line
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,iSec,Var1
        t1 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
#ifdef DEBUG
        write(6,*) "iYr0",iYr0,iYr,iMon,iDay,iHour,iMin,iSec
        write(6,*) "t1",t1,real(t1,4)/3600/24/365
        write(6,*) "TC_8",TC_8,real(TC_8,4)/3600/24/365
        write(6,*) "diff_days",(TC_8-t1)/3600./24.
#endif

        if(t1.gt.TC_8) then
           write(6,*) "Data of type ",which," does not start early enough, exiting"
!"
           stop
        endif

        !Second line
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,iSec,Var2
        t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
#ifdef DEBUG
          write(6,*) "t1,t2,T8",t1,t2,TC_8
#endif

        do
         if(t2.lt.TC_8) then
          t1=t2
          Var1=Var2
          read(ifile,*) iYr,iMon,iDay,iHour,iMin,iSec,Var2
          t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
#ifdef DEBUG
          write(6,*) "t1,t2,T8",t1,t2,TC_8
          write(6,*) "var1,var2",Var1,Var2
#endif
         else
          exit
         endif
        enddo

      endif


      if(t2.le.TC_8) then
        t1=t2
        Var1=Var2
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,iSec,Var2
        t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
      endif        

      fac = real(TC_8 - t1)
      fac = real(fac,4) / real(( t2 - t1 ),4)
      Var(1,1) = Var1 + ( Var2 - Var1 ) * fac


      if(init.eq.2) rewind(ifile)

      return 
      end subroutine USER_Read

