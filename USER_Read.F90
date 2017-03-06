      subroutine USER_Read(TC_8,Var,which)

      USE Model_dim
      USE DATE_TIME

      IMPLICIT NONE

      integer*8, intent (in) :: TC_8
      real, intent (out) :: Var(IM,JM)
      integer, save :: init=1
      integer*8,save :: t1,t2
      integer*8 :: savet
      real,save :: Var1,Var2
      real :: fac,savev
      character, intent(in) :: which
      character*100 :: filename
      integer :: ifile
!    Specify variables for dates and times
      integer iYr, iMon, iDay, iHour, iMin, iSec
      
      iSec = 0

      ifile = 2000 + ichar(which)

      if(init.eq.1) then

        select case(which)
         case("p")  !Solar Radiation
          filename = "data/INPUT/Solar.dat"
         case("w")  !Wind Speed
          filename = "data/INPUT/Wind.dat"
         case("t")  !Temperature
          filename = "data/INPUT/Temp.dat"
         case("s")  !Temperature
          filename = "data/INPUT/Sal.dat"
         case default
           write(6,*) "Error in USER_Read, input=",which
        end select 

        open(unit=ifile,file=filename,status="old")

        !First line
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,Var1
        t1 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

        if(t1.gt.TC_8) then
           write(6,*) t1,TC_8,TC_8-t1
           write(6,*) "Data of type ",which," does not start early enough, exiting"
!"
           stop
        endif

        !Second line
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,Var2
        t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

        do
         if(t2.lt.TC_8) then
          t1=t2
          Var1=Var2
          read(ifile,*) iYr,iMon,iDay,iHour,iMin,Var2
         else
          exit
         endif
        enddo

        init=0
      endif

      if(t2.le.TC_8) then
        t1=t2
        Var1=Var2
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,Var2
        t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
      endif        

      fac = real(TC_8 - t1)
      fac = real(fac,4) / real(( t2 - t1 ),4)
      Var(1,1) = Var1 + ( Var2 - Var1 ) * fac

      return 
      end subroutine USER_Read

