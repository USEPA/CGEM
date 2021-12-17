      subroutine USER_Read_Solar(TC_8,Var)

      USE Model_dim
      USE DATE_TIME

      IMPLICIT NONE

      integer*8, intent (in) :: TC_8
      real, intent (out) :: Var(im,jm)
      integer*8,save :: t1,t2
      real,save :: Var1,Var2, VarTemp
      real :: fac
      character(100) :: filename
      integer :: ifile
!    Specify variables for dates and times
      integer iYr, iMon, iDay, iHour, iMin, iSec
      integer, save :: init=1
      real, parameter :: cv        = 2.77e14 ! multiplicative factor used
                                                 ! to convert from
                                                 ! watts/m2
                                                 ! to photons/cm2/sec
                                                 ! Morel and Smith
                                                 ! (1974)

      ifile = 2001 

      if(init.eq.1) then

        !Solar Radiation
        write(filename,'(A, A)') trim(DATADIR),'/INPUT/Solar.dat'
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
           write(6,*) "Solar data does not start early enough, exiting"
!"
           stop
        endif

        !Second line
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,iSec,Var2
        t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )

#ifdef DEBUG
        write(6,*) "Read_Solar"
        write(6,*) "iYr0",iYr0,iYr,iMon,iDay,iHour,iMin,iSec
        write(6,*) "t1",t1,real(t1,4)/3600/24/365
        write(6,*) "TC_8",TC_8,real(TC_8,4)/3600/24/365
        write(6,*) "diff_days",(TC_8-t1)/3600./24.
        write(6,*)
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

      init=0

      endif


      if(t2.le.TC_8) then
        t1=t2
        Var1=Var2
        read(ifile,*) iYr,iMon,iDay,iHour,iMin,iSec,Var2
        t2 = TOTAL_SECONDS( iYr0, iYr, iMon, iDay, iHour, iMin, iSec )
      endif        

      fac = real(TC_8 - t1)
      fac = real(fac,4) / real(( t2 - t1 ),4)
      !Var(1,1) = Var1 + ( Var2 - Var1 ) * fac
      VarTemp = Var1 + ( Var2 - Var1 ) * fac

      !Var(1,1) = cv*Var(1,1)
      Var = cv*VarTemp

      return 
      end subroutine USER_Read_Solar

