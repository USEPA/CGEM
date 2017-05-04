      subroutine Calc_Sal(Sal)

      USE Model_dim

      IMPLICIT NONE

      real, intent (out) :: Sal(im,jm,nsl) !Salinity in psu
      character(200) filename

       write(filename,'(A, A)') trim(DATADIR),'/S.dat'
       open (19,file=filename,status='old')
       read (19,*)  !Assumes header file
       read (19,*) Sal
       close(19)


      return 
      end subroutine Calc_Sal 
