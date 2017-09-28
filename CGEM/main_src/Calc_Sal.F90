      subroutine Calc_Sal(Sal)

      USE Model_dim

      IMPLICIT NONE

      real, intent (out) :: Sal(im,jm,nsl) !Salinity in psu
      real input_S 
      character(200) filename

       write(filename,'(A, A)') trim(DATADIR),'/S.dat'
       open (19,file=filename,status='old')
       read (19,*)  !Assumes header file
       read (19,*) input_S
       close(19)

       Sal=input_S

#ifdef map_code
      write(6,*)
      write(6,*) "Reads a single value, then initializes whole array"
      write(6,*) "Main data directory, S.dat" 
      write(6,*) "In Calc_Sal, Sal=",input_S
      write(6,*)
#endif


      return 
      end subroutine Calc_Sal 
