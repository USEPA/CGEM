      Subroutine USER_Set_Initial_Conditions(filename)

      USE Model_dim
      USE State_Vars !f
      USE Fill_Value

      implicit none
 
      integer ii,i,j,k,nz
      integer ierr,isum
      character(*),intent(in) :: filename
      real init(nf)

isum=0
! Read in from "InitialConditions.txt" which has 1 value for each state variable.
! There is no header line.
      open(19,file=filename,status='old')
      do ii = 1,nf
         read(19,*,IOSTAT=ierr) init(ii)
         isum=isum+1
#ifdef DEBUG 
         write(6,*) "ierr,init",ierr,init(ii)
#endif
         if(ierr>0) then
           write(6,*) "Invalid input at line",isum,"of",filename
           stop
         elseif (ierr<0.and.isum.ne.nf) then
           write(6,*) "Input file",filename,"only has",isum, "lines"
           write(6,*) "Expecting",nf
           stop
         endif
         
      enddo
      close(19)

      do j=1,jm
       do i=1,im
          nz=nza(i,j)
          do k = 1, nz
             do ii=1,nf
             f(i,j,k,ii) = init(ii)
             enddo
          enddo
       enddo
      enddo

#ifdef DEBUG 
write(6,*) "------USER_Set_Initial_Conditions--"
write(6,*) "  Read one value for each state variable"
write(6,*) "  Then set grid constant"
write(6,*) "  filename=",filename
write(6,*)
#endif

      return

      end subroutine USER_Set_Initial_Conditions

