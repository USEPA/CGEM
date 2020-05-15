!*****************************************************************************************
! PURPOSE: USER_Set_Initial_Conditions.F90 - This subroutine reads in initial conditions.
! 
! HISTORY: 2020-05-14 Wilson Melendez  - Added handling of initial conditions in 
!                                        NetCDF format.
!*****************************************************************************************
      Subroutine USER_Set_Initial_Conditions(filename)

      USE Model_dim
      USE State_Vars 
      USE Fill_Value
      USE NETCDF_UTILITIES

      implicit none
 
      integer ii,i,j,k,nz
      integer ierr,isum
      integer init_id, tp_id
      integer ERR
      INTEGER NF_INQ_VARID
      EXTERNAL NF_INQ_VARID
      character(*),intent(in) :: filename
      character(len = 4) :: file_extension = ".txt"
      real init(nf)
      real, dimension(im,jm,km) :: varInitCond

#ifdef map_code
write(6,*) "------USER_Set_Initial_Conditions--"
write(6,*) "  Read one value for each state variable"
write(6,*) "  Then set grid constant"
write(6,*) "  filename=",filename
write(6,*) 
#endif
 
      if (index(filename, file_extension) == 0) then   ! NetCDF file
         call nf_open(filename, 0, init_id)
         ERR = NF_INQ_VARID( init_id, 'TP',tp_id)
         CALL CHKERR( ERR, 'inquire TP id in initial conditions NetCDF file')     
         call nf_get_vara_real(init_id, tp_id, (/1, 1, 1/), (/im, jm, km /), varInitCond)
      
         do j = 1, jm
            do i  = 1, im
               nz = nza(i,j)
               do k = 1, nz
                  f(i,j,k,19) = varInitCond(i,j,k)
               enddo
            enddo
         enddo

         call nf_close(init_id)
      else 
         isum=0
         ! Read in from "InitialConditions.txt" which has 1 value for each state variable.
         ! There is no header line.
         open(19, file=filename, status='old')
         do ii = 1,nf
            read(19,*,IOSTAT=ierr) init(ii)
            isum = isum +  1

           ! write(6,*) "ierr, ii, init",ierr, ii, init(ii)  ! Wilson Melendez
#ifdef DEBUG 
           write(6,*) "ierr,init",ierr,init(ii)
#endif
           if(ierr > 0) then
              write(6,*) "Invalid input at line",isum,"of",filename
              stop
           elseif (ierr < 0 .and. isum .ne. nf) then
              write(6,*) "Input file",filename,"only has",isum, "lines"
              write(6,*) "Expecting",nf
              stop
           endif
         
         enddo
         close(19)
      endif

      return

      end subroutine USER_Set_Initial_Conditions

