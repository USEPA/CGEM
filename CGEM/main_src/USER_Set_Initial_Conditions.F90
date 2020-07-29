      Subroutine USER_Set_Initial_Conditions(filename,myid,numprocs)

      USE Model_dim
      USE State_Vars 
      USE Fill_Value
      use INPUT_VARS, only:icent,jcent
      use serial
      use mpi_interface
      USE NETCDF_UTILITIES

      implicit none

      integer ii,i,j,k,nz
      integer ierr,isum,myi,mpierr
      integer init_id, tp_id
      integer, intent(in) :: myid, numprocs
      character(*),intent(in) :: filename
      character(len = 4) :: file_extension = ".txt"
      real init(nf)
      real, dimension(im,jm,km) :: varInitCond

      !eventually put this in NETCDF_UTILITIES?
      integer ERR
      INTEGER NF_INQ_VARID
      EXTERNAL NF_INQ_VARID

   if (index(filename,file_extension) == 0 ) then !NetCDF file

     if(myid.eq.0) then

       call nf_open(filename, 0, init_id)
       ERR = NF_INQ_VARID( init_id, 'TP', tp_id)
       CALL CHKERR( ERR, 'inquire TP id in initial conditions NetCDF file')
       call nf_get_vara_real(init_id, tp_id, (/1,1,1/), (/im,jm,km/), varInitCond)
     
     endif

     if(numprocs.gt.1) call MPI_BCAST(varInitCond,im*jm*km,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

     do j=1,jm
          myi=1
       do i=myi_start,myi_end
          nz=nza(i,j)
          do k = 1, nz
             do ii=1,nf
               f(myi,j,k,19) = varInitCond(i,j,k)
             enddo
          enddo
          myi = myi+1
       enddo
     enddo

     call nf_close(init_id)
     
   else

     isum=0
     ! Read in from "InitialConditions.txt" which has 1 value for each state variable.
     ! There is no header line.
     if(myid.eq.0) then
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
     endif

     if(numprocs.gt.1) call MPI_BCAST(init,nf,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
        do j=1,jm
            myi=1
         do i=myi_start,myi_end
            nz=nza(i,j)
            do k = 1, nz
               do ii=1,nf
               f(myi,j,k,ii) = init(ii)
               enddo
            enddo
            myi = myi+1
         enddo
        enddo

   endif !index(filename.....)




#ifdef DEBUG 
write(6,*) "------USER_Set_Initial_Conditions--"
write(6,*) "  Read one value for each state variable"
write(6,*) "  Then set grid constant"
write(6,*) "  filename=",filename
write(6,*) "first state var at i,j,k=",f(icent,jcent,1,1),icent,jcent,1
write(6,*)
#endif

      return

      end subroutine USER_Set_Initial_Conditions

