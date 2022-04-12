      Subroutine AdvNeighbors(f,my_im,jm,nsl,nf,myid,numprocs)
      !Dummy subroutine for serial code 
      integer, intent(in) :: my_im,jm,nsl,nf,myid,numprocs
      real, intent(in) :: f(0:(my_im+1),jm,nsl,nf)
      ! integer :: myid,my_im,numprocs
      RETURN
      END Subroutine AdvNeighbors 
