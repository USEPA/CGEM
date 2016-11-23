      Subroutine USER_Set_Initial_Conditions(f,fm,wsm)

      USE Model_dim
      USE CGEM_VARS
      USE INPUT_VARS, ONLY:QminN,QminP,QmaxN,QmaxP

      implicit none
 
      real f(im,jm,nsl,nf)
      integer ii,i,j,k,fm(im,jm)
      real wsm(im,jm) 


! Read in from "InitialConditions.txt" which has 1 value for each state variable.
! This is for a single cell.  There is no header line.
      open(19,file="InitialConditions.txt",status='old')
      do ii = 1,nf
         read(19,*) f(1,1,1,ii)
      enddo
      close(19)

! Error checking
      do ii = 1,nospA
        if(f(1,1,1,iQn(ii)).lt.QminN(ii)) then
           write(6,*) "For Qn ",ii,"initial Qn less than QminN" 
           stop
        endif
        if(f(1,1,1,iQp(ii)).lt.QminP(ii)) then
           write(6,*) "For Qp ",ii,"initial Qp less than QminP" 
           stop
        endif
        if(f(1,1,1,iQn(ii)).gt.QmaxN(ii)) then
           write(6,*) "For Qn ",ii,"initial Qn greater than QmaxN" 
           stop
        endif
        if(f(1,1,1,iQp(ii)).gt.QmaxP(ii)) then
           write(6,*) "For Qp ",ii,"initial Qp greater than QmaxP" 
           stop
        endif

      enddo

!Values on shelf will be erroneous, set to -9999 as 'land' marker:
      do ii = 1, nf
       do j=1,jm
        do i=1,im
          if(wsm(i,j).eq.0.) then !if shelf
           f(i,j,nsl,ii) = -9999
          endif
        enddo
       enddo
      enddo


      end subroutine USER_Set_Initial_Conditions

