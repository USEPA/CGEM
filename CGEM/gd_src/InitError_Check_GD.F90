      Subroutine InitError_Check_CGEM()

      USE Model_dim
      USE CGEM_VARS
      USE INPUT_VARS_CGEM, ONLY:QminN,QminP,QmaxN,QmaxP
      USE State_Vars

      implicit none
 
      integer ii


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


      end subroutine InitError_Check_CGEM

