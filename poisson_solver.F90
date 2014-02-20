      subroutine poisson_solver
      implicit none
      include 'runhydro.h'
!************************************************************
!*
!*  Global Variables

      real, dimension(numr,numz,numphi) :: pot, rho
      common /poisson/ pot, rho

!*
!************************************************************      
!*
!*   Local variables


!*
!************************************************************      
	
      call setup

      call potsetup

      call potential_solver      !No Prob 

      return
      end subroutine poisson_solver

