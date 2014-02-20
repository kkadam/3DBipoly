
!Gridsize
       integer, parameter :: numr = 160
       integer, parameter :: numz = 160
       integer, parameter :: numphi = 128

!Polytropic index        
       real, parameter :: np1=5
       real, parameter :: np2=1

       real, parameter :: mu1=5
       real, parameter :: mu2=1
       
!Specify Boundary points A and B for a 2d configuration     
      integer, parameter :: ix=15
      integer, parameter :: ax=150
      integer, parameter :: ay=2
      
      integer, parameter :: bx=2
      integer, parameter :: by=135

!Not sure what these are
       integer, parameter :: rlwb = 2, rupb = numr - 1

       integer, parameter :: zlwb = 2, zupb = numz - 1

       integer, parameter :: philwb = 1, phiupb = numphi

       integer, parameter :: numphi_by_two = numphi / 2

       real, parameter :: numphiinv = 1.0 / numphi

