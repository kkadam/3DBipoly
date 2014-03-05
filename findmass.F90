subroutine findmass(rho_2i,m_core,m)
!Returns total mass (m) from common array rho
  implicit none
  include 'runhydro.h'
 
!*  Global Variables
   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*  
   real :: m, dr,pi,r,Re,rho_2i,m_core,dm,dphi
   integer :: i,j,k, count
!*   
   
   Pi=3.14159265359 
   dr=1.0/(numr-1)
   dphi=2*pi/numphi       
   
   Re=(ax-1.5)/(numr-1.5)
   m=0.0
   m_core=0.0
   count=0
   
   
   do i=2,ax
     do j=2,numz
       do k=1,numphi        
         r=(i-1.5)*dr  
         dm=rho(i,j,k)*dphi*r*dr**2
         m=m+dm
         if (rho(i,j,k).gt.rho_2i) then         
           m_core=m_core+dm
         endif
       enddo
     enddo
   enddo
   m_core=m_core*2/Re**3
   m=m*2/Re**3
   
   print*,"m_core", m_core
   print*, "m", m
   
   
!   print*,"findmass mass=", m, "masscount", count 
end subroutine findmass
   
