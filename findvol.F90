subroutine findvol(vol)
!Returns average value of density (rav) and total volume (vol) 
!from common array rho 
  implicit none
  include 'runhydro.h'
 
!*
!*  Global Variables

   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*
!*  Local Variables  
   real :: m, dr, pi,vol,r,Re,dphi
   integer :: i,j,k,count
    
    
!  print*, ">>> rhoavg"
  Pi=3.14159265359
  count=0
   
   
   dr=1.0/(numr-1)
   dphi=2*pi/numphi     
   Re=(ax-1.5)/(numr-1.5)
   vol=0.0
   
   do i=2,ax
     do j=2,numz  
       do k=1, numphi        
         r=(i-1.5)*dr
         if (rho(i,j,k).gt.0.0) then
           vol=vol+dphi*r*dr**2
          !count=count+1
         endif
       enddo
     enddo
   enddo 
   
   vol=vol*2/Re**3
   
!   print*, "rhoavg vol=",vol,"volcount", count
!   print*, "rhoavg density=",rav
   
   
end subroutine findvol
   
