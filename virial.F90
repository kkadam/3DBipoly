subroutine virial(T,W,P,omega,rho_2i)
  implicit none
  include 'runhydro.h'

  real, dimension(numr,numz,numphi) :: pot, rho
  common /poisson/ pot, rho  

  real, dimension(numr,numz,numphi) :: enth
  common /vir/enth  
  
  
  real :: T, W, P, omega, r, m, dr, pi, press, Re, rho_2i, dphi
  integer :: i,j,k
  
  Pi=3.14159265359
  
  dr=1.0/(numr-1)
  dphi=2*pi/numphi     
  Re=(ax-1.5)/(numr-1.5)
  print*, dr
  !Find rotational energy T
  T=0.0
  do i=2,ax
    do j=2,numz 
      do k=1, numphi
        r=(i-1.5)*dr
        m=rho(i,j,k)*dphi*r*dr**2
        T=T+0.5*m*r**2*omega**2
      enddo
    enddo
  enddo  
  T=T*2/Re**5

  !Find Potential energy W
  W=0.0
  do i=2,ax
    do j=2,numz  
      do k=1, numphi
        r=(i-1.5)*dr
        W=W-0.5*pot(i,j,k)*rho(i,j,k)*dphi*r*dr**2
      enddo
    enddo
  enddo   	  
  W=W*2/Re**3  

  !Find pressure energy P
  P=0.0
  do i=2,ax
    do j=2,numz 
      do k=1, numphi          
        r=(i-1.5)*dr
        if (rho(i,j,k).gt.rho_2i) then
          press=rho(i,j,k)*enth(i,j,k)/(1.0+np1)
        else
          press=rho(i,j,k)*enth(i,j,k)/(1.0+np2) 
        endif
        P=P+press*dphi*r*dr**2
      enddo
    enddo
  enddo  
  P=P*2/Re**3 
  return
end subroutine virial
