!************************************************************
!*
!*  MAIN
!*
!************************************************************
	
program main
      implicit none
      include 'runhydro.h'
      
!************************************************************
!*
!*  Global Variables

      real, dimension(numr,numz,numphi) :: pot, rho
      common /poisson/ pot, rho

      real, dimension(numr,numz,numphi) :: psi
      
      real, dimension(numr,numz,numphi) :: enth
      common /vir/enth
!*
!************************************************************      
!*
!*   Local variables
      real :: w, phi_a, phi_b, h_a, h_b, psi_a,psi_b,phi_c
      real :: rho_c, c_0, rho_norm, h_max
      integer :: i,j,k,count
      real :: cpu1,cpu2, p_max,cput
      real :: phi_i, psi_i, rho_2i, gamma1, gamma2, h_2i
      real :: c1,c2,omega_sq,d_c1,d_c2,d_omega_sq,c1_old,c2_old,omega_sq_old
      real :: k1,k2, re, rho_1i, h_1i, h_norm, rho_2i_norm
      
!* 
!************************************************************    
    
	
      call cpu_time(cpu1)
      print*, "SCF Started!!"   

      gamma1=1+1.0/np1
      gamma2=1+1.0/np2
      
!Guess the initial density
      call guessrho
!      call print1d(rho,"x",1,"rhoix.1")  
!      call print1d(rho,"y",1,"rhoiy.1")
!      call print2d(rho,"rhoi.2") 
	
!Find rotational potential	
      do i=1,numr
        do j=1,numz
          do k=1,numphi
            w=(i-1.5)/(ax-1.5)
            psi(i,j,k)=-w**2/2.0
          enddo
        enddo
      enddo   

   
!Normalization      
      Re=(ax-1.5)/(numr-1.5)
     
      
!!!!!!!Iterate till Convergence!!!!!!!
      d_c1=1
      d_c2=1
      d_omega_sq=1
      count=0
      
      do while ((d_c1 .gt. 1d-2).and.(d_c2.gt.1d-2).and.(d_omega_sq.gt.1d-2))
        count=count+1
        
        
!!!!!!!!!!!!!! wtf       
 !     do i=1, numr
 !       do j= 1, numphi
 !         rho(i,1,j)=rho(i,2,j)
 !       enddo
 !     enddo
       
	
	
	
!!!!!!!!!!!!!!        
!      call print1d(rho,"x",1,"rhox.1")  
!      call print1d(rho,"y",1,"rhoy.1")
!      call print2d(rho,"rho.2")         
        

        !Poisson solve for density      
        call poisson_solver
        pot=pot/Re**2

       
!       call print1d(pot,"x",1,"potx.1") 
!       call print1d(pot,"y",1,"poty.1") 
!       call print2d(pot,"pot.2") 
!Find the constants c1, c2 and omega_sq      
        phi_a=pot(ax,ay,1) 
        phi_b=pot(bx,by,1)
        psi_a=psi(ax,ay,1)
        psi_b=psi(bx,by,1)
        
        phi_i=pot(ix,2,1)
        psi_i=psi(ix,2,1)
        rho_2i=rho(ix,2,1)
        
        rho_1i=rho_2i*mu1/mu2
        
        print*, "phi_a", phi_a
        print*, "phi_b", phi_b
        print*, "phi_i",phi_i
        print*,"psi_a", psi_a
        print*, "psi_b", psi_b
        
        print*, "rho_2i", rho_2i
        
        
!Edited for torous        c2=phi_b
	
	c2=(phi_a*psi_b-phi_b*psi_a)/(psi_b-psi_a)
	
        omega_sq=(c2-phi_a)/psi_a
        
        h_2i=c2-phi_i-omega_sq*psi_i
        
!        K2=h_2i/(np2+1)/rho_2i**(1.0/np2)
!        K1=K2*rho_2i**(gamma2)/rho_1i**(gamma1)
        
        h_1i=h_2i*(np1+1)/(np2+1)*rho_2i/rho_1i
        
        !h_1i= (np1+1)*k1*rho_i**(1/np1)
        !h_norm=h_1i*(np1+1)/(np2+1)*rho_2i/rho_i
        
        c1=h_1i+phi_i+omega_sq*psi_i
        

        
        !Get enthalpy      
        do i=1,numr
          do j=1,numz
            do k=1,numphi
              if (rho(i,j,k).gt.rho_2i) then  
                enth(i,j,k)=  c1 - pot(i,j,k) - omega_sq* psi(i,j,k)
                !enth(i,j,1)=enth(i,j,1)/h_norm
              else
                enth(i,j,k)=  c2 - pot(i,j,k) - omega_sq* psi(i,j,k)	    
              endif
            enddo  
          enddo
        enddo  
        
        h_max=maxval(enth)
        rho_2i_norm=mu2/mu1*(h_1i/h_max)**np1
        
        !Find the new normalized density      
        !enth=enth/h_max
        
      
        do i=1,numr
          do j=1,numz
            do k=1,numphi
	      if (enth(i,j,k).gt.0) then 
	         if (rho(i,j,k).gt.rho_2i) then  
                   rho(i,j,k)=(enth(i,j,k)/h_max)**np1
                   !rho(i,j,1)=enth(i,j,1)/(np1+1)/K1
                 else
              	   rho(i,j,k)=rho_2i_norm*(enth(i,j,k)/h_2i)**np2
              	   !rho(i,j,1)=enth(i,j,1)/(np2+1)/K2	  
                 endif
	      else
                rho(i,j,k)=0.0
              endif  
            enddo
          enddo
        enddo          
      
        rho_norm=maxval(rho)
      
       
        
        
        rho=rho/rho_norm
        d_c1=abs((c1_old-c1)/c1)
        d_c2=abs((c2_old-c2)/c2)
        d_omega_sq=abs((omega_sq_old-omega_sq)/omega_sq)          
  
        c1_old=c1
        c2_old=c2
        omega_sq_old=omega_sq
        
        print*, "Iteration number = ",count
        print*,"c1 = ",c1, "c2 = ",c2, "omega_sq = ", omega_sq
        print*,"d_c1 = ",d_c1, "dc_2 = ", d_c2, "d_omega_sq = ", d_omega_sq  
     call print2d(rho,"rho.2")        
     enddo
     print*,rho_2i 
 
     call cpu_time(cpu2)
     cput=(cpu2-cpu1)/60.0
     
    call print1d(rho,"x",1,"rhofx.1")  
    call print1d(rho,"y",1,"rhofy.1") 
    call print2d(rho,"rhof.2") 
!     call getinfo(omega_sq,c_0,h_max,rho_2i,count,cput)
!     call print2default(rho)
!     call print1default(rho,"x",2)
     print*,"==========================================================================="
      
      
      
      stop
      end program main
