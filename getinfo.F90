subroutine getinfo(h_0,h_max,rho_2i,count,cput)
!Prints summary of the run on the screen and in a file 
  implicit none
  include 'runhydro.h'

  real::rav, mom, m, vol, h_0,am, mac_x,mac_y,pi,rb,h_max,p_max,&
  cput, omega, T, W, P, rho_2i, m_core, frac_core, r_core
  character(len=100) :: filename
  character*20 char_np1, char_ax, char_by, char_numr, char_numz, char_m  
  character*20 char_vol, char_rav, char_mom, char_h_0, char_am, char_rb, &
  char_p_max,char_mac_x,char_mac_y,char_cput,char_count, char_bx, char_T, &
  char_W, char_3P, char_ix, char_np2, char_mu1, char_mu2, char_rcore, &
  char_m_core, char_frac_core
  integer :: count
  
  Pi=3.14159265359
  rb=(by-1.5)/(ax-1.5)
  
  call findmass(rho_2i,m_core,m)
  print*, "!!",m_core,"!!",m,"!!",rho_2i,"!!"
  call findvol(vol)
  print*,"!!",vol,"!!"
  call findmom(mom)  
  print*,"!!",mom,"!!"
  frac_core = m_core/m
  
  print*,"frac_core", frac_core
  
  rav=m/vol
  
  omega=(abs(h_0))**(0.5)
  am=mom*omega
  
  mac_y=h_0/4.0/pi/rav
  mac_x=am**2/(4.0*pi*m**(10.0/3)*rav**(-1/3.0))
  
  p_max=h_max/(1.0+np1)
  
  r_core=(ix-1.5)*1.0/(ax-1.5)  
  
  call virial(T,W,P,omega,rho_2i)
  
  
  
!!Convert numbers to strings
  

  if (ax.lt.100) then
    write (char_ax, "(I2)") ax
  else
    write (char_ax, "(I3)") ax
  endif

  if ((by.lt.100).and.(by.gt.9)) then
    write (char_by, "(I2)") by
  elseif (by.lt.10) then  
    write (char_by, "(I1)") by
  else
    write (char_by, "(I3)") by
  endif

  if ((bx.lt.100).and.(bx.gt.10)) then
    write (char_bx, "(I2)") bx
  elseif (bx.lt.10) then  
    write (char_bx, "(I1)") bx
  else
    write (char_bx, "(I3)") bx
  endif 
  
  write (char_np1, "(F3.1)") np1
  write (char_np2, "(F3.1)") np2
  write (char_numr, "(I3)") numr
  write (char_numz, "(I3)") numz
  write (char_ix, "(I2)") ix
  write (char_mu1,"(F3.1)") mu1
  write (char_mu2,"(F3.1)") mu2
  write (char_rcore,"(F6.4)") r_core
  
  write (char_m, "(F6.4)") m
  write (char_m_core, "(F6.4)") m_core
  write (char_frac_core, "(F6.4)") frac_core
  write (char_vol, "(F6.4)") vol
  write (char_rav, "(F6.4)") rav
  write (char_mom, "(F6.4)") mom
  write (char_h_0, "(F6.4)") h_0
  write (char_am, "(F6.4)") am
  write (char_rb, "(F6.4)") rb
  write (char_T, "(F6.4)") T
  write (char_W, "(F6.4)") W
  write (char_3P, "(F6.4)") 3*P
  
  write (char_p_max, "(F6.4)") p_max
  write (char_mac_x, "(F6.4)") mac_x
  write (char_mac_y, "(F6.4)") mac_y
  write (char_count, "(I2)") count
  write (char_cput, "(F8.4)") cput
  
  
  
  if (bx==2) then
!!Make filename	
    filename='Bi3D_'//trim(char_ix)//"_"//trim(char_np1)//'w'//trim(char_mu1)&
    //'_'//trim(char_np2)//'w'//trim(char_mu2)//'_'//trim(char_ax)//"x"//&
    trim(char_by)//"_"//trim(char_numr)//".info"   
  else
    filename='Bi3D_-'//trim(char_ix)//"_"//trim(char_np1)//'w'//trim(char_mu1)&
    //'_'//trim(char_np2)//'w'//trim(char_mu2)//'_'//trim(char_ax)//"x"//&
    trim(char_by)//"_"//trim(char_numr)//".info" 
  endif
  	  
  	  
  print*,"================================SUMMARY===================================="
  print*,"n_core  = ", trim(char_np1), "  n_env = ", trim(char_np2)
  print*,"mu_core = ", trim(char_mu1), "  mu_env = ", trim(char_mu2)
  print*,"Equatorial r_core = ", trim(char_rcore)
  print*, "Core mass = ", trim(char_m_core)
  print*, "Core mass fraction = ", trim(char_frac_core)
  print*,"Resolution = ", trim(char_numr),"x", trim(char_numz)
  print*,"b/a = ", trim(char_by), "/", trim(char_ax)
  print*,"  rb  ","  Omega_sq  ","  M     ", "  V   ","    J   ","    T   ", "    -W   "&
  ,"  3PI  ","   P_max  "
  print*,trim(char_rb),"   ",trim(char_h_0),"   ",trim(char_m),"  ", trim(char_vol), &
  "  ",trim(char_am),"  ",trim(char_T),"  ",trim(char_W),"  ",trim(char_3P),"  ",  &
  trim(char_p_max)
  
  print*,"cpu time =", trim(char_cput) , " min"
  
  print*,"==============================OUTPUT FILES================================="  
  
!!Data is in following format
!!np1 np2 mu1 mu2 ix by ax numr numz r_core rb m_core frac_core Omega_sq M V J T -W 3PI P_max 
  
  open(unit=10,file=filename)
  write(10,*) trim(char_np1)," ",trim(char_np2)," ",trim(char_mu1)," ",trim(char_mu2),&
  " ",trim(char_ix)," ",trim(char_by)," ",trim(char_ax), " ",trim(char_numr)," ", &
  trim(char_numz)," ",trim(char_rcore)," ",trim(char_rb)," ",trim(char_m_core)," ",&
  trim(char_frac_core)," ",trim(char_h_0)," ",trim(char_m)," ",trim(char_vol)," ", trim(char_am),&
  " ",trim(char_T)," ", trim(char_W)," ",trim(char_3P)," ",trim(char_p_max)," ",trim(char_count),&
  " ",trim(char_cput)
  close(10)

  print*, trim(filename)


end subroutine getinfo
