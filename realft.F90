!C-------------------------------------------------------------
!C     REALFT
!C-------------------------------------------------------------

      subroutine realft(data,nx,ny,nz,isign)

!c-------------------------------------------------------------
      implicit none
      integer :: nx, ny, nz, isign
! pmm added declarations for these too

      real,dimension(nx,ny,nz)::data

      real,dimension(nx,ny)::h1i,h1r,h2i,h2r

      real::theta,wi,wpi,wpr,wr,wtemp

      real :: c1, c2, wis, wrs
! pmm I added c1, c2 because version in hydrocode had used implicit
! data typing with ci being implicitly declared real
      integer :: i, i1, i2, i3, i4

      integer :: n2p3

      theta=3.141592653589793/(nz/2)
      c1=0.5
      if (isign.eq.1) then
        c2=-0.5

        call four1(data,nx,ny,nz/2,+1)
      
      else
        c2=0.5
        theta=-theta
      endif
      wpr=-2.0*sin(0.5*theta)**2
      wpi=sin(theta)
      wr=1.0+wpr
      wi=wpi
      n2p3=nz+3
      do i=2,nz/4
        i1=2*i-1
        i2=i1+1
        i3=n2p3-i2
        i4=i3+1
        wrs=wr
        wis=wi
        h1r=c1*(data(:,:,i1)+data(:,:,i3))
        h1i=c1*(data(:,:,i2)-data(:,:,i4))
        h2r=-c2*(data(:,:,i2)+data(:,:,i4))
        h2i=c2*(data(:,:,i1)-data(:,:,i3))
        data(:,:,i1)=h1r+wrs*h2r-wis*h2i
        data(:,:,i2)=h1i+wrs*h2i+wis*h2r
        data(:,:,i3)=h1r-wrs*h2r+wis*h2i
        data(:,:,i4)=-h1i+wrs*h2i+wis*h2r
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
      enddo
      if (isign.eq.1) then
        h1r(:,:)=data(:,:,1)
        data(:,:,1)=h1r(:,:)+data(:,:,2)
        data(:,:,2)=h1r(:,:)-data(:,:,2)
      else
        h1r(:,:)=data(:,:,1)
        data(:,:,1)=c1*(h1r(:,:)+data(:,:,2))
        data(:,:,2)=c1*(h1r(:,:)-data(:,:,2))
        call four1(data,nx,ny,nz/2,-1)
      endif
      return
      end

!C-------------------------------------------------------------
!C     FOUR1
!C-------------------------------------------------------------

      subroutine four1(data,nx,ny,nnz,isign)

!c-------------------------------------------------------------

!      include 'proc.h'

      real,dimension(nx,ny,2*nnz)::data

      real,dimension(nx,ny)::tempi,tempr

      real::theta,wi,wpi,wpr,wr,wtemp
      integer::nx,ny,nnz,isign,i,istep,j,m,mmax,nz

      
      nz=2*nnz
      j=1
      do i=1,nz,2
        if(j.gt.i)then
          tempr=data(:,:,j)
          tempi=data(:,:,j+1)
          data(:,:,j)=data(:,:,i)
          data(:,:,j+1)=data(:,:,i+1)
          data(:,:,i)=tempr
          data(:,:,i+1)=tempi
        endif
        m=nz/2
 1      continue
        if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
      enddo
      mmax=2
 2    continue
      if (nz.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959/(isign*mmax)
        wpr=-2.*sin(0.5*theta)**2
        wpi=sin(theta)
        wr=1.
        wi=0.
        do m=1,mmax,2
          do i=m,nz,istep
            j=i+mmax
            tempr=wr*data(:,:,j)-wi*data(:,:,j+1)
            tempi=wr*data(:,:,j+1)+wi*data(:,:,j)
            data(:,:,j)=data(:,:,i)-tempr
            data(:,:,j+1)=data(:,:,i+1)-tempi
            data(:,:,i)=data(:,:,i)+tempr
            data(:,:,i+1)=data(:,:,i+1)+tempi
          enddo
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
        enddo
        mmax=istep
        
      goto 2
      endif
      
     
      return
      end
