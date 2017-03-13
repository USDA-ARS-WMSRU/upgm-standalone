subroutine splint(xa,ya,y2a,n,x,y)
!
implicit none
!
! Dummy arguments
!
integer :: n
real :: x,y
real,dimension(n) :: xa,y2a,ya
!
! Local variables
!
real :: a,b,h
integer :: k,khi,klo
!
klo = 1
khi = n
do
!.... begin debug lines
!        do i=1,13
!           write (56,*)'n=',n,' xa(i)=', xa(i),' ya(i)=',ya(i),
!     &     ' y2a(i)=',y2a(i)
!        end do
! ......end debug lines
  if (khi-klo>1) then
     k = (khi+klo)/2
     if (xa(k)>x) then
        khi = k
     else
        klo = k
     end if
     go to 10
  end if
  h = xa(khi) - xa(klo)
  if (h==0.) write (*,*) 'crop/splint.for: bad xa input.'
  a = (xa(khi)-x)/h
  b = (x-xa(klo))/h
  y = a*ya(klo) + b*ya(khi) + ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
  go to 99999
 10   end do
!.... debug line
!       write(56,*)x,y
!
99999 end subroutine splint
