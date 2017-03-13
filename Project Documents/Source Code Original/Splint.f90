SUBROUTINE splint(xa,ya,y2a,n,x,y)
!
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER :: n
REAL :: x,y
REAL,DIMENSION(n) :: xa,y2a,ya
!
! Local variables
!
REAL :: a,b,h
INTEGER :: k,khi,klo
!
! + + + argument definitions + + +
!     n - 
!     x - 
!     xa - 
!     y - 
!     y2a - 
!     ya - 
 
! + + + local variables
!     a - 
!     b - 
!     h - 
!     k - 
!     khi - 
!     klo - 
 
klo = 1
khi = n
DO
!.... begin debug lines
!        do i=1,13
!           write (56,*)'n=',n,' xa(i)=', xa(i),' ya(i)=',ya(i),
!     &     ' y2a(i)=',y2a(i)
!        end do
! ......end debug lines
  IF (khi-klo.GT.1) THEN
     k = (khi+klo)/2
     IF (xa(k).GT.x) THEN
        khi = k
     ELSE
        klo = k
     END IF
     GO TO 10
  END IF
  h = xa(khi) - xa(klo)
  IF (h.EQ.0.) WRITE (*,*) 'crop/splint.for: bad xa input.'
  a = (xa(khi)-x)/h
  b = (x-xa(klo))/h
  y = a*ya(klo) + b*ya(khi) + ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
  GO TO 99999
 10   END DO
!.... debug line
!       write(56,*)x,y
!
99999 END SUBROUTINE splint
