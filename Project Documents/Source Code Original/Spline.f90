SUBROUTINE spline(x,y,n,yp1,ypn,y2)
!
IMPLICIT NONE
!
! PARAMETER definitions
!
INTEGER,PARAMETER :: nmax = 100
!
! Subroutine arguments
!
INTEGER :: n
REAL :: yp1,ypn
REAL,DIMENSION(n) :: x,y,y2
!
! Local variables
!
INTEGER :: i,k
REAL :: p,qn,sig,un
REAL,DIMENSION(nmax) :: u
!
! + + + argument definitions + + +
!     n - 
!     x - 
!     y - 
!     y2 - 
!     yp1 - 
!     ypn - 

!     + + + parameter definitions + + +
!     nmax - 

! + + + local variables

!     + + + local variable definitions + + +
!     i - loop control variable
!     k - loop control variable
!     p - 
!     qn - 
!     sig -
!     u -  
!     un - 
 
IF (yp1.GT..99E30) THEN
  y2(1) = 0.
  u(1) = 0.
ELSE
  y2(1) = -0.5
  u(1) = (3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
END IF
DO i = 2,n - 1
  sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
  p = sig*y2(i-1) + 2.
  y2(i) = (sig-1.)/p
  u(i) = (6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))          &
       & /(x(i+1)-x(i-1))-sig*u(i-1))/p
END DO
IF (ypn.GT..99E30) THEN
  qn = 0.
  un = 0.
ELSE
  qn = 0.5
  un = (3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
END IF
y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.)
DO k = n - 1,1, - 1
  y2(k) = y2(k)*y2(k+1) + u(k)
END DO
!       debug lines. remove after debugging.
!        do i=1,n
!                write(56,*)x(i),y(i),y2(i)
!        end do
!
END SUBROUTINE spline
