SUBROUTINE trapzd(a,b,s,n)
!
!  this routine is an algorithm which performs integration on a
!  function.  the algorithm is based on the extended trapzoidal rule.
!  the routine integrates the function between a and b.  n represents
!  the n'th stage of refinment of the trapzoid rule.  n=1 gives the
!  crudest estimate of the integrated function, subsequent call with
!  n=2,3,... improve the accuracy of the calculation.  s is the value
!  of the integral and should not be modified between sequential calls.
!  for more info refer to "numerical recipies-the art of scientific computing"
!  cambridge university press, 1986.
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: a,b,s
INTEGER :: n
!
! Local variables
!
REAL :: del,sum,x
REAL :: func
INTEGER :: it,j,tmn
!
IF (n.EQ.1) THEN
  s = 0.5*(b-a)*(func(a)+func(b))
  it = 1
ELSE
  tmn = it
  del = (b-a)/tmn
  x = a + 0.5*del
  sum = 0.0
  DO j = 1,it
     sum = sum + func(x)
     x = x + del
  END DO
  s = 0.5*(s+(b-a)*sum/tmn)
  it = 2*it
END IF
!
END SUBROUTINE trapzd
