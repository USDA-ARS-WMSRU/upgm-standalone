FUNCTION rerf(y)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: y
REAL :: rerf
!
! Local variables
!
REAL :: a,a1,a2,alogy,b,b1,b2,base,c,c1,c2,d,d1,e,er1,er2,f,g,h,i,j,sqy,y2,y3,  &
      & y4,y5,z
REAL :: alog
!
!**********************************************************************
!     + + +  purpose + + +
!     for the equation y = erf(x), the function returns the value of x
!     given the value of y, i.e., the reverse of the error function.
!     written by l. hagen and coded by i. elmanyawi
 
!     + + +  argument declarations + + +
 
!     + + + local variables + + +
!     + + + end specifications + + +
!
a = 0.000009477
b = -2.76913995
c = 0.88589485
d = 2.30199509
e = -2.44996884
f = -0.14332263
g = 2.246990417
h = -0.54046067
i = -0.68292239
j = 0.15092933
z = y
y = abs(y)
!
a1 = 2857.5463
b1 = -0.00016423
c1 = -5717.0696
d1 = -2857.5783
a2 = 69161.649
b2 = -277330.28
c2 = 208168.95
!
sqy = sqrt(y)
alogy = alog(y)
y2 = y*y
y3 = y2*y
y4 = y3*y
y5 = y4*y
!
base = (a+c*y+e*y2+g*y3+i*y4)/(1.000000+b*y+d*y2+f*y3+h*y4+j*y5)
!
IF (y.LE.0.966105) THEN
  rerf = base
ELSE IF (y.GT.0.999311) THEN
!
  er2 = a2 + b2*y*sqy + c2*y2
  rerf = base - er2
ELSE
!
  er1 = a1 + b1/alogy + c1*alogy/y + d1/y2
  rerf = base - er1
END IF
rerf = rerf*z/y
END FUNCTION rerf
