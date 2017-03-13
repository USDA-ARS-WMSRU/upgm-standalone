FUNCTION setbds(clay,sand,om)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: clay,om,sand
REAL :: setbds
!
! Local variables
!
REAL :: fi,fj,mbd,mbd_hi_hj,mi,mj,tempsum
INTEGER :: hi,hj,li,lj
INTEGER :: int
REAL,DIMENSION(0:10,0:10) :: mbdtv
!
!     + + + purpose + + +
!     the following function estimates settled soil bulk density from
!     intrinsic properties. see rawls (1983) soil science 135, 123-125.
 
!     should eventually be called by main to initialize the values
!     for each subregion (unless soil composition changes).
 
!     + + + keywords + + +
!     bulk density, initialization
 
!     + + + parameters and common blocks + + +
!     none
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
 
!     setbd - settled bulk density
!     clay  - fraction of soil clay content
!     sand  - fraction of soil sand content
!     om    - organic matter
 
!     + + + local variables + + +
!
 
!     li  - index into table less than sand content
!     lj  - index into table less than clay content
!     hi  - index into table higher than sand content
!     hj  - index into table higher than clay content
!     mi  - value between indexes for interpolation for sand
!     mj  - value between indexes for interpolation for clay
!     fi  - fraction of distance between grid cells for sand
!     fj  - fraction of distance between grid cells for clay
!     mbdtv (0:10,0:10) - data table of settled bulk density
!                         as a function of sand (across the top)
!                         and clay (down the side)
!     mbd   - mineral bulk density without organic matter
!     mbd_hi_hj - value for mbdtv(hi,hj), if outside triangular
!                 part of table it is reflected from mbdtv(li,lj)
!                 otherwise it is just the real point
 
!     + + + subroutines called + + +
 
!     + + + function declaratons + + +
 
!     + + + data initializations + + +
!     first index in this direction ->
!     second index || goes down
!                  \/
DATA mbdtv/1.48,1.25,1.00,1.06,1.16,1.22,1.30,1.39,1.45,1.51,1.52,1.52,1.40,    &
   & 1.19,1.25,1.32,1.40,1.52,1.58,1.63,1.65,0.,1.52,1.40,1.25,1.35,1.45,1.53,  &
   & 1.60,1.64,1.72,0.,0.,1.52,1.40,1.29,1.41,1.50,1.57,1.63,1.68,0.,0.,0.,1.50,&
   & 1.40,1.35,1.43,1.53,1.61,1.64,0.,0.,0.,0.,1.46,1.40,1.40,1.43,1.53,1.62,0.,&
   & 0.,0.,0.,0.,1.45,1.40,1.38,1.42,1.50,0.,0.,0.,0.,0.,0.,1.42,1.37,1.33,1.33,&
   & 0.,0.,0.,0.,0.,0.,0.,1.33,1.32,1.20,0.,0.,0.,0.,0.,0.,0.,0.,1.23,1.18,0.,  &
   & 0.,0.,0.,0.,0.,0.,0.,0.,1.15,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
 
!     + + + end specifications + + +
 
tempsum = sand + clay
IF (tempsum.GT.1.0) THEN
  sand = sand/tempsum
  clay = clay/tempsum
  WRITE (*,*) 'setbds: sand plus clay fractions greater than 1.0'
  WRITE (*,*) 'values adjusted by averaging the difference'
END IF
 
!      i = nint(sand*100.0/10.0)
!      j = nint(clay*100.0/10.0)
 
!      mbd = mbdtv(i,j)
 
mi = sand*10.0
li = int(mi)
fi = mi - li
hi = min(li+1,10)
 
mj = clay*10.0
lj = int(mj)
fj = mj - lj
hj = min(lj+1,10)
 
!     check for table edge
IF (li+lj.EQ.10) THEN
!         on table edge, no interpolation necessary
  mbd = mbdtv(li,lj)
ELSE
  IF (hi+hj.GT.10) THEN
!             interpolation on the triangular edge of the table
!             mirror li,lj value to make using grid interpolation possible
     mbd_hi_hj = mbdtv(li,hj) + mbdtv(hi,lj) - mbdtv(li,lj)
  ELSE
!             interpolation within the table, use grid point
     mbd_hi_hj = mbdtv(hi,hj)
  END IF
  mbd = (1-fi)*(1-fj)*mbdtv(li,lj) + (1-fi)*fj*mbdtv(li,hj) + fi*(1-fj)         &
      & *mbdtv(hi,lj) + fi*fj*mbd_hi_hj
END IF
 
setbds = 1.0/((om/0.224)+(1.0-om)/mbd)
! 
END FUNCTION setbds
