SUBROUTINE buryadj(burycoef,mnrbc,speed,stdspeed,minspeed,maxspeed,depth,       &
                 & stddepth,mindepth,maxdepth)
!
IMPLICIT NONE
!
! PARAMETER definitions
!
REAL,PARAMETER :: expspeed = 0.5,s1speed = 0.6,s2speed = 0.4,expdepth = 2.7
!
! Subroutine arguments
!
REAL :: depth,maxdepth,maxspeed,mindepth,minspeed,speed,stddepth,stdspeed
INTEGER :: mnrbc
REAL,DIMENSION(mnrbc) :: burycoef
!
! Local variables
!
INTEGER :: index
REAL :: rdepth,rspeed
!
!     argument declarations
 
!     argument definitions
!     burycoef - burial fraction coefficient to be adjusted
!     mnrbc    - number of burial coefficients (residue burial classes)
!     speed    - actual
!     stdspeed - standard, where coefficient remains unchanged
!     minspeed - minimum
!     maxspeed - maximum
!     depth    - actual
!     stddepth - standard, where coefficient remains unchanged
!     mindepth - minimum
!     maxdepth - maximum
 
!     local variable declarations
 
 
!     find speed adjustment parameter
speed = max(min(speed,maxspeed),minspeed)
rspeed = (s1speed+s2speed*(speed/maxspeed)**expspeed)                           &
       & /(s1speed+s2speed*(stdspeed/maxspeed)**expspeed)
 
!     find depth adjustment parameter
depth = max(min(depth,maxdepth),mindepth)
rdepth = (1.0-(1.0-depth/maxdepth)**expdepth)                                   &
       & /(1.0-(1.0-stddepth/maxdepth)**expdepth)
 
!     adjust burial coefficients and keep within range 0 to 1
DO index = 1,mnrbc
  burycoef(index) = burycoef(index)*rspeed*rdepth
  burycoef(index) = min(1.0,max(0.0,burycoef(index)))
END DO
!
END SUBROUTINE buryadj
