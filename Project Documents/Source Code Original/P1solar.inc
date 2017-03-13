!$Author: wagner $
!$Date: 2002-08-30 21:19:33 $
!$Revision: 1.2 $
!$Source: /weru/cvs/weps/weps.src/inc/p1solar.inc,v $

!     parameters used to define sunrise and sunset (daylength) for different purposes

      real             beamrise
      real             civilrise
      real             nautrise
      real             astrorise

      parameter( beamrise = 90.833 )
      parameter( civilrise = 96.0 )
      parameter( nautrise = 102.0 )
      parameter( astrorise = 108.0 )

!     beamrise   - solar altitude angle at which the upper edge of the sun is visible
!     civilrise  - solar altitude angle defined as civil twilight
!     nautrise   - solar altitude angle defined as nautical twilight
!     astrorise   - solar altitude angle defined as astronomical twilight
