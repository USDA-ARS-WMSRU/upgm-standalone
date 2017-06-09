!$Author: wagner $
!$Date: 2002-09-27 20:39:12 $
!$Revision: 1.4 $
!$Source: /weru/cvs/weps/weps.src/inc/s1agg.inc,v $
!     This common block contains the variables representing the soil
!     aggregate properties for each soil layer.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             asdagd(0:mnsz, mnsub) !not used
      real             aseags(0:mnsz, mnsub) !not used
      real             aslagm(0:mnsz, mnsub) !not used
      real             aslagn(0:mnsz, mnsub) !not used
      real             aslagx(0:mnsz, mnsub) !not used
      real             as0ags(0:mnsz, mnsub) !not used
      real             aseagm(mnsz, mnsub)   !not used
      real             aseagmn(mnsz, mnsub) !not used
      real             aseagmx(mnsz, mnsub) !not used
      real             ask4d(mnsz, mnsub) !not used
      real             aslmin(mnsz, mnsub) !not used
      real             aslmax(mnsz, mnsub) !not used

      common / s1agg /                                                  &
     & asdagd, aseags, aslagm, aslagn, aslagx, as0ags,                  &
     & aseagm, aseagmn, aseagmx, ask4d, aslmin, aslmax

!     + + + VARIABLE DEFINITIONS + + +
!     for each subregion, the following variables are defined as:

!     asdagd - Soil layer aggregate density (Mg/m^3)
!     aseags - Soil layer dry aggregate stability (J/m^2)
!     aslagm - Soil layer aggregate size geometric mean dia. (mm)
!     aslagn - Minimum aggregate size (mm)
!     aslagx - Maximum aggregate size (mm)
!     as0ags - Soil layer aggregate size geometric standard dev. (mm)
!     aseagm - soil layer mean aggregate stabillity (J/m^2)
!     aseagmn - soil layer minimum aggregate stability
!     aseagmx - soil layer maximum aggregate stability
!     k4d    - soil layer drying stability coefficient
!     aslmin - min values of geom. mean agg. diameter (eq. S-45, S-46)
!     aslmax - max values of geom. mean agg. diameter (eq. S-45, S-46)
