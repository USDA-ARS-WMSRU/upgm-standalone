!$Author: wagner $
!$Date: 2002-02-22 21:47:23 $
!$Revision: 1.6 $
!$Source: /weru/cvs/weps/weps.src/inc/s1sgeo.inc,v $
!     This common block contains the variables representing the soil
!     surface properties for each subregion.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             aszrgh(mnsub) !used
      real             aszrho(mnsub) !not used
      real             asxrgw(mnsub) !not used
      real             asxrgs(mnsub) !used
      real             asargo(mnsub) !used
      real             asxdks(mnsub) !not used
      real             asxdkh(mnsub) !not used
      real             as0rrk(mnsub) !not used
      real             aslrrc(mnsub) !not used
      real             aslrr(mnsub)  !not used
      real             aslrro(mnsub) !not used

      common / s1sgeo /                                                 &
     &                 aszrgh, aszrho, asxrgw, asxrgs, asargo, asxdks,  &
     &                 asxdkh, as0rrk, aslrrc, aslrr, aslrro

!     + + + VARIABLE DEFINITIONS + + +

!     aszrgh - Ridge height (mm)
!     aszrho - original ridge height, after tillage, mm.

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the ridge height for each of
!              the subregions.

!     asxrgw - Ridge width (mm)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the ridge width for each of
!              the subregions.

!     asxrgs - Ridge spacing (mm)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the ridge spacing for each of
!              the subregions.

!     asargo - Ridge orientation (degrees)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the orientation width for each
!              of the subregions.

!     asxdks - Dike spacing (mm)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the dike spacing for each of
!              the subregions.

!     asxdkh - Dike Height (mm)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the dike height for each of
!              the subregions.

!     as0rrk - Random roughness parameter K (shape)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the random roughness shape
!              parameter (K) for each of  the subregions.

!     aslrrc - Random roughness parameter C (scale)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the random roughness scale
!              parameter (C) for each of  the subregions.

!     aslrr - Allmaras random roughness parameter (mm) 
!     aslrro - original random roughness height, after tillage, mm

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the Allmaras random roughness 
!              parameter for each of the subregions.

!     ------------------------------------------------------------------
