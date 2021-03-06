!$Author: fredfox $
!$Date: 2005-03-23 17:48:24 $
!$Revision: 1.8 $
!$Source: /weru/cvs/weps/weps.src/inc/s1surf.inc,v $
!     ------------------------------------------------------------------
!     file 's1surf.inc'
!
!     This common block contains the state variables relating to
!     the surface soil properties and pm10 fractions in suspended soil
!
!     Parameter include files:  p1werm.inc, p1const.inc
!
!     + + + VARIABLE DECLARATIONS + + +

      real             aszcr(mnsub)
      real             asfcr(mnsub)
      real             asmlos(mnsub)
      real             asflos(mnsub)
      real             asdcr(mnsub)
      real             asecr(mnsub)
      real             asfald(mnsub)
      real             asfalw(mnsub)
      real             acanag(mnsub)
      real	       acancr(mnsub)
      real             asf10an(mnsub)
      real             asf10en(mnsub)
      real             asf10bk(mnsub)
      common / s1surf /                                                 &
     & aszcr, asfcr, asmlos, asflos, asdcr, asecr, asfald, asfalw,      &
     & acanag, acancr,asf10an,asf10en, asf10bk
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     aszcr  - Soil crust thickness (mm)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the average soil surface crust
!              thickness value for each of the subregions.
!
!     asfcr  - Fraction of soil surface covered by crust (m^2/m^2)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the fractional amount of soil
!              surface covered by crust for each of the subregions.
!
!     asmlos - Amount of loose erodible material on crusted fraction
!              of soil surface (Mg/m^2)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the quantity of loose erodible
!              material residing on the soil surface crust for each
!              of the subregions.  If there is no crust present,
!              this variable has no valid value.  The units are
!              Mg of loose erodible material per m^2 of crusted
!              surface area (not total surface area).
!
!     asflos - Fraction of crusted soil surface containing loose erodible
!              material surface (m^2/m^2)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the fraction of loose material
!              on the soil's crusted surface for each of the subregions.
!              If there is no crust present, this variable has no valid
!              value.  The units are m^2 of crusted surface area containing
!              loose erodible material per m^2 of crusted surface area
!              (not total surface area).
!
!     asdcr  - Soil crust density (Mg/m^2)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the average soil surface crust
!              density value for each of the subregions.

!     asecr  - Soil crust stability (J/m^2)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the average soil surface crust
!              stability value for each of the subregions.

!     asfald - Dry soil albedo

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the dry soil albedo
!              value for each of the subregions.
!
!     asfalw - Wet soil albedo
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the wet soil albedo
!              value for each of the subregions.
!
!     ascanag - coeffienct of abrasion for aggregates (1/m)
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!     ascancr - coeffienct of abrasion for crust (1/m)
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!     asf10an - soil fraction pm10 in abraded suspension
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!     asf10en - soil fraction pm10 in emitted suspension
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!     asf10bk - soil fraction pm10 in saltation breakage suspension
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!     ------------------------------------------------------------------
