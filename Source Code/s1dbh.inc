!$Author: jt $
!$Date: 2005-02-24 22:35:07 $
!$Revision: 1.8 $
!$Source: /weru/cvs/weps/weps.src/inc/s1dbh.inc,v $
!     ------------------------------------------------------------------
!     file 's1dbh.inc'
!
!     This common block contains the variables representing intrinsic
!     soil properties of particle size (sand, silt, and clay) which are
!     used by HYDROLOGY for each soil layer.
!
!     Parameter include files:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      real             asfsan(0:mnsz, mnsub) !not used
      real             asfsil(0:mnsz, mnsub) !not used
      real             asfcla(0:mnsz, mnsub) !used
      real             asvroc(0:mnsz, mnsub) !not used
      real             asfvcs(mnsz, mnsub) !not used
      real             asfcs(mnsz, mnsub) !not used
      real             asfms(mnsz, mnsub) !not used
      real             asffs(mnsz, mnsub) !not used
      real             asfvfs(mnsz, mnsub) !not used
      real             asfwdc(mnsz, mnsub)       ! Not used - not input in Version 1.0 IFC file

!
      common / s1dbh /                                                  &
     &                 asfsan, asfsil, asfcla, asvroc,                  &
     &                 asfvcs, asfcs, asfms, asffs, asfvfs, asfwdc
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     asfsan  - Soil layer sand content (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0         1.0
!              typical        dependent on silt and clay
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer sand content
!              for each of the subregions.
!
!     asfsil  - Soil layer silt content (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed           0.0         1.0
!              typical        dependent on clay and sand
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer silt content
!              for each of the subregions.
!
!     asfcla  - Soil layer clay content (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical        dependent on sand and silt
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer clay content
!              for each of the subregions.
!
!     asvroc  - Soil layer coarse fragments (m^3/m^3)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0          1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer coarse
!              fragment (>2mm) content for each of the subregions.
!
!     asfvcs - Soil layer content of very coarse sand (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer coarse sand
!              (1.0 - x.0 mm) content for each of the subregions.
!
!
!     asfcs  - Soil layer content of coarse sand (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer coarse sand
!              (0.5 - 1.0 mm) content for each of the subregions.
!
!     asfms  - Soil layer content of medium sand (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer medium sand
!              (0.25 - 0.5 mm) content for each of the subregions.
!
!     asffs  - Soil layer content of fine sand (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer fine sand
!              (0.1 - 0.25 mm) content for each of the subregions.
!
!     asfvfs - Soil layer content of very fine sand sand (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer very fine sand
!              (0.05 - 0.1 mm) content for each of the subregions.
!
!     asfwdc - Soil layer content of water dispersible clay (Mg/Mg)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.0        1.0
!              typical
!              ------------   ---------  ---------
!
!            - This variable contains the soil layer water dispersible
!              clay content for each of the subregions.
!     ------------------------------------------------------------------
