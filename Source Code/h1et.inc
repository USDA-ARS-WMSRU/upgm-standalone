!$Author: fredfox $
!$Date: 2005-02-01 22:58:50 $
!$Revision: 1.4 $
!$Source: /weru/cvs/weps/weps.src/inc/h1et.inc,v $
!     -----------------------------------------------------------------
!     file 'h1et.inc'

!     This common block contains the variables representing the
!     evapotransporation parameters in WERM.

!     Parameter include files:

!     + + + VARIABLE DECLARATIONS + + +

      real             ahzea
      real             ahzep
      real             ahzeta
      real             ahzetp
      real             ahzpta
      real             ahzptp
      real             ah0drat

      common /h1et/                                                     &
     &               ahzea, ahzep, ahzeta, ahzetp, ahzpta, ahzptp,      &
     &               ah0drat

!     + + + VARIABLE DEFINITIONS + + +

!     ahzea  - Actual bare soil evaporation (mm/day)
!     ahzep  - Potential bare soil evaporation (mm/day)
!     ahzeta - Actual evapotranspiration (mm/day)
!     ahzetp - potential evapotranspiration (mm/day)
!     ahzpta - Actual plant transpiration (mm/day)
!     ahzptp - potential plant transpiration (mm/day)
!     ah0drat - dryness ratio
