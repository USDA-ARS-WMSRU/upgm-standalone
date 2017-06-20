!$Author: fredfox $
!$Date: 2005-03-25 15:23:40 $
!$Revision: 1.12 $
!$Source: /weru/cvs/weps/weps.src/inc/c1gen.inc,v $

!     This common block contains the state variables relating to
!     the general purpose crop information.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer          ac0rg(mnsub) !used
      real             acdpop(mnsub) !used
      real             acytgt(mnsub) !used
      integer          achyfg(mnsub) !used
      character*80     acynmu(mnsub) !used
      real             acycon(mnsub) !used
      real             acywct(mnsub) !used
      integer          acrsfg(mnsub) !used
      real             acxrow(mnsub) !used
      integer          acthudf(mnsub) !used
      common / c1gen /                                                  &
     &                 ac0rg,                                           &
     &                 acdpop,                                          &
     &                 acytgt,                                          &
     &                 achyfg,                                          &
     &                 acynmu,                                          &
     &                 acycon, acywct,                                  &
     &                 acrsfg,                                          &
     &                 acxrow,                                          &
     &                 acthudf

!     + + + VARIABLE DEFINITIONS + + +
!     ac0rg  - Crop seeding location in relation to ridge
!         0     o plant in furrow
!         1     o plant on ridge
!     acdpop - Crop seeding density (#/m^2)
!     acytgt - target yield (in units shown below)
!     achyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)
!     acynmu - string for name of units in which yield of interest will be
!              reported
!     acycon - conversion factor from Kg/m^2 to units named in acynmu (all
!              dry weight)
!     acywct - water content at which yield is to be reported (percent)
!     acrsfg - row spacing flag
!         0      o Broadcast Planting
!         1      o Use Specified Row Spacing
!         2      o Use Existing Ridge Spacing
!     acxrow - Crop row spacing (m)
!     acthudf - heat units or days to maturity flag
!         0      o Days to maturity and average conditions used to find heat units
!         1      o Heat units specified used directly
!     aplant_day - planting date (day of month)
!     aplant_month - planting date (month of rotation year)
!     aplant_rotyr - planting date (rotation year)
