!$Author: fredfox $
!$Date: 2007-01-17 15:56:12 $
!$Revision: 1.4 $
!$Source: /weru/cvs/weps/weps.src/inc/crop/prevstate.inc,v $

!     This common block contains variables representing the crop
!     properties for each subregion from the last time the crop growth
!     routines were called. When the crop growth model is called
!     again these values represent the previous day. These are compared
!     to the state variables to determine any operations done on the
!     crop and the resetting of the crop state required to properly
!     simulate the crop response to the operation.

!     Parameter include files required:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +
      real             prevstandstem(mnsub) !used
      real             prevstandleaf(mnsub)  !used
      real             prevstandstore(mnsub)  !used

      real             prevflatstem(mnsub)  !used
      real             prevflatleaf(mnsub)  !used
      real             prevflatstore(mnsub)  !used

      real             prevmshoot(mnsub)  !used
      real             prevmtotshoot(mnsub)  !used
      real             prevbgstemz(mnsz,mnsub)  !used

      real             prevrootstorez(mnsz,mnsub)  !used
      real             prevrootfiberz(mnsz,mnsub)  !used

      real             prevht(mnsub)  !used
      real             prevzshoot(mnsub)  !used
      real             prevstm(mnsub)  !used
      real             prevrtd(mnsub)  !used

      integer          prevdayap(mnsub)  !used
      real             prevhucum(mnsub)  !used
      real             prevrthucum(mnsub)  !used

      real             prevgrainf(mnsub)  !used
      real             prevchillucum(mnsub)  !used
      real             prevliveleaf(mnsub)  !used

      common / prevstate /                                              &
     &           prevstandstem, prevstandleaf, prevstandstore,          &
     &           prevflatstem, prevflatleaf, prevflatstore,             &
     &           prevmshoot, prevmtotshoot, prevbgstemz,                &
     &           prevrootstorez, prevrootfiberz,                        &
     &           prevht, prevzshoot, prevstm, prevrtd,                  &
     &           prevdayap, prevhucum, prevrthucum,                     &
     &           prevgrainf, prevchillucum, prevliveleaf

!     + + + VARIABLE DEFINITIONS + + +

!     prevstandstem - crop standing stem mass (kg/m^2)
!     prevstandleaf - crop standing leaf mass (kg/m^2)
!     prevstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))

!     prevflatstem  - crop flat stem mass (kg/m^2)
!     prevflatleaf  - crop flat leaf mass (kg/m^2)
!     prevflatstore - crop flat storage mass (kg/m^2)

!     prevmshoot - mass of shoot growing from root storage biomass (kg/m^2)
!     prevmtotshoot - total mass of shoot growing from root storage biomass (kg/m^2)
!                     in the period from beginning to completion of emegence heat units
!     prevbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!     prevrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     prevrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     prevht  - Crop height (m)
!     prevzshoot - length of actively growing shoot from root biomass (m)
!     prevstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     prevrtd  - Crop root depth (m)

!     prevdayap - number of days of growth completed since crop planted
!     prevhucum - crop accumulated heat units
!     prevrthucum - crop accumulated heat units with no vernalization/photoperiod delay
!     prevgrainf - internally computed grain fraction of reproductive mass
!     prevchillucum - accumulated chilling units (days)
!     prevliveleaf - fraction of standing plant leaf which is living (transpiring)
