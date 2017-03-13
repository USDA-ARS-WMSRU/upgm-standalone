SUBROUTINE crop_endseason(bc0nam,bm0cfl,bnslay,bc0idc,bcdayam,bcthum,bcxstmrep,&
                           & bprevstandstem,bprevstandleaf,bprevstandstore,    &
                           & bprevflatstem,bprevflatleaf,bprevflatstore,       &
                           & bprevbgstemz,bprevrootstorez,bprevrootfiberz,     &
                           & bprevht,bprevstm,bprevrtd,bprevdayap,bprevhucum,  &
                           & bprevrthucum,bprevgrainf,bprevchillucum,          &
                           & bprevliveleaf,bcdayspring,mature_warn_flg,acycon, &   
                           & acynmu,ies,joints,boots,heads,antss,mats,hrs,     &
                           & phenolflg)    
!                        
!debe passed acycon from main to print out the calculated yield. acycon is needed
!for the calculation. acycon is the conversion factor.
!debe passed in antss, hrs, ies, joints, and mats for printing out these growth 
! stage arrays for anthesis, harvest ready, internode elongation, jointing and 
! maturity. Other growth stage arrays will need to be added for other crops. 
!
IMPLICIT NONE
!
INCLUDE 'file.fi'
!
! Subroutine arguments
!
REAL :: acycon,bcthum,bcxstmrep,bprevchillucum,bprevflatleaf,bprevflatstem,     &
      & bprevflatstore,bprevgrainf,bprevht,bprevhucum,bprevliveleaf,bprevrtd,   &
      & bprevrthucum,bprevstandleaf,bprevstandstem,bprevstandstore,bprevstm
CHARACTER(6) :: acynmu
INTEGER :: bc0idc,bcdayam,bcdayspring,bm0cfl,bnslay,bprevdayap,mature_warn_flg, &
         & phenolflg
CHARACTER(80) :: bc0nam
REAL,DIMENSION(*) :: bprevbgstemz,bprevrootfiberz,bprevrootstorez
INTEGER,DIMENSION(4) :: antss,boots,heads,hrs,ies,joints,mats
!
! Local variables
!
REAL :: bg_stem_sum,hi,hui,root_fiber_sum,root_store_sum,totbiomass,yield
INTEGER :: dd,lay,mm,yy
!
!debe added declaration of acynum to print out units for current crop yield.
!
! local variables
!
!debe added declaration of local variable yield. also totbiomass for
! for calculating the final total biomass and hi for calculating the
! harvest index.
!
!     the following subroutine arguments are not used: bprevliveleaf,
!     bprevrthucum     jcaii  8/08/08
!
!     + + + purpose + + +
!     prints out crop status variables that are of interest at the end of the season
 
!     + + + keywords + + +
!     crop model status
 
!     + + + argument declarations + + +
!     acycon - conversion factor from kg/m^2 to units named in acynmu (all
!              dry weight)
!     bc0nam - crop name
!     bm0cfl - flag to print crop submodel output
!     bnslay - number of soil layers
!     bc0idc - crop type:annual,perennial,etc
!     bcdayam - number of days since crop matured
!     bcthum - potential heat units for crop maturity (deg. c)
!     bcxstmrep - a representative diameter so that
!                 acdstm*acxstmrep*aczht=acrsai
!     bprevbgstemz - crop stem mass below soil surface by layer
!     bprevchillucum - accumulated chilling units
!     bprevdayap - number of days of growth completed since crop planted
!     bprevflatleaf - crop flat leaf mass
!     bprevflatstem - crop flat stem mass
!     bprevflatstore - crop flat storage mass
!     bprevgrainf - internally computed grain fraction of reproductive
!                   mass
!     bprevht - crop height
!     bprevhucum - crop accumulated heat units
!     bprevliveleaf - fraction of standing plant leaf which is living
!                     (transpiring). not currently used.
!     bprevrootfiberz - crop root fibrous mass by soil layer
!     bprevrootstorez - crop root storage mass by soil layer
!                       (tubers (potatoes, carrots), extended leaf
!                       (onion), seeds (peanuts)).
!     bprevrtd - crop root depth
!     bprevrthucum - crop accumulated heat units with no
!                    vernalization/photoperiod delay. not currently used.
!     bprevstandleaf - crop standing leaf mass
!     bprevstandstem - crop standing stem mass
!     bprevstandstore - crop standing storage mass (head with seed, or
!                       vegetative head (cabbage, pineapple))
!     bprevstm - number of crop stems per unit area. it is computed by
!                taking the tillering factor times the plant population
!                density.
!     bcdayspring - day of year in which a winter annual releases stored
!                   growth
!     mature_warn_flg - flag to indicate use of crop maturity warning
!                0  - no crop maturity warning given for any crop
!                1  - warnings generated for any crop unless supressed
!                     by crop type
 
!     + + + global common blocks + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     bg_stem_sum - sum of below ground stem
!     dd - the current day
!     hi - harvest index. yield/totbiomass
!     hui - heat unit index (ratio of acthucum to acthum)
!     lay - loop counter
!     mm -  the current month
!     root_fiber_sum - sum of root fiber
!     root_store_sum - sum of root storage
!     yield - the calculated yield of the crop.
!             yield = bprevstandstore*acycon*bprevgrainf
!     totbiomass - the final total biomass.
!       totbiomass = bprevstandleaf+bprevstandstem+bprevstandstore+
!                    bprevflatleaf+bprevflatstem+bprevflatstore
!     yy - the current year
!
!     + + + newly added variables + + +
!     antss - start of anthesis growth stage for corn, dry beans, hay millet,
!             proso millet, sorghum (first bloom), spring barley, spring
!             wheat, sunflower, winter barley and winter wheat. in dry beans,
!             the start of anthesis growth stage and there is one open
!             flower per plant =100% bloom. this array includes daynum,
!             year, month and day of when this stage was reached.
!     hrs - time to harvest ripe growth stage for corn, dry beans, hay
!           millet, proso millet, sorghum, spring barley, spring wheat,
!           sunflower, winter barley and winter wheat. in dry beans, 80%
!           of pods are at the mature color in dry beans. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     ies - start of internode elongation growth stage for corn, hay millet,
!           proso millet, sorghum, spring barley, spring wheat, winter barley,
!           and winter wheat. for sunflower, this stage occurs when the
!           internode below the inflorescence elongates 0.5 to 2.0 cm above
!           the nearest leaf on the stem. this array includes daynum, year,
!           month and day of when this stage was reached.
!     joints - jointing growth stage for hay millet, proso millet, sorghum,
!              spring barley, spring wheat, winter barley and winter wheat.
!              this array includes daynum, year, month and day of when this
!              stage was reached.
!     mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, spring barley, spring
!            wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. this array includes
!            daynum, year, month and day of when this stage was reached.
!     phenolflg - a flag that determines if the UPGM/WEPS method of determining maturity 
!                 will be used (phenolflg =0) or the PhenologyMMS method will be used (phenolflg = 1).
!     + + + end of specifications + + +
 
!     day of year

  !phenolflg1 = 0
  !mats1 = 170
  !
CALL caldatw(dd,mm,yy)
 
      ! end of season print statements when crop submodel output flag set
      ! added initialization flag to prevent printing if crop not yet initialized
 !print*, 'in Crop_endseason and bprevhucum = ', bprevhucum, 'bcthum = ', bcthum

 IF ((bcthum.GT.0.0) .AND. (phenolflg .EQ. 0)) THEN
   hui = bprevhucum/bcthum
 ! print*, 'in Crop_endseason hui = ', hui   
 ELSEIF ((bcthum.GT.0.0) .AND. (phenolflg .EQ. 1) .AND. (mats(1) .NE. 999)) THEN   
  hui = 1.0
 ! print*, 'in Crop_endseason hui = ', hui
ELSE
  hui = 0.0
END IF
 
      ! print end-of-season (before harvest) crop state
IF ((bm0cfl.GE.0)) THEN          ! always print this one now - lew
  bg_stem_sum = 0.0
  root_store_sum = 0.0
  root_fiber_sum = 0.0
  DO lay = 1,bnslay
     bg_stem_sum = bg_stem_sum + bprevbgstemz(lay)
     root_store_sum = root_store_sum + bprevrootstorez(lay)
     root_fiber_sum = root_fiber_sum + bprevrootfiberz(lay)
  END DO
!debe added yield to hold the calculated yield value for the current crop
! and added it to the following write statement and format statment.
!this puts yield in the units for which yield is reported for the current
!  crop.
  yield = bprevstandstore*acycon*bprevgrainf
 
! debe will need to use the yield units variable (acynmu) to print the units
! next to the yield value instead of above it in cpout because the units
! vary among crops and so there is not a constant value to print in the
! output subroutine cpout as text. couldn't get the variable 'acynmu' to be
! used in cpout to print the current yield units for the current crop.
 
!debe added calculation for final total biomass. add it to write and
!   format statements. this is in kg/m^2
  totbiomass = bprevstandleaf + bprevstandstem + bprevstandstore +              &
             & bprevflatleaf + bprevflatstem + bprevflatstore
 
!debe added calculation for hi (harvest index). add it to write
! and format statements. yield and totbiomass are likely in
! different units. multiply totbiomass by the conversion factor so that
! yield and totbiomass are in the same units.
 
  hi = yield/(totbiomass*acycon)
 
  WRITE (luoseason,1000) yy,bprevstandstem,bprevstandleaf,bprevstandstore,      &
                       & bprevflatstem,bprevflatleaf,bprevflatstore,bg_stem_sum,&
                       & root_store_sum,root_fiber_sum,bprevht,bprevstm,        &
                       & bprevrtd,bprevgrainf,bcxstmrep,bprevdayap,             &
                       & bprevchillucum,bprevhucum,bcthum,hui,bcdayam,          &
                       & bcdayspring,bc0nam,yield,acynmu,totbiomass,hi,         &
                       & ies(1),joints(1),boots(1),heads(1),antss(1),mats(1),   &
                       & hrs(1)
END IF
 
      ! for annual crops, always write out warning message
      ! if harvested before maturity
IF ((hui.LT.1.0).AND.(mature_warn_flg.GT.0).AND.                                &
  & ((bc0idc.EQ.1).OR.(bc0idc.EQ.2).OR.(bc0idc.EQ.4).OR.(bc0idc.EQ.5)))         &
  & WRITE (UNIT=6,FMT='(1x,3(a),i2,''/'',i2,''/'',i2,a,f5.1,a,a)') 'warning: ', &
  & bc0nam(1:len_trim(bc0nam)),' harvested ',dd,mm,yy,' only reached ',         &
  & hui*100.0,'% of maturity',' (check crop selection, planting, harvest dates)'
 
!     + + + output formats + + +
 1000 FORMAT (1x,i4,13(1x,f7.3),1x,f7.5,1x,i4,3(1x,f6.1),1x,f5.3,1x,i4,1x,i6,1x,&
            & a12,1x,f7.2,7x,a3,'/ac',2x,f7.3,9x,f5.3,11x,i3,13x,i3,5x,i3,5x, &
            & i3,6x,i3,6x,i3,7x,i3)
!debe added formatting to output yield, units, totbiomass, hi
!
END SUBROUTINE crop_endseason
