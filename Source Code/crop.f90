    subroutine crop(ctrl,clidat,soils,bio,prevbio,biotot,cropres,daysim)
    use upgm_simdata, only : controls
    use support, only : daylen
    use soil, only : soildata
    use constants, only : mgtokg, civilrise, mnsz, sp, int32
    use climate, only : climate_data
    use datetime, only : dayear, caldat
    use biomaterial
    use crop_data_struct_defs, only: crop_residue
    implicit none
    !
    ! PARAMETER definitions
    !
    real(sp), parameter :: chilluv = 50.0
    real(sp), parameter :: shoot_delay = 7.0
    real(sp), parameter :: verndelmax = 0.04
    real(sp), parameter :: dev_floor = 0.01
    real(sp), parameter :: spring_trig = 0.29
    !
    ! Dummy arguments
    !

    type(controls),     intent(inout) :: ctrl
    type(climate_data), intent(inout) :: clidat
    type(biomatter),    intent(inout) :: bio, prevbio
    type(biototal),     intent(inout) :: biotot
    type(crop_residue), intent(inout) :: cropres
    type(soildata),     intent(inout) :: soils
    integer(int32),     intent(in) :: daysim
    !
    ! Local saved variables
    !
    real(sp), save :: ln
    real(sp), save :: co2eff
    real(sp), save :: canhty
    real(sp), save :: elong
    real(sp), save :: gdda
    real(sp), save :: gddday
    real(sp), save :: gdde
    real(sp), save :: gdds
    real(sp), save :: gddv
    real(sp), save :: hu_delay
    real(sp), save :: photo_delay
    real(sp), save :: pot_leaf_mass
    real(sp), save :: pot_stems
    real(sp), save :: root_store_rel
    real(sp), save :: todayln
    real(sp), save :: trend
    real(sp), save :: vern_delay
    real(sp), save :: yestln
    real(sp), dimension(100,2), save :: lnpout
    real(sp), dimension(20), save :: dgdde
    real(sp), dimension(20), save :: dgdds
    real(sp), dimension(20), save :: dgddv
    real(sp), dimension(15,5), save :: gddwsf
    integer(int32), save  :: daa
    integer(int32), save  :: dae
    integer(int32), save  :: dap
    integer(int32), save  :: dav
    integer(int32), save  :: daynum
    integer(int32), save  :: dd
    integer(int32), save  :: lay
    integer(int32), save  :: lncntr
    integer(int32), save  :: mm
    integer(int32), save  :: pdate
    integer(int32), save  :: rowcntr
    integer(int32), save  :: tempsw
    integer(int32), save  :: verns
    integer(int32), save  :: yr
    integer(int32), save  :: yy
    integer(int32), dimension(20), save :: ddae
    integer(int32), dimension(20), save :: ddap
    integer(int32), dimension(20), save :: ddav
    logical, save :: endphenol,jan1
    !
    ! Local variables
    !
    real(sp) :: huc1
    real(sp) :: a_fr
    real(sp) :: b_fr
    real(sp), dimension(400,2) :: lnarray
    integer(int32) :: jd
    !
    ! Start of crop.f90
    !
    !
    !     day of year
    call caldat(ctrl%sim%juldate, dd,mm,yy)
    jd = dayear(dd,mm,yy)
    !
    !debe set daynum = jd here at the top of the subroutine
    daynum = jd
    !
    do lay = 1,soils%spp%nslay
        soils%scp%asfcce(lay) = soils%scp%asfcce(lay)*100.
        soils%scp%asfom(lay) = soils%scp%asfom(lay)*100.
        soils%spp%asfcla(lay) = soils%spp%asfcla(lay)*100.
        ctrl%ndat%wn(lay) = 0.0
        ctrl%ndat%wp(lay) = 0.0
        ctrl%ndat%wno3(lay) = soils%scp%asftan(lay)
        ctrl%ndat%ap(lay) = soils%scp%asftap(lay)
        ctrl%ndat%rsd(lay) = soils%scp%asfsmb(lay)*10.0
    end do

    !     initialize growth and nutrient variables when crop is planted
    !     bio%growth%am0cif is flag to initialize crop at start of planting

    if (bio%growth%am0cif) then
        call cinit(ctrl,clidat,bio,soils, dd,mm,yy,canhty,daa,dae,dap,dav,   &
            & ddae,ddap,ddav,dgdde,dgdds,dgddv,elong,endphenol,gddday,gdda,gdde, &
            & gdds,gddv,gddwsf,jan1,lnarray,lncntr,lnpout,pdate,rowcntr,   &
            & tempsw,todayln,verns,yestln,yr,ln,co2eff)
        !debe 091208 added the following to be initialized in cinit: ddap, elong,
        ! gddday, yr, leaf number variables, verns, gddwsf to be initialized. later
        ! added bio%upgm%canht for initialization for canopy height.
        ! added tempsw to be initialized in cinit and used in emerge. passed bio%upgm%seedsw to
        ! cinit to initialize tempsw with the value in bio%upgm%seedsw. 5122011

        ! set previous values to initial values
        prevbio%mass%standstem = bio%mass%standstem
        prevbio%mass%standleaf = bio%mass%standleaf
        prevbio%mass%standstore = bio%mass%standstore
        prevbio%mass%flatstem = bio%mass%flatstem
        prevbio%mass%flatleaf = bio%mass%flatleaf
        prevbio%mass%flatstore = bio%mass%flatstore
        prevbio%growth%mshoot = bio%growth%mshoot
        do lay = 1,soils%spp%nslay
            prevbio%mass%stemz(lay) = bio%mass%stemz(lay)
            prevbio%mass%rootstorez(lay) = bio%mass%rootstorez(lay)
            prevbio%mass%rootfiberz(lay) = bio%mass%rootfiberz(lay)
        end do
        prevbio%geometry%zht = bio%geometry%zht
        prevbio%geometry%zshoot = bio%geometry%zshoot
        prevbio%geometry%dstm = bio%geometry%dstm
        prevbio%geometry%zrtd = bio%geometry%zrtd
        prevbio%growth%dayap = bio%growth%dayap
        prevbio%growth%thucum = bio%growth%thucum
        prevbio%growth%trthucum = bio%growth%trthucum
        prevbio%geometry%grainf = bio%geometry%grainf
        prevbio%growth%tchillucum = bio%growth%tchillucum
        prevbio%growth%fliveleaf = bio%growth%fliveleaf

        if (bio%growth%am0cfl>=1) then
            ! put double blank lines in daily files to create growth blocks
            write (ctrl%handles%luocrop,*)           ! crop.out
            write (ctrl%handles%luocrop,*)           ! crop.out
            write (ctrl%handles%luoshoot,*)          ! shoot.out
            write (ctrl%handles%luoshoot,*)          ! shoot.out
            write (ctrl%handles%luocanopyht,*)       ! canopyht.out debe added phenol and emerge.out lines
            write (ctrl%handles%luocanopyht,*)       ! canopyht.out
            write (ctrl%handles%luoemerge,*)         ! emerge.out
            write (ctrl%handles%luoemerge,*)         ! emerge.out
        end if

        bio%growth%am0cif = .false.          !turn off after initialization is complete
    else
        ! calculate day length
        clidat%hrlty = clidat%hrlt
        clidat%hrlt = daylen(clidat%xlat,jd,civilrise)

        ! set trend direction for living leaf area from external forces
        trend = (bio%growth%fliveleaf*bio%mass%standleaf) - (prevbio%growth%fliveleaf*prevbio%mass%standleaf)
        if ((trend/=0.0).and.(bio%growth%thucum/bio%database%thum>bio%database%hue)) bio%growth%leafareatrend = trend
        ! trend non-zero and heat units past emergence
    end if
    !
    !if ((daynum.ge.258).and.(daynum.le.365)) then
    !    print*, 'in crop before call to phenolmms, daynum = ', daynum
    !end if

    !debe added call to phenolmms. call it after call to cinit to insure that
    !variables are initialized.
    call phenolmms(ctrl, clidat, bio, daa,dae,  &
        & dap,dav,daynum,dd,ddae,ddap,ddav,dgdde,dgdds,     &
        & dgddv,endphenol, gdda,gddday,gdde,gdds,gddv,gddwsf,jan1,   &
        & lnarray,lncntr,lnpout,mm,pdate,rowcntr,todayln,yestln,yy,ln)

    !debe added bio%upgm%dayhtinc to be able to pass the daily increase in height to growth
    ! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
    ! bio%upgm%canopyflg = 1.

    !debe moved setting of jan1 to true out of phenolmms so that it can be
    ! passed into phenolmms the day after vernalization has occurred so that
    ! dav (days after vernalization) will be = 1 on the first day after
    ! vernalization.
    if ((mm==1).and.(dd==1).and.(yy==2)) jan1 = .true.

    ! check for consecutive "warm" days based on daily
    ! average temperature
    if (0.5*(clidat%awtdmx+clidat%awtdmn)>bio%database%tmin) then
        ! this is a warm day
        bio%growth%twarmdays = bio%growth%twarmdays + 1
    else
        ! reduce warm day total, but do not zero, for proper fall
        ! regrow of perennials
        bio%growth%twarmdays = bio%growth%twarmdays/2
    end if

    ! accumulate chill units
    call chillu(bio%growth%tchillucum,clidat%awtdmx,clidat%awtdmn)

    ! zero out temp pool variables used in testing for residue
    ! from regrowth in callcrop
    cropres%standstem = 0.0
    cropres%standleaf = 0.0
    cropres%standstore = 0.0
    cropres%flatstem = 0.0
    cropres%flatleaf = 0.0
    cropres%flatstore = 0.0

    ! check crop type for shoot growth action
    if ((bio%database%fleaf2stor>0.0).or.(bio%database%fstem2stor>0.0).or.(bio%database%fstor2stor>0.0)) then
        if ((bio%database%idc==2).or.(bio%database%idc==5)) then
            ! check winter annuals for completion of vernalization,
            ! warming and spring day length
            if ((bio%growth%zgrowpt>0.0).and.(bio%growth%tchillucum>=chilluv).and.                       &
                & (bio%growth%twarmdays>=shoot_delay*bio%database%tverndel/verndelmax).and.(clidat%huiy>spring_trig)&
                & ) then
            ! vernalized and ready to grow in spring
            bio%growth%thu_shoot_beg = bio%growth%thucum/bio%database%thum
            bio%growth%thu_shoot_end = bio%growth%thucum/bio%database%thum + bio%database%hue

            call shootnum(soils%spp%nslay,bio%database%idc,bio%geometry%dpop,bio%database%shoot,bio%database%dmaxshoot,bio%growth%mtotshoot,    &
                & bio%mass%rootstorez,bio%geometry%dstm)
            ! eliminate diversion of biomass to crown storage
            bio%database%fleaf2stor = 0.0
            bio%database%fstem2stor = 0.0
            bio%database%fstor2stor = 0.0
            ! set day of year on which transition took place
            bio%growth%dayspring = jd

            end if
        else if (bio%database%idc/=7) then
            ! bi-annuals and perennials with tuber dormancy don't need
            ! either of these checks. doing nothing here prevents
            ! resprouting after defoliation
            ! check summer annuals and perennials for removal of all
            ! (most) leaf mass
            if (bio%growth%leafareatrend<0.0) then     ! last change in leaf area was a
                ! reduction
                ! ! 0.42 * 2 = 0.84
                if (bio%growth%fliveleaf*bio%mass%standleaf<0.84*bio%database%storeinit*bio%geometry%dpop*mgtokg*bio%database%fleafstem&
                    & /(bio%database%fleafstem+1.0)) then            ! below minimum living leaf mass
                !(which is twice seed leaf mass)
                if (bio%growth%twarmdays>=shoot_delay) then   ! enough warm days to start
                    ! regrowth
                    if (bio%growth%thucum/bio%database%thum>=bio%database%hue) then   ! heat units past emergence
                        ! ! not yet mature
                        if ((bio%growth%thucum<bio%database%thum).or.((bio%database%idc==3).or.(bio%database%idc==6))) then
                            ! perennial
                            ! find out how much root store could be released for regrowth
                            call shootnum(soils%spp%nslay,bio%database%idc,bio%geometry%dpop,bio%database%shoot,bio%database%dmaxshoot,    &
                                & root_store_rel,bio%mass%rootstorez,pot_stems)
                            ! find the potential leaf mass to be achieved with regrowth
                            if (bio%database%zloc_regrow>0.0) then
                                pot_leaf_mass = bio%mass%standleaf +                           &
                                    & 0.42*min(root_store_rel,bio%growth%mtotshoot)     &
                                    & *bio%database%fleafstem/(bio%database%fleafstem+1.0)
                            else
                                pot_leaf_mass = 0.42*root_store_rel*bio%database%fleafstem/         &
                                    & (bio%database%fleafstem+1.0)
                            end if
                            ! is present living leaf mass less than leaf mass from
                            ! storage regrowth
                            if ((bio%growth%fliveleaf*bio%mass%standleaf)<pot_leaf_mass) then
                                ! regrow possible from shoot for perennials, annuals.
                                ! reset growth clock
                                bio%growth%thucum = 0.0
                                bio%growth%thu_shoot_beg = 0.0
                                bio%growth%thu_shoot_end = bio%database%hue
                                ! reset shoot grow configuration
                                if (bio%database%zloc_regrow>0.0) then
                                    ! regrows from stem, stem does not become residue
                                    ! note, flat leaves are dead leaves, no storage in
                                    ! shoot.
                                    bio%growth%mshoot = bio%mass%standstem + bio%mass%flatstem + bio%mass%standleaf
                                    do lay = 1,soils%spp%nslay
                                        bio%growth%mshoot = bio%growth%mshoot + bio%mass%stemz(lay)
                                    end do
                                    bio%growth%mtotshoot = min(root_store_rel,bio%growth%mtotshoot)
                                else
                                    ! regrows from crown, stem becomes residue
                                    cropres%standstem = bio%mass%standstem
                                    cropres%standleaf = bio%mass%standleaf
                                    cropres%standstore = bio%mass%standstore
                                    cropres%flatstem = bio%mass%flatstem
                                    cropres%flatleaf = bio%mass%flatleaf
                                    cropres%flatstore = bio%mass%flatstore
                                    do lay = 1,soils%spp%nslay
                                        cropres%bgstemz(lay) = bio%mass%stemz(lay)
                                    end do
                                    cropres%grainf = bio%geometry%grainf
                                    cropres%zht = bio%geometry%zht
                                    cropres%dstm = bio%geometry%dstm
                                    cropres%xstmrep = biotot%xstmrep
                                    ! reset crop values to indicate new growth cycle
                                    bio%growth%mshoot = 0.0
                                    bio%mass%standstem = 0.0
                                    bio%mass%standleaf = 0.0
                                    bio%mass%standstore = 0.0
                                    bio%mass%flatstem = 0.0
                                    bio%mass%flatleaf = 0.0
                                    bio%mass%flatstore = 0.0
                                    do lay = 1,soils%spp%nslay
                                        bio%mass%stemz(lay) = 0.0
                                    end do
                                    bio%geometry%grainf = 0.0
                                    bio%geometry%zht = 0.0
                                    bio%growth%mtotshoot = root_store_rel
                                    bio%geometry%dstm = pot_stems
                                end if
                            end if
                        end if
                    end if
                end if
                end if
            end if
        end if
    end if

    ! calculate growing degree days
    ! set default heat unit delay value
    hu_delay = 1.0
    if (bio%database%thum<=0.0) then
        ! always keep this invalid plant in first stage growth
        clidat%huiy = 0.0
        clidat%hui = 0.0
    else
        ! previous day heat unit index
        clidat%huiy = bio%growth%thucum/bio%database%thum
        clidat%huirty = bio%growth%trthucum/bio%database%thum
        ! check for growth completion
        if (clidat%huiy<1.0) then
            ! accumulate additional for today
            ! check for emergence status
            if ((clidat%huiy>=bio%database%hue).and.(clidat%huiy<spring_trig)) then
                ! emergence completed, account for vernalization and
                ! photo period by delaying development rate until chill
                ! units completed and spring trigger reached

                !vern_delay = 1.0 minus the thermal delay coefficient pre-vernalization
                ! multiplied by the value of the total number of chill units needed for
                ! vernalization (parameter set to 50) minus the accumulated chilling
                ! units in days.
                vern_delay = 1.0 - bio%database%tverndel*(chilluv-bio%growth%tchillucum)
                photo_delay = 1.0
                !hu_delay: combined reduction in heat unit accummulation
                hu_delay = max(dev_floor,min(vern_delay,photo_delay))
            end if
            ! do not accumulate heat units if daily minimum is below
            ! freezing.
            !if( bwtdmn .gt. 0.0 ) then accumulate heat units using set heat unit
            ! delay
            bio%growth%thucum = bio%growth%thucum + huc1(clidat%awtdmx,clidat%awtdmn,bio%database%topt,bio%database%tmin)*hu_delay
            !              end if
            ! root depth growth heat units
            bio%growth%trthucum = bio%growth%trthucum + huc1(clidat%awtdmx,clidat%awtdmn,bio%database%topt,bio%database%tmin)
            ! do not cap this for annuals, to allow it to continue
            ! root mass partition is reduced to lower levels after the
            ! first full year. out of range is capped in the function
            ! in growth.for
            ! bio%growth%trthucum = min(bio%growth%trthucum, bio%database%thum)
            ! calculate heat unit index
            clidat%hui = bio%growth%thucum/bio%database%thum
            clidat%huirt = bio%growth%trthucum/bio%database%thum

        end if

    end if

    !de and gm changed this line of code for 'if(huiy.lt.1.0) then'
    ! to incorporate the phenologymms way of determining maturity.
    !de changed the if statement to include the decision of which method will
    ! be used to determine maturity using bio%upgm%phenolflg. if bio%upgm%phenolflg is 1 then the phenologymms
    ! way of reaching maturity is to be used and that occurs when bio%upgm%mats(1) has a value not equal
    ! to 999. if bio%upgm%phenolflg = 0, then the weps way will be used. when huiy is greater than or
    ! equal to 1.0, maturity has been reached. this allows the possibility of using either
    ! approach to determine maturity.

    if (((bio%upgm%phenolflg==1).and.(bio%upgm%mats(1)==999)).or.((bio%upgm%phenolflg==0).and.(clidat%huiy<1.0))) then

        ! crop growth not yet complete
        ! increment day after planting counter since growth happens
        ! same day
        !                           on the day of planting.
        ! if (daynum.gt.pdate) bio%growth%dayap = bio%growth%dayap + 1!debe added to prevent bio%growth%dayap incrementing. also, doesn't work for crops going over winter, e.g. winter wheat
        bio%growth%dayap = bio%growth%dayap + 1   ! changed back to this way because of comment above that
        ! growth happens on planting day.

        ! seedling, transplant initialization and winter annual shoot
        ! growth calculations using root reserves
        ! daily shoot growth
        !
        ! try #1, change bio%growth%thu_shoot_end in debugger on daynum 264 and 265
        !   (daysim = 7&8) from 0.05 to 0.06 so shoot_grow would be called and
        !   d_shoot_mass became negative in shoot_grow (~line 242) and emerge
        !   never got called:
        !  if ((huiy .lt. 0.06).and.(hui.gt.bio%growth%thu_shoot_beg))

        ! try #2, change bio%growth%thu_shoot_end with following code on daynum 264
        !   and 265 (daysim = 7&8)from 0.05 to 0.06 so shoot_grow would be called
        !      if ((daysim .eq. 7) .or. (daysim .eq. 8)) then
        !          bio%growth%thu_shoot_end = 0.06
        !      else
        !          bio%growth%thu_shoot_end = 0.05
        !      endif

        ! try #3, if using phenologymms emergence code (bio%upgm%emrgflg=1), emergence
        ! has not occurred yet (bio%upgm%ems(1)=999), and hui.gt.bio%growth%thu_shoot_beg) call
        ! shoot_grow:
        if ((bio%upgm%emrgflg==1).and.(bio%upgm%ems(1)==999).and.(clidat%hui>bio%growth%thu_shoot_beg)) then
            !      if ((daysim .eq. 7) .or. (daysim .eq. 8)) then
            !          bio%growth%thu_shoot_end = 0.06
            !      else
            !          bio%growth%thu_shoot_end = 0.05
            !      endif

            !debe replaced bio%growth%thucum (upgm/weps) with gdds (phenologymms) in the call
            ! to shoot_grow.
            !
            ! the following "fix" will also need to reset bio%growth%thu_shoot_end back to
            ! original value once emergence occurs. to avoid the problem where huiy
            ! = bio%growth%thu_shoot_end in the call to shoot_grow, add 0.00001 to huiy below:
            if (clidat%huiy>=bio%growth%thu_shoot_end) bio%growth%thu_shoot_end = clidat%huiy + 0.00001      !should 'then' be here? no, then is above
            call shoot_grow(ctrl,clidat, bio, soils%spp%nslay,soils%spp%aszlyd(1),bio%geometry%dpop,bio%database%zmxc,bio%database%zmrt,bio%database%fleafstem,bio%database%fshoot,   &
                & bio%database%ssa,bio%database%ssb,bio%database%diammax,clidat%hui,clidat%huiy,bio%growth%thu_shoot_beg,         &
                & bio%growth%thu_shoot_end,bio%mass%standstem,bio%mass%standleaf,bio%mass%standstore,   &
                & bio%mass%flatstem,bio%mass%flatleaf,bio%mass%flatstore,bio%growth%mshoot,bio%growth%mtotshoot, &
                & bio%mass%stemz,bio%mass%rootstorez,bio%mass%rootfiberz,bio%geometry%zht,bio%geometry%zshoot,     &
                & bio%geometry%dstm,bio%geometry%zrtd,bio%growth%zgrowpt,bio%growth%fliveleaf,bio%bname,bio%geometry%hyfg,         &
                & bio%database%yld_coef,bio%database%resid_int,bio%database%grf,daysim,dap,gdds,bio%database%growdepth, &
                & bio%upgm%seedsw,bio%bname,bio%upgm%soilwat,bio%upgm%wfpslo,bio%upgm%wfpsup,bio%upgm%germgdd,bio%upgm%ergdd,bio%upgm%gddtbg,&
                & ddap,dgdds,elong,bio%upgm%ems,bio%upgm%germs,gddday,yy,bio%upgm%emrgflg,ctrl%sim%icli,ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py,&
                & yr,ctrl%sim%cliname,bio%upgm%egdd,bio%upgm%ggdd,tempsw)                          !used dap in place of bio%growth%dayap
        else if ((clidat%huiy<bio%growth%thu_shoot_end).and.(clidat%hui>bio%growth%thu_shoot_beg)) then
            call shoot_grow(ctrl,clidat, bio, soils%spp%nslay,soils%spp%aszlyd(1),bio%geometry%dpop,bio%database%zmxc,bio%database%zmrt,bio%database%fleafstem,bio%database%fshoot,   &
                & bio%database%ssa,bio%database%ssb,bio%database%diammax,clidat%hui,clidat%huiy,bio%growth%thu_shoot_beg,         &
                & bio%growth%thu_shoot_end,bio%mass%standstem,bio%mass%standleaf,bio%mass%standstore,   &
                & bio%mass%flatstem,bio%mass%flatleaf,bio%mass%flatstore,bio%growth%mshoot,bio%growth%mtotshoot, &
                & bio%mass%stemz,bio%mass%rootstorez,bio%mass%rootfiberz,bio%geometry%zht,bio%geometry%zshoot,     &
                & bio%geometry%dstm,bio%geometry%zrtd,bio%growth%zgrowpt,bio%growth%fliveleaf,bio%bname,bio%geometry%hyfg,         &
                & bio%database%yld_coef,bio%database%resid_int,bio%database%grf,daysim,dap,gdds,bio%database%growdepth, &
                & bio%upgm%seedsw,bio%bname,bio%upgm%soilwat,bio%upgm%wfpslo,bio%upgm%wfpsup,bio%upgm%germgdd,bio%upgm%ergdd,bio%upgm%gddtbg,&
                & ddap,dgdds,elong,bio%upgm%ems,bio%upgm%germs,gddday,yy,bio%upgm%emrgflg,ctrl%sim%icli,ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py,&
                & yr,ctrl%sim%cliname,bio%upgm%egdd,bio%upgm%ggdd,tempsw)        !used dap in place of this bio%growth%dayap
        end if
        ! ends the if bio%upgm%emrgflg = 1 if block

        !debe added the new arrays bio%upgm%egdd, bio%upgm%ggdd to the calls above to shoot_grow to be
        ! passed on to emerge where they are used. added tempsw as the array index of these arrays.

        if ((clidat%huiy<bio%growth%thu_shoot_end).and.(clidat%hui>=bio%growth%thu_shoot_end)) then
            ! shoot growth completed on this day
            ! move growing point to regrowth depth after shoot growth
            ! complete
            bio%growth%zgrowpt = (-bio%database%zloc_regrow)
            ! single blank line to separate shoot growth periods
            if (bio%growth%am0cfl>=1) write (ctrl%handles%luoshoot,*)
            ! shoot.out

        end if

        ! temporary location
        call scrv1(bio%database%fd1(1),bio%database%fd1(2),bio%database%fd2(1),bio%database%fd2(2),a_fr,b_fr)   ! frost damage
        !   print*, 'in crop before call to growth, daysim = ', daysim

        !          ! calculate plant growth state variables
        call growth(ctrl, soils%spp%nslay,bio, soils%spp%aszlyd(1),bio%database%ck,bio%database%grf,bio%database%ehu0,bio%database%zmxc,bio%database%idc,bio%bname,a_fr,b_fr,  &
            & bio%geometry%xrow,bio%database%diammax,bio%database%zmrt,bio%database%tmin,bio%database%topt,bio%database%bceff,bio%database%alf,bio%database%blf,    &
            & bio%database%clf,bio%database%dlf,bio%database%arp,bio%database%brp,bio%database%crp,bio%database%drp,bio%database%aht,bio%database%bht,bio%database%ssa,   &
            & bio%database%ssb,bio%database%sla,bio%database%xstm,soils%spp%ahtsmn,clidat%awtdmx,clidat%awtdmn,clidat%aweirr,ctrl%cropstress%ahfwsf,clidat%hui,clidat%huiy, &
            & clidat%huirt,clidat%huirty,hu_delay,bio%growth%thu_shoot_end,bio%database%baflg,bio%database%baf,bio%database%yraf,bio%geometry%hyfg,&
            & bio%database%fleaf2stor,bio%database%fstem2stor,bio%database%fstor2stor,bio%database%yld_coef,bio%database%resid_int,    &
            & bio%mass%standstem,bio%mass%standleaf,bio%mass%standstore,bio%mass%flatstem,bio%mass%flatleaf,  &
            & bio%mass%flatstore,bio%mass%rootstorez,bio%mass%rootfiberz,bio%mass%stemz,bio%geometry%zht,bio%geometry%dstm, &
            & bio%geometry%zrtd,bio%growth%fliveleaf,bio%growth%dayap,bio%geometry%grainf,bio%geometry%dpop,bio%upgm%dayhtinc,daysim,gddday,&
            & bio%upgm%growth_stress,bio%upgm%canht,canhty,bio%upgm%canopyflg,bio%upgm%antss,bio%upgm%phenolflg,bio%upgm%boots,bio%upgm%heads, &
            & bio%upgm%joints,bio%upgm%mats,ln,bio%upgm%co2x,bio%upgm%co2y,clidat%co2atmos,co2eff,clidat%ts)

        ! debe added bio%upgm%joints, bio%upgm%heads and bio%upgm%mats to send to growth.

        !debe090408 added gddday to print out gdd.
        ! a. later added variables for canopyht to be passed to growth to canopyht subroutine.
        ! b. later decided to call canopyht right from crop which was done below after the call to
        ! phenol.
        ! c. even later most all calls to subroutines from phenologymms were put into a new
        ! subroutine called phenolmms which is called above here in crop.

        !debe added bio%upgm%growth_stress because it is now read in.
        !debe added bio%upgm%canopyflg and bio%upgm%canht to be passed to growth so that bio%upgm%canht calculated
        ! from the phenologymms canopy height subroutine can be used in place of dht in growth.
        !debe added bio%upgm%dayhtinc to be able to pass the daily increase in height to growth
        ! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
        ! bio%upgm%canopyflg = 1.
        !debe added bio%upgm%antss to the growth subroutine to limit calculating canopy height after anthesis.
        !debe added canhty (yesterday's canopy height) so that it will be initialized the first time
        ! it is passed to growth and so it can be saved here and be available tomorrow to compare
        ! with the 'today' value tomorrow.
        !debe added passing bio%upgm%antss, bio%upgm%phenolflg and bio%upgm%boots to growth.

        ! set trend direction for living leaf area
        trend = (bio%growth%fliveleaf*bio%mass%standleaf) - (prevbio%growth%fliveleaf*prevbio%mass%standleaf)
        if ((trend/=0.0).and.(bio%growth%thucum/bio%database%thum>bio%database%hue)) bio%growth%leafareatrend = trend
        ! trend non-zero and heat units past emergence
        ! set saved values of crop state variables for comparison next
        ! time
        prevbio%mass%standstem = bio%mass%standstem
        prevbio%mass%standleaf = bio%mass%standleaf
        prevbio%mass%standstore = bio%mass%standstore
        prevbio%mass%flatstem = bio%mass%flatstem
        prevbio%mass%flatleaf = bio%mass%flatleaf
        prevbio%mass%flatstore = bio%mass%flatstore
        prevbio%growth%mshoot = bio%growth%mshoot
        do lay = 1,soils%spp%nslay
            prevbio%mass%stemz(lay) = bio%mass%stemz(lay)
            prevbio%mass%rootstorez(lay) = bio%mass%rootstorez(lay)
            prevbio%mass%rootfiberz(lay) = bio%mass%rootfiberz(lay)
        end do
        prevbio%geometry%zht = bio%geometry%zht
        prevbio%geometry%zshoot = bio%geometry%zshoot
        prevbio%geometry%dstm = bio%geometry%dstm
        prevbio%geometry%zrtd = bio%geometry%zrtd
        prevbio%growth%dayap = bio%growth%dayap
        prevbio%growth%thucum = bio%growth%thucum
        prevbio%growth%trthucum = bio%growth%trthucum
        prevbio%geometry%grainf = bio%geometry%grainf
        prevbio%growth%tchillucum = bio%growth%tchillucum
        prevbio%growth%fliveleaf = bio%growth%fliveleaf
    else
        ! heat units completed, crop leaf mass is non transpiring
        bio%growth%fliveleaf = 0.0

        ! check for mature perennial that may re-sprout before fall
        ! (alfalfa, grasses)
        if ((bio%database%idc==3).or.(bio%database%idc==6)) then
            ! check for growing weather and regrowth ready state
            ! transfer all mature biomass to residue pool
            ! find number of stems to regrow
            ! reset heat units to start shoot regrowth
        end if

        ! accumulate days after maturity
        bio%growth%dayam = bio%growth%dayam + 1

    end if
    !
    ! debe 091008 changed the following phenol variables to the upgm variable
    ! name already being used:
    ! phenology name = upgm name
    ! cname          = bio%bname
    ! year           = yy

    do lay = 1,soils%spp%nslay
        soils%scp%asfcce(lay) = soils%scp%asfcce(lay)/100.
        soils%scp%asfom(lay) = soils%scp%asfom(lay)/100.
        soils%spp%asfcla(lay) = soils%spp%asfcla(lay)/100.
    end do
    !
    end subroutine crop


    !
    !debe bio%upgm%dummy1 is not currently used here or passed on but left it here in
    ! case it is used in the future. ! verns is also not currently used.
    !debe added bio%upgm%growth_stress because it is now being read in from a file.
    !debe added variables for dry beans in call to phenol.
    !debe added temperature variables bio%upgm%toptlo, topup, bio%upgm%tbase.
    !debe added bio%upgm%canopyflg.
    !debe added bio%upgm%ecanht so that it can be read in instead of set in the code for each crop.
    !debe  - growth stage variables are now declared and initialized in main via cropinit.
    ! bio%upgm%phenolflg is read in from upgm_crop.dat. these are now passed into crop from main via callcrop.
    !
    ! ***** emergence *****
    !
    !debe added the following emergence arguments so that they can be passed to
    ! shoot_grow. these include bio%upgm%seedsw, bio%bname, bio%upgm%soilwat, bio%upgm%wfpslo, bio%upgm%wfpsup,
    ! bio%upgm%germgdd, bio%upgm%ergdd, gddday, bio%upgm%gddtbg.  ac0nam is passed into bio%bname in main to
    ! be passed to emerge.
    !debe added the emergence flag (bio%upgm%emrgflg)to determine whether to call the
    ! emerge subroutine or not.
    !debe added ctrl%sim%icli to pass to emerge to enable printing the weather file name
    ! in emerge.out. also, added ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py to print planting date in emerge.out.
    ! later changed to using ctrl%sim%cliname to print the weather file name.  it is read
    ! in from the header in the climate file.
    !debe changed bio%upgm%seedsw from integer to real to allow moving a half soil moisture level.
    ! later changed it back because array subscripts must be integers or constants.
    !debe added two new arrays of 6 elements to be used in emerge.for to
    ! enable adding values for bio%upgm%germgdd and bio%upgm%ergdd for two soil moisture
    ! levels that are half steps between dry and medium and medium and optimum.
    !debe added tempsw to be passed to cinit to be initialized and then used in emerge.
    !
    ! ***** phenology *****
    !
    !debe added bio%upgm%aepa and bio%upgm%first7 because they are initialized in cropinit and
    ! need to be passed to crop. other phenology variables include: bio%upgm%dummy2,
    ! bio%upgm%pchron.
    !
    ! ***** gddcalc *****
    !
    !debe added the variable for the maximum upper temperature (bio%upgm%tupper) value
    !and bio%upgm%tbase, bio%upgm%toptlo, topup values.
    ! read in from upgm_crop.dat in main. it will be passed from crop to
    ! gddcalc.
    !debe added bio%upgm%callgdd which is initialized in cropinit and used in crop.
    !debe added bio%upgm%gmethod passed into crop which is read in from upgm_crop.dat
    ! in main.
    !
    ! ***** canopyht *****
    !
    !debe added bio%upgm%maxht, read in from upgm_crop in main and passed to canopyht.
    ! add bio%upgm%canht so the final canopy height can be passed back to main and
    ! printed in phenol.out.
    !debe added bio%upgm%canopyflg to determine which method of calculating plant height
    ! will be used. if 0, use weps/upgm method; if 1, use phenologymms method.
    !debe added bio%upgm%dayhtinc to be able to pass the daily increase in height to growth
    ! for the ht_dia_sai subroutine in place of the weps variable dht when
    ! bio%upgm%canopyflg = 1.
    !
    ! ***** co2 *****
    !debe added bio%upgm%co2x, bio%upgm%co2y for the co2 arrays and clidat%co2atmos for the atmospheric co2 level.
    ! Also, added co2eff which will be the effect of the read-in atmospheric co2
    ! level on the plant, i.e., the adjustment factor. Nedded to have it initialized before calculating
    ! this value in Growth.f90
    !
    !
    !debe added canhty so it can be initialized in cinit
    !
    !print*,'just after declarations in crop, bio%upgm%seedsw  ', bio%upgm%seedsw
    !print*,'just after declarations in crop, bio%upgm%silks(1) = ', bio%upgm%silks(1)
    !

    ! ***** upgm/weps variables *****
    !
    !debe added bio%upgm%growth_stress because it is now being read in from a file.
    !
    !     + + + argument declarations + + +
    !
    ! ***** newly added variables *****
    ! note: some variables are used in more than one subroutine
    !
    ! ******** seedling emergence ********
    !debe added variables that need to be passed to emerge after initialization.

    !***** phenology *****
    !
    ! phenol and leaf number variables
    ! debe added gddwsf array to be passed to cinit for initialization.
    !note: leaf number variables are not used yet because determining growth
    ! stages by the number of leaves is not currently implemented.
    !debe changed dimensions of dummy 1 and 2 arrays for stressed and
    ! non-stressed values
    !
    !***** phenology maturity *****
    ! debe added bio%upgm%phenolflg to determine whether the upgm/weps method of determining maturity will
    ! be used or whether the phenologymms method will be used. if bio%upgm%phenolflg = 0, the upgm/weps way will
    ! be used and if bio%upgm%phenolflg = 1, the phenologymms way will be used. later this variable will be read in in main and initialized
    ! and passed to crop. for testing, bio%upgm%phenolflg will be a local variable only.

    ! ***** gdd calculation *****
    !
    !debe added bio%upgm%callgdd as a flag to either call the new gddcalc subroutine
    ! or use the code within crop.
    ! gcm 013009 added bio%upgm%gmethod and three methods of calculating gddday
    !
    ! ***** vernalization *****
    !
    !debe added jan1 variable for vernalization to test if
    ! jan1 has been passed for winter crops
    !
    !***** other variables *****
    !
    !
    ! ***** canopy height *****
    !
    !debe added bio%upgm%canopyflg, bio%upgm%dayhtinc, bio%upgm%ecanht.
    !
    !***** CO2 variables *****
    ! debe added clidat%co2atmos, co2eff, bio%upgm%co2x, bio%upgm%co2y
    !

    ! + + + local variables + + +
    !
    !debe added verns variable to allow for accumulation of gdd after
    ! vernalization. this will be used to trigger tillering in crops that
    ! tiller. this will be done in the appropriate phenol_cropname.
    !debe 020309 added local variables tavgtemp, tmaxtemp, tmintemp to
    !  calculate gdd by method 2 within crop.
    ! the rest of the local variables are upgm/weps variables.
    !
    !dayear, daylen and huc1 are functions

    !debe added to save variables for next day's use.
    !
    !
    !     the following subroutine arguments are not used: bio%database%transf, bhtsmx,
    !     bhzeta, bhzpta, bhzptp, bio%growth%am0cgf, prevbio%growth%mtotshoot, bwzdpt   jcaii  8/08/08
    !
    !     + + + purpose + + +
    !     this is the main program for implementing the crop growth
    !     calculations in the various subroutines. for any questions refer
    !     to amare retta at the usda wind erosion research laboratory,
    !     university, manhattan ks 66506.

    !     + + + keywords + + +
    !     wind erosion crop model

    !     the following subroutine arguments are not used: bio%database%transf, bhtsmx,
    !     bhzeta, bhzpta, bhzptp, bio%growth%am0cgf, prevbio%growth%mtotshoot, bwzdpt   jcaii  8/08/08

    !     + + + argument definitions + + +
    !     bio%database%aht (ac0aht)- height s-curve parameter
    !     bio%database%alf (ac0alf)- leaf partitioning parameter
    !     bio%database%arp - rprd partitioning parameter
    !     bio%database%bceff - biomass conversion efficiency (kg/ha/mj). currently
    !                not used.
    !     bio%database%bht - height s-curve parameter
    !     bio%database%blf - leaf partitioning parameter.
    !     bc0bn1 - normal fraction of n in crop biomass at emergence.
    !              currently not used.
    !     bc0bn2 - normal fraction of n in crop biomass at midseason
    !     bc0bn3 - normal fraction of n in crop biomass at maturity
    !     bc0bp1 - normal fraction of p in crop biomass at emergence
    !     bc0bp2 - normal fraction of p in crop biomass at midseason
    !     bc0bp3 - normal fraction of p in crop biomass at maturity
    !     bio%database%brp - rprd partitioning parameter
    !     bc0ck  - light extinction coefficient (fraction)
    !     bio%database%clf - leaf partitioning parameter
    !     bio%database%crp - rprd partitioning parameter
    !     bio%database%diammax - crop maximum plant diameter (m)
    !     bio%database%dlf - leaf partitioning parameter
    !     bio%database%drp - rprd partitioning parameter
    !     bio%database%fd1(1) - minimum temperature below zero (c)
    !     bio%database%fd2(1) - minimum temperature below zero (c)
    !     bio%database%growdepth - depth of growing point at time of planting (m)
    !     bio%database%hue - relative heat unit for emergence (fraction)
    !     bc0idc - crop type:annual,perennial,etc
    !     bc0nam - crop name
    !     bio%database%shoot - mass from root storage required for each shoot (mg/shoot)
    !     bio%database%sla - specific leaf area (cm^2/g)
    !     bio%database%ssa - stem area to mass coefficient a, result is m^2 per plant
    !     bio%database%ssb - stem area to mass coefficient b, argument is kg per plant
    !     bio%database%storeinit - db input, crop storage root mass initialzation
    !                    (mg/plant)
    !     bio%database%transf - currently not used. db input flag:
    !                 0 = crop is planted using stored biomass of seed or
    !                     vegatative propagants
    !                 1 = crop is planted as a transplant with roots, stems
    !                     and leaves present
    !     bio%database%baf (acbaf) - biomass adjustment factor
    !     bio%database%baflg (acbaflg) - flag for biomass adjustment action
    !         0     o normal crop growth
    !         1     o find biomass adjustment factor for target yield
    !         2     o use given biomass adjustment factor
    !     bio%growth%dayam - number of days since crop matured
    !     bio%growth%dayap - number of days of growth completed since crop planted
    !     bio%growth%dayspring - day of year in which a winter annual releases stored
    !                   growth
    !     bio%database%dmaxshoot - maximum number of shoots possible from each plant
    !     bio%geometry%dpop - crop seeding density (#/m^2)
    !     bio%geometry%dstm - number of crop stems per unit area (#/m^2)
    !     bcehu0 - relative gdd at start of senescence
    !     bio%database%fleaf2stor - fraction of assimilate partitioned to leaf that is
    !                    diverted to root store
    !     bio%database%fleafstem - crop leaf to stem mass ratio for shoots
    !     bio%growth%fliveleaf - fraction of standing plant leaf which is living
    !                   (transpiring)
    !     bio%database%fshoot - crop ratio of shoot diameter to length
    !     bio%database%fstem2stor - fraction of assimilate partitioned to stem that is
    !                    diverted to root store
    !     bio%database%fstor2stor - fraction of assimilate partitioned to standing
    !                    storage (reproductive)that is diverted to root store
    !     bio%geometry%grainf - internally computed reproductive grain fraction
    !     bcgrf - fraction of reproductive biomass that is yield
    !     bio%geometry%hyfg - flag indicating the part of plant to apply the "grain
    !              fraction",
    !              grf, to when removing that plant part for yield
    !         0     grf applied to above ground storage (seeds, reproductive)
    !         1     grf times growth stage factor (see growth.for) applied to
    !               above ground storage (seeds, reproductive)
    !         2     grf applied to all aboveground biomass (forage)
    !         3     grf applied to leaf mass (tobacco)
    !         4     grf applied to stem mass (sugarcane)
    !         5     grf applied to below ground storage mass (potatoes,
    !               peanuts)
    !     bio%growth%leafareatrend - direction in which leaf area is trending. saves
    !                       trend even if leaf area is static for long
    !                       periods.
    !     bio%mass%stemz - crop stem mass below soil surface by soil layer
    !                  (kg/m^2)
    !     bio%mass%flatleaf  - crop flat leaf mass (kg/m^2)
    !     bio%mass%flatstem  - crop flat stem mass (kg/m^2)
    !     bio%mass%flatstore - crop flat storage mass (kg/m^2)
    !     bio%mass%rootfiberz - crop root fibrous mass by soil layer (kg/m^2)
    !     bio%mass%rootstorez - crop root storage mass by soil layer (kg/m^2)
    !                     (tubers (potatoes, carrots), extended leaf (onion),
    !                     seeds (peanuts))
    !     bio%growth%mshoot - crop shoot mass grown from root storage (kg/m^2)
    !                this is a "breakout" mass and does not represent a
    !                unique pool since this mass is distributed into below
    !                ground stem and standing stem as each increment of the
    !                shoot is added
    !     bio%mass%standleaf - crop standing leaf mass (kg/m^2)
    !     bio%mass%standstem - crop standing stem mass (kg/m^2)
    !     bio%mass%standstore - crop standing storage mass (kg/m^2)
    !                     (head with seed, or vegetative head (cabbage,
    !                     pineapple))
    !     bio%growth%mtotshoot - total mass of shoot growing from root storage
    !                   biomass (kg/m^2) in the period from beginning to
    !                   completion of emegence heat units
    !     bio%database%resid_int - residue intercept (kg/m^2)
    !                   harvest_residue = bio%database%yld_coef(kg/kg)
    !                   * yield + bio%database%resid_int (kg/m^2)
    !     bio%growth%tchillucum - accumulated chilling units (days)
    !     bio%database%tdtm - days to maturity (same as dtm)
    !     bio%growth%thu_shoot_beg - heat unit index (fraction) for beginning of
    !                       shoot grow from root storage period
    !     bio%growth%thu_shoot_end - heat unit index (fraction) for end of shoot
    !                       grow from root storage period
    !     bio%growth%thucum - crop accumulated heat units
    !     bio%database%thudf (acthudf) - heat units or days to maturity flag
    !                         0 = days to maturity and average conditions
    !                             used to find heat units
    !                         1 = heat units specified used directly
    !     bio%database%thum - potential heat units for crop maturity (deg. c)
    !     bio%database%tmin - base temperature (deg. c)
    !     bio%database%topt - optimum temperature (deg. c)
    !     bio%growth%trthucum - accumulated root growth heat units (degree-days)
    !     bio%database%tverndel - thermal delay coefficient pre-vernalization
    !     bio%growth%twarmdays - number of consecutive days that the daily average
    !                   temperature has been above the minimum growth
    !                   temperature
    !     bio%geometry%xrow (acxrow) - crop row spacing (m)
    !     bio%database%xstm - crop stem diameter (m)
    !     bio%database%xstmrep - a representative diameter so that
    !                 acdstm*acxstmrep*aczht=acrsai
    !     bio%database%yld_coef - yield coefficient (kg/kg)
    !                  harvest_residue = bio%database%yld_coef(kg/kg)
    !                  * yield + bio%database%resid_int (kg/m^2)
    !     bio%database%yraf (acyraf) - yield to biomass ratio adjustment factor
    !     bio%growth%zgrowpt - depth in the soil of the gowing point (m)
    !     bio%geometry%zht - crop height (m)
    !     bio%database%zloc_regrow - location of regrowth point (+ on stem, 0 or
    !                     negative from crown at or below surface) (m)
    !     bio%database%zmrt - maximum root depth
    !     bczmxc - maximum potential plant height (m)
    !     bio%geometry%zrtd  - crop root depth (m)
    !     bio%geometry%zshoot - length of actively growing shoot from root biomass (m)
    !     bio%deriv%mbgz - residue amount by soil layer
    !     ctrl%cropstress%ahfwsf - water stress factor ratio (0-1).  this is read in daily.
    !     soils%spp%ahtsmn - daily minimum soil temperature by layer (deg.c)
    !     bhtsmx - daily maximum soil temperature by layer (deg.c). currently
    !              not used.
    !     bhzeta (ahzeta) - actual evapotranspiration (mm/day). currently
    !              not used.
    !     bhzpta (ahzpta) - actual plant transpiration (mm/day). currently
    !              not used.
    !     bhzptp (ahzptp) - potential plant transpiration (mm/day). currently
    !              not used.
    !     bio%growth%am0cgf (am0cgf) - flag to run crop growth if between plant and
    !                       harvest. currently not used.
    !     bio%growth%am0cif - flag to initialize crop at start of planting
    !     bnslay - number of soil layers
    !     prevbio%mass%stemz (prevbgstemz) - crop stem mass below soil surface by
    !                    layer (kg/m^2)
    !     prevbio%growth%tchillucum (prevchillucum) - accumulated chilling units (days)
    !     prevbio%growth%dayap - number of days of growth completed since crop planted
    !     prevbio%mass%flatleaf (prevflatleaf)  - crop flat leaf mass (kg/m^2)
    !     prevbio%mass%flatstem (prevflatstem)  - crop flat stem mass (kg/m^2)
    !     prevbio%mass%flatstore (prevflatstore) - crop flat storage mass (kg/m^2)
    !     prevbio%geometry%grainf (prevgrainf) - internally computed grain fraction of
    !                                reproductive mass
    !     prevbio%geometry%zht (prevht) - crop height (m)
    !     prevbio%growth%thucum (prevhucum) - crop accumulated heat units
    !     prevbio%growth%fliveleaf (prevliveleaf) - fraction of standing plant leaf
    !                                    which is living (transpiring)
    !     prevbio%growth%mshoot (prevmshoot) - mass of shoot growing from root storage
    !                                biomass (kg/m^2)
    !     prevbio%growth%mtotshoot (prevmtotshoot) - total mass of shoot growing from
    !                                      root storage biomass (kg/m^2) in
    !                                      the period from beginning to
    !                                      completion of emergence heat units.
    !                                      currently not used.
    !     prevbio%mass%rootfiberz (prevrootfiberz) - crop root fibrous mass by soil
    !                                        layer (kg/m^2)
    !     prevbio%mass%rootstorez (prevrootstorez) - crop root storage mass by soil
    !                                        layer (kg/m^2) (tubers (potatoes,
    !                                        carrots), extended leaf (onion),
    !                                        seeds (peanuts))
    !      prevbio%geometry%zrtd (prevrtd) - crop root depth (m)
    !     prevbio%growth%trthucum (prevrthucum) - crop accumulated heat units with no
    !                                  vernalization/photoperiod delay
    !     prevbio%mass%standleaf (prevstandleaf) - crop standing leaf mass (kg/m^2)
    !     prevbio%mass%standstem (prevstandstem) - crop standing stem mass (kg/m^2)
    !     prevbio%mass%standstore (prevstandstore) - crop standing storage mass
    !                                        (kg/m^2)(head with seed, or
    !                                        vegetativehead (cabbage,
    !                                        pineapple))
    !     prevbio%geometry%dstm (prevstm) - number of crop stems per unit area (#/m^2). it
    !                          is computed by taking the tillering factor
    !                          times the plant population density.
    !     prevbio%geometry%zshoot (prevzshoot) - length of actively growing shoot from
    !                                root biomass (m)
    !     bs0ph  - soil ph
    !     bsdblk - bulk density of a layer (g/cm^3=t/m^3)
    !     bsfcce - calcium carbonate (%)
    !     bsfcec - cation exchange capacity (cmol/kg)
    !     bsfcla - % clay
    !     bsfom - percent organic matter
    !     bsfsmb - sum of bases (cmol/kg)
    !     bsftan - total available n in a layer from all sources (kg/ha)
    !     bsftap - total available p in a layer from all sources (kg/ha)
    !     bsmno3 - amount of applied n (t/ha)
    !     soils%spp%aszlyd(1) - depth from top of soil to botom of layer, m
    !     bszlyt (aszlyt) - soil layer thicknesses for each subregion (mm)
    !     cropres%dstm - number of crop stems per unit area (#/m^2). it is computed
    !              by taking the tillering factor times the plant population
    !              density.
    !     cropres%grainf - internally computed grain fraction of reproductive mass
    !     cropres%bgstemz - crop buried stem mass by layer (kg/m^2)
    !     cropres%flatleaf - crop flat leaf mass (kg/m^2)
    !     cropres%flatstem - crop flat stem mass (kg/m^2)
    !     cropres%flatstore - crop flat storage mass (kg/m^2)
    !     cropres%standleaf - crop standing leaf mass (kg/m^2)
    !     cropres%standstem - crop standing stem mass (kg/m^2)
    !     cropres%standstore - crop standing storage mass (kg/m^2)
    !                     (head with seed, or vegetative head (cabbage,
    !                     pineapple))
    !     cropres%xstmrep - a representative diameter so that
    !                 acdstm*acxstmrep*aczht=acrsai
    !     cropres%zht - crop height (m)
    !     bweirr (aweirr) - daily global radiation (mj/m^2)
    !     bwtdmn - daily minimum air temperature (deg.c)
    !     bwtdmx - daily maximum air temperature (deg.c)
    !     bwzdpt (awzdpt) - daily precipitation (mm). currently not used.
    !     bio%database%fd1(2) - fraction of biomass lost each day due to frost
    !     io%database%fd2(2) - fraction of biomass lost each day due to frost
    !     daysim - day of the simulation
    !     bio%upgm%growth_stress - flag setting which turns on water or temperature
    !                   stress (or both)
    !                   bio%upgm%growth_stress = 0  ! no stress values applied
    !                   bio%upgm%growth_stress = 1  ! turn on water stress
    !                   bio%upgm%growth_stress = 2  ! turn on temperature stress
    !                   bio%upgm%growth_stress = 3  ! turn on both
    !     debe: because it is now being read in, it is commented out in command.inc
    !     + + + local variable definitions + + +
    !     dd - the current day
    !     hu_delay - combined reduction in heat unit accummulation
    !     lay - this is a counter
    !     mm - the current month
    !     photo_delay - reduction in heat unit accumulation based on
    !                   photoperiod
    !     pot_leaf_mass - potential leaf mass which could be released for
    !                     regrowth.
    !     pot_stems - potential number of stems which could be released for
    !                 regrowth
    !     root_store_rel - root storage which could be released for regrowth
    !     trend - test computation for trend direction of living leaf area
    !     vern_delay - reduction in heat unit accumulation based on
    !                  vernalization
    !     yy - the current year

    !     + + + local parameter definitions + + +
    !     chilluv - total of chill units require for vernalization (deg c)
    !     shoot_delay - number of days minimum temperature must be above base
    !                   crop growth temperature for shoot growth to occur
    !     verndelmax - maximum value of vernalization delay parameter
    !                  (see actverndel definition in include file)
    !     dev_floor - minimum development rate fraction allowed (
    !                 1-full rate, 0-no development)
    !     max_photo_per - photo period where maximum development rate occurs
    !                     (hours)
    !     spring_trig - heat units ratio to spring allowing release of
    !                   winter annual crown storage

    !     + + + common block variables definitions + + +
    !     a_fr - parameter in the frost damage s-curve
    !     am0cfl - flag to print crop submodel output
    !     ap - total available p in a layer from all sources (kg/ha)
    !     b_fr - parameter in the frost damage s-curve
    !     civilrise - parameter. solar altitude angle defined as civil
    !                 twilight
    !     hrlt - day length on day i (h)
    !     huirt - heat unit index used to drive root growth (no delays)
    !     hrlty - day length on day (i-1)
    !     hui - heat unit index (ratio of acthucum to acthum)
    !     huirty - heat unit index for root expansion (ratio of actrthucum to
    !              acthum) on day (i-1)
    !     huiy - heat unit index (ratio of acthucum to acthum) on day (i-1)
    !     jd - today's julian date calander
    !     mgtokg - parameter (mgtokg = 0.000001); to convert milligrams to
    !              kilograms, multiply by 0.000001
    !     rsd - current amount of residue (t/ha) in a layer
    !     wn - organic n concentration of humus (g/t)
    !     wno3 - total available n in a layer from all sources (kg/ha)
    !     wp - organic p concentration of humus (g/t)
    !     xlat - latitude of a location (deg.)

    !     + + +  newly added arguments definitions + + +
    !     bio%upgm%aepa - the parameter for duration of anthesis (i.e., gdd from start
    !            to end of anthesis.
    !     bio%upgm%aifs - awn initials formed growth stage for spring barley and winter
    !            barley. this array includes daynum, year, month and day of
    !            when this stage was reached.
    !     bio%upgm%antes - end of anthesis growth stage for hay millet, proso millet,
    !             spring barley, spring wheat, sunflower, winter barley and
    !             winter wheat. this array includes daynum, year, month and
    !             day of when this stage was reached.
    !     bio%upgm%antss - start of anthesis growth stage for hay millet, proso millet,
    !             sorghum (first bloom), spring barley, spring wheat,
    !             sunflower, winter barley and winter wheat. also, dry beans
    !             and corn.this array includes daynum, year, month and day
    !             of when this stage was reached.
    !     bio%upgm%blstrs - blister growth stage in corn. this array includes daynum,
    !              year, month and day of when this stage was reached.
    !     bio%upgm%boots - booting growth stage for hay millet, proso millet, spring
    !             barley, spring wheat, winter barley and winter wheat. this
    !             array includes daynum, year, month and day of when this
    !             stage was reached.  booting is defined as flag leaf has
    !             completed its growth.
    !     bio%upgm%browns - when the back of the sunflower head is yellow and there
    !              may be some brown spotting. this array includes daynum,
    !              year, month and day of when this stage was reached.
    !     bio%upgm%callgdd - a flag to switch between methods for determining gdd.
    !               if the flag is set to true then gddcalc subroutine is
    !               called. otherwise, the code within crop is used.
    !     bio%upgm%canht - holds the final canopy height of the crop for the current day.
    !     canhty - yesterday's canopy height value.
    !     bio%upgm%canopyflg - a flag to indicate if the weps/upgm method to calculate
    !                 plant height will be used. value will then be 0. if using
    !                 the phenologymms method, the value will be 1.
    !     ctrl%sim%cliname - the name of the location for the climate data
    !     cname - bio%bname passed to phenol.
    !     clidat%co2atmos - the atmospheric level of CO2.
    !     co2eff - the effect on the plant of the read-in atmospherice co2 level.
    !              The adjustment factor
    !     bio%upgm%co2x - the CO2 levels in ppm. The x axis on the relationship curve.
    !     bio%upgm%co2y - the relative effect at different levels of CO2, i.e. bio%upgm%co2x.
    !     bio%upgm%cots - cotyledonary and unifoliolate leaves are visible in dry
    !            beans. this array includes daynum, year, month and day
    !            of when this stage was reached.
    !     bio%bname - name of the crop
    !     daa - days after anthesis
    !     dae - days after emergence
    !     dap - days after planting
    !     dav - days after vernalization
    !     bio%upgm%dayhtinc - the increase in plant height for today.
    !     daynum - day number of the year
    !     ddae - array holding the dae value for each growth stage
    !     ddap - array holding the dap value for each growth stage
    !     ddav - array holding the dav value for each growth stage
    !     bio%upgm%dents - the dent growth stage in corn. this array includes daynum,
    !             year, month and day of when this stage was reached.
    !     dgdde - array holding the gdde value for each growth stage
    !     dgdds - array holding the gdds value for each growth stage
    !     dgddv - array holding the gddv value for each growth stage
    !     bio%upgm%doughs - the dough growth stage in corn. this array includes daynum,
    !              year, month and day of when this stage was reached.
    !     bio%upgm%drs - double ridge growth stage for hay millet, proso millet,
    !           spring barley, spring wheat, winter barley and winter wheat.
    !           this array includes daynum, year, month and day of when this
    !           stage was reached.
    !     bio%upgm%dummy2 - an array to hold the gdd values, both under stressed
    !              and non- stressed conditions,required to reach each growth
    !              stage of the current crop.
    !     bio%upgm%ears - the ear initiation stage in corn. this array includes daynum,
    !            year, month and day of when this stage was reached.
    !     bio%upgm%ecanht - this is the maximum canopy height of the crop in phase 1 of
    !              the canoy height growth.  this is usually from emergence to
    !              when the plant begins elongating stems but this stage varies
    !              among crops. it is an input parameter and is read in from upgm_crop.dat.
    !     bio%upgm%egdd - a 6 element array that holds the bio%upgm%ergdd values plus calculated values
    !            for two intermediate soil moisture level values in elements 2 and 4.
    !     elong - total elongation of the emerging seedling based on the
    !             day's gdd (mm)
    !     bio%upgm%emrgflg - a flag to determine if the new emerge subroutine should be
    !               called (bio%upgm%emrgflg=1) or to proceed with the weps/upgm method
    !               of achieving emergence (bio%upgm%emrgflg=0).
    !     bio%upgm%ems - day when emergence occurred in all crops. this array includes
    !           daynum, year, month and day of when this event occurred.
    !     bio%upgm%endlgs - end of leaf growth stage in sorghum. this array includes
    !              daynum, year, month and day of when this stage was reached.
    !     endphenol - a flag to indicate if this subroutine should be called
    !                 again the next day.
    !     bio%upgm%epods - one pod has reached the maximum length in dry beans.
    !             this array includes daynum, year, month and day of when
    !             this stage was reached.
    !     bio%upgm%ergdd - an array holding 4 elongation rates in mm per gdd
    !             based on each soil moisture description.
    !     bio%upgm%eseeds - there is one pod with fully developed seeds in dry
    !              beans. this array includes daynum, year, month and day
    !              of when this stage was reached.
    !     bio%upgm%first7 - used to set the value of bio%upgm%aepa the first time phenolww is
    !              called.
    !     bio%upgm%fps - flower primordium initiation growth stage. this array includes
    !           daynum, year, month and day of when this stage was reached.
    !     bio%upgm%fullbs - full bloom growth stage in sorghum. this array includes
    !            daynum, year, month and day of when this stage was reached.
    !     gdda - sum of growing degree days since anthesis
    !     gddday - the number of gdd with 0°C base temperature for that day.
    !     gdde - sum of growing degree days since emergence
    !     gdds - sum of growing degree days since seeding
    !     bio%upgm%gddtbg - used to accumulate gdd for seeds planted in dust after a
    !              rainfall event has moved the soil moisture condition to
    !              dry.  the seed can begin accumulating gdd's germination.
    !     gddv - sum of growing degree days since vernalization
    !     gddwsf - an array to hold the gn and gs gdd values plus the high
    !              and low water stress factor values.  these are used in
    !              calculating the slope of the line for each growth stage
    !              and this is then used to calculate the adjusted gdd value
    !              for the current growth stage.
    !              column one contains the gn values and is y2.
    !              column two contains the gs values and is y1.
    !              column three contains wsfhi (high water stress) and is x1.
    !              column four contains wsflo (low water stress) and is x2.
    !              column five contains the adjgdd value for the stage.
    !     bio%upgm%germgdd - an array holding 4 germination times in gdd at base 0°c
    !               for the soil moisture levels
    !     bio%upgm%germs - simulated day that germination occurs
    !     bio%upgm%ggdd - a 6 element array that holds the bio%upgm%germgdd values plus calculated values for
    !           two intermediate soil moisture level values in elements 2 and 4.
    !     bio%upgm%gmethod - selects the method whereby gdd will be calculated.
    !               a value of 1 corresponds to method 1 in phenologymms and
    !               is used for crops such as winter wheat, winter barley and
    !               proso millet. a value of 2 corresponds to method 2 in
    !               phenologymms and is used for crops such as corn, sorghum
    !               and sunflower.  a value of 3 is the way that weps/upgm
    !               calculated ggd for the day.
    !     bio%upgm%gpds - growing point differentiation growth stage in sorghum. this
    !            array includes daynum, year, month and day of when this stage
    !            was reached.
    !     bio%upgm%halfbs - half bloom growth stage in sorghum. this array includes
    !            daynum, year, month and day of when this stage was reached.
    !     bio%upgm%heads - heading growth stage for hay millet, proso millet, spring
    !             barley, spring wheat, winter barley and winter wheat. this
    !             array includes daynum, year, month and day of when this
    !             stage was reached.
    !     bio%upgm%hrs - time to harvest ripe growth stage for corn, hay millet, proso
    !           millet, sorghum, spring barley, spring wheat, sunflower,
    !           winter barley and winter wheat. 80% of pods are at the mature
    !           color in dry beans. this array includes daynum, year, month
    !           and day of when this stage was reached.
    !     ctrl%sim%icli - a flag to determine which type of weather file to read.  a
    !            value of 1 indicates that climate data should be read from
    !            the cligen weather file.  a value of 0 indicates that a
    !            historical climate file will be used.
    !     bio%upgm%ies - start of internode elongation growth stage for corn, hay
    !           millet, proso millet, sorghum, spring barley, spring wheat,
    !           winter barley, and winter wheat. for sunflower, this stage
    !           occurs when the internode below the inflorescence elongates
    !           0.5 to 2.0 cm above the nearest leaf on the stem. this array
    !           includes daynum, year, month and day of when this stage was
    !           reached.
    !     bio%upgm%ies2 - for sunflower, this is when the internode below the
    !            inflorescence continues lengthening and lifts the head above
    !            the surrounding leaves more than 2 cm. this array includes
    !            daynum, year, month and day of when this stage was reached.
    !     bio%upgm%infls - the sunflower inflorescence becomes visible. this array
    !             includes daynum, year, month and day of when this stage was
    !             reached.
    !     jan1 - a flag to test if january 1 has occurred.  if it has passed,
    !            then the winter annual crop is assumed to have completed
    !            vernalization.
    !     bio%upgm%joints - jointing growth stage for hay millet, proso millet,
    !              sorghum, spring barley, spring wheat, winter barley and
    !              winter wheat. this array includes daynum, year, month and
    !              day of when this stage was reached.
    !     bio%upgm%lf1s - stage when the first trifoliolate leaf is unfolded in dry
    !            beans. this array includes daynum, year, month and day of
    !            when this stage was reached.
    !     bio%upgm%lf12s - the 12 leaf growth stage for corn and sunflower. this array
    !             includes daynum, year, month and day of when this stage was
    !             reached.
    !     bio%upgm%lf2s - stage when the second trifoliolate leaf is unfolded in dry
    !            beans. this array includes daynum, year, month and day of
    !            when this stage was reached.
    !     bio%upgm%lf3s - stage when the third trifoliolate leaf is unfolded in dry
    !            beans. this array includes daynum, year, month and day of
    !            when this stage was reached.
    !     bio%upgm%lf4s - the 4 leaf growth stage for corn and sunflower and the
    !            stage when the fourth trifoliolate leaf is unfolded in dry
    !            beans. this array includes daynum, year, month and day of
    !            when this stage was reached.
    !     bio%upgm%lf8s - the 8 leaf growth stage for sunflower. this array includes
    !            daynum, year, month and day of when this stage was reached.
    !     lnarray - an array to hold the leaf number calculated for each day
    !     lncntr - counter for the leafno subroutine
    !     lnpout - an array used in writing out daynum and the number of
    !              leaves on that day. the values are written each time a new
    !              leaf has appeared, i.e. when the integer version of the
    !              real leaf number has incremented.
    !     bio%upgm%mats - physiological maturity growth stage for corn, hay millet,
    !            proso millet, sorghum, spring barley, spring wheat,
    !            sunflower, winter barley and winter wheat. in dry beans,
    !            one pod has changed color/striped. this array includes
    !            daynum, year, month and day of when this stage was
    !            reached.
    !     bio%upgm%maxht - this is the maximum canopy height of the crop.  it is an
    !             input parameter and is read in from upgm_crop.dat.
    !     bio%upgm%mffls - the stage of mid to full flower in dry beans. this array
    !             includes daynum, year, month and day of when this stage
    !             was reached.
    !     bio%upgm%milks - the milk growth stage in corn. this array includes daynum,
    !             year, month and day of when this stage was reached.
    !     bio%upgm%mpods - the stage when 50% of the pods are at the maximum length.
    !             this array includes daynum, year, month and day of when
    !             this stage was reached.
    !     bio%upgm%mseeds - the stage when 50% of the pods have fully developed seeds
    !              in dry beans. this array includes daynum, year, month and
    !              day of when this stage was reached.
    !     bio%upgm%opens - the sunflower inflorescence begins to open. this array
    !             includes daynum, year, month and day of when this stage
    !             was reached.
    !     bio%upgm%pchron - phyllochron value which is the number of gdd per leaf.
    !     ctrl%mngt%pd - planting day
    !     pdate - day of year planting can occur
    !     ctrl%mngt%pm - planting month
    !     ctrl%mngt%py - planting year.  currently, not the calendar year.
    !     rowcntr - a counter for the rows in an array
    !     bio%upgm%seedsw - soil water content at seed depth.  it is read in as
    !              optimum, medium, dry or planted in dust and converted
    !              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
    !              4 = planted in dust
    !     bio%upgm%silks - the silking growth stage in corn. this array includes
    !             daynum, year, month and day of when this stage was reached.
    !     bio%upgm%soilwat - an array holding the swtype for each soil moisture
    !               condition
    !     bio%upgm%srs - single ridge growth stage for hay millet, proso millet,
    !           spring barley, spring wheat, winter barley and winter wheat.
    !           this array includes daynum, year, month and day of when this
    !           stage was reached.
    !     bio%upgm%tbase - lowest temperature below which no growth occurs (deg.c).
    !     tempsw - a new variable to designate the array subscripts for the new 6 element
    !              arrays: bio%upgm%egdd, bio%upgm%ggdd used in emerge.
    !     bio%upgm%tis - start of tillering growth stage for corn, hay millet, proso
    !           millet, sorghum, spring barley, spring wheat, winter barley
    !           and winter wheat. this array includes daynum, year, month and
    !           day of when this stage was reached.
    !     todayln - the value of the current day's leaf number.
    !     bio%upgm%toptlo - the lower temperature in the optimum range for plant
    !              growth (deg.c).
    !     bio%upgm%toptup - the upper temperature in the optimum range for plant
    !              growth (deg.c).
    !     bio%upgm%tsints - tassel initiation growth stage in corn. this array
    !              includes daynum, year, month and day of when this stage
    !              was reached.
    !     bio%upgm%tss - terminal spikelet growth stage for spring and winter wheat.
    !           this array includes daynum, year, month and day of when this
    !           stage was reached.
    !     bio%upgm%tupper - upper/maximum temperature for plant growth (deg.c).
    !              no growth with temperatures above this point.
    !     verns - sum of gdd after vernalization. currently not used.
    !     bio%upgm%wfpslo - an array holding the low values for each soil moisture
    !              condition.
    !     bio%upgm%wfpsup - an array holding the high values for each soil moisture
    !              condition.
    !     bio%upgm%yelows - back of the sunflower head is a light yellow. this array
    !              includes daynum, year, month and day of when this stage
    !              was reached.
    !     yestln - the value of yesterday's leaf number
    !     yr - year

    !     + + + new local variable definitions + + +
    !     bio%upgm%phenolflg - a flag that determines if the upgm/weps method of determining maturity
    !                 will be used (bio%upgm%phenolflg =0) or the phenologymms method will be used (bio%upgm%phenolflg = 1).
    !     tavgtemp - the variable that holds the average of tmaxtemp and
    !                tmintemp.
    !     tmaxtemp - the variable that is set equal to bio%database%tmin if the day's
    !                maximum air temperature is less than the base
    !                temperature. also, it is set equal to bio%upgm%tupper if the day's
    !                maximum air temperature is greater than the
    !                upper/maximum temperature.
    !     tmintemp - the variable that is set equal to bio%database%tmin if the day's
    !                minimum air temperature is less than the base temperature.
    !                also, it is set equal to bio%upgm%tupper if the day's minimum air
    !                temperature is greater than the upper/maximum temperature.

    !     + + + global common blocks + + +

    !     + + + common blocks + + +

    !     + + + subroutines called + + +
    !     caldatw
    !     canopyht
    !     chillu
    !     cinit
    !     huc1 - this is a function
    !     gddcalc
    !     growth
    !     leafno
    !     npcy - this is not called
    !     phenol
    !     scrv1
    !     shoot_grow
    !     shootnum
    !
    !     + + + function declarations + + +

    !     + + + end of specifications + + +

    !print*,'in crop before doing stuff, bio%upgm%seedsw = ', bio%upgm%seedsw, 'daysim = ', daysim
    !debe cname is not set equal to bio%bname anywhere. added that so that the
    !correct bio%bname will get passed to phenol.for.

    ! debe added bio%upgm%phenolflg