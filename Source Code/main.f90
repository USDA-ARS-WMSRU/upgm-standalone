    program main
    use upgm_simdata
    use constants, only : int32, dp, mnbpls, mncz
    use datetime, only : julday, date
    use climate
    use soil
    use biomaterial
    use nitrogen
    !
    implicit none
    !
    ! State for the program execution
    !
    type(biomatter) :: bio,prevbio
    type(biomatter), dimension(mnbpls) ::  residue
    type(biototal) :: biotot
    type(soildata) :: soils
    type(climate_data) :: clidat
    type(controls) :: ctrl
    !
    ! Locals to MAIN for reading in setup files and so on.    
    !
    type(date) :: startdate
    type(date) :: enddate
    integer(int32) :: i  = 0
    integer(int32) :: row = 4
    character(len=40) :: seedbed = ''
    integer(int32)  :: offsetforfiles = 0
    real(dp)       :: current_depth = 0
    integer(int32) :: max_depth = 1
    integer(int32) :: io = 0
    integer(int32) :: upgmflg
    !
    ! Start Main
    !
    ! open input files
    ctrl = initialize_ctrl(offsetforfiles)
    call open_inputfiles(ctrl)

    ! calculate the max profile depth
    max_depth = 0
    do
        ! start max depth at 1
        max_depth = max_depth + 1
        read (ctrl%handles%soilprofile, *, IOSTAT=io) soils%spp%aszlyt(max_depth)
        if (io > 0) then
            ! error, exit
            exit
        else if (io < 0) then
            ! eof, exit
            exit
        else
            ! thickness for layer max_depth read in,
            ! accumulate into current_depth
            current_depth = current_depth +  soils%spp%aszlyt(max_depth)
            ! set depth for max_depth to be current_depth
            soils%spp%aszlyd(max_depth) = current_depth
        end if
    enddo

    !
    ! number of soil layers
    !
    soils%spp%nslay = max_depth - 1 !Overshoot by 1 in read soil profile loop

    bio = create_biomatter(soils%spp%nslay, mncz)
    do i=1, mnbpls
        residue(i) = create_biomatter(soils%spp%nslay, mncz)
    end do
    i = 0
    prevbio = create_biomatter(soils%spp%nslay, mncz)
    biotot = create_biototal(soils%spp%nslay, mncz)
    clidat%co2atmos = 0.0
    call cropinit(ctrl,soils,bio, biotot)

    ctrl%sim%growcrop_flg = .false.
    bio%growth%am0cif = .false.         ! flag to initialize crop initialization routines (set to true on planting date)
    bio%growth%am0cfl = 1               ! flag to specify if detailed (submodel) output file should be generated
    bio%growth%am0cgf = .false.         ! supposed to indicate a growing crop
    ctrl%sim%am0hrvfl = 0                        ! harvest flag (default is off)
    bio%geometry%xrow = 0.2286          ! row spacing (m)
    ctrl%cropstress%ahfwsf = 1.0        ! water stress factor
    !
    ! assign values to some soil layer properties
    !
    soils%scp%asfcce(1) = 0.0
    soils%scp%asfcec(1) = 0.0
    soils%scp%asfom(1) = 3.0
    soils%scp%asftan(1) = 0.0
    soils%scp%asftap(1) = 0.0
    soils%scp%asmno3 = 0.0
    soils%spp%asfcla(1) = 20.0
    bio%deriv%mbgz = 0.0
    soils%spp%asdblk(1) = 1.0
    !
    soils%spp%ahtsmn(1) = 22.0
    !
    ctrl%sim%amalat = -38.0
    !
    ctrl%sim%am0cdb = 1        ! set crop debug output flag (default to no output)
    !
    bio%growth%thucum = 0.0 ! initialize accumulated heat units
    bio%deriv%mst = 0.0    ! initialize total standing crop mass
    bio%deriv%mrt = 0.0    ! initialize total root crop mass
    !
    ! read in plant parameters from cropxml.dat
    !
    read (ctrl%handles%cropxml,' (a80) ') bio%bname
    !debe set cropname to the proper form so it can be used in the phenol and
    ! canopyht subroutines and not need to be changed in either one.
bio%bname = trim(bio%bname)

    if (bio%bname=='corn') then
        bio%bname = 'corn'
    else if (bio%bname=='drybeans') then
        bio%bname = 'dry beans'
        !debe added the following for hay millet.  the crop parameters are for
        ! pearl millet, forage.  this is the only forage millet in the crop
        ! parameters file.
    else if (bio%bname=='milletpearlforage') then
        bio%bname = 'hay millet'
    else if (bio%bname=='milletfoxtailseed') then
        bio%bname = 'hay millet'
    else if (bio%bname=='milletprosograin') then
        bio%bname = 'proso millet'
    else if (bio%bname=='sorghum') then
        bio%bname = 'sorghum'
    else if (bio%bname=='soybean') then
        bio%bname = 'soybean'
    else if (bio%bname=='barleyspring') then
        bio%bname = 'spring barley'
    else if (bio%bname=='wheatspring') then
        bio%bname = 'spring wheat'
    else if (bio%bname=='sunflower') then
        bio%bname = 'sunflower'
    else if (bio%bname=='barleywinter') then
        bio%bname = 'winter barley'
    else if (bio%bname=='wheatwinter') then
        bio%bname = 'winter wheat'
    end if
    read (ctrl%handles%cropxml,*) bio%geometry%dpop,bio%database%dmaxshoot,bio%database%baflg,bio%database%ytgt,bio%database%baf,bio%database%yraf,    &
        & bio%geometry%hyfg,bio%database%ynmu
    read (ctrl%handles%cropxml,*) bio%database%ywct,bio%database%ycon,bio%database%idc,bio%database%grf,bio%database%ck,bio%database%ehu0,bio%database%zmxc, &
        & bio%database%growdepth
    read (ctrl%handles%cropxml,*) bio%database%zmrt,bio%database%tmin,bio%database%topt,bio%database%thudf,bio%database%tdtm,bio%database%thum,        &
        & bio%database%fd1(1),bio%database%fd2(1)
    read (ctrl%handles%cropxml,*) bio%database%fd1(2),bio%database%fd2(2),bio%database%tverndel,bio%database%bceff,bio%database%alf,bio%database%blf&
        & ,bio%database%clf,bio%database%dlf
    read (ctrl%handles%cropxml,*) bio%database%arp,bio%database%brp,bio%database%crp,bio%database%drp,bio%database%aht,bio%database%bht,bio%database%ssa&
        & ,bio%database%ssb
    read (ctrl%handles%cropxml,*) bio%database%sla,bio%database%hue,bio%database%transf,bio%database%diammax,bio%database%storeinit,      &
        & bio%database%shoot,bio%database%fleafstem,bio%database%fshoot
    read (ctrl%handles%cropxml,*) bio%database%fleaf2stor,bio%database%fstem2stor,bio%database%fstor2stor,bio%database%rbc,            &
        & bio%database%dkrate(1),bio%database%dkrate(2),bio%database%dkrate(3),bio%database%dkrate(4)
    read (ctrl%handles%cropxml,*) bio%database%dkrate(5),bio%database%xstm,bio%database%ddsthrsh,bio%database%covfact,bio%database%resevapa,    &
        & bio%database%resevapb,bio%database%yld_coef,bio%database%resid_int
    !
    !read management information from upgm_mgmt.dat. currently it includes
    ! starting and ending day, month, and year for planting and harvest.
    !
    read (ctrl%handles%upgmmgt,*) startdate%day,startdate%mon,startdate%year,enddate%day,enddate%mon,enddate%year
    read (ctrl%handles%upgmmgt,*) ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py,ctrl%mngt%hd,ctrl%mngt%hm,ctrl%mngt%hy

    ctrl%sim%start_jday = julday(startdate%day,startdate%mon,startdate%year)

    ctrl%sim%end_jday = julday(enddate%day,enddate%mon,enddate%year)

    ctrl%sim%plant_jday = julday(ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py)
    ctrl%sim%harvest_jday = julday(ctrl%mngt%hd,ctrl%mngt%hm,ctrl%mngt%hy)

    print *,'crop=',bio%bname

    ! ***** emergence *****
    !
    !debe added canopyflg
    !read in canopyflg and emergence data for the crop from upgm_crop.dat.
    !debe added reading in phenolflg from upgm_crop.dat

    read (ctrl%handles%upgmcrop,*) bio%upgm%canopyflg,bio%upgm%emrgflg,bio%upgm%phenolflg
    if ((bio%upgm%canopyflg .EQ. 1) .OR. (bio%upgm%emrgflg .EQ. 1) .OR. (bio%upgm%phenolflg .EQ. 1)) then
        read (ctrl%handles%upgmcrop,*) seedbed
        if (seedbed=='Optimum') then
            bio%upgm%seedsw = 1          !set seedsw = to a real number. changed back to integer 2/23/11
        else if (seedbed=='Medium') then
            bio%upgm%seedsw = 2
        else if (seedbed=='Dry') then
            bio%upgm%seedsw = 3
        else if (seedbed=='Plantedindust') then
            bio%upgm%seedsw = 4
        end if

        ! force All flags to be either 0 or 1. nathan 8/13/2015
        if(bio%upgm%canopyflg==0.and.bio%upgm%emrgflg==0.and.bio%upgm%phenolflg==0) then
            upgmflg = 0
        else
            upgmflg = 1
            bio%upgm%canopyflg = 1
            bio%upgm%emrgflg = 1
            bio%upgm%phenolflg = 1
        end if

        ! RMarquez 06.08.2017 -> Added in check to switch UPGM off if a user chooses a crop that is not supported.
        if(upgmflg == 1 .and. index("corn,sorghum,dry beans,spring barley,sunflower," //&
            "winter barley,hay millet,proso millet,spring wheat,winter wheat,soybean" &
            ,trim(bio%bname)) == 0) then
        upgmflg = 0
        bio%upgm%canopyflg = 0
        bio%upgm%emrgflg = 0
        bio%upgm%phenolflg = 0
        end if


        print *,'seedbed = ',seedbed
        print *,'canopyflg = ',bio%upgm%canopyflg,'emrgflg = ',bio%upgm%emrgflg,'phenolflg = ',bio%upgm%phenolflg
        !, 'seedsw = ', seedsw        
        !
        ! put these values into 5 one dimensional arrays.
        do i = 1,row
            read (ctrl%handles%upgmcrop,*) bio%upgm%soilwat(i)          !swtype = soil moisture condition.
            read (ctrl%handles%upgmcrop,*) bio%upgm%wfpslo(i)           !wlow = lower range of soil moisture
            read (ctrl%handles%upgmcrop,*) bio%upgm%wfpsup(i)            !wup = upper range of soil moisture
            read (ctrl%handles%upgmcrop,*) bio%upgm%germgdd(i)          !germd = gdd's for germination at soil moisture
            read (ctrl%handles%upgmcrop,*) bio%upgm%ergdd(i)          !elrate = elongation for emergence

        end do

        !
        !
        ! ***** phenology *****
        !
        !the following is read in whether leaf number or gdd is used.
        ! read in phenology parameters and 4 temperature values from upgm_crop.dat.
        read (ctrl%handles%upgmcrop,*) bio%upgm%pchron
        read (ctrl%handles%upgmcrop,*) bio%upgm%tbase
        read (ctrl%handles%upgmcrop,*) bio%upgm%toptlo
        read (ctrl%handles%upgmcrop,*) bio%upgm%toptup
        read (ctrl%handles%upgmcrop,*) bio%upgm%tupper

        ! read in method of calculating gdd (gmethod) from upgm_crop.dat
        read (ctrl%handles%upgmcrop,*) bio%upgm%gmethod

        !debe added reading in maxht value for canopy height subroutine.
        !debe added ecanht for height in phase 1 of canopy height.
        read (ctrl%handles%upgmcrop,*) bio%upgm%maxht,bio%upgm%ecanht
        print *,'maxht = ',bio%upgm%maxht,'ecanht = ',bio%upgm%ecanht

        !debe added reading in growth_stress to set which kind of stress is to be used:
        ! 0=no stress, 1=water stress only, 2=temp stress only,
        ! 3=min of water and temp stress.
        read (ctrl%handles%upgmcrop,*) bio%upgm%growth_stress
        print *,'growth_stress = ',bio%upgm%growth_stress
        !debe changed dimensions of dummy1 and dummy2 to allow both non-stresseed
        ! and stressed values to be read in.
        do i = 1,32
            read (ctrl%handles%upgmcrop,*) bio%upgm%dummy1(i),bio%upgm%dummy2(i)
            if (bio%upgm%dummy1(i)=='ln'.or.bio%upgm%dummy1(i)=='ls') bio%upgm%dummy2(i) = bio%upgm%dummy2(i)*bio%upgm%pchron
        end do
    end if

        !DE 5/18/18 add calculation of thum to be used when upgmflg = 1
    if (upgmflg==1) then
        if (bio%bname=='corn') then
            bio%database%thum = bio%upgm%dummy2(2)+bio%upgm%dummy2(6)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)  &
                  & +bio%upgm%dummy2(11)+bio%upgm%dummy2(12)+bio%upgm%dummy2(13)
        else if (bio%bname=='dry beans') then
            bio%database%thum = bio%upgm%dummy2(2)+bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)+bio%upgm%dummy2(11)           &
                  & +bio%upgm%dummy2(12)+bio%upgm%dummy2(13)
        !debe added the following for hay millet.  the crop parameters are for
        ! pearl millet, forage.  this is the only forage millet in the crop
        ! parameters file.
        else if (bio%bname=='hay millet') then
            bio%database%thum = bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        else if (bio%bname=='hay millet') then
            bio%database%thum = bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        else if (bio%bname=='proso millet') then
            bio%database%thum = bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        else if (bio%bname=='sorghum') then
            bio%database%thum = bio%upgm%dummy2(5)+bio%upgm%dummy2(6)+bio%upgm%dummy2(7)+bio%upgm%dummy2(10)
        else if (bio%bname=='soybean') then
            bio%database%thum = bio%upgm%dummy2(2)+bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)+bio%upgm%dummy2(11)           &
                  & +bio%upgm%dummy2(12)+bio%upgm%dummy2(13)+bio%upgm%dummy2(14)+bio%upgm%dummy2(15)
        else if (bio%bname=='spring barley') then
            bio%database%thum = bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        else if (bio%bname=='spring wheat') then
            bio%database%thum = bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        else if (bio%bname=='sunflower') then
            bio%database%thum = bio%upgm%dummy2(2)+bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)+bio%upgm%dummy2(11)           &
                  & +bio%upgm%dummy2(12)+bio%upgm%dummy2(13)
        else if (bio%bname=='winter barley') then
            bio%database%thum = bio%upgm%dummy2(2)+bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        else if (bio%bname=='winter wheat') then
            bio%database%thum = bio%upgm%dummy2(2)+bio%upgm%dummy2(3)+bio%upgm%dummy2(4)+bio%upgm%dummy2(5)+bio%upgm%dummy2(6)  &
                  & +bio%upgm%dummy2(7)+bio%upgm%dummy2(8)+bio%upgm%dummy2(9)+bio%upgm%dummy2(10)
        end if
    end if
    print *, 'cropname = ', bio%bname, 'thum = ', bio%database%thum

    !debe added for CO2 table
    do i = 1, 10
        read (ctrl%handles%upgmco2,*) bio%upgm%co2x(i),bio%upgm%co2y(i)
    end do

    call climate_init(ctrl,clidat,ctrl%sim%icli,ctrl%sim%cliname)    ! reads monthly and yearly climate variables

    if (bio%growth%am0cfl>0) then
        call open_outputfiles(ctrl)
        !debe added for canopy height output
        call cpout(ctrl, bio)                              ! print headings for crop output files
    end if
    !
    if (bio%growth%am0cfl>1) call fopenk(ctrl%handles%luoallcrop,'allcrop.prn','unknown')     ! main crop debug output file
    if (ctrl%sim%am0cdb>0) call fopenk(ctrl%handles%cdbugfile,'cdbug.out','unknown')       ! crop submodel debug output file
    !
   call upgm_driver(ctrl, clidat, soils, bio, residue, biotot, prevbio)

    ! RMarquez 6.21.2017 -> need to free all allocated memory.
    call destroy_biomatter(bio)
    do i=1, mnbpls
        call destroy_biomatter(residue(i))
    end do
    call destroy_biomatter(prevbio)
    call destroy_biototal(biotot)
    end program main

    
    
    
    
    
    !

    !
    !debe added these variables to be initialized. added canopyflg for determining
    ! which method of calculating canopy height will be used. added dayhtinc to get
    ! the daily increase in height when using the phenologymms method of calculating
    ! canopy height. debe added the CO2 variables to be initialized.
    !Rmarquez 2.10.17 -> Added read code for the new soil layer profile file.
    !       intent: provide actual profile layer data (depth, thickness) for the weps/upgm code
    !
    !set thickness and depth of first layer (defaulting to a value deeper than
    ! roots will reach)
    !
! ***** upgm/weps variables*****
!
! ***** newly added variables for seedling emergence *****
!         ***** submodel from phenologymms *****
!debe added declarations for seedbed, swtype, wlow, wup, germd, elrate
! variables read in from upgm_crop.dat and initialized in main. these
! variables were used to fill the following arrays which are used in
! emerge: ergdd(4), germgdd(4), wfpslo(4), wfpsup(4). the four arrays are
! initialized in cinit.
!debe added the variable emrgflg to switch between the old and the
! new way of doing emergence. the old way does not call the emerge
! subroutine and emrgflg has a value of 0. when emrgflg = 1, the emerge
! subroutine is called. this flag is set in upgm_crop.dat file and read in
! from that file. it is initialized in cropinit.
!debe added gddtbg to be used in emerge. it accumulates gdd for seeds
! planted in dust after a rainfall event has moved the soil moisture
! condition to dry. then the seed can begin to accumulate gdd to begin
! germination. it is initialized in cropinit.
!debe added the variable cropname to be used in emerge.  it is set equal
! to the upgm variable ac0nam(1)in main.
!debe added two new arrays of 6 elements to be used in emerge.for to
! enable adding values for germgdd and ergdd for two soil moisture
! levels that are half steps between dry and medium and medium and optimum.
!
! ***** newly added variables for the phenology *****
!       ***** submodel from phenologymms *****
!debe added these variables for phenology: dummy1(30), dummy2(30), pchron,
! first7 and aepa. they are initialized in cropinit.
!debe changed the dimensions of dummy1 and 2 arrays for stressed and
! non-stressed values. this was necessary to allow all gn and gs values to
! be included in the input file.  in phenologymms only gn or gs (or ln or
! ls) values are read in based on the user's selection. with no interface
! to allow the user to select the conditions (stressed or non-stressed and
! using gdd or number of leaves) values for both conditions must be made
! available in the file.  the variable dummy1 holds whether the condition
! is gn or gs. the variable dummy2 holds the gdd value for the growth
! stage.  number of leaves option for determining the growth stage is not
! currently implemented.
!
! ***** newly added variables for the gddcalc *****
!     ***** subroutine from phenologymms *****
! ***** also for canopy height *****
!
!debe added the variable for maximum upper temperature (tupper) for use in
! the gddcalc subroutine.
!debe 020409 added the variable callgdd to pass to cropinit for
! initilization. this is a flag to determine if the old way of calculating
! gdd will be used or if the gddcalc subroutine will be called. a value of
! .true. indicates the subroutine will be called.
!debe added gmethod to be read in from upgm_crop.dat
!debe add reading in the location climate name
!debe added ecanht for height of phase 1 of canopy growth.
!
!debe added in two variables for canopy height. added growth_stress so that
! it could be read in instead of setting it in the code. it is a weps/upgm variable.
!debe added more temperature variables (tbase, toptlo, toptup) for the
! different gdd calculation methods. these variables are those used in
! phenologymms code.
!debe added canopyflg to indicate whether the old way of determining plant height, i.e.
! weps/upgm method (0) will be used or the method form phenologymms (1).
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
 
!debe changed seedsw from integer to real so that soil moisture levels would
! be advanced upwards more slowly when a large precip event occurred prior
! to emergence. changed it back to integer because array subscripts should
! be integers.
!
!debe added three variables for CO2: co2x, co2y and c02atmos. co2eff was also added but
!appears only in these subroutines: crop.f90,cinit.f90,growth.f90
!
!Rmarquez 2.10.17 -> added new file and file read code for new soil profile file.
!   This file provides actual soil layer information for depth and thickness.
!
!locals
!
!     + + + argument definitions + + +
!
!     + + + weps/upgm local variable definitions + + +
!     chkid - read in the day from the water stress file.
!     chkim - read in the month from the water stress file.
!     chkiy - read in the year from the water stress file.
!     ed - the ending day read in from upgm_mgmt.dat.
!     em - the ending month read in from upgm_mgmt.dat.
!     end_jday - the day of the year for the ending date.
!     ey - the ending year read in from upgm_mgmt.dat.
!     growcrop_flg - flag to indicate if the crop should continue
!                    growing.
!     harvest_jday - the day of the year for the harvest date.
!     hd - the harvest day read in from upgm_mgmt.dat.
!     hm - the harvest month read in from upgm_mgmt.dat.
!     hy - the harvest year read in from upgm_mgmt.dat.
!     icli - used to determine if the climate file is generated or
!            historical.  if the value is 1, it is a cligen weather file.
!            if the value is not 1, it is a historical weather file.
!     id - the day in the date of the climate data read in.
!     im - the month in the date of the climate data read in.
!     iy - the year in the date of the climate data read in.
!     julday - a function which returns the julian day number.
!     mature_warn_flg - flag to indicate use of crop maturity warning.
!                0  - no crop maturity warning given for any crop.
!                1  - warnings generated for any crop unless supressed
!                     by crop type.
!     pd - the planting day read in from upgm_mgmt.dat.
!     plant_jday - the day of the year for the planting date.
!     pm - the planting month read in from upgm_mgmt.dat.
!     py - the planting year read in from upgm_mgmt.dat.
!     sd - the starting day read in from upgm_mgmt.dat.
!     sm - the starting month read in from upgm_mgmt.dat.
!     sr - used to access a cell in an array.
!     start_jday - the day of the year for the starting date.
!     sy - the starting year read in from upgm_mgmt.dat.
 
!     + + + common block variables definitions + + +
!     ac0aht - height s-curve parameter
!     ac0alf - leaf partitioning parameter
!     ac0arp - rprd partitioning parameter
!     ac0bceff - biomass conversion efficiency
!     ac0bht - height s-curve parameter
!     ac0blf - leaf partitioning parameter
!     ac0brp - rprd partitioning parameter
!     ac0ck - canopy light extinction coefficient (0.0 < ac0ck < 1.0)
!     ac0clf - leaf partitioning parameter
!     ac0crp - rprd partitioning parameter
!     ac0diammax - crop maximum plant diameter (m)
!     ac0dlf - leaf partitioning parameter
!     ac0drp - rprd partitioning parameter
!     ac0fd1 - xy coordinate for 1st pt on frost damage curve
!     ac0fd2 - xy coordinate for 2nd pt on frost damage curve
!     ac0growdepth - depth of growing point at time of planting (m)
!     ac0hue - relative heat unit for emergence (fraction)
!     ac0idc - the crop type number (1 = annual, perennial, . . .)
!     ac0nam - crop name
!     ac0shoot - mass from root storage required for each regrowth shoot
!                (mg/shoot) seed shoots are smaller and adjusted for
!                available seed mass
!     ac0sla - specific leaf area (cm^2/g)
!     ac0ssa - stem area to mass coefficient a, result is m^2 per plant
!     ac0ssb - stem area to mass coefficient b, argument is kg per plant
!     ac0storeinit - db input, crop storage root mass initialzation
!                    (mg/plant)
!     ac0transf - db input flag:
!                 0 = crop is planted using stored biomass of seed or vegatative propagants
!                 1 = crop is planted as a transplant with roots, stems and leaves present
!     acbaf - biomass adjustment factor
!     acbaflg - flag for biomass adjustment action
!               0 = normal crop growth
!               1 = find biomass adjustment factor for target yield
!               2 = use given biomass adjustment factor
!     accovfact - residue cover factor (m^2/kg)
!     acdayam - number of days since crop matured
!     acdayspring - day of year in which a winter annual released stored
!                   growth
!     acddsthrsh - decomposition days required for first stem fall
!     acdkrate - array of decomposition rate parameters
!     acdkrate(1) - standing residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(2) - flat residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(3) - buried residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(4) - root residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(5) - stem residue number decline rate (d<1) (#/m^2/day)?
!                   (fall rate)
!     acdmaxshoot - maximum number of shoots possible from each plant
!     acdpop - crop seeding density (#/m^2)
!     acehu0 - heat unit index leaf senescence starts
!     acfleaf2stor - fraction of assimilate partitioned to leaf that is
!                    diverted to root store
!     acfleafstem - crop leaf to stem mass ratio for shoots
!     acfshoot - crop ratio of shoot diameter to length
!     acfstem2stor - fraction of assimilate partitioned to stem that is
!                    diverted to root store
!     acfstor2stor - fraction of assimilate partitioned to standing storage
!                    (reproductive) that is diverted to root store
!     acgrf - fraction of reproductive biomass that is grain (mg/mg)
!     achyfg - flag indicating the part of plant to apply the "grain
!              fraction", grf, to when removing that plant part for yield
!              0 = grf applied to above ground storage (seeds,
!                  reproductive)
!              1 = grf times growth stage factor (see growth.for) applied
!                  to above ground storage (seeds, reproductive)
!              2 = grf applied to all aboveground biomass (forage)
!              3 = grf applied to leaf mass (tobacco)
!              4 = grf applied to stem mass (sugarcane)
!              5 = grf applied to below ground storage mass (potatoes,
!                  peanuts)
!     acmrt - total crop root mass (rootfiber + rootstore) (kg/m^2)
!     acmst - standing crop mass (standstem + standleaf + standstore)
!             (kg/m^2)
!     acrbc - crop residue burial class (it exists in crop so it can be
!             carried into residue)
!             1 = fragile-very small (soybeans) residue
!             2 = moderately tough-short (wheat) residue
!             3 = non fragile-med (corn) residue
!             4 = woody-large residue
!             5 = gravel-rock
!     acresevapa - coefficient a in relation
!                  ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)
!     acresevapb - coefficient b in relation
!                  ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)
!     acresid_int - residue intercept (kg/m^2)
!                   harvest_residue = acyld_coef(kg/kg) *
!                   yield + acresid_int (kg/m^2)
!     actdtm - days from planting to maturity for summer crops, or
!              the days
!     acthucum - crop accumulated heat units
!     acthudf - heat units or days to maturity flag
!     acthum - accumulated heat units from planting to maturity, or from
!              start of growth to maturity for perennial crops
!     actmin - minimum temperature for plant growth (deg c)
!     actopt - optimal temperature for plant growth (deg c)
!     actverndel - thermal delay coefficient pre-vernalization
!     acxrow - crop row spacing
!     acxstm - crop stem diameter (m)
!     acxstmrep - a representative diameter so that
!                 acdstm*acxstmrep*aczht=acrsai
!     acycon - conversion factor from kg/m^2 to units named in acynmu (all
!              dry weight)
!     acyld_coef - yield coefficient (kg/kg)
!                  harvest_residue = acyld_coef(kg/kg) *
!                  yield + acresid_int (kg/m^2)
!     acynmu - string for name of units in which yield of interest will be
!              reported
!     acyraf - yield to biomass ratio adjustment factor
!     acytgt - target yield
!     acywct - water content at which yield is to be reported (percent)
!     aczmrt - maximum crop root depth (m)
!     aczmxc - maximum crop height (m)
!     admbgz - buried residue mass by soil layer
!     ah0cb - power of brooks and corey water release curve model
!     aheaep - soil air entry potential (j/kg)
!     ahfice - fraction of soil water in layer which is frozen
!     ahfwsf - water stress factor ratio (0-1). this is read in daily.
!     ahrsk - saturated soil hydraulic conductivity (m/s)
!     ahrwc - soil water content (kg/kg)
!     ahrwca - available soil layer water content (kg/kg)
!     ahrwcf - soil layer field capacity water content (kg/kg)
!     ahrwcs - soil layer saturated water content (kg/kg)
!     ahrwcw - soil layer wilting point water content (kg/kg)
!     ahtsav - mean daily soil temperature (deg c)
!     ahtsmn - daily minimum soil temperature by layer
!     ahtsmx - daily maximum soil temperature by layer. currently not
!              used.
!     ahzea - actual bare soil evaporation (mm/day)
!     ahzep - potential bare soil evaporation (mm/day)
!     ahzeta - actual evapotranspiration (mm/day)
!     ahzetp - potential evapotranspiration (mm/day)
!     ahzpta - actual plant transpiration (mm/day)
!     ahzptp - potential plant transpiration (mm/day)
!     am0cdb - flag to print crop variables before and after call to crop
!     am0cfl - flag to print crop submodel output
!     am0cgf - flag to run crop growth if between plant and harvest.
!              currently not used.
!     am0cif - flag to initialize crop at start of planting
!     am0jd - current julian day of simulation
!     amalat - latitude of a location
!     amzele - average site elevation (m) (unitless)
!     as0ags - soil layer aggregate size geometric standard dev. (mm)
!     asdblk - soil layer bulk density for each subregion
!     aseags - soil layer dry aggregate stability (j/m^2)
!     asfsan - soil layer sand content
!     asfcce - soil layer calcium carbonate equivalent for each of the
!              subregions
!     asfcec - soil layer cation exchange capacity
!     asfcla - soil layer clay content (mg/mg)
!     asfom - soil layer organic matter content
!     asfsil - soil layer silt content (mg/mg)
!     asftan - total available n in a layer from all sources
!     asftap - total available p in a layer from all sources
!     aslagm - soil layer aggregate size geometric mean dia. (mm)
!     aslagn - minimum aggregate size (mm)
!     aslagx - maximum aggregate size (mm)
!     aslrr - allmaras random roughness parameter (mm)
!     aslrrc - random roughness parameter c (scale)
!     asmno3 - amount of applied no3 as fertilizer
!     aszlyd - depth from top of soil to botom of layer
!     aszlyt - soil layer thicknesses for each subregion
!     awadir - predominant daily wind direction (degrees)
!     awhrmx - hour maximum daily wind speed occurs (hr)
!     awrrh - daily relative humidity
!     awudmn - minimum daily wind speed (m/s)
!     awudmx - maximum daily wind speed (m/s)
!     cook_yield - flag setting which uses input from crop record to
!                  guarantee a fixed yield/residue ratio at harvest
!     growth_stress- flag setting which turns on water or temperature
!                   stress (or both)
!                   growth_stress = 0  ! no stress values applied
!                   growth_stress = 1  ! turn on water stress
!                   growth_stress = 2  ! turn on temperature stress
!                   growth_stress = 3  ! turn on both
!     debe because it is now being read in, it is commented out in command.inc
!     max_arg_exp - maximum value allowed for argument to exponential
!                   function without overflowing
!     max_real - maximum real number allowed
!     nslay - number of soil layers
!     prevbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!     prevchillucum - accumulated chilling units (days)
!     prevdayap - number of days of growth completed since crop planted
!     prevflatleaf - crop flat leaf mass (kg/m^2)
!     prevflatstem - crop flat stem mass (kg/m^2)
!     prevflatstore - crop flat storage mass (kg/m^2)
!     prevgrainf - internally computed grain fraction of reproductive
!                  mass
!     prevht - crop height (m)
!     prevhucum - crop accumulated heat units
!     prevliveleaf - fraction of standing plant leaf which is living
!                    (transpiring)
!     prevrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     prevrootstorez - crop root storage mass by soil layer (kg/m^2)
!     prevrtd - crop root depth (m)
!     prevrthucum - crop accumulated heat units with no
!                   vernalization/photoperiod delay
!     prevstandleaf - crop standing leaf mass (kg/m^2)
!     prevstandstem - crop standing stem mass (kg/m^2)
!     prevstandstore - crop standing storage mass (kg/m^2)
!     prevstm - number of crop stems per unit area (#/m^2). it is computed
!             by taking the tillering factor times the plant population
!             density.
 
!     + + +  newly added arguments definitions + + +
 
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis.
!     aifs - awns initials formed growth stage for spring barley and winter
!            barley. this array includes daynum, year, month and day of when
!            this stage was reached.
!     antes - end of anthesis growth stage for hay millet, proso millet,
!             spring barley, spring wheat, sunflower, winter barley and winter
!             wheat. this array includes daynum, year, month and day of when
!             this stage was reached.
!     antss - start of anthesis growth stage for corn, dry beans, hay millet,
!             proso millet, sorghum (first bloom), soybean, spring barley, spring
!             wheat, sunflower, winter barley and winter wheat. in dry beans,
!             the start of anthesis growth stage and there is one open
!             flower per plant =100% bloom. in soybean, beginning bloom, one open flower
!             at any node. this array includes daynum, year, month and day
!              of when this stage was reached.
!     blstrs - blister growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     bmats - in soybean, the beginning of maturity. There is one pod anywhere with its mature color.
!             this array includes daynum, year, month and day of when this stage was reached
!     boots - booting growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this array
!             includes daynum, year, month and day of when this stage was
!             reached.  booting is defined as flag leaf has completed its
!             growth.
!     browns - when the back of the sunflower head is yellow and there may be
!              some brown spotting. this array includes daynum, year, month
!              and day of when this stage was reached.
!     callgdd - a flag to switch between methods for determining gdd.
!               if the flag is set to true then gddcalc subroutine is
!               called. otherwise, the code within crop is used.
!     canht -  the height of the plant canopy.
!     canopyflg - a flag to indicate if the weps/upgm method to calculate
!                 plant height will be used. value will then be 0. if using
!                 the phenologymms method, the value will be 1.
!     cliname - the name of the location for the climate data.
!     co2atmos - the daily atmospheric level of CO2.
!     co2x - the CO2 levels in ppm. The x axis on the relationship curve.
!     co2y - the relative effect at different levels of CO2, i.e. co2x.
!     cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
!            of when this stage was reached.
!     cropname - crop name.
!     dayhtinc - the increase in plant height for today.
!     dents - the dent growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     doughs - the dough growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     drs - double ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     dummy1 - in determining the next phenological growth stage, this
!              holds whether the condition is gn or gs, that is when gdd
!              values are used to advance to the next growth stage is it
!              done under non-stressed or stressed conditions.
!     dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     ears - the ear initiation stage in corn. this array includes daynum,
!            year, month and day of when this stage was reached.
!     ecanht - this is the maximum canopy height of the crop in phase 1 of
!              the canopy height growth.  this is usually from emergence to
!              when the plant begins elongating stems but this stage varies
!              among crops. it is an input parameter and is read in from upgm_crop.dat.
!     egdd - a 6 element array that holds the ergdd values plus calculated values
!            for two intermediate soil moisture level values in elements 2 and 4.
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred in all crops. this array includes
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     epods - one pod has reached the maximum length in dry beans (early
!             pod set). in soybean, beginning pod and one pod is 3/16" 
!             long at one of the four uppermost nodes. this array includes 
!             daynum,year, month and day of when this stage was reached.
!     ergdd - an array holding 4 elongation rates in mm per gdd
!             based on each soil moisture description.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans (early seed fill). in soybean, beginning seed, 
!              and seed is 1/8" long in pod at one of the four uppermost 
!              nodes. this array includes daynum, year, month and day of
!              when this stage was reached.
!     first7 - used to set the value of aepa the first time
!              phenol_cropname is called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     gddtbg - used to accumulate gdd for seeds planted in dust after a
!              rainfall event has moved the soil moisture condition to
!              dry.  the seed can begin accumulating gdd's germination.
!     germgdd - an array holding 4 germination times in gdd at tbase for
!               the soil moisture levels.
!     germs - simulated day that germination occurs
!     ggdd - a 6 element array that holds the germgdd values plus calculated values for
!           two intermediate soil moisture level values in elements 2 and 4.
!     gmethod - selects the method whereby gdd will be calculated.  a value of
!               1 corresponds to method 1 in phenologymms and is used for
!               crops such as winter wheat, winter barley and proso millet. a
!               value of 2 corresponds to method 2 in phenologymms and is used
!               for crops such as corn, sorghum and sunflower.  a value of 3
!               is the way that weps/upgm calculated ggd for the day.
!     gpds - growing point differentiation growth stage in sorghum. this
!            array includes daynum, year, month and day of when this stage
!            was reached.
!     halfbs - half bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     heads - heading growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this
!             array includes daynum, year, month and day of when this stage
!             was reached.
!     hrs - time to harvest ready growth stage for corn, dry beans, hay
!           millet, proso millet, sorghum, soybean, spring barley, spring wheat,
!           sunflower, winter barley and winter wheat. in dry beans, 80%
!           of pods are at the mature color in dry beans. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     ies - start of internode elongation growth stage for corn, hay millet,
!           proso millet, sorghum, soybean, spring barley, spring wheat, 
!           winter barley, and winter wheat. for sunflower, this stage occurs when the
!           internode below the inflorescence elongates 0.5 to 2.0 cm above
!           the nearest leaf on the stem. this array includes daynum, year,
!           month and day of when this stage was reached.
!     ies2 - for sunflower, this is when the internode below the inflorescence
!            continues lengthening and lifts the head above the surrounding
!            leaves more than 2 cm. this array includes daynum, year,
!            month and day of when this stage was reached.
!     infls - the sunflower inflorescence becomes visible. this array includes
!             daynum, year, month and day of when this stage was reached.
!     joints - jointing growth stage for hay millet, proso millet, sorghum,
!              spring barley, spring wheat, winter barley and winter wheat.
!              this array includes daynum, year, month and day of when this
!              stage was reached.
!     lf1s - stage when the first trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf12s - the 12 leaf growth stage for corn and sunflower. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     lf2s - stage when the second trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf3s - stage when the third trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf4s - the 4 leaf growth stage for corn and sunflower and the
!            stage when the fourth trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf5s - the stage when the fifth trifoliolate leaf is unfolded in 
!            soybean.  this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf8s - the 8 leaf growth stage for sunflower. this array includes
!            daynum, year, month and day of when this stage was reached.
!     mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, spring barley, spring
!            wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. in soybean, fll maturity, 
!            95  % of the pods have reached their mature color. this array includes
!            daynum, year, month and day of when this stage was reached.
!     maxht - this is the maximum canopy height of the crop.  it is an
!             input parameter and is read in from upgm_crop.dat.
!     mffls - the stage of mid to full flower in dry beans. in soybeans, full 
!             bloom, one open flower at one of the two uppermost nodes. this array
!             includes daynum, year, month and day of when this stage
!             was reached.
!     milks - the milk growth stage in corn. this array includes daynum, year,
!             month and day of when this stage was reached.
!     mpods - the stage when 50% of the pods are at the maximum length in dry beans.
!             in soybean, full pod and pod is 3/4" long at one of the four uppermost nodes.    
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     mseeds - the stage when 50% of the pods have fully developed seeds
!              in dry beans. in soybean, full seed and a pod contains a green seed 
!              that fulls the pod cavity at one of the four upermost nodes. this 
!              array includes daynum, year, month and day of when this stage was reached.
!     opens - the sunflower inflorescence begins to open. this array includes
!             daynum, year, month and day of when this stage was reached.
!     pchron - phyllochron value which is the number of gdd per leaf.
!     phenolflg - a flag that determines if the upgm/weps method of determining maturity
!                 will be used (phenolflg =0) or the phenologymms method will be used (phenolflg = 1).
!     seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1= optimum, 2 = medium, 3 = dry and
!              4 = planted in dust. it was changed to real to allow moving in
!              half steps to the next soil moisture level in accomplishing emergence.
!              it was later changed back to integer.
!     silks - the silking growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     soilwat - an array holding the swtype for each soil moisture
!               condition.
!     srs - single ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     tbase - lowest temperature below which no growth occurs (deg.c).
!     tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley and
!           winter wheat. this array includes daynum, year, month and day of
!           when this stage was reached.
!     toptlo - the lower temperature in the optimum range for plant
!              growth (deg.c).
!     toptup - the upper temperature in the optimum range for plant
!              growth (deg.c).
!     tsints - tassel initiation growth stage in corn. this array includes
!              daynum, year, month and day of when this stage was reached.
!     tss - terminal spikelet growth stage for spring and winter wheat. this
!           array includes daynum, year, month and day of when this stage was
!           reached.
!     tupper - upper/maximum temperature for plant growth (deg.c).
!              no growth with temperatures above this point.
!     wfpslo - an array holding the low values for each soil moisture
!              condition.
!     wfpsup - an array holding the high values for each soil moisture
!              condition.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
!
!     + + + new local variable definitions + + +
!     elrate - elongation rate of the emerging seedling based on the soil
!              moisture level.  it is read into the ergdd array. (mm)
!     germd - gdd's for germination at soil moisture.  this is read into
!             the germgdd array.
!     i - counter variable.
!     row - counter variable for reading values into emergence arrays.
!     seedbed - contains the soil moisture condition of the seedbed.
!     swtype - soil moisture condition. this is read into the soilwat
!              array.
!     wlow - lower range of soil moisture. read into the wfpslo array.
!     wup - upper range of soil moisture. read into the wfpsup array.
 