    subroutine upgm_driver(ctrl, clidat, soils, bio, residue, biotot, prevbio)
    !
    use constants, only : int32, sp
    use upgm_simdata, only : controls
    use climate, only : climate_data
    use soil, only : soildata
    use datetime, only : caldat, date
    use biomaterial
    implicit none

    ! Arguments
    !
    type(controls) :: ctrl
    type(climate_data) :: clidat
    type(soildata) :: soils
    type(biomatter) :: bio, prevbio
    type(biomatter), dimension(*) ::  residue
    type(biototal) :: biotot

    ! Local variables
    !
    type(date) :: checkdate
    type(date) :: currdate
    type(date) :: co2date
    integer(int32) :: day_iter
    integer(int32) :: daysim
    integer(int32) :: julday
    integer(int32) :: mature_warn_flg = 0  !flag to indicate use of crop maturity warning
    !                0  - no crop maturity warning given for any crop
    !                1  - warnings generated for any crop unless supressed
    !                     by crop type

    ! start of time loop - does this work correctly across multiple years and crops?
    !

    do day_iter = ctrl%sim%start_jday,ctrl%sim%end_jday   ! currently must start on 1/1 and end on 12/31
        !
        ctrl%sim%juldate = day_iter
        ! calculate the gregorian date from julian
        call caldat(ctrl%sim%juldate,currdate%day,currdate%mon,currdate%year)

        ! read in this day's climate data
        call climate_input(ctrl, clidat, currdate)

        ! read in this day's water stress value
        read (ctrl%handles%upgmstress,*) checkdate%day,checkdate%mon,checkdate%year,ctrl%cropstress%ahfwsf     !upgm_stress.dat
        if ((checkdate%day/=currdate%day).or.(checkdate%mon/=currdate%mon).or.(checkdate%year/=currdate%year)) then
            write (*,*) 'error in dates in water stress file - stop'
            call exit(1)
        end if

        ! read in this day'satmospheric co2 value.
        read (ctrl%handles%upgmco2atmos,*) co2date%day, co2date%mon, co2date%year, clidat%co2atmos  !upgm_co2atmos.dat

        !************************************************************************************
        !start of "crop growth" -> same for all models.


        ! if planting date is the current day: do stuff!
        if (ctrl%sim%juldate==ctrl%sim%plant_jday) then
            write (*,*) 'planting date: ',ctrl%mngt%pd,'/',ctrl%mngt%pm,'/',ctrl%mngt%py
            ctrl%sim%growcrop_flg = .true.
            bio%growth%am0cif = .true.
            bio%growth%am0cgf = .true.
        end if
        !

        ! debe added new method of determining when harvest day occurs utilizing the
        ! phenolflg and writing out the value in season.out
        if (((bio%upgm%phenolflg==1).and.(bio%upgm%hrs(1)/=999).and.ctrl%sim%am0hrvfl==0).or.                    &
            & ((bio%upgm%phenolflg==0).and.( ctrl%sim%juldate==ctrl%sim%harvest_jday))) then

        if (bio%upgm%phenolflg==1) then
            write (*,*) 'harvest date: ',bio%upgm%hrs(1),bio%upgm%hrs(2),bio%upgm%hrs(3),bio%upgm%hrs(4)
        else if (bio%upgm%phenolflg==0) then
            write (*,*) 'harvest date: ',ctrl%mngt%hd,'/',ctrl%mngt%hm,'/',ctrl%mngt%hy
        end if
        bio%growth%am0cgf = .false.
        ctrl%sim%am0hrvfl = 1     ! debe uncommented this line because it is now needed to prevent crop_endseason
        ! being called after harvest every day until the end date of simulation.
        !
        call crop_endseason(ctrl, bio, prevbio, biotot, soils%spp%nslay, mature_warn_flg)
        !debe added acycon to the passing arguments to crop_endseason to allow calculation
        ! of the yield in crop_endseason.
        !debe added phenolflg and several growth stage arrays to be passed to crop_endseason
        ! to print out in season.out. as different crops are tested more growth stage variables
        ! appropriate to each crop may need to be passed to crop_endseason.
        end if
        !
        if (ctrl%sim%growcrop_flg.eqv..true.) then
            !debe added the following variables for the emerge subroutine to the call to
            ! the callcrop subroutine: seedsw, soilwat, wfpslo, wfpsup, germgdd, ergdd,
            ! cropname, ddap, dgdds, elong, gddday.
            !debe added the variables icli and pd, pm, py to the call to the callcrop
            ! subroutine to enable printing the weather file name and planting date
            ! in emerge.out.
            !
            !debe added tupper and callgdd for the gddcalc subroutine to the call to
            ! the callcrop subroutine. tupper is read in from upgm_crop.dat. callgdd is
            ! initialized in cropinit.
            !debe added canopyflg to callcrop.
            !
            ! (am0jd-plant_jday+1) = current day of crop growth
            ! debe added temperature variables: tbase, toptlo, topup, tupper, canopyflg
            ! debe added passing the new variable 'phenolflg' which is read in from upgm_crop.dat
            ! and the phenological growth stages variables to callcrop which will pass
            ! them on to crop.
            daysim = ctrl%sim%juldate-ctrl%sim%plant_jday+1
            call callcrop(ctrl,clidat,soils,bio,residue,biotot,prevbio,daysim)
            !,   &
            !    & bio%upgm%dayhtinc,bio%upgm%dents,bio%upgm%doughs,bio%upgm%drs,bio%upgm%dummy1,bio%upgm%dummy2,bio%upgm%ears,bio%upgm%ecanht,bio%upgm%egdd,    &
            !    & bio%upgm%emrgflg,bio%upgm%ems,bio%upgm%endlgs,bio%upgm%epods,bio%upgm%ergdd,bio%upgm%eseeds,bio%upgm%first7,bio%upgm%fps,bio%upgm%fullbs,     &
            !    & bio%upgm%gddtbg,bio%upgm%germgdd,bio%upgm%germs,bio%upgm%ggdd,bio%upgm%gmethod,bio%upgm%gpds,bio%upgm%growth_stress,bio%upgm%halfbs, &
            !    & bio%upgm%heads,bio%upgm%hrs,ctrl%sim%icli,bio%upgm%ies,bio%upgm%ies2,bio%upgm%infls,bio%upgm%joints,bio%upgm%lf12s,bio%upgm%lf1s,bio%upgm%lf2s,bio%upgm%lf3s,   &
            !    & bio%upgm%lf4s,bio%upgm%lf8s,bio%upgm%mats,bio%upgm%maxht,bio%upgm%mffls,bio%upgm%milks,bio%upgm%mpods,bio%upgm%mseeds,bio%upgm%opens,bio%upgm%pchron,  &
            !    & ctrl%mngt%pd,bio%upgm%phenolflg,ctrl%mngt%pm,ctrl%mngt%py,bio%upgm%seedsw,bio%upgm%silks,bio%upgm%soilwat,bio%upgm%srs,bio%upgm%tbase,bio%upgm%tis,bio%upgm%toptlo,&
            !    & bio%upgm%toptup,bio%upgm%tsints,bio%upgm%tss,bio%upgm%tupper,bio%upgm%wfpslo,bio%upgm%wfpsup,bio%upgm%yelows,bio%upgm%co2x,bio%upgm%co2y,     &
            !    & clidat%co2atmos)

            !if (am0jd.eq.harvest_jday) growcrop_flg = .false.
            !de and gm changed the above code so that if the input harvest date is
            ! reached before hrs(1) has a value .ne. to 999 when using phenolflg = 1,
            ! simulation will continue until it does reach a harvest date.
            if (((bio%upgm%phenolflg==1).and.(bio%upgm%hrs(1)/=999)).or.                                 &
                & ((bio%upgm%phenolflg==0).and.(ctrl%sim%juldate==ctrl%sim%harvest_jday))) ctrl%sim%growcrop_flg = .false.

        end if

        !end of "crop growth" -> same for all models.
        !************************************************************************************

        ! end if !end leap year check
    end do ! end do loop for current day
    ! debe print out canopy height and the canopyflg at the end of the run
    write (ctrl%handles%luophenol,1000) bio%upgm%canht,bio%upgm%canopyflg
    !
1000 format ('canopy height =',2x,f5.1,'(cm) ','canopyflg = ',2x,i1)

    end subroutine upgm_driver
