subroutine upgm_driver(                                         &
        &       ctrl,clidat,soils,bio,residue,biotot,prevbio,   &
        &       icli,pd,pm,py,hd,hm,hy, am0hrvfl)
!
    use constants, only : mnsz, mnsub, mnbpls, mncz, mndk, mnhhrs
    use upgm_simdata, only : controls
    use climate, only : climate_data
    use soil, only : soildata
    use biomaterial
implicit none

!
! Dummy arguments
!
    type(controls) :: ctrl
    type(climate_data) :: clidat
    type(soildata) :: soils
    type(biomatter) :: bio, prevbio
    type(biomatter), dimension(*) ::  residue
    type(biototal) :: biotot

integer :: am0hrvfl,    &
         & icli,pd,pm,py,hd,hm, hy
integer(kind=4) :: day_iter
!
! Local variables
!
integer :: chkid,chkim,chkiy,ed,em,ey,i,id,im,iy,julday,               &
         & mature_warn_flg,row,sd,sm,sy,co2dy,co2mn,co2yr
real :: elrate,germd,wlow,wup
!
! local variables
!
!real, save :: canht
!
! start of time loop - does this work correctly across multiple years and crops?
!

do day_iter = ctrl%sim%start_jday,ctrl%sim%end_jday   ! currently must start on 1/1 and end on 12/31
!
  ctrl%sim%juldate = day_iter
 
  call caldat(ctrl%sim%juldate,id,im,iy)
!  if ((im.lt.4).and.(iy.eq.1)) then
!      print*, 'in main id = ', id,'im = ', im, 'iy = ', iy !, 'wwtdmx = ', wwtdmx(dayidx)
!  end if
!debe added check to determine if it is a leap year. if it is not leap year then continue.
! if it is leap year skip this day and go to 3/1/yr.
!  if ((id.eq.29).and.(im.eq.2)) then  !this didn't work
!   id = id + 1
!   im = im + 1
!   print * , 'in leap year id = ', id, 'im = ', im
!  end if
!
 
!  if ((id.ne.29).or.(im.ne.2)) then !check if leap year
    ! this reads in one day of climate data
  call climate_input(ctrl, clidat, id,im,iy,icli)
!  if ((id.ne.29).or.(im.ne.2)) then !check if leap year
    !
    ! read in daily water stress
  read (ctrl%handles%upgmstress,*) chkid,chkim,chkiy,ctrl%cropstress%ahfwsf     !upgm_stress.dat
  if ((chkid/=id).or.(chkim/=im).or.(chkiy/=iy)) then
     write (*,*) 'error in dates in water stress file - stop'
     call exit(1)
  end if
  
!read in daily atmospheric co2 values.
  !co2dy = day; co2mn = month; co2yr = year
  !co2atmos = atmospheric co2 value for the day.
  read (ctrl%handles%upgmco2atmos,*) co2dy, co2mn, co2yr, clidat%co2atmos  !upgm_co2atmos.dat

!************************************************************************************
  !start of "crop growth" -> same for all models. 
  
  
!  if ((id.ne.29).or.(im.ne.2)) then !check if leap year
    !
  if (ctrl%sim%juldate==ctrl%sim%plant_jday) then
     write (*,*) 'planting date: ',pd,'/',pm,'/',py
     ctrl%sim%growcrop_flg = .true.
     bio%growth%am0cif = .true.
     bio%growth%am0cgf = .true.
  end if
!
 
    !  if (am0jd.eq.harvest_jday) then !original way
 
    ! debe added new method of determining when harvest day occurs utilizing the
    ! phenolflg and writing out the value in season.out
  if (((bio%upgm%phenolflg==1).and.(bio%upgm%hrs(1)/=999).and.am0hrvfl==0).or.                    &
    & ((bio%upgm%phenolflg==0).and.( ctrl%sim%juldate==ctrl%sim%harvest_jday))) then
 
     if (bio%upgm%phenolflg==1) then
        write (*,*) 'harvest date: ',bio%upgm%hrs(1),bio%upgm%hrs(2),bio%upgm%hrs(3),bio%upgm%hrs(4)
     else if (bio%upgm%phenolflg==0) then
        write (*,*) 'harvest date: ',hd,'/',hm,'/',hy
     end if
     bio%growth%am0cgf = .false.
     am0hrvfl = 1     ! debe uncommented this line because it is now needed to prevent crop_endseason
                      ! being called after harvest every day until the end date of simulation.
    !
     call crop_endseason(ctrl,bio%bname,bio%growth%am0cfl,soils%spp%nslay,bio%database%idc,bio%growth%dayam,    &
                       & bio%database%thum,biotot%xstmrep,prevbio%mass%standstem,   &
                       & prevbio%mass%standleaf,prevbio%mass%standstore,prevbio%mass%flatstem, &
                       & prevbio%mass%flatleaf,prevbio%mass%flatstore,prevbio%mass%stemz,  &
                       & prevbio%mass%rootstorez,prevbio%mass%rootfiberz,prevbio%geometry%zht,  &
                       & prevbio%geometry%dstm,prevbio%geometry%zrtd,prevbio%growth%dayap,prevbio%growth%thucum,   &
                       & prevbio%growth%trthucum,prevbio%geometry%grainf,prevbio%growth%tchillucum,      &
                       & prevbio%growth%fliveleaf,bio%growth%dayspring,mature_warn_flg,      &
                       & bio%database%ycon,bio%database%ynmu,bio%upgm%ies,bio%upgm%joints,bio%upgm%boots,bio%upgm%heads,bio%upgm%antss,bio%upgm%mats,bio%upgm%hrs,   &
                       & bio%upgm%phenolflg)
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
     call callcrop(ctrl,clidat,soils,bio,residue,biotot,prevbio,bio%upgm%aepa,bio%upgm%aifs,ctrl%sim%juldate-ctrl%sim%plant_jday+1,1,bio%upgm%antes,bio%upgm%antss,bio%upgm%blstrs,bio%upgm%boots,     &
                 & bio%upgm%browns,bio%upgm%callgdd,bio%upgm%canht,bio%upgm%canopyflg,ctrl%sim%cliname,bio%upgm%cots,bio%bname,        &
                 & bio%upgm%dayhtinc,bio%upgm%dents,bio%upgm%doughs,bio%upgm%drs,bio%upgm%dummy1,bio%upgm%dummy2,bio%upgm%ears,bio%upgm%ecanht,bio%upgm%egdd,    &
                 & bio%upgm%emrgflg,bio%upgm%ems,bio%upgm%endlgs,bio%upgm%epods,bio%upgm%ergdd,bio%upgm%eseeds,bio%upgm%first7,bio%upgm%fps,bio%upgm%fullbs,     &
                 & bio%upgm%gddtbg,bio%upgm%germgdd,bio%upgm%germs,bio%upgm%ggdd,bio%upgm%gmethod,bio%upgm%gpds,bio%upgm%growth_stress,bio%upgm%halfbs, &
                 & bio%upgm%heads,bio%upgm%hrs,icli,bio%upgm%ies,bio%upgm%ies2,bio%upgm%infls,bio%upgm%joints,bio%upgm%lf12s,bio%upgm%lf1s,bio%upgm%lf2s,bio%upgm%lf3s,   &
                 & bio%upgm%lf4s,bio%upgm%lf8s,bio%upgm%mats,bio%upgm%maxht,bio%upgm%mffls,bio%upgm%milks,bio%upgm%mpods,bio%upgm%mseeds,bio%upgm%opens,bio%upgm%pchron,  &
                 & pd,bio%upgm%phenolflg,pm,py,bio%upgm%seedsw,bio%upgm%silks,bio%upgm%soilwat,bio%upgm%srs,bio%upgm%tbase,bio%upgm%tis,bio%upgm%toptlo,&
                 & bio%upgm%toptup,bio%upgm%tsints,bio%upgm%tss,bio%upgm%tupper,bio%upgm%wfpslo,bio%upgm%wfpsup,bio%upgm%yelows,bio%upgm%co2x,bio%upgm%co2y,     &
                 & clidat%co2atmos)
 
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
