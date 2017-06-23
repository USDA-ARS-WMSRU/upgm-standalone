subroutine upgm_driver(ctrl,clidat,sppdat,bio,sr,start_jday,end_jday,plant_jday,harvest_jday,aepa,aifs,&
                     & antes,antss,blstrs,boots,browns,callgdd,canht,canopyflg, &
                     & cliname,cots,cropname,dayhtinc,dents,doughs,drs,dummy1,  &
                     & dummy2,ears,ecanht,egdd,emrgflg,ems,endlgs,epods,ergdd,  &
                     & eseeds,first7,fps,fullbs,gddtbg,germgdd,germs,ggdd,      &
                     & gmethod,gpds,growth_stress,halfbs,heads,hrs,icli,ies,    &
                     & ies2,infls,joints,lf12s,lf1s,lf2s,lf3s,lf4s,lf8s,mats,   &
                     & maxht,mffls,milks,mpods,mseeds,opens,pchron,pd,phenolflg,&
                     & pm,py,hd,hm,hy,seedsw,silks,soilwat,srs,tbase,tis,toptlo,&
                     & toptup,tsints,tss,tupper,wfpslo,wfpsup,yelows,seedbed,   &
                     & swtype,growcrop_flg,am0hrvfl,co2x,co2y,co2atmos)
!
    use constants, only : mnsz, mnsub, mnbpls, mncz, mndk, mnhhrs
    use upgm_simdata, only : controls
    use climate, only : climate_data
    use soil, only : soil_phys_props
    use biomaterial
implicit none
!
include 'file.fi'
include 's1dbc.inc'
include 'd1glob.inc'
include 'c1gen.inc'
include 'h1hydro.inc'
include 'c1info.inc'
include 'c1db1.inc'
include 'c1db2.inc'
include 'c1glob.inc'
include 'h1et.inc'
include 'prevstate.inc'
!
! Dummy arguments
!
    type(controls) :: ctrl
    type(climate_data) :: clidat
    type(soil_phys_props) :: sppdat
    type(biomatter) :: bio
real :: aepa,canht,dayhtinc,ecanht,gddtbg,maxht,pchron,tbase,toptlo,toptup,     &
      & tupper,co2atmos
integer :: am0hrvfl,canopyflg,emrgflg,end_jday,first7,gmethod,growth_stress,    &
         & harvest_jday,icli,pd,phenolflg,plant_jday,pm,py,hd,hm,hy,seedsw,sr,  &
         & start_jday 
logical :: callgdd,growcrop_flg
character(80) :: cliname
character(80) :: cropname
character(40) :: seedbed,swtype
integer,dimension(4) :: aifs,antes,antss,blstrs,boots,browns,cots,dents,doughs, &
                      & drs,ears,ems,endlgs,epods,eseeds,fps,fullbs,germs,gpds, &
                      & halfbs,heads,hrs,ies,ies2,infls,joints,lf12s,lf1s,lf2s, &
                      & lf3s,lf4s,lf8s,mats,mffls,milks,mpods,mseeds,opens,     &
                      & silks,srs,tis,tsints,tss,yelows
character(5),dimension(30) :: dummy1
real,dimension(30) :: dummy2
real,dimension(6) :: egdd,ggdd
real,dimension(4) :: ergdd,germgdd,wfpslo,wfpsup
character(80),dimension(4) :: soilwat
real,dimension(10) :: co2x,co2y
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

do day_iter = start_jday,end_jday   ! currently must start on 1/1 and end on 12/31
!
  ctrl%sim%julday = day_iter
 
  call caldat(ctrl%sim%julday,id,im,iy)
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
  call climate_input(clidat, id,im,iy,icli)
!  if ((id.ne.29).or.(im.ne.2)) then !check if leap year
    !
    ! read in daily water stress
  read (upgmstress,*) chkid,chkim,chkiy,ahfwsf(1)     !upgm_stress.dat
  if ((chkid/=id).or.(chkim/=im).or.(chkiy/=iy)) then
     write (*,*) 'error in dates in water stress file - stop'
     call exit(1)
  end if
  
!read in daily atmospheric co2 values.
  !co2dy = day; co2mn = month; co2yr = year
  !co2atmos = atmospheric co2 value for the day.
  read (upgmco2atmos,*) co2dy, co2mn, co2yr, co2atmos  !upgm_co2atmos.dat

!************************************************************************************
  !start of "crop growth" -> same for all models. 
  
  
!  if ((id.ne.29).or.(im.ne.2)) then !check if leap year
    !
  if (ctrl%sim%julday==plant_jday) then
     write (*,*) 'planting date: ',pd,'/',pm,'/',py
     growcrop_flg = .true.
     bio%growth%am0cif = .true.
     bio%growth%am0cgf = .true.
  end if
!
 
    !  if (am0jd.eq.harvest_jday) then !original way
 
    ! debe added new method of determining when harvest day occurs utilizing the
    ! phenolflg and writing out the value in season.out
  if (((phenolflg==1).and.(hrs(1)/=999).and.am0hrvfl==0).or.                    &
    & ((phenolflg==0).and.( ctrl%sim%julday==harvest_jday))) then
 
     if (phenolflg==1) then
        write (*,*) 'harvest date: ',hrs(1),hrs(2),hrs(3),hrs(4)
     else if (phenolflg==0) then
        write (*,*) 'harvest date: ',hd,'/',hm,'/',hy
     end if
     bio%growth%am0cgf = .false.
     am0hrvfl = 1     ! debe uncommented this line because it is now needed to prevent crop_endseason
                      ! being called after harvest every day until the end date of simulation.
    !
     call crop_endseason(ctrl,ac0nam(sr),bio%growth%am0cfl,sppdat%nslay,ac0idc(sr),acdayam(sr),    &
                       & acthum(sr),acxstmrep(sr),prevstandstem(sr),            &
                       & prevstandleaf(sr),prevstandstore(sr),prevflatstem(sr), &
                       & prevflatleaf(sr),prevflatstore(sr),prevbgstemz(1,sr),  &
                       & prevrootstorez(1,sr),prevrootfiberz(1,sr),prevht(sr),  &
                       & prevstm(sr),prevrtd(sr),prevdayap(sr),prevhucum(sr),   &
                       & prevrthucum(sr),prevgrainf(sr),prevchillucum(sr),      &
                       & prevliveleaf(sr),acdayspring(sr),mature_warn_flg,      &
                       & acycon(sr),acynmu(sr),ies,joints,boots,heads,antss,mats,hrs,   &
                       & phenolflg)
    !debe added acycon to the passing arguments to crop_endseason to allow calculation
    ! of the yield in crop_endseason.
    !debe added phenolflg and several growth stage arrays to be passed to crop_endseason
    ! to print out in season.out. as different crops are tested more growth stage variables
    ! appropriate to each crop may need to be passed to crop_endseason.
  end if
    !
  if (growcrop_flg.eqv..true.) then
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
     call callcrop(ctrl,clidat,sppdat,bio,aepa,aifs,ctrl%sim%julday-plant_jday+1,1,antes,antss,blstrs,boots,     &
                 & browns,callgdd,canht,canopyflg,cliname,cots,cropname,        &
                 & dayhtinc,dents,doughs,drs,dummy1,dummy2,ears,ecanht,egdd,    &
                 & emrgflg,ems,endlgs,epods,ergdd,eseeds,first7,fps,fullbs,     &
                 & gddtbg,germgdd,germs,ggdd,gmethod,gpds,growth_stress,halfbs, &
                 & heads,hrs,icli,ies,ies2,infls,joints,lf12s,lf1s,lf2s,lf3s,   &
                 & lf4s,lf8s,mats,maxht,mffls,milks,mpods,mseeds,opens,pchron,  &
                 & pd,phenolflg,pm,py,seedsw,silks,soilwat,srs,tbase,tis,toptlo,&
                 & toptup,tsints,tss,tupper,wfpslo,wfpsup,yelows,co2x,co2y,     &
                 & co2atmos)
 
         !if (am0jd.eq.harvest_jday) growcrop_flg = .false.
         !de and gm changed the above code so that if the input harvest date is
         ! reached before hrs(1) has a value .ne. to 999 when using phenolflg = 1,
         ! simulation will continue until it does reach a harvest date.
     if (((phenolflg==1).and.(hrs(1)/=999)).or.                                 &
       & ((phenolflg==0).and.(ctrl%sim%julday==harvest_jday))) growcrop_flg = .false.
 
  end if
  
    !end of "crop growth" -> same for all models.
!************************************************************************************
   
! end if !end leap year check
end do ! end do loop for current day
    ! debe print out canopy height and the canopyflg at the end of the run
write (luophenol,1000) canht,canopyflg
    !
 1000 format ('canopy height =',2x,f5.1,'(cm) ','canopyflg = ',2x,i1)
 
end subroutine upgm_driver
