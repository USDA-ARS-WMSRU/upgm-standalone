subroutine output_phenol(ctrl, aifs,antes,antss,blstrs,bmats,boots,browns,cname,daa,dae,dap,&
                       & dav,ddae,ddap,ddav,dents,dgdde,dgdds,dgddv,doughs,drs, &
                       & ears,ems,endlgs,fps,fullbs,gdda,gdde,gdds,gddv,gpds,   &
                       & halfbs,heads,hrs,ies,ies2,infls,joints,lf12s,lf4s,lf5s,lf8s,&
                       & lnarray,lnpout,mats,milks,opens,outf,pchron,pdate,     & 
                       & pyear,silks,srs,tis,tsints,tss,year,yelows)
!
!  the output subroutine prints output to ascii files and the screen.
!
!  inputs:  aifs(r), antes(r), antss(r), boots(r), canht(r), cname(r),
!           daa(r), dae(r), dap(r), dav(r), ddae(r), ddap(r), ddav(r),
!           dgdde(r), dgdds(r), dgddv(r), drs(r), ems(r), endlgs(r),
!           fps(r), fullbs(r), gdda(r), gdde(r), gdds(r), gddv(r),
!           gpds(r), halfbs(r), heads(r), hrs(r), ies(r), joints(r),
!           mats(r), outf(c), pchron (r), pdate(r), srs(r),
!           tis(r), tss(r)
 
!  outputs: aifs(r), antes(r), antss(r), boots(r), canht(r), cname(r),
!           daa(r), dae(r), dap(r), dav(r), ddae(r), ddap(r), ddav(r),
!           dgdde(r), dgdds(r), dgddv(r), drs(r), ems(r), endlgs(r),
!           fps(r), fullbs(r), gdda(r), gdde(r), gdds(r), gddv(r),
!           gpds(r), halfbs(r), heads(r), hrs(r), ies(r), joints(r),
!           mats(r), outf(c), pdate(r), srs(r), tis(r), tss(r)
!
!
    use upgm_simdata, only : controls
implicit none
!
! Dummy arguments
!
    type(controls) :: ctrl
character(22) :: cname,outf
integer :: daa,dae,dap,dav,pdate,pyear,year
real :: gdda,gdde,gdds,gddv,pchron 
integer,dimension(4) :: aifs,antes,antss,blstrs,boots,bmats,browns,dents,doughs,drs, &
                      & ears,ems,endlgs,fps,fullbs,gpds,halfbs,heads,hrs,ies,        &
                      & ies2,infls,joints,lf12s,lf4s,lf8s,lf5s,mats,milks,opens,     &
                      & silks,srs,tis,tsints,tss,yelows
integer,dimension(20) :: ddae,ddap,ddav
real,dimension(20) :: dgdde,dgdds,dgddv
real,dimension(400,2) :: lnarray
real,dimension(60,2) :: lnpout
!
! Local variables
!
integer :: i,j
integer,dimension(4) :: pdatearr
real :: real
!
!debe 091508 added for writing out phenology info to a file
!
! initialize variables
!
do i = 1,4
  pdatearr(i) = 0
end do
pdatearr(2) = pyear
 
j = 1
 
!  print out some stuff to the screen:
 
print *,'subroutine output was called'
print *,'crop is: ',cname              ! de added
! ! print *, 'canopy height is: ', canht  ! de added
print *,'gdds = ',gdds
print *,'dap = ',dap
print *,'gdde = ',gdde
print *,'dae = ',dae
print *,'gddv = ',gddv
print *,'dav = ',dav
print *,'gdda = ',gdda
print *,'daa = ',daa
print *,'year = ',year
 
!  want to echo out inputs and settings first
 
!      outf = 'results/phenol.out'
!      open (unit=64, file=outf)
 
! fill pdatearr
pdatearr(1) = pdate
call date1(pdatearr)
 
! round canopy height to an integer
   !   icanht = nint(canht)
 
! de took out vernaliztion from the output for the spring crops that
! still displayed it in the output. 7/16/07
 
! determine number of leaves for each day.  fill leafno array.
 !     call leafno(daynum, gdde, lnarray, lnpout, pchron)
 
 
! warn user that if planting date is outside the weather years or
! results in a harvest date outside of the weather years in the
! selected location '999' will display in the output.
!debe 091508 put the followingin commented out section in cpout.
!      write (ctrl%handles%luophenol, 145)
! 145  format ('note: if 999 is displayed in the output, the planting',
!     . /1x,'date may be outside of the weather years in the selected',
!     . /1x, 'weather file. also, the selected planting date might',
!     . ' result', /1x, 'in a harvest date outside of the years in the',
!     .  /1x, 'weather file.', /1x)
!
!!  heading for leaf number table
!      write (14,99)cname
!99    format (42x, a14, /1x, 42x,'leaf number', /1x, 39x, 'doy', 2x,
!     . 'leaf number', /1x, 38x, '------------------')
 
! write out winter wheat phenology results:
 
if (cname=='winter wheat') then
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,1000) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (14,1000) real(boots(1)),dgdde(9)/pchron
 
  write (14,1100)
!      leaf number table
 
 !    .  icanht
! 150	format (' phenological event', 7x, 'day of year', 2x, 'date', 2x,
!     .  'dap', 5x, 'dae', 5x, 'dav', 5x, 'gdd ap', 5x, 'gdd ae', 5x,
!     .  'gdd av', 5x, 'nolvs', /1x
  write (ctrl%handles%luophenol,1200) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),     &
                       & ems(4),ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),  &
                       & ddae(2),dgdds(2),dgdde(2),dgdde(2)/pchron,srs(1),srs(3)&
                       & ,srs(4),ddap(3),ddae(3),ddav(3),dgdds(3),dgdde(3),     &
                       & dgddv(3),dgdde(3)/pchron,drs(1),drs(3),drs(4),ddap(4), &
                       & ddae(4),ddav(4),dgdds(4),dgdde(4),dgddv(4),dgdde(4)    &
                       & /pchron,fps(1),fps(3),fps(4),ddap(7),ddae(7),ddav(7),  &
                       & dgdds(7),dgdde(7),dgddv(7),dgdde(7)/pchron,ies(1),     &
                       & ies(3),ies(4),ddap(6),ddae(6),ddav(6),dgdds(6),dgdde(6)&
                       & ,dgddv(6),dgdde(6)/pchron,tss(1),tss(3),tss(4),ddap(5),&
                       & ddae(5),ddav(5),dgdds(5),dgdde(5),dgddv(5),dgdde(5)    &
                       & /pchron,joints(1),joints(3),joints(4),ddap(8),ddae(8), &
                       & ddav(8),dgdds(8),dgdde(8),dgddv(8),dgdde(8)/pchron,    &
                       & boots(1),boots(3),boots(4),ddap(9),ddae(9),ddav(9),    &
                       & dgdds(9),dgdde(9),dgddv(9),dgdde(9)/pchron,heads(1),   &
                       & heads(3),heads(4),ddap(10),ddae(10),ddav(10),dgdds(10),&
                       & dgdde(10),dgddv(10),dgdde(9)/pchron,antss(1),antss(3), &
                       & antss(4),ddap(11),ddae(11),ddav(11),dgdds(11),dgdde(11)&
                       & ,dgddv(11),dgdde(9)/pchron,antes(1),antes(3),antes(4), &
                       & ddap(12),ddae(12),ddav(12),dgdds(12),dgdde(12),        &
                       & dgddv(12),dgdde(9)/pchron,mats(1),mats(3),mats(4),     &
                       & ddap(13),ddae(13),ddav(13),dgdds(13),dgdde(13),        &
                       & dgddv(13),dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),ddap(14)&
                       & ,ddae(14),ddav(14),dgdds(14),dgdde(14),dgddv(14),      &
                       & dgdde(9)/pchron                      !,
   !  . 'canopy height (cm)', 11x, i6)
 
end if
 
! write out spring wheat phenology resuts:
 
if (cname=='spring wheat') then
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,1300) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (14,1300) real(boots(1)),dgdde(9)/pchron
 
  write (14,1400)
!      leaf number table
 
  write (14,1500) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),ddae(2),dgdds(2)&
                & ,dgdde(2),dgdde(2)/pchron,srs(1),srs(3),srs(4),ddap(3),ddae(3)&
                & ,dgdds(3),dgdde(3),dgdde(3)/pchron,drs(1),drs(3),drs(4),      &
                & ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)/pchron,fps(1),     &
                & fps(3),fps(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)      &
                & /pchron,ies(1),ies(3),ies(4),ddap(6),ddae(6),dgdds(6),dgdde(6)&
                & ,dgdde(6)/pchron,tss(1),tss(3),tss(4),ddap(5),ddae(5),dgdds(5)&
                & ,dgdde(5),dgdde(5)/pchron,joints(1),joints(3),joints(4),      &
                & ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(8)/pchron,boots(1),   &
                & boots(3),boots(4),ddap(9),ddae(9),dgdds(9),dgdde(9),dgdde(9)  &
                & /pchron,heads(1),heads(3),heads(4),ddap(10),ddae(10),dgdds(10)&
                & ,dgdde(10),dgdde(9)/pchron,antss(1),antss(3),antss(4),ddap(11)&
                & ,ddae(11),dgdds(11),dgdde(11),dgdde(9)/pchron,antes(1),       &
                & antes(3),antes(4),ddap(12),ddae(12),dgdds(12),dgdde(12),      &
                & dgdde(9)/pchron,mats(1),mats(3),mats(4),ddap(13),ddae(13),    &
                & dgdds(13),dgdde(13),dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),     &
                & ddap(14),ddae(14),dgdds(14),dgdde(14),dgdde(9)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
 
! write out winter barley phenology results:
 
if (cname=='winter barley') then
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,1600) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (14,1600) real(boots(1)),dgdde(9)/pchron
 
  write (14,1700)
!      leaf number table
 
 
 
  write (14,1800) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),ddae(2),dgdds(2)&
                & ,dgdde(2),dgdde(2)/pchron,srs(1),srs(3),srs(4),ddap(3),ddae(3)&
                & ,ddav(3),dgdds(3),dgdde(3),dgddv(3),dgdde(3)/pchron,drs(1),   &
                & drs(3),drs(4),ddap(4),ddae(4),ddav(4),dgdds(4),dgdde(4),      &
                & dgddv(4),dgdde(4)/pchron,fps(1),fps(3),fps(4),ddap(7),ddae(7),&
                & ddav(7),dgdds(7),dgdde(7),dgddv(7),dgdde(7)/pchron,ies(1),    &
                & ies(3),ies(4),ddap(6),ddae(6),ddav(6),dgdds(6),dgdde(6),      &
                & dgddv(6),dgdde(6)/pchron,aifs(1),aifs(3),aifs(4),ddap(5),     &
                & ddae(5),ddav(5),dgdds(5),dgdde(5),dgddv(5),dgdde(5)/pchron,   &
                & joints(1),joints(3),joints(4),ddap(8),ddae(8),ddav(8),dgdds(8)&
                & ,dgdde(8),dgddv(8),dgdde(8)/pchron,boots(1),boots(3),boots(4),&
                & ddap(9),ddae(9),ddav(9),dgdds(9),dgdde(9),dgddv(9),dgdde(9)   &
                & /pchron,heads(1),heads(3),heads(4),ddap(10),ddae(10),ddav(10),&
                & dgdds(10),dgdde(10),dgddv(10),dgdde(9)/pchron,antss(1),       &
                & antss(3),antss(4),ddap(11),ddae(11),ddav(11),dgdds(11),       &
                & dgdde(11),dgddv(11),dgdde(9)/pchron,antes(1),antes(3),antes(4)&
                & ,ddap(12),ddae(12),ddav(12),dgdds(12),dgdde(12),dgddv(12),    &
                & dgdde(9)/pchron,mats(1),mats(3),mats(4),ddap(13),ddae(13),    &
                & ddav(13),dgdds(13),dgdde(13),dgddv(13),dgdde(9)/pchron,hrs(1),&
                & hrs(3),hrs(4),ddap(14),ddae(14),ddav(14),dgdds(14),dgdde(14), &
                & dgddv(14),dgdde(9)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
! write out spring barley phenology results:
 
if (cname=='spring barley') then
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,1900) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (14,1900) real(boots(1)),dgdde(9)/pchron
 
  write (14,2000)
!      leaf number table
 
 
  write (14,2100) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),ddae(2),dgdds(2)&
                & ,dgdde(2),dgdde(2)/pchron,srs(1),srs(3),srs(4),ddap(3),ddae(3)&
                & ,dgdds(3),dgdde(3),dgdde(3)/pchron,drs(1),drs(3),drs(4),      &
                & ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)/pchron,fps(1),     &
                & fps(3),fps(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)      &
                & /pchron,ies(1),ies(3),ies(4),ddap(6),ddae(6),dgdds(6),dgdde(6)&
                & ,dgdde(6)/pchron,aifs(1),aifs(3),aifs(4),ddap(5),ddae(5),     &
                & dgdds(5),dgdde(5),dgdde(5)/pchron,joints(1),joints(3),        &
                & joints(4),ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(8)/pchron,  &
                & boots(1),boots(3),boots(4),ddap(9),ddae(9),dgdds(9),dgdde(9), &
                & dgdde(9)/pchron,heads(1),heads(3),heads(4),ddap(10),ddae(10), &
                & dgdds(10),dgdde(10),dgdde(9)/pchron,antss(1),antss(3),antss(4)&
                & ,ddap(11),ddae(11),dgdds(11),dgdde(11),dgdde(9)/pchron,       &
                & antes(1),antes(3),antes(4),ddap(12),ddae(12),dgdds(12),       &
                & dgdde(12),dgdde(9)/pchron,mats(1),mats(3),mats(4),ddap(13),   &
                & ddae(13),dgdds(13),dgdde(13),dgdde(9)/pchron,hrs(1),hrs(3),   &
                & hrs(4),ddap(14),ddae(14),dgdds(14),dgdde(14),dgdde(9)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
 
! write out hay millet phenology results:
 
if (cname=='hay millet') then
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,2200) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (14,2200) real(boots(1)),dgdde(9)/pchron
 
  write (14,2300)
!      leaf number table
 
  write (14,2400) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),ddae(2),dgdds(2)&
                & ,dgdde(2),dgdde(2)/pchron,srs(1),srs(3),srs(4),ddap(3),ddae(3)&
                & ,dgdds(3),dgdde(3),dgdde(3)/pchron,drs(1),drs(3),drs(4),      &
                & ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)/pchron,fps(1),     &
                & fps(3),fps(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)      &
                & /pchron,ies(1),ies(3),ies(4),ddap(6),ddae(6),dgdds(6),dgdde(6)&
                & ,dgdde(6)/pchron,tss(1),tss(3),tss(4),ddap(5),ddae(5),dgdds(5)&
                & ,dgdde(5),dgdde(5)/pchron,joints(1),joints(3),joints(4),      &
                & ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(8)/pchron,boots(1),   &
                & boots(3),boots(4),ddap(9),ddae(9),dgdds(9),dgdde(9),dgdde(9)  &
                & /pchron,heads(1),heads(3),heads(4),ddap(10),ddae(10),dgdds(10)&
                & ,dgdde(10),dgdde(9)/pchron,antss(1),antss(3),antss(4),ddap(11)&
                & ,ddae(11),dgdds(11),dgdde(11),dgdde(9)/pchron,antes(1),       &
                & antes(3),antes(4),ddap(12),ddae(12),dgdds(12),dgdde(12),      &
                & dgdde(9)/pchron,mats(1),mats(3),mats(4),ddap(13),ddae(13),    &
                & dgdds(13),dgdde(13),dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),     &
                & ddap(14),ddae(14),dgdds(14),dgdde(14),dgdde(9)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
! write out proso millet phenology results:
if (cname=='proso millet') then
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,2500) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (14,2500) real(boots(1)),dgdde(9)/pchron
 
  write (14,2600)
 
 
  write (14,2800) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),ddae(2),dgdds(2)&
                & ,dgdde(2),dgdde(2)/pchron,srs(1),srs(3),srs(4),ddap(3),ddae(3)&
                & ,dgdds(3),dgdde(3),dgdde(3)/pchron,drs(1),drs(3),drs(4),      &
                & ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)/pchron,fps(1),     &
                & fps(3),fps(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)      &
                & /pchron,ies(1),ies(3),ies(4),ddap(6),ddae(6),dgdds(6),dgdde(6)&
                & ,dgdde(6)/pchron,tss(1),tss(3),tss(4),ddap(5),ddae(5),dgdds(5)&
                & ,dgdde(5),dgdde(5)/pchron,joints(1),joints(3),joints(4),      &
                & ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(8)/pchron,boots(1),   &
                & boots(3),boots(4),ddap(9),ddae(9),dgdds(9),dgdde(9),dgdde(9)  &
                & /pchron,heads(1),heads(3),heads(4),ddap(10),ddae(10),dgdds(10)&
                & ,dgdde(10),dgdde(9)/pchron,antss(1),antss(3),antss(4),ddap(11)&
                & ,ddae(11),dgdds(11),dgdde(11),dgdde(9)/pchron,antes(1),       &
                & antes(3),antes(4),ddap(12),ddae(12),dgdds(12),dgdde(12),      &
                & dgdde(9)/pchron,mats(1),mats(3),mats(4),ddap(13),ddae(13),    &
                & dgdds(13),dgdde(13),dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),     &
                & ddap(14),ddae(14),dgdds(14),dgdde(14),dgdde(9)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
! write out sorghum phenology reults:
 
if (cname=='sorghum') then
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(6)/pchron)
     write (14,2900) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer endlgs(1) to a real number
  write (14,2900) real(endlgs(1)),dgdde(6)/pchron
 
  write (14,3000)
!      leaf number table
 
 
  write (14,3100) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),ddae(2),dgdds(2)&
                & ,dgdde(2),dgdde(2)/pchron,gpds(1),gpds(3),gpds(4),ddap(5),    &
                & ddae(5),dgdds(5),dgdde(5),dgdde(5)/pchron,ies(1),ies(3),ies(4)&
                & ,ddap(3),ddae(3),dgdds(3),dgdde(3),dgdde(3)/pchron,joints(1), &
                & joints(3),joints(4),ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)&
                & /pchron,endlgs(1),endlgs(3),endlgs(4),ddap(6),ddae(6),dgdds(6)&
                & ,dgdde(6),dgdde(6)/pchron,antss(1),antss(3),antss(4),ddap(7), &
                & ddae(7),dgdds(7),dgdde(7),dgdde(6)/pchron,halfbs(1),halfbs(3),&
                & halfbs(4),ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(6)/pchron,  &
                & fullbs(1),fullbs(3),fullbs(4),ddap(9),ddae(9),dgdds(9),       &
                & dgdde(9),dgdde(6)/pchron,antes(1),antes(3),antes(4),ddap(12), &
                & ddae(12),dgdds(12),dgdde(12),dgdde(6)/pchron,mats(1),mats(3), &
                & mats(4),ddap(10),ddae(10),dgdds(10),dgdde(10),dgdde(6)/pchron,&
                & hrs(1),hrs(3),hrs(4),ddap(11),ddae(11),dgdds(11),dgdde(11),   &
                & dgdde(6)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
! write out corn phenology results:
 
if (cname=='corn') then
  print *,'in output_phenol in corn. ies(1) = ',ies(1)
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(7)/pchron)
     write (14,3200) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer antss(1) to a real number
  write (14,3200) real(antss(1)),dgdde(7)/pchron
 
  write (14,3300)
!      leaf number table
 
  write (14,3400) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),lf4s(1),lf4s(3),lf4s(4),ddap(2),ddae(2),     &
                & dgdds(2),dgdde(2),dgdde(2)/pchron,tsints(1),tsints(3),        &
                & tsints(4),ddap(3),ddae(3),dgdds(3),dgdde(3),dgdde(3)/pchron,  &
                & ears(1),ears(3),ears(4),ddap(4),ddae(4),dgdds(4),dgdde(4),    &
                & dgdde(4)/pchron,ies(1),ies(3),ies(4),ddap(5),ddae(5),dgdds(5),&
                & dgdde(5),dgdde(5)/pchron,lf12s(1),lf12s(3),lf12s(4),ddap(6),  &
                & ddae(6),dgdds(6),dgdde(6),dgdde(6)/pchron,antss(1),antss(3),  &
                & antss(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)/pchron,   &
                & silks(1),silks(3),silks(4),ddap(8),ddae(8),dgdds(8),dgdde(8), &
                & dgdde(7)/pchron,blstrs(1),blstrs(3),blstrs(4),ddap(9),ddae(9),&
                & dgdds(9),dgdde(9),dgdde(7)/pchron,milks(1),milks(3),milks(4), &
                & ddap(10),ddae(10),dgdds(10),dgdde(10),dgdde(7)/pchron,        &
                & doughs(1),doughs(3),doughs(4),ddap(11),ddae(11),dgdds(11),    &
                & dgdde(11),dgdde(7)/pchron,dents(1),dents(3),dents(4),ddap(12),&
                & ddae(12),dgdds(12),dgdde(12),dgdde(7)/pchron,mats(1),mats(3), &
                & mats(4),ddap(13),ddae(13),dgdds(13),dgdde(13),dgdde(7)/pchron,&
                & hrs(1),hrs(3),hrs(4),ddap(14),ddae(14),dgdds(14),dgdde(14),   &
                & dgdde(7)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
if (cname=='sunflower') then
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (14,3500) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer antss(1) to a real number
  write (14,3500) real(antss(1)),dgdde(9)/pchron
 
  write (14,3600)
!      leaf number table
 
  write (14,3700) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),ems(4),     &
                & ddap(1),dgdds(1),lf4s(1),lf4s(3),lf4s(4),ddap(2),ddae(2),     &
                & dgdds(2),dgdde(2),dgdde(2)/pchron,lf8s(1),lf8s(3),lf8s(4),    &
                & ddap(3),ddae(3),dgdds(3),dgdde(3),dgdde(3)/pchron,lf12s(1),   &
                & lf12s(3),lf12s(4),ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)  &
                & /pchron,infls(1),infls(3),infls(4),ddap(5),ddae(5),dgdds(5),  &
                & dgdde(5),dgdde(5)/pchron,ies(1),ies(3),ies(4),ddap(6),ddae(6),&
                & dgdds(6),dgdde(6),dgdde(6)/pchron,ies2(1),ies2(3),ies2(4),    &
                & ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)/pchron,opens(1),   &
                & opens(3),opens(4),ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(8)  &
                & /pchron,antss(1),antss(3),antss(4),ddap(9),ddae(9),dgdds(9),  &
                & dgdde(9),dgdde(9)/pchron,antes(1),antes(3),antes(4),ddap(10), &
                & ddae(10),dgdds(10),dgdde(10),dgdde(9)/pchron,yelows(1),       &
                & yelows(3),yelows(4),ddap(11),ddae(11),dgdds(11),dgdde(11),    &
                & dgdde(9)/pchron,browns(1),browns(3),browns(4),ddap(12),       &
                & ddae(12),dgdds(12),dgdde(12),dgdde(9)/pchron,mats(1),mats(3), &
                & mats(4),ddap(13),ddae(13),dgdds(13),dgdde(13),dgdde(9)/pchron,&
                & hrs(1),hrs(3),hrs(4),ddap(14),ddae(14),dgdds(14),dgdde(14),   &
                & dgdde(9)/pchron
!     . 'canopy height (cm)', 11x, i6)
 
end if
 
close (unit=14)
 1000 format (40x,f5.1,6x,f4.1)
 1100 format (/1x) ! write a blank line after outputting the
 1200 format (' planting date',18x,i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,&
             &'/',i2,1x,i4,21x,f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,  &
            & i4,4x,i4,13x,f6.1,5x,f6.1,15x,f6.1,/1x,'single ridge',19x,i4,2x,  &
            & i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,  &
             &'double ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'floret primordia init begins',3x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,  &
             &'stem elongation begins',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'end spikelet initiation',8x,i4, &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,  &
             &'anthesis starts',16x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',18x,i4,2x,i2,'/',i2,  &
            & 1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,            &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
                                                            !,
 1300 format (40x,f5.1,6x,f4.1)
 1400 format (/1x) ! write a blank line after outputting the
!     ,  icanht
 1500 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,24x,    &
            & f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'single ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'double ridge',19x,i4,2x,i2,'/',i2,&
            & 1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                          &
             &'floret primordia init begins',3x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'stem elongation begins',9x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'end spikelet initiation',8x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1, &
            & 5x,f6.1,4x,f6.1,/1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,&
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x,i2,'/',i2,1x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 1600 format (40x,f5.1,6x,f4.1)
 1700 format (/1x) ! write a blank line after outputting the
!     .  icanht
 1800 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'dav',5x,'gdd ap',5x,'gdd ae',5x,'gdd av',5x,'nolvs',    &
             &/1x'planting date',18x,i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x, &
            & i2,'/',i2,1x,i4,21x,f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,  &
            & 1x,i4,4x,i4,13x,f6.1,5x,f6.1,15x,f6.1,/1x,'single ridge',19x,i4,  &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'double ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1, &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'floret primordia init begins',3x,i4, &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'stem elongation begins',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,&
            & 5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'awn initials formed',12x,i4, &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,  &
             &'anthesis starts',16x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',18x,i4,2x,i2,'/',i2,  &
            & 1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,            &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 1900 format (40x,f5.1,6x,f4.1)
 2000 format (/1x) ! write a blank line after outputting the
!     .  icanht
 2100 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,24x,    &
            & f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'single ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'double ridge',19x,i4,2x,i2,'/',i2,&
            & 1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                          &
             &'floret primordia init begins',3x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'stem elongation begins',9x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'awn initials formed', &
            & 12x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,    &
            & f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,    &
            & f6.1,4x,f6.1,/1x,'heading',24x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,    &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x,i2,'/',i2,1x,&
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',18x,i4,2x,i2,&
             &'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                   &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 2200 format (40x,f5.1,6x,f4.1)
 2300 format (/1x) ! write a blank line after outputting the
!     ,  icanht
 2400 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,24x,    &
            & f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'single ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'double ridge',19x,i4,2x,i2,'/',i2,&
            & 1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                          &
             &'floret primordia init begins',3x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'stem elongation begins',9x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'end spikelet initiation',8x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1, &
            & 5x,f6.1,4x,f6.1,/1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,&
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x,i2,'/',i2,1x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 2500 format (40x,f5.1,6x,f4.1)
 2600 format (/1x) ! write a blank line after outputting the
!      leaf number table
 
 2700 format (40x,f5.1,6x,f4.1)
!     ,  icanht
 2800 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,24x,    &
            & f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'single ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'double ridge',19x,i4,2x,i2,'/',i2,&
            & 1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                          &
             &'floret primordia init begins',3x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'stem elongation begins',9x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'end spikelet initiation',8x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1, &
            & 5x,f6.1,4x,f6.1,/1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,&
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x,i2,'/',i2,1x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 2900 format (40x,f5.1,6x,f4.1)
 3000 format (/1x) ! write a blank line after outputting the
!     .  icanht
 3100 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs'/1x'planting date',18x,i4,&
            & 2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,24x,f6.1,  &
            & /1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,    &
            & f6.1,4x,f6.1,/1x,'growing point differentiation',2x,i4,2x,i2,'/', &
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'internode elongation begins',4x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'end of leaf growth',13x,i4,2x,i2, &
             &'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts', &
            & 16x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'half bloom',21x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,  &
            & f6.1,/1x,'full bloom',21x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x, &
            & f6.1,4x,f6.1,/1x,'anthesis ends',18x,i4,2x,i2,'/',i2,1x,i4,4x,i4, &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'physiological maturity',9x,i4,2x,i2, &
             &'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',   &
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 3200 format (40x,f5.1,6x,f4.1)
 3300 format (/1x) ! write a blank line after outputting the
!     ,  icanht
 3400 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,13x,    &
            & f6.1,/1x,'leaf 4 (v4)',20x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,&
            & f6.1,4x,f6.1,/1x,'tassel initiation',14x,i4,2x,i2,'/',i2,1x,i4,4x,&
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'ear initiation',17x,i4,2x,i2,'/', &
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'internode elongation begins',4x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'leaf 12 (v12)',18x,i4,2x,i2,'/',i2,1x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'tasseling',22x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'silking (r1)',19x,i4, &
            & 2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,             &
             &'blister (r2)',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,&
            & f6.1,/1x,'milk (r3)',22x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,4x,f6.1,/1x,'dough (r4)',21x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'dent (r5)',22x,i4,2x,i2,'/',i2,1x,i4,4x,&
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'maturity (r6)',18x,i4,2x,i2,'/',  &
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,&
            & 2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 3500 format (40x,f5.1,6x,f4.1)
 3600 format (/1x) ! write a blank line after outputting the
!     .  icanht
 3700 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,13x,    &
            & f6.1,/1x,'leaf 4',25x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,&
            & 4x,f6.1,/1x,'leaf 8',25x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,4x,f6.1,/1x,'leaf 12',24x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,    &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'inflorescence visible',10x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'internode elongation',&
            & 11x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'internode elongation > 2',7x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,&
            & 5x,f6.1,4x,f6.1,/1x,'inflorescence opens',12x,i4,2x,i2,'/',i2,1x, &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'head back yellow',15x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,    &
            & f6.1,4x,f6.1,/1x,'head yellow brown',14x,i4,2x,i2,'/',i2,1x,i4,4x,&
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'physiological maturity',9x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
!
end subroutine output_phenol
