Subroutine climate_input(clidat, ccd,ccm,ccy,icli)
!
    use constants, only : langleydaytomj
    use climate, only : n_header, climate_data
implicit none
!
include 'file.fi'
!include 'w1clig.inc'
!include 'w1cli.inc'
!
! Dummy arguments
!
    type(climate_data) :: clidat
integer :: ccd,ccm,ccy,icli
!
! Local variables
!
integer :: dayidx = 0
integer :: i
real :: dummy
character(80) :: header
!
!     + + + argument definitions + + +
!     ccd - day
!     ccm - month
!     ccy - year
!     icli - used to determine if the climate file is generated or
!            historical.  if the value is 1, it is a cligen weather file.
!            if the value is not 1, it is a historical weather file.
 
!     + + + local variable definitions + + +
!     chkid - not currently used in this subroutine
!     chkim - not currently used in this subroutine
!     chkiy - not currently used in this subroutine
!     dayidx - index for arrays based on the day of the month
!     dummy -
!     header - holds the header information read in from a climate file
!     ioc -
!     isleap - to check if it is a leap year. not currently  used.
!     wgrad - global radiation (ly/day) as read in from cligen.
!     wrnflg - not currently used
!
!     + + + common block variables definitions + + +
!     amzele - average site elevation (m)
!     awdair - average air density (kg/m^3) for the month
!     awdurpt - same as wwdurpt - duration of daily precipitation
!     aweirr - daily global radiation (mj/m^2)
!     awpeakipt - same as wwpeakipt - normalized intensity of peak daily
!                 precipitation (peak intensity/average intensity)
!     awpeaktpt - same as wwpeaktpt - normalized time to peak of daily
!                 precipitation (time to peak/duration)
!     awtdav - mean daily air temperature (deg c)
!     awtdmn - minimum daily air temperature (deg c)
!     awtdmnnext - minimum daily air temperature of the next day (deg c)
!     awtdmx - maximum daily air temperature (deg c)
!     awtdmxprev - maximum daily air temperature of the previous day
!                  (deg c)
!     awtdpt - daily dew point air temperature (deg c)
!     awzdpt - daily precipitation (mm)
!     maxday - maximum number of days in the year
!     n_header - number of lines in the cligen file header
!     wcd - day array
!     wcm - month array
!     wcy - year array
!     wwdurpt - duration of daily precipitation (hours)
!     wwpeakipt - normalized intensity of peak daily
!                 precipitation (peak intensity/average intensity)
!     wwpeaktpt - normalized time to peak of daily
!                 precipitation (time to peak/duration)
!     wwtdmn - minimum daily air temperature (deg c)
!     wwtdmx - maximum daily air temperature (deg c)
!     wwtdpt - daily dew point air temperature (deg c)
!     wwzdpt - daily precipitation (mm)
 
!     + + + function declarations + + +
!     dayear
!     lstday
 
!     dayear, lstday declared here but commented out in code jcaii 8/08/08
!data wrnflg/.true./
 
! this code added to re-initialize the reading of a cligen file
! following the "initialization" phase.  it is triggered if the
! day (ccd) passed to the subroutine is set to zero - lew
!
!if (ccd.eq.0) then
!  dayidx = 0
!  rewind luicli
!  return
!end if
!
if (icli==0) then    ! read in historical precip, tmax, tmin, and solar radiation
!  if ((ccd.eq.29).and.(ccm.eq.2)) then
!    dayidx = ccd
!    read (upgmcli,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),wwzdpt(dayidx),wwtdmx(dayidx), &
!           & wwtdmn(dayidx),wgrad(dayidx)
!
!    ccd = 1
!    ccm = 3
!
!  end if
 
  dayidx = ccd
!
  read (upgmcli,*) clidat%wcd(dayidx),clidat%wcm(dayidx),clidat%wcy(dayidx),clidat%wwzdpt(dayidx),clidat%wwtdmx(dayidx), &
           & clidat%wwtdmn(dayidx),clidat%wgrad(dayidx)
 
!   print*, ' in if statement checking for leap year'
  if ((clidat%wcd(dayidx)==29).and.(clidat%wcm(dayidx)==2)) read (upgmcli,*) clidat%wcd(dayidx),clidat%wcm(dayidx)&
    & ,clidat%wcy(dayidx),clidat%wwzdpt(dayidx),clidat%wwtdmx(dayidx),clidat%wwtdmn(dayidx),clidat%wgrad(dayidx)
 
! if ((wcm(dayidx).lt.4).and.(wcy(dayidx).eq.2)) then
! print*, 'day= ', wcd(dayidx), 'month= ', wcm(dayidx), 'tmax= ', wwtdmx(dayidx)
! endif
!
! read (upgmcli,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),                               &
!         &  wwzdpt(dayidx),wwdurpt(dayidx),wwpeaktpt(dayidx)                   &
!         &  ,wwpeakipt(dayidx),wwtdmx(dayidx),wwtdmn(dayidx)                   &
!         &  ,wgrad(dayidx),dummy,dummy,wwtdpt(dayidx)
!
  if ((clidat%wcd(dayidx)/=ccd).or.(clidat%wcm(dayidx)/=ccm).or.(clidat%wcy(dayidx)/=ccy)) then
     write (*,*) 'error in dates in historical climate file - stop'
     call exit(1)
  end if
!
end if
!
if (icli==1) then    ! read from standard cligen climate file
  if (dayidx==0) then
!
     rewind luicli
!debe 031309 changed above to header = 16 to allow for climate file name
! entered in the first row of the header.
!
     do i = 1,n_header
!   read (luicli,1000,err=30) header
        read (luicli,1000) header
     end do
  end if
!
! load data buffers if it is the first day of a year
!20   if ((ccd.eq.1).and.(ccm.eq.1)) then
! maxday = 365
! if (isleap(ccy)) maxday = 366
! do dayidx = 1,maxday
!    ioc = 0
!
  dayidx = ccd
!
!    read (luicli,*,iostat=ioc) wcd(dayidx),wcm(dayidx),wcy(dayidx),            &
  read (luicli,*) clidat%wcd(dayidx),clidat%wcm(dayidx),clidat%wcy(dayidx),clidat%wwzdpt(dayidx),           &
                & dummy,dummy,dummy,          &
                & clidat%wwtdmx(dayidx),clidat%wwtdmn(dayidx),clidat%wgrad(dayidx),dummy,dummy,      &
                & dummy
!
! 1030     format (2(2x,i2),1x,i4,1x,2f6.2,f5.2,1x,f6.2,3f7.2,f6.2,
!     &      2f7.2)
          !write(*,*) 'dayidx,maxday,ccy: ',dayidx,maxday,ccy
!
!    if (ioc.eq.-1) then         ! we have a failure reading the file
!
!           !check if only one 365 day year in cligen file
!       if (dayidx.eq.366) then
!              !since we are short, we will set the 366th day values
!              !to the last day values read in
!          wcd(dayidx) = wcd(dayidx-1)
!          wcm(dayidx) = wcm(dayidx-1)
!          wcy(dayidx) = wcy(dayidx-1)
!          wwzdpt(dayidx) = wwzdpt(dayidx-1)
!          wwdurpt(dayidx) = wwdurpt(dayidx-1)
!          wwpeaktpt(dayidx) = wwpeaktpt(dayidx-1)
!          wwpeakipt(dayidx) = wwpeakipt(dayidx-1)
!          wwtdmx(dayidx) = wwtdmx(dayidx-1)
!          wwtdmn(dayidx) = wwtdmn(dayidx-1)
!          wgrad(dayidx) = wgrad(dayidx-1)
!          wwtdpt(dayidx) = wwtdpt(dayidx-1)
!          write (6,*) 'weps thinks it is a leap year and cligen does not.'
!          write (6,*) 'so we just reuse day 365 out of the cligen file.'
!              write(6,2030) !print heading
!              write(6,2040) !print added cligen day
!    &           dayidx, wcd(dayidx), wcm(dayidx), wcy(dayidx)
!              write(6,2050) !print weps date
!    &           dayear(lstday(ccm,ccy),12,ccy),lstday(ccm,ccy),12,ccy
!       else if ((ccd.eq.1).and.(ccm.eq.1).and.(ccy.ne.1)) then
!          rewind luicli
!          write (6,1300)   !print heading
              !print cligen date
!          write (6,1400) dayidx,wcd(dayidx),wcm(dayidx),wcy(dayidx)
!          write (6,1600)   !print end of cligen file message
!          go to 10
!       else
!          go to 40
!       end if
!    end if
! end do
!  dayidx = 1
!end if
!
      !hmm, bill has this being done only once when it gets triggered.
!if (wrnflg) then
!  if ((wcd(dayidx).ne.ccd).or.(wcm(dayidx).ne.ccm).or.(wcy(dayidx).ne.ccy)) then
!     write (*,1100)
!     write (6,1200) ccd,ccm,ccy,wcd(dayidx),wcm(dayidx),wcy(dayidx)
!     wrnflg = .false.
!  end if
!end if
!
end if
!
clidat%awzdpt = clidat%wwzdpt(dayidx)
!
clidat%awtdmn = clidat%wwtdmn(dayidx)
clidat%awtdmx = clidat%wwtdmx(dayidx)
!
clidat%aweirr = clidat%wgrad(dayidx)*langleydaytomj
!dayidx = dayidx + 1
!
return
!
! error returns and stops
!
! 30   write (0,*) 'unexpected error in cligen header'
!call exit(1)
! 40   write (0,*) 'unexpected error reading cligen file day ',dayidx
!call exit(1)
!
 1000 format (a80)
!1100 format (' warning !',28x,' day       month       year')
!1200 format (' current simulation date -              ',i2,9x,i2,8x,i4,/,      &
!            &' does not match current cligen date -   ',i2,9x,i2,8x,i4,/)
!
!1300 format (' warning !',12x,' day-of-year day       month       year')
!1400 format (' current cligen date - ',i3,9x,i2,9x,i2,8x,i4)
!1500 format (' current weps   date - ',i3,9x,i2,9x,i2,8x,i4)
!1600 format (' is beyond the end of file - ','rewinding to top of cligen file',&
!           & /)
!
end subroutine climate_input
