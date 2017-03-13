SUBROUTINE test_crop_getcli(ccd,ccm,ccy,icli)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'file.fi'
INCLUDE 'w1clig.inc'
INCLUDE 'm1sim.inc'
INCLUDE 'w1pavg.inc'
INCLUDE 'w1cli.inc'
!
! Subroutine arguments
!
INTEGER :: ccd,ccm,ccy,icli
!
! Local variables
!
INTEGER :: chkid,chkim,chkiy,dayidx,i,ioc
REAL :: dummy
CHARACTER(80) :: header
!
!     + + + local common blocks + + +
!
!     + + + arguments + + +
 
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
DATA dayidx/0/
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
IF (icli.EQ.0) THEN  ! read in historical precip, tmax, tmin, and solar radiation
!  IF ((ccd.eq.29).and.(ccm.eq.2)) THEN
!    dayidx = ccd
!    READ (7,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),wwzdpt(dayidx),wwtdmx(dayidx), &
!           & wwtdmn(dayidx),wgrad(dayidx)
!
!    ccd = 1
!    ccm = 3
!
!  END IF
    
  dayidx = ccd
!
  READ (7,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),wwzdpt(dayidx),wwtdmx(dayidx), &
           & wwtdmn(dayidx),wgrad(dayidx)

  IF ((wcd(dayidx).eq.29).and.(wcm(dayidx).eq.2)) THEN
!   print*, ' in if statement checking for leap year'
   READ (7,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),wwzdpt(dayidx),wwtdmx(dayidx),&
           & wwtdmn(dayidx),wgrad(dayidx)
  END IF        

! IF ((wcm(dayidx).lt.4).and.(wcy(dayidx).eq.2)) THEN
! print*, 'day= ', wcd(dayidx), 'month= ', wcm(dayidx), 'tmax= ', wwtdmx(dayidx)
! ENDIF
!
! read (7,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),                               &
!         &  wwzdpt(dayidx),wwdurpt(dayidx),wwpeaktpt(dayidx)                   &
!         &  ,wwpeakipt(dayidx),wwtdmx(dayidx),wwtdmn(dayidx)                   &
!         &  ,wgrad(dayidx),dummy,dummy,wwtdpt(dayidx)
!
  IF ((wcd(dayidx).NE.ccd).OR.(wcm(dayidx).NE.ccm).OR.(wcy(dayidx).NE.ccy)) THEN
     WRITE (*,*) 'error in dates in historical climate file - stop'
     CALL exit(1)
  END IF
!
! set dummy values for other variables so they won't blow up in debug mode
!
  wwdurpt(dayidx) = 0.0
  wwpeaktpt(dayidx) = 0.0
  wwpeakipt(dayidx) = 0.0
  wwtdpt(dayidx) = 0.0
!
END IF
!
IF (icli.EQ.1) THEN  ! read from standard cligen climate file
  IF (dayidx.EQ.0) THEN
!
     REWIND luicli
!  n_header = 15        !we are assuming that we are only working with latest cligen (forest service)
     n_header = 16
!debe 031309 changed above to header = 16 to allow for climate file name
! entered in the first row of the header.
!
     DO i = 1,n_header
!   read (luicli,1000,err=30) header
        READ (luicli,1000) header
     END DO
  END IF
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
  READ (luicli,*) wcd(dayidx),wcm(dayidx),wcy(dayidx),wwzdpt(dayidx),           &
                & wwdurpt(dayidx),wwpeaktpt(dayidx),wwpeakipt(dayidx),          &
                & wwtdmx(dayidx),wwtdmn(dayidx),wgrad(dayidx),dummy,dummy,      &
                & wwtdpt(dayidx)
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
END IF
!
awzdpt = wwzdpt(dayidx)
awdurpt = wwdurpt(dayidx)
awpeaktpt = wwpeaktpt(dayidx)
awpeakipt = wwpeakipt(dayidx)
!
IF (dayidx.GT.1) THEN
  awtdmxprev = wwtdmx(dayidx-1)
ELSE
  awtdmxprev = wwtdmx(dayidx)
END IF
!
awtdmn = wwtdmn(dayidx)
awtdmx = wwtdmx(dayidx)
!
IF (dayidx.LT.maxday) THEN
  awtdmnnext = wwtdmn(dayidx+1)
ELSE
  awtdmnnext = wwtdmn(dayidx)
END IF
!
awtdpt = wwtdpt(dayidx)
aweirr = wgrad(dayidx)*0.04186
!dayidx = dayidx + 1
!
! calculate air density from temperature and pressure
!
awtdav = (awtdmx+awtdmn)/2.
awdair = 348.56*(1.013-0.1183*(amzele/1000.)+0.0048*(amzele/1000.)**2.)         &
       & /(awtdav+273.1)
!
RETURN
!
! error returns and stops
!
! 30   write (0,*) 'unexpected error in cligen header'
!call exit(1)
! 40   write (0,*) 'unexpected error reading cligen file day ',dayidx
!call exit(1)
!
 1000 FORMAT (a80)
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
END SUBROUTINE test_crop_getcli
