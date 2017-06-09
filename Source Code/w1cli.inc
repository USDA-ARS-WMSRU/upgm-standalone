!$Author: wagner $
!$Date: 2002-08-30 21:48:07 $
!$Revision: 1.5 $
!$Source: /weru/cvs/weps/weps.src/inc/main/w1cli.inc,v $
!     This common block contains the buffers that cligen data is read into

!     Edit History
!     08-Mar-99   wjr   created
!
!     Parameter include files:

!     + + + VARIABLE DECLARATIONS + + +

      integer  wcd(mndayr) !init but not used
      integer  wcm(mndayr) !init but not used
      integer  wcy(mndayr) !init but not used
      integer  maxday !used to read in climate
      integer  n_header !used to read in cligen stuff
      
      real     wwzdpt(mndayr) !not used
      real     wwdurpt(mndayr)
      real     wwpeaktpt(mndayr)
      real     wwpeakipt(mndayr)
      real     wwtdmx(mndayr)
      real     wwtdmn(mndayr)
      real     wgrad(mndayr)
      real     wwtdpt(mndayr)
      
      common   /w1cli/                                                  &
     &  wwzdpt, wwtdmx, wwtdmn, wgrad,                                  &
     &  wwtdpt, wwdurpt, wwpeaktpt, wwpeakipt,                          &
     &  wcd, wcm, wcy, maxday, n_header
      
! the following definitions don't match the variables declared.  
! this should be fixed.      

!     + + + VARIABLE DEFINITIONS + + +

!     wcd    - day 
!     wcm    - month
!     wcy    - year
!     maxday - maximum number of days in the year
!     n_header - number of lines in the cligen file header

!     wwzdpt - Daily precipitation (mm)
!     wwdurpt - Duration of Daily precipitation (hours)
!     wwpeaktpt - Normalized time to peak of Daily
!                 precipitation (time to peak/duration)
!     wwpeakipt - Normalized intensity of peak Daily
!                 precipitation (peak intensity/average intensity)
!     wwtdmx - Maximum daily air temperature (deg C)
!     wwtdmn - Minimum daily air temperature (deg C)
!     wgrad - Daily global radiation (langleys/day) (as in cligen file)
!     wwtdpt - Daily dew point air temperature (deg C)




