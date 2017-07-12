!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: module that contains support functions for the growth model
!
!   use: make appropriate calls to subroutines as necessary
!
!------------------------------------------------------------------------------
    module support
    use constants, only : dp
    implicit none
    
    interface
        ! + + + function daylen(...) + + +
        ! this function calculates the daylength (hours) for any simulation
        ! site based on the global position of the site, and day of the
        ! year.  the inputs for the function are day of the year, and latitude
        ! of the site.
        module function daylen(dlat, idoy, riseangle) result(dylen)
            ! arguments
            real,intent(in) :: dlat        ! latitude of the site, degrees (north > 0, south < 0) 
            integer,intent(in) :: idoy     ! day of year 
            real,intent(in) :: riseangle   ! angle of earths rotation where sunrise occurs.
            ! this varies depending on calculation of direct beam, civil twilight, nautical twilight or astronomical twilight daylen
            real :: dylen      ! the length of the day
        end function daylen
    
    
    
    end interface
    
    contains
    
    module function daylen(dlat,idoy,riseangle)  result (dylen)
    !
    implicit none
    ! arguments
    real,intent(in) :: dlat        ! latitude of the site, degrees (north > 0, south < 0)
    integer,intent(in) :: idoy     ! day of year
    real,intent(in) :: riseangle   ! angle of earths rotation where sunrise occurs.
    ! this varies depending on calculation of direct beam, civil twilight, nautical twilight or astronomical twilight daylen
    real :: dylen      ! the length of the day
    ! locals
    real :: dec             ! dec - declination of earth with respect to the sun (degrees)
    real :: h               ! h - hour angle (degrees)
    real :: declination     ! declination -
    real :: hourangle       ! hourangle - angle of sun for a particular hour
    ! start function
    dec = declination(idoy)
    !     sunrise or sunset hour angle
    h = hourangle(dlat,dec,riseangle)
    !     calculate the length of the day
    dylen = 2.0*h/15.0
    !
    end function daylen

 
    
    
    
    
    end module support
    
!------------------------------------------------------------------------------  