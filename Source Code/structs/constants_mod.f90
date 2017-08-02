!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: module that contains named constants used in the program
!
!   use: include constant module and variables are available.
!
!------------------------------------------------------------------------------
    
    module constants
    implicit none
    
    ! general constants
    real, parameter :: pi = 4 * atan (1.0)          ! value of pi in radians
    integer, parameter :: juliantogreg = 2299161
    integer, parameter :: gregtojulian = 15 + 31*(10+12*1582)
    
    ! unit conversion constants
    real, parameter :: mmtom = 1.0 / 1000.0         ! mm to m
    real, parameter :: degtorad = pi/180.0          ! degrees to radians
    real, parameter :: radtodeg = 180.0/pi          ! radians to degrees
    real, parameter :: hatom2 = 10000.0             ! Hectares to m^2
    real, parameter :: mgtokg = 1.0 / 1000000.0     ! mg to kg  
    real, parameter :: langleydaytomj = 0.04186     ! langleys/day to mega joules
    
    ! solar constants
    real, parameter :: civilrise = 96.0             ! solar altitude angle defined as civil twilight
    
    ! math precision constants
    real, parameter :: max_real = huge(1.0)*0.999150
    real, parameter :: max_arg_exp = log(max_real)
    
    ! dimension constants
    integer, parameter :: mncz = 5  
    integer, parameter :: mnsub = 4        
    integer, parameter :: mnsz = 100
    integer, parameter :: mntime = 1400
    integer, parameter :: mnbpls = 5
    integer, parameter :: mndk = 5
    integer, parameter :: mnhhrs = 24
    integer, parameter :: num_months = 12
    integer, parameter :: mndayr = 366
    
    
    integer, parameter :: int32 = kind(4)
    integer, parameter :: int64 = kind(8)
    integer, parameter :: sp = kind(4)
    integer, parameter :: dp = kind(8)          ! double precision for use with kind and real
    
    end module constants
    
!------------------------------------------------------------------------------  