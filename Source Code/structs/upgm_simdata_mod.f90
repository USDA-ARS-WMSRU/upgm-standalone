!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: module that contains simulation data/controls for UPGM
!
!   use: module should be declared in main initialization area, passed around
!
!------------------------------------------------------------------------------
    
    module upgm_simdata
    implicit none

    ! simulation data
    type simulation
        integer :: plant_day    ! planting day
        integer :: plant_mon    ! planting month
        integer :: plant_year   ! planting year.  currently, not the calendar year.
        integer :: init_day     ! initial simulation day
        integer :: init_mon     ! initial sim month
        integer :: init_year    ! initial sim year
        integer :: julday       ! julian day
        
        integer :: tisr         ! The last accessed day of simulation month.
        integer :: tday         ! The last accessed month of simulation year.
        integer :: tmo          ! The last accessed year of simulation run.
        integer :: tyr          ! The last accessed subregion index.
        
        real(kind=4) :: amalat  !  Latitude of simulation site (degrees)
    end type simulation
    
    type controls
        type(simulation) :: sim
    end type controls
    
    type(controls) :: upgm_ctrls
    
    end module upgm_simdata
    
!------------------------------------------------------------------------------  