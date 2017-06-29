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
    use stress, only : crop_stress
    implicit none

    ! simulation data
    type simulation
        integer :: plant_day    ! planting day
        integer :: plant_mon    ! planting month
        integer :: plant_year   ! planting year.  currently, not the calendar year.
        integer :: juldate       ! julian date
        
        
        integer :: tisr         ! The last accessed day of simulation month.
        integer :: tday         ! The last accessed month of simulation year.
        integer :: tmo          ! The last accessed year of simulation run.
        integer :: tyr          ! The last accessed subregion index.
        
        real(kind=4) :: amalat  !  Latitude of simulation site (degrees)
        
        integer :: cook_yield = 1 ! default to using functional yield/residue ratio info
!     cook_yield - flag setting which uses input from crop record to 
!                  guarantee a fixed yield/redsidue ratio at harvest
!                  (this is cooking the books :-(
        real :: water_stress_max = 0.0
!     water_stress_max - Cap water stress at some maximum value
!                  (note maximum stress occurs at 0.0 and minimum stress at 1.0)
!                   water_stress_max = x.xx   ! specified stress limit        
        integer :: winter_ann_root = 1 ! RMarquez 06/13/2017 -> changed this to 1, has some impact on wheat.
        !select root growth option for winter annuals
        !winter_ann_root = 0                                    ! root depth grows at same rate as height
        !winter_ann_root = 1                                    ! root depth grows with fall heat units
        integer :: am0cdb  ! flag to print CROP variables before and after call to CROP
        
        character(len=80) :: ac0nam
    end type simulation
    
    type controls
        type(simulation) :: sim
        type(crop_stress) :: cropstress
    end type controls
    
    type(controls) :: upgm_ctrls
    
    end module upgm_simdata
    
!------------------------------------------------------------------------------  