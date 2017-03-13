SUBROUTINE cpout
!
IMPLICIT NONE
!
INCLUDE 'file.fi'
INCLUDE 'm1flag.inc'
!
!     author : a. retta - 11/19/96
!     + + + purpose + + +
!     prints headers for the crop submodel output files

!     + + + common block variable definitions + + +
!     am0cfl - flag to print CROP output

      ! season.out headers
WRITE (luoseason,1500)
WRITE (luoseason,1600)
WRITE (luoseason,1700)
 
IF (am0cfl.GT.0) THEN
 
         ! crop.out headers
  WRITE (luocrop,1000)
  WRITE (luocrop,1100)
  WRITE (luocrop,1200)
 
         ! shoot.out headers
  WRITE (luoshoot,1300)
  WRITE (luoshoot,1400)
 
         ! inpt.out headers
  WRITE (luoinpt,1800)
  WRITE (luoinpt,1900)
  
         ! debe added for emergence output headers
  WRITE (luoemerge,2000)
  WRITE (luoemerge,2100)
  WRITE (luoemerge,2200)
  
         ! debe added for phenology output headers
  WRITE (luophenol, 2300)
  WRITE (luophenol, 2600)

        ! debe added for canopy height output
  WRITE (luocanopyht, 2700)
  WRITE (luocanopyht, 2800)
  
END IF
 
!     + + + output formats + + +
 1000 FORMAT (                                                                  &
     &'#                           stand   stand   stand   flat    flat    flat &
     &   root    root  bel.grnd  total   total')
 1100 FORMAT (                                                                  &
     &'#daysim doy year dap heatui stem    leaf    store   stem    leaf    store&
     &   store   fiber   stem    leaf    stem    height  #stem   lai     eff_lai&
     & rootd  grainf tempst watstf  frost  ffa    ffw   par     apar     massinc&
     &    p_rw   p_st   p_lf   p_rp  stdflt pdiam  parea  fpdiam fparea hu_del c&
     &rop     gddday')
 1200 FORMAT (                                                                  &
     &'#                           kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^&
     &2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  meters          m^2/m^2 m^2/m^2&
     & meters                                           mj/m^2  mj/m^2   kg/plnt&
     &                                      meters m^2')
 
 1300 FORMAT ('#daysim doy year dap heatui ',                                   &
             &'s_root_sum f_root_sum tot_mass_req end_shoot_mass ',             &
             &'end_root_mass d_root_mass d_shoot_mass d_s_root_mass ',          &
             &'end_stem_mass end_stem_area end_shoot_len bczshoot ',            &
             &'bcmshoot bcdstm bc0nam gddday')!debe 090308 added gddday to output daily gdd values
 1400 FORMAT ('#(dy) (dy) (yr) (dy) (c)    ',                                   &
             &'(kg/m^2)   (kg/m^2)   (mg/shoot)   (mg/shoot)     ',             &
             &'(mg/shoot)    (mg/shoot)  (mg/shoot)   (mg/shoot)    ',          &
             &'(mg/shoot)    (m^2/shoot)   (m)           (m)      ',            &
             &'(kg/m^2) (#/m^2)')
 
 1500 FORMAT (                                                                  &
     &'#     standing                flat                    root               &
     &                      root')
 1600 FORMAT (                                                                  &
     &'#year stem    leaf    store   stem    leaf    store   stem    store   fib&
     &er   height stemcount depth   grainf  stmrepd dapl chill  hucum   mxhu hui&
     &nd dafm spring crop_name      yield       units    total biomass  HI     & 
     & internode elongation jointing booting heading anthesis maturity harvest &
     &ready')
!debe added yield and units, final total biomass and formatting to output
 1700 FORMAT (                                                                  &
     &'#     kg/m^2  -----------------------------------------------------------&
     &--   meter  #/m^2     meter   ------  meter  ----  deg_c  deg_c  deg_c ---&
     &-- ---- ------ ---------      --------    ------   kg/m^2         -----   &
     &-------------------- -------- ------- ------- -------- -------- ----------&
     &---')
 
 1800 FORMAT (                                                                  &
            &'#plant harvest 0=days_mat calc_d_mat db_d_mat calc_heatu db_heatu'&
           & )
 1900 FORMAT (                                                                  &
            &'# doy    doy   1=heatunit    days      days    degree_c  degree_c'&
           & )
 
 ! debe added for emergence output           
 2000 FORMAT('#                      days after  planting   planting  &
             & emergence       seedbed          emergence   daily   accumulated  weather')        
 
 2100 FORMAT('#cropname         doy   planting     date       depth       date &
             &        moisture           method   thermal    thermal      file')
 
 2200 FORMAT('#                          dy      dy  mn  yr     cm     doy  dy &
             & mn yr                               time         time')         
 
!debe added for leaf number table and phenological output
 2300 FORMAT('NOTE: If 999 is displayed in the output, the planting ',          & 
            & 'date or the resulting harvest date may be outside of', /1x,      &
            & 'the weather years in the selected weather file.', /1x)
            
 !  Heading for Leaf Number table          
 2600 FORMAT (42x,'Leaf Number', /1x, 39x, 'DOY', 2x,           & 
       'Leaf Number', /1x, 38x, '------------------')
       
 ! debe added for canopy height output    
 2700 FORMAT ('daysim',1x,'doy',4x,'date',3x,'canopyflg',3x,'dht',4x,'strs',4x,&
             & 'bczht',x,'dayhtinc',1x,'strsdayhtinc',7x,'canhty',7x,'canht',3x,'antss(1)')
             
 2800 FORMAT (12x,'dy',1x,'mn',1x,'yr',13x,'(m)',13x,'(m)',4x,'(cm)',7x,'(cm)',&
             & 11x,'(cm)', 11x,'(cm)',7x,'doy')
!           
END SUBROUTINE cpout
