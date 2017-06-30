subroutine cpout(ctrl,bio)
!
    use biomaterial
    use upgm_simdata, only : controls
implicit none
!
    type(biomatter) :: bio
    type(controls) :: ctrl
!     author : a. retta - 11/19/96
!     + + + purpose + + +
!     prints headers for the crop submodel output files
 
!     + + + common block variable definitions + + +
!     am0cfl - flag to print crop output
 
      ! season.out headers
write (ctrl%handles%luoseason,1500)
write (ctrl%handles%luoseason,1600)
write (ctrl%handles%luoseason,1700)
 
if (bio%growth%am0cfl>0) then
 
         ! crop.out headers
  write (ctrl%handles%luocrop,1000)
  write (ctrl%handles%luocrop,1100)
  write (ctrl%handles%luocrop,1200)
 
         ! shoot.out headers
  write (ctrl%handles%luoshoot,1300)
  write (ctrl%handles%luoshoot,1400)
 
         ! inpt.out headers
  write (ctrl%handles%luoinpt,1800)
  write (ctrl%handles%luoinpt,1900)
 
         ! debe added for emergence output headers
  write (ctrl%handles%luoemerge,2000)
  write (ctrl%handles%luoemerge,2100)
  write (ctrl%handles%luoemerge,2200)
 
         ! debe added for phenology output headers
  write (ctrl%handles%luophenol,2300)
  write (ctrl%handles%luophenol,2400)
 
        ! debe added for canopy height output
  write (ctrl%handles%luocanopyht,2500)
  write (ctrl%handles%luocanopyht,2600)
 
end if
 
!     + + + output formats + + +
 1000 format (                                                                  &
     &'#                           stand   stand   stand   flat    flat    flat &
     &   root    root  bel.grnd  total   total')
 1100 format (                                                                  &
     &'#daysim doy year dap heatui stem    leaf    store   stem    leaf    store&
     &   store   fiber   stem    leaf    stem    height  #stem   lai     eff_lai&
     & rootd  grainf tempst watstf  frost  ffa    ffw   par     apar     massinc&
     &    p_rw   p_st   p_lf   p_rp  stdflt pdiam  parea  fpdiam fparea hu_del c&
     &rop     gddday  leaf number') !debe added leaf number to output daily leaf number
 1200 format (                                                                  &
     &'#                           kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^&
     &2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  kg/m^2  meters          m^2/m^2 m^2/m^2&
     & meters                                           mj/m^2  mj/m^2   kg/plnt&
     &                                      meters m^2'         )
 
 1300 format ('#daysim doy year dap heatui ',                                   &
             &'s_root_sum f_root_sum tot_mass_req end_shoot_mass ',             &
             &'end_root_mass d_root_mass d_shoot_mass d_s_root_mass ',          &
             &'end_stem_mass end_stem_area end_shoot_len bczshoot ',            &
             &'bcmshoot bcdstm bc0nam gddday')
                                              !debe 090308 added gddday to output daily gdd values
 1400 format ('#(dy) (dy) (yr) (dy) (c)    ',                                   &
             &'(kg/m^2)   (kg/m^2)   (mg/shoot)   (mg/shoot)     ',             &
             &'(mg/shoot)    (mg/shoot)  (mg/shoot)   (mg/shoot)    ',          &
             &'(mg/shoot)    (m^2/shoot)   (m)           (m)      ',            &
             &'(kg/m^2) (#/m^2)')
 
 1500 format (                                                                  &
     &'#     standing                flat                    root               &
     &                      root')
 1600 format (                                                                  &
     &'#year stem    leaf    store   stem    leaf    store   stem    store   fib&
     &er   height stemcount depth   grainf  stmrepd dapl chill  hucum   mxhu hui&
     &nd dafm spring crop_name      yield       units    total biomass  hi      &
     &internode elongation jointing booting heading anthesis maturity harvest re&
     &ady')
!debe added yield and units, final total biomass and formatting to output
 1700 format (                                                                  &
     &'#     kg/m^2  -----------------------------------------------------------&
     &--   meter  #/m^2     meter   ------  meter  ----  deg_c  deg_c  deg_c ---&
     &-- ---- ------ ---------      --------    ------   kg/m^2         -----   &
     &-------------------- -------- ------- ------- -------- -------- ----------&
     &---')
 
 1800 format (                                                                  &
            &'#plant harvest 0=days_mat calc_d_mat db_d_mat calc_heatu db_heatu'&
           & )
 1900 format (                                                                  &
            &'# doy    doy   1=heatunit    days      days    degree_c  degree_c'&
           & )
 
 ! debe added for emergence output
 2000 format (                                                                  &
     &'#                      days after  planting   planting   emergence       &
     &seedbed          emergence   daily   accumulated  weather')
 
 2100 format (                                                                  &
     &'#cropname         doy   planting     date       depth       date         &
     &moisture           method   thermal    thermal      file')
 
 2200 format (                                                                  &
     &'#                          dy      dy  mn  yr     cm     doy  dy  mn yr  &
     &                             time         time')
 
!debe added for leaf number table and phenological output
 2300 format ('note: if 999 is displayed in the output, the planting ',         &
             &'date or the resulting harvest date may be outside of',/1x,       &
             &'the weather years in the selected weather file.',/1x)
 
 !  heading for leaf number table
 2400 format (42x,'leaf number',/1x,39x,'doy',2x,'leaf number',/1x,38x,         &
             &'------------------')
 
 ! debe added for canopy height output
 2500 format ('daysim',1x,'doy',4x,'date',3x,'canopyflg',3x,'dht',4x,'strs',4x, &
             &'bczht',x,'dayhtinc',1x,'strsdayhtinc',7x,'canhty',7x,'canht',3x, &
             &'antss(1)')
 
 2600 format (12x,'dy',1x,'mn',1x,'yr',13x,'(m)',13x,'(m)',4x,'(cm)',7x,'(cm)', &
            & 11x,'(cm)',11x,'(cm)',7x,'doy')
!
end subroutine cpout
