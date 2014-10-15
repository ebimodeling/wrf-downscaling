

MODULE module_fddaobs_rtfdda


































CONTAINS


  SUBROUTINE fddaobs_init(nudge_opt, maxdom, inest, parid,             &
                          idynin, dtramp, fdaend, restart,             &
                          twindo_cg, twindo, itimestep,                &
                          no_pbl_nudge_uv,                             &
                          no_pbl_nudge_t,                              &
                          no_pbl_nudge_q,                              &
                          sfc_scheme_horiz, sfc_scheme_vert,           &
                          maxsnd_gap,                                  &
                          sfcfact, sfcfacr, dpsmx,                     &
                          nudge_wind, nudge_temp, nudge_mois,          &
                          nudgezfullr1_uv, nudgezrampr1_uv,            &
                          nudgezfullr2_uv, nudgezrampr2_uv,            &
                          nudgezfullr4_uv, nudgezrampr4_uv,            &
                          nudgezfullr1_t,  nudgezrampr1_t,             &
                          nudgezfullr2_t,  nudgezrampr2_t,             &
                          nudgezfullr4_t,  nudgezrampr4_t,             &
                          nudgezfullr1_q,  nudgezrampr1_q,             &
                          nudgezfullr2_q,  nudgezrampr2_q,             &
                          nudgezfullr4_q,  nudgezrampr4_q,             &
                          nudgezfullmin,   nudgezrampmin, nudgezmax,   &
                          xlat, xlong,                                 &
                          start_year, start_month, start_day,          &
                          start_hour, start_minute, start_second,      &
                          p00, t00, tlp,                               &
                          znu, p_top,                                  &

                          fdob,                                        &

                          iprt,                                        &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     




  USE module_model_constants, ONLY : g, r_d
  USE module_domain
  USE module_dm, ONLY : wrf_dm_min_real

  IMPLICIT NONE





  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: nudge_opt(maxdom)
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde,                 &
                          ims,ime, jms,jme, kms,kme,                 &
                          its,ite, jts,jte, kts,kte
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  INTEGER, intent(in)  :: idynin         
  REAL,    intent(in)  :: dtramp         
  REAL,    intent(in)  :: fdaend(maxdom) 
  LOGICAL, intent(in)  :: restart
  REAL, intent(in)     :: twindo_cg      
  REAL, intent(in)     :: twindo
  INTEGER, intent(in)  :: itimestep
  INTEGER , INTENT(IN) :: no_pbl_nudge_uv(maxdom)  
  INTEGER , INTENT(IN) :: no_pbl_nudge_t(maxdom)   
  INTEGER , INTENT(IN) :: no_pbl_nudge_q(maxdom)   
  INTEGER , INTENT(IN) :: sfc_scheme_horiz 
  INTEGER , INTENT(IN) :: sfc_scheme_vert  
  REAL    , INTENT(IN) :: maxsnd_gap      
  REAL, intent(in)     :: sfcfact      
  REAL, intent(in)     :: sfcfacr      
  REAL, intent(in)     :: dpsmx        
  INTEGER , INTENT(IN) :: nudge_wind(maxdom)       
  INTEGER , INTENT(IN) :: nudge_temp(maxdom)       
  INTEGER , INTENT(IN) :: nudge_mois(maxdom)       
  REAL, INTENT(IN)     :: nudgezfullr1_uv  
  REAL, INTENT(IN)     :: nudgezrampr1_uv  
  REAL, INTENT(IN)     :: nudgezfullr2_uv  
  REAL, INTENT(IN)     :: nudgezrampr2_uv  
  REAL, INTENT(IN)     :: nudgezfullr4_uv  
  REAL, INTENT(IN)     :: nudgezrampr4_uv  
  REAL, INTENT(IN)     :: nudgezfullr1_t   
  REAL, INTENT(IN)     :: nudgezrampr1_t   
  REAL, INTENT(IN)     :: nudgezfullr2_t   
  REAL, INTENT(IN)     :: nudgezrampr2_t   
  REAL, INTENT(IN)     :: nudgezfullr4_t   
  REAL, INTENT(IN)     :: nudgezrampr4_t   
  REAL, INTENT(IN)     :: nudgezfullr1_q   
  REAL, INTENT(IN)     :: nudgezrampr1_q   
  REAL, INTENT(IN)     :: nudgezfullr2_q   
  REAL, INTENT(IN)     :: nudgezrampr2_q   
  REAL, INTENT(IN)     :: nudgezfullr4_q   
  REAL, INTENT(IN)     :: nudgezrampr4_q   
  REAL, INTENT(IN)     :: nudgezfullmin    
  REAL, INTENT(IN)     :: nudgezrampmin    
  REAL, INTENT(IN)     :: nudgezmax        
  REAL, INTENT(IN)     :: xlat ( ims:ime, jms:jme )        
  REAL, INTENT(IN)     :: xlong( ims:ime, jms:jme )        
  INTEGER, intent(in)  :: start_year   
  INTEGER, intent(in)  :: start_month  
  INTEGER, intent(in)  :: start_day    
  INTEGER, intent(in)  :: start_hour   
  INTEGER, intent(in)  :: start_minute 
  INTEGER, intent(in)  :: start_second 
  REAL, INTENT(IN)     :: p00          
  REAL, INTENT(IN)     :: t00          
  REAL, INTENT(IN)     :: tlp          
  REAL, INTENT(IN)     :: p_top        
  REAL, INTENT(IN)     :: znu( kms:kme )      

  TYPE(fdob_type), intent(inout)  :: fdob

  LOGICAL, intent(in)  :: iprt         


  logical            :: nudge_flag      
  integer            :: ktau            
  integer            :: nest            
  integer            :: idom            
  integer            :: parent          
  real               :: conv            
  real               :: tl1             
  real               :: tl2             
  real               :: xn1
  real               :: known_lat       
  real               :: known_lon       
  character(len=200) :: msg             
  real               :: z_at_p( kms:kme )  
  integer            :: i,j,k           





  nudge_flag = (nudge_opt(inest) .eq. 1)
  if (.not. nudge_flag) return

  call wrf_message("")
  write(msg,fmt='(a,i2)') ' OBSERVATION NUDGING IS ACTIVATED FOR MESH ',inest
  call wrf_message(msg)

  ktau  = itimestep
  if(restart) then
    fdob%ktaur = ktau
  else
    fdob%ktaur = 0 
  endif


  CALL date_string(start_year, start_month, start_day, start_hour,        &
                   start_minute, start_second, fdob%sdate)


  fdob%iwtsig = 0





  fdob%datend = 0.
  fdob%ieodi = 0


  if(idynin.eq.1)then

     if(dtramp.gt.0.)then
        fdob%datend = fdaend(inest) - dtramp
     else
        fdob%datend = fdaend(inest)
     endif
     if(iprt) then
        call wrf_message("")
        write(msg,fmt='(a,i3,a)')                                              &
          ' *** DYNAMIC-INITIALIZATION OPTION FOR INEST = ', inest, ' ***'
        call wrf_message(msg)
        write(msg,*) ' FDAEND,DATEND,DTRAMP: ',fdaend(inest),fdob%datend,dtramp
        call wrf_message(msg)
        call wrf_message("")
     endif
  endif




  if(sfc_scheme_horiz.eq.1) then
     call wrf_message('MM5 scheme selected for horizontal spreading of surface obs')
  elseif (sfc_scheme_horiz.eq.0) then
     call wrf_message('WRF scheme selected for horizontal spreading of surface obs')
  else
     write(msg,fmt='(a,i3)') 'Unknown h-spreading scheme for surface obs: ',sfc_scheme_horiz
     call wrf_message(msg)
     call wrf_message("Valid selections: 0=WRF scheme, 1=Original MM5 scheme")
     call wrf_error_fatal3("module_fddaobs_rtfdda.b",230,&
'fddaobs_init: module_fddaobs_rtfdda STOP' )
  endif

  if(sfc_scheme_vert.eq.1) then
     call wrf_message('Original simple scheme selected for vertical spreading of surface obs')
  elseif (sfc_scheme_vert.eq.0) then
     call wrf_message("Regime-based VIF scheme selected for vertical spreading of surface obs")
  else
     write(msg,fmt='(a,i3)') 'Unknown v-spreading scheme for surface obs: ',sfc_scheme_vert
     call wrf_message(msg)
     call wrf_message("Valid selections: 0=Regime-based VIF scheme, 1=Original simple scheme")
     call wrf_error_fatal3("module_fddaobs_rtfdda.b",242,&
'fddaobs_init: module_fddaobs_rtfdda STOP' )
  endif
  fdob%sfc_scheme_horiz = sfc_scheme_horiz 
  fdob%sfc_scheme_vert  = sfc_scheme_vert



  fdob%sfcfact = sfcfact
  fdob%sfcfacr = sfcfacr


  fdob%window = twindo
  call wrf_message("")
  write(msg,fmt='(a,i3)') '*** TIME WINDOW SETTINGS FOR NEST ',inest
  call wrf_message(msg)
  write(msg,fmt='(a,f6.3,2(a,f5.3))') '    TWINDO (hrs) = ',twindo,      &
            '  SFCFACT = ',sfcfact,'  SFCFACR = ',sfcfacr
  call wrf_message(msg)
  call wrf_message("")

  if(inest.eq.1) then
    if(twindo .eq. 0.) then
      if(iprt) then
        call wrf_message("")
        write(msg,*) '*** WARNING: TWINDO=0 on the coarse domain.'
        call wrf_message(msg)
        write(msg,*) '*** Did you forget to set twindo in the fdda namelist?'
        call wrf_message(msg)
        call wrf_message("")
      endif
    endif
  else        
    if(twindo .eq. 0.) then
      fdob%window = twindo_cg
      if(iprt) then
        call wrf_message("")
        write(msg,fmt='(a,i2)') 'WARNING: TWINDO=0. for nest ',inest
        call wrf_message(msg)
        write(msg,fmt='(a,f12.5,a)') 'Default to coarse-grid value of ', twindo_cg,' hrs'
        call wrf_message(msg)
        call wrf_message("")
      endif
    endif
  endif



  fdob%domain_tot=0
  do nest=1,maxdom
    fdob%domain_tot = fdob%domain_tot + nudge_opt(nest)
  end do


  if(dpsmx.gt.0.) then
       fdob%dpsmx = dpsmx
       fdob%dcon = 1.0/fdob%dpsmx
  else 
       call wrf_error_fatal3("module_fddaobs_rtfdda.b",300,&
'fddaobs_init: Namelist variable dpsmx must be greater than zero!')
  endif


  CALL get_base_state_height_column( p_top, p00, t00, tlp, g, r_d, znu,   &
                                     fdob%base_state,  kts, kte, kds,kde, kms,kme )


  fdob%nudge_uv_pbl  = .true.
  fdob%nudge_t_pbl   = .true.
  fdob%nudge_q_pbl   = .true.
  if(no_pbl_nudge_uv(inest) .eq. 1) fdob%nudge_uv_pbl  = .false.
  if(no_pbl_nudge_t(inest) .eq. 1)  fdob%nudge_t_pbl   = .false.
  if(no_pbl_nudge_q(inest) .eq. 1)  fdob%nudge_q_pbl   = .false.

  if(no_pbl_nudge_uv(inest) .eq. 1) then
    fdob%nudge_uv_pbl  = .false.
    write(msg,*) '   --> Obs nudging for U/V is turned off in PBL'
    call wrf_message(msg)
  endif
  if(no_pbl_nudge_t(inest) .eq. 1)  then
    fdob%nudge_t_pbl   = .false.
    write(msg,*) '   --> Obs nudging for T is turned off in PBL'
    call wrf_message(msg)
  endif
  if(no_pbl_nudge_q(inest) .eq. 1)  then
    fdob%nudge_q_pbl   = .false.
    write(msg,*) '   --> Obs nudging for Q is turned off in PBL'
    call wrf_message(msg)
  endif


  fdob%max_sndng_gap = maxsnd_gap
  write(msg,fmt='(a,f6.1)')  &
  '*** MAX PRESSURE GAP (cb) for interpolation between soundings = ',maxsnd_gap
  call wrf_message(msg)
  call wrf_message("")


  if(sfc_scheme_vert.eq.0) then
    fdob%vif_uv(1) = nudgezfullr1_uv
    fdob%vif_uv(2) = nudgezrampr1_uv
    fdob%vif_uv(3) = nudgezfullr2_uv
    fdob%vif_uv(4) = nudgezrampr2_uv
    fdob%vif_uv(5) = nudgezfullr4_uv
    fdob%vif_uv(6) = nudgezrampr4_uv
    fdob%vif_t (1) = nudgezfullr1_t
    fdob%vif_t (2) = nudgezrampr1_t
    fdob%vif_t (3) = nudgezfullr2_t
    fdob%vif_t (4) = nudgezrampr2_t
    fdob%vif_t (5) = nudgezfullr4_t
    fdob%vif_t (6) = nudgezrampr4_t
    fdob%vif_q (1) = nudgezfullr1_q
    fdob%vif_q (2) = nudgezrampr1_q
    fdob%vif_q (3) = nudgezfullr2_q
    fdob%vif_q (4) = nudgezrampr2_q
    fdob%vif_q (5) = nudgezfullr4_q
    fdob%vif_q (6) = nudgezrampr4_q


    if(nudgezmax.le.0.) then
      write(msg,*) 'STOP! OBS NAMELIST INPUT obs_nudgezmax MUST BE GREATER THAN ZERO.'
      call wrf_message(msg)
      write(msg,*) 'THE NAMELIST VALUE IS',nudgezmax
      call wrf_message(msg)
      call wrf_error_fatal3("module_fddaobs_rtfdda.b",366,&
'fddaobs_init: STOP on bad obs_nudgemax value' )
    endif
    if(nudgezfullmin.lt.0.) then
      write(msg,*) 'STOP! OBS NAMELIST INPUT obs_nudgezfullmin MUST BE NONNEGATIVE.'
      call wrf_message(msg)
      write(msg,*) 'THE NAMELIST VALUE IS',nudgezfullmin
      call wrf_message(msg)
      call wrf_error_fatal3("module_fddaobs_rtfdda.b",374,&
'fddaobs_init: STOP on bad obs_nudgefullmin value' )
    endif
    if(nudgezrampmin.lt.0.) then
      write(msg,*) 'STOP! OBS NAMELIST INPUT obs_nudgezrampmin MUST BE NONNEGATIVE.'
      call wrf_message(msg)
      write(msg,*) 'THE NAMELIST VALUE IS',nudgezrampmin
      call wrf_message(msg)
      call wrf_error_fatal3("module_fddaobs_rtfdda.b",382,&
'fddaobs_init: STOP on bad obs_nudgerampmin value' )
    endif
    if(nudgezmax.lt.nudgezfullmin+nudgezrampmin) then
      write(msg,*) 'STOP! INCONSISTENT OBS NAMELIST INPUTS.'
      call wrf_message(msg)
      write(msg,fmt='(3(a,f12.3))') 'obs_nudgezmax = ',nudgezmax,                &
                              ' obs_nudgezfullmin = ',nudgezfullmin,       &
                              ' obs_nudgezrampmin = ',nudgezrampmin
      call wrf_message(msg)
      write(msg,*) 'REQUIRE NUDGEZMAX >= NUDGEZFULLMIN + NUDGEZRAMPMIN'
      call wrf_message(msg)
      call wrf_error_fatal3("module_fddaobs_rtfdda.b",394,&
'fddaobs_init: STOP on inconsistent namelist values' )
    endif
 
    fdob%vif_fullmin = nudgezfullmin
    fdob%vif_rampmin = nudgezrampmin
    fdob%vif_max     = nudgezmax
 




    if(nudgezfullmin.gt.0.0) then
        if(nudgezfullmin .lt. 1.1*fdob%base_state(1)) then
           fdob%vif_fullmin = 1.1*fdob%base_state(1)
        endif
    endif


    if( (nudge_wind(inest).eq.1) .or. (nudge_temp(inest).eq.1)                             &
                                               .or. (nudge_mois(inest).eq.1) ) then
    call wrf_message("")
    write(msg,fmt='(a,i2,a)') ' *** SETUP DESCRIPTION FOR SURFACE OBS NUDGING ON MESH ',inest,' :'
    call wrf_message(msg)

    call wrf_message("")
    write(msg,fmt='(a,i5,a)') '  NUDGEZMAX: The maximum height at which nudging will be'//     &
                        ' applied from surface obs is ', nint(nudgezmax),' m AGL.'
    call wrf_message(msg)
    call wrf_message("")
    write(msg,fmt='(a,i3,a)') '  NUDGEZFULLMIN: The minimum height of full nudging weight'//   &
                          ' for surface obs is ', nint(fdob%vif_fullmin),' m.'
    call wrf_message(msg)
    if(nudgezfullmin.lt.fdob%vif_fullmin) then
        write(msg,fmt='(a,i3,a)') '  ***WARNING***: NUDGEZFULLMIN has been increased from'//   &
                              ' the user-input value of ',nint(nudgezfullmin),' m.'
        call wrf_message(msg)
        write(msg,fmt='(a,i3,a)') '  to ensure that at least the bottom model level is'//      &
                              ' included in full nudging.'
        call wrf_message(msg)
    endif
    call wrf_message("")
    write(msg,fmt='(a,i3,a)') '  NUDGEZRAMPMIN: The minimum height to ramp from full to no'//  &
                          ' nudging for surface obs is ', nint(nudgezrampmin),' m.'
    call wrf_message(msg)
    call wrf_message("")
    endif   


    if(nudge_wind(inest) .eq. 1) then
      call print_vif_var('wind', fdob%vif_uv, nudgezfullmin, nudgezrampmin)
      call wrf_message("")
    endif
    if(nudge_temp(inest) .eq. 1) then
      call print_vif_var('temp', fdob%vif_t,  nudgezfullmin, nudgezrampmin)
      call wrf_message("")
    endif
    if(nudge_mois(inest) .eq. 1) then
      call print_vif_var('mois', fdob%vif_q,  nudgezfullmin, nudgezrampmin)
      call wrf_message("")
    endif

    if( (nudge_wind(inest).eq.1) .or. (nudge_temp(inest).eq.1)                             &
                                                 .or. (nudge_mois(inest).eq.1) ) then
    write(msg,fmt='(a,i2)') ' *** END SETUP DESCRIPTION FOR SURFACE OBS NUDGING ON MESH ',inest
    call wrf_message(msg)
    call wrf_message("")
    endif
  endif   


  fdob%pfree = 50.0
  fdob%rinfmn = 1.0
  fdob%rinfmx = 2.0



      IF (its .eq. 1 .AND. jts .eq. 1) then
         known_lat = xlat(1,1)
         known_lon = xlong(1,1)
      ELSE
         known_lat = 9999.
         known_lon = 9999.
      END IF
      fdob%known_lat = wrf_dm_min_real(known_lat)
      fdob%known_lon = wrf_dm_min_real(known_lon)



  do nest=1,maxdom


    if (nest .eq. 1) then
      fdob%levidn(nest) = 0  
    else
      fdob%levidn(nest) = 1  
    endif
    idom = nest
100 parent = parid(idom)      

      if (parent .gt. 1) then   
        fdob%levidn(nest) = fdob%levidn(nest) + 1
        idom = parent
        goto 100
      endif
  enddo

















  RETURN

  END SUBROUTINE fddaobs_init



SUBROUTINE errob(inest, ub, vb, tb, t0, qvb, pbase, pp, rovcp,  &
                 z,                                             &
                 uratx, vratx, tratx, kpbl,                     &
                 nndgv, nerrf, niobf, maxdom,                   &
                 levidn, parid, nstat, nstaw,                   &
                 iswind, istemp, ismois, ispstr,                &
                 timeob, rio, rjo, rko,                         &
                 varobs, errf, ktau, xtime,                     &
                 iratio, npfi,                                  &
                 prt_max, prt_freq, iprt,                       &
                 obs_prt, stnid_prt, lat_prt, lon_prt,          &
                 mlat_prt, mlon_prt,                            & 
                 ids,ide, jds,jde, kds,kde,                     &
                 ims,ime, jms,jme, kms,kme,                     &
                 its,ite, jts,jte, kts,kte  )



  USE module_dm, ONLY : get_full_obs_vector, wrf_dm_sum_real



  USE module_model_constants, ONLY : rcp


  IMPLICIT NONE




































  INTEGER, INTENT(IN)  :: inest                  
  INTEGER, INTENT(IN)  :: nndgv                  
  INTEGER, INTENT(IN)  :: nerrf                  
  INTEGER, INTENT(IN)  :: niobf                  
  INTEGER, INTENT(IN)  :: maxdom                 
  INTEGER, INTENT(IN)  :: levidn(maxdom)         
  INTEGER, INTENT(IN)  :: parid(maxdom)          
  INTEGER, INTENT(IN)  :: ktau                   
  REAL, INTENT(IN)     :: xtime                  
  INTEGER, INTENT(IN)  :: iratio                 
  INTEGER, INTENT(IN)  :: npfi                   
  INTEGER, INTENT(IN)  :: prt_max                
  INTEGER, INTENT(IN)  :: prt_freq               
  LOGICAL, INTENT(IN)  :: iprt                   
  INTEGER, INTENT(IN)  :: obs_prt(prt_max)       
  INTEGER, INTENT(IN)  :: stnid_prt(40,prt_max)  
  REAL, INTENT(IN)     :: lat_prt(prt_max)       
  REAL, INTENT(IN)     :: lon_prt(prt_max)       
  REAL, INTENT(IN)     :: mlat_prt(prt_max)      
  REAL, INTENT(IN)     :: mlon_prt(prt_max)      
  INTEGER, INTENT(IN)  :: nstat                  
  INTEGER, INTENT(IN)  :: nstaw                  
  INTEGER, intent(in)  :: iswind
  INTEGER, intent(in)  :: istemp
  INTEGER, intent(in)  :: ismois
  INTEGER, intent(in)  :: ispstr
  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  

  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: t0
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme ) 
  REAL,   INTENT(IN)  :: rovcp
  REAL,    INTENT(IN) :: z( ims:ime, kms:kme, jms:jme ) 
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme ) 
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme ) 
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme ) 
  INTEGER,INTENT(IN) :: kpbl( ims:ime, jms:jme )  
  REAL,   INTENT(IN) :: timeob(niobf)             
  REAL,   INTENT(IN) :: rio(niobf)                
  REAL,   INTENT(IN) :: rjo(niobf)                
  REAL,   INTENT(INOUT) :: rko(niobf)             
  REAL,   INTENT(INOUT) :: varobs(nndgv, niobf)
  REAL,   INTENT(INOUT) :: errf(nerrf, niobf)


  INTEGER :: iobmg(niobf)   
  INTEGER :: jobmg(niobf)   
  INTEGER :: ia(niobf)
  INTEGER :: ib(niobf)
  INTEGER :: ic(niobf)
  REAL :: pbbo(kds:kde)    
  REAL :: ppbo(kds:kde)    

  REAL :: ra(niobf)
  REAL :: rb(niobf)
  REAL :: rc(niobf)
  REAL :: dxobmg(niobf)     
  REAL :: dyobmg(niobf)     
  INTEGER MM(MAXDOM)
  INTEGER NNL
  real :: uratio( ims:ime, jms:jme )   
  real :: vratio( ims:ime, jms:jme )   
  real :: pug1,pug2,pvg1,pvg2
  character(len=200) :: msg            


  real, parameter :: gridx_t = 0.5     
  real, parameter :: gridy_t = 0.5     
  real, parameter :: gridx_u = 0.0     
  real, parameter :: gridy_u = 0.5     
  real, parameter :: gridx_v = 0.5     
  real, parameter :: gridy_v = 0.0     

  real :: dummy = 99999.

  real :: pbhi, pphi
  real :: obs_pottemp                  


  integer nsta,ivar,n,ityp
  integer iob,job,kob,iob_ms,job_ms
  integer k,kbot,nml,nlb,nle
  integer iobm,jobm,iobp,jobp,kobp,inpf,i,j
  integer i_start,i_end,j_start,j_end    
  integer k_start,k_end
  integer ips                            
  integer pnx                            

  real gridx,gridy,dxob,dyob,dzob,dxob_ms,dyob_ms
  real pob
  real hob
  real uratiob,vratiob,tratiob,tratxob,fnpf


  LOGICAL MP_LOCAL_DUMMASK(NIOBF)  
  LOGICAL MP_LOCAL_UOBMASK(NIOBF)  
  LOGICAL MP_LOCAL_VOBMASK(NIOBF)  
  LOGICAL MP_LOCAL_COBMASK(NIOBF)  




  NSTA=NSTAT







  if (iprt) then
    write(msg,fmt='(a,i5,a,i2,a,i5,a)') '++++++CALL ERROB AT KTAU = ', &
            KTAU,' AND INEST = ',INEST,':  NSTA = ',NSTAW,' ++++++'
    call wrf_message(msg)
  endif

  ERRF = 0.0    


  i_start = max( its-1,ids )
  i_end   = min( ite+1,ide-1 )
  j_start = max( jts-1,jds )
  j_end   = min( jte+1,jde-1 )
  k_start = kts
  k_end = min( kte, kde-1 )

  DO ITYP=1,3   


    IF(ITYP.EQ.1) THEN        
       GRIDX = gridx_u
       GRIDY = gridy_u
    ELSE IF(ITYP.EQ.2) THEN   
       GRIDX = gridx_v
       GRIDY = gridy_v
    ELSE                      
       GRIDX = gridx_t
       GRIDY = gridy_t
    ENDIF


    IF(ityp.eq.1)THEN
      call upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme, uratx, uratio)
    ELSE IF (ityp.eq.2) THEN
      call vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme, vratx, vratio)
    ENDIF

    IF(INEST.EQ.1) THEN       
      DO N=1,NSTA
        RA(N)=RIO(N)-GRIDX
        RB(N)=RJO(N)-GRIDY
        IA(N)=RA(N)
        IB(N)=RB(N)
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
        DXOB=RA(N)-FLOAT(IA(N))
        DYOB=RB(N)-FLOAT(IB(N))


        if(ityp.eq.1) then
          iobmg(n) = MIN0(MAX0(1,int(RIO(n)-gridx_t)),ide-1)
          jobmg(n) = MIN0(MAX0(1,int(RJO(n)-gridy_t)),jde-1)
          dxobmg(n) = RIO(N)-gridx_t-FLOAT(int(RIO(N)-gridx_t))
          dyobmg(n) = RJO(N)-gridy_t-FLOAT(int(RJO(N)-gridy_t))
        endif
        iob_ms = iobmg(n)
        job_ms = jobmg(n)
        dxob_ms = dxobmg(n)
        dyob_ms = dyobmg(n)











        pob = 0.0



        MP_LOCAL_DUMMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
  
        IF ( MP_LOCAL_DUMMASK(N) ) THEN




          do k = kds, kde
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )  
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )  
          enddo







          if(abs(rko(n)+99).lt.1.)then
            pob = varobs(5,n)

            if(pob .eq.-888888.) then
               hob = varobs(6,n)
               if(hob .gt. -800000. ) then
                 pob = ht_to_p( hob, ppbo, pbbo, z, iob_ms, job_ms,          &
                                dxob_ms, dyob_ms, k_start, k_end, kds,kde,   &
                                ims,ime, jms,jme, kms,kme )
               endif
            endif

            if(pob .gt.-800000.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 199
                endif
              enddo
 199          continue

              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)

              rko(n) = real(kbot+1)-                                    &
                 ( (pob-pbhi-pphi) / (pbbo(kbot)+ppbo(kbot)-pbhi-pphi) )

              rko(n)=max(rko(n),1.0)
            endif
          endif


        ENDIF       



        if(varobs(5,n) .eq. -888888. .and. varobs(6,n) .gt. -800000.) then
           varobs(5,n) = wrf_dm_sum_real ( pob )
        endif

        RC(N)=RKO(N)

      ENDDO      

    ELSE       








      DO N=1,NSTA
          RA(N)=RIO(N)-GRIDX           
          RB(N)=RJO(N)-GRIDY           
          IA(N)=RA(N)
          IB(N)=RB(N)
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          DXOB=RA(N)-FLOAT(IA(N))
          DYOB=RB(N)-FLOAT(IB(N))


          if(ityp.eq.1) then
            iobmg(n) = MIN0(MAX0(1,int(RIO(n)-gridx_t)),ide-1)
            jobmg(n) = MIN0(MAX0(1,int(RJO(n)-gridy_t)),jde-1)
            dxobmg(n) = RIO(N)-gridx_t-FLOAT(int(RIO(N)-gridx_t))
            dyobmg(n) = RJO(N)-gridy_t-FLOAT(int(RJO(N)-gridy_t))
          endif 
          iob_ms = iobmg(n)
          job_ms = jobmg(n)
          dxob_ms = dxobmg(n)
          dyob_ms = dyobmg(n)


        pob = 0.0



        MP_LOCAL_DUMMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)

        IF ( MP_LOCAL_DUMMASK(N) ) THEN




          do k = kds, kde
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )
          enddo







          if(abs(rko(n)+99).lt.1.)then
            pob = varobs(5,n)

            if(pob .eq.-888888.) then
               hob = varobs(6,n)
               if(hob .gt. -800000. ) then
                 pob = ht_to_p( hob, ppbo, pbbo, z, iob_ms, job_ms,          &
                                dxob_ms, dyob_ms, k_start, k_end, kds,kde,   &
                                ims,ime, jms,jme, kms,kme )
               endif
            endif

            if(pob .gt.-800000.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 198
                endif
              enddo
 198          continue

              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)

              rko(n) = real(kbot+1)-                                    &
                 ( (pob-pbhi-pphi) / (pbbo(kbot)+ppbo(kbot)-pbhi-pphi) )
              rko(n)=max(rko(n),1.0)
            endif
          endif


        ENDIF       



        if(varobs(5,n) .eq. -888888. .and. varobs(6,n) .gt. -800000.) then
           varobs(5,n) = wrf_dm_sum_real ( pob )
        endif

        RC(N)=RKO(N)

      ENDDO      
    
    ENDIF      








    IF(ITYP.EQ.1) THEN
      NLB=1
      NLE=1
    ELSE IF(ITYP.EQ.2) THEN
      NLB=2
      NLE=2
    ELSE
      NLB=3
      NLE=5
    ENDIF
    DO IVAR=NLB,NLE
      DO N=1,NSTA
        IF((RA(N)-1.).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((RB(N)-1.).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((FLOAT(ide)-2.0*GRIDX-RA(N)-1.E-10).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((FLOAT(jde)-2.0*GRIDY-RB(N)-1.E-10).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        if(rc(n).lt.1.)errf(ivar,n)=errf(ivar,n)+dummy
      ENDDO
    ENDDO



    DO N=1,NSTA
        IA(N)=RA(N)
        IB(N)=RB(N)
        IC(N)=RC(N)
    ENDDO
    DO N=1,NSTA
        RA(N)=RA(N)-FLOAT(IA(N))
        RB(N)=RB(N)-FLOAT(IB(N))
        RC(N)=RC(N)-FLOAT(IC(N))
    ENDDO





    if(ityp.eq.1) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)


        MP_LOCAL_UOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)

      ENDDO
    endif
    if(ityp.eq.2) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)


        MP_LOCAL_VOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)

      ENDDO
    endif
    if(ityp.eq.3) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)


        MP_LOCAL_COBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)

      ENDDO
    endif




    IF(ITYP.EQ.1) THEN

      DO N=1,NSTA
        IF(MP_LOCAL_UOBMASK(N)) THEN
          ERRF(9,N)=rko(n)       
        ENDIF
      ENDDO

      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))


          IF(MP_LOCAL_UOBMASK(N))THEN     

            KOBP=MIN0(KOB+1,k_end)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)


            PUG1 = .5*( pbase(IOBM,1,JOB) + pbase(IOB,1,JOB) )
            PUG2 = .5*( pbase(IOB,1,JOB) + pbase(IOBP,1,JOB) )


            if(rko(n).eq.1.0)then
              uratiob=((1.-DYOB)*((1.-DXOB)*uratio(IOB,JOB)+     &
                    DXOB*uratio(IOBP,JOB)                        &
                  )+DYOB*((1.-DXOB)*uratio(IOB,JOBP)+            &
                  DXOB*uratio(IOBP,JOBP)))
            else
              uratiob=1.
            endif

            if(abs(uratiob).lt.1.0e-3) then
              uratiob=1.
            endif






            ERRF(1,N)=ERRF(1,N)+uratiob*VAROBS(1,N)-((1.-DZOB)*        &
                      ((1.-DyOB)*((1.-                                 &
                      DxOB)*UB(IOB,KOB,JOB)+DxOB*UB(IOB+1,KOB,JOB)     &
                      )+DyOB*((1.-DxOB)*UB(IOB,KOB,JOB+1)+DxOB*        &
                      UB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                      *UB(IOB,KOBP,JOB)+DxOB*UB(IOB+1,KOBP,JOB))+      &
                      DyOB*((1.-DxOB)*UB(IOB,KOBP,JOB+1)+DxOB*         &
                      UB(IOB+1,KOBP,JOB+1))))























            ERRF(7,N)=.001*( (1.-DXOB)*PUG1 + DXOB*PUG2 )
  

          ENDIF       

        ENDDO    

      ENDIF    

    ENDIF   




    IF(ITYP.EQ.2) THEN

      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))


          IF(MP_LOCAL_VOBMASK(N))THEN     

            KOBP=MIN0(KOB+1,k_end)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)


            PVG1 = .5*( pbase(IOB,1,JOBM) + pbase(IOB,1,JOB) )
            PVG2 = .5*( pbase(IOB,1,JOB) + pbase(IOB,1,JOBP) )


            if(rko(n).eq.1.0)then
              vratiob=((1.-DYOB)*((1.-DXOB)*vratio(IOB,JOB)+     &
                    DXOB*vratio(IOBP,JOB)                        &
                  )+DYOB*((1.-DXOB)*vratio(IOB,JOBP)+            &
                  DXOB*vratio(IOBP,JOBP)))
            else
              vratiob=1.
            endif

            if(abs(vratiob).lt.1.0e-3) then
              vratiob=1.
            endif




  

            ERRF(2,N)=ERRF(2,N)+vratiob*VAROBS(2,N)-((1.-DZOB)*        &
                     ((1.-DyOB)*((1.-                                  &
                      DxOB)*VB(IOB,KOB,JOB)+DxOB*VB(IOB+1,KOB,JOB)     &
                      )+DyOB*((1.-DxOB)*VB(IOB,KOB,JOB+1)+DxOB*        &
                      VB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                      *VB(IOB,KOBP,JOB)+DxOB*VB(IOB+1,KOBP,JOB))+      &
                      DyOB*((1.-DxOB)*VB(IOB,KOBP,JOB+1)+DxOB*         &
                      VB(IOB+1,KOBP,JOB+1))))













  

            ERRF(8,N)=.001*( (1.-DYOB)*PVG1 + DYOB*PVG2 )
  

          ENDIF       

        ENDDO    

      ENDIF    

    ENDIF   




    IF(ITYP.EQ.3) THEN

      IF(ISTEMP.EQ.1 .OR. ISMOIS.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)

          IF(MP_LOCAL_COBMASK(N)) THEN     

            KOB=MIN0(k_end,IC(N))
            KOBP=MIN0(KOB+1,K_END)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)


            if(rko(n).eq.1.0)then
              tratxob=((1.-DYOB)*((1.-DXOB)*tratx(IOB,JOB)+        &
                    DXOB*tratx(IOB+1,JOB)                          &
                  )+DYOB*((1.-DXOB)*tratx(IOB,JOB+1)+              &
                  DXOB*tratx(IOB+1,JOB+1)))
            else
              tratxob=1.
            endif


            if(abs(tratxob) .lt. 1.0E-3) tratxob=1.


            obs_pottemp = -888888.
            if(varobs(3,n).gt.-800000. .and. varobs(5,n).gt.-800000) then
              obs_pottemp = varobs(3,n)*(100./varobs(5,n))**RCP - t0
            endif

            ERRF(3,N)=ERRF(3,N)+tratxob*obs_pottemp-((1.-DZOB)*     &
                      ((1.-DyOB)*((1.-                              &
                      DxOB)*(TB(IOB,KOB,JOB))+DxOB*                 &
                      (TB(IOB+1,KOB,JOB)))+DyOB*((1.-DxOB)*         &
                      (TB(IOB,KOB,JOB+1))+DxOB*                     &
                      (TB(IOB+1,KOB,JOB+1))))+DZOB*((1.-            &
                      DyOB)*((1.-DxOB)*(TB(IOB,KOBP,JOB))+DxOB*     &
                      (TB(IOB+1,KOBP,JOB)))+DyOB*((1.-DxOB)*        &
                      (TB(IOB,KOBP,JOB+1))+DxOB*                    &
                      (TB(IOB+1,KOBP,JOB+1)))))






















            ERRF(4,N)=ERRF(4,N)+VAROBS(4,N)-((1.-DZOB)*((1.-DyOB)*((1.- &
                      DxOB)*QVB(IOB,KOB,JOB)+DxOB*                      &
                      QVB(IOB+1,KOB,JOB))+DyOB*((1.-DxOB)*              &
                      QVB(IOB,KOB,JOB+1)+DxOB*                          &
                      QVB(IOB+1,KOB,JOB+1)))+DZOB*((1.-                 &
                      DyOB)*((1.-DxOB)*QVB(IOB,KOBP,JOB)+DxOB           &
                      *QVB(IOB+1,KOBP,JOB))+DyOB*((1.-DxOB              &
                      )*QVB(IOB,KOBP,JOB+1)+DxOB*                       &
                      QVB(IOB+1,KOBP,JOB+1))))



            ERRF(10,N)=ERRF(10,N)+((1.-DZOB)*((1.-DyOB)*((1.- &
                      DxOB)*QVB(IOB,KOB,JOB)+DxOB*                      &
                      QVB(IOB+1,KOB,JOB))+DyOB*((1.-DxOB)*              &
                      QVB(IOB,KOB,JOB+1)+DxOB*                          &
                      QVB(IOB+1,KOB,JOB+1)))+DZOB*((1.-                 &
                      DyOB)*((1.-DxOB)*QVB(IOB,KOBP,JOB)+DxOB           &
                      *QVB(IOB+1,KOBP,JOB))+DyOB*((1.-DxOB              &
                      )*QVB(IOB,KOBP,JOB+1)+DxOB*                       &
                      QVB(IOB+1,KOBP,JOB+1))))



            ERRF(6,N)= .001*                                            &
                      ((1.-DyOB)*((1.-DxOB)*pbase(IOB,1,JOB)+DxOB*      &
                      pbase(IOB+1,1,JOB))+DyOB*((1.-DxOB)*              &
                      pbase(IOB,1,JOB+1)+DxOB*pbase(IOB+1,1,JOB+1) ))


          ENDIF       

        ENDDO     

      ENDIF   




      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)

        IF(MP_LOCAL_COBMASK(N)) THEN    

          DXOB=RA(N)
          DYOB=RB(N)
          ERRF(5,N) = kpbl(iob+nint(dxob),job+nint(dyob))


        ENDIF       

      ENDDO

    ENDIF   

  ENDDO   



  CALL get_full_obs_vector(nsta, nerrf, niobf, mp_local_uobmask,     &
                           mp_local_vobmask, mp_local_cobmask, errf)





  do n=1,nsta
    rko(n) = errf(9,n)
  enddo




  CALL print_obs_info(iprt,inest,niobf,rio,rjo,rko,                  &
                      prt_max,prt_freq,obs_prt,stnid_prt,            &
                      lat_prt,lon_prt,mlat_prt,mlon_prt,             &
                      timeob,xtime)


  IF(INEST.EQ.1)THEN
    INPF=NPFI
  ELSE
    FNPF=IRATIO**LEVIDN(INEST)
    INPF=FNPF*NPFI
  ENDIF

  do n=1,nsta
    if((abs(errf(3,n)).gt.20.).and.           &
           (errf(3,n).gt.-800000.))then

       errf(1,n)=-888888.
       errf(2,n)=-888888.
       errf(3,n)=-888888.
       errf(4,n)=-888888.
       varobs(1,n)=-888888.
       varobs(2,n)=-888888.
       varobs(3,n)=-888888.
       varobs(4,n)=-888888.
    endif
  enddo






  RETURN
  END SUBROUTINE errob

  SUBROUTINE upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme,  &
                    arrin, arrout)





  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     
  INTEGER, INTENT(IN) :: i_end       
  INTEGER, INTENT(IN) :: j_start     
  INTEGER, INTENT(IN) :: j_end       
  INTEGER, INTENT(IN) :: ids         
  INTEGER, INTENT(IN) :: ide         
  INTEGER, INTENT(IN) :: ims         
  INTEGER, INTENT(IN) :: ime         
  INTEGER, INTENT(IN) :: jms         
  INTEGER, INTENT(IN) :: jme         
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  


  integer :: i, j


  do j = j_start, j_end
    do i = max(2,i_start), i_end
       arrout(i,j) = 0.5*(arrin(i,j)+arrin(i-1,j))
    enddo
  enddo


  if(i_start .eq. ids) then
    do j = j_start, j_end
      arrout(i_start,j) = arrin(i_start,j)
    enddo
  endif
  if(i_end .eq. ide-1) then
    do j = j_start, j_end
      arrout(i_end+1,j) = arrin(i_end,j)
    enddo
  endif

  RETURN
  END SUBROUTINE upoint

  SUBROUTINE vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme,  &
                    arrin, arrout)





  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     
  INTEGER, INTENT(IN) :: i_end       
  INTEGER, INTENT(IN) :: j_start     
  INTEGER, INTENT(IN) :: j_end       
  INTEGER, INTENT(IN) :: jds         
  INTEGER, INTENT(IN) :: jde         
  INTEGER, INTENT(IN) :: ims         
  INTEGER, INTENT(IN) :: ime         
  INTEGER, INTENT(IN) :: jms         
  INTEGER, INTENT(IN) :: jme         
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  


  integer :: i, j


  do j = max(2,j_start), j_end
    do i = i_start, i_end
      arrout(i,j) = 0.5*(arrin(i,j)+arrin(i,j-1))
    enddo
  enddo


  if(j_start .eq. jds) then
    do i = i_start, i_end
      arrout(i,j_start) = arrin(i,j_start)
    enddo
  endif
  if(j_end .eq. jde-1) then
    do i = i_start, i_end
      arrout(i,j_end+1) = arrin(i,j_end)
    enddo
  endif

  RETURN
  END SUBROUTINE vpoint

  LOGICAL FUNCTION TILE_MASK(iloc, jloc, its, ite, jts, jte)








  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iloc
  INTEGER, INTENT(IN) :: jloc
  INTEGER, INTENT(IN) :: its
  INTEGER, INTENT(IN) :: ite
  INTEGER, INTENT(IN) :: jts
  INTEGER, INTENT(IN) :: jte


  LOGICAL :: retval

  TILE_MASK = (iloc .LE. ite .AND. iloc .GE. its .AND.    &
               jloc .LE. jte .AND. jloc .GE. jts )

  RETURN
  END FUNCTION TILE_MASK


  SUBROUTINE nudob(j, ivar, aten, inest, ifrest, ktau, ktaur,         &
                       xtime, mu, msfx, msfy, nndgv, nerrf, niobf, maxdom,   &
                       npfi, ionf, rinxy, twindo,                     &
                       nudge_pbl,                                     &
                       sfcfact, sfcfacr,                              &
                       levidn,                                        &
                       parid, nstat,                                  &
                       rinfmn, rinfmx, pfree, dcon, tfaci,            &
                       sfc_scheme_horiz, sfc_scheme_vert, maxsnd_gap, &
                       lev_in_ob, plfo, nlevs_ob,                     &
                       iratio, dx, dtmin, rio, rjo, rko,              &
                       timeob, varobs, errf, pbase, ptop, pp,         &
                       iswind, istemp, ismois, giv, git, giq,         &
                       savwt, kpblt, nscan,                           &
                       vih1, vih2, terrh, zslab,                      &
                       iprt,                                          &
                       ids,ide, jds,jde, kds,kde,                     &  
                       ims,ime, jms,jme, kms,kme,                     &  
                       its,ite, jts,jte, kts,kte,                     &  
                       qvb, obs_scl_neg_qv_innov ) 


  USE module_model_constants
  USE module_domain

  IMPLICIT NONE






















































  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  
  INTEGER, INTENT(IN)  :: j                          
  INTEGER, INTENT(IN)  :: ivar
  INTEGER, INTENT(IN)  :: inest                      
  LOGICAL, INTENT(IN)  :: ifrest
  INTEGER, INTENT(IN)  :: ktau
  INTEGER, INTENT(IN)  :: ktaur
  REAL, INTENT(IN)     :: xtime                      
  INTEGER, INTENT(IN)  :: nndgv                      
  INTEGER, INTENT(IN)  :: nerrf                      
  INTEGER, INTENT(IN)  :: niobf                      
  INTEGER, INTENT(IN)  :: maxdom                     
  INTEGER, INTENT(IN)  :: npfi 
  INTEGER, INTENT(IN)  :: ionf
  REAL, INTENT(IN)     :: rinxy
  REAL, INTENT(IN)     :: twindo
  REAL, intent(in)     :: sfcfact                    
  REAL, intent(in)     :: sfcfacr                    
  LOGICAL, intent(in)  :: nudge_pbl                  
  INTEGER, INTENT(IN)  :: levidn(maxdom)             
  INTEGER, INTENT(IN)  :: parid(maxdom)              
  INTEGER, INTENT(IN)  :: nstat                      
  REAL,    INTENT(IN)  :: rinfmn          
  REAL,    INTENT(IN)  :: rinfmx          
  REAL,    INTENT(IN)  :: pfree           
  REAL,    INTENT(IN)  :: dcon            
  REAL,    INTENT(IN)  :: tfaci           
  INTEGER , INTENT(IN) :: sfc_scheme_horiz 
  INTEGER , INTENT(IN) :: sfc_scheme_vert  
  REAL    , INTENT(IN) :: maxsnd_gap      
  REAL, INTENT(IN)     :: lev_in_ob(niobf)           
  REAL, intent(IN)     :: plfo(niobf)                
  REAL, INTENT(IN)     :: nlevs_ob(niobf)            
  INTEGER, INTENT(IN)  :: iratio                     
  REAL, INTENT(IN)     :: dx                         
  REAL, INTENT(IN)     :: dtmin                      
  REAL, INTENT(IN)     :: rio(niobf)                 
  REAL, INTENT(IN)     :: rjo(niobf)                 
  REAL, INTENT(INOUT)  :: rko(niobf)                 
  REAL, INTENT(IN)     :: timeob(niobf)
  REAL, INTENT(IN)     :: varobs(nndgv,niobf)
  REAL, INTENT(IN)     :: errf(nerrf, niobf)
  REAL, INTENT(IN)     :: pbase( ims:ime, kms:kme )  
  REAL, INTENT(IN)     :: ptop
  REAL, INTENT(IN)     :: pp( ims:ime, kms:kme ) 
  REAL, INTENT(IN)     :: mu(ims:ime)   
  REAL, INTENT(IN)     :: msfx(ims:ime)  
  REAL, INTENT(IN)     :: msfy(ims:ime)  
  INTEGER, intent(in)  :: iswind        
  INTEGER, intent(in)  :: istemp        
  INTEGER, intent(in)  :: ismois        
  REAL, intent(in)     :: giv           
  REAL, intent(in)     :: git           
  REAL, intent(in)     :: giq           
  REAL, INTENT(INOUT)  :: aten( ims:ime, kms:kme)
  REAL, INTENT(INOUT)  :: savwt( nndgv, ims:ime, kms:kme )
  INTEGER, INTENT(IN)  :: kpblt(ims:ime)
  INTEGER, INTENT(IN)  :: nscan                      
  REAL, INTENT(IN)     :: vih1(its:ite) 
  REAL, INTENT(IN)     :: vih2(its:ite) 
  REAL, INTENT(IN)     :: terrh(ims:ime) 


  REAL, INTENT(IN)     :: zslab(ims:ime, kms:kme)    
  LOGICAL, INTENT(IN)  :: iprt                       
  REAL                 :: abs_pdiff_below, abs_pdiff_above
  REAL,   INTENT(IN)   :: qvb( ims:ime, kms:kme, jms:jme ) 
  INTEGER, INTENT(IN)  :: obs_scl_neg_qv_innov 
  REAL                 :: qvb_at_cur_loc, qvb_at_ob_loc 
  REAL                 :: SCALE_FACTOR_NEG_QV_INNOV 
  REAL                 :: QVB_CUR_LOC_OVER_OB_LOC 


  integer :: mm(maxdom)
  integer :: kobs                  
  integer :: kpbl_obs(nstat)       
  real :: ra(niobf)
  real :: rb(niobf)
  real :: psurf(niobf)
  real :: wtsig(kms:kme),wt(ims:ime,kms:kme),wt2err(ims:ime,kms:kme)
  real :: rscale(ims:ime)           
  real :: wtij(ims:ime)             
  real :: reserf(100)
  character*40 name
  character*3 chr_hr
  character(len=200) :: msg            


  integer :: i,k,iplo,icut,ipl,inpf,infr,jjjn
  integer :: igrid,n,nml,nnl,nsthis,nsmetar,nsspeci,nsship
  integer :: nssynop,nstemp,nspilot,nssatwnds,nssams,nsprofs
  integer :: maxi,mini,maxj,minj,nnn,nsndlev,njcsnd,kob
  integer :: komin,komax,nn,nhi,nlo,nnjc
  integer :: i_s,i_e
  integer :: istq
  real :: gfactor,rfactor,gridx,gridy,rindx,ris
  real :: grfacx,grfacy
  real :: timewt,pob
  real :: ri,rj,rx,ry,rsq,pdfac,erfivr,dk,slope,rinfac
  real :: dprim,dsq,d     
  real :: rinprs,pijk,pobhi,poblo,pdiffj,w2eowt,gitq
  real :: dz_ramp         

  real :: scratch
  integer :: kk 







  if(NSCAN.ne.0) then
    IF (iprt) then
        write(msg,*) 'SAVWT must be resized for NSCAN=1'
        call wrf_message(msg)
    ENDIF
    call wrf_error_fatal3("module_fddaobs_rtfdda.b",1727,&
'wrf_fddaobs_in: in4dob' )
  endif
  IPLO=0  + NSCAN*4
  GFACTOR=1. +  NSCAN*(-1. + 0.33333) 
  RFACTOR=1. +  NSCAN*(-1. + 3.0)




  if(inest.eq.1.and.ivar.lt.3.and.(j.le.2.or.j.ge.jde-1)) then


    return
  endif
  if(inest.eq.1.and.ivar.ge.3.and.(j.le.2.or.j.ge.jde-2)) then


    return
  endif


  ICUT=0
  IF(INEST.GT.1)ICUT=1
  i_s = max0(2+icut,its)
  i_e = min0(ide-1-icut,ite)

  IPL=IVAR    + IPLO     



  INPF=(IRATIO**LEVIDN(INEST))*NPFI
  INFR=(IRATIO**LEVIDN(INEST))*IONF

  GRIDX=0.0
  GRIDY=0.0
  IGRID=0
  IF(IVAR.GE.3)THEN
    GRIDX=0.5
    GRIDY=0.5
    IGRID=1
  ELSEIF(IVAR.eq.1) THEN
    GRIDY=0.5
    GRIDX=0.0
    IGRID=1
  ELSEIF(IVAR.eq.2) THEN
    GRIDX=0.5
    GRIDY=0.0
    IGRID=1
  ENDIF




  RINDX=RINXY*1000./DX          * RFACTOR   
  RIS=RINDX*RINDX
  IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 5
  IF(MOD(KTAU,INFR).NE.0)GOTO 126
5 CONTINUE
  IF (iprt) THEN
   IF(J.EQ.10) then
       write(msg,6) INEST,J,KTAU,XTIME,IVAR,IPL,rindx
       call wrf_message(msg)
   ENDIF
  ENDIF
6 FORMAT(1X,'OBS NUDGING FOR IN,J,KTAU,XTIME,',                    &
            'IVAR,IPL: ',I2,1X,I2,1X,I5,1X,F8.2,1X,I2,1X,I2,       &
            ' rindx=',f4.1)








  DO N=1,NSTAT
    RA(N)=RIO(N)-GRIDX
    RB(N)=RJO(N)-GRIDY
  ENDDO


  DO I=its,ite
    DO K=1,kte
      WT(I,K)=0.0
      WT2ERR(I,K)=0.0
    ENDDO
  ENDDO











  DO N=1,NSTAT
    IF(IVAR.GE.3)THEN
      PSURF(N)=ERRF(6,N)
    ELSE
      IF(IVAR.EQ.1)THEN
        PSURF(N)=ERRF(7,N)        
      ELSE
        PSURF(N)=ERRF(8,N)        
      ENDIF
    ENDIF
  ENDDO




  MAXJ=J+IFIX(RINDX*RINFMX+0.99)                                             
  MINJ=J-IFIX(RINDX*RINFMX+0.99)                                             





  n=1


  DO nnn=1,NSTAT   







    nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1   





    njcsnd=nsndlev

    pob=varobs(5,n)












    IF( ABS(ERRF(IVAR,N)).GT.9.E4 .and. njcsnd.eq.1 ) THEN


    ELSEIF( RB(N).LT.FLOAT(MINJ) .OR. RB(N).GT.FLOAT(MAXJ) ) THEN



    ELSE    










      rko(n) = errf(9,n)        
      kpbl_obs(n) = errf(5,n)   


      KOB=nint(RKO(N)+0.45)
      KOB=MIN0(kte,KOB)
      KOB=MAX0(1,KOB)


      IF(KOB.EQ.1.AND.IVAR.LE.4.and.nlevs_ob(n).lt.1.5) THEN


        timewt = get_timewt(xtime,dtmin,twindo,sfcfact,timeob(n))

        DO K=1,kte
          WTSIG(K)=0.0
        ENDDO









        MAXI=IFIX(RA(N)+0.99+RINDX*sfcfacr)
        MAXI=MIN0(ide-1,MAXI)
        MINI=IFIX(RA(N)-RINDX*sfcfacr-0.99)
        MINI=MAX0(2,MINI)









        if (  RA(N).GE.(0.-RINDX*sfcfacr/3)                        &
        .and. RA(N).LE.float(ide)+RINDX*sfcfacr/3                  &
        .and. RB(N).GE.(0.-RINDX*sfcfacr/3)                        &
        .and. RB(N).LE.float(jde)+RINDX*sfcfacr/3) then













          RJ=FLOAT(J)
          RX=RJ-RB(N)

          ERFIVR=ERRF(IVAR,N)
          QVB_AT_OB_LOC=ERRF(10,N)
 

          if(nudge_pbl) then


            if(SFC_SCHEME_HORIZ.eq.1) then        
              DO I=max0(its,MINI),min0(ite,MAXI)
                RI=FLOAT(I)
                RY=RI-RA(N)
                RIS=RINDX*RINDX*sfcfacr*sfcfacr
                RSQ=RX*RX+RY*RY

                DPRIM=SQRT(RSQ)
                D=DPRIM+RINDX*DCON*ABS(psurf(n)-.001*pbase(i,1))
                DSQ=D*D
                WTIJ(i)=(RIS-DSQ)/(RIS+DSQ)
                WTIJ(i)=AMAX1(0.0,WTIJ(i))
              ENDDO
            else                                 
              DO I=max0(its,MINI),min0(ite,MAXI)
                RI=FLOAT(I)
                RY=RI-RA(N)
                RIS=RINDX*RINDX*sfcfacr*sfcfacr
                RSQ=RX*RX+RY*RY

                wtij(i)=(ris-rsq)/(ris+rsq)
                scratch = (abs (psurf(n)-.001*pbase(i,1))*DCON)
                pdfac=1.-AMIN1(1.0,scratch)
                wtij(i)=wtij(i)*pdfac
                WTIJ(i)=AMAX1(0.0,WTIJ(i))
              ENDDO
            endif


            if(SFC_SCHEME_VERT.eq.1) then         

              DO I=max0(its,MINI),min0(ite,MAXI)


                komax=max0(3,kpblt(i))
                IF (iprt) THEN
                  if (kpblt(i).gt.25 .and. ktau.ne.0)                         &
                                     write(6,552)inest,i,j,kpblt(i)
552               FORMAT('kpblt is gt 25, inest,i,j,kpblt=',4i4)
                ENDIF

                if(kpblt(i).gt.25) komax=3
                komin=1
                dk=float(komax)

                do k=komin,komax

                  wtsig(k)=float(komax-k+1)/dk
                  WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ(i)

                  
                  
                  
                  
                  
                  IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.gt.0)) THEN
                   QVB_AT_CUR_LOC = MAX(QVB(I,K,J),0.0)
                   
                   
                   
                   IF((ERFIVR.LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                    
                    
                    
                    QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                    IF(obs_scl_neg_qv_innov.eq.1) THEN
                     
                     
                     SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/ERFIVR))
                    ELSE
                     
                     
                     IF (iprt) then
                      write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                      call wrf_message(msg)
                     ENDIF
                     call wrf_error_fatal3("module_fddaobs_rtfdda.b",2040,&
'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                    ENDIF
                    
                    ERFIVR = ERFIVR*SCALE_FACTOR_NEG_QV_INNOV
                   ENDIF

                  ENDIF
                  WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*WTSIG(K)    &
                              *WTSIG(K)*ERFIVR

                enddo
              ENDDO

            else                                


























 
              DO I=max0(its,MINI),min0(ite,MAXI)

                dz_ramp = 1.0 / max( 1.0, vih2(i)-vih1(i) )   

           LML: do k = kts, kte 
                  wtsig(k) = min( 1.0, 1.0 + ( vih1(i)-zslab(i,k)+terrh(i) ) * dz_ramp )
                  wtsig(k) = max( 0.0, wtsig(k))

                  if(wtsig(k).le.0.0) EXIT LML
                    WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ(i)

                   
                   
                   
                   
                   IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.gt.0)) THEN
                    QVB_AT_CUR_LOC = MAX(QVB(I,K,J),0.0)
                    
                    
                    
                    
                    IF((ERFIVR.LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                     
                     
                     
                     QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                     IF(obs_scl_neg_qv_innov.eq.1) THEN
                      
                      
                      SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/ERFIVR))
                     ELSE
                      
                      
                      IF (iprt) then
                       write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                       call wrf_message(msg)
                      ENDIF
                      call wrf_error_fatal3("module_fddaobs_rtfdda.b",2119,&
'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                     ENDIF
                     
                     ERFIVR = ERFIVR*SCALE_FACTOR_NEG_QV_INNOV

                    ENDIF
                   ENDIF


                   WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*WTSIG(K)    &
                                *WTSIG(K)*ERFIVR
                  enddo LML
              ENDDO
            endif

          endif 

        endif   


      ELSE    


        timewt = get_timewt(xtime,dtmin,twindo,1.,timeob(n))











        slope = (RINFMN-RINFMX)/(psurf(n)-PFREE)

        RINFAC=SLOPE*POB+RINFMX-SLOPE*pfree
        RINFAC=AMAX1(RINFAC,RINFMN)
        RINFAC=AMIN1(RINFAC,RINFMX)


        if(nsndlev.gt.1) RINFAC = RINFMX 


        MAXI=IFIX(RA(N)+0.99+RINDX*RINFAC)
        MAXI=MIN0(ide-IGRID,MAXI)
        MINI=IFIX(RA(N)-RINDX*RINFAC-0.99)
        MINI=MAX0(1,MINI)

















        if(   RA(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RA(N).LE.float(ide)+RINFAC*RINDX/3                &
        .and. RB(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RB(N).LE.float(jde)+RINFAC*RINDX/3) then












          RJ=FLOAT(J)
          RX=RJ-RB(N)


          ERFIVR=ERRF(IVAR,N)

          QVB_AT_OB_LOC=ERRF(10,N)

          nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1




          njcsnd=nsndlev

          DO I=max0(its,MINI),min0(ite,MAXI)

            RI=FLOAT(I)
            RY=RI-RA(N)
            RIS=RINDX*RINFAC*RINDX*RINFAC
            RSQ=RX*RX+RY*RY

            WTIJ(i)=(RIS-RSQ)/(RIS+RSQ)

            WTIJ=AMAX1(0.0,WTIJ(i))


            if(nsndlev.eq.1) then
              rinprs=7.5

            else
             rinprs=3.0
            endif






            if(nsndlev.eq.1)then 









              do k=kte,1,-1
                pijk = .001*(pbase(i,k)+pp(i,k))

                if(pijk.ge.(pob+rinprs)) then
                  komin=k
                  go to 325
                endif
              enddo
              komin=1
 325          continue

              do k=3,kte
                pijk = .001*(pbase(i,k)+pp(i,k))
                if(pijk.le.(pob-rinprs)) then
                  komax=k
                  go to 326
                endif
              enddo
              komax=kte   
 326          continue




              if( (kpblt(i).le.komax) .and. (kpblt(i).ge.komin) ) then
                 kobs = 1
                 OBS_K: do k = komin, komax
                     if( pob .gt. .001*(pbase(i,k)+pp(i,k)) ) then
                        kobs = k
                        EXIT OBS_K
                     endif
                 enddo OBS_K

                 if(kobs.gt.kpbl_obs(n)) then

                     komin=max0(kobs, komin)   
                 else                          

                     komax=min0(kpblt(i), komax)
                 endif
              endif







              do k=1,kte
                reserf(k)=0.0
                wtsig(k)=0.0
              enddo



              if(nudge_pbl .or. komin.ge.kpblt(i)) then
                do k=komin,komax
                  pijk = .001*(pbase(i,k)+pp(i,k))
                  reserf(k)=erfivr
                  wtsig(k)=1.-abs(pijk-pob)/rinprs
                  wtsig(k)=amax1(wtsig(k),0.0)


                  WT(I,K)=WT(I,K)+TIMEWT*WTIJ(i)*wtsig(k)


                  
                  
                  
                  
                  
                  IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.GT.0)) THEN
                   QVB_AT_CUR_LOC = MAX(QVB(I,K,J),0.0)
                   
                   
                   
                   IF((ERFIVR.LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                    
                    
                    
                    QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                    IF(obs_scl_neg_qv_innov.eq.1) THEN
                     
                     
                     SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/ERFIVR))
                    ELSE
                     
                     
                     IF (iprt) then
                      write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                      call wrf_message(msg)
                     ENDIF
                     call wrf_error_fatal3("module_fddaobs_rtfdda.b",2343,&
'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                    ENDIF
                    reserf(k) = reserf(k)*SCALE_FACTOR_NEG_QV_INNOV
                   ENDIF
                  ENDIF

                  WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*        &
                              reserf(k)*wtsig(k)*wtsig(k)
                enddo
              endif

            else








              if(nlevs_ob(n+nsndlev-1).ne.lev_in_ob(n+nsndlev-1)) then
                IF (iprt) THEN
                  write(msg,*) "n = ",n,"nsndlev = ",nsndlev 
                  call wrf_message(msg)
                  write(msg,*) "nlevs_ob,lev_in_ob",                          &
                           nlevs_ob(n+nsndlev-1),lev_in_ob(n+nsndlev-1)
                  call wrf_message(msg)
                  call wrf_message("in nudobs.F: sounding level messed up, stopping")
                ENDIF
                call wrf_error_fatal3("module_fddaobs_rtfdda.b",2373,&
'wrf_fddaobs_in: in4dob' )
             endif       







              komin=1
              komax=kte-2



              do k=1,kte
                reserf(k)=0.0
                wtsig(k)=0.0
              enddo


              do k=komax,komin,-1
  
                pijk = .001*(pbase(i,k)+pp(i,k))


                if(pijk.gt.varobs(5,n)) then
                  go to 1501
                endif


                if(pijk.le.varobs(5,n+nsndlev-1)) then 
                  go to 1501
                endif




                slope = (RINFMN-RINFMX)/ (.001*pbase(i,1)-PFREE)
                RINFAC=SLOPE*pijk+RINFMX-SLOPE*PFREE              
                RINFAC=AMAX1(RINFAC,RINFMN)      
                RINFAC=AMIN1(RINFAC,RINFMX)
                RIS=RINDX*RINFAC*RINDX*RINFAC  
                RSQ=RX*RX+RY*RY               


                WTIJ(i)=(RIS-RSQ)/(RIS+RSQ)      
                WTIJ(i)=AMAX1(0.0,WTIJ(i))



               abs_pdiff_above = maxsnd_gap+1.0
               abs_pdiff_below = maxsnd_gap+1.0


                do nn=2,nsndlev


                  pobhi=-888888.

                  if(varobs(ivar,n+nn-1).gt.-800000.                           &
                  .and. varobs(5,n+nn-1).gt.-800000.) then
                    pobhi=varobs(5,n+nn-1)
                    nhi=n+nn-1


                    abs_pdiff_above=abs(pobhi-pijk)
                    if(pobhi.le.pijk .and. abs_pdiff_above.le.maxsnd_gap) then
                      go to 1502        
                    endif
                  endif

                enddo




                go to 1501
 1502           continue

                nlo=nhi-1
                do nnjc=nhi-1,n,-1 
                  if(varobs(ivar,nnjc).gt.-800000.                             &
                  .and. varobs(5,nnjc).gt.-800000.) then
                    poblo=varobs(5,nnjc)
                    nlo=nnjc


                    abs_pdiff_below=abs(poblo-pijk)
                    if(poblo.ge.pijk .and. abs_pdiff_below.le.maxsnd_gap) then
                      go to 1505        
                    endif
                  endif
                enddo





                go to 1501
 1505           continue





                if((abs_pdiff_below+abs_pdiff_above).gt.maxsnd_gap) then
                 goto 1501
                endif









                IF(abs(pobhi-poblo).lt.0.000001) THEN
                 pdiffj=0
                ELSE
                 
                 pdiffj=alog(pijk/poblo)/alog(pobhi/poblo)
                ENDIF

                reserf(k)=errf(ivar,nlo)+                               &
                            (errf(ivar,nhi)-errf(ivar,nlo))*pdiffj

                
                
                
                
                
                IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.GT.0)) THEN
                 QVB_AT_CUR_LOC = QVB(I,K,J)
                 
                 QVB_AT_OB_LOC=errf(10,nlo)+                               &
                              (errf(10,nhi)-errf(10,nlo))*pdiffj
                 
                 
                 
                 
                 IF((reserf(k).LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                  
                  
                  
                  QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                  IF(obs_scl_neg_qv_innov.eq.1) THEN
                   
                   
                   SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/reserf(k)))
                  ELSE
                   
                   
                   IF (iprt) then
                    write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                    call wrf_message(msg)
                   ENDIF
                   call wrf_error_fatal3("module_fddaobs_rtfdda.b",2531,&
'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                  ENDIF
                  reserf(k) = reserf(k)*SCALE_FACTOR_NEG_QV_INNOV
                 ENDIF

                ENDIF

                wtsig(k)=1.
  
 1501           continue



                if(nudge_pbl .or. k.gt.kpblt(i)) then

                  WT(I,K)=WT(I,K)+TIMEWT*WTIJ(i)*wtsig(k)
  
                  WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*        &
                              reserf(k)*wtsig(k)*wtsig(k)
                endif

              enddo   


            endif  




          ENDDO 

        endif 



      ENDIF 


    ENDIF  



    n=n+njcsnd


    if(n.gt.nstat)then

      go to 1203
    endif



  ENDDO  


 1203 continue






  DO K=kts,kte
    DO I=its,ite
      IF(WT(I,K).EQ.0)THEN
        WT2ERR(I,K)=0.0
      ENDIF
      IF(WT(I,K).EQ.0)THEN
        WT(I,K)=1.0
      ENDIF
    ENDDO
  ENDDO

126 CONTINUE

  IF(IVAR.GE.3)GOTO 170


 



  IF (IVAR == 1) THEN
     call calc_rcouple_scales(mu,msfy,rscale,ims,ime,its,ite)
  ELSE IF (IVAR == 2) THEN
     call calc_rcouple_scales(mu,msfx,rscale,ims,ime,its,ite)
  END IF
 
  DO K=1,kte

    DO I=i_s,i_e

      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
        W2EOWT=WT2ERR(I,K)/WT(I,K)
      ELSE
        W2EOWT=SAVWT(IPL,I,K)



      ENDIF






























        ATEN(i,k)=ATEN(i,k)+GIV*RSCALE(I)                        &
                    *W2EOWT*TFACI                           &
                    *ISWIND       *GFACTOR   








    ENDDO
  ENDDO

  IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
    DO K=1,kte
      DO I=its,ite
        SAVWT(IPL,I,K)=WT2ERR(I,K)/WT(I,K)


      ENDDO
    ENDDO
  ENDIF

  RETURN

170 CONTINUE



  IF(3-IVAR.LT.0)THEN
    GITQ=GIQ
  ELSE
    GITQ=GIT
  ENDIF
  IF(3-IVAR.LT.0)THEN
    ISTQ=ISMOIS
  ELSE
    ISTQ=ISTEMP
  ENDIF

  DO K=1,kte
    DO I=i_s,i_e
      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
        W2EOWT=WT2ERR(I,K)/WT(I,K)
      ELSE
        W2EOWT=SAVWT(IPL,I,K)
      ENDIF










 










      ATEN(i,k)=ATEN(i,k)+GITQ*MU(I)                       &
                  *W2EOWT*TFACI*ISTQ       *GFACTOR   








    ENDDO
  ENDDO

  IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR)) THEN
    DO K=1,kte
      DO I=its,ite
        SAVWT(IPL,I,K)=WT2ERR(I,K)/WT(I,K)
      ENDDO
    ENDDO
  ENDIF

  RETURN
  END SUBROUTINE nudob

  SUBROUTINE date_string(year, month, day, hour, minute, second, cdate)




  IMPLICIT NONE


  INTEGER, INTENT(IN)  :: year
  INTEGER, INTENT(IN)  :: month
  INTEGER, INTENT(IN)  :: day
  INTEGER, INTENT(IN)  :: hour
  INTEGER, INTENT(IN)  :: minute
  INTEGER, INTENT(IN)  :: second
  CHARACTER*19, INTENT(INOUT) :: cdate


  integer   :: ic                    

      cdate(1:19)  = "0000-00-00_00:00:00"
      write(cdate( 1: 4),'(i4)') year
      write(cdate( 6: 7),'(i2)') month
      write(cdate( 9:10),'(i2)') day
      write(cdate(12:13),'(i2)') hour
      write(cdate(15:16),'(i2)') minute
      write(cdate(18:19),'(i2)') second
      do ic = 1,19
        if(cdate(ic:ic) .eq. " ") cdate(ic:ic) = "0"
      enddo

  RETURN
  END SUBROUTINE date_string

  SUBROUTINE calc_rcouple_scales(a, msf, rscale, ims,ime, its,ite)

  IMPLICIT NONE


  INTEGER, INTENT(IN)  :: ims,ime           
  INTEGER, INTENT(IN)  :: its,ite           
  REAL, INTENT(IN)     :: a( ims:ime )      
  REAL, INTENT(IN)     :: msf( ims:ime )    
  REAL, INTENT(OUT)    :: rscale( ims:ime ) 


  integer :: i


  do i = its,ite
    rscale(i) = a(i)/msf(i)
  enddo

  RETURN
  END SUBROUTINE calc_rcouple_scales

  SUBROUTINE print_obs_info(iprt,inest,niobf,rio,rjo,rko,                &
                            prt_max,prt_freq,obs,stnid,lat,lon,          &
                            mlat,mlon,timeob,xtime)




  IMPLICIT NONE

  LOGICAL, intent(in)    :: iprt          
  INTEGER, intent(in)    :: inest         
  INTEGER, intent(in)    :: niobf         
  REAL,    intent(in)    :: rio(niobf)    
  REAL,    intent(in)    :: rjo(niobf)    
  REAL,    intent(in)    :: rko(niobf)    
  INTEGER, intent(in)    :: prt_max        
  INTEGER, intent(in)    :: prt_freq       
  INTEGER, intent(in)    :: obs(prt_max)  
  INTEGER, intent(in)    :: stnid(40,prt_max) 
  REAL,    intent(in)    :: lat(prt_max)  
  REAL,    intent(in)    :: lon(prt_max)  
  REAL,    intent(in)    :: mlat(prt_max) 
  REAL,    intent(in)    :: mlon(prt_max) 
  REAL,    intent(in)    :: timeob(niobf) 
  REAL,    intent(in)    :: xtime         


  integer :: i                    
  integer :: n                    
  integer :: pnx                  
  character(len=200) :: msg       
  character(len=20)  :: station_id 

  if(iprt) then
    if(prt_max.gt.0) then

      if(obs(1).ne.-999) then

        call wrf_message("")
        write(msg,fmt='(a,i4,a,f8.1,a)') 'REPORTING OBS MASS-PT LOCS FOR NEST ',  &
                                     inest,' AT XTIME=',xtime,' MINUTES'
        call wrf_message(msg)

        write(msg,fmt='(a,i4,a,i5,a)') 'FREQ=',prt_freq,', MAX=',prt_max,         &
                           ' LOCS, NEWLY READ OBS ONLY, -999 => OBS OFF PROC'
        call wrf_message(msg)
        call wrf_message("")

        write(msg,fmt='(3a)') '    OBS#     I       J       K     OBS LAT',       &
                          '  OBS LON   XLAT(I,J)  XLONG(I,J)  TIME(hrs)',     &
                          '  OBS STATION ID'
        call wrf_message(msg)

      endif
    endif



    do n=1,prt_max
       pnx = obs(n)
       if(pnx.ne.-999) then

           do i = 1,15
              station_id(i:i) = char(stnid(i,n))
           enddo
           write(msg,fmt='(2x,i7,3f8.3,2f9.3,2x,f9.3,2x,f9.3,3x,f6.2,7x,a15)')    &
               pnx,rio(pnx)-.5,rjo(pnx)-.5,rko(pnx),lat(n),lon(n),            &
               mlat(n),mlon(n),timeob(pnx),station_id
        call wrf_message(msg)
       endif
    enddo
    if(obs(1).ne.-999) call wrf_message("")
  endif
  END SUBROUTINE print_obs_info

  REAL FUNCTION ht_to_p( h, pbbc, ppbc, z, ic, jc, dx, dy,                    &
                         k_start, k_end, kds,kde, ims,ime, jms,jme, kms,kme )









  IMPLICIT NONE

  REAL,    INTENT(IN)  :: h                                
  INTEGER, INTENT(IN)  :: k_start, k_end                   
  INTEGER, INTENT(IN)  :: kds,kde                          
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        
  REAL,    INTENT(IN)  :: pbbc(kds:kde)                    
  REAL,    INTENT(IN)  :: ppbc(kds:kde)                    
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   
  INTEGER, INTENT(IN)  :: ic                               
  INTEGER, INTENT(IN)  :: jc                               
  REAL,    INTENT(IN)  :: dx                               
  REAL,    INTENT(IN)  :: dy                               


  INTEGER :: k               
  INTEGER :: klo             
  REAL :: zlo                
  REAL :: zhi                
  REAL :: p                  
  REAL :: ln_p               
  REAL :: ln_plo             
  REAL :: ln_phi             
  REAL :: z_at_p( kms:kme )  


  call get_height_column(z, ic, jc, dx, dy, z_at_p,                   &
                         k_start, k_end, kds,kde,                     &
                         ims,ime, jms,jme, kms,kme )




  ZLEVS: do k = k_start+1, k_end
    klo = k-1
    if(h .le. z_at_p(k)) then
      EXIT ZLEVS
    endif
  enddo ZLEVS

  zlo = z_at_p(klo)
  zhi = z_at_p(klo+1)


  ln_plo = log( pbbc(klo+1) + ppbc(klo+1) )
  ln_phi = log( pbbc(klo) + ppbc(klo) )
  if(h.le.zlo) then
    ln_p = ln_phi     
  else if (h.ge.zhi) then
    ln_p = ln_plo     
  else
    ln_p = ln_plo + (ln_phi-ln_plo)*((zhi-h)/(zhi-zlo)) 
  endif


  p = exp(ln_p)
  ht_to_p = p
  RETURN
  END FUNCTION ht_to_p

  SUBROUTINE get_height_column( z, ic, jc, dx, dy, z_at_p,                  &
                                k_start, k_end, kds,kde,                    &
                                ims,ime, jms,jme, kms,kme )




  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: k_start, k_end                   
  INTEGER, INTENT(IN)  :: kds,kde                          
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   
  INTEGER, INTENT(IN)  :: ic                               
  INTEGER, INTENT(IN)  :: jc                               
  REAL,    INTENT(IN)  :: dx                               
  REAL,    INTENT(IN)  :: dy                               
  REAL,    INTENT(OUT) :: z_at_p( kms:kme )                


  INTEGER :: k             


  do k = kds, kde
      z_at_p(k) =                                     & 
         (1.-DY)*( (1.-DX)*z(IC,K,JC) +               &
                            DX *z(IC+1,K,JC) ) +      &
             DY* ( (1.-DX)*z(IC,K,JC+1) +             &
                            DX *z(IC+1,K,JC+1) )
  enddo

  END SUBROUTINE get_height_column

  SUBROUTINE get_base_state_height_column( p_top, p00, t00, a, g, r_d,    &
                               znu, z_at_p,  k_start, k_end, kds,kde, kms,kme )











  IMPLICIT NONE

  REAL, INTENT(IN)     :: p_top        
  REAL, INTENT(IN)     :: p00          
  REAL, INTENT(IN)     :: t00          
  REAL, INTENT(IN)     :: a            
  REAL, INTENT(IN)     :: g                
  REAL, INTENT(IN)     :: r_d              
  INTEGER, INTENT(IN)  :: k_start, k_end   
  INTEGER, INTENT(IN)  :: kds,kde          
  INTEGER, INTENT(IN)  :: kms,kme          
  REAL, INTENT(IN)  :: znu( kms:kme )      
  REAL, INTENT(OUT) :: z_at_p( kms:kme )   


  integer :: k             
  real    :: ps0           
  real    :: pb(kms:kme)   
  real    :: logterm       
  real    :: ginv          
  
  ginv = 1/g


   do k = k_start, k_end
     pb(k) = znu(k)*(p00 - p_top) + p_top
   enddo


   do k = k_start, k_end
     logterm = log(pb(k)/p00)
     z_at_p(k) = .5*r_d*a*ginv*logterm*logterm - r_d*t00*ginv*logterm
   enddo

  END SUBROUTINE get_base_state_height_column

  REAL FUNCTION get_timewt(xtime,dtmin,twindo,scalef,obtime)




  IMPLICIT NONE

  REAL, INTENT(IN)  :: xtime              
  REAL, INTENT(IN)  :: dtmin              
  REAL, INTENT(IN)  :: twindo             
  REAL, INTENT(IN)  :: scalef             
  REAL, INTENT(IN)  :: obtime             


  real :: fdtim            
  real :: tw1              
  real :: tw2              
  real :: tconst           
  real :: ttim             
  real :: dift             
  real :: timewt           


  FDTIM=XTIME-DTMIN

  TW1=TWINDO/2.*60.*scalef
  TW2=TWINDO*60.*scalef
  TCONST=1./TW1
  TIMEWT=0.0
  TTIM=obtime*60.

  DIFT=ABS(FDTIM-TTIM)
  IF(DIFT.LE.TW1)TIMEWT=1.0
  IF(DIFT.GT.TW1.AND.DIFT.LE.TW2) THEN
     IF(FDTIM.LT.TTIM)TIMEWT=(FDTIM-(TTIM-TW2))*TCONST
     IF(FDTIM.GT.TTIM)TIMEWT=((TTIM+TW2)-FDTIM)*TCONST
  ENDIF
  get_timewt = timewt
  END FUNCTION get_timewt

  SUBROUTINE print_vif_var(var, vif, nfullmin, nrampmin )




  IMPLICIT NONE

  character(len=4), intent(in)  :: var      
  real,             intent(in)  :: vif(6)   
  real,             intent(in)  :: nfullmin 
  real,             intent(in)  :: nrampmin 


  character(len=200) :: msg1, msg2
  character(len=8) :: regime
  real :: nfullr1, nrampr1
  real :: nfullr2, nrampr2
  real :: nfullr4, nrampr4

  nfullr1 = vif(1)
  nrampr1 = vif(2)
  nfullr2 = vif(3)
  nrampr2 = vif(4)
  nfullr4 = vif(5)
  nrampr4 = vif(6)

  if(var.eq.'wind') then
    write(msg1,fmt='(a)') '  For winds:'
  elseif (var.eq.'temp') then
    write(msg1,fmt='(a)') '  For temperature:'
  elseif (var.eq.'mois') then
    write(msg1,fmt='(a)') '  For moisture:'
  else
    write(msg1,fmt='(a,a4)') 'Unknown variable type: ',var
    call wrf_message(msg1)
    call wrf_error_fatal3("module_fddaobs_rtfdda.b",3113,&
'print_vif_var: module_fddaobs_rtfdda STOP' )
  endif
      
  call wrf_message(msg1)


  call print_vif_regime(1, nfullr1, nrampr1, nfullmin, nrampmin) 
  call print_vif_regime(2, nfullr2, nrampr2, nfullmin, nrampmin) 
  call print_vif_regime(4, nfullr4, nrampr4, nfullmin, nrampmin) 

  END SUBROUTINE print_vif_var

  SUBROUTINE print_vif_regime(reg, nfullr, nrampr, nfullmin, nrampmin )




  IMPLICIT NONE

  integer, intent(in)  :: reg          
  real,    intent(in)  :: nfullr       
  real,    intent(in)  :: nrampr       
  real,    intent(in)  :: nfullmin     
  real,    intent(in)  :: nrampmin     


  character(len=200) :: msg1, msg2
  character(len=8) :: regime

  if(reg.eq.1) then
     write(regime,fmt='(a)') 'Regime 1'
  elseif (reg.eq.2) then
     write(regime,fmt='(a)') 'Regime 2'
  elseif (reg.eq.4) then
     write(regime,fmt='(a)') 'Regime 4'
  else
     write(msg1,fmt='(a,i3)') 'Unknown regime number: ',reg
     call wrf_message(msg1)
     call wrf_error_fatal3("module_fddaobs_rtfdda.b",3152,&
'print_vif_regime: module_fddaobs_rtfdda STOP' )
  endif


  if(nfullr.lt.0) then
     if(nfullr.eq.-5000) then
       write(msg1,fmt='(2x,a8,a)') regime, ': Full weighting to the PBL top'
     elseif (nfullr.lt.-5000) then
       write(msg1,fmt='(2x,a8,a,i4,a)') regime, ': Full weighting to ',int(-5000.-nfullr), &
                                          ' m above the PBL top'
     else
       write(msg1,fmt='(2x,a8,a,i4,a)') regime, ': Full weighting to ',int(nfullr+5000.),  &
                                          ' m below the PBL top'
     endif
  else
     write(msg1,fmt='(2x,a8,a,i4,a)') regime, ': Full weighting through ',                 &
                                     int(max(nfullr,nfullmin)),' m'
  endif


  if(nrampr.lt.0) then
     if(nrampr.eq.-5000) then
       write(msg2,fmt='(a)') ' and a vertical rampdown up to the PBL top.'
     elseif (nrampr.lt.-5000) then
       write(msg2,fmt='(a,i4,a)') ' and a vertical rampdown to ',int(-5000.-nrampr),    &
                            ' m above the PBL top.'
     else
       write(msg2,fmt='(a,i4,a)') ' and a vertical rampdown to ',int(nrampr+5000.),     &
                            ' m below the PBL top.'
     endif
  else
     write(msg2,fmt='(a,i4,a)') ' and a vertical rampdown in the next ',                &
                          int(max(nrampr,nrampmin)),' m.'
  endif
  call wrf_message(TRIM(msg1)//msg2)

  END SUBROUTINE print_vif_regime


END MODULE module_fddaobs_rtfdda

