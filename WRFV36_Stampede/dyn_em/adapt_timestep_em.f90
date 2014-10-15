RECURSIVE SUBROUTINE adapt_timestep(grid, config_flags)
















  USE module_domain
  USE module_configure
  USE module_dm, ONLY : wrf_dm_maxval, wrf_dm_minval, wrf_dm_mintile_double, wrf_dm_tile_val_int, wrf_dm_maxtile_real
  USE module_bc_em

  IMPLICIT NONE

  TYPE(domain) , TARGET , INTENT(INOUT)      :: grid
  TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  LOGICAL                                    :: use_last2
  REAL                                       :: curr_secs
  REAL                                       :: max_increase_factor
  REAL                                       :: time_to_output, &
                                                time_to_bc
  INTEGER                                    :: idex=0, jdex=0
  INTEGER                                    :: rc
  TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval, dtInterval
  TYPE(WRFU_TimeInterval)                    :: dtInterval_horiz
  TYPE(WRFU_TimeInterval)                    :: dtInterval_vert  
  TYPE(WRFU_TimeInterval)                    :: parent_dtInterval
  INTEGER                                    :: num_small_steps
  integer                                    :: tile
  LOGICAL                                    :: stepping_to_bc
  INTEGER                                    :: bc_time, output_time
  double precision                           :: dt = 0
  INTEGER, PARAMETER                         :: precision = 100
  INTEGER                                    :: dt_num, dt_den, dt_whole
  INTEGER                                    :: num, den, history_interval_sec
  TYPE(WRFU_TimeInterval)                    :: last_dtInterval
  REAL                                       :: real_time
  REAL                                       :: max_vert_cfl, max_horiz_cfl

  
  
  
  

  use_last2 = .FALSE.

  
  
  

  CALL WRFU_TimeIntervalSet(grid%last_dtInterval,  S=grid%last_dt_sec, &
                         Sn=grid%last_dt_sec_num, Sd=grid%last_dt_sec_den)

  
  
  
  
  

  if (grid%last_step_updated == grid%itimestep) then
     return
  else
     grid%last_step_updated = grid%itimestep
  endif

  
  
  
  if (grid%id .ne. 1) then
     grid%adapt_step_using_child = grid%parents(1)%ptr%adapt_step_using_child;
  endif

  
  
  
  
  
  







  last_dtInterval = grid%last_dtInterval

  
  
  

  tmpTimeInterval = domain_get_current_time ( grid ) - &
                    domain_get_sim_start_time ( grid )

  
  
  
  
  
  
  curr_secs = real_time(tmpTimeInterval)

  
  
  
  
  max_increase_factor = 1. + grid%max_step_increase_pct / 100.

  
  
  
  
  
  
  if ( ( domain_get_advanceCount ( grid ) .EQ. 1 ) .AND. ( .NOT. config_flags%restart ) ) then
     if ( grid%starting_time_step_den .EQ. 0 ) then
        CALL WRFU_TimeIntervalSet(dtInterval, Sn=grid%starting_time_step, Sd=1)
     else
        CALL WRFU_TimeIntervalSet(dtInterval, Sn=grid%starting_time_step, Sd=grid%starting_time_step_den)
     end if
     curr_secs = 0
     CALL WRFU_TimeIntervalSet(last_dtInterval, Sn=0, Sd=1)

  else

     if (grid%stepping_to_time) then
        max_vert_cfl = grid%last_max_vert_cfl
        max_horiz_cfl = grid%last_max_horiz_cfl
     else
        max_vert_cfl = grid%max_vert_cfl
        max_horiz_cfl = grid%max_horiz_cfl
     endif

     CALL calc_dt(dtInterval_vert, max_vert_cfl, max_increase_factor, &
          precision, last_dtInterval, grid%target_cfl)

     CALL calc_dt(dtInterval_horiz, max_horiz_cfl, max_increase_factor, &
          precision, last_dtInterval, grid%target_hcfl)

     if (dtInterval_vert < dtInterval_horiz) then
        dtInterval = dtInterval_vert
     else
        dtInterval = dtInterval_horiz
     endif

  endif

  

  num = NINT( max_increase_factor * precision )
  den = precision
  tmpTimeInterval = last_dtInterval * num / den
  if ( (domain_get_current_time ( grid ) .ne. domain_get_start_time ( grid )) &
       .and. (dtInterval .gt. tmpTimeInterval ) ) then
     dtInterval = tmpTimeInterval
  endif

  
  
  
  
  dt = real_time(dtInterval)
  num = NINT(dt * precision)
  den = precision
  CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)

  

  if ( grid%max_time_step_den .EQ. 0 ) then
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%max_time_step, Sd=1)
  else
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%max_time_step, Sd=grid%max_time_step_den)
  end if
  if (dtInterval .gt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  

  if ( grid%min_time_step_den .EQ. 0 ) then
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%min_time_step, Sd=1)
  else
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%min_time_step, Sd=grid%min_time_step_den)
  end if
  if (dtInterval .lt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  
  
  
  
  
  
  
  
  if (grid%nested) then

     dt = real_time(dtInterval)
        
     if (.not. grid%adapt_step_using_child) then 

        
     
        num_small_steps = CEILING( grid%parents(1)%ptr%dt / dt )


        call wrf_dm_maxval(num_small_steps, idex, jdex)

        dtInterval = domain_get_time_step(grid%parents(1)%ptr) / &
             num_small_steps
     else

        num_small_steps = FLOOR( grid%parents(1)%ptr%dt / dt )


        call wrf_dm_minval(num_small_steps, idex, jdex)

        if (num_small_steps < 1) then
           num_small_steps = 1
        endif

     endif
  endif


  
  
  
  
  dt = real_time(dtInterval)


  call wrf_dm_mintile_double(dt, tile)
  CALL WRFU_TimeIntervalGet(dtInterval,Sn=dt_num,Sd=dt_den,S=dt_whole)
  call wrf_dm_tile_val_int(dt_num, tile)
  call wrf_dm_tile_val_int(dt_den, tile)
  call wrf_dm_tile_val_int(dt_whole, tile)
  CALL WRFU_TimeIntervalSet(dtInterval, Sn = dt_whole*dt_den + dt_num, Sd = dt_den)

  call wrf_dm_maxtile_real(grid%max_vert_cfl, tile)
  call wrf_dm_maxtile_real(grid%max_horiz_cfl, tile)


  if ((grid%nested) .and. (grid%adapt_step_using_child)) then 

     grid%dt = real_time(dtInterval)

     
     grid%parents(1)%ptr%dt = grid%dt * num_small_steps
     parent_dtInterval = dtInterval * num_small_steps

     
     
     
     CALL WRFU_ClockSet ( grid%parents(1)%ptr%domain_clock,        &
          timeStep=parent_dtInterval, &
          rc=rc )
     
  endif


  
  
  
  
  

  grid%stepping_to_time = .FALSE.
  time_to_bc = grid%interval_seconds - grid%dtbc
  num = INT(time_to_bc * precision + 0.5)
  den = precision
  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)
  
  if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
       ( tmpTimeInterval .GT. dtInterval ) ) then
     dtInterval = tmpTimeInterval / 2
     
     use_last2 = .TRUE.
     stepping_to_bc = .true.
     grid%stepping_to_time = .TRUE.
     
  elseif (tmpTimeInterval .LE. dtInterval) then
     
     bc_time = NINT ( (curr_secs + time_to_bc) / ( grid%interval_seconds ) ) &
          * ( grid%interval_seconds )
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=bc_time)
     dtInterval = tmpTimeInterval - &
          (domain_get_current_time(grid) - domain_get_sim_start_time(grid))
     
     use_last2 = .TRUE.
     stepping_to_bc = .true.
     grid%stepping_to_time = .TRUE.
  else
     stepping_to_bc = .false.
  endif

  
  
  
  
  
  

  if ((grid%step_to_output_time) .and. (.not. stepping_to_bc) .and. &
       (.not. grid%nested)) then

     IF ( grid%history_interval_m .EQ. 0 ) grid%history_interval_m = grid%history_interval
     history_interval_sec = grid%history_interval_s + grid%history_interval_m*60 + &
                            grid%history_interval_h*3600 + grid%history_interval_d*86400

     time_to_output = history_interval_sec - &
          mod( curr_secs, REAL(history_interval_sec) )
     num = INT(time_to_output * precision + 0.5)
     den = precision
     call WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)

     if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
          ( tmpTimeInterval .GT. dtInterval ) ) then
        dtInterval = tmpTimeInterval / 2
        use_last2 = .TRUE.
        grid%stepping_to_time = .TRUE.

     elseif (tmpTimeInterval .LE. dtInterval) then
        
        
        
        

        
        
        
        
        output_time = NINT ( (curr_secs + time_to_output) /  &
             (history_interval_sec) ) * (history_interval_sec)
        CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=output_time)
        dtInterval = tmpTimeInterval - &
             (domain_get_current_time(grid) - domain_get_sim_start_time(grid))

        use_last2 = .TRUE.
        grid%stepping_to_time = .TRUE.
     endif
  endif

  
  
  
  
  
  

  if (grid%id == 1) then
     if ((grid%adaptation_domain > 1) .and. &
          (grid%max_dom == 2) .and. &
          (.not. grid%stepping_to_time) .and. &
          (domain_get_current_time(grid) .ne. &
          domain_get_start_time(grid)) &
          ) then
        
        grid%adapt_step_using_child = .TRUE.
     else
        grid%adapt_step_using_child = .FALSE.
     endif
  endif


  if (use_last2) then
     grid%last_dtInterval = last_dtInterval
     grid%last_max_vert_cfl = grid%last_max_vert_cfl
     grid%last_max_horiz_cfl = grid%last_max_horiz_cfl
  else
     grid%last_dtInterval = dtInterval
     grid%last_max_vert_cfl = grid%max_vert_cfl
     grid%last_max_horiz_cfl = grid%max_horiz_cfl
  endif

  grid%dt = real_time(dtInterval)

  grid%last_max_vert_cfl = grid%max_vert_cfl

  
  
  
  CALL WRFU_ClockSet ( grid%domain_clock,        &
       timeStep=dtInterval, &
       rc=rc )

  
  
  
  
  
  
  
  if ((grid%id == 1) .and. (grid%adapt_step_using_child)) then
     
     
     
     
     
     
     
     if (grid%stepping_to_time) then
        grid%adapt_step_using_child = .FALSE.
     endif
     call adapt_timestep(grid%nests(1)%ptr, config_flags)
  endif

  
  
  
  if (grid%id == 1) then
     CALL lbc_fcx_gcx ( grid%fcx , grid%gcx , grid%spec_bdy_width , &
          grid%spec_zone , grid%relax_zone , grid%dt , config_flags%spec_exp , &
          config_flags%specified , config_flags%nested )
  endif



  CALL WRFU_TimeIntervalGet(grid%last_dtInterval,  S=grid%last_dt_sec, &
                         Sn=grid%last_dt_sec_num, Sd=grid%last_dt_sec_den)

END SUBROUTINE adapt_timestep

SUBROUTINE calc_dt(dtInterval, max_cfl, max_increase_factor, precision, &
     last_dtInterval, target_cfl)

  USE module_domain

  TYPE(WRFU_TimeInterval) ,INTENT(OUT)      :: dtInterval
  REAL                    ,INTENT(IN)       :: max_cfl
  REAL                    ,INTENT(IN)       :: max_increase_factor
  INTEGER                 ,INTENT(IN)       :: precision
  REAL                    ,INTENT(IN)       :: target_cfl
  TYPE(WRFU_TimeInterval) ,INTENT(IN)       :: last_dtInterval
  REAL                                      :: factor
  INTEGER                                   :: num, den
  

  if (max_cfl < 0.001) then 
     
     
     
     
     num = INT(max_increase_factor * precision + 0.5)
     den = precision
     dtInterval = last_dtInterval * num / den

  else
     
     
     
     
     
     if (max_cfl .gt. target_cfl) then
        
        
        
        
        
        
        factor = ( target_cfl - 0.5 * (max_cfl - target_cfl) ) / max_cfl
        num = INT(factor * precision + 0.5)
        den = precision

        dtInterval = last_dtInterval * num / den

     else
        
        
        
        
        factor = target_cfl / max_cfl
        num = INT(factor * precision + 0.5)
        den = precision
        dtInterval = last_dtInterval * num / den
     endif
  endif

END SUBROUTINE calc_dt


FUNCTION real_time( timeinterval ) RESULT ( out_time )

  USE module_domain

  IMPLICIT NONE 












      REAL :: out_time
      INTEGER :: dt_num, dt_den, dt_whole


      TYPE(WRFU_TimeInterval), intent(INOUT) :: timeinterval

      CALL WRFU_TimeIntervalGet(timeinterval,Sn=dt_num,Sd=dt_den,S=dt_whole)
      if (ABS(dt_den) < 1) then
         out_time = dt_whole
      else
         out_time = dt_whole + dt_num / REAL(dt_den)
      endif
END FUNCTION 

FUNCTION real_time_r8( timeinterval ) RESULT ( out_time )

  USE module_domain

  IMPLICIT NONE 












      REAL(KIND=8) :: out_time
      INTEGER(selected_int_kind(14)) :: dt_whole
      INTEGER :: dt_num, dt_den


      TYPE(WRFU_TimeInterval), intent(INOUT) :: timeinterval

      CALL WRFU_TimeIntervalGet(timeinterval,Sn=dt_num,Sd=dt_den,S_i8=dt_whole)
      if (ABS(dt_den) < 1) then
         out_time = REAL(dt_whole)
      else
         out_time = REAL(dt_whole) + REAL(dt_num,8)/REAL(dt_den,8)
      endif
END FUNCTION real_time_r8
