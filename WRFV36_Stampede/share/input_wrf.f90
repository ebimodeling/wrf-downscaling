


  SUBROUTINE input_wrf ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_io
    USE module_io_wrf
    USE module_date_time
    USE module_bc_time_utilities
    USE module_utility
    IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024


  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(IN) :: switch
    INTEGER, INTENT(INOUT) :: ierr

    
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    TYPE( fieldlist ), POINTER :: p

    INTEGER newswitch, itrace

    INTEGER       iname(9)
    INTEGER       iordering(3)
    INTEGER       icurrent_date(24)
    INTEGER       i,j,k
    INTEGER       icnt
    INTEGER       ndim
    INTEGER       ilen
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*256 errmess, currtimestr
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    Type(WRFU_Time) time, currtime, currentTime
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    CHARACTER*80  fname
    CHARACTER*80  dname, memord
    LOGICAL dryrun
    INTEGER idt
    INTEGER itmp
    INTEGER filestate, ierr3
    INTEGER :: ide_compare , jde_compare , kde_compare
    CHARACTER (len=19) simulation_start_date , first_date_input , first_date_nml
    INTEGER first_date_start_year   , &
            first_date_start_month  , &
            first_date_start_day    , &
            first_date_start_hour   , &
            first_date_start_minute , &
            first_date_start_second
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    LOGICAL reset_simulation_start
    REAL dx_compare , dy_compare , dum
    INTEGER :: num_land_cat_compare
    CHARACTER (LEN=256) :: MMINLU

    

    REAL, ALLOCATABLE, DIMENSION(:      ) ::  f_vint_1d
    REAL, ALLOCATABLE, DIMENSION(:,:,:  ) ::  f_vint_3d
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) ::  f_vint_4d
    integer :: ed1_c,em1_c,ep1_c
    integer :: ed2_c,em2_c,ep2_c
    integer :: n_ref_m,i_inter

    

    INTEGER max_wrf_alarms_compare, seconds
    CHARACTER*80 alarmname, timestr
    TYPE(WRFU_Time) :: curtime, ringTime
    TYPE(WRFU_TimeInterval) :: interval, interval2
    integer s, iring

    

    CHARACTER (LEN=80) :: input_name
    LOGICAL :: this_is_an_ideal_run
    INTEGER :: loop, hypsometric_opt

    CHARACTER (LEN=256) :: a_message











    WRITE(wrf_err_message,*)'input_wrf: begin, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    CALL modify_io_masks ( grid%id )   

    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )


    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: input_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal3("<stdin>",272,&
wrf_err_message )
    ENDIF

    WRITE(wrf_err_message,*)'input_wrf: fid,filestate = ',fid,filestate
    CALL wrf_debug( 300 , wrf_err_message )

    dryrun        = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'input_wrf: dryrun = ',dryrun,' switch ',switch
    CALL wrf_debug( 300 , wrf_err_message )

    check_if_dryrun : IF ( .NOT. dryrun ) THEN

      IF ( switch .EQ. input_only ) THEN

        
        
        
        

        CALL wrf_get_dom_ti_char ( fid , 'TITLE' , input_name , ierr )
        grid%this_is_an_ideal_run = INDEX(TRIM(input_name) , 'IDEAL' ) .NE. 0
        IF ( grid%this_is_an_ideal_run ) THEN
          grid%hypsometric_opt = 1
          config_flags%hypsometric_opt = 1
	  DO loop = 1 , grid%max_dom
            CALL nl_set_hypsometric_opt ( loop , 1 ) 
          END DO
          WRITE(wrf_err_message,*)'Ideal cases do not support the hypsometric option.'
          CALL wrf_debug( 0 , wrf_err_message )
        END IF
      END IF

      

      IF ( switch .EQ. restart_only ) THEN

        
 
        CALL wrf_get_dom_ti_integer( fid , 'MAX_WRF_ALARMS', max_wrf_alarms_compare, 1, icnt, ierr )
        IF ( max_wrf_alarms_compare .NE. MAX_WRF_ALARMS ) THEN
          WRITE(wrf_err_message,*)'MAX_WRF_ALARMS different in restart file (',max_wrf_alarms_compare,&
                                  ') from in code (',MAX_WRF_ALARMS,').  Disregarding info in restart file.'
        ELSE
          curtime = domain_get_current_time( grid )
          DO i = auxinput1_only, MAX_WRF_ALARMS
            IF ( grid%alarms_created(i) .AND. .NOT. i .EQ. boundary_only ) THEN
              write(alarmname,'("WRF_ALARM_ISRINGING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), iring, 1, icnt, ierr )
  
              write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), seconds, 1, icnt, ierr )
              IF ( ierr .EQ. 0 &
                   .AND. seconds .GE. 0 ) THEN  
                                                
  
                
                
                CALL WRFU_AlarmGet( grid%alarms(i), ringinterval=interval2 )

                IF (config_flags%override_restart_timers) THEN
                   IF (i .EQ. restart_only) THEN
                      seconds = grid%restart_interval_d * 86400 + &
                                grid%restart_interval_h *  3600 + &
                                grid%restart_interval_m *    60 + &
                                grid%restart_interval   *    60 + &
                                grid%restart_interval_s
                   ENDIF
                ENDIF

                CALL WRFU_TimeIntervalSet(interval,S=seconds)
                ringTime = curtime + interval
                CALL WRFU_AlarmSet( grid%alarms(i), RingInterval=interval2, RingTime=ringTime )

              ENDIF

              IF ( iring .EQ. 1 ) THEN
                CALL WRFU_AlarmRingerOn( grid%alarms( i ) )
              ELSE
                CALL WRFU_AlarmRingerOff( grid%alarms( i ) )
              ENDIF
            ENDIF
          ENDDO
        ENDIF

     
     

      IF ( switch .EQ. restart_only .AND. .NOT. config_flags%override_restart_timers ) THEN

        
 
        CALL wrf_get_dom_ti_integer( fid , 'MAX_WRF_ALARMS', max_wrf_alarms_compare, 1, icnt, ierr )
        IF ( max_wrf_alarms_compare .NE. MAX_WRF_ALARMS ) THEN
          WRITE(wrf_err_message,*)'MAX_WRF_ALARMS different in restart file (',max_wrf_alarms_compare,&
                                  ') from in code (',MAX_WRF_ALARMS,').  Disregarding info in restart file.'
        ELSE
          curtime = domain_get_current_time( grid )
          DO i = 1, auxinput1_only-1
            IF ( grid%alarms_created(i) .AND. .NOT. i .EQ. boundary_only ) THEN
              write(alarmname,'("WRF_ALARM_ISRINGING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), iring, 1, icnt, ierr )
  
              write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_",i2.2)')i
              CALL wrf_get_dom_ti_integer( fid, TRIM(alarmname), seconds, 1, icnt, ierr )
              IF ( ierr .EQ. 0 &
                   .AND. seconds .GE. 0 ) THEN  
                                                
  
                
                
                CALL WRFU_AlarmGet( grid%alarms(i), ringinterval=interval2 )

                IF (config_flags%override_restart_timers) THEN
                   IF (i .EQ. history_only) THEN
                      seconds = grid%history_interval_d * 86400 + &
                                grid%history_interval_h *  3600 + &
                                grid%history_interval_m *    60 + &
                                grid%history_interval   *    60 + &
                                grid%history_interval_s
                   ENDIF
                ENDIF

                CALL WRFU_TimeIntervalSet(interval,S=seconds)
                ringTime = curtime + interval
                CALL WRFU_AlarmSet( grid%alarms(i), RingInterval=interval, RingTime=ringTime )

              ENDIF

              IF ( iring .EQ. 1 ) THEN
                CALL WRFU_AlarmRingerOn( grid%alarms( i ) )
              ELSE
                CALL WRFU_AlarmRingerOff( grid%alarms( i ) )
              ENDIF
            ENDIF
          ENDDO
        ENDIF

      ENDIF

      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , simulation_start_date , ierr )
      CALL nl_get_reset_simulation_start ( 1, reset_simulation_start )
      IF ( ( ierr .EQ. 0 ) .AND. ( .NOT. reset_simulation_start ) ) THEN
        
        READ ( simulation_start_date , fmt = '(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)' ) &
               simulation_start_year,   simulation_start_month,                     &
               simulation_start_day,    simulation_start_hour,                      &
               simulation_start_minute, simulation_start_second
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( switch .EQ. input_only  ) THEN
          WRITE(wrf_err_message,*)fid,' input_wrf, input_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ELSE IF ( switch .EQ. restart_only  ) THEN
          WRITE(wrf_err_message,*)fid,' input_wrf, restart_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ENDIF
      ELSE
        CALL nl_get_start_year   ( 1 , simulation_start_year   )
        CALL nl_get_start_month  ( 1 , simulation_start_month  )
        CALL nl_get_start_day    ( 1 , simulation_start_day    )
        CALL nl_get_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_get_start_minute ( 1 , simulation_start_minute )
        CALL nl_get_start_second ( 1 , simulation_start_second )
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( reset_simulation_start ) THEN
          CALL wrf_message('input_wrf: forcing SIMULATION_START_DATE = head_grid start time')
          CALL wrf_message('           due to namelist variable reset_simulation_start')
        ELSE
          CALL wrf_message('input_wrf: SIMULATION_START_DATE not available in input')
          CALL wrf_message('will use head_grid start time from namelist')
        ENDIF
      ENDIF
      
      
      
      CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
      
      WRITE(wrf_err_message,*) 'input_wrf:  set xtime to ',grid%xtime
      CALL wrf_debug ( 100, TRIM(wrf_err_message) )
    ELSE IF ( switch .EQ. auxinput1_only ) then
      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , first_date_input , ierr )
      WRITE(wrf_err_message,*)'metgrid input_wrf.F first_date_input = ',first_date_input
      CALL wrf_message(wrf_err_message)
      CALL nl_get_start_year   ( 1 , first_date_start_year   )
      CALL nl_get_start_month  ( 1 , first_date_start_month  )
      CALL nl_get_start_day    ( 1 , first_date_start_day    )
      CALL nl_get_start_hour   ( 1 , first_date_start_hour   )
      CALL nl_get_start_minute ( 1 , first_date_start_minute )
      CALL nl_get_start_second ( 1 , first_date_start_second )
      WRITE ( first_date_nml, fmt = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             first_date_start_year,   first_date_start_month,                     &
             first_date_start_day,    first_date_start_hour,                      &
             first_date_start_minute, first_date_start_second
      WRITE (wrf_err_message,*) 'metgrid input_wrf.F first_date_nml = ',first_date_nml
      CALL wrf_message( TRIM(wrf_err_message ) )
    ENDIF

    
    

    IF ( ( switch .EQ.     input_only  ) .OR. &
         ( switch .EQ. auxinput1_only ) ) THEN
       ierr = 0
       CALL wrf_get_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,    ide_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' ,  jde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       IF ( ierr3 .NE. 0 ) CALL wrf_error_fatal3("<stdin>",494,&
'wrf_get_dom_ti_integer getting dimension information from dataset' )


       IF ( ( switch .EQ. input_only ) .AND. ( config_flags%io_form_input .EQ. 2 ) ) THEN

        
        
        

        hypsometric_opt = -1
        CALL wrf_get_dom_ti_integer ( fid , 'HYPSOMETRIC_OPT' , hypsometric_opt , 1 , icnt , ierr )
        IF ( ( hypsometric_opt .NE. 1 ) .AND. ( hypsometric_opt .NE. 2 ) ) THEN
          grid%hypsometric_opt = 1
          config_flags%hypsometric_opt = 1
	  DO loop = 1 , grid%max_dom
            CALL nl_set_hypsometric_opt ( loop , 1 ) 
          END DO
          WRITE(wrf_err_message,*)'Resetting the hypsometric_opt from default value of 2 to 1'
          CALL wrf_debug( 0 , wrf_err_message )
        END IF
       END IF

       

       CALL wrf_get_dom_ti_real ( fid , 'DX' ,  dx_compare , 1 , icnt , ierr )
       CALL wrf_get_dom_ti_real ( fid , 'DY' ,  dy_compare , 1 , icnt , ierr )
       IF ( ( ABS ( dx_compare - config_flags%dx ) .GT. 1.E-5 * dx_compare ) .OR. &
            ( ABS ( dy_compare - config_flags%dy ) .GT. 1.E-5 * dy_compare ) ) THEN
          IF ( ( config_flags%polar ) .AND. ( config_flags%grid_id .EQ. 1 ) ) THEN
             WRITE(wrf_err_message,*)'input_wrf: DX and DY from input file expected to be wrong'
             CALL wrf_debug ( 1 , wrf_err_message )
          ELSE
             WRITE(wrf_err_message,*)'dx and dy from file     ',dx_compare,dy_compare
             CALL wrf_message(wrf_err_message)
             WRITE(wrf_err_message,*)'dx and dy from namelist ',config_flags%dx,config_flags%dy
             CALL wrf_message(wrf_err_message)
             CALL wrf_error_fatal3("<stdin>",531,&
'DX and DY do not match comparing namelist to the input file' )
          END IF
       END IF
    END IF

    IF ( ( switch .EQ. input_only ) .OR. ( switch .EQ. auxinput2_only ) .OR. ( switch .EQ. auxinput1_only ) ) THEN
       ierr = 0
       IF      ( ( switch .EQ. input_only ) .OR. ( switch .EQ. auxinput2_only ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( switch .EQ. auxinput1_only ) .AND. ( grid%id .GE. 2 ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'i_parent_start' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( switch .EQ. auxinput1_only ) .AND. ( grid%id .EQ. 1 ) ) THEN
          itmp  = config_flags%i_parent_start
          ierr3 = 0 
       END IF
       ierr = max( ierr, ierr3 )
       IF ( itmp .NE. config_flags%i_parent_start ) THEN
          ierr = 1
          WRITE(wrf_err_message,*)'i_parent_start from namelist.input file = ',config_flags%i_parent_start
          CALL wrf_message(wrf_err_message)
          WRITE(wrf_err_message,*)'i_parent_start from gridded input file  = ',itmp
          CALL wrf_message(wrf_err_message)
       END IF
       IF      ( ( switch .EQ. input_only ) .OR. ( switch .EQ. auxinput2_only ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( switch .EQ. auxinput1_only ) .AND. ( grid%id .GE. 2 ) ) THEN
          CALL wrf_get_dom_ti_integer ( fid , 'j_parent_start' ,    itmp , 1 , icnt , ierr3 )
       ELSE IF ( ( switch .EQ. auxinput1_only ) .AND. ( grid%id .EQ. 1 ) ) THEN
          itmp  = config_flags%j_parent_start
          ierr3 = 0 
       END IF
       ierr = max( ierr, ierr3 )
       IF ( itmp .NE. config_flags%j_parent_start ) THEN
          ierr = 1
          WRITE(wrf_err_message,*)'j_parent_start from namelist.input file = ',config_flags%j_parent_start
          CALL wrf_message(wrf_err_message)
          WRITE(wrf_err_message,*)'j_parent_start from gridded input file  = ',itmp
          CALL wrf_message(wrf_err_message)
       END IF
       IF ( ierr .NE. 0 ) THEN
          CALL wrf_error_fatal3("<stdin>",572,&
'Nest start locations do not match: namelist.input vs gridded input file' )
       END IF
    END IF

    

    
    

    IF ( switch .NE. boundary_only ) THEN
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',config_flags%cen_lat
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lat ( grid%id , config_flags%cen_lat )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',config_flags%cen_lon
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lon ( grid%id , config_flags%cen_lon )
    ELSE
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )
    END IF

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT1' ,  config_flags%truelat1 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT1 returns ',config_flags%truelat1
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat1 ( grid%id , config_flags%truelat1 )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT2' ,  config_flags%truelat2 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT2 returns ',config_flags%truelat2
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat2 ( grid%id , config_flags%truelat2 )

    CALL wrf_get_dom_ti_real ( fid , 'MOAD_CEN_LAT' ,  config_flags%moad_cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for MOAD_CEN_LAT returns ',config_flags%moad_cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_moad_cen_lat ( grid%id , config_flags%moad_cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'STAND_LON' ,  config_flags%stand_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for STAND_LON returns ',config_flags%stand_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_stand_lon ( grid%id , config_flags%stand_lon )


    CALL wrf_get_dom_ti_real ( fid , 'POLE_LAT' ,  config_flags%pole_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for POLE_LAT returns ',config_flags%pole_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_pole_lat ( grid%id , config_flags%pole_lat )

    CALL wrf_get_dom_ti_real ( fid , 'POLE_LON' ,  config_flags%pole_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for POLE_LON returns ',config_flags%pole_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_pole_lon ( grid%id , config_flags%pole_lon )









    IF ( program_name(1:7) .EQ. "REAL_EM" ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'P_TOP' ,  grid%p_top , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for P_TOP returns ',grid%p_top
      CALL wrf_debug ( 300 , wrf_err_message )
    ENDIF

    IF ( switch .NE. boundary_only ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GMT returns ',config_flags%gmt
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_gmt ( grid%id , config_flags%gmt )

      CALL wrf_get_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULYR returns ',config_flags%julyr
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julyr ( grid%id , config_flags%julyr )

      CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULDAY returns ',config_flags%julday
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julday ( grid%id , config_flags%julday )
    ENDIF

    CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAP_PROJ returns ',config_flags%map_proj
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_map_proj ( grid%id , config_flags%map_proj )
    grid%map_proj = config_flags%map_proj

    CALL wrf_get_dom_ti_char ( fid , 'MMINLU', mminlu , ierr )
    IF ( ierr .NE. 0 ) THEN
      WRITE(wrf_err_message,*)'MMINLU error on input'
      mminlu = " "
      CALL wrf_debug ( 0 , wrf_err_message )
    ELSE IF ( ( ( mminlu(1:1) .GE. "A" ) .AND. ( mminlu(1:1) .LE. "Z" ) ) .OR. &
              ( ( mminlu(1:1) .GE. "a" ) .AND. ( mminlu(1:1) .LE. "z" ) ) .OR. &
              ( ( mminlu(1:1) .GE. "0" ) .AND. ( mminlu(1:1) .LE. "9" ) ) ) THEN
       
    ELSE IF ( mminlu(1:1) .EQ. " " ) THEN
       mminlu = " "
    ELSE
       mminlu = " "
    END IF
    call wrf_debug( 1 , "mminlu = '" // TRIM(mminlu) // "'")
    if (index(mminlu, char(0)) > 0) mminlu(index(mminlu, char(0)):) = " "
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_char for MMINLU returns ' // TRIM(mminlu)
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_mminlu ( grid%id, mminlu )

    
    
    

    IF ( switch .EQ. input_only ) THEN
      call wrf_get_dom_ti_integer(fid, "NUM_LAND_CAT", num_land_cat_compare, 1, icnt, ierr)
      if ( (ierr .NE. 0) .OR. ( num_land_cat_compare .LT. 1 ) .OR. ( num_land_cat_compare .GT. 1000 ) ) then
        IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
          call wrf_debug( 1 , "Must be old WPS data, assuming 20 levels for NUM_LAND_CAT")
          num_land_cat_compare = 20
        ELSE
          call wrf_debug( 1 , "Must be old WPS data, assuming 24 levels for NUM_LAND_CAT")
          num_land_cat_compare = 24
        END IF
      endif
      if ( config_flags%num_land_cat /= num_land_cat_compare ) then
        call wrf_message("----------------- ERROR -------------------")
        WRITE(wrf_err_message,'("namelist    : NUM_LAND_CAT = ",I10)') config_flags%num_land_cat
        call wrf_message(wrf_err_message)
        WRITE(wrf_err_message,'("input files : NUM_LAND_CAT = ",I10, " (from geogrid selections).")') num_land_cat_compare
        call wrf_message(wrf_err_message)
        call wrf_error_fatal3("<stdin>",711,&
"Mismatch between namelist and wrf input files for dimension NUM_LAND_CAT")
      endif
    ENDIF

    
    
    

    IF ( ( switch .EQ. auxinput1_only ) .AND. &
         ( first_date_nml .EQ. first_date_input ) )  THEN
       CALL wrf_get_dom_ti_integer ( fid, 'NUM_METGRID_SOIL_LEVELS', itmp, 1, icnt, ierr )
   
       IF ( ierr .EQ. 0 ) THEN

          IF ( itmp .EQ. 1 ) THEN
             call wrf_error_fatal3("<stdin>",727,&
"NUM_METGRID_SOIL_LEVELS must be greater than 1")
          END IF
          WRITE(wrf_err_message,*)'input_wrf: global attribute NUM_METGRID_SOIL_LEVELS returns ', itmp
          CALL wrf_debug ( 300 , wrf_err_message )
          IF ( config_flags%num_metgrid_soil_levels /= itmp ) THEN
             call wrf_message("----------------- ERROR -------------------")
             WRITE(wrf_err_message,'("namelist    : num_metgrid_soil_levels = ",I10)') config_flags%num_metgrid_soil_levels
             call wrf_message(wrf_err_message)
             WRITE(wrf_err_message,'("input files : NUM_METGRID_SOIL_LEVELS = ",I10, " (from met_em files).")') itmp
             call wrf_message(wrf_err_message)
             call wrf_error_fatal3("<stdin>",738,&
"Mismatch between namelist and global attribute NUM_METGRID_SOIL_LEVELS")
          END IF
       END IF
    END IF


    
    

    IF ( switch .EQ. input_only  ) THEN
       CALL wrf_get_dom_ti_integer ( fid, 'SF_SURFACE_PHYSICS', itmp, 1, icnt, ierr )
       IF ( ierr .EQ. 0 ) THEN
          WRITE(wrf_err_message,*)'input_wrf: global attribute SF_SURFACE_PHYSICS returns ', itmp
          CALL wrf_debug ( 300 , wrf_err_message )
          IF ( config_flags%sf_surface_physics /= itmp ) THEN
             IF ( ( config_flags%sf_surface_physics == LSMSCHEME ) .and. ( itmp == NOAHMPSCHEME ) ) then
                
             ELSE IF ( ( config_flags%sf_surface_physics == NOAHMPSCHEME ) .and. ( itmp == LSMSCHEME ) ) then
                
             ELSE
                call wrf_message("----------------- ERROR -------------------")
                WRITE(wrf_err_message,'("namelist    : sf_surface_physics = ",I10)') config_flags%sf_surface_physics
                call wrf_message(wrf_err_message)
                WRITE(wrf_err_message,'("input files : SF_SURFACE_PHYSICS = ",I10, " (from wrfinput files).")') itmp
                call wrf_message(wrf_err_message)
                call wrf_error_fatal3("<stdin>",764,&
"Mismatch between namelist and global attribute SF_SURFACE_PHYSICS")
             END IF
          END IF
       END IF
    END IF


    CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISWATER returns ',config_flags%iswater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%iswater = 14
         ELSE IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
              config_flags%iswater = 17
         ELSE
              config_flags%iswater = 16
         ENDIF
    ENDIF
    CALL nl_set_iswater ( grid%id , config_flags%iswater )
    grid%iswater = config_flags%iswater

    CALL wrf_get_dom_ti_integer ( fid , 'ISLAKE' ,  config_flags%islake , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISLAKE returns ',config_flags%islake
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%islake = -1
    ENDIF
    CALL nl_set_islake ( grid%id , config_flags%islake )
    grid%islake = config_flags%islake

    CALL wrf_get_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISICE returns ',config_flags%isice
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE.  0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isice = 14
         ELSE IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
              config_flags%isice = 15
         ELSE
              config_flags%isice = 24
         ENDIF
    ENDIF
    CALL nl_set_isice ( grid%id , config_flags%isice )
    grid%isice = config_flags%isice

    CALL wrf_get_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISURBAN returns ',config_flags%isurban
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isurban = 13
         ELSE IF (mminlu == 'MODIFIED_IGBP_MODIS_NOAH') THEN
              config_flags%isurban = 13
         ELSE
              config_flags%isurban = 1
         ENDIF
    ENDIF
    CALL nl_set_isurban ( grid%id , config_flags%isurban )
    grid%isurban = config_flags%isurban

    CALL wrf_get_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISOILWATER returns ',config_flags%isoilwater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%isoilwater = 14
    ENDIF
    CALL nl_set_isoilwater ( grid%id , config_flags%isoilwater )
    grid%isoilwater = config_flags%isoilwater




    IF      ( ( switch .EQ.     input_only  ) .OR. &
            ( ( switch .EQ. auxinput1_only ) .AND. &
              ( config_flags%auxinput1_inname(1:8) .EQ. 'wrf_real' ) ) ) THEN

       

       IF ( ( ide .NE. ide_compare    ) .OR. &
            ( kde .NE. kde_compare    ) .OR. &
            ( jde .NE. jde_compare    ) ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist ide,jde,kde=',ide,jde,kde,&
                                  '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
          CALL wrf_error_fatal3("<stdin>",849,&
wrf_err_message )
       ENDIF

    ELSE IF ( switch .EQ. auxinput1_only ) THEN

       

       IF ( ( ide                             .NE. ide_compare ) .OR. &
            ( config_flags%num_metgrid_levels .NE. kde_compare ) .OR. &
            ( jde                             .NE. jde_compare ) ) THEN
         WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  ',&
                                 'namelist ide,jde,num_metgrid_levels=',ide,jde,config_flags%num_metgrid_levels,&
                                 '; input data ide,jde,num_metgrid_levels=',ide_compare , jde_compare , kde_compare
         CALL wrf_error_fatal3("<stdin>",863,&
wrf_err_message )
       ENDIF
    ENDIF




    ENDIF check_if_dryrun










    3003 continue

    CALL wrf_get_next_time(fid, current_date , ierr)
    WRITE(wrf_err_message,*)fid,' input_wrf: wrf_get_next_time current_date: ',current_date(1:19),' Status = ',ierr
    CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
    IF ( ierr .NE. 0 .AND. ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ ) THEN
      CALL wrf_message ( TRIM(wrf_err_message ) )
      IF ( switch .EQ. boundary_only ) THEN
        WRITE(wrf_err_message,*) ' ... May have run out of valid boundary conditions in file ',TRIM(fname)
        CALL wrf_error_fatal3("<stdin>",891,&
TRIM(wrf_err_message) )
      ELSE
        WRITE(wrf_err_message,*) '... Could not find matching time in input file ',TRIM(fname)
        CALL wrf_error_fatal3("<stdin>",895,&
TRIM(wrf_err_message) )
      ENDIF
    ELSE IF ( ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ) THEN




      SELECT CASE ( switch )
        CASE ( input_only, auxinput1_only, auxinput2_only,       &
               auxinput3_only, auxinput4_only, auxinput5_only,  &
               auxinput6_only, auxinput7_only, auxinput8_only,  &
               auxinput9_only, auxinput10_only )
            CALL wrf_atotime( current_date(1:19), time )
            CALL domain_clock_get( grid, current_time=currtime, &
                                         current_timestr=currtimestr )


            CALL domain_clockprint(150, grid, &
                   'DEBUG input_wrf():  get CurrTime from clock,')
            IF ( time .NE. currtime ) THEN
                WRITE( wrf_err_message , * )'Time in file: ',trim( current_date(1:19) )
                CALL wrf_message ( trim(wrf_err_message) )
                WRITE( wrf_err_message , * )'Time on domain: ',trim( currtimestr )
                CALL wrf_message ( trim(wrf_err_message) )
                CALL wrf_message( "**WARNING** Time in input file not equal to time on domain **WARNING**" )
                WRITE(wrf_err_message,*) "**WARNING** Trying next time in file ",TRIM(fname)," ..."
                CALL wrf_message( TRIM(wrf_err_message) )
                GOTO 3003
            ENDIF
        CASE DEFAULT
      END SELECT
    ENDIF






    IF ( switch .EQ. boundary_only ) THEN
        CALL domain_clock_get( grid, current_time=currentTime )
        CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
        CALL wrf_atotime( this_datestr(1:19), grid%this_bdy_time )
        CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
        CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
        IF( currentTime .GE. grid%next_bdy_time ) THEN
          IF ( wrf_dm_on_monitor() ) THEN
             write(a_message,*) 'THIS TIME ',this_datestr(1:19),'NEXT TIME ',next_datestr(1:19)
             CALL wrf_message ( a_message ) 
          END IF
          RETURN
        ENDIF
    ENDIF

    

    n_ref_m = config_flags%vert_refine_fact






    IF ( (first_input   .LE. switch .AND. switch .LE. last_input) .OR. &
         (first_history .LE. switch .AND. switch .LE. last_history) .OR. &
         switch .EQ. restart_only    ) THEN
      newswitch = switch
      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) ) 
        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN   
          IF ( p%Ndim .EQ. 0 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                dname = p%DataName
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%rfield_0d             , & 
                                    WRF_FLOAT               , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    '0'                     , & 
                                    ''                      , & 
                     "input_wrf.F" // ' reading 0d real ' // TRIM(p%VarName)     , & 
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_0d             , & 
                                    WRF_DOUBLE              , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    '0'                     , & 
                                    ''                      , & 
                     "input_wrf.F" // ' reading 0d double ' // TRIM(p%VarName)     , & 
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_0d             , & 
                                    WRF_INTEGER             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    '0'                     , & 
                                    ''                      , & 
                     "input_wrf.F" // ' reading 0d integer ' // TRIM(p%VarName)     , & 
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%lfield_0d             , & 
                                    WRF_LOGICAL             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    '0'                     , & 
                                    ''                      , & 
                     "input_wrf.F" // ' reading 0d logical ' // TRIM(p%VarName)     , & 
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     1 , 1 , 1 , 1 , 1 , 1 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 1 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder

                  i_inter = 0

                  

                  if( TRIM(p%dimname1).EQ.'bottom_top'.OR.TRIM(p%dimname1).EQ.'bottom_top_stag') i_inter = 1 

                  
                  IF      ( p%Type .EQ. 'r' ) THEN
                    IF( (i_inter.eq.1.and.n_ref_m.ge.2).and.(switch.eq.history_only) ) THEN
                       em1_c = (p%em1 - 1)/n_ref_m +1
                       ed1_c = em1_c
                       ep1_c = em1_c
                       if (TRIM(p%dimname1).EQ.'bottom_top') then
                       ed1_c = em1_c-1
                       ep1_c = em1_c-1
                       endif
                       allocate (f_vint_1d(em1_c))

                       CALL wrf_ext_read_field (  &
                                       fid                     , & 
                                       current_date(1:19)      , & 
                                       TRIM(dname)             , & 
                                       f_vint_1d                , & 
                                       WRF_FLOAT               , & 
                                       grid%communicator       , & 
                                       grid%iocommunicator     , & 
                                       grid%domdesc            , & 
                                       grid%bdy_mask           , & 
                                       TRIM(memord)            , & 
                                       p%Stagger               , & 
                        "input_wrf.F" // ' reading 1d real ' // TRIM(p%VarName)     , & 
                        p%sd1 , ed1_c , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                        p%sm1 , em1_c , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                        p%sp1 , ep1_c , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                        ierr )

                        do k=1,ed1_c
                        p%rfield_1d(k) = f_vint_1d(k)
                        enddo
                        deallocate (f_vint_1d)

                     ELSE
                       CALL wrf_ext_read_field (  &
                                       fid                     , & 
                                       current_date(1:19)      , & 
                                       TRIM(dname)             , & 
                                       p%rfield_1d             , & 
                                       WRF_FLOAT               , & 
                                       grid%communicator       , & 
                                       grid%iocommunicator     , & 
                                       grid%domdesc            , & 
                                       grid%bdy_mask           , & 
                                       TRIM(memord)            , & 
                                       p%Stagger               , & 
                        "input_wrf.F" // ' reading 1d real ' // TRIM(p%VarName)     , & 
                        p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                        ierr )
                     END IF
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_1d             , & 
                                    WRF_DOUBLE              , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 1d double ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_1d             , & 
                                    WRF_INTEGER             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 1d integer ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%lfield_1d             , & 
                                    WRF_LOGICAL             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 1d logical ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 2 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%rfield_2d             , & 
                                    WRF_FLOAT               , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 2d real ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_2d             , & 
                                    WRF_DOUBLE              , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 2d double ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_2d             , & 
                                    WRF_INTEGER             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 2d integer ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%lfield_2d             , & 
                                    WRF_LOGICAL             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 2d logical ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder

                  i_inter = 0

                  

                  if( TRIM(p%dimname2).EQ.'bottom_top'.OR.TRIM(p%dimname2).EQ.'bottom_top_stag') i_inter = 1


                  IF      ( p%Type .EQ. 'r' ) THEN
                    IF( (i_inter.eq.1.and.n_ref_m.ge.2).and.(switch.eq.history_only) ) then
                       em2_c = (p%em2 - 1)/n_ref_m +1
                       ed2_c = em2_c
                       ep2_c = em2_c
                       if (TRIM(p%dimname2).EQ.'bottom_top') then
                       ed2_c = em2_c-1
                       ep2_c = em2_c-1
                       endif
                       allocate (f_vint_3d(p%sm1:p%em1,em2_c,p%sm3:p%em3))
                       CALL wrf_ext_read_field (  &
                                       fid                     , & 
                                       current_date(1:19)      , & 
                                       TRIM(dname)             , & 
                                       f_vint_3d               , & 
                                       WRF_FLOAT               , & 
                                       grid%communicator       , & 
                                       grid%iocommunicator     , & 
                                       grid%domdesc            , & 
                                       grid%bdy_mask           , & 
                                       TRIM(memord)            , & 
                                       TRIM(p%Stagger)         , & 
                        "input_wrf.F" // ' reading 3d real ' // TRIM(p%VarName)     , & 
                        p%sd1 , p%ed1 , p%sd2 , ed2_c , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , em2_c , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , ep2_c , p%sp3 , p%ep3 ,  &
                        ierr )

                        do j = p%sm3,p%em3
                        do k = 1,ed2_c
                        do i = p%sm1,p%em1
                        p%rfield_3d(i,k,j) = f_vint_3d(i,k,j)
                        enddo
                        enddo
                        enddo
                        deallocate (f_vint_3d)
                    ELSE
                       CALL wrf_ext_read_field (  &
                                       fid                     , & 
                                       current_date(1:19)      , & 
                                       TRIM(dname)             , & 
                                       p%rfield_3d             , & 
                                       WRF_FLOAT               , & 
                                       grid%communicator       , & 
                                       grid%iocommunicator     , & 
                                       grid%domdesc            , & 
                                       grid%bdy_mask           , & 
                                       TRIM(memord)            , & 
                                       TRIM(p%Stagger)         , & 
                        "input_wrf.F" // ' reading 3d real ' // TRIM(p%VarName)     , & 
                        p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                        ierr )
                    ENDIF
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_3d             , & 
                                    WRF_DOUBLE              , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 3d double ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_read_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_3d             , & 
                                    WRF_INTEGER             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                     "input_wrf.F" // ' reading 3d integer ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )

                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN




            DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
              IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream( p%streams_table(grid%id,itrace)%stream,newswitch)) THEN
                dname = p%dname_table( grid%id, itrace )
                IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                memord = p%MemoryOrder

                i_inter = 0

                

                if( TRIM(p%dimname2).EQ.'bottom_top'.OR.TRIM(p%dimname2).EQ.'bottom_top_stag') i_inter = 1


                IF      ( p%Type .EQ. 'r' ) THEN
                    IF( (i_inter.eq.1.and.n_ref_m.ge.2).and.(switch.eq.history_only) ) then
                       em2_c = (p%em2 - 1)/n_ref_m +1
                       ed2_c = em2_c
                       ep2_c = em2_c
                       if (TRIM(p%dimname2).EQ.'bottom_top') then
                       ed2_c = em2_c-1
                       ep2_c = em2_c-1
                       endif
                       allocate (f_vint_4d(p%sm1:p%em1,em2_c,p%sm3:p%em3,p%num_table(grid%id)))

                       CALL wrf_ext_read_field_arr (  &
                                         fid                     , & 
                                         current_date(1:19)      , & 
                                         TRIM(dname)             , & 
                                         f_vint_4d               , & 
                                         itrace, 1, 1, 1         , & 
                                         1, 1, 1                 , & 
                                         4               , &
                                         WRF_FLOAT               , & 
                                         grid%communicator       , & 
                                         grid%iocommunicator     , & 
                                         grid%domdesc            , & 
                                         grid%bdy_mask           , & 
                                         TRIM(memord)            , & 
                                         TRIM(p%Stagger)         , & 
                        "input_wrf.F" // ' reading 4d real ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                        p%sd1 , p%ed1 , p%sd2 , ed2_c , p%sd3 , p%ed3 ,  &
                        p%sm1 , p%em1 , p%sm2 , em2_c , p%sm3 , p%em3 ,  &
                        p%sp1 , p%ep1 , p%sp2 , ep2_c , p%sp3 , p%ep3 ,  &
                        ierr )
                        do j = p%sm3,p%em3
                        do k = 1,ed2_c
                        do i = p%sm1,p%em1
                        p%rfield_4d(i,k,j,itrace) = f_vint_4d(i,k,j,itrace)
                        enddo
                        enddo
                        enddo
                        deallocate (f_vint_4d)
                   ELSE
                        CALL wrf_ext_read_field_arr (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%rfield_4d             , & 
                                    itrace, 1, 1, 1         , & 
                                    1, 1, 1                 , & 
                                    4               , &
                                    WRF_FLOAT               , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                       "input_wrf.F" // ' reading 4d real ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                       p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                       p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                       p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                       ierr )
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                  CALL wrf_ext_read_field_arr (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_4d             , & 
                                    itrace, 1, 1, 1         , & 
                                    1, 1, 1                 , & 
                                    8               , &
                                    WRF_DOUBLE              , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                   "input_wrf.F" // ' reading 4d double ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                   p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                   p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                   p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                   ierr )
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                  CALL wrf_ext_read_field_arr (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_4d             , & 
                                    itrace, 1, 1, 1         , & 
                                    1, 1, 1                 , & 
                                    4               , &
                                    WRF_INTEGER             , & 
                                    grid%communicator       , & 
                                    grid%iocommunicator     , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                   "input_wrf.F" // ' reading 4d integer ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                   p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                   p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                   p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                   ierr )
                ENDIF
              ENDIF
            ENDDO  
          ENDIF
        ENDIF
        p => p%next
      ENDDO
    ELSE
      IF ( switch .EQ. boundary_only ) THEN
        CALL wrf_bdyin( fid , grid , config_flags , switch , ierr )
      ENDIF
    ENDIF


    CALL wrf_tsin( grid , ierr )

    if (config_flags%track_loc_in > 0 ) then
       call track_input( grid , ierr )
    end if



    WRITE(wrf_err_message,*)'input_wrf: end, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    RETURN
  END SUBROUTINE input_wrf
