


























































PROGRAM nup_em

   USE module_machine
   USE module_domain, ONLY : domain, wrfu_timeinterval, alloc_and_configure_domain, &
      domain_clock_set, domain_get_current_time, domain_get_stop_time, head_grid, &
      domain_clock_get, domain_clockadvance
   USE module_domain_type, ONLY : program_name
   USE module_streams
   USE module_initialize_real, only : wrfu_initialize
   USE module_integrate
   USE module_driver_constants
   USE module_configure, only : grid_config_rec_type, model_config_rec
   USE module_io_domain
   USE module_utility

   USE module_timing
   USE module_wrf_error

   USE module_dm





   USE module_bc
   USE module_big_step_utilities_em
   USE module_get_file_names








   IMPLICIT NONE
 
   INTERFACE
     
     SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags)
       USE module_domain
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_read_wrf_chem_bioemiss
     SUBROUTINE nup ( parent_grid , nested_grid, in_id, out_id, newly_opened )
       USE module_domain
       TYPE (domain), POINTER :: parent_grid, nested_grid
       INTEGER, INTENT(IN) :: in_id, out_id    
       LOGICAL, INTENT(IN) :: newly_opened     
     END SUBROUTINE nup

   END INTERFACE

   TYPE(WRFU_TimeInterval) :: RingInterval



   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: its , ite , jts , jte , kts , kte
   INTEGER :: ijds , ijde , spec_bdy_width
   INTEGER :: i , j , k
   INTEGER :: time_loop_max , time_loop
   INTEGER :: total_time_sec , file_counter
   INTEGER :: julyr , julday , iswater , map_proj
   INTEGER :: icnt

   REAL    :: dt , new_bdy_frq
   REAL    :: gmt , cen_lat , cen_lon , dx , dy , truelat1 , truelat2 , moad_cen_lat , stand_lon

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp2

   CHARACTER(LEN=19) :: start_timestr , current_timestr , end_timestr, timestr
   CHARACTER(LEN=19) :: stopTimeStr



   INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

   REAL    :: time
   INTEGER :: rc

   INTEGER :: loop , levels_to_process
   INTEGER , PARAMETER :: max_sanity_file_loop = 100

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain, parent_grid , nested_grid
   TYPE (domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , fido, fidb , idum1 , idum2 , ierr
   INTEGER :: status_next_var
   INTEGER :: debug_level
   LOGICAL :: newly_opened
   CHARACTER (LEN=19) :: date_string


   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* 65536
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor


   INTEGER                 :: idsi, in_id, out_id
   INTEGER                 :: e_sn, e_we, pgr
   CHARACTER (LEN=80)      :: inpname , outname , bdyname
   CHARACTER (LEN=80)      :: si_inpname
   CHARACTER *19 :: temp19
   CHARACTER *24 :: temp24 , temp24b
   CHARACTER *132 :: fname
   CHARACTER(len=24) :: start_date_hold

   CHARACTER (LEN=80)      :: message
integer :: ii

   CHARACTER (LEN=10) :: release_version = 'V3.6      '

   
   

   INTERFACE

      SUBROUTINE med_feedback_domain ( parent_grid , nested_grid )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: parent_grid , nested_grid
      END SUBROUTINE med_feedback_domain

      SUBROUTINE Setup_Timekeeping( parent_grid )
         USE module_domain
         TYPE(domain), POINTER :: parent_grid
      END SUBROUTINE Setup_Timekeeping

   END INTERFACE

   

   program_name = "NUP_EM " // TRIM(release_version) // " PREPROCESSOR"


   CALL disable_quilting


   
   
   
   

   CALL init_modules(1)   



   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )

   CALL init_modules(2)   

   
   
   
   


   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize




   

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   
   
   CALL nl_set_spec_zone( 1, 0 )

   
   

   NULLIFY( null_domain )



   CALL       nl_set_shw( 1, 0 )
   CALL       nl_set_shw( 2, 0 )
   CALL       nl_set_i_parent_start( 2, 1 )
   CALL       nl_set_j_parent_start( 2, 1 )
   CALL       nl_get_e_we( 2, e_we )
   CALL       nl_get_e_sn( 2, e_sn )
   CALL       nl_get_parent_grid_ratio( 2, pgr )

   
   

   e_we = e_we / pgr + 2
   e_sn = e_sn / pgr + 2 
   CALL       nl_set_e_we( 1, e_we )
   CALL       nl_set_e_sn( 1, e_sn )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'nup_em: calling alloc_and_configure_domain coarse ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   parent_grid => head_grid

   

   CALL Setup_Timekeeping ( parent_grid )

   CALL domain_clock_set( head_grid, &
                          time_step_seconds=model_config_rec%interval_seconds )

   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( parent_grid%id , idum1, idum2 )



   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain fine ' )
   CALL alloc_and_configure_domain ( domain_id  = 2 ,                  &
                                     grid       = nested_grid ,        &
                                     parent     = parent_grid ,        &
                                     kid        = 1                   )



   parent_grid%ed31 = parent_grid%ed31 - 2
   parent_grid%ed33 = parent_grid%ed33 - 2
   CALL       nl_set_e_we( 1, e_we-2 )
   CALL       nl_set_e_sn( 1, e_sn-2 )



   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( nested_grid%id , idum1, idum2 )

   

   CALL Setup_Timekeeping ( nested_grid )
   

   CALL WRFU_AlarmGet( nested_grid%alarms(HISTORY_ALARM), RingInterval=RingInterval )
   CALL WRFU_ClockSet( nested_grid%domain_clock, TimeStep=RingInterval, rc=rc )
   CALL WRFU_ClockSet( parent_grid%domain_clock, TimeStep=RingInterval, rc=rc )
   
   


   

   CALL init_wrfio

   
   


   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )


   
   in_id = 0
   out_id = 0
   main_loop : DO WHILE ( domain_get_current_time(nested_grid) .LT. domain_get_stop_time(nested_grid) )

      IF( WRFU_AlarmIsRinging( nested_grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
        CALL domain_clock_get( nested_grid, current_timestr=timestr )
        newly_opened = .FALSE.
        IF ( in_id.EQ. 0 ) THEN
          CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
          CALL construct_filename2a ( fname , config_flags%history_outname , nested_grid%id , 2 , timestr )
          CALL open_r_dataset ( in_id, TRIM(fname), nested_grid ,  &
                                 config_flags , 'DATASET=HISTORY' , ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE(message,*)'Failed to open ',TRIM(fname),' for reading. '
            CALL wrf_message(message)
            EXIT main_loop
          ENDIF

          CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
          CALL construct_filename2a ( fname , config_flags%history_outname , parent_grid%id , 2 , timestr )
          CALL open_w_dataset ( out_id, TRIM(fname), parent_grid ,  &
                                 config_flags , output_history, 'DATASET=HISTORY' , ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE(message,*)'Failed to open ',TRIM(fname),' for writing. '
            CALL wrf_message(message)
            EXIT main_loop
          ENDIF
          newly_opened = .TRUE.
        ENDIF

        CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
        CALL input_history ( in_id, nested_grid , config_flags , ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE(message,*)'Unable to read time ',timestr
          CALL wrf_message(message)
          EXIT main_loop
        ENDIF

        CALL nup ( nested_grid , parent_grid, in_id, out_id, newly_opened  )

        CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
        CALL output_history ( out_id, parent_grid , config_flags , ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE(message,*)'Unable to write time ',timestr
          CALL wrf_message(message)
          EXIT main_loop
        ENDIF

        nested_grid%nframes(history_only) = nested_grid%nframes(history_only) + 1
        IF ( nested_grid%nframes(history_only) >= config_flags%frames_per_outfile ) THEN
          CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
          CALL close_dataset ( in_id , config_flags , "DATASET=HISTORY" )
          CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
          CALL close_dataset ( out_id , config_flags , "DATASET=HISTORY" )
          in_id = 0
          out_id = 0
          nested_grid%nframes(history_only) = 0
        ENDIF
        CALL WRFU_AlarmRingerOff( nested_grid%alarms( HISTORY_ALARM ), rc=rc )
      ENDIF
      CALL domain_clockadvance( nested_grid )
      CALL domain_clockadvance( parent_grid )
   ENDDO main_loop
   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL med_shutdown_io ( parent_grid , config_flags )

   CALL wrf_debug ( 0 , 'nup_em: SUCCESS COMPLETE NUP_EM INIT' )



   CALL WRFU_Finalize( rc=rc )

END PROGRAM nup_em

SUBROUTINE nup ( nested_grid, parent_grid , in_id, out_id, newly_opened ) 
  USE module_domain
  USE module_io_domain
  USE module_utility
  USE module_timing
  USE module_wrf_error

  IMPLICIT NONE


  TYPE(domain), POINTER :: parent_grid, nested_grid
  INTEGER, INTENT(IN) :: in_id, out_id    
  LOGICAL, INTENT(IN) :: newly_opened     

  INTEGER :: julyr , julday , iswater , map_proj
  INTEGER :: icnt, ierr
  REAL    :: dt , new_bdy_frq
  REAL    :: gmt , cen_lat , cen_lon , dx , dy , truelat1 , truelat2 , moad_cen_lat , stand_lon
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp1
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
  REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp2
  INTEGER :: ids , ide , jds , jde , kds , kde
  INTEGER :: ims , ime , jms , jme , kms , kme
  INTEGER :: ips , ipe , jps , jpe , kps , kpe
  INTEGER :: its , ite , jts , jte , kts , kte

  INTERFACE
     SUBROUTINE med_feedback_domain ( parent_grid , nested_grid )
        USE module_domain
        USE module_configure
        TYPE(domain), POINTER :: parent_grid , nested_grid
     END SUBROUTINE med_feedback_domain
     SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
        USE module_domain
        USE module_configure
        TYPE(domain), POINTER :: parent_grid , nested_grid
     END SUBROUTINE med_interp_domain
  END INTERFACE

  IF ( newly_opened ) THEN
    CALL wrf_get_dom_ti_integer ( in_id , 'MAP_PROJ' , map_proj , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'DX'  , dx  , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'DY'  , dy  , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'CEN_LAT' , cen_lat , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'CEN_LON' , cen_lon , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'TRUELAT1' , truelat1 , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'TRUELAT2' , truelat2 , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , icnt , ierr ) 
    CALL wrf_get_dom_ti_real    ( in_id , 'STAND_LON' , stand_lon , 1 , icnt , ierr ) 



    CALL wrf_get_dom_ti_integer ( in_id , 'ISWATER' , iswater , 1 , icnt , ierr ) 
  ENDIF

  parent_grid%fnm    = nested_grid%fnm
  parent_grid%fnp    = nested_grid%fnp
  parent_grid%rdnw   = nested_grid%rdnw
  parent_grid%rdn    = nested_grid%rdn
  parent_grid%dnw    = nested_grid%dnw
  parent_grid%dn     = nested_grid%dn 
  parent_grid%znu    = nested_grid%znu
  parent_grid%znw    = nested_grid%znw

  parent_grid%zs        = nested_grid%zs
  parent_grid%dzs       = nested_grid%dzs

  parent_grid%p_top     = nested_grid%p_top
  parent_grid%rdx       = nested_grid%rdx * 3.
  parent_grid%rdy       = nested_grid%rdy * 3.
  parent_grid%resm      = nested_grid%resm
  parent_grid%zetatop   = nested_grid%zetatop
  parent_grid%cf1       = nested_grid%cf1
  parent_grid%cf2       = nested_grid%cf2
  parent_grid%cf3       = nested_grid%cf3

  parent_grid%cfn       = nested_grid%cfn 
  parent_grid%cfn1      = nested_grid%cfn1








  

  ids = parent_grid%sd31
  ide = parent_grid%ed31
  kds = parent_grid%sd32
  kde = parent_grid%ed32
  jds = parent_grid%sd33
  jde = parent_grid%ed33

  ims = parent_grid%sm31
  ime = parent_grid%em31
  kms = parent_grid%sm32
  kme = parent_grid%em32
  jms = parent_grid%sm33
  jme = parent_grid%em33

  ips = parent_grid%sp31
  ipe = parent_grid%ep31
  kps = parent_grid%sp32
  kpe = parent_grid%ep32
  jps = parent_grid%sp33
  jpe = parent_grid%ep33

  nested_grid%imask_nostag = 1
  nested_grid%imask_xstag = 1
  nested_grid%imask_ystag = 1
  nested_grid%imask_xystag = 1


  CALL med_feedback_domain ( parent_grid , nested_grid )

  parent_grid%ht_int = parent_grid%ht




         
   
  IF ( newly_opened ) THEN
    CALL wrf_put_dom_ti_integer ( out_id , 'MAP_PROJ' , map_proj , 1 , ierr ) 


    CALL wrf_put_dom_ti_real    ( out_id , 'CEN_LAT' , cen_lat , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'CEN_LON' , cen_lon , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'TRUELAT1' , truelat1 , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'TRUELAT2' , truelat2 , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , ierr ) 
    CALL wrf_put_dom_ti_real    ( out_id , 'STAND_LON' , stand_lon , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( out_id , 'ISWATER' , iswater , 1 , ierr ) 

    CALL wrf_put_dom_ti_real    ( out_id , 'GMT' , gmt , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( out_id , 'JULYR' , julyr , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( out_id , 'JULDAY' , julday , 1 , ierr ) 
  ENDIF

END SUBROUTINE nup

SUBROUTINE land_percentages ( xland , &
                              landuse_frac , soil_top_cat , soil_bot_cat , &
                              isltyp , ivgtyp , &
                              num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater )
   USE module_soil_pre

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater

   INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
   REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
   REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
   REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT) :: xland

   CALL process_percent_cat_new ( xland , &
                                  landuse_frac , soil_top_cat , soil_bot_cat , &
                                  isltyp , ivgtyp , &
                                  num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

END SUBROUTINE land_percentages

SUBROUTINE check_consistency ( ivgtyp , isltyp , landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isltyp , ivgtyp
   REAL    , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: landmask

   LOGICAL :: oops
   INTEGER :: oops_count , i , j

   oops = .FALSE.
   oops_count = 0

   DO j = jts, MIN(jde-1,jte)
      DO i = its, MIN(ide-1,ite)
         IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater ) ) .OR. &
              ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater ) ) ) THEN
            print *,'mismatch in landmask and veg type'
            print *,'i,j=',i,j, '  landmask =',NINT(landmask(i,j)),'  ivgtyp=',ivgtyp(i,j)
            oops = .TRUE.
            oops_count = oops_count + 1
landmask(i,j) = 0
ivgtyp(i,j)=16
isltyp(i,j)=14
         END IF
      END DO
   END DO

   IF ( oops ) THEN
      CALL wrf_debug( 0, 'mismatch in check_consistency, turned to water points, be careful' )
   END IF

END SUBROUTINE check_consistency

SUBROUTINE check_consistency2( ivgtyp , isltyp , landmask , &
                               tmn , tsk , sst , xland , &
                               tslb , smois , sh2o , &
                               num_soil_layers , id , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte , &
                               iswater )

   USE module_configure
   USE module_optional_input

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte 
   INTEGER , INTENT(IN) :: num_soil_layers , id

   INTEGER , DIMENSION(ims:ime,jms:jme) :: ivgtyp , isltyp
   REAL    , DIMENSION(ims:ime,jms:jme) :: landmask , tmn , tsk , sst , xland
   REAL    , DIMENSION(ims:ime,num_soil_layers,jms:jme) :: tslb , smois , sh2o

   INTEGER :: oops1 , oops2
   INTEGER :: i , j , k

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(id) )

         CASE ( SLABSCHEME , LSMSCHEME , RUCLSMSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                     tmn(i,j) = sst(i,j)
                     tsk(i,j) = sst(i,j)
                  ELSE IF ( landmask(i,j) .LT. 0.5 ) THEN
                     tmn(i,j) = tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( tsk(i,j) .LT. 170 .or. tsk(i,j) .GT. 400. ) THEN
               print *,'error in the TSK'
               print *,'i,j=',i,j
               print *,'landmask=',landmask(i,j)
               print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                  tsk(i,j)=tmn(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tsk(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal3("<stdin>",684,&
'TSK unreasonable' )
               end if
            END IF
         END DO
      END DO

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tmn(i,j) .LT. 170. ) .OR. ( tmn(i,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TMN'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                  tmn(i,j)=tsk(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tmn(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal3("<stdin>",705,&
'TMN unreasonable' )
               endif
            END IF
         END DO
      END DO

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tslb(i,1,j) .LT. 170. ) .OR. ( tslb(i,1,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TSLB'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
                  print *,'tslb = ',tslb(i,:,j)
                  print *,'old smois = ',smois(i,:,j)
                  DO l = 1 , num_soil_layers
                     sh2o(i,l,j) = 0.0
                  END DO
                  DO l = 1 , num_soil_layers
                     smois(i,l,j) = 0.3
                  END DO
                  if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tsk(i,j)
                     END DO
                  else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=sst(i,j)
                     END DO
                  else if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tmn(i,j)
                     END DO
                  else
                     CALL wrf_error_fatal3("<stdin>",742,&
'TSLB unreasonable' )
                  endif
            END IF
         END DO
      END DO

      

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater .OR. isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater .OR. isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  ivgtyp(i,j) = 5
                  isltyp(i,j) = 8
                  landmask(i,j) = 1
                  xland(i,j) = 1
               ELSE IF ( sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  ivgtyp(i,j) = iswater
                  isltyp(i,j) = 14
                  landmask(i,j) = 0
                  xland(i,j) = 2
               ELSE
                  print *,'the landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'ivgtyp=',ivgtyp(i,j)
                  print *,'isltyp=',isltyp(i,j)
                  print *,'iswater=', iswater
                  print *,'tslb=',tslb(i,:,j)
                  print *,'sst=',sst(i,j)
                  CALL wrf_error_fatal3("<stdin>",778,&
'mismatch_landmask_ivgtyp' )
               END IF
            END IF
         END DO
      END DO
if (oops1.gt.0) then
print *,'points artificially set to land : ',oops1
endif
if(oops2.gt.0) then
print *,'points artificially set to water: ',oops2
endif

END SUBROUTINE check_consistency2
