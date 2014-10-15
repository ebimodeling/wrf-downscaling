




MODULE module_wrf_top





   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_check_a_mundo

   USE module_timing
   USE module_wrf_error
   USE module_nesting


   USE module_dm, ONLY : wrf_dm_initialize


   USE module_cpl, ONLY : coupler_on, cpl_finalize, cpl_defdomain

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (domain) , pointer :: parent_grid, new_nest
   LOGICAL                                :: a_nest_was_opened
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER        :: kid, nestid
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   LOGICAL :: input_from_file


   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* 65536
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor


   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message
   CHARACTER (LEN=256) , PRIVATE :: a_message

   INTERFACE 
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping


     SUBROUTINE wrf_dfi_write_initialized_state( )
     END SUBROUTINE wrf_dfi_write_initialized_state
 
     SUBROUTINE wrf_dfi_startfwd_init( )
     END SUBROUTINE wrf_dfi_startfwd_init
     
     SUBROUTINE wrf_dfi_startbck_init( )
     END SUBROUTINE wrf_dfi_startbck_init
     
     SUBROUTINE wrf_dfi_bck_init( )
     END SUBROUTINE wrf_dfi_bck_init
     
     SUBROUTINE wrf_dfi_fwd_init( )
     END SUBROUTINE wrf_dfi_fwd_init
     
     SUBROUTINE wrf_dfi_fst_init( )
     END SUBROUTINE wrf_dfi_fst_init
     
     SUBROUTINE wrf_dfi_array_reset ( )
     END SUBROUTINE wrf_dfi_array_reset


     SUBROUTINE med_nest_initial ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_initial

   END INTERFACE


CONTAINS


   SUBROUTINE wrf_init( no_init1 )









     LOGICAL, OPTIONAL, INTENT(IN) :: no_init1
     INTEGER i, myproc, nproc, hostid, loccomm, ierr, buddcounter, mydevice
     INTEGER, ALLOCATABLE :: hostids(:), budds(:)
     CHARACTER*512 hostname
   CHARACTER (LEN=10) :: release_version = 'V3.6      '





















   program_name = "WRF " // TRIM(release_version) // " MODEL"

   
   
   CALL init_modules(1)
   IF ( .NOT. PRESENT( no_init1 ) ) THEN
     



     CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN )

   ENDIF
   
   CALL init_modules(2)
















   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize




   CALL set_derived_rconfigs
   CALL check_nml_consistency
   CALL set_physics_rconfigs















   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   

   NULLIFY( null_domain )










   CALL nl_get_max_dom( 1, max_dom )
   IF ( max_dom > 1 ) THEN
   END IF

















   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )


   
   
   
   IF ( head_grid%dfi_opt .NE. DFI_NODFI ) head_grid%dfi_stage = DFI_SETUP


   CALL Setup_Timekeeping (head_grid)






















   CALL med_initialdata_input( head_grid , config_flags )

   IF ( config_flags%write_restart_at_0h ) THEN
      CALL med_restart_out ( head_grid, config_flags )

      CALL wrf_debug ( 0 , ' 0 h restart only wrf: SUCCESS COMPLETE WRF' )




      CALL wrf_finalize( )
   END IF

   
   head_grid%start_subtime = domain_get_start_time ( head_grid )
   head_grid%stop_subtime = domain_get_stop_time ( head_grid )

   
   
   
   
   

   IF ( head_grid%dfi_opt .NE. DFI_NODFI ) THEN
      CALL alloc_doms_for_dfi ( head_grid )
   END IF

   IF (coupler_on) CALL cpl_defdomain( head_grid ) 

   END SUBROUTINE wrf_init



   SUBROUTINE wrf_run( )














   
   

   CALL       wrf_debug ( 100 , 'wrf: calling integrate' )
   CALL integrate ( head_grid )
   CALL       wrf_debug ( 100 , 'wrf: back from integrate' )

   END SUBROUTINE wrf_run



   SUBROUTINE wrf_finalize( no_shutdown )















     LOGICAL, OPTIONAL, INTENT(IN) :: no_shutdown

   
   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )

   
   
   IF ( .NOT. PRESENT( no_shutdown ) ) THEN
     
      IF (coupler_on) THEN 
         CALL cpl_finalize() 
      ELSE
         CALL WRFU_Finalize
         CALL wrf_shutdown
      ENDIF
   ENDIF

   END SUBROUTINE wrf_finalize


   SUBROUTINE wrf_dfi()



      IMPLICIT NONE


      
      IF ( config_flags%dfi_opt .NE. DFI_NODFI ) THEN
   
         SELECT CASE ( config_flags%dfi_opt ) 
     
            CASE (DFI_DFL)
               wrf_err_message = 'Initializing with DFL'
               CALL wrf_message(TRIM(wrf_err_message))
   
               wrf_err_message = '   Filtering forward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_fwd_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               CALL wrf_dfi_fst_init()
   
               IF ( config_flags%dfi_write_filtered_input ) THEN
                  CALL wrf_dfi_write_initialized_state()
               END IF
   
            CASE (DFI_DDFI)
               wrf_err_message = 'Initializing with DDFI'
               CALL wrf_message(TRIM(wrf_err_message))
   
               wrf_err_message = '   Integrating backward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_bck_init()
               CALL wrf_run()
   
               wrf_err_message = '   Filtering forward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_fwd_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               CALL wrf_dfi_fst_init()
   
               IF ( config_flags%dfi_write_filtered_input ) THEN
                  CALL wrf_dfi_write_initialized_state()
               END IF
   
            CASE (DFI_TDFI)
               wrf_err_message = 'Initializing with TDFI'
               CALL wrf_message(TRIM(wrf_err_message))
   
               wrf_err_message = '   Integrating backward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_bck_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               wrf_err_message = '   Filtering forward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_fwd_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               CALL wrf_dfi_fst_init()
   
               IF ( config_flags%dfi_write_filtered_input ) THEN
                  CALL wrf_dfi_write_initialized_state()
               END IF
   
            CASE DEFAULT
               wrf_err_message = 'Unrecognized DFI_OPT in namelist'
               CALL wrf_error_fatal3("<stdin>",461,&
TRIM(wrf_err_message))
   
         END SELECT
   
      END IF


   END SUBROUTINE wrf_dfi

   SUBROUTINE set_derived_rconfigs








      IMPLICIT NONE

      INTEGER :: i



      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = model_config_rec % mp_physics(i)
        ENDDO
      END IF



   END SUBROUTINE set_derived_rconfigs

   RECURSIVE SUBROUTINE alloc_doms_for_dfi ( grid )
   
      

      TYPE (domain) , pointer :: grid

      

      TYPE (domain) , pointer :: new_nest_loc
      TYPE (grid_config_rec_type) :: parent_config_flags
      INTEGER :: nestid_loc , kid_loc
   
         
         
         
   
         DO WHILE ( nests_to_open( grid , nestid_loc , kid_loc ) )

            
            
   
            CALL alloc_and_configure_domain ( domain_id  = nestid_loc   , &
                                              grid       = new_nest_loc , &
                                              parent     = grid         , &
                                              kid        = kid_loc        )
         
print *,'for parent domain id #',grid%id,', found child domain #',nestid_loc
            

            new_nest_loc%dfi_opt = head_grid%dfi_opt
            new_nest_loc%dfi_stage = DFI_SETUP
         
            

            CALL Setup_Timekeeping (new_nest_loc)

            

            CALL model_to_grid_config_rec ( grid%id , model_config_rec , parent_config_flags )
            CALL med_nest_initial ( grid , new_nest_loc , config_flags )

            
            
   
            CALL alloc_doms_for_dfi ( new_nest_loc )
   
         END DO
   
   END SUBROUTINE alloc_doms_for_dfi

END MODULE module_wrf_top
