


MODULE module_integrate

CONTAINS

RECURSIVE SUBROUTINE integrate ( grid )



   USE module_domain
   USE module_driver_constants
   USE module_nesting
   USE module_configure
   USE module_timing
   USE module_utility
   USE module_cpl, ONLY : coupler_on, cpl_snd, cpl_defdomain

   IMPLICIT NONE

   

   TYPE(domain) , POINTER :: grid






















































































































   

   CHARACTER*32                           :: outname, rstname
   TYPE(domain) , POINTER                 :: grid_ptr , new_nest
   TYPE(domain)                           :: intermediate_grid
   INTEGER                                :: step
   INTEGER                                :: nestid , kid
   LOGICAL                                :: a_nest_was_opened
   INTEGER                                :: fid , rid
   LOGICAL                                :: lbc_opened
   REAL                                   :: time, btime, bfrq
   CHARACTER*256                          :: message, message2,message3
   TYPE (grid_config_rec_type)            :: config_flags
   LOGICAL , EXTERNAL                     :: wrf_dm_on_monitor
   INTEGER                                :: idum1 , idum2 , ierr , open_status
   LOGICAL                                :: should_do_last_io

   
   INTERFACE
       
     SUBROUTINE solve_interface ( grid )
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE solve_interface
       
       
     SUBROUTINE med_calc_model_time ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_calc_model_time
       
       
     SUBROUTINE med_before_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_before_solve_io
       
       
     SUBROUTINE med_after_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_after_solve_io
       
       
     SUBROUTINE med_pre_nest_initial ( parent , newid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  parent
       INTEGER, INTENT(IN)    ::  newid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_pre_nest_initial
     SUBROUTINE med_nest_initial ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_initial
       
       
     SUBROUTINE med_nest_force ( parent , grid )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid, parent
     END SUBROUTINE med_nest_force









       
       
     SUBROUTINE med_nest_feedback ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_feedback

       
       
     SUBROUTINE med_last_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_last_solve_io
       
       
     SUBROUTINE med_setup_step ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_setup_step
       
       
     SUBROUTINE med_endup_step ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_endup_step
       
       

     SUBROUTINE Setup_Timekeeping( grid )
       USE module_domain
       TYPE(domain), POINTER :: grid
     END SUBROUTINE

     SUBROUTINE dfi_accumulate( grid )
       USE module_domain
       TYPE(domain), POINTER :: grid
     END SUBROUTINE

   END INTERFACE

   
   
   CALL set_current_grid_ptr( grid )

   IF ( .NOT. domain_clockisstoptime( grid ) ) THEN
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
      IF ( config_flags%grid_allowed ) THEN
         CALL domain_clockprint ( 150, grid, 'DEBUG:  top of integrate(),' )
         DO WHILE ( .NOT. domain_clockisstopsubtime(grid) )
            IF ( wrf_dm_on_monitor() ) THEN
               CALL start_timing
            END IF
            CALL med_setup_step ( grid , config_flags )
            a_nest_was_opened = .false.
            
            DO WHILE ( nests_to_open( grid , nestid , kid ) )
               
               
               a_nest_was_opened = .true.
               CALL med_pre_nest_initial ( grid , nestid , config_flags )
               CALL alloc_and_configure_domain ( domain_id  = nestid ,   &
                                                 grid       = new_nest , &
                                                 parent     = grid ,     &
                                                 kid        = kid        )
               CALL Setup_Timekeeping (new_nest)
               CALL med_nest_initial ( grid , new_nest , config_flags )
               IF ( grid%dfi_stage == DFI_STARTFWD ) THEN
                  CALL wrf_dfi_startfwd_init(new_nest)
               ENDIF
               IF (coupler_on) CALL cpl_defdomain( new_nest ) 
            END DO
            IF ( a_nest_was_opened ) THEN
               CALL set_overlaps ( grid )   
            END IF

            
            CALL dfi_accumulate ( grid )

            CALL med_before_solve_io ( grid , config_flags )
            grid_ptr => grid
            DO WHILE ( ASSOCIATED( grid_ptr ) )
               CALL set_current_grid_ptr( grid_ptr )
               CALL wrf_debug( 100 , 'module_integrate: calling solve interface ' )
               CALL solve_interface ( grid_ptr ) 
               CALL domain_clockadvance ( grid_ptr )
               CALL wrf_debug( 100 , 'module_integrate: back from solve interface ' )
               
               
               CALL domain_time_test( grid_ptr, 'domain_clockadvance' )
               grid_ptr => grid_ptr%sibling
            END DO
            CALL set_current_grid_ptr( grid )
            CALL med_calc_model_time ( grid , config_flags )
            CALL med_after_solve_io ( grid , config_flags )
            grid_ptr => grid
            DO WHILE ( ASSOCIATED( grid_ptr ) )
               DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
                   CALL set_current_grid_ptr( grid_ptr%nests(kid)%ptr )
                   
                   CALL wrf_debug( 100 , 'module_integrate: calling med_nest_force ' )
                   CALL med_nest_force ( grid_ptr , grid_ptr%nests(kid)%ptr )
                   CALL wrf_debug( 100 , 'module_integrate: back from med_nest_force ' )
                   grid_ptr%nests(kid)%ptr%start_subtime = &
                     domain_get_current_time(grid) - domain_get_time_step(grid)
                   grid_ptr%nests(kid)%ptr%stop_subtime = &
                     domain_get_current_time(grid)
                   CALL integrate ( grid_ptr%nests(kid)%ptr ) 
                   CALL wrf_debug( 100 , 'module_integrate: back from recursive call to integrate ' )
                   IF ( .NOT. ( domain_clockisstoptime(head_grid              ) .OR. &
                                domain_clockisstoptime(grid                   ) .OR. &
                                domain_clockisstoptime(grid_ptr%nests(kid)%ptr) ) )  THEN
                     CALL wrf_debug( 100 , 'module_integrate: calling med_nest_feedback ' )
                     CALL med_nest_feedback ( grid_ptr , grid_ptr%nests(kid)%ptr , config_flags )
                     CALL wrf_debug( 100 , 'module_integrate: back from med_nest_feedback ' )
                   END IF





                 END IF
               END DO
               IF (coupler_on) CALL cpl_snd( grid_ptr ) 
               grid_ptr => grid_ptr%sibling
            END DO
            CALL set_current_grid_ptr( grid )
            
            IF ( wrf_dm_on_monitor() ) THEN
               CALL domain_clock_get ( grid, current_timestr=message2 )

               if (config_flags%use_adaptive_time_step) then
                  WRITE ( message , FMT = '("main (dt=",F6.2,"): time ",A," on domain ",I3)' ) grid%dt, TRIM(message2), grid%id
               else
                  WRITE ( message , FMT = '("main: time ",A," on domain ",I3)' ) TRIM(message2), grid%id
               endif



               CALL end_timing ( TRIM(message) )
            END IF
            CALL med_endup_step ( grid , config_flags )
         END DO

         
         CALL dfi_accumulate ( grid )

         
         
         IF ( grid%id .EQ. 1 ) THEN               
            CALL med_last_solve_io ( grid , config_flags )
         ELSE

            should_do_last_io = domain_clockisstoptime( head_grid )
            grid_ptr => grid 
            DO WHILE ( grid_ptr%id .NE. 1 )
               IF ( domain_clockisstoptime( grid_ptr ) ) THEN
                  should_do_last_io = .TRUE. 
               END IF
               grid_ptr => grid_ptr%parents(1)%ptr
            ENDDO
            IF ( should_do_last_io ) THEN 
               grid_ptr => grid 
               CALL med_nest_feedback ( grid_ptr%parents(1)%ptr, grid , config_flags )
               CALL med_last_solve_io ( grid , config_flags )
            ENDIF
         ENDIF
      ENDIF
   END IF
   
END SUBROUTINE integrate

END MODULE module_integrate

