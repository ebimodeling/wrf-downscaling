

   MODULE module_check_a_mundo


















      USE module_state_description
      USE module_wrf_error
      USE module_configure

      IMPLICIT NONE



   CONTAINS



   SUBROUTINE  check_nml_consistency
 






      IMPLICIT NONE

      LOGICAL :: exists
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      INTEGER :: i, oops
      LOGICAL :: km_opt_already_done , diff_opt_already_done








   model_config_rec % wrf_hydro = 0













      km_opt_already_done = .FALSE.
      diff_opt_already_done = .FALSE.
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. -1 ) THEN
            model_config_rec % km_opt(i) = model_config_rec % km_opt(1)
            IF ( .NOT. km_opt_already_done ) THEN
               CALL wrf_message ( "Setting blank km_opt entries to domain #1 values.")
               CALL wrf_message ( " --> The km_opt entry in the namelist.input is now max_domains." )
            END IF
            km_opt_already_done = .TRUE.
         END IF
         IF ( model_config_rec % diff_opt(i) .EQ. -1 ) THEN
            model_config_rec % diff_opt(i) = model_config_rec % diff_opt(1)
            IF ( .NOT. diff_opt_already_done ) THEN
               CALL wrf_message ( "Setting blank diff_opt entries to domain #1 values.")
               CALL wrf_message ( " --> The diff_opt entry in the namelist.input is now max_domains." )
            END IF
            diff_opt_already_done = .TRUE.
         END IF
      ENDDO







      IF ( ( model_config_rec %   km_opt(1) .EQ. -1 ) .OR. &
           ( model_config_rec % diff_opt(1) .EQ. -1 ) ) THEN
            wrf_err_message = 'Both km_opt and diff_opt need to be set in the namelist.input file.'
         CALL wrf_error_fatal3("<stdin>",100,&
TRIM( wrf_err_message ) )
      END IF







      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_surface_physics(i)     .NE. &
              model_config_rec % sf_surface_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_surface_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_surface_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",116,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO






      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_sfclay_physics(i)     .NE. &
              model_config_rec % sf_sfclay_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_sfclay_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_sfclay_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",132,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO






      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % mp_physics(i)     .NE. &
              model_config_rec % mp_physics(1) ) THEN
            wrf_err_message = '--- ERROR: mp_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix mp_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",148,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO






      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_lw_physics(i)     .NE. &
              model_config_rec % ra_lw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_lw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_lw_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",164,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_sw_physics(i)     .NE. &
              model_config_rec % ra_sw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_sw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_sw_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",175,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO






      DO i = 2, model_config_rec % max_dom
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. model_config_rec % bl_pbl_physics(1) ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. 0                                    ) ) THEN
            wrf_err_message = '--- ERROR: bl_pbl_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix bl_pbl_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",191,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO







      DO i = 2, model_config_rec % max_dom
         IF ( ( model_config_rec % cu_physics(i) .NE. model_config_rec % cu_physics(1) ) .AND. &
              ( model_config_rec % cu_physics(i) .NE. 0                                ) ) THEN
            wrf_err_message = '--- ERROR: cu_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix cu_physics in namelist.input '
            CALL wrf_error_fatal3("<stdin>",208,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO





      IF ( ( model_config_rec%fractional_seaice .EQ. 0 ).AND. &
              ( model_config_rec%tice2tsk_if2cold ) ) THEN
            wrf_err_message = '--- WARNING: You set tice2tsk_if2cold = .true.,  but fractional_seaice = 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- WARNING: tice2tsk_if2cold will have no effect on results.'
            CALL wrf_message ( wrf_err_message )
      END IF





      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%fine_input_stream(i) .NE. 0 ).AND. &
              ( model_config_rec%io_form_auxinput2 .EQ. 0 ) ) THEN
            wrf_err_message = '--- ERROR: If fine_input_stream /= 0, io_form_auxinput2 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput2 in the time_control namelist (probably to 2).'
            CALL wrf_error_fatal3("<stdin>",235,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO







      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%sf_surface_physics(i) == NOAHMPSCHEME ) THEN

            

            IF ( model_config_rec%sf_urban_physics(i) /= 0 ) THEN
               WRITE(wrf_err_message, '(" --- ERROR:   Noah-MP LSM scheme (sf_surface_physics==", I2, ")")') NOAHMPSCHEME
               CALL wrf_message ( TRIM ( wrf_err_message ) )
               WRITE(wrf_err_message, '("              does not work with urban physics schemes")')
               CALL wrf_error_fatal3("<stdin>",255,&
TRIM ( wrf_err_message ) )
            ENDIF

         END IF
      END DO




      IF ( model_config_rec%seaice_albedo_opt == 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( model_config_rec%sf_surface_physics(i) /= LSMSCHEME ) .AND. &
                 ( model_config_rec%sf_surface_physics(i) /= NOAHMPSCHEME ) ) THEN

               write (wrf_err_message, '(" --- ERROR:   seaice_albedo_opt == 1 works only with ")')
               CALL wrf_message ( TRIM ( wrf_err_message ) )
               write (wrf_err_message, '("              sf_surface_physics == ", I2, " (Noah) or ", I2, " (Noah-MP).")') &
               LSMSCHEME, NOAHMPSCHEME
               call wrf_error_fatal3("<stdin>",274,&
TRIM ( wrf_err_message ) )

            END IF
            
         END DO

      END IF










   model_config_rec % stoch_force_global_opt=0  
 
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_force_opt(i) .NE. 0)  then 
           model_config_rec % stoch_force_global_opt=1 
         endif
   ENDDO 






   IF ( model_config_rec % perturb_bdy .EQ. 1 ) then
        model_config_rec % stoch_force_global_opt=1
      wrf_err_message = '--- WARNING: perturb_bdy=1 option uses SKEBS pattern and may'
      CALL wrf_message ( wrf_err_message )
      wrf_err_message = '             increase computation time.'
      CALL wrf_message ( wrf_err_message )
   ENDIF





   IF ( ( model_config_rec%traj_opt .EQ. 0 ) .AND. &
        ( model_config_rec%num_traj .NE. 0 ) ) THEN
         WRITE (wrf_err_message, FMT='(A,A)') '--- WARNING: traj_opt is zero, but ', &
                'num_traj is not zero; setting num_traj to zero.'
         CALL wrf_message ( wrf_err_message )
         model_config_rec%num_traj = 0 
   END IF







      IF ( model_config_rec%hypsometric_opt .EQ. 2 &
           .AND. model_config_rec%adjust_heights ) THEN
           WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: hypsometric_opt is 2, ', &
                  'setting adjust_heights = F'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%adjust_heights = .false.
      ENDIF







      IF ( model_config_rec%sst_update .EQ. 0 ) THEN
         model_config_rec%io_form_auxinput4 = 0
         DO i = 1, model_config_rec % max_dom
            WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: sst_update is 0, ', &
                  'setting io_form_auxinput4 = 0 and auxinput4_interval = 0 for all domains'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%auxinput4_interval(i)   = 0
            model_config_rec%auxinput4_interval_y(i) = 0
            model_config_rec%auxinput4_interval_d(i) = 0
            model_config_rec%auxinput4_interval_h(i) = 0
            model_config_rec%auxinput4_interval_m(i) = 0
            model_config_rec%auxinput4_interval_s(i) = 0
         ENDDO
      ELSE
         IF ( model_config_rec%io_form_auxinput4 .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, io_form_auxinput4 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput4 in the time_control namelist (probably to 2).'
            CALL wrf_error_fatal3("<stdin>",363,&
TRIM( wrf_err_message ) )
         END IF
      END IF






      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%grid_sfdda(i) .EQ. 1 ).AND. &
              ( model_config_rec%grid_fdda (i) .NE. 1 ) ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda = 1, then grid_fdda must also = 1 for that domain '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Change grid_fdda or grid_sfdda in namelist.input '
            CALL wrf_error_fatal3("<stdin>",379,&
TRIM( wrf_err_message ) )
         END IF
      ENDDO









      DO i = 1, model_config_rec % max_dom

         IF ( model_config_rec%grid_fdda(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: grid_fdda is 0 for domain ', &
                         i, ', setting gfdda interval and ending time to 0 for that domain.'
            CALL wrf_message ( wrf_err_message )

            model_config_rec%gfdda_end_y(i) = 0
            model_config_rec%gfdda_end_d(i) = 0
            model_config_rec%gfdda_end_h(i) = 0
            model_config_rec%gfdda_end_m(i) = 0
            model_config_rec%gfdda_end_s(i) = 0
            model_config_rec%gfdda_interval(i)   = 0
            model_config_rec%gfdda_interval_y(i) = 0
            model_config_rec%gfdda_interval_d(i) = 0
            model_config_rec%gfdda_interval_h(i) = 0
            model_config_rec%gfdda_interval_m(i) = 0
            model_config_rec%gfdda_interval_s(i) = 0
         END IF

         IF ( ( model_config_rec%grid_sfdda(i) .EQ. 0 ) .AND. &
              ( model_config_rec%pxlsm_soil_nudge(i) .EQ. 0 ) ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') &
                         '--- NOTE: both grid_sfdda and pxlsm_soil_nudge are 0 for domain ', &
                         i, ', setting sgfdda interval and ending time to 0 for that domain.'
            CALL wrf_message ( wrf_err_message )

            model_config_rec%sgfdda_end_y(i) = 0
            model_config_rec%sgfdda_end_d(i) = 0
            model_config_rec%sgfdda_end_h(i) = 0
            model_config_rec%sgfdda_end_m(i) = 0
            model_config_rec%sgfdda_end_s(i) = 0
            model_config_rec%sgfdda_interval(i)   = 0
            model_config_rec%sgfdda_interval_y(i) = 0
            model_config_rec%sgfdda_interval_d(i) = 0
            model_config_rec%sgfdda_interval_h(i) = 0
            model_config_rec%sgfdda_interval_m(i) = 0
            model_config_rec%sgfdda_interval_s(i) = 0
         END IF

         IF ( model_config_rec%obs_nudge_opt(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: obs_nudge_opt is 0 for domain ', &
                         i, ', setting obs nudging interval and ending time to 0 for that domain.'
            CALL wrf_message ( wrf_err_message )

            model_config_rec%fdda_end(i) = 0
            model_config_rec%auxinput11_interval(i)   = 0
            model_config_rec%auxinput11_interval_y(i) = 0
            model_config_rec%auxinput11_interval_d(i) = 0
            model_config_rec%auxinput11_interval_h(i) = 0
            model_config_rec%auxinput11_interval_m(i) = 0
            model_config_rec%auxinput11_interval_s(i) = 0
            model_config_rec%auxinput11_end(i)   = 0
            model_config_rec%auxinput11_end_y(i) = 0
            model_config_rec%auxinput11_end_d(i) = 0
            model_config_rec%auxinput11_end_h(i) = 0
            model_config_rec%auxinput11_end_m(i) = 0
            model_config_rec%auxinput11_end_s(i) = 0
         END IF

      ENDDO      





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. QNSEPBLSCHEME ) .AND. &
              ( model_config_rec%mfshconv(i) .NE. 0 ) ) THEN
            model_config_rec%mfshconv(i) = 0
            oops = oops + 1
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'bl_pbl_physics /= 4, implies mfshconv must be 0, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .EQ. GRIMSSHCUSCHEME ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
               
            ELSE
               model_config_rec%shcu_physics(i) = 0
               oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'bl_pbl_physics /= 1,5,6 implies shcu_physics cannot be 3, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF





      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. YSUSCHEME ) .AND. &
              ( model_config_rec%gwd_opt .EQ. GWDOPT ) ) THEN
            model_config_rec%gwd_opt = 0
            oops = oops + 1
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'bl_pbl_physics /= 1, implies gwd_opt cannot be 1, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF






      IF ( MAXVAL( model_config_rec%grid_fdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_gfdda = 0
      ELSE
         IF ( model_config_rec%io_form_gfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_fdda /= 0, io_form_gfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_gfdda in the time_control namelist (probably to 2).'
            CALL wrf_error_fatal3("<stdin>",522,&
TRIM( wrf_err_message ) )
         END IF
      END IF
      IF ( MAXVAL( model_config_rec%grid_sfdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_sgfdda = 0
      ELSE
         IF ( model_config_rec%io_form_sgfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda /= 0, io_form_sgfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_sgfdda in the time_control namelist (probably to 2).'
            CALL wrf_error_fatal3("<stdin>",533,&
TRIM( wrf_err_message ) )
         END IF
      END IF





      IF ( model_config_rec%p_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( MAX ( model_config_rec%auxhist23_interval  (i) , &
                         model_config_rec%auxhist23_interval_d(i) , &
                         model_config_rec%auxhist23_interval_h(i) , &
                         model_config_rec%auxhist23_interval_m(i) , &
                         model_config_rec%auxhist23_interval_s(i) ) == 0 ) .OR. &
                 (  model_config_rec%io_form_auxhist23 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: p_lev_diags requires auxhist23 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist23_interval (max_dom) and io_form_auxhist23'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 23 for pressure-level diags'
               CALL wrf_error_fatal3("<stdin>",555,&
TRIM( wrf_err_message ) )
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            model_config_rec%p_lev_interval(i) = model_config_rec%auxhist23_interval  (i)*   60 + &
                                                 model_config_rec%auxhist23_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist23_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist23_interval_m(i)*   60 + &
                                                 model_config_rec%auxhist23_interval_s(i)
         END DO
      END IF





      IF ( ( model_config_rec%nwp_diagnostics .NE. 0 ) .AND. &
           ( model_config_rec%history_interval(1) .EQ. 0 ) ) THEN
         wrf_err_message = '--- ERROR:  nwp_diagnostics requires the use of "history_interval" namelist.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace interval variable with "history_interval".'
         CALL wrf_error_fatal3("<stdin>",577,&
wrf_err_message )
      END IF








      IF ( model_config_rec%omlcall .NE. 0 ) THEN
         wrf_err_message = '--- ERROR:  The namelist.input variable "omlcall" has been renamed.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace "omlcall" with the new name "sf_ocean_physics".'
         CALL wrf_error_fatal3("<stdin>",592,&
wrf_err_message )
      END IF







      IF ( model_config_rec%use_adaptive_time_step ) THEN
         IF ( ( model_config_rec%cu_physics(1) .EQ. BMJSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. MESO_SAS     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SASSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. OSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. NSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. TIEDTKESCHEME ) ) THEN
            wrf_err_message = '--- WARNING: If use_adaptive_time_step, must use cudt=0 for the following CU schemes:'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '---          BMJ, all SAS, Tiedtke' 
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '---          CUDT=0 has been done for you.'
            CALL wrf_message ( wrf_err_message )
            DO i = 1, model_config_rec % max_dom
               model_config_rec%cudt(i) = 0
            END DO
         END IF
      END IF






      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%cu_rad_feedback(i) .EQV. .TRUE. )  .OR. &
              ( model_config_rec%cu_rad_feedback(i) .EQV. .true. ) ) THEN
            IF ( ( model_config_rec%cu_physics(1) .EQ. GFSCHEME     ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. G3SCHEME     ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. GDSCHEME     ) ) THEN
               wrf_err_message = '--- WARNING: Turning on cu_rad_feedback also requires setting cu_diag== 1'
               CALL wrf_message ( wrf_err_message )
               model_config_rec%cu_diag(i) = 1
            ELSE
               model_config_rec%cu_diag(i) = 0
            END IF
         END IF
      END DO





 
       DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_diag(i) .EQ. G3TAVE ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. GDSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. GFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. G3SCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using cu_diag=1 requires use of one of the following CU schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell (G3) CU scheme' 
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Devenyi (GD) CU scheme' 
                CALL wrf_error_fatal3("<stdin>",656,&
wrf_err_message )
          END IF
         END IF
       END DO
 




      IF ( wrf_dm_on_monitor() ) THEN
         CALL wrf_tsin_exist ( exists )
         IF ( exists ) THEN
            model_config_rec%process_time_series = 1
         ELSE
            model_config_rec%process_time_series = 0
         END IF
      END IF

      CALL wrf_dm_bcast_integer(model_config_rec%process_time_series, 1)







      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%cu_physics(i) .EQ. GDSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. GFSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. G3SCHEME ) ) THEN
            model_config_rec%cu_diag(i) = 1
         ELSE
            model_config_rec%cu_diag(i) = 0
         END IF
      END DO





      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              ( model_config_rec%sf_sfclay_physics(i) .NE. TEMFSFCSCHEME ) )  THEN
            wrf_err_message = '--- ERROR: Using bl_pbl_physics=10 requires sf_sfclay_physics=10 '
            CALL wrf_error_fatal3("<stdin>",701,&
TRIM(wrf_err_message) )
         ELSEIF ( ( model_config_rec%bl_pbl_physics(i) .NE. TEMFPBLSCHEME ) .AND. &
                  ( model_config_rec%sf_sfclay_physics(i) .EQ. TEMFSFCSCHEME ) ) THEN
            wrf_err_message = '--- ERROR: Using sf_sfclay_physics=10 requires bl_pbl_physics=10 '
            CALL wrf_error_fatal3("<stdin>",706,&
TRIM(wrf_err_message) )
         END IF
      ENDDO      





      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              (model_config_rec%dfi_opt .NE. DFI_NODFI) )  THEN
            wrf_err_message = '--- ERROR: DFI not available for bl_pbl_physics=10 '
            CALL wrf_error_fatal3("<stdin>",719,&
TRIM(wrf_err_message) )
         END IF
      ENDDO      










      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%grav_settling(i) .NE. FOGSETTLING0 ) THEN
                model_config_rec%grav_settling(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'mp_physics == 28, already has gravitational fog settling; resetting grav_settling to 0'
         CALL wrf_message ( wrf_err_message )
      END IF
 



      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%use_aero_icbc .AND. model_config_rec%scalar_pblmix(i) .NE. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 1
                oops = oops + 1
            END IF
         END IF
      ENDDO      
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'For mp_physics == 28 and use_aero_icbc is true, recommend to turn on scalar_pblmix'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = 'resetting scalar_pblmix = 1'
         CALL wrf_message ( wrf_err_message )
      END IF













      model_config_rec%auxinput10_begin_d     =       model_config_rec%gfdda_begin_d
      model_config_rec%auxinput10_begin_h     =       model_config_rec%gfdda_begin_h
      model_config_rec%auxinput10_begin_m     =       model_config_rec%gfdda_begin_m
      model_config_rec%auxinput10_begin_s     =       model_config_rec%gfdda_begin_s
      model_config_rec%auxinput10_begin_y     =       model_config_rec%gfdda_begin_y
      model_config_rec%auxinput10_end_d       =       model_config_rec%gfdda_end_d
      model_config_rec%auxinput10_end_h       =       model_config_rec%gfdda_end_h
      model_config_rec%auxinput10_end_m       =       model_config_rec%gfdda_end_m
      model_config_rec%auxinput10_end_s       =       model_config_rec%gfdda_end_s
      model_config_rec%auxinput10_end_y       =       model_config_rec%gfdda_end_y
      model_config_rec%auxinput10_inname      =       model_config_rec%gfdda_inname
      model_config_rec%auxinput10_interval    =       model_config_rec%gfdda_interval
      model_config_rec%auxinput10_interval_d  =       model_config_rec%gfdda_interval_d
      model_config_rec%auxinput10_interval_h  =       model_config_rec%gfdda_interval_h
      model_config_rec%auxinput10_interval_m  =       model_config_rec%gfdda_interval_m
      model_config_rec%auxinput10_interval_s  =       model_config_rec%gfdda_interval_s
      model_config_rec%auxinput10_interval_y  =       model_config_rec%gfdda_interval_y
      model_config_rec%io_form_auxinput10     =       model_config_rec%io_form_gfdda
      model_config_rec%auxinput9_begin_d      =       model_config_rec%sgfdda_begin_d
      model_config_rec%auxinput9_begin_h      =       model_config_rec%sgfdda_begin_h
      model_config_rec%auxinput9_begin_m      =       model_config_rec%sgfdda_begin_m
      model_config_rec%auxinput9_begin_s      =       model_config_rec%sgfdda_begin_s
      model_config_rec%auxinput9_begin_y      =       model_config_rec%sgfdda_begin_y
      model_config_rec%auxinput9_end_d        =       model_config_rec%sgfdda_end_d
      model_config_rec%auxinput9_end_h        =       model_config_rec%sgfdda_end_h
      model_config_rec%auxinput9_end_m        =       model_config_rec%sgfdda_end_m
      model_config_rec%auxinput9_end_s        =       model_config_rec%sgfdda_end_s
      model_config_rec%auxinput9_end_y        =       model_config_rec%sgfdda_end_y
      model_config_rec%auxinput9_inname       =       model_config_rec%sgfdda_inname
      model_config_rec%auxinput9_interval     =       model_config_rec%sgfdda_interval
      model_config_rec%auxinput9_interval_d   =       model_config_rec%sgfdda_interval_d
      model_config_rec%auxinput9_interval_h   =       model_config_rec%sgfdda_interval_h
      model_config_rec%auxinput9_interval_m   =       model_config_rec%sgfdda_interval_m
      model_config_rec%auxinput9_interval_s   =       model_config_rec%sgfdda_interval_s
      model_config_rec%auxinput9_interval_y   =       model_config_rec%sgfdda_interval_y
      model_config_rec%io_form_auxinput9      =       model_config_rec%io_form_sgfdda
      IF (model_config_rec%prec_acc_dt(1) .gt. 0.) model_config_rec%prec_acc_opt = 1
      IF (model_config_rec%bucket_mm .gt. 0.) model_config_rec%bucketr_opt = 1



   END SUBROUTINE 



   SUBROUTINE set_physics_rconfigs










      IMPLICIT NONE




      INTEGER :: numsoiltemp , nummosaictemp

      IF ( model_config_rec % sf_surface_mosaic .EQ. 1 ) THEN
      
      numsoiltemp = model_config_rec % num_soil_layers
      nummosaictemp = model_config_rec % mosaic_cat
      
         model_config_rec % mosaic_cat_soil = numsoiltemp * nummosaictemp

         wrf_err_message = '--- NOTE: Noah-mosaic is in use, setting:  ' // &
                           'mosaic_cat_soil = mosaic_cat * num_soil_layers'
         CALL wrf_message ( wrf_err_message )

      END IF     
      





      IF (( model_config_rec % ra_lw_physics(1) .EQ. CAMLWSCHEME ) .OR. & 
          ( model_config_rec % ra_sw_physics(1) .EQ. CAMSWSCHEME )) THEN
         model_config_rec % paerlev = 29
         model_config_rec % levsiz = 59
         model_config_rec % cam_abs_dim1 = 4 
         model_config_rec % cam_abs_dim2 = model_config_rec % e_vert(1)

         wrf_err_message = '--- NOTE: CAM radiation is in use, setting:  ' // &
                           'paerlev=29, levsiz=59, cam_abs_dim1=4, cam_abs_dim2=e_vert'
         CALL wrf_message ( wrf_err_message )

      END IF






      IF (( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME )) THEN
         model_config_rec % levsiz = 59
         model_config_rec % alevsiz = 12
         model_config_rec % no_src_types = 6

         wrf_err_message = '--- NOTE: RRTMG radiation is in use, setting:  ' // &
                           'levsiz=59, alevsiz=12, no_src_types=6'
         CALL wrf_message ( wrf_err_message )

      END IF






      IF ( model_config_rec % sf_surface_physics(1) .EQ. 0           ) &
           model_config_rec % num_soil_layers = 5
      IF ( model_config_rec % sf_surface_physics(1) .EQ. SLABSCHEME  ) &
           model_config_rec % num_soil_layers = 5
      IF ( model_config_rec % sf_surface_physics(1) .EQ. LSMSCHEME   ) &
           model_config_rec % num_soil_layers = 4
      IF ( model_config_rec % sf_surface_physics(1) .EQ. NOAHMPSCHEME   ) &
           model_config_rec % num_soil_layers = 4
      IF ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME .AND. &
           (model_config_rec % num_soil_layers .NE. 6 .AND. model_config_rec % num_soil_layers .NE. 9) ) &
           model_config_rec % num_soil_layers = 6
      IF ( model_config_rec % sf_surface_physics(1) .EQ. PXLSMSCHEME ) &
           model_config_rec % num_soil_layers = 2
      IF ( model_config_rec % sf_surface_physics(1) .EQ. CLMSCHEME ) &
           model_config_rec % num_soil_layers = 10
      IF ( model_config_rec % sf_surface_physics(1) .EQ. 88          ) &
           model_config_rec % num_soil_layers = 4

      WRITE (wrf_err_message, FMT='(A,I6)') '--- NOTE: num_soil_layers has been set to ', &
                                             model_config_rec % num_soil_layers
      CALL wrf_message ( wrf_err_message )

   END SUBROUTINE set_physics_rconfigs



   END MODULE module_check_a_mundo


