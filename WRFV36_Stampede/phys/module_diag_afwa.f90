









MODULE module_diag_afwa

CONTAINS

  SUBROUTINE afwa_diagnostics_driver (   grid , config_flags     &
                             , moist                             &
                             , scalar                            &
                             , chem                              &
                             , th_phy , pi_phy , p_phy           &
                             , dz8w , p8w , t8w , rho_phy        &
                             , ids, ide, jds, jde, kds, kde      &
                             , ims, ime, jms, jme, kms, kme      &
                             , ips, ipe, jps, jpe, kps, kpe      &
                             , its, ite, jts, jte                &
                             , k_start, k_end               )

    
    USE module_domain
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants

    USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval


    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER             :: k_start , k_end, its, ite, jts, jte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_moist),    &
         INTENT(IN   ) ::                                moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_scalar),    &
         INTENT(IN   ) ::                                scalar

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_chem),     &
         INTENT(IN   ) ::                                 chem

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                               th_phy  &
                                              ,         pi_phy  &
                                              ,          p_phy  &
                                              ,           dz8w  &
                                              ,            p8w  &
                                              ,            t8w  &
                                              ,        rho_phy

    
    
    CHARACTER*256 :: message, timestr 
    INTEGER :: i,j,k
    INTEGER :: icing_opt
    REAL :: bdump
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::      qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  &
                                              ,          qvapr  &
                                              ,         qcloud  &
                                              ,           qice  &
                                              ,         ncloud  &
                                              ,             rh  &
                                              ,           ptot  &
                                              ,            z_e  &
                                              ,           zagl

    REAL, DIMENSION( ims:ime, jms:jme, 5 ) ::            dustc
    REAL, DIMENSION( ims:ime, jms:jme ) ::                rh2m  &
                                              ,        wind10m

    
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep

    
    
    write ( message, * ) 'inside afwa_diagnostics_driver'
    CALL wrf_debug( 100 , message )

    
    
    
    
    
    CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time, &
         ringinterval=histint)
    CALL WRFU_ALARMGET( grid%alarms( AUXHIST2_ALARM ), prevringtime=aux2_time, &
         ringinterval=aux2int)

    
    
    CALL domain_clock_get ( grid, current_time=CurrTime, &
         current_timestr=timestr, time_step=dtint )

    
    
    
    is_after_history_dump = ( Currtime .lt. hist_time + dtint )

    
    
    is_output_timestep = (Currtime .ge. hist_time + histint - dtint .or. &
                         Currtime .ge. aux2_time + aux2int - dtint )
    write ( message, * ) 'is output timestep? ', is_output_timestep
    CALL wrf_debug( 100 , message )
        
    
    
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          qvapr(i,k,j) = moist(i,k,j,P_QV)
          qrain(i,k,j) = moist(i,k,j,P_QR)
          qsnow(i,k,j) = moist(i,k,j,P_QS)
          qgrpl(i,k,j) = moist(i,k,j,P_QG)
          qcloud(i,k,j) = moist(i,k,j,P_QC)
          qice(i,k,j) = moist(i,k,j,P_QI)
          ncloud(i,k,j) = scalar(i,k,j,P_QNC)
        ENDDO
      ENDDO
    ENDDO
    
    
    
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
        ENDDO
      ENDDO
    ENDDO

    
    
    DO i=ims,ime
      DO k=kms,kme    
        DO j=jms,jme
          rh(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j))
        ENDDO
      ENDDO
    ENDDO

    dustc(ims:ime,jms:jme,:)=0.
   
    
    
    
    IF ( config_flags % afwa_severe_opt == 1 ) THEN

      
      
      
      
      
      IF ( is_after_history_dump ) THEN
        DO j = jms, jme
          DO i = ims, ime
            grid%wspd10max(i,j) = 0.
            grid%w_up_max(i,j) = 0.
            grid%w_dn_max(i,j) = 0.
            grid%tcoli_max(i,j) = 0.
            grid%up_heli_max(i,j) = 0.
            grid%refd_max(i,j) = 0.
            grid%afwa_llws(i,j) = 0.
            grid%afwa_hail(i,j) = 0.
            grid%afwa_tornado(i,j) = 0.
            grid%midrh_min_old(i,j) = grid%midrh_min(i,j) 
            grid%midrh_min(i,j) = 999.
          ENDDO
        ENDDO
      ENDIF  

      CALL severe_wx_diagnostics ( grid % wspd10max             &
                             , grid % w_up_max                  &
                             , grid % w_dn_max                  &
                             , grid % up_heli_max               &
                             , grid % tcoli_max                 &
                             , grid % midrh_min_old             &
                             , grid % midrh_min                 &
                             , grid % afwa_hail                 &
                             , grid % afwa_cape                 &
                             , grid % afwa_zlfc                 &
                             , grid % afwa_plfc                 &
                             , grid % afwa_llws                 &
                             , grid % afwa_tornado              &
                             , grid % u10                       &
                             , grid % v10                       &
                             , grid % w_2                       &
                             , grid % uh                        &
                             , grid % t_phy                     &
                             , grid % t2                        &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % u_phy                     &
                             , grid % v_phy                     &
                             , ptot                             &
                             , qice                             &
                             , qsnow                            &
                             , qgrpl                            &
                             , grid % rho                       &
                             , dz8w                             &
                             , rh                               &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end               )
    ENDIF   

    
    
    IF ( config_flags % afwa_ptype_opt == 1 ) THEN
    
      
      
      IF ( grid % itimestep .eq. 1) THEN
        DO i=ims,ime
          DO j=jms,jme
            grid % afwa_rain(i,j)=0.
            grid % afwa_snow(i,j)=0.
            grid % afwa_ice(i,j)=0.
            grid % afwa_fzra(i,j)=0.
            grid % afwa_snowfall(i,j)=0.
          ENDDO
        ENDDO
      ENDIF
  
      
      
      DO i=ims,ime
        DO j=jms,jme
          grid%afwa_precip(i,j)=grid%raincv(i,j)+grid%rainncv(i,j)
        ENDDO
      ENDDO

      
      
      CALL precip_type_diagnostics ( grid % t_phy               &
                             , grid % t2                        &
                             , rh                               &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % afwa_precip               &
                             , grid % swdown                    &
                             , grid % afwa_rain                 &
                             , grid % afwa_snow                 &
                             , grid % afwa_ice                  &
                             , grid % afwa_fzra                 &
                             , grid % afwa_snowfall             &
                             , grid % afwa_ptype_ccn_tmp        &
                             , grid % afwa_ptype_tot_melt       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
    ENDIF  
  
    
    
    IF ( is_output_timestep ) THEN      

      
      
      
      IF ( config_flags % afwa_radar_opt == 1 .or. &
         config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating Radar'
        CALL wrf_debug( 100 , message )
        CALL wrf_dbzcalc ( grid%rho                             &
                             , grid%t_phy                       &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  

      
      
      IF ( config_flags % afwa_radar_opt == 1 ) THEN
        write ( message, * ) 'Calculating derived radar variables'
        CALL wrf_debug( 100 , message )
        CALL radar_diagnostics ( grid % refd                    &
                             , grid % refd_com                  &
                             , grid % refd_max                  &
                             , grid % echotop                   &
                             , grid % z                         &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  

      
      
      IF ( config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating VIL'
        CALL wrf_debug( 100 , message )
        CALL vert_int_liquid_diagnostics ( grid % vil           &
                             , grid % radarvil                  &
                             , grid % t_phy                     &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , dz8w                             &
                             , grid % rho                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  

      
      
      IF ( config_flags % afwa_icing_opt == 1 ) THEN

        
        
        
        IF ( config_flags % mp_physics == GSFCGCESCHEME ) THEN
          icing_opt=1
        ELSEIF ( config_flags % mp_physics == ETAMPNEW ) THEN
          icing_opt=2
        ELSEIF ( config_flags % mp_physics == THOMPSON ) THEN
          icing_opt=3
        ELSEIF ( config_flags % mp_physics == WSM5SCHEME .OR.   &
                 config_flags % mp_physics == WSM6SCHEME ) THEN
          icing_opt=4
        ELSEIF ( config_flags % mp_physics == MORR_TWO_MOMENT ) THEN
          
          
          IF (config_flags % progn > 0) THEN
             icing_opt=6
          ELSE
             icing_opt=5
          ENDIF
        ELSEIF ( config_flags % mp_physics == WDM6SCHEME ) THEN
          icing_opt=7
        ELSE
          icing_opt=0  
        ENDIF
 
        write ( message, * ) 'Calculating Icing with icing opt ',icing_opt 
        CALL wrf_debug( 100 , message )
        CALL icing_diagnostics ( icing_opt                      &
                             , grid % fzlev                     &
                             , grid % icing_lg                  &
                             , grid % icing_sm                  &
                             , grid % qicing_lg_max             &
                             , grid % qicing_sm_max             &
                             , grid % qicing_lg                 &
                             , grid % qicing_sm                 &
                             , grid % icingtop                  &
                             , grid % icingbot                  &
                             , grid % t_phy                     &
                             , grid % z                         &
                             , dz8w                             &
                             , grid % rho                       &
                             , qrain                            &
                             , qcloud                           &
                             , ncloud                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  

      IF ( config_flags % afwa_vis_opt == 1 ) THEN
   
        
        
        DO i=ims,ime
          DO j=jms,jme
            rh2m(i,j)=calc_rh(grid%psfc(i,j), grid%t2(i,j), grid%q2(i,j))
          ENDDO
        ENDDO
      
        
        
        DO i=ims,ime
          DO j=jms,jme
            wind10m(i,j)=uv_wind(grid%u10(i,j),grid%v10(i,j))
          ENDDO
        ENDDO

        write ( message, * ) 'Calculating visibility'
        CALL wrf_debug( 100 , message )
        CALL vis_diagnostics ( qcloud(ims:ime,k_start,jms:jme)  &
                             , qrain(ims:ime,k_start,jms:jme)   &
                             , qice(ims:ime,k_start,jms:jme)    &
                             , qsnow(ims:ime,k_start,jms:jme)   &
                             , wind10m                          &
                             , rh2m                             &
                             , dustc                            &
                             , grid % afwa_vis                  &
                             , grid % afwa_vis_dust             &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe ) 
      ENDIF

      IF ( config_flags % afwa_cloud_opt == 1 ) THEN
        CALL cloud_diagnostics (qcloud                          &
                             , qice                             &
                             , qsnow                            &
                             , rh                               &
                             , dz8w                             &
                             , grid % rho                       &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % afwa_cloud                &
                             , grid % afwa_cloud_ceil           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF

    ENDIF  

  END SUBROUTINE afwa_diagnostics_driver



  SUBROUTINE severe_wx_diagnostics ( wspd10max                  &
                             , w_up_max                         &
                             , w_dn_max                         &
                             , up_heli_max                      &
                             , tcoli_max                        &
                             , midrh_min_old                    &
                             , midrh_min                        &
                             , afwa_hail                        &
                             , cape                             &
                             , zlfc                             &
                             , plfc                             &
                             , llws_max                         &
                             , afwa_tornado                     &
                             , u10                              &
                             , v10                              &
                             , w_2                              &
                             , uh                               &
                             , t_phy                            &
                             , t2                               &
                             , z                                &
                             , ht                               &
                             , u_phy                            &
                             , v_phy                            &
                             , p                                &
                             , qi                               &
                             , qs                               &
                             , qg                               &
                             , rho                              &
                             , dz8w                             &
                             , rh                               &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end               )

    INTEGER, INTENT(IN) :: its, ite, jts, jte, k_start, k_end   &
                         , ims, ime, jms, jme, kms, kme

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                    p  &
                                              ,            w_2  &
                                              ,          t_phy  &
                                              ,          u_phy  &
                                              ,          v_phy  &
                                              ,             qi  &
                                              ,             qs  &
                                              ,             qg  &
                                              ,            rho  &
                                              ,              z  &
                                              ,           dz8w  &
                                              ,             rh


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                  u10  &
                                              ,            v10  &
                                              ,             uh  &
                                              ,             t2  &
                                              ,             ht  &
                                              ,  midrh_min_old  &
                                              ,    up_heli_max


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                            wspd10max  &
                                              ,       w_up_max  &
                                              ,       w_dn_max  &
                                              ,      tcoli_max  &
                                              ,      midrh_min  &
                                              ,       llws_max  &
                                              ,      afwa_hail  &
                                              ,   afwa_tornado

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                 cape  &
                                              ,           zlfc  &
                                              ,           plfc

    
    
    INTEGER :: i,j,k
    INTEGER :: kts,kte
    REAL    :: zagl, zlfc_msl, melt_term, midrh_term, hail, midrh
    REAL    :: tornado, lfc_term, shr_term, midrh2_term, uh_term
    REAL    :: u2000, v2000, us, vs
    REAL    :: wind_vel, p_tot, tcoli
    INTEGER :: nz, ostat
    LOGICAL :: is_target_level
    REAL, DIMENSION( ims:ime, jms:jme ) ::                w_up  &
                                              ,           w_dn  &
                                              ,           llws
                         

    
    
    DO i=ims,ime
      DO j=jms,jme
        is_target_level=.false.
        DO k=kms,kme    
          zagl = z(i,k,j) - ht(i,j)
          IF ( ( zagl >= 3500. ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            midrh = rh(i,k-1,j) + (3500. - (z(i,k-1,j) - ht(i,j))) &
                    * ((rh(i,k,j) - rh(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            IF ( midrh .lt. midrh_min(i,j) ) THEN
              midrh_min(i,j) = midrh
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    
    
    DO j = jts, jte
      DO i = its, ite
        
        wind_vel = uv_wind ( u10(i,j) , v10(i,j) )
        IF ( wind_vel .GT. wspd10max(i,j) ) THEN
          wspd10max(i,j) = wind_vel
        ENDIF
      ENDDO
    ENDDO
 
    
    
    w_up=0.
    w_dn=0.
    DO j = jts, jte
      DO k = k_start, k_end
        DO i = its, ite
          p_tot = p(i,k,j) / 100.
 
          
          
          
          
          
          
          

          
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .GT. w_up(i,j) ) THEN
            w_up(i,j) = w_2(i,k,j)
            IF ( w_up(i,j) .GT. w_up_max(i,j) ) THEN
              w_up_max(i,j) = w_up(i,j)
            ENDIF
          ENDIF
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .LT. w_dn(i,j) ) THEN
            w_dn(i,j) = w_2(i,k,j)
            IF ( w_dn(i,j) .GT. w_dn_max(i,j) ) THEN
              w_dn_max(i,j) = w_dn(i,j)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
   
    
    
    DO j = jts, jte
      DO i = its, ite
        melt_term=max(t2(i,j)-288.15,0.)
        midrh_term=max(2*(min(midrh_min(i,j),midrh_min_old(i,j))-70.),0.)
        hail=max((w_up(i,j)/1.4)**1.25-melt_term-midrh_term,0.)
        IF ( hail .gt. afwa_hail(i,j) ) THEN
          afwa_hail(i,j)=hail
        ENDIF
      ENDDO
    ENDDO

    
    
    
    DO j = jts, jte
      DO i = its, ite
        tcoli=0.
        DO k = k_start, k_end
          tcoli =  tcoli + &
          (qi (i,k,j) + &
           qs (i,k,j) + &
           qg (i,k,j))  &
           *dz8w (i,k,j) * rho(i,k,j)
        ENDDO
        IF ( tcoli .GT. tcoli_max(i,j) ) THEN
          tcoli_max(i,j) = tcoli
        ENDIF
      ENDDO
    ENDDO

    
    
    nz = k_end - k_start
    DO j = jts, jte
      DO i = its, ite
        ostat = Buoyancy (                                   nz &
                                     , t_phy(i,kms:kme      ,j) &
                                     ,    rh(i,kms:kme      ,j) &
                                     ,     p(i,kms:kme      ,j) &
                                     ,     z(i,kms:kme      ,j) &
                                     ,                        1 &
                                     ,                cape(i,j) &
                                     ,                 zlfc_msl &
                                     ,                plfc(i,j) &
                                     ,                        3 ) 
        IF ( ostat /= 0 ) then
          WRITE (*,*) "something went wrong with buoyancy calc at i=",i," j=",j
        ENDIF
        
        
        
        zlfc(i,j)=zlfc_msl-ht(i,j)

      ENDDO
    ENDDO

    
    
    DO j = jts, jte
      DO i = its, ite
        is_target_level=.false.
        DO k=kms,kme    
          zagl = z(i,k,j) - ht(i,j)
          IF ( ( zagl >= 609.6 ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            u2000 = u_phy(i,k-1,j) + (609.6 - (z(i,k-1,j) - ht(i,j))) &
                    * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            v2000 = v_phy(i,k-1,j) + (609.6 - (z(i,k-1,j) - ht(i,j))) &
                    * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            us = u2000 - u10(i,j) 
            vs = v2000 - v10(i,j) 
            llws(i,j) = uv_wind ( us , vs )
            IF ( llws(i,j) .gt. llws_max(i,j) ) THEN
              llws_max(i,j) = llws(i,j)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    
    
    DO j = jts, jte
      DO i = its, ite
        IF ( zlfc(i,j) .ge. 0. ) THEN
          
          uh_term = min(max((uh(i,j) - 25.) / 50., 0.), 1.)
          shr_term = min(max((llws(i,j) - 2.) / 10., 0.), 1.)
          lfc_term = min(max((3000. - zlfc(i,j)) / 1500., 0.), 1.)
          midrh2_term = min(max((90. - min(midrh_min(i,j),midrh_min_old(i,j))) / 30., 0.), 1.)
          tornado = 50. * uh_term * shr_term * lfc_term * midrh2_term
          IF (tornado .gt. afwa_tornado(i,j)) THEN
            afwa_tornado(i,j) = tornado
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    

  END SUBROUTINE severe_wx_diagnostics



  SUBROUTINE vert_int_liquid_diagnostics ( vil                  &
                             , radarvil                         &
                             , t_phy                            &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , dz8w                             &
                             , rho                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                     rho  &
                                              ,          qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  & 
                                              ,          t_phy  &
                                              ,            z_e  & 
                                              ,           dz8w

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                  vil  &
                                              ,       radarvil

    
    
    INTEGER :: i,j,k,ktime

    
    
    
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      vil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        vil (i,j) =  vil (i,j) + &
         (qrain (i,k,j) + &
          qsnow (i,k,j) + &
          qgrpl (i,k,j))  &
          *dz8w (i,k,j) * rho(i,k,j)
      ENDDO
    ENDDO
    ENDDO

    
    
    
    
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      radarvil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        radarvil (i,j) = radarvil (i,j) + &
        0.00344 * z_e(i,k,j)**0.57143 &
        *dz8w (i,k,j)/1000.0
      END DO
    END DO
    END DO

  END SUBROUTINE vert_int_liquid_diagnostics



  SUBROUTINE icing_diagnostics ( icing_opt                      &
                             , fzlev                            &
                             , icing_lg                         &
                             , icing_sm                         & 
                             , qicing_lg_max                    &
                             , qicing_sm_max                    &
                             , qicing_lg                        &
                             , qicing_sm                        &
                             , icingtop                         &
                             , icingbot                         &
                             , t_phy                            &
                             , z                                &
                             , dz8w                             &
                             , rho                              &
                             , qrain                            &
                             , qcloud                           &
                             , ncloud                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, INTENT(IN) :: icing_opt

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                       z  &
                                              ,          qrain  &
                                              ,         qcloud  &
                                              ,         ncloud  &
                                              ,            rho  &
                                              ,           dz8w  &
                                              ,          t_phy

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                fzlev  &
                                              ,       icing_lg  &
                                              ,       icing_sm  &
                                              ,  qicing_lg_max  &
                                              ,  qicing_sm_max  &
                                              ,       icingtop  &
                                              ,       icingbot

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(  OUT) ::                            qicing_lg  &
                                              ,      qicing_sm
         

    
    
    INTEGER :: i,j,k,ktime,ktop,kbot
    REAL    :: qcfrac_lg, qcfrac_sm, qc, qr, small, all

    
    
    fzlev (ips:ipe,jps:jpe) = -999.        
    icingtop (ips:ipe,jps:jpe) = -999.     
    icingbot (ips:ipe,jps:jpe) = -999.     
    icing_lg (ips:ipe,jps:jpe) = 0.        
    icing_sm (ips:ipe,jps:jpe) = 0.
    qicing_lg_max (ips:ipe,jps:jpe) = 0. 
    qicing_sm_max (ips:ipe,jps:jpe) = 0. 
    qicing_sm(ips:ipe,kps:kpe,jps:jpe)=0.
    qicing_lg(ips:ipe,kps:kpe,jps:jpe)=0.   

    
    
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)

      
      
      ktop=-1
      kbot=-1
      DO k = kps, MIN(kpe,kde-1)
        IF (t_phy(i,k,j) .lt. 273.15) THEN

          
          
          
          
          
          qc = qcloud (i,k,j)
          qr = qrain (i,k,j)
          nc = ncloud(i,k,j)
          den = rho(i,k,j)
          qcfrac_lg = 0.
          qcfrac_sm = 0.
          
          
          
          IF (icing_opt .eq. 2) THEN
            IF (qc .lt. 2.5E-4) THEN
              qcfrac_lg = 395000. * qc**2. + 102.9 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 276.1 * qc - 0.01861
            ELSE
              qcfrac_lg = 0.3 * log(641.789 * qc) + 0.4
            ENDIF

          
          
          
          
          
          
          
          
          
          
          
          
          

          
          
          
          ELSEIF ((icing_opt .eq. 3) .OR. (icing_opt .eq. 4)) THEN
            IF (qc .lt. 5.E-4) THEN
              qcfrac_lg = 50420.0 * qc**2. + 29.39 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 97.65 * qc - 0.02152
            ELSE
              qcfrac_lg = 0.2 * log(646.908 * qc) + 0.135
            ENDIF

          
          
          ELSEIF (icing_opt .eq. 5) THEN
            IF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 28000. * qc**2. + 0.1 * qc 
            ELSEIF (qc .lt. 2.6E-3) THEN
              qcfrac_lg = 112.351 * qc - 0.102272
            ELSE 
              qcfrac_lg = 0.3 * log(654.92 * qc) * 0.301607
            ENDIF

          
          
          ELSEIF ((icing_opt .eq. 6) .OR. (icing_opt .eq. 7)) THEN
            IF ((qc .gt. 1.0E-12) .and. (nc .gt. 1.0E-12)) THEN
               small = -nc * exp(-nc*3141.59265*(5.E-5)**3./(6000.*den*qc))+nc
               all = -nc * exp(-nc*3141.59265*(2.)**3./(6000.*den*qc))+nc
               qcfrac_lg = 1. - (small / all)
            ELSE
               qcfac_lg = 0.
            ENDIF
          ENDIF
          qcfrac_lg = max(qcfrac_lg, 0.)
          
          
          
          IF (icing_opt .ne. 0 ) THEN
            qcfrac_sm = 1 - qcfrac_lg
          ENDIF

          
          
          qicing_lg (i,k,j) = max(qr + qcfrac_lg * qc, 0.)
          qicing_sm (i,k,j) = max(qcfrac_sm * qc, 0.)        

          
          
          icing_lg (i,j) = icing_lg (i,j) + qicing_lg (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)
          icing_sm (i,j) = icing_sm (i,j) + qicing_sm (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)

          
          
          IF ( qicing_lg(i,k,j) .gt. qicing_lg_max(i,j) ) THEN
            qicing_lg_max (i,j) = qicing_lg(i,k,j)
          ENDIF
          IF ( qicing_sm(i,k,j) .gt. qicing_sm_max(i,j) ) THEN
            qicing_sm_max (i,j) = qicing_sm(i,k,j)
          ENDIF
           
          
          
          IF (fzlev (i,j) .eq. -999.) THEN  
            IF (k .ne. kps) THEN  
              fzlev (i,j) = z (i,k-1,j) + &
                             ((273.15 - t_phy (i,k-1,j)) &
                            /(t_phy (i,k,j) - t_phy (i,k-1,j))) &
                            *(z (i,k,j) - z (i,k-1,j))
            ELSE  
              fzlev(i,j) = z (i,k,j)
            ENDIF
          ENDIF

          
          
          
          
          IF ((qicing_lg (i,k,j) + qicing_sm (i,k,j)) .ge. 1.E-5) THEN
            IF (kbot .eq. -1) kbot = k  
            ktop=k
          ENDIF
        ENDIF
      END DO

      
      
      
      IF (kbot .ne. -1) THEN
        IF (kbot .ne. kps) THEN 
          icingbot (i,j) = z (i,kbot-1,j) + ((1.E-5 - &
                   (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j))) &
                  / ((qicing_lg (i,kbot,j) + qicing_sm (i,kbot,j)) &
                  - (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j)))) &
                  * (z (i,kbot,j) - z (i,kbot-1,j))
          icingbot (i,j) = MAX(icingbot (i,j), fzlev (i,j))
        ELSE  
          icingbot (i,j) = z(i,kbot,j)
        ENDIF
      ENDIF

      
      
      
      IF (ktop .ne. -1 .and. ktop .ne. kpe) THEN 
        icingtop (i,j) = z (i,ktop,j) + ((1.E-5 - &
                 (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j))) &
                 / ((qicing_lg (i,ktop+1,j) + qicing_sm (i,ktop+1,j)) &
                 - (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j)))) &
                 * (z (i,ktop+1,j) - z (i,ktop,j))
        icingtop (i,j) = MAX(icingtop (i,j), icingbot (i,j))
      ENDIF
    END DO
    END DO

  END SUBROUTINE icing_diagnostics



  SUBROUTINE radar_diagnostics ( refd                           &
                             , refd_com                         &
                             , refd_max                         &
                             , echotop                          &
                             , z                                &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                       z  &
                                              ,            z_e

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                 refd  &
                                              ,       refd_com  &
                                              ,       refd_max  &
                                              ,        echotop

    
    
    INTEGER :: i,j,k,ktime
    
    DO j = jps, MIN(jpe,jde-1)
    DO i = ips, MIN(ipe,ide-1)
      ktop = -1  
      echotop (i,j) = 0.
      refd_com (i,j) = 0.
      refd (i,j) = 0.
      DO k = kps, MIN(kpe,kde-1)
        IF (z_e(i,k,j) .gt. 1.e-20) THEN

          
          
          IF (k == kps) refd(i,j) = MAX(10.0 * log10(z_e(i,k,j)),0.)
   
          
          
          IF (refd(i,j) .gt. refd_max(i,j)) refd_max(i,j) = refd(i,j)

          
          
          IF (10.0 * log10(z_e(i,k,j)) .gt. refd_com(i,j)) THEN
            refd_com(i,j) = 10.0 * log10(z_e(i,k,j))
          ENDIF
        ENDIF
        
        
        
        IF ( z_e(i,k,j) .gt. 63.0957) THEN
          ktop = k
        ENDIF
      END DO
      IF ( ktop .ne. -1 ) THEN  
        echotop (i,j) = z (i,ktop,j) + &
                          ((63.0957 - z_e (i,ktop,j)) &
                         /(z_e (i,ktop+1,j) - z_e (i,ktop,j))) &
                         *(z (i,ktop+1,j) - z (i,ktop,j))
      ENDIF
    END DO
    END DO

  END SUBROUTINE radar_diagnostics



  SUBROUTINE wrf_dbzcalc( rho                                   &
                             , t_phy                            &
                             , qr                               &
                             , qs                               &
                             , qg                               &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                  rho  &
                                              ,          t_phy  &
                                              ,             qr  &
                                              ,             qs  &
                                              ,             qg

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(  OUT) ::                                  z_e

    REAL :: factor_r, factor_s, factor_g, factorb_s, factorb_g, ronv, sonv, gonv
    REAL :: temp_c, rhoair, qgr, qra, qsn
    INTEGER :: i, j, k

    INTEGER, PARAMETER :: iBrightBand = 1
    REAL, PARAMETER :: T_0 = 273.15
    REAL, PARAMETER :: PI = 3.1415926536
    REAL, PARAMETER :: rgas=287.04, gamma_seven = 720.0, alpha2 = 0.224

    
    
    REAL, PARAMETER :: rho_w = 1000.0, rho_r = 1000.0, rho_s = 100.0
    REAL, PARAMETER :: rho_g = 400.0, rho_i = 890.0
    REAL, PARAMETER :: ron=8.e6, son=2.e7, gon=5.e7, r1=1.e-15
    REAL, PARAMETER :: ron_min = 8.e6, ron2=1.e10
    REAL, PARAMETER :: ron_qr0 = 0.0001, ron_delqr0 = 0.25*ron_qr0
    REAL, PARAMETER :: ron_const1r = (ron2-ron_min)*0.5
    REAL, PARAMETER :: ron_const2r = (ron2+ron_min)*0.5

    
    
    ronv = 8.e6    
    sonv = 2.e7    
    gonv = 4.e6    

    factor_r = gamma_seven * 1.e18 * (1./(pi*rho_r))**1.75
    factor_s = gamma_seven * 1.e18 * (1./(pi*rho_s))**1.75  &
              * (rho_s/rho_w)**2 * alpha2
    factor_g = gamma_seven * 1.e18 * (1./(pi*rho_g))**1.75  &
              * (rho_g/rho_w)**2 * alpha2

    
    
    DO j = jps, jpe
    DO k = kps, kpe
    DO i = ips, ipe

      factorb_s = factor_s
      factorb_g = factor_g

      
      
      
      IF( iBrightBand == 1 ) THEN
        IF (t_phy(i,k,j) > T_0) THEN
          factorb_s = factor_s /alpha2
          factorb_g = factor_g /alpha2
        ENDIF
      ENDIF
 
      
      
      temp_c = amin1(-0.001, t_phy(i,k,j)- T_0)
      sonv = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))
      gonv = gon
      qgr = QG(i,k,j)
      qra = QR(i,k,j)
      qsn = QS(i,k,j)
      IF (qgr.gt.r1) THEN
        gonv = 2.38*(pi*rho_g/(rho(i,k,j)*qgr))**0.92
        gonv = max(1.e4, min(gonv,gon))
      ENDIF
      ronv = ron2
      IF (qra.gt. r1) THEN
        ronv = ron_const1r*tanh((ron_qr0-qra)/ron_delqr0) + ron_const2r
      ENDIF
 
      IF (qra < 0.0 ) qra = 0.0
      IF (qsn < 0.0 ) qsn = 0.0
      IF (qgr < 0.0 ) qgr = 0.0
      z_e(i,k,j) = factor_r * (rho(i,k,j) * qra)**1.75 / ronv**.75 + &
                     factorb_s * (rho(i,k,j) * qsn)**1.75 / sonv**.75 + &
                     factorb_g * (rho(i,k,j) * qgr)**1.75 / gonv**.75
 
      IF ( z_e(i,k,j) < 0.0 ) z_e(i,k,j) = 0.0
 
    END DO
    END DO
    END DO

  END SUBROUTINE wrf_dbzcalc



  SUBROUTINE precip_type_diagnostics ( t_phy                    &
                             , t2                               &
                             , rh                               &
                             , z                                &
                             , ht                               &
                             , precip                           &
                             , swdown                           &
                             , rain                             &
                             , snow                             &
                             , ice                              &
                             , frz_rain                         &
                             , snowfall                         &
                             , ccn_tmp                          &
                             , total_melt                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                t_phy  &
                                              ,             rh  &
                                              ,              z
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   t2  &
                                              ,             ht  &
                                              ,         precip  &
                                              ,         swdown
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                             snowfall  &
                                              ,           rain  &
                                              ,       frz_rain  &
                                              ,           snow  &
                                              ,            ice
    REAL, INTENT(IN) :: ccn_tmp
    REAL, INTENT(IN) :: total_melt

    
    
    REAL, DIMENSION( ims:ime, jms:jme ) ::                      &
                                     melt                       &
                                   , mod_2m_tmp                 &
                                   , cloud_top_tmp              &
                                   , maxtmp

    INTEGER, DIMENSION( ims:ime, jms:jme ) ::                   &
                                     cloud_top_k_index          &
                                   , precip_type

    LOGICAL, DIMENSION (ims:ime, jms:jme ) ::                   &
                                     saturation 

    REAL, PARAMETER :: snow_ratio=5.0

    
    
    
    
    
    DO i=ips,ipe
    DO j=jps,jpe
  
      saturation(i,j)=.false.
      melt(i,j)=0.0 
      precip_type(i,j)=0
        
      
      
      
      mod_2m_tmp(i,j)=t2(i,j)+(swdown(i,j)/100.0)
      maxtmp(i,j)=mod_2m_tmp(i,j)
  
      
      
      IF (precip(i,j) .gt. 0.0) THEN
        
        IF (mod_2m_tmp(i,j) .gt. 275.15) THEN
          precip_type(i,j)=1  
        ELSE
  
          
          
          
          
          cloud_top_k_index(i,j)=kpe
          DO k=kpe,kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
              IF (t_phy(i,k,j) .gt. maxtmp(i,j)) THEN
                maxtmp(i,j)=t_phy(i,k,j)
              ENDIF
              IF ( ( rh(i,k,j) .gt. 80 ) .and. & 
                   ( .NOT. saturation(i,j) ) ) THEN
                cloud_top_tmp(i,j)=t_phy(i,k,j)
                cloud_top_k_index(i,j)=k
                saturation(i,j)=.true.
                precip_type(i,j)=2 
              ENDIF
              IF ( ( rh(i,k,j) .le. 70 ) .and. &
                   ( saturation(i,j) ) ) THEN
                saturation(i,j)=.false.
              ENDIF
            ENDIF
          ENDDO

          
          
          
          IF (cloud_top_tmp(i,j) .le. ccn_tmp .and. &
          maxtmp(i,j) .le. 273.15) THEN
            precip_type(i,j)=2  
          ENDIF

          
          
          
          DO k=cloud_top_k_index(i,j),kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
 
              
              
              
              IF (cloud_top_tmp(i,j) .eq. t_phy(i,k,j) .and. &
              cloud_top_tmp(i,j) .gt. ccn_tmp) THEN
                 precip_type(i,j)=1  
              ENDIF

              
              
              
              
              IF ((precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3) .and. &
              t_phy(i,k,j) .gt. 273.15) THEN
                melt(i,j)=melt(i,j)+9.8*(((t_phy(i,k,j)-273.15)/273.15)* &
                          (z(i,k,j)-z(i,k-1,j)))
                IF (melt(i,j) .gt. total_melt) THEN
                  precip_type(i,j)=1  
                  melt(i,j)=0.0  
                ENDIF
              ENDIF

              
              
              
              
              IF (t_phy(i,k,j) .le. 273.15 .and. &
              melt(i,j) .gt. total_melt/4.0 .and. &
              (precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3)) THEN
                precip_type(i,j)=3  
                melt(i,j)=0.0
              ENDIF
             
              
              
              
              IF (precip_type(i,j) .eq. 1) THEN
                IF (t_phy(i,k,j) .le. ccn_tmp) THEN
                  precip_type(i,j)=3  
                ENDIF
              ENDIF
            ENDIF  
          ENDDO  
        ENDIF  

        
        
        IF (precip_type(i,j) .eq. 3) THEN 
          ice(i,j)=ice(i,j)+precip(i,j)
        ENDIF
        IF (precip_type(i,j) .eq. 2) THEN
          snow(i,j)=snow(i,j)+precip(i,j)
          snowfall(i,j)=snowfall(i,j)+snow_ratio*precip(i,j) &
                        *(5.-mod_2m_tmp(i,j)+273.15)**0.5
        ENDIF
        IF (precip_type(i,j) .eq. 1) THEN
          IF (mod_2m_tmp(i,j) .gt. 273.15) THEN
            rain(i,j)=rain(i,j)+precip(i,j)
          ELSE
            frz_rain(i,j)=frz_rain(i,j)+precip(i,j)
          ENDIF
        ENDIF

      ENDIF  

    ENDDO  
    ENDDO  

  END SUBROUTINE precip_type_diagnostics



  SUBROUTINE vis_diagnostics ( qcloud                           &
                             , qrain                            &
                             , qice                             &
                             , qsnow                            &
                             , wind10m                          &
                             , rh2m                             &
                             , dustc                            &
                             , vis                              &
                             , vis_dust                         &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, PARAMETER :: ndust=5

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                               qcloud  &
                                              ,          qrain  &
                                              ,           qice  & 
                                              ,          qsnow  & 
                                              ,        wind10m  & 
                                              ,           rh2m
    REAL, DIMENSION( ims:ime, jms:jme, ndust ),                 &
         INTENT(IN   ) ::                                dustc
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                  vis  &
                                              ,       vis_dust

    
    
    INTEGER :: i,j,k,d
    REAL, PARAMETER :: visfactor=3.912
    REAL, DIMENSION (ndust) :: dustfact
    REAL :: bc, br, bi, bs, dust_extcoeff, hydro_extcoeff, extcoeff, vis_haze

    
    
    
    dustfact=(/1.470E-6,7.877E-7,4.623E-7,2.429E-7,1.387E-7/)

    DO i=ims,ime
      DO j=jms,jme

        
        
        bc=144.7*qcloud(i,j)**0.88
        br=2.240*qrain(i,j)**0.75
        bi=327.8*qice(i,j)
        bs=10.36*qsnow(i,j)**0.78
        hydro_extcoeff=bc+br+bi+bs

        
        
        dust_extcoeff=0.
        DO d=1,ndust
          dust_extcoeff=dust_extcoeff+dustfact(d)*dustc(i,j,d)
        ENDDO

        
        
        vis_haze=1500.*(105.-rh2m(i,j)+wind10m(i,j))
        
        
        
        
        
        extcoeff=hydro_extcoeff+dust_extcoeff
        IF (extcoeff .gt. 0.) THEN
          vis(i,j)=MIN(visfactor/extcoeff,vis_haze)
        ELSE
          vis(i,j)=999999.
        ENDIF

        
        
        
        IF (dust_extcoeff .gt. 0.) THEN
          vis_dust(i,j)=MIN(visfactor/dust_extcoeff,999999.)
        ELSE
          vis_dust(i,j)=999999.
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE vis_diagnostics
  
  

  SUBROUTINE cloud_diagnostics (qcloud                          &
                             , qice                             &
                             , qsnow                            &
                             , rh                               &
                             , dz8w                             &
                             , rho                              &
                             , z                                &
                             , ht                               &
                             , cloud                            &
                             , cloud_ceil                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                               qcloud  &
                                              ,           qice  & 
                                              ,          qsnow  & 
                                              ,             rh  & 
                                              ,           dz8w  & 
                                              ,            rho  &
                                              ,              z

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   ht

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                cloud  &
                                              ,     cloud_ceil

    
    
    INTEGER :: i, j, k
    REAL    :: tot_cld_cond, maxrh, cld_frm_cnd, cld_frm_rh

    
    
    
    DO i=ims,ime
      DO j=jms,jme
        tot_cld_cond = 0.
        maxrh = -9999.
        cloud_ceil(i,j) = -9999.
        DO k=kms,kme

          
          
          tot_cld_cond = tot_cld_cond + (qcloud (i,k,j) + qice (i,k,j) &
                         + qsnow (i,k,j)) * dz8w (i,k,j) * rho(i,k,j)

          
          
          IF (rh (i,k,j) .gt. maxrh) THEN
            maxrh = rh (i,k,j)
          ENDIF
          
          
          
          
          cld_frm_cnd = 50. * tot_cld_cond
          cld_frm_rh = MAX(((maxrh - 70.) / 30.),0.)
          cloud (i,j) = MAX(cld_frm_cnd,cld_frm_rh)

          
          
          
          IF ( cloud_ceil (i,j) .eq. -9999. .and. cloud (i,j) .gt. 0.8 ) THEN
            cloud_ceil (i,j) = z (i,k,j) - ht (i,j)
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE cloud_diagnostics



  
  
  
  
  
  
  
  
  
  
  FUNCTION calc_rh ( p, t, qv ) result ( rh )
    
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: p, t, qv
    REAL :: rh

    
    
    REAL, PARAMETER :: pq0=379.90516
    REAL, PARAMETER :: a2=17.2693882
    REAL, PARAMETER :: a3=273.16
    REAL, PARAMETER :: a4=35.86
    REAL, PARAMETER :: rhmin=1.
    REAL :: q, qs
    INTEGER :: i,j,k
  
    
    
    
      q=qv/(1.0+qv)
      qs=pq0/p*exp(a2*(t-a3)/(t-a4))
      rh=100.*q/qs
      IF (rh .gt. 100.) THEN
        rh=100.
      ELSE IF (rh .lt. rhmin) THEN
        rh=rhmin
      ENDIF

  END FUNCTION calc_rh



  
  
  
  
  
  
  
  
  
  
  FUNCTION uv_wind ( u, v ) result ( wind_speed )
 
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: u, v
    REAL :: wind_speed

    wind_speed = sqrt( u*u + v*v )

  END FUNCTION uv_wind


  
  
  
  
  
  
  
  
  
  
  
  FUNCTION Theta ( t, p )
  IMPLICIT NONE

     
     
     REAL, INTENT ( IN ) :: t
     REAL, INTENT ( IN ) :: p
     REAL                :: theta

     REAL :: Rd 
     REAL :: Cp 
     REAL :: p0 
  
     Rd =  287.04
     Cp = 1004.67
     p0 = 1000.00

     
     
     theta = t * ( (p0/p)**(Rd/Cp) )
  
  END FUNCTION Theta



  
  
  
  
  
  
  
  
  
  
  FUNCTION Thetae ( tK, p, rh, mixr )
  IMPLICIT NONE

     
     
     REAL :: tK        
     REAL :: p         
     REAL :: rh        
     REAL :: mixr      
     REAL :: te        
     REAL :: thetae    
  
     REAL, PARAMETER :: R  = 287.04         
     REAL, PARAMETER :: P0 = 1000.0         
     REAL, PARAMETER :: lv = 2.54*(10**6)   
                                            
     REAL, PARAMETER :: cp = 1004.67        
                                            
     REAL :: tlc                            
  
     
     
     tlc = TLCL ( tK, rh )
  
     
     
     thetae = (tK * (p0/p)**( (R/Cp)*(1.- ( (.28E-3)*mixr*1000.) ) ) )* &
                 exp( (((3.376/tlc)-.00254))*&
                    (mixr*1000.*(1.+(.81E-3)*mixr*1000.)) )
  
  END FUNCTION Thetae



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  FUNCTION The2T ( thetaeK, pres, flag ) result ( tparcel )
  IMPLICIT NONE
  
     
     
     REAL,    INTENT     ( IN ) :: thetaeK
     REAL,    INTENT     ( IN ) :: pres
     LOGICAL, INTENT ( INOUT )  :: flag
     REAL                       :: tparcel
  
     REAL :: thetaK
     REAL :: tovtheta
     REAL :: tcheck
     REAL :: svpr, svpr2
     REAL :: smixr, smixr2
     REAL :: thetae_check, thetae_check2
     REAL :: tguess_2, correction
  
     LOGICAL :: found
     INTEGER :: iter
  
     REAL :: R     
     REAL :: Cp    
     REAL :: kappa 
     REAL :: Lv    
  
     R     = 287.04
     Cp    = 1004.67
     Kappa = R/Cp
     Lv    = 2.500E+6

     
     
     tovtheta = (pres/100000.0)**(r/cp)
     tparcel  = thetaeK/exp(lv*.012/(cp*295.))*tovtheta

     iter = 1
     found = .false.
     flag = .false.

     DO
        IF ( iter > 105 ) EXIT

        tguess_2 = tparcel + REAL ( 1 )

        svpr   = 6.122 * exp ( (17.67*(tparcel-273.15)) / (tparcel-29.66) )
        smixr  = ( 0.622*svpr ) / ( (pres/100.0)-svpr )
        svpr2  = 6.122 * exp ( (17.67*(tguess_2-273.15)) / (tguess_2-29.66) )
        smixr2 = ( 0.622*svpr2 ) / ( (pres/100.0)-svpr2 )

        
        
        
        
        
        
        
        
        
        
        

        
        
        

        
        thetae_check  = Thetae ( tparcel,  pres/100., 100., smixr  )
        thetae_check2 = Thetae ( tguess_2, pres/100., 100., smixr2 )

        
        
        IF ( ABS (thetaeK-thetae_check) < .001) THEN
           found = .true.
           flag  = .true.
           EXIT
        END IF

        
        

        
        correction = ( thetaeK-thetae_check ) / ( thetae_check2-thetae_check )
        tparcel = tparcel + correction

        iter = iter + 1
     END DO

     IF ( .not. found ) THEN
        print*, "Warning! Thetae to temperature calculation did not converge!"
        print*, "Thetae ", thetaeK, "Pressure ", pres
     END IF

  END FUNCTION The2T



  
  
  
  
  
  
  
  
  
  
  FUNCTION VirtualTemperature ( tK, w ) result ( Tv )
  IMPLICIT NONE

     
     real, intent ( in ) :: tK 
     real, intent ( in ) :: w  
     real                :: Tv 

     Tv = tK * ( 1.0 + (w/0.622) ) / ( 1.0 + w )

  END FUNCTION VirtualTemperature




  
  
  
  
  
  
  
  
  
  
  
  
  
  
  FUNCTION SaturationMixingRatio ( tK, p ) result ( ws )

    IMPLICIT NONE

    REAL, INTENT ( IN ) :: tK
    REAL, INTENT ( IN ) :: p
    REAL                :: ws

    REAL :: es

    es = 6.122 * exp ( (17.67*(tK-273.15))/ (tK-29.66) )
    ws = ( 0.622*es ) / ( (p/100.0)-es )

  END FUNCTION SaturationMixingRatio



  
  
  
  
  
  
  
  
  
  
  
  
  
  FUNCTION TLCL ( tk, rh )
    
    IMPLICIT NONE
 
    REAL, INTENT ( IN ) :: tK   
    REAL, INTENT ( IN ) :: rh   
    REAL                :: tlcl
    
    REAL :: denom, term1, term2

    term1 = 1.0 / ( tK - 55.0 )
    IF ( rh > REAL (0) ) THEN
      term2 = ( LOG (rh/100.0)  / 2840.0 )
    ELSE
      term2 = ( LOG (0.001/1.0) / 2840.0 )
    END IF
    denom = term1 - term2
    tlcl = ( 1.0 / denom ) + REAL ( 55 ) 

  END FUNCTION TLCL



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    FUNCTION Buoyancy ( nz, tk, rh, p, hgt, sfc, cape, zlfc, plfc, parcel ) &
               result (ostat)
  
      IMPLICIT NONE
  
      INTEGER, INTENT ( IN )  :: nz          
      INTEGER, INTENT ( IN )  :: sfc         
      REAL,    INTENT ( IN )  :: tk   ( nz ) 
      REAL,    INTENT ( IN )  :: rh   ( nz ) 
      REAL,    INTENT ( IN )  :: p    ( nz ) 
      REAL,    INTENT ( IN )  :: hgt  ( nz ) 
      REAL,    INTENT ( OUT ) :: cape        
      REAL,    INTENT ( OUT ) :: zlfc        
      REAL,    INTENT ( OUT ) :: plfc        
      INTEGER                 :: ostat       
                                             

      INTEGER, INTENT ( IN  ) :: parcel      
                                             
                                             
  
      
      
      REAL                    :: ws   ( nz ) 
      REAL                    :: w    ( nz ) 
      REAL                    :: buoy ( nz ) 
      REAL                    :: tlclK       
      REAL                    :: plcl        
      REAL                    :: nbuoy       
      REAL                    :: pbuoy       
  
      
      
      REAL                    :: srctK       
      REAL                    :: srcrh       
      REAL                    :: srcws       
      REAL                    :: srcw        
      REAL                    :: srcp        
      REAL                    :: srctheta    
      REAL                    :: srcthetaeK  
      INTEGER                 :: srclev      
      INTEGER                 :: sfcoff      
      REAL                    :: spdiff      
   
      
      
      REAL                    :: ptK        
      REAL                    :: ptvK       
      REAL                    :: tvK        
      REAL                    :: pw         
  
      
      
      INTEGER                 :: i, j, k    
      INTEGER                 :: lfclev     
      INTEGER                 :: prcl       
      INTEGER                 :: mlev       
      INTEGER                 :: lyrcnt     
      LOGICAL                 :: flag       
      LOGICAL                 :: wflag      
      REAL                    :: freeze     
      REAL                    :: CIN        
      REAL                    :: pdiff      
  
      
      
      REAL                    :: Rd         
         PARAMETER ( Rd = 287.058 )         
      REAL                    :: Cp         
         PARAMETER ( Cp = 1004.67 )         
      REAL                    :: g          
         PARAMETER ( g  = 9.80665 )         
      REAL                    :: RUNDEF
         PARAMETER ( RUNDEF = -9.999E30 )
  
      
      
      ostat  = 0
      CAPE   = REAL ( 0 )
      ZLFC   = RUNDEF
      PLFC   = RUNDEF
  
      
      
      
      
      
      IF ( parcel > 3 .or. parcel < 1 ) THEN
         
         
         
         prcl = 1
      ELSE
         prcl =  parcel
      END IF
  
      
      
      
      
      
      
      
      
  
      
      
      DO k = sfc, nz
        ws  ( k )   = SaturationMixingRatio ( tK(k), p(k) )
        w   ( k )   = ( rh(k)/100.0 ) * ws ( k )
      END DO
  
      sfcoff=0
      DO k = 2, nz
         spdiff = ( p (1) - p (k) ) / REAL ( 100 )
         IF ( spdiff >= 25. .and. spdiff <= 50. ) THEN
            sfcoff = ( k - 1 )
            EXIT
         END IF
      END DO
   
      sfcoff = 0  
   
      srclev      = sfc+sfcoff
      srctK       = tK    ( sfc+sfcoff )
      srcrh       = rh    ( sfc+sfcoff )
      srcp        = p     ( sfc+sfcoff )
      srcws       = ws    ( sfc+sfcoff )
      srcw        = w     ( sfc+sfcoff )
      srctheta    = Theta ( tK(sfc+sfcoff), p(sfc+sfcoff)/100.0 )
   
      
      
      
      mlev = sfc + 1
      DO k = sfc + 1, nz
   
         
         
         pdiff = ( p (k) - p (sfc) ) / REAL ( 100 )
         IF ( pdiff <= REAL (100) ) mlev = k
   
         IF ( prcl == 1 ) THEN
            IF ( (p(k) > 70000.0) .and. (w(k) > srcw) ) THEN
               srctheta = Theta ( tK(k), p(k)/100.0 )
               srcw = w ( k )
               srclev  = k
               srctK   = tK ( k )
               srcrh   = rh ( k )
               srcp    = p  ( k )
            END IF
         END IF
   
      END DO
   
      
      
      
      lyrcnt =  mlev - sfc + 1
      IF ( prcl == 2 ) THEN
   
         srclev   = sfc
         srctK    = SUM ( tK (sfc:mlev) ) / REAL ( lyrcnt )
         srcw     = SUM ( w  (sfc:mlev) ) / REAL ( lyrcnt )
         srcrh    = SUM ( rh (sfc:mlev) ) / REAL ( lyrcnt )
         srcp     = SUM ( p  (sfc:mlev) ) / REAL ( lyrcnt )
         srctheta = Theta ( srctK, srcp/100. )
   
      END IF
   
      
      
  
  
  
  
  
  
  
  
  
  
  
  
  
   
      srcthetaeK = Thetae ( srctK, srcp/100.0, srcrh, srcw )
   
      
      
  
  
  
  
  
  
  
  
  
   
   
      
      
      tlclK = TLCL ( tK(srclev), rh(srclev) )
      plcl  = p(srclev) * ( (tlclK/tK(srclev))**(Cp/Rd) )
   
      
      
  
  
  
  
  
  
   
   
      buoy  = REAL ( 0 )
      pw    = srcw
      wflag = .false.
      DO k  = srclev, nz
         IF ( tK (k) < 253.15 ) EXIT
         IF ( p (k) <= plcl ) THEN
   
            
            
            
            
            
            
            
   
            IF ( wflag ) THEN
               flag  = .false.
   
               
               
               
               
               
               
               
               ptK   = The2T ( srcthetaeK, p(k), flag )
   
               
               
               
               
               
               pw = SaturationMixingRatio ( ptK, p(k) )
   
               
               
               
               ptvK  = VirtualTemperature ( ptK, pw )
               tvK   = VirtualTemperature ( tK (k), w (k) )
   
               
               
               
   
               
               
               freeze = 0.033 * ( 263.15 - pTvK )
               IF ( freeze > 1.0 ) freeze = 1.0
               IF ( freeze < 0.0 ) freeze = 0.0
   
               
               
               
               freeze = freeze * 333700.0 * ( srcw - pw ) / 1005.7
   
               pTvK = pTvK - pTvK * ( srcw - pw ) + freeze
               buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
            ELSE
   
               
               
               
               
               ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
               
               
               
               ptvK  = VirtualTemperature ( ptK, srcw )
   
               
               
               tvK   = VirtualTemperature ( tK (k), w (k) )
   
               
               
               buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
               wflag = .true.
   
            END IF
   
         ELSE
   
            
            
            
            
            ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
            
            
            
            ptvK  = VirtualTemperature ( ptK, srcw )
   
            
            
            tvK   = VirtualTemperature ( tK (k), w (k) )
   
            
            
            buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
         END IF
   
         
         
  
   
      END DO
   
      
      
      flag   = .false.
      lfclev = -1
      nbuoy  = REAL ( 0 )
      pbuoy = REAL ( 0 )
      DO k = sfc + 1, nz
         IF ( tK (k) < 253.15 ) EXIT
         CAPE = CAPE + MAX ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
         CIN  = CIN  + MIN ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
   
         
         
         IF ( flag .and. buoy (k) > REAL (0) ) THEN
            pbuoy = pbuoy + buoy (k)
         END IF
   
         
         
         IF ( .not. flag .and. buoy (k) > REAL (0) .and. p (k) < plcl ) THEN
            flag = .true.
            pbuoy = pbuoy + buoy (k)
            lfclev = k
         END IF
   
         
         
         
         IF ( flag .and. buoy (k) < REAL (0) ) THEN
            nbuoy = nbuoy + buoy (k)

            
            
            
            
            IF ( ABS (nbuoy) > pbuoy ) THEN
               flag   = .false.
               nbuoy  = REAL ( 0 )
               pbuoy  = REAL ( 0 )
               lfclev = -1
            END IF
         END IF
   
      END DO
   
      
      
      IF ( lfclev > 0 ) THEN
         PLFC = p   ( lfclev )
         ZLFC = hgt ( lfclev )
      END IF
   
      IF ( PLFC /= PLFC .OR. PLFC < REAL (0) ) THEN
         PLFC = REAL ( -1 )
         ZLFC = REAL ( -1 )
      END IF
   
      IF ( CAPE /= CAPE ) cape = REAL ( 0 )
   
      
      
  
  
  
  
  
  
   
  END FUNCTION Buoyancy 

END MODULE module_diag_afwa
