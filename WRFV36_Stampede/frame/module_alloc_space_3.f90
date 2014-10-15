


MODULE module_alloc_space_3
CONTAINS










   SUBROUTINE alloc_space_field_core_3 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , num_bytes_allocated , &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_domain_type
      USE module_configure, ONLY : model_config_rec, grid_config_rec_type, in_use_for_config, model_to_grid_config_rec

      USE module_scalar_tables 

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
 
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in

      INTEGER(KIND=8) , INTENT(INOUT)         :: num_bytes_allocated


      
      INTEGER idum1, idum2, spec_bdy_width
      REAL    initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain
      INTEGER setinitval
      INTEGER sr_x, sr_y

      
      INTEGER ierr

      INTEGER                              :: loop

   

      TYPE ( grid_config_rec_type ) :: config_flags

      INTEGER                         :: k_start , k_end, its, ite, jts, jte
      INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                         ims , ime , jms , jme , kms , kme , &
                                         ips , ipe , jps , jpe , kps , kpe

      INTEGER                         :: sids , side , sjds , sjde , skds , skde , &
                                         sims , sime , sjms , sjme , skms , skme , &
                                         sips , sipe , sjps , sjpe , skps , skpe


      INTEGER ::              imsx, imex, jmsx, jmex, kmsx, kmex,    &
                              ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                              imsy, imey, jmsy, jmey, kmsy, kmey,    &
                              ipsy, ipey, jpsy, jpey, kpsy, kpey

      data_ordering : SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
             ids = sd31 ; ide = ed31 ; jds = sd32 ; jde = ed32 ; kds = sd33 ; kde = ed33 ;
             ims = sm31 ; ime = em31 ; jms = sm32 ; jme = em32 ; kms = sm33 ; kme = em33 ;
             ips = sp31 ; ipe = ep31 ; jps = sp32 ; jpe = ep32 ; kps = sp33 ; kpe = ep33 ;
             imsx = sm31x ; imex = em31x ; jmsx = sm32x ; jmex = em32x ; kmsx = sm33x ; kmex = em33x ;
             ipsx = sp31x ; ipex = ep31x ; jpsx = sp32x ; jpex = ep32x ; kpsx = sp33x ; kpex = ep33x ;
             imsy = sm31y ; imey = em31y ; jmsy = sm32y ; jmey = em32y ; kmsy = sm33y ; kmey = em33y ;
             ipsy = sp31y ; ipey = ep31y ; jpsy = sp32y ; jpey = ep32y ; kpsy = sp33y ; kpey = ep33y ;
         CASE  ( DATA_ORDER_YXZ )
             ids = sd32  ; ide = ed32  ; jds = sd31  ; jde = ed31  ; kds = sd33  ; kde = ed33  ;
             ims = sm32  ; ime = em32  ; jms = sm31  ; jme = em31  ; kms = sm33  ; kme = em33  ;
             ips = sp32  ; ipe = ep32  ; jps = sp31  ; jpe = ep31  ; kps = sp33  ; kpe = ep33  ;
             imsx = sm32x  ; imex = em32x  ; jmsx = sm31x  ; jmex = em31x  ; kmsx = sm33x  ; kmex = em33x  ;
             ipsx = sp32x  ; ipex = ep32x  ; jpsx = sp31x  ; jpex = ep31x  ; kpsx = sp33x  ; kpex = ep33x  ;
             imsy = sm32y  ; imey = em32y  ; jmsy = sm31y  ; jmey = em31y  ; kmsy = sm33y  ; kmey = em33y  ;
             ipsy = sp32y  ; ipey = ep32y  ; jpsy = sp31y  ; jpey = ep31y  ; kpsy = sp33y  ; kpey = ep33y  ;
         CASE  ( DATA_ORDER_ZXY )
             ids = sd32  ; ide = ed32  ; jds = sd33  ; jde = ed33  ; kds = sd31  ; kde = ed31  ;
             ims = sm32  ; ime = em32  ; jms = sm33  ; jme = em33  ; kms = sm31  ; kme = em31  ;
             ips = sp32  ; ipe = ep32  ; jps = sp33  ; jpe = ep33  ; kps = sp31  ; kpe = ep31  ;
             imsx = sm32x  ; imex = em32x  ; jmsx = sm33x  ; jmex = em33x  ; kmsx = sm31x  ; kmex = em31x  ;
             ipsx = sp32x  ; ipex = ep32x  ; jpsx = sp33x  ; jpex = ep33x  ; kpsx = sp31x  ; kpex = ep31x  ;
             imsy = sm32y  ; imey = em32y  ; jmsy = sm33y  ; jmey = em33y  ; kmsy = sm31y  ; kmey = em31y  ;
             ipsy = sp32y  ; ipey = ep32y  ; jpsy = sp33y  ; jpey = ep33y  ; kpsy = sp31y  ; kpey = ep31y  ;
         CASE  ( DATA_ORDER_ZYX )
             ids = sd33  ; ide = ed33  ; jds = sd32  ; jde = ed32  ; kds = sd31  ; kde = ed31  ;
             ims = sm33  ; ime = em33  ; jms = sm32  ; jme = em32  ; kms = sm31  ; kme = em31  ;
             ips = sp33  ; ipe = ep33  ; jps = sp32  ; jpe = ep32  ; kps = sp31  ; kpe = ep31  ;
             imsx = sm33x  ; imex = em33x  ; jmsx = sm32x  ; jmex = em32x  ; kmsx = sm31x  ; kmex = em31x  ;
             ipsx = sp33x  ; ipex = ep33x  ; jpsx = sp32x  ; jpex = ep32x  ; kpsx = sp31x  ; kpex = ep31x  ;
             imsy = sm33y  ; imey = em33y  ; jmsy = sm32y  ; jmey = em32y  ; kmsy = sm31y  ; kmey = em31y  ;
             ipsy = sp33y  ; ipey = ep33y  ; jpsy = sp32y  ; jpey = ep32y  ; kpsy = sp31y  ; kpey = ep31y  ;
         CASE  ( DATA_ORDER_XZY )
             ids = sd31  ; ide = ed31  ; jds = sd33  ; jde = ed33  ; kds = sd32  ; kde = ed32  ;
             ims = sm31  ; ime = em31  ; jms = sm33  ; jme = em33  ; kms = sm32  ; kme = em32  ;
             ips = sp31  ; ipe = ep31  ; jps = sp33  ; jpe = ep33  ; kps = sp32  ; kpe = ep32  ;
             imsx = sm31x  ; imex = em31x  ; jmsx = sm33x  ; jmex = em33x  ; kmsx = sm32x  ; kmex = em32x  ;
             ipsx = sp31x  ; ipex = ep31x  ; jpsx = sp33x  ; jpex = ep33x  ; kpsx = sp32x  ; kpex = ep32x  ;
             imsy = sm31y  ; imey = em31y  ; jmsy = sm33y  ; jmey = em33y  ; kmsy = sm32y  ; kmey = em32y  ;
             ipsy = sp31y  ; ipey = ep31y  ; jpsy = sp33y  ; jpey = ep33y  ; kpsy = sp32y  ; kpey = ep32y  ;
         CASE  ( DATA_ORDER_YZX )
             ids = sd33  ; ide = ed33  ; jds = sd31  ; jde = ed31  ; kds = sd32  ; kde = ed32  ;
             ims = sm33  ; ime = em33  ; jms = sm31  ; jme = em31  ; kms = sm32  ; kme = em32  ;
             ips = sp33  ; ipe = ep33  ; jps = sp31  ; jpe = ep31  ; kps = sp32  ; kpe = ep32  ;
             imsx = sm33x  ; imex = em33x  ; jmsx = sm31x  ; jmex = em31x  ; kmsx = sm32x  ; kmex = em32x  ;
             ipsx = sp33x  ; ipex = ep33x  ; jpsx = sp31x  ; jpex = ep31x  ; kpsx = sp32x  ; kpex = ep32x  ;
             imsy = sm33y  ; imey = em33y  ; jmsy = sm31y  ; jmey = em31y  ; kmsy = sm32y  ; kmey = em32y  ;
             ipsy = sp33y  ; ipey = ep33y  ; jpsy = sp31y  ; jpey = ep31y  ; kpsy = sp32y  ; kpey = ep32y  ;
      END SELECT data_ordering

      CALL model_to_grid_config_rec ( id , model_config_rec , config_flags )

      CALL nl_get_sr_x( id , sr_x )
      CALL nl_get_sr_y( id , sr_y )

      tl = tl_in
      inter_domain = inter_domain_in

      CALL get_initial_data_value ( initial_data_value )

      setinitval = setinitval_in

      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )







IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%ts_buf_size=0
IF ( setinitval .EQ. 3 ) grid%max_ts_locs=0
IF ( setinitval .EQ. 3 ) grid%vortex_interval=0
IF ( setinitval .EQ. 3 ) grid%max_vortex_speed=0
IF ( setinitval .EQ. 3 ) grid%corral_dist=0
IF ( setinitval .EQ. 3 ) grid%track_level=0
IF ( setinitval .EQ. 3 ) grid%time_to_move=initial_data_value
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%swap_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%swap_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%reorder_mesh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%perturb_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%eta_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%max_dz=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ocean_levels=0
IF ( setinitval .EQ. 3 ) grid%ocean_z=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ocean_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ocean_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_traj=0
IF ( setinitval .EQ. 3 ) grid%max_ts_level=0
IF ( setinitval .EQ. 3 ) grid%track_loc_in=0
IF ( setinitval .EQ. 3 ) grid%num_ext_model_couple_dom=0
IF ( setinitval .EQ. 3 ) grid%insert_bogus_storm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%remove_storm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%num_storm=0
IF ( setinitval .EQ. 3 ) grid%latc_loc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lonc_loc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vmax_meters_per_second=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rmax=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vmax_ratio=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rankine_lid=initial_data_value
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%nssl_cccn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_alphah=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_alphahl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnoh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnohl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnor=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnos=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_rho_qh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_rho_qhl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_rho_qs=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsfcgce_hail=0
IF ( setinitval .EQ. 3 ) grid%gsfcgce_2ice=0
IF ( setinitval .EQ. 3 ) grid%progn=0
IF ( setinitval .EQ. 3 ) grid%accum_mode=initial_data_value
IF ( setinitval .EQ. 3 ) grid%aitken_mode=initial_data_value
IF ( setinitval .EQ. 3 ) grid%coarse_mode=initial_data_value
IF ( setinitval .EQ. 3 ) grid%do_radar_ref=0
IF ( setinitval .EQ. 3 ) grid%ra_lw_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_sw_physics=0
IF ( setinitval .EQ. 3 ) grid%radt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%naer=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_sfclay_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_pbl_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_mynn_tkebudget=0
IF ( setinitval .EQ. 3 ) grid%bl_mynn_tkeadvect=.FALSE.
IF ( setinitval .EQ. 3 ) grid%bl_mynn_cloudpdf=0
IF ( setinitval .EQ. 3 ) grid%mfshconv=0
IF ( setinitval .EQ. 3 ) grid%sf_urban_physics=0
IF ( setinitval .EQ. 3 ) grid%bldt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_physics=0
IF ( setinitval .EQ. 3 ) grid%shcu_physics=0
IF ( setinitval .EQ. 3 ) grid%cu_diag=0
IF ( setinitval .EQ. 3 ) grid%kfeta_trigger=0
IF ( setinitval .EQ. 3 ) grid%nsas_dx_factor=0
IF ( setinitval .EQ. 3 ) grid%cudt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsmdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isfflx=0
IF ( setinitval .EQ. 3 ) grid%ifsnow=0
IF ( setinitval .EQ. 3 ) grid%icloud=0
IF ( setinitval .EQ. 3 ) grid%swrad_scat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%surface_input_source=0
IF ( setinitval .EQ. 3 ) grid%num_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%maxpatch=0
IF ( setinitval .EQ. 3 ) grid%num_snow_layers=0
IF ( setinitval .EQ. 3 ) grid%num_snso_layers=0
IF ( setinitval .EQ. 3 ) grid%num_urban_layers=0
IF ( setinitval .EQ. 3 ) grid%num_urban_hi=0
IF ( setinitval .EQ. 3 ) grid%num_months=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_mosaic=0
IF ( setinitval .EQ. 3 ) grid%mosaic_cat=0
IF ( setinitval .EQ. 3 ) grid%mosaic_cat_soil=0
IF ( setinitval .EQ. 3 ) grid%mosaic_lu=0
IF ( setinitval .EQ. 3 ) grid%mosaic_soil=0
IF ( setinitval .EQ. 3 ) grid%maxiens=0
IF ( setinitval .EQ. 3 ) grid%maxens=0
IF ( setinitval .EQ. 3 ) grid%maxens2=0
IF ( setinitval .EQ. 3 ) grid%maxens3=0
IF ( setinitval .EQ. 3 ) grid%ensdim=0
IF ( setinitval .EQ. 3 ) grid%cugd_avedx=0
IF ( setinitval .EQ. 3 ) grid%clos_choice=0
IF ( setinitval .EQ. 3 ) grid%imomentum=0
IF ( setinitval .EQ. 3 ) grid%ishallow=0
IF ( setinitval .EQ. 3 ) grid%convtrans_avglen_m=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_land_cat=0
IF ( setinitval .EQ. 3 ) grid%num_soil_cat=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out_thresh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_threshold=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sst_update=0
IF ( setinitval .EQ. 3 ) grid%sst_skin=0
IF ( setinitval .EQ. 3 ) grid%tmn_update=0
IF ( setinitval .EQ. 3 ) grid%usemonalb=.FALSE.
IF ( setinitval .EQ. 3 ) grid%rdmaxalb=.FALSE.
IF ( setinitval .EQ. 3 ) grid%rdlai2d=.FALSE.
IF ( setinitval .EQ. 3 ) grid%ua_phys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%co2tf=0
IF ( setinitval .EQ. 3 ) grid%ra_call_offset=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_freq_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%levsiz=0
IF ( setinitval .EQ. 3 ) grid%paerlev=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim1=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim2=0
IF ( setinitval .EQ. 3 ) grid%lagday=0
IF ( setinitval .EQ. 3 ) grid%no_src_types=0
IF ( setinitval .EQ. 3 ) grid%alevsiz=0
IF ( setinitval .EQ. 3 ) grid%o3input=0
IF ( setinitval .EQ. 3 ) grid%aer_opt=0
IF ( setinitval .EQ. 3 ) grid%swint_opt=0
IF ( setinitval .EQ. 3 ) grid%aer_type=0
IF ( setinitval .EQ. 3 ) grid%aer_aod550_opt=0
IF ( setinitval .EQ. 3 ) grid%aer_angexp_opt=0
IF ( setinitval .EQ. 3 ) grid%aer_ssa_opt=0
IF ( setinitval .EQ. 3 ) grid%aer_asy_opt=0
IF ( setinitval .EQ. 3 ) grid%aer_aod550_val=initial_data_value
IF ( setinitval .EQ. 3 ) grid%aer_angexp_val=initial_data_value
IF ( setinitval .EQ. 3 ) grid%aer_ssa_val=initial_data_value
IF ( setinitval .EQ. 3 ) grid%aer_asy_val=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_rad_feedback=.FALSE.
IF ( setinitval .EQ. 3 ) grid%icloud_cu=0
IF ( setinitval .EQ. 3 ) grid%pxlsm_smois_init=0
IF ( setinitval .EQ. 3 ) grid%omlcall=0
IF ( setinitval .EQ. 3 ) grid%sf_ocean_physics=0
IF ( setinitval .EQ. 3 ) grid%traj_opt=0
IF ( setinitval .EQ. 3 ) grid%tracercall=0
IF ( setinitval .EQ. 3 ) grid%omdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%oml_hml0=initial_data_value
IF ( setinitval .EQ. 3 ) grid%oml_gamma=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isftcflx=0
IF ( setinitval .EQ. 3 ) grid%iz0tlnd=0
IF ( setinitval .EQ. 3 ) grid%shadlen=initial_data_value
IF ( setinitval .EQ. 3 ) grid%slope_rad=0
IF ( setinitval .EQ. 3 ) grid%topo_shading=0
IF ( setinitval .EQ. 3 ) grid%topo_wind=0
IF ( setinitval .EQ. 3 ) grid%no_mp_heating=0
IF ( setinitval .EQ. 3 ) grid%fractional_seaice=0
IF ( setinitval .EQ. 3 ) grid%seaice_snowdepth_opt=0
IF ( setinitval .EQ. 3 ) grid%seaice_snowdepth_max=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_snowdepth_min=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_albedo_opt=0
IF ( setinitval .EQ. 3 ) grid%seaice_albedo_default=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_thickness_opt=0
IF ( setinitval .EQ. 3 ) grid%seaice_thickness_default=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tice2tsk_if2cold=.FALSE.
IF ( setinitval .EQ. 3 ) grid%bucket_mm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%bucket_j=initial_data_value
IF ( setinitval .EQ. 3 ) grid%mp_tend_lim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%prec_acc_dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%prec_acc_opt=0
IF ( setinitval .EQ. 3 ) grid%bucketr_opt=0
IF ( setinitval .EQ. 3 ) grid%process_time_series=0
IF ( setinitval .EQ. 3 ) grid%grav_settling=0
IF ( setinitval .EQ. 3 ) grid%sas_pgcon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%scalar_pblmix=0
IF ( setinitval .EQ. 3 ) grid%tracer_pblmix=0
IF ( setinitval .EQ. 3 ) grid%use_aero_icbc=.FALSE.
IF ( setinitval .EQ. 3 ) grid%afwa_diag_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_ptype_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_vil_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_radar_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_severe_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_icing_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_vis_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_cloud_opt=0
IF ( setinitval .EQ. 3 ) grid%afwa_ptype_ccn_tmp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%afwa_ptype_tot_melt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%afwa_ccn_conc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%afwa_hail_opt=0
IF ( setinitval .EQ. 3 ) grid%dveg=0
IF ( setinitval .EQ. 3 ) grid%opt_crs=0
IF ( setinitval .EQ. 3 ) grid%opt_btr=0
IF ( setinitval .EQ. 3 ) grid%opt_run=0
IF ( setinitval .EQ. 3 ) grid%opt_sfc=0
IF ( setinitval .EQ. 3 ) grid%opt_frz=0
IF ( setinitval .EQ. 3 ) grid%opt_inf=0
IF ( setinitval .EQ. 3 ) grid%opt_rad=0
IF ( setinitval .EQ. 3 ) grid%opt_alb=0
IF ( setinitval .EQ. 3 ) grid%opt_snf=0
IF ( setinitval .EQ. 3 ) grid%opt_tbot=0
IF ( setinitval .EQ. 3 ) grid%opt_stc=0
IF ( setinitval .EQ. 3 ) grid%wtddt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%wrf_hydro=0
IF ( setinitval .EQ. 3 ) grid%fgdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fgdtzero=0
IF ( setinitval .EQ. 3 ) grid%grid_fdda=0
IF ( setinitval .EQ. 3 ) grid%grid_sfdda=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_uv=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_t=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_ph=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_q=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_ph=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_ph=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_q=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_q=0
IF ( setinitval .EQ. 3 ) grid%dk_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%dk_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%dk_zfac_ph=0
IF ( setinitval .EQ. 3 ) grid%guv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%guv_sfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gt_sfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gq_sfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gph=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtramp_min=initial_data_value
IF ( setinitval .EQ. 3 ) grid%if_ramping=0
IF ( setinitval .EQ. 3 ) grid%rinblw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%xwavenum=0
IF ( setinitval .EQ. 3 ) grid%ywavenum=0
IF ( setinitval .EQ. 3 ) grid%pxlsm_soil_nudge=0
IF ( setinitval .EQ. 3 ) grid%obs_nudge_opt=0
IF ( setinitval .EQ. 3 ) grid%max_obs=0
IF ( setinitval .EQ. 3 ) grid%fdda_start=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdda_end=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_wind=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_wind=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_temp=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_mois=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_mois=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_pstr=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_pstr=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_no_pbl_nudge_uv=0
IF ( setinitval .EQ. 3 ) grid%obs_no_pbl_nudge_t=0
IF ( setinitval .EQ. 3 ) grid%obs_no_pbl_nudge_q=0
IF ( setinitval .EQ. 3 ) grid%obs_sfc_scheme_horiz=0
IF ( setinitval .EQ. 3 ) grid%obs_sfc_scheme_vert=0
IF ( setinitval .EQ. 3 ) grid%obs_max_sndng_gap=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr1_uv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr1_uv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr2_uv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr2_uv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr4_uv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr4_uv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr1_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr1_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr2_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr2_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr4_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr4_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr1_q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr1_q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr2_q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr2_q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullr4_q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampr4_q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezfullmin=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezrampmin=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudgezmax=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_sfcfact=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_sfcfacr=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_dpsmx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_rinxy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_rinsig=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_twindo=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_npfi=0
IF ( setinitval .EQ. 3 ) grid%obs_ionf=0
IF ( setinitval .EQ. 3 ) grid%obs_idynin=0
IF ( setinitval .EQ. 3 ) grid%obs_dtramp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_prt_max=0
IF ( setinitval .EQ. 3 ) grid%obs_prt_freq=0
IF ( setinitval .EQ. 3 ) grid%obs_ipf_in4dob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_errob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_nudob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_init=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_scl_neg_qv_innov=0
IF ( setinitval .EQ. 3 ) grid%scm_force=0
IF ( setinitval .EQ. 3 ) grid%scm_force_dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_force_layers=0
IF ( setinitval .EQ. 3 ) grid%scm_lu_index=0
IF ( setinitval .EQ. 3 ) grid%scm_isltyp=0
IF ( setinitval .EQ. 3 ) grid%scm_vegfra=initial_data_value
IF ( setinitval .EQ. 3 ) grid%scm_canwat=0
IF ( setinitval .EQ. 3 ) grid%scm_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%scm_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%scm_th_t_tend=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_qv_t_tend=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_th_adv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_wind_adv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_qv_adv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_ql_adv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_vert_adv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%num_force_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%scm_soilt_force=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_soilq_force=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_force_th_largescale=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_force_qv_largescale=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_force_ql_largescale=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_force_wind_largescale=.FALSE.
IF ( setinitval .EQ. 3 ) grid%scm_force_skintemp=0
IF ( setinitval .EQ. 3 ) grid%scm_force_flux=0
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%diff_opt_dfi=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt_dfi=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%rad_nudge=0
IF ( setinitval .EQ. 3 ) grid%gwd_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%diff_6th_factor=initial_data_value
IF ( setinitval .EQ. 3 ) grid%diff_6th_opt=0
IF ( setinitval .EQ. 3 ) grid%c_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%c_k=initial_data_value
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%use_input_w=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%momentum_adv_opt=0
IF ( setinitval .EQ. 3 ) grid%moist_adv_opt=0
IF ( setinitval .EQ. 3 ) grid%moist_adv_dfi_opt=0
IF ( setinitval .EQ. 3 ) grid%chem_adv_opt=0
IF ( setinitval .EQ. 3 ) grid%tracer_adv_opt=0
IF ( setinitval .EQ. 3 ) grid%scalar_adv_opt=0
IF ( setinitval .EQ. 3 ) grid%tke_adv_opt=0
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_isotropic=0
IF ( setinitval .EQ. 3 ) grid%mix_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%top_lid=.FALSE.
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%coriolis2d=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_full_fields=.FALSE.
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iso_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%use_baseparam_fr_nml=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fft_filter_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rotated_pole=.FALSE.
IF ( setinitval .EQ. 3 ) grid%do_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%do_curvature=.FALSE.
IF ( setinitval .EQ. 3 ) grid%do_gradp=.FALSE.
IF ( setinitval .EQ. 3 ) grid%tracer_opt=0
IF ( setinitval .EQ. 3 ) grid%tenddiag=0
IF ( setinitval .EQ. 3 ) grid%spec_bdy_width=0
IF ( setinitval .EQ. 3 ) grid%spec_zone=0
IF ( setinitval .EQ. 3 ) grid%relax_zone=0
IF ( setinitval .EQ. 3 ) grid%specified=.FALSE.
IF ( setinitval .EQ. 3 ) grid%constant_bc=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%polar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nested=.FALSE.
IF ( setinitval .EQ. 3 ) grid%spec_exp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%real_data_init_type=0
IF ( setinitval .EQ. 3 ) grid%have_bcs_moist=.FALSE.
IF ( setinitval .EQ. 3 ) grid%have_bcs_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%background_proc_id=0
IF ( setinitval .EQ. 3 ) grid%forecast_proc_id=0
IF ( setinitval .EQ. 3 ) grid%production_status=0
IF ( setinitval .EQ. 3 ) grid%compression=0
IF ( setinitval .EQ. 3 ) grid%nobs_ndg_vars=0
IF ( setinitval .EQ. 3 ) grid%nobs_err_flds=0
IF ( setinitval .EQ. 3 ) grid%cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cen_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stand_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pole_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pole_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%flag_metgrid=0
IF ( setinitval .EQ. 3 ) grid%flag_snow=0
IF ( setinitval .EQ. 3 ) grid%flag_psfc=0
IF ( setinitval .EQ. 3 ) grid%flag_sm000010=0
IF ( setinitval .EQ. 3 ) grid%flag_sm010040=0
IF ( setinitval .EQ. 3 ) grid%flag_sm040100=0
IF ( setinitval .EQ. 3 ) grid%flag_sm100200=0
IF ( setinitval .EQ. 3 ) grid%flag_st000010=0
IF ( setinitval .EQ. 3 ) grid%flag_st010040=0
IF ( setinitval .EQ. 3 ) grid%flag_st040100=0
IF ( setinitval .EQ. 3 ) grid%flag_st100200=0
IF ( setinitval .EQ. 3 ) grid%flag_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%flag_slp=0
IF ( setinitval .EQ. 3 ) grid%flag_soilhgt=0
IF ( setinitval .EQ. 3 ) grid%flag_mf_xy=0
IF ( setinitval .EQ. 3 ) grid%bdyfrq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iswater=0
IF ( setinitval .EQ. 3 ) grid%islake=0
IF ( setinitval .EQ. 3 ) grid%isice=0
IF ( setinitval .EQ. 3 ) grid%isurban=0
IF ( setinitval .EQ. 3 ) grid%isoilwater=0
IF ( setinitval .EQ. 3 ) grid%map_proj=0
IF ( setinitval .EQ. 3 ) grid%use_wps_input=0
IF ( setinitval .EQ. 3 ) grid%dfi_stage=0
IF ( setinitval .EQ. 3 ) grid%mp_physics_dfi=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'nodyn_dummy'
   grid%tail_statevars%DataName = 'NODYN_DUMMY'
   grid%tail_statevars%Description = ''
   grid%tail_statevars%Units = ''
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%nodyn_dummy
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%nodyn_dummy=0
IF ( setinitval .EQ. 3 ) grid%windfarm_opt=0
IF ( setinitval .EQ. 3 ) grid%windfarm_ij=0
IF(in_use_for_config(id,'ic_flashcount').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ic_flashcount(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",619,&
    'frame/module_domain.f: Failed to allocate grid%ic_flashcount(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ic_flashcount=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ic_flashcount'
  grid%tail_statevars%DataName = 'IC_FLASHCOUNT'
  grid%tail_statevars%Description = 'Accumulated IC flash count'
  grid%tail_statevars%Units = ' '
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ic_flashcount
  grid%tail_statevars%streams(1) = 1 
  grid%tail_statevars%streams(2) = 2097152 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ic_flashcount(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",669,&
    'frame/module_domain.f: Failed to allocate grid%ic_flashcount(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ic_flashrate').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ic_flashrate(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",678,&
    'frame/module_domain.f: Failed to allocate grid%ic_flashrate(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ic_flashrate=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ic_flashrate'
  grid%tail_statevars%DataName = 'IC_FLASHRATE'
  grid%tail_statevars%Description = 'IC flash rate'
  grid%tail_statevars%Units = ' /s'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ic_flashrate
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 2097152 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ic_flashrate(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",728,&
    'frame/module_domain.f: Failed to allocate grid%ic_flashrate(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cg_flashcount').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cg_flashcount(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",737,&
    'frame/module_domain.f: Failed to allocate grid%cg_flashcount(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cg_flashcount=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cg_flashcount'
  grid%tail_statevars%DataName = 'CG_FLASHCOUNT'
  grid%tail_statevars%Description = 'Accumulated CG flash count'
  grid%tail_statevars%Units = ' '
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cg_flashcount
  grid%tail_statevars%streams(1) = 1 
  grid%tail_statevars%streams(2) = 2097152 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cg_flashcount(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",787,&
    'frame/module_domain.f: Failed to allocate grid%cg_flashcount(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cg_flashrate').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cg_flashrate(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",796,&
    'frame/module_domain.f: Failed to allocate grid%cg_flashrate(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cg_flashrate=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cg_flashrate'
  grid%tail_statevars%DataName = 'CG_FLASHRATE'
  grid%tail_statevars%Description = 'CG flash rate'
  grid%tail_statevars%Units = ' /s'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cg_flashrate
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 2097152 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cg_flashrate(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",846,&
    'frame/module_domain.f: Failed to allocate grid%cg_flashrate(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%lightning_option=0
IF ( setinitval .EQ. 3 ) grid%lightning_dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lightning_start_seconds=initial_data_value
IF ( setinitval .EQ. 3 ) grid%flashrate_factor=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iccg_method=0
IF ( setinitval .EQ. 3 ) grid%iccg_prescribed_num=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iccg_prescribed_den=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cellcount_method=0
IF ( setinitval .EQ. 3 ) grid%cldtop_adjustment=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_lake_physics=0
IF(in_use_for_config(id,'iccg_in_num').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%iccg_in_num(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",865,&
    'frame/module_domain.f: Failed to allocate grid%iccg_in_num(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iccg_in_num=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'iccg_in_num'
  grid%tail_statevars%DataName = 'ICCG_IN_NUM'
  grid%tail_statevars%Description = 'IC:CG input numerator'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%iccg_in_num
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 2098176 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%iccg_in_num(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",915,&
    'frame/module_domain.f: Failed to allocate grid%iccg_in_num(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iccg_in_den').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%iccg_in_den(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",924,&
    'frame/module_domain.f: Failed to allocate grid%iccg_in_den(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iccg_in_den=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'iccg_in_den'
  grid%tail_statevars%DataName = 'ICCG_IN_DEN'
  grid%tail_statevars%Description = 'IC:CG input denominator'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%iccg_in_den
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 2098176 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%iccg_in_den(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",974,&
    'frame/module_domain.f: Failed to allocate grid%iccg_in_den(1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'domain_tot'
   grid%tail_statevars%DataName = 'DOMAIN_TOT'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%domain_tot
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%domain_tot=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'ieodi'
   grid%tail_statevars%DataName = 'IEODI'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%ieodi
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%ieodi=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'iwtsig'
   grid%tail_statevars%DataName = 'IWTSIG'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%iwtsig
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%iwtsig=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'nstat'
   grid%tail_statevars%DataName = 'NSTAT'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%nstat
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%nstat=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'nstaw'
   grid%tail_statevars%DataName = 'NSTAW'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%nstaw
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%nstaw=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'ktaur'
   grid%tail_statevars%DataName = 'KTAUR'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%ktaur
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%ktaur=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'levidn(max_domains)'
   grid%tail_statevars%DataName = 'LEVIDN(MAX_DOMAINS)'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%levidn(max_domains)
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%levidn(max_domains)=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'refprt(max_domains)'
   grid%tail_statevars%DataName = 'REFPRT(MAX_DOMAINS)'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%refprt(max_domains)
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%refprt(max_domains)=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'window'
   grid%tail_statevars%DataName = 'WINDOW'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%window
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%window=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'rtlast'
   grid%tail_statevars%DataName = 'RTLAST'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%rtlast
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%rtlast=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'datend'
   grid%tail_statevars%DataName = 'DATEND'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%datend
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%datend=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'nudge_uv_pbl'
   grid%tail_statevars%DataName = 'NUDGE_UV_PBL'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'l'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%lfield_0d => grid%fdob%nudge_uv_pbl
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%nudge_uv_pbl=.FALSE.
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'nudge_t_pbl'
   grid%tail_statevars%DataName = 'NUDGE_T_PBL'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'l'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%lfield_0d => grid%fdob%nudge_t_pbl
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%nudge_t_pbl=.FALSE.
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'nudge_q_pbl'
   grid%tail_statevars%DataName = 'NUDGE_Q_PBL'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'l'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%lfield_0d => grid%fdob%nudge_q_pbl
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%nudge_q_pbl=.FALSE.
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'sfc_scheme_horiz'
   grid%tail_statevars%DataName = 'SFC_SCHEME_HORIZ'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%sfc_scheme_horiz
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%sfc_scheme_horiz=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'sfc_scheme_vert'
   grid%tail_statevars%DataName = 'SFC_SCHEME_VERT'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%ifield_0d => grid%fdob%sfc_scheme_vert
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%sfc_scheme_vert=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'max_sndng_gap'
   grid%tail_statevars%DataName = 'MAX_SNDNG_GAP'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%max_sndng_gap
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%max_sndng_gap=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'sfcfact'
   grid%tail_statevars%DataName = 'SFCFACT'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%sfcfact
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%sfcfact=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'sfcfacr'
   grid%tail_statevars%DataName = 'SFCFACR'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%sfcfacr
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%sfcfacr=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'rinfmn'
   grid%tail_statevars%DataName = 'RINFMN'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%rinfmn
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%rinfmn=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'rinfmx'
   grid%tail_statevars%DataName = 'RINFMX'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%rinfmx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%rinfmx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'pfree'
   grid%tail_statevars%DataName = 'PFREE'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%pfree
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%pfree=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'dcon'
   grid%tail_statevars%DataName = 'DCON'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%dcon
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%dcon=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'dpsmx'
   grid%tail_statevars%DataName = 'DPSMX'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%dpsmx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%dpsmx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'tfaci'
   grid%tail_statevars%DataName = 'TFACI'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%tfaci
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%tfaci=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'known_lat'
   grid%tail_statevars%DataName = 'KNOWN_LAT'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%known_lat
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%known_lat=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'known_lon'
   grid%tail_statevars%DataName = 'KNOWN_LON'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%known_lon
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%known_lon=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'xtime_at_rest'
   grid%tail_statevars%DataName = 'XTIME_AT_REST'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%xtime_at_rest
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%xtime_at_rest=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'vif_uv(6)'
   grid%tail_statevars%DataName = 'VIF_UV(6)'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%vif_uv(6)
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%vif_uv(6)=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'vif_t(6)'
   grid%tail_statevars%DataName = 'VIF_T(6)'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%vif_t(6)
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%vif_t(6)=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'vif_q(6)'
   grid%tail_statevars%DataName = 'VIF_Q(6)'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%vif_q(6)
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%vif_q(6)=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'vif_fullmin'
   grid%tail_statevars%DataName = 'VIF_FULLMIN'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%vif_fullmin
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%vif_fullmin=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'vif_rampmin'
   grid%tail_statevars%DataName = 'VIF_RAMPMIN'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%vif_rampmin
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%vif_rampmin=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient    = '  '
   grid%tail_statevars%VarName = 'vif_max'
   grid%tail_statevars%DataName = 'VIF_MAX'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type    = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart  = .FALSE.
   grid%tail_statevars%Ndim    = 0
   grid%tail_statevars%scalar_array  = .FALSE. 
   grid%tail_statevars%rfield_0d => grid%fdob%vif_max
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  ENDIF
IF ( setinitval .EQ. 3 ) grid%fdob%vif_max=initial_data_value
IF(in_use_for_config(id,'varobs').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%nobs_ndg_vars)-(1)+1))*(((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%varobs(1:model_config_rec%nobs_ndg_vars,1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1629,&
    'frame/module_domain.f: Failed to allocate grid%fdob%varobs(1:model_config_rec%nobs_ndg_vars,1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%varobs=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'varobs'
  grid%tail_statevars%DataName = 'VAROBS'
  grid%tail_statevars%Description = 'observational values in each variable'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'CC'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fdob%varobs
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%nobs_ndg_vars
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%max_obs
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%nobs_ndg_vars
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%max_obs
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%nobs_ndg_vars
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%max_obs
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%varobs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1677,&
    'frame/module_domain.f: Failed to allocate grid%fdob%varobs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'errf').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%nobs_err_flds)-(1)+1))*(((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%errf(1:model_config_rec%nobs_err_flds,1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1686,&
    'frame/module_domain.f: Failed to allocate grid%fdob%errf(1:model_config_rec%nobs_err_flds,1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%errf=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'errf'
  grid%tail_statevars%DataName = 'ERRF'
  grid%tail_statevars%Description = 'errors between model and obs values'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'CC'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fdob%errf
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%nobs_err_flds
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%max_obs
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%nobs_err_flds
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%max_obs
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%nobs_err_flds
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%max_obs
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%errf(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1734,&
    'frame/module_domain.f: Failed to allocate grid%fdob%errf(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'timeob').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%timeob(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1743,&
    'frame/module_domain.f: Failed to allocate grid%fdob%timeob(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%timeob=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'timeob'
  grid%tail_statevars%DataName = 'TIMEOB'
  grid%tail_statevars%Description = 'model times for each observation'
  grid%tail_statevars%Units = 'hours'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%timeob
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%timeob(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1791,&
    'frame/module_domain.f: Failed to allocate grid%fdob%timeob(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'nlevs_ob').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%nlevs_ob(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1800,&
    'frame/module_domain.f: Failed to allocate grid%fdob%nlevs_ob(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%nlevs_ob=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'nlevs_ob'
  grid%tail_statevars%DataName = 'NLEVS_OB'
  grid%tail_statevars%Description = 'numbers of levels in sounding obs'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%nlevs_ob
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%nlevs_ob(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1848,&
    'frame/module_domain.f: Failed to allocate grid%fdob%nlevs_ob(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lev_in_ob').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%lev_in_ob(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1857,&
    'frame/module_domain.f: Failed to allocate grid%fdob%lev_in_ob(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%lev_in_ob=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'lev_in_ob'
  grid%tail_statevars%DataName = 'LEV_IN_OB'
  grid%tail_statevars%Description = 'level in sounding-type obs'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%lev_in_ob
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%lev_in_ob(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1905,&
    'frame/module_domain.f: Failed to allocate grid%fdob%lev_in_ob(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'plfo').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%plfo(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1914,&
    'frame/module_domain.f: Failed to allocate grid%fdob%plfo(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%plfo=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'plfo'
  grid%tail_statevars%DataName = 'PLFO'
  grid%tail_statevars%Description = 'index for type of obs-platform'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%plfo
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%plfo(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1962,&
    'frame/module_domain.f: Failed to allocate grid%fdob%plfo(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'elevob').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%elevob(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1971,&
    'frame/module_domain.f: Failed to allocate grid%fdob%elevob(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%elevob=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'elevob'
  grid%tail_statevars%DataName = 'ELEVOB'
  grid%tail_statevars%Description = 'elevation of observation'
  grid%tail_statevars%Units = 'meters'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%elevob
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%elevob(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2019,&
    'frame/module_domain.f: Failed to allocate grid%fdob%elevob(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rio').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%rio(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2028,&
    'frame/module_domain.f: Failed to allocate grid%fdob%rio(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rio=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rio'
  grid%tail_statevars%DataName = 'RIO'
  grid%tail_statevars%Description = 'west-east grid coordinate'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%rio
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%rio(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2076,&
    'frame/module_domain.f: Failed to allocate grid%fdob%rio(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rjo').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%rjo(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2085,&
    'frame/module_domain.f: Failed to allocate grid%fdob%rjo(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rjo=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rjo'
  grid%tail_statevars%DataName = 'RJO'
  grid%tail_statevars%Description = 'south-north grid coordinate'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%rjo
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%rjo(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2133,&
    'frame/module_domain.f: Failed to allocate grid%fdob%rjo(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rko').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%max_obs)-(1)+1))) * 4
  ALLOCATE(grid%fdob%rko(1:model_config_rec%max_obs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2142,&
    'frame/module_domain.f: Failed to allocate grid%fdob%rko(1:model_config_rec%max_obs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rko=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rko'
  grid%tail_statevars%DataName = 'RKO'
  grid%tail_statevars%Description = 'vertical grid coordinate'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%rko
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%max_obs
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%max_obs
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%max_obs
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%rko(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2190,&
    'frame/module_domain.f: Failed to allocate grid%fdob%rko(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'obsprt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%obs_prt_max)-(1)+1))) * 4
  ALLOCATE(grid%fdob%obsprt(1:model_config_rec%obs_prt_max),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2199,&
    'frame/module_domain.f: Failed to allocate grid%fdob%obsprt(1:model_config_rec%obs_prt_max). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%obsprt=0
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'obsprt'
  grid%tail_statevars%DataName = 'OBSPRT'
  grid%tail_statevars%Description = 'obs index for diagnostic printout'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'i'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%ifield_1d => grid%fdob%obsprt
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%obs_prt_max
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%obs_prt_max
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%obs_prt_max
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%obsprt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2247,&
    'frame/module_domain.f: Failed to allocate grid%fdob%obsprt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'latprt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%obs_prt_max)-(1)+1))) * 4
  ALLOCATE(grid%fdob%latprt(1:model_config_rec%obs_prt_max),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2256,&
    'frame/module_domain.f: Failed to allocate grid%fdob%latprt(1:model_config_rec%obs_prt_max). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%latprt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'latprt'
  grid%tail_statevars%DataName = 'LATPRT'
  grid%tail_statevars%Description = 'obs latitude for diagnostic printout'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%latprt
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%obs_prt_max
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%obs_prt_max
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%obs_prt_max
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%latprt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2304,&
    'frame/module_domain.f: Failed to allocate grid%fdob%latprt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lonprt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%obs_prt_max)-(1)+1))) * 4
  ALLOCATE(grid%fdob%lonprt(1:model_config_rec%obs_prt_max),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2313,&
    'frame/module_domain.f: Failed to allocate grid%fdob%lonprt(1:model_config_rec%obs_prt_max). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%lonprt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'lonprt'
  grid%tail_statevars%DataName = 'LONPRT'
  grid%tail_statevars%Description = 'obs longitude for diagnostic printout'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%lonprt
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%obs_prt_max
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%obs_prt_max
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%obs_prt_max
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%lonprt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2361,&
    'frame/module_domain.f: Failed to allocate grid%fdob%lonprt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mlatprt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%obs_prt_max)-(1)+1))) * 4
  ALLOCATE(grid%fdob%mlatprt(1:model_config_rec%obs_prt_max),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2370,&
    'frame/module_domain.f: Failed to allocate grid%fdob%mlatprt(1:model_config_rec%obs_prt_max). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%mlatprt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mlatprt'
  grid%tail_statevars%DataName = 'MLATPRT'
  grid%tail_statevars%Description = 'model latitude at obs location'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%mlatprt
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%obs_prt_max
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%obs_prt_max
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%obs_prt_max
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%mlatprt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2418,&
    'frame/module_domain.f: Failed to allocate grid%fdob%mlatprt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mlonprt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%obs_prt_max)-(1)+1))) * 4
  ALLOCATE(grid%fdob%mlonprt(1:model_config_rec%obs_prt_max),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2427,&
    'frame/module_domain.f: Failed to allocate grid%fdob%mlonprt(1:model_config_rec%obs_prt_max). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%mlonprt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mlonprt'
  grid%tail_statevars%DataName = 'MLONPRT'
  grid%tail_statevars%Description = 'model longitude at obs location'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'C'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%mlonprt
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%obs_prt_max
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%obs_prt_max
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%obs_prt_max
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%mlonprt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2475,&
    'frame/module_domain.f: Failed to allocate grid%fdob%mlonprt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'stnidprt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((40)-(1)+1))*(((model_config_rec%obs_prt_max)-(1)+1))) * 4
  ALLOCATE(grid%fdob%stnidprt(1:40,1:model_config_rec%obs_prt_max),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2484,&
    'frame/module_domain.f: Failed to allocate grid%fdob%stnidprt(1:40,1:model_config_rec%obs_prt_max). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%stnidprt=0
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'stnidprt'
  grid%tail_statevars%DataName = 'STNIDPRT'
  grid%tail_statevars%Description = 'obs station id for diagnostic printout'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'i'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'CC'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%ifield_2d => grid%fdob%stnidprt
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = 40
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%obs_prt_max
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = 40
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%obs_prt_max
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = 40
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%obs_prt_max
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%stnidprt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2532,&
    'frame/module_domain.f: Failed to allocate grid%fdob%stnidprt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'base_state').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%fdob%base_state(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2541,&
    'frame/module_domain.f: Failed to allocate grid%fdob%base_state(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%base_state=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'base_state'
  grid%tail_statevars%DataName = 'BASE_STATE'
  grid%tail_statevars%Description = 'base-state height on half (mass) levels'
  grid%tail_statevars%Units = 'meters'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'Z'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 1
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%fdob%base_state
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = kds
  grid%tail_statevars%ed1 = (kde-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = kms
  grid%tail_statevars%em1 = kme
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = kps
  grid%tail_statevars%ep1 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = 'bottom_top'
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fdob%base_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2589,&
    'frame/module_domain.f: Failed to allocate grid%fdob%base_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%t_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2598,&
    'frame/module_domain.f: Failed to allocate grid%t_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't_xxx'
  grid%tail_statevars%DataName = 'T_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%t_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%t_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2648,&
    'frame/module_domain.f: Failed to allocate grid%t_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%u_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2657,&
    'frame/module_domain.f: Failed to allocate grid%u_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'u_xxx'
  grid%tail_statevars%DataName = 'U_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'X'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%u_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( ide, ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%u_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2707,&
    'frame/module_domain.f: Failed to allocate grid%u_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ru_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%ru_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2716,&
    'frame/module_domain.f: Failed to allocate grid%ru_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ru_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ru_xxx'
  grid%tail_statevars%DataName = 'RU_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'X'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%ru_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( ide, ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%ru_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2766,&
    'frame/module_domain.f: Failed to allocate grid%ru_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%v_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2775,&
    'frame/module_domain.f: Failed to allocate grid%v_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'v_xxx'
  grid%tail_statevars%DataName = 'V_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'Y'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%v_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = jde
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( jde, jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north_stag'
  ENDIF
ELSE
  ALLOCATE(grid%v_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2825,&
    'frame/module_domain.f: Failed to allocate grid%v_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rv_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%rv_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2834,&
    'frame/module_domain.f: Failed to allocate grid%rv_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rv_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rv_xxx'
  grid%tail_statevars%DataName = 'RV_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'Y'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rv_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = jde
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( jde, jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north_stag'
  ENDIF
ELSE
  ALLOCATE(grid%rv_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2884,&
    'frame/module_domain.f: Failed to allocate grid%rv_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%w_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2893,&
    'frame/module_domain.f: Failed to allocate grid%w_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'w_xxx'
  grid%tail_statevars%DataName = 'W_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'Z'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%w_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( kde, kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%w_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2943,&
    'frame/module_domain.f: Failed to allocate grid%w_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ww_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%ww_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2952,&
    'frame/module_domain.f: Failed to allocate grid%ww_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ww_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ww_xxx'
  grid%tail_statevars%DataName = 'WW_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'Z'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%ww_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( kde, kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%ww_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3002,&
    'frame/module_domain.f: Failed to allocate grid%ww_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ph_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%ph_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3011,&
    'frame/module_domain.f: Failed to allocate grid%ph_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ph_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ph_xxx'
  grid%tail_statevars%DataName = 'PH_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = 'Z'
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%ph_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( kde, kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%ph_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3061,&
    'frame/module_domain.f: Failed to allocate grid%ph_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dum_yyy').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31y)-(sm31y)+1))*(((em32y)-(sm32y)+1))*(((em33y)-(sm33y)+1))) * 4
  ALLOCATE(grid%dum_yyy(sm31y:em31y,sm32y:em32y,sm33y:em33y),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3070,&
    'frame/module_domain.f: Failed to allocate grid%dum_yyy(sm31y:em31y,sm32y:em32y,sm33y:em33y). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dum_yyy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dum_yyy'
  grid%tail_statevars%DataName = 'DUM_YYY'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'Y'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%dum_yyy
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsy
  grid%tail_statevars%em1 = imey
  grid%tail_statevars%sm2 = kmsy
  grid%tail_statevars%em2 = kmey
  grid%tail_statevars%sm3 = jmsy
  grid%tail_statevars%em3 = jmey
  grid%tail_statevars%sp1 = ipsy
  grid%tail_statevars%ep1 = MIN( (ide-1), ipey )
  grid%tail_statevars%sp2 = kpsy
  grid%tail_statevars%ep2 = MIN( (kde-1), kpey )
  grid%tail_statevars%sp3 = jpsy
  grid%tail_statevars%ep3 = MIN( (jde-1), jpey )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%dum_yyy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3120,&
    'frame/module_domain.f: Failed to allocate grid%dum_yyy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fourd_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%fourd_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3129,&
    'frame/module_domain.f: Failed to allocate grid%fourd_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fourd_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fourd_xxx'
  grid%tail_statevars%DataName = 'FOURD_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%fourd_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%fourd_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3179,&
    'frame/module_domain.f: Failed to allocate grid%fourd_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'clat_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%clat_xxx(sm31x:em31x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3188,&
    'frame/module_domain.f: Failed to allocate grid%clat_xxx(sm31x:em31x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%clat_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'clat_xxx'
  grid%tail_statevars%DataName = 'CLAT_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%clat_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = jmsx
  grid%tail_statevars%em2 = jmex
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = jpsx
  grid%tail_statevars%ep2 = MIN( (jde-1), jpex )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%clat_xxx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3238,&
    'frame/module_domain.f: Failed to allocate grid%clat_xxx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ht_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%ht_xxx(sm31x:em31x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3247,&
    'frame/module_domain.f: Failed to allocate grid%ht_xxx(sm31x:em31x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ht_xxx'
  grid%tail_statevars%DataName = 'HT_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ht_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = jmsx
  grid%tail_statevars%em2 = jmex
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = jpsx
  grid%tail_statevars%ep2 = MIN( (jde-1), jpex )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ht_xxx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3297,&
    'frame/module_domain.f: Failed to allocate grid%ht_xxx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mf_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%mf_xxx(sm31x:em31x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3306,&
    'frame/module_domain.f: Failed to allocate grid%mf_xxx(sm31x:em31x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mf_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mf_xxx'
  grid%tail_statevars%DataName = 'MF_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 2
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%mf_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = jmsx
  grid%tail_statevars%em2 = jmex
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = jpsx
  grid%tail_statevars%ep2 = MIN( (jde-1), jpex )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%mf_xxx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3356,&
    'frame/module_domain.f: Failed to allocate grid%mf_xxx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dif_analysis').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dif_analysis(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3365,&
    'frame/module_domain.f: Failed to allocate grid%dif_analysis(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dif_analysis=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dif_analysis'
  grid%tail_statevars%DataName = 'DIF_ANALYSIS'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = ' '
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%dif_analysis
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%dif_analysis(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3415,&
    'frame/module_domain.f: Failed to allocate grid%dif_analysis(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dif_xxx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31x)-(sm31x)+1))*(((em32x)-(sm32x)+1))*(((em33x)-(sm33x)+1))) * 4
  ALLOCATE(grid%dif_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3424,&
    'frame/module_domain.f: Failed to allocate grid%dif_xxx(sm31x:em31x,sm32x:em32x,sm33x:em33x). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dif_xxx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dif_xxx'
  grid%tail_statevars%DataName = 'DIF_XXX'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'X'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%dif_xxx
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsx
  grid%tail_statevars%em1 = imex
  grid%tail_statevars%sm2 = kmsx
  grid%tail_statevars%em2 = kmex
  grid%tail_statevars%sm3 = jmsx
  grid%tail_statevars%em3 = jmex
  grid%tail_statevars%sp1 = ipsx
  grid%tail_statevars%ep1 = MIN( (ide-1), ipex )
  grid%tail_statevars%sp2 = kpsx
  grid%tail_statevars%ep2 = MIN( (kde-1), kpex )
  grid%tail_statevars%sp3 = jpsx
  grid%tail_statevars%ep3 = MIN( (jde-1), jpex )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%dif_xxx(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3474,&
    'frame/module_domain.f: Failed to allocate grid%dif_xxx(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dif_yyy').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31y)-(sm31y)+1))*(((em32y)-(sm32y)+1))*(((em33y)-(sm33y)+1))) * 4
  ALLOCATE(grid%dif_yyy(sm31y:em31y,sm32y:em32y,sm33y:em33y),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3483,&
    'frame/module_domain.f: Failed to allocate grid%dif_yyy(sm31y:em31y,sm32y:em32y,sm33y:em33y). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dif_yyy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dif_yyy'
  grid%tail_statevars%DataName = 'DIF_YYY'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type    = 'r'
  grid%tail_statevars%ProcOrient    = 'Y'
  grid%tail_statevars%MemoryOrder  = 'XZY'
  grid%tail_statevars%Stagger      = ''
  grid%tail_statevars%Ntl     = 0
  grid%tail_statevars%Ndim    = 3
  grid%tail_statevars%Restart  = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%dif_yyy
  grid%tail_statevars%streams(1) = 0 
  grid%tail_statevars%streams(2) = 0 
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = imsy
  grid%tail_statevars%em1 = imey
  grid%tail_statevars%sm2 = kmsy
  grid%tail_statevars%em2 = kmey
  grid%tail_statevars%sm3 = jmsy
  grid%tail_statevars%em3 = jmey
  grid%tail_statevars%sp1 = ipsy
  grid%tail_statevars%ep1 = MIN( (ide-1), ipey )
  grid%tail_statevars%sp2 = kpsy
  grid%tail_statevars%ep2 = MIN( (kde-1), kpey )
  grid%tail_statevars%sp3 = jpsy
  grid%tail_statevars%ep3 = MIN( (jde-1), jpey )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%dif_yyy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3533,&
    'frame/module_domain.f: Failed to allocate grid%dif_yyy(1,1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%override_restart_timers=.FALSE.
IF ( setinitval .EQ. 3 ) grid%auxhist1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist12_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist12=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist12=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist13_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist13=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist13=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist14_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist14=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist14=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist15_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist15=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist15=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist16_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist16=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist16=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist17_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist17=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist17=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist18_end=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist18=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist18=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_begin=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist19_end=0


   END SUBROUTINE alloc_space_field_core_3

END MODULE module_alloc_space_3

