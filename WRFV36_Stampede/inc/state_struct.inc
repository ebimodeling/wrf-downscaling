!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_struct.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real                                     :: hfx_force
real                                     :: lh_force
real                                     :: tsk_force
real                                     :: hfx_force_tend
real                                     :: lh_force_tend
real                                     :: tsk_force_tend
real                                     :: cfn
real                                     :: cfn1
integer                                  :: step_number
logical                                  :: this_is_an_ideal_run
logical                                  :: stepping_to_time
integer                                  :: last_step_updated
logical                                  :: adapt_step_using_child
integer                                  :: last_dt_sec
integer                                  :: last_dt_sec_num
integer                                  :: last_dt_sec_den
integer                                  :: last_dt_yr
integer                                  :: last_dt_mm
real                                     :: rdx
real                                     :: rdy
real                                     :: dts
real                                     :: dtseps
real                                     :: resm
real                                     :: zetatop
real                                     :: cf1
real                                     :: cf2
real                                     :: cf3
integer                                  :: number_at_same_level
real                                     :: radtacttime
real                                     :: bldtacttime
real                                     :: cudtacttime
integer                                  :: itimestep
real                                     :: xtime
real                                     :: julian
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
logical                                  :: press_adj
real                                     :: xi
real                                     :: xj
real                                     :: vc_i
real                                     :: vc_j
integer                                  :: has_reqc
integer                                  :: has_reqi
integer                                  :: has_reqs
real                                     :: dtbc
integer                                  :: ifndsnowh
integer                                  :: ifndsoilw
integer                                  :: ifndalbsi
integer                                  :: ifndsnowsi
integer                                  :: ifndicedepth
real                                     :: hcoeff_tot
real                                     :: declin
real                                     :: solcon
integer                                  :: min_ptchsz
real                                     :: u_frame
real                                     :: v_frame
logical                                  :: just_read_auxinput4
real                                     :: p_top
real                                     :: t00
real                                     :: p00
real                                     :: tlp
real                                     :: tiso
real                                     :: max_msftx
real                                     :: max_msfty
integer                                  :: stepave_count
integer                                  :: stepcu
integer                                  :: stepra
integer                                  :: stepwtd
integer                                  :: landuse_isice
integer                                  :: landuse_lucats
integer                                  :: landuse_luseas
integer                                  :: landuse_isn
integer                                  :: nyear
real                                     :: nday
integer                                  :: stepbl
logical                                  :: warm_rain
logical                                  :: adv_moist_cond
integer                                  :: save_topo_from_real
integer                                  :: stepfg
logical                                  :: moved
real                                     :: max_cfl
integer                                  :: run_days
integer                                  :: run_hours
integer                                  :: run_minutes
integer                                  :: run_seconds
integer                                  :: start_year
integer                                  :: start_month
integer                                  :: start_day
integer                                  :: start_hour
integer                                  :: start_minute
integer                                  :: start_second
integer                                  :: end_year
integer                                  :: end_month
integer                                  :: end_day
integer                                  :: end_hour
integer                                  :: end_minute
integer                                  :: end_second
integer                                  :: interval_seconds
logical                                  :: input_from_file
integer                                  :: fine_input_stream
logical                                  :: input_from_hires
character*256                               :: rsmas_data_path
logical                                  :: all_ic_times
integer                                  :: julyr
integer                                  :: julday
real                                     :: gmt
character*256                               :: input_inname
character*256                               :: input_outname
character*256                               :: bdy_inname
character*256                               :: bdy_outname
character*256                               :: rst_inname
character*256                               :: rst_outname
logical                                  :: write_input
logical                                  :: write_restart_at_0h
logical                                  :: write_hist_at_0h_rst
logical                                  :: adjust_output_times
logical                                  :: adjust_input_times
integer                                  :: diag_print
logical                                  :: nocolons
logical                                  :: cycling
integer                                  :: output_diagnostics
integer                                  :: nwp_diagnostics
integer                                  :: dfi_opt
integer                                  :: dfi_savehydmeteors
integer                                  :: dfi_nfilter
logical                                  :: dfi_write_filtered_input
logical                                  :: dfi_write_dfi_history
integer                                  :: dfi_cutoff_seconds
integer                                  :: dfi_time_dim
integer                                  :: dfi_fwdstop_year
integer                                  :: dfi_fwdstop_month
integer                                  :: dfi_fwdstop_day
integer                                  :: dfi_fwdstop_hour
integer                                  :: dfi_fwdstop_minute
integer                                  :: dfi_fwdstop_second
integer                                  :: dfi_bckstop_year
integer                                  :: dfi_bckstop_month
integer                                  :: dfi_bckstop_day
integer                                  :: dfi_bckstop_hour
integer                                  :: dfi_bckstop_minute
integer                                  :: dfi_bckstop_second
integer                                  :: time_step
integer                                  :: time_step_fract_num
integer                                  :: time_step_fract_den
integer                                  :: time_step_dfi
integer                                  :: min_time_step
integer                                  :: min_time_step_den
integer                                  :: max_time_step
integer                                  :: max_time_step_den
real                                     :: target_cfl
real                                     :: target_hcfl
integer                                  :: max_step_increase_pct
integer                                  :: starting_time_step
integer                                  :: starting_time_step_den
logical                                  :: step_to_output_time
integer                                  :: adaptation_domain
logical                                  :: use_adaptive_time_step
logical                                  :: use_adaptive_time_step_dfi
integer                                  :: max_dom
integer                                  :: lats_to_mic
integer                                  :: s_we
integer                                  :: e_we
integer                                  :: s_sn
integer                                  :: e_sn
integer                                  :: s_vert
integer                                  :: e_vert
integer                                  :: num_metgrid_levels
integer                                  :: num_metgrid_soil_levels
real                                     :: p_top_requested
logical                                  :: interp_theta
integer                                  :: interp_type
integer                                  :: vert_refine_fact
integer                                  :: extrap_type
integer                                  :: t_extrap_type
integer                                  :: hypsometric_opt
logical                                  :: lowest_lev_from_sfc
logical                                  :: use_levels_below_ground
logical                                  :: use_tavg_for_tsk
logical                                  :: use_surface
integer                                  :: lagrange_order
integer                                  :: force_sfc_in_vinterp
real                                     :: zap_close_levels
logical                                  :: sfcp_to_sfcp
logical                                  :: adjust_heights
logical                                  :: smooth_cg_topo
integer                                  :: nest_interp_coord
logical                                  :: aggregate_lu
logical                                  :: rh2qv_wrt_liquid
integer                                  :: rh2qv_method
real                                     :: qv_max_p_safe
real                                     :: qv_max_flag
real                                     :: qv_max_value
real                                     :: qv_min_p_safe
real                                     :: qv_min_flag
real                                     :: qv_min_value
real                                     :: dx
real                                     :: dy
integer                                  :: grid_id
logical                                  :: grid_allowed
integer                                  :: parent_id
integer                                  :: i_parent_start
integer                                  :: j_parent_start
integer                                  :: parent_grid_ratio
integer                                  :: parent_time_step_ratio
integer                                  :: feedback
integer                                  :: smooth_option
integer                                  :: blend_width
real                                     :: ztop
integer                                  :: moad_grid_ratio
integer                                  :: moad_time_step_ratio
integer                                  :: shw
integer                                  :: tile_sz_x
integer                                  :: tile_sz_y
integer                                  :: numtiles
integer                                  :: numtiles_inc
integer                                  :: numtiles_x
integer                                  :: numtiles_y
integer                                  :: tile_strategy
integer                                  :: nproc_x
integer                                  :: nproc_y
integer                                  :: irand
real                                     :: dt
integer                                  :: num_moves
integer                                  :: ts_buf_size
integer                                  :: max_ts_locs
integer                                  :: vortex_interval
integer                                  :: max_vortex_speed
integer                                  :: corral_dist
integer                                  :: track_level
real                                     :: time_to_move
integer                                  :: move_id
integer                                  :: move_interval
integer                                  :: move_cd_x
integer                                  :: move_cd_y
logical                                  :: swap_x
logical                                  :: swap_y
logical                                  :: cycle_x
logical                                  :: cycle_y
logical                                  :: reorder_mesh
logical                                  :: perturb_input
real                                     :: eta_levels
real                                     :: max_dz
integer                                  :: ocean_levels
real                                     :: ocean_z
real                                     :: ocean_t
real                                     :: ocean_s
integer                                  :: num_traj
integer                                  :: max_ts_level
integer                                  :: track_loc_in
integer                                  :: num_ext_model_couple_dom
logical                                  :: insert_bogus_storm
logical                                  :: remove_storm
integer                                  :: num_storm
real                                     :: latc_loc
real                                     :: lonc_loc
real                                     :: vmax_meters_per_second
real                                     :: rmax
real                                     :: vmax_ratio
real                                     :: rankine_lid
integer                                  :: mp_physics
real                                     :: nssl_cccn
real                                     :: nssl_alphah
real                                     :: nssl_alphahl
real                                     :: nssl_cnoh
real                                     :: nssl_cnohl
real                                     :: nssl_cnor
real                                     :: nssl_cnos
real                                     :: nssl_rho_qh
real                                     :: nssl_rho_qhl
real                                     :: nssl_rho_qs
integer                                  :: gsfcgce_hail
integer                                  :: gsfcgce_2ice
integer                                  :: progn
real                                     :: accum_mode
real                                     :: aitken_mode
real                                     :: coarse_mode
integer                                  :: do_radar_ref
integer                                  :: ra_lw_physics
integer                                  :: ra_sw_physics
real                                     :: radt
real                                     :: naer
integer                                  :: sf_sfclay_physics
integer                                  :: sf_surface_physics
integer                                  :: bl_pbl_physics
integer                                  :: bl_mynn_tkebudget
logical                                  :: bl_mynn_tkeadvect
integer                                  :: bl_mynn_cloudpdf
integer                                  :: mfshconv
integer                                  :: sf_urban_physics
real                                     :: bldt
integer                                  :: cu_physics
integer                                  :: shcu_physics
integer                                  :: cu_diag
integer                                  :: kfeta_trigger
integer                                  :: nsas_dx_factor
real                                     :: cudt
real                                     :: gsmdt
integer                                  :: isfflx
integer                                  :: ifsnow
integer                                  :: icloud
real                                     :: swrad_scat
integer                                  :: surface_input_source
integer                                  :: num_soil_layers
integer                                  :: maxpatch
integer                                  :: num_snow_layers
integer                                  :: num_snso_layers
integer                                  :: num_urban_layers
integer                                  :: num_urban_hi
integer                                  :: num_months
integer                                  :: sf_surface_mosaic
integer                                  :: mosaic_cat
integer                                  :: mosaic_cat_soil
integer                                  :: mosaic_lu
integer                                  :: mosaic_soil
integer                                  :: maxiens
integer                                  :: maxens
integer                                  :: maxens2
integer                                  :: maxens3
integer                                  :: ensdim
integer                                  :: cugd_avedx
integer                                  :: clos_choice
integer                                  :: imomentum
integer                                  :: ishallow
real                                     :: convtrans_avglen_m
integer                                  :: num_land_cat
integer                                  :: num_soil_cat
integer                                  :: mp_zero_out
real                                     :: mp_zero_out_thresh
real                                     :: seaice_threshold
integer                                  :: sst_update
integer                                  :: sst_skin
integer                                  :: tmn_update
logical                                  :: usemonalb
logical                                  :: rdmaxalb
logical                                  :: rdlai2d
logical                                  :: ua_phys
integer                                  :: co2tf
integer                                  :: ra_call_offset
real                                     :: cam_abs_freq_s
integer                                  :: levsiz
integer                                  :: paerlev
integer                                  :: cam_abs_dim1
integer                                  :: cam_abs_dim2
integer                                  :: lagday
integer                                  :: no_src_types
integer                                  :: alevsiz
integer                                  :: o3input
integer                                  :: aer_opt
integer                                  :: swint_opt
integer                                  :: aer_type
integer                                  :: aer_aod550_opt
integer                                  :: aer_angexp_opt
integer                                  :: aer_ssa_opt
integer                                  :: aer_asy_opt
real                                     :: aer_aod550_val
real                                     :: aer_angexp_val
real                                     :: aer_ssa_val
real                                     :: aer_asy_val
logical                                  :: cu_rad_feedback
integer                                  :: icloud_cu
integer                                  :: pxlsm_smois_init
integer                                  :: omlcall
integer                                  :: sf_ocean_physics
integer                                  :: traj_opt
integer                                  :: tracercall
real                                     :: omdt
real                                     :: oml_hml0
real                                     :: oml_gamma
integer                                  :: isftcflx
integer                                  :: iz0tlnd
real                                     :: shadlen
integer                                  :: slope_rad
integer                                  :: topo_shading
integer                                  :: topo_wind
integer                                  :: no_mp_heating
integer                                  :: fractional_seaice
integer                                  :: seaice_snowdepth_opt
real                                     :: seaice_snowdepth_max
real                                     :: seaice_snowdepth_min
integer                                  :: seaice_albedo_opt
real                                     :: seaice_albedo_default
integer                                  :: seaice_thickness_opt
real                                     :: seaice_thickness_default
logical                                  :: tice2tsk_if2cold
real                                     :: bucket_mm
real                                     :: bucket_j
real                                     :: mp_tend_lim
real                                     :: prec_acc_dt
integer                                  :: prec_acc_opt
integer                                  :: bucketr_opt
integer                                  :: process_time_series
integer                                  :: grav_settling
real                                     :: sas_pgcon
integer                                  :: scalar_pblmix
integer                                  :: tracer_pblmix
logical                                  :: use_aero_icbc
integer                                  :: afwa_diag_opt
integer                                  :: afwa_ptype_opt
integer                                  :: afwa_vil_opt
integer                                  :: afwa_radar_opt
integer                                  :: afwa_severe_opt
integer                                  :: afwa_icing_opt
integer                                  :: afwa_vis_opt
integer                                  :: afwa_cloud_opt
real                                     :: afwa_ptype_ccn_tmp
real                                     :: afwa_ptype_tot_melt
real                                     :: afwa_ccn_conc
integer                                  :: afwa_hail_opt
integer                                  :: dveg
integer                                  :: opt_crs
integer                                  :: opt_btr
integer                                  :: opt_run
integer                                  :: opt_sfc
integer                                  :: opt_frz
integer                                  :: opt_inf
integer                                  :: opt_rad
integer                                  :: opt_alb
integer                                  :: opt_snf
integer                                  :: opt_tbot
integer                                  :: opt_stc
real                                     :: wtddt
integer                                  :: wrf_hydro
real                                     :: fgdt
integer                                  :: fgdtzero
integer                                  :: grid_fdda
integer                                  :: grid_sfdda
integer                                  :: if_no_pbl_nudging_uv
integer                                  :: if_no_pbl_nudging_t
integer                                  :: if_no_pbl_nudging_ph
integer                                  :: if_no_pbl_nudging_q
integer                                  :: if_zfac_uv
integer                                  :: k_zfac_uv
integer                                  :: if_zfac_t
integer                                  :: k_zfac_t
integer                                  :: if_zfac_ph
integer                                  :: k_zfac_ph
integer                                  :: if_zfac_q
integer                                  :: k_zfac_q
integer                                  :: dk_zfac_uv
integer                                  :: dk_zfac_t
integer                                  :: dk_zfac_ph
real                                     :: guv
real                                     :: guv_sfc
real                                     :: gt
real                                     :: gt_sfc
real                                     :: gq
real                                     :: gq_sfc
real                                     :: gph
real                                     :: dtramp_min
integer                                  :: if_ramping
real                                     :: rinblw
integer                                  :: xwavenum
integer                                  :: ywavenum
integer                                  :: pxlsm_soil_nudge
integer                                  :: obs_nudge_opt
integer                                  :: max_obs
real                                     :: fdda_start
real                                     :: fdda_end
integer                                  :: obs_nudge_wind
real                                     :: obs_coef_wind
integer                                  :: obs_nudge_temp
real                                     :: obs_coef_temp
integer                                  :: obs_nudge_mois
real                                     :: obs_coef_mois
integer                                  :: obs_nudge_pstr
real                                     :: obs_coef_pstr
integer                                  :: obs_no_pbl_nudge_uv
integer                                  :: obs_no_pbl_nudge_t
integer                                  :: obs_no_pbl_nudge_q
integer                                  :: obs_sfc_scheme_horiz
integer                                  :: obs_sfc_scheme_vert
real                                     :: obs_max_sndng_gap
real                                     :: obs_nudgezfullr1_uv
real                                     :: obs_nudgezrampr1_uv
real                                     :: obs_nudgezfullr2_uv
real                                     :: obs_nudgezrampr2_uv
real                                     :: obs_nudgezfullr4_uv
real                                     :: obs_nudgezrampr4_uv
real                                     :: obs_nudgezfullr1_t
real                                     :: obs_nudgezrampr1_t
real                                     :: obs_nudgezfullr2_t
real                                     :: obs_nudgezrampr2_t
real                                     :: obs_nudgezfullr4_t
real                                     :: obs_nudgezrampr4_t
real                                     :: obs_nudgezfullr1_q
real                                     :: obs_nudgezrampr1_q
real                                     :: obs_nudgezfullr2_q
real                                     :: obs_nudgezrampr2_q
real                                     :: obs_nudgezfullr4_q
real                                     :: obs_nudgezrampr4_q
real                                     :: obs_nudgezfullmin
real                                     :: obs_nudgezrampmin
real                                     :: obs_nudgezmax
real                                     :: obs_sfcfact
real                                     :: obs_sfcfacr
real                                     :: obs_dpsmx
real                                     :: obs_rinxy
real                                     :: obs_rinsig
real                                     :: obs_twindo
integer                                  :: obs_npfi
integer                                  :: obs_ionf
integer                                  :: obs_idynin
real                                     :: obs_dtramp
integer                                  :: obs_prt_max
integer                                  :: obs_prt_freq
logical                                  :: obs_ipf_in4dob
logical                                  :: obs_ipf_errob
logical                                  :: obs_ipf_nudob
logical                                  :: obs_ipf_init
integer                                  :: obs_scl_neg_qv_innov
integer                                  :: scm_force
real                                     :: scm_force_dx
integer                                  :: num_force_layers
integer                                  :: scm_lu_index
integer                                  :: scm_isltyp
real                                     :: scm_vegfra
integer                                  :: scm_canwat
real                                     :: scm_lat
real                                     :: scm_lon
logical                                  :: scm_th_t_tend
logical                                  :: scm_qv_t_tend
logical                                  :: scm_th_adv
logical                                  :: scm_wind_adv
logical                                  :: scm_qv_adv
logical                                  :: scm_ql_adv
logical                                  :: scm_vert_adv
integer                                  :: num_force_soil_layers
logical                                  :: scm_soilt_force
logical                                  :: scm_soilq_force
logical                                  :: scm_force_th_largescale
logical                                  :: scm_force_qv_largescale
logical                                  :: scm_force_ql_largescale
logical                                  :: scm_force_wind_largescale
integer                                  :: scm_force_skintemp
integer                                  :: scm_force_flux
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: w_damping
integer                                  :: diff_opt
integer                                  :: diff_opt_dfi
integer                                  :: km_opt
integer                                  :: km_opt_dfi
integer                                  :: damp_opt
integer                                  :: rad_nudge
integer                                  :: gwd_opt
real                                     :: zdamp
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: diff_6th_factor
integer                                  :: diff_6th_opt
real                                     :: c_s
real                                     :: c_k
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
logical                                  :: non_hydrostatic
logical                                  :: use_input_w
integer                                  :: time_step_sound
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
integer                                  :: momentum_adv_opt
integer                                  :: moist_adv_opt
integer                                  :: moist_adv_dfi_opt
integer                                  :: chem_adv_opt
integer                                  :: tracer_adv_opt
integer                                  :: scalar_adv_opt
integer                                  :: tke_adv_opt
logical                                  :: top_radiation
integer                                  :: mix_isotropic
real                                     :: mix_upper_bound
logical                                  :: top_lid
real                                     :: tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
logical                                  :: coriolis2d
logical                                  :: mix_full_fields
real                                     :: base_pres
real                                     :: base_temp
real                                     :: base_lapse
real                                     :: iso_temp
logical                                  :: use_baseparam_fr_nml
real                                     :: fft_filter_lat
logical                                  :: rotated_pole
logical                                  :: do_coriolis
logical                                  :: do_curvature
logical                                  :: do_gradp
integer                                  :: tracer_opt
integer                                  :: tenddiag
integer                                  :: spec_bdy_width
integer                                  :: spec_zone
integer                                  :: relax_zone
logical                                  :: specified
logical                                  :: constant_bc
logical                                  :: periodic_x
logical                                  :: symmetric_xs
logical                                  :: symmetric_xe
logical                                  :: open_xs
logical                                  :: open_xe
logical                                  :: periodic_y
logical                                  :: symmetric_ys
logical                                  :: symmetric_ye
logical                                  :: open_ys
logical                                  :: open_ye
logical                                  :: polar
logical                                  :: nested
real                                     :: spec_exp
integer                                  :: real_data_init_type
logical                                  :: have_bcs_moist
logical                                  :: have_bcs_scalar
integer                                  :: background_proc_id
integer                                  :: forecast_proc_id
integer                                  :: production_status
integer                                  :: compression
integer                                  :: nobs_ndg_vars
integer                                  :: nobs_err_flds
real                                     :: cen_lat
real                                     :: cen_lon
real                                     :: truelat1
real                                     :: truelat2
real                                     :: moad_cen_lat
real                                     :: stand_lon
real                                     :: pole_lat
real                                     :: pole_lon
integer                                  :: flag_metgrid
integer                                  :: flag_snow
integer                                  :: flag_psfc
integer                                  :: flag_sm000010
integer                                  :: flag_sm010040
integer                                  :: flag_sm040100
integer                                  :: flag_sm100200
integer                                  :: flag_st000010
integer                                  :: flag_st010040
integer                                  :: flag_st040100
integer                                  :: flag_st100200
integer                                  :: flag_soil_layers
integer                                  :: flag_slp
integer                                  :: flag_soilhgt
integer                                  :: flag_mf_xy
real                                     :: bdyfrq
character*256                               :: mminlu
integer                                  :: iswater
integer                                  :: islake
integer                                  :: isice
integer                                  :: isurban
integer                                  :: isoilwater
integer                                  :: map_proj
integer                                  :: use_wps_input
integer                                  :: dfi_stage
integer                                  :: mp_physics_dfi
integer                                  :: nodyn_dummy
integer                                  :: windfarm_opt
integer                                  :: windfarm_ij
integer                                  :: lightning_option
real                                     :: lightning_dt
real                                     :: lightning_start_seconds
real                                     :: flashrate_factor
integer                                  :: iccg_method
real                                     :: iccg_prescribed_num
real                                     :: iccg_prescribed_den
integer                                  :: cellcount_method
real                                     :: cldtop_adjustment
integer                                  :: sf_lake_physics
TYPE(fdob_type)                               :: fdob
character*256                               :: auxinput1_inname
integer                                  :: io_form_auxinput1
logical                                  :: override_restart_timers
integer                                  :: auxhist1_oid
character*256                               :: auxhist1_inname
character*256                               :: auxhist1_outname
integer                                  :: auxhist1_interval_y
integer                                  :: auxhist1_interval_d
integer                                  :: auxhist1_interval_h
integer                                  :: auxhist1_interval_m
integer                                  :: auxhist1_interval_s
integer                                  :: auxhist1_interval
integer                                  :: auxhist1_begin_y
integer                                  :: auxhist1_begin_d
integer                                  :: auxhist1_begin_h
integer                                  :: auxhist1_begin_m
integer                                  :: auxhist1_begin_s
integer                                  :: auxhist1_begin
integer                                  :: auxhist1_end_y
integer                                  :: auxhist1_end_d
integer                                  :: auxhist1_end_h
integer                                  :: auxhist1_end_m
integer                                  :: auxhist1_end_s
integer                                  :: auxhist1_end
integer                                  :: io_form_auxhist1
integer                                  :: frames_per_auxhist1
integer                                  :: auxhist2_oid
character*256                               :: auxhist2_inname
character*256                               :: auxhist2_outname
integer                                  :: auxhist2_interval_y
integer                                  :: auxhist2_interval_d
integer                                  :: auxhist2_interval_h
integer                                  :: auxhist2_interval_m
integer                                  :: auxhist2_interval_s
integer                                  :: auxhist2_interval
integer                                  :: auxhist2_begin_y
integer                                  :: auxhist2_begin_d
integer                                  :: auxhist2_begin_h
integer                                  :: auxhist2_begin_m
integer                                  :: auxhist2_begin_s
integer                                  :: auxhist2_begin
integer                                  :: auxhist2_end_y
integer                                  :: auxhist2_end_d
integer                                  :: auxhist2_end_h
integer                                  :: auxhist2_end_m
integer                                  :: auxhist2_end_s
integer                                  :: auxhist2_end
integer                                  :: io_form_auxhist2
integer                                  :: frames_per_auxhist2
integer                                  :: auxhist3_oid
character*256                               :: auxhist3_inname
character*256                               :: auxhist3_outname
integer                                  :: auxhist3_interval_y
integer                                  :: auxhist3_interval_d
integer                                  :: auxhist3_interval_h
integer                                  :: auxhist3_interval_m
integer                                  :: auxhist3_interval_s
integer                                  :: auxhist3_interval
integer                                  :: auxhist3_begin_y
integer                                  :: auxhist3_begin_d
integer                                  :: auxhist3_begin_h
integer                                  :: auxhist3_begin_m
integer                                  :: auxhist3_begin_s
integer                                  :: auxhist3_begin
integer                                  :: auxhist3_end_y
integer                                  :: auxhist3_end_d
integer                                  :: auxhist3_end_h
integer                                  :: auxhist3_end_m
integer                                  :: auxhist3_end_s
integer                                  :: auxhist3_end
integer                                  :: io_form_auxhist3
integer                                  :: frames_per_auxhist3
integer                                  :: auxhist4_oid
character*256                               :: auxhist4_inname
character*256                               :: auxhist4_outname
integer                                  :: auxhist4_interval_y
integer                                  :: auxhist4_interval_d
integer                                  :: auxhist4_interval_h
integer                                  :: auxhist4_interval_m
integer                                  :: auxhist4_interval_s
integer                                  :: auxhist4_interval
integer                                  :: auxhist4_begin_y
integer                                  :: auxhist4_begin_d
integer                                  :: auxhist4_begin_h
integer                                  :: auxhist4_begin_m
integer                                  :: auxhist4_begin_s
integer                                  :: auxhist4_begin
integer                                  :: auxhist4_end_y
integer                                  :: auxhist4_end_d
integer                                  :: auxhist4_end_h
integer                                  :: auxhist4_end_m
integer                                  :: auxhist4_end_s
integer                                  :: auxhist4_end
integer                                  :: io_form_auxhist4
integer                                  :: frames_per_auxhist4
integer                                  :: auxhist5_oid
character*256                               :: auxhist5_inname
character*256                               :: auxhist5_outname
integer                                  :: auxhist5_interval_y
integer                                  :: auxhist5_interval_d
integer                                  :: auxhist5_interval_h
integer                                  :: auxhist5_interval_m
integer                                  :: auxhist5_interval_s
integer                                  :: auxhist5_interval
integer                                  :: auxhist5_begin_y
integer                                  :: auxhist5_begin_d
integer                                  :: auxhist5_begin_h
integer                                  :: auxhist5_begin_m
integer                                  :: auxhist5_begin_s
integer                                  :: auxhist5_begin
integer                                  :: auxhist5_end_y
integer                                  :: auxhist5_end_d
integer                                  :: auxhist5_end_h
integer                                  :: auxhist5_end_m
integer                                  :: auxhist5_end_s
integer                                  :: auxhist5_end
integer                                  :: io_form_auxhist5
integer                                  :: frames_per_auxhist5
integer                                  :: auxhist6_oid
character*256                               :: auxhist6_inname
character*256                               :: auxhist6_outname
integer                                  :: auxhist6_interval_y
integer                                  :: auxhist6_interval_d
integer                                  :: auxhist6_interval_h
integer                                  :: auxhist6_interval_m
integer                                  :: auxhist6_interval_s
integer                                  :: auxhist6_interval
integer                                  :: auxhist6_begin_y
integer                                  :: auxhist6_begin_d
integer                                  :: auxhist6_begin_h
integer                                  :: auxhist6_begin_m
integer                                  :: auxhist6_begin_s
integer                                  :: auxhist6_begin
integer                                  :: auxhist6_end_y
integer                                  :: auxhist6_end_d
integer                                  :: auxhist6_end_h
integer                                  :: auxhist6_end_m
integer                                  :: auxhist6_end_s
integer                                  :: auxhist6_end
integer                                  :: io_form_auxhist6
integer                                  :: frames_per_auxhist6
integer                                  :: auxhist7_oid
character*256                               :: auxhist7_inname
character*256                               :: auxhist7_outname
integer                                  :: auxhist7_interval_y
integer                                  :: auxhist7_interval_d
integer                                  :: auxhist7_interval_h
integer                                  :: auxhist7_interval_m
integer                                  :: auxhist7_interval_s
integer                                  :: auxhist7_interval
integer                                  :: auxhist7_begin_y
integer                                  :: auxhist7_begin_d
integer                                  :: auxhist7_begin_h
integer                                  :: auxhist7_begin_m
integer                                  :: auxhist7_begin_s
integer                                  :: auxhist7_begin
integer                                  :: auxhist7_end_y
integer                                  :: auxhist7_end_d
integer                                  :: auxhist7_end_h
integer                                  :: auxhist7_end_m
integer                                  :: auxhist7_end_s
integer                                  :: auxhist7_end
integer                                  :: io_form_auxhist7
integer                                  :: frames_per_auxhist7
integer                                  :: auxhist8_oid
character*256                               :: auxhist8_inname
character*256                               :: auxhist8_outname
integer                                  :: auxhist8_interval_y
integer                                  :: auxhist8_interval_d
integer                                  :: auxhist8_interval_h
integer                                  :: auxhist8_interval_m
integer                                  :: auxhist8_interval_s
integer                                  :: auxhist8_interval
integer                                  :: auxhist8_begin_y
integer                                  :: auxhist8_begin_d
integer                                  :: auxhist8_begin_h
integer                                  :: auxhist8_begin_m
integer                                  :: auxhist8_begin_s
integer                                  :: auxhist8_begin
integer                                  :: auxhist8_end_y
integer                                  :: auxhist8_end_d
integer                                  :: auxhist8_end_h
integer                                  :: auxhist8_end_m
integer                                  :: auxhist8_end_s
integer                                  :: auxhist8_end
integer                                  :: io_form_auxhist8
integer                                  :: frames_per_auxhist8
integer                                  :: auxhist9_oid
character*256                               :: auxhist9_inname
character*256                               :: auxhist9_outname
integer                                  :: auxhist9_interval_y
integer                                  :: auxhist9_interval_d
integer                                  :: auxhist9_interval_h
integer                                  :: auxhist9_interval_m
integer                                  :: auxhist9_interval_s
integer                                  :: auxhist9_interval
integer                                  :: auxhist9_begin_y
integer                                  :: auxhist9_begin_d
integer                                  :: auxhist9_begin_h
integer                                  :: auxhist9_begin_m
integer                                  :: auxhist9_begin_s
integer                                  :: auxhist9_begin
integer                                  :: auxhist9_end_y
integer                                  :: auxhist9_end_d
integer                                  :: auxhist9_end_h
integer                                  :: auxhist9_end_m
integer                                  :: auxhist9_end_s
integer                                  :: auxhist9_end
integer                                  :: io_form_auxhist9
integer                                  :: frames_per_auxhist9
integer                                  :: auxhist10_oid
character*256                               :: auxhist10_inname
character*256                               :: auxhist10_outname
integer                                  :: auxhist10_interval_y
integer                                  :: auxhist10_interval_d
integer                                  :: auxhist10_interval_h
integer                                  :: auxhist10_interval_m
integer                                  :: auxhist10_interval_s
integer                                  :: auxhist10_interval
integer                                  :: auxhist10_begin_y
integer                                  :: auxhist10_begin_d
integer                                  :: auxhist10_begin_h
integer                                  :: auxhist10_begin_m
integer                                  :: auxhist10_begin_s
integer                                  :: auxhist10_begin
integer                                  :: auxhist10_end_y
integer                                  :: auxhist10_end_d
integer                                  :: auxhist10_end_h
integer                                  :: auxhist10_end_m
integer                                  :: auxhist10_end_s
integer                                  :: auxhist10_end
integer                                  :: io_form_auxhist10
integer                                  :: frames_per_auxhist10
integer                                  :: auxhist11_oid
character*256                               :: auxhist11_inname
character*256                               :: auxhist11_outname
integer                                  :: auxhist11_interval_y
integer                                  :: auxhist11_interval_d
integer                                  :: auxhist11_interval_h
integer                                  :: auxhist11_interval_m
integer                                  :: auxhist11_interval_s
integer                                  :: auxhist11_interval
integer                                  :: auxhist11_begin_y
integer                                  :: auxhist11_begin_d
integer                                  :: auxhist11_begin_h
integer                                  :: auxhist11_begin_m
integer                                  :: auxhist11_begin_s
integer                                  :: auxhist11_begin
integer                                  :: auxhist11_end_y
integer                                  :: auxhist11_end_d
integer                                  :: auxhist11_end_h
integer                                  :: auxhist11_end_m
integer                                  :: auxhist11_end_s
integer                                  :: auxhist11_end
integer                                  :: io_form_auxhist11
integer                                  :: frames_per_auxhist11
integer                                  :: auxhist12_oid
character*256                               :: auxhist12_inname
character*256                               :: auxhist12_outname
integer                                  :: auxhist12_interval_y
integer                                  :: auxhist12_interval_d
integer                                  :: auxhist12_interval_h
integer                                  :: auxhist12_interval_m
integer                                  :: auxhist12_interval_s
integer                                  :: auxhist12_interval
integer                                  :: auxhist12_begin_y
integer                                  :: auxhist12_begin_d
integer                                  :: auxhist12_begin_h
integer                                  :: auxhist12_begin_m
integer                                  :: auxhist12_begin_s
integer                                  :: auxhist12_begin
integer                                  :: auxhist12_end_y
integer                                  :: auxhist12_end_d
integer                                  :: auxhist12_end_h
integer                                  :: auxhist12_end_m
integer                                  :: auxhist12_end_s
integer                                  :: auxhist12_end
integer                                  :: io_form_auxhist12
integer                                  :: frames_per_auxhist12
integer                                  :: auxhist13_oid
character*256                               :: auxhist13_inname
character*256                               :: auxhist13_outname
integer                                  :: auxhist13_interval_y
integer                                  :: auxhist13_interval_d
integer                                  :: auxhist13_interval_h
integer                                  :: auxhist13_interval_m
integer                                  :: auxhist13_interval_s
integer                                  :: auxhist13_interval
integer                                  :: auxhist13_begin_y
integer                                  :: auxhist13_begin_d
integer                                  :: auxhist13_begin_h
integer                                  :: auxhist13_begin_m
integer                                  :: auxhist13_begin_s
integer                                  :: auxhist13_begin
integer                                  :: auxhist13_end_y
integer                                  :: auxhist13_end_d
integer                                  :: auxhist13_end_h
integer                                  :: auxhist13_end_m
integer                                  :: auxhist13_end_s
integer                                  :: auxhist13_end
integer                                  :: io_form_auxhist13
integer                                  :: frames_per_auxhist13
integer                                  :: auxhist14_oid
character*256                               :: auxhist14_inname
character*256                               :: auxhist14_outname
integer                                  :: auxhist14_interval_y
integer                                  :: auxhist14_interval_d
integer                                  :: auxhist14_interval_h
integer                                  :: auxhist14_interval_m
integer                                  :: auxhist14_interval_s
integer                                  :: auxhist14_interval
integer                                  :: auxhist14_begin_y
integer                                  :: auxhist14_begin_d
integer                                  :: auxhist14_begin_h
integer                                  :: auxhist14_begin_m
integer                                  :: auxhist14_begin_s
integer                                  :: auxhist14_begin
integer                                  :: auxhist14_end_y
integer                                  :: auxhist14_end_d
integer                                  :: auxhist14_end_h
integer                                  :: auxhist14_end_m
integer                                  :: auxhist14_end_s
integer                                  :: auxhist14_end
integer                                  :: io_form_auxhist14
integer                                  :: frames_per_auxhist14
integer                                  :: auxhist15_oid
character*256                               :: auxhist15_inname
character*256                               :: auxhist15_outname
integer                                  :: auxhist15_interval_y
integer                                  :: auxhist15_interval_d
integer                                  :: auxhist15_interval_h
integer                                  :: auxhist15_interval_m
integer                                  :: auxhist15_interval_s
integer                                  :: auxhist15_interval
integer                                  :: auxhist15_begin_y
integer                                  :: auxhist15_begin_d
integer                                  :: auxhist15_begin_h
integer                                  :: auxhist15_begin_m
integer                                  :: auxhist15_begin_s
integer                                  :: auxhist15_begin
integer                                  :: auxhist15_end_y
integer                                  :: auxhist15_end_d
integer                                  :: auxhist15_end_h
integer                                  :: auxhist15_end_m
integer                                  :: auxhist15_end_s
integer                                  :: auxhist15_end
integer                                  :: io_form_auxhist15
integer                                  :: frames_per_auxhist15
integer                                  :: auxhist16_oid
character*256                               :: auxhist16_inname
character*256                               :: auxhist16_outname
integer                                  :: auxhist16_interval_y
integer                                  :: auxhist16_interval_d
integer                                  :: auxhist16_interval_h
integer                                  :: auxhist16_interval_m
integer                                  :: auxhist16_interval_s
integer                                  :: auxhist16_interval
integer                                  :: auxhist16_begin_y
integer                                  :: auxhist16_begin_d
integer                                  :: auxhist16_begin_h
integer                                  :: auxhist16_begin_m
integer                                  :: auxhist16_begin_s
integer                                  :: auxhist16_begin
integer                                  :: auxhist16_end_y
integer                                  :: auxhist16_end_d
integer                                  :: auxhist16_end_h
integer                                  :: auxhist16_end_m
integer                                  :: auxhist16_end_s
integer                                  :: auxhist16_end
integer                                  :: io_form_auxhist16
integer                                  :: frames_per_auxhist16
integer                                  :: auxhist17_oid
character*256                               :: auxhist17_inname
character*256                               :: auxhist17_outname
integer                                  :: auxhist17_interval_y
integer                                  :: auxhist17_interval_d
integer                                  :: auxhist17_interval_h
integer                                  :: auxhist17_interval_m
integer                                  :: auxhist17_interval_s
integer                                  :: auxhist17_interval
integer                                  :: auxhist17_begin_y
integer                                  :: auxhist17_begin_d
integer                                  :: auxhist17_begin_h
integer                                  :: auxhist17_begin_m
integer                                  :: auxhist17_begin_s
integer                                  :: auxhist17_begin
integer                                  :: auxhist17_end_y
integer                                  :: auxhist17_end_d
integer                                  :: auxhist17_end_h
integer                                  :: auxhist17_end_m
integer                                  :: auxhist17_end_s
integer                                  :: auxhist17_end
integer                                  :: io_form_auxhist17
integer                                  :: frames_per_auxhist17
integer                                  :: auxhist18_oid
character*256                               :: auxhist18_inname
character*256                               :: auxhist18_outname
integer                                  :: auxhist18_interval_y
integer                                  :: auxhist18_interval_d
integer                                  :: auxhist18_interval_h
integer                                  :: auxhist18_interval_m
integer                                  :: auxhist18_interval_s
integer                                  :: auxhist18_interval
integer                                  :: auxhist18_begin_y
integer                                  :: auxhist18_begin_d
integer                                  :: auxhist18_begin_h
integer                                  :: auxhist18_begin_m
integer                                  :: auxhist18_begin_s
integer                                  :: auxhist18_begin
integer                                  :: auxhist18_end_y
integer                                  :: auxhist18_end_d
integer                                  :: auxhist18_end_h
integer                                  :: auxhist18_end_m
integer                                  :: auxhist18_end_s
integer                                  :: auxhist18_end
integer                                  :: io_form_auxhist18
integer                                  :: frames_per_auxhist18
integer                                  :: auxhist19_oid
character*256                               :: auxhist19_inname
character*256                               :: auxhist19_outname
integer                                  :: auxhist19_interval_y
integer                                  :: auxhist19_interval_d
integer                                  :: auxhist19_interval_h
integer                                  :: auxhist19_interval_m
integer                                  :: auxhist19_interval_s
integer                                  :: auxhist19_interval
integer                                  :: auxhist19_begin_y
integer                                  :: auxhist19_begin_d
integer                                  :: auxhist19_begin_h
integer                                  :: auxhist19_begin_m
integer                                  :: auxhist19_begin_s
integer                                  :: auxhist19_begin
integer                                  :: auxhist19_end_y
integer                                  :: auxhist19_end_d
integer                                  :: auxhist19_end_h
integer                                  :: auxhist19_end_m
integer                                  :: auxhist19_end_s
integer                                  :: auxhist19_end
integer                                  :: io_form_auxhist19
integer                                  :: frames_per_auxhist19
integer                                  :: auxhist20_oid
character*256                               :: auxhist20_inname
character*256                               :: auxhist20_outname
integer                                  :: auxhist20_interval_y
integer                                  :: auxhist20_interval_d
integer                                  :: auxhist20_interval_h
integer                                  :: auxhist20_interval_m
integer                                  :: auxhist20_interval_s
integer                                  :: auxhist20_interval
integer                                  :: auxhist20_begin_y
integer                                  :: auxhist20_begin_d
integer                                  :: auxhist20_begin_h
integer                                  :: auxhist20_begin_m
integer                                  :: auxhist20_begin_s
integer                                  :: auxhist20_begin
integer                                  :: auxhist20_end_y
integer                                  :: auxhist20_end_d
integer                                  :: auxhist20_end_h
integer                                  :: auxhist20_end_m
integer                                  :: auxhist20_end_s
integer                                  :: auxhist20_end
integer                                  :: io_form_auxhist20
integer                                  :: frames_per_auxhist20
integer                                  :: auxhist21_oid
character*256                               :: auxhist21_inname
character*256                               :: auxhist21_outname
integer                                  :: auxhist21_interval_y
integer                                  :: auxhist21_interval_d
integer                                  :: auxhist21_interval_h
integer                                  :: auxhist21_interval_m
integer                                  :: auxhist21_interval_s
integer                                  :: auxhist21_interval
integer                                  :: auxhist21_begin_y
integer                                  :: auxhist21_begin_d
integer                                  :: auxhist21_begin_h
integer                                  :: auxhist21_begin_m
integer                                  :: auxhist21_begin_s
integer                                  :: auxhist21_begin
integer                                  :: auxhist21_end_y
integer                                  :: auxhist21_end_d
integer                                  :: auxhist21_end_h
integer                                  :: auxhist21_end_m
integer                                  :: auxhist21_end_s
integer                                  :: auxhist21_end
integer                                  :: io_form_auxhist21
integer                                  :: frames_per_auxhist21
integer                                  :: auxhist22_oid
character*256                               :: auxhist22_inname
character*256                               :: auxhist22_outname
integer                                  :: auxhist22_interval_y
integer                                  :: auxhist22_interval_d
integer                                  :: auxhist22_interval_h
integer                                  :: auxhist22_interval_m
integer                                  :: auxhist22_interval_s
integer                                  :: auxhist22_interval
integer                                  :: auxhist22_begin_y
integer                                  :: auxhist22_begin_d
integer                                  :: auxhist22_begin_h
integer                                  :: auxhist22_begin_m
integer                                  :: auxhist22_begin_s
integer                                  :: auxhist22_begin
integer                                  :: auxhist22_end_y
integer                                  :: auxhist22_end_d
integer                                  :: auxhist22_end_h
integer                                  :: auxhist22_end_m
integer                                  :: auxhist22_end_s
integer                                  :: auxhist22_end
integer                                  :: io_form_auxhist22
integer                                  :: frames_per_auxhist22
integer                                  :: auxhist23_oid
character*256                               :: auxhist23_inname
character*256                               :: auxhist23_outname
integer                                  :: auxhist23_interval_y
integer                                  :: auxhist23_interval_d
integer                                  :: auxhist23_interval_h
integer                                  :: auxhist23_interval_m
integer                                  :: auxhist23_interval_s
integer                                  :: auxhist23_interval
integer                                  :: auxhist23_begin_y
integer                                  :: auxhist23_begin_d
integer                                  :: auxhist23_begin_h
integer                                  :: auxhist23_begin_m
integer                                  :: auxhist23_begin_s
integer                                  :: auxhist23_begin
integer                                  :: auxhist23_end_y
integer                                  :: auxhist23_end_d
integer                                  :: auxhist23_end_h
integer                                  :: auxhist23_end_m
integer                                  :: auxhist23_end_s
integer                                  :: auxhist23_end
integer                                  :: io_form_auxhist23
integer                                  :: frames_per_auxhist23
integer                                  :: auxhist24_oid
character*256                               :: auxhist24_inname
character*256                               :: auxhist24_outname
integer                                  :: auxhist24_interval_y
integer                                  :: auxhist24_interval_d
integer                                  :: auxhist24_interval_h
integer                                  :: auxhist24_interval_m
integer                                  :: auxhist24_interval_s
integer                                  :: auxhist24_interval
integer                                  :: auxhist24_begin_y
integer                                  :: auxhist24_begin_d
integer                                  :: auxhist24_begin_h
integer                                  :: auxhist24_begin_m
integer                                  :: auxhist24_begin_s
integer                                  :: auxhist24_begin
integer                                  :: auxhist24_end_y
integer                                  :: auxhist24_end_d
integer                                  :: auxhist24_end_h
integer                                  :: auxhist24_end_m
integer                                  :: auxhist24_end_s
integer                                  :: auxhist24_end
integer                                  :: io_form_auxhist24
integer                                  :: frames_per_auxhist24
integer                                  :: auxinput1_oid
character*256                               :: auxinput1_outname
integer                                  :: auxinput1_interval_y
integer                                  :: auxinput1_interval_d
integer                                  :: auxinput1_interval_h
integer                                  :: auxinput1_interval_m
integer                                  :: auxinput1_interval_s
integer                                  :: auxinput1_interval
integer                                  :: auxinput1_begin_y
integer                                  :: auxinput1_begin_d
integer                                  :: auxinput1_begin_h
integer                                  :: auxinput1_begin_m
integer                                  :: auxinput1_begin_s
integer                                  :: auxinput1_begin
integer                                  :: auxinput1_end_y
integer                                  :: auxinput1_end_d
integer                                  :: auxinput1_end_h
integer                                  :: auxinput1_end_m
integer                                  :: auxinput1_end_s
integer                                  :: auxinput1_end
integer                                  :: frames_per_auxinput1
integer                                  :: auxinput2_oid
character*256                               :: auxinput2_inname
character*256                               :: auxinput2_outname
integer                                  :: auxinput2_interval_y
integer                                  :: auxinput2_interval_d
integer                                  :: auxinput2_interval_h
integer                                  :: auxinput2_interval_m
integer                                  :: auxinput2_interval_s
integer                                  :: auxinput2_interval
integer                                  :: auxinput2_begin_y
integer                                  :: auxinput2_begin_d
integer                                  :: auxinput2_begin_h
integer                                  :: auxinput2_begin_m
integer                                  :: auxinput2_begin_s
integer                                  :: auxinput2_begin
integer                                  :: auxinput2_end_y
integer                                  :: auxinput2_end_d
integer                                  :: auxinput2_end_h
integer                                  :: auxinput2_end_m
integer                                  :: auxinput2_end_s
integer                                  :: auxinput2_end
integer                                  :: io_form_auxinput2
integer                                  :: frames_per_auxinput2
integer                                  :: auxinput3_oid
character*256                               :: auxinput3_inname
character*256                               :: auxinput3_outname
integer                                  :: auxinput3_interval_y
integer                                  :: auxinput3_interval_d
integer                                  :: auxinput3_interval_h
integer                                  :: auxinput3_interval_m
integer                                  :: auxinput3_interval_s
integer                                  :: auxinput3_interval
integer                                  :: auxinput3_begin_y
integer                                  :: auxinput3_begin_d
integer                                  :: auxinput3_begin_h
integer                                  :: auxinput3_begin_m
integer                                  :: auxinput3_begin_s
integer                                  :: auxinput3_begin
integer                                  :: auxinput3_end_y
integer                                  :: auxinput3_end_d
integer                                  :: auxinput3_end_h
integer                                  :: auxinput3_end_m
integer                                  :: auxinput3_end_s
integer                                  :: auxinput3_end
integer                                  :: io_form_auxinput3
integer                                  :: frames_per_auxinput3
integer                                  :: auxinput4_oid
character*256                               :: auxinput4_inname
character*256                               :: auxinput4_outname
integer                                  :: auxinput4_interval_y
integer                                  :: auxinput4_interval_d
integer                                  :: auxinput4_interval_h
integer                                  :: auxinput4_interval_m
integer                                  :: auxinput4_interval_s
integer                                  :: auxinput4_interval
integer                                  :: auxinput4_begin_y
integer                                  :: auxinput4_begin_d
integer                                  :: auxinput4_begin_h
integer                                  :: auxinput4_begin_m
integer                                  :: auxinput4_begin_s
integer                                  :: auxinput4_begin
integer                                  :: auxinput4_end_y
integer                                  :: auxinput4_end_d
integer                                  :: auxinput4_end_h
integer                                  :: auxinput4_end_m
integer                                  :: auxinput4_end_s
integer                                  :: auxinput4_end
integer                                  :: io_form_auxinput4
integer                                  :: frames_per_auxinput4
integer                                  :: auxinput5_oid
character*256                               :: auxinput5_inname
character*256                               :: auxinput5_outname
integer                                  :: auxinput5_interval_y
integer                                  :: auxinput5_interval_d
integer                                  :: auxinput5_interval_h
integer                                  :: auxinput5_interval_m
integer                                  :: auxinput5_interval_s
integer                                  :: auxinput5_interval
integer                                  :: auxinput5_begin_y
integer                                  :: auxinput5_begin_d
integer                                  :: auxinput5_begin_h
integer                                  :: auxinput5_begin_m
integer                                  :: auxinput5_begin_s
integer                                  :: auxinput5_begin
integer                                  :: auxinput5_end_y
integer                                  :: auxinput5_end_d
integer                                  :: auxinput5_end_h
integer                                  :: auxinput5_end_m
integer                                  :: auxinput5_end_s
integer                                  :: auxinput5_end
integer                                  :: io_form_auxinput5
integer                                  :: frames_per_auxinput5
integer                                  :: auxinput6_oid
character*256                               :: auxinput6_inname
character*256                               :: auxinput6_outname
integer                                  :: auxinput6_interval_y
integer                                  :: auxinput6_interval_d
integer                                  :: auxinput6_interval_h
integer                                  :: auxinput6_interval_m
integer                                  :: auxinput6_interval_s
integer                                  :: auxinput6_interval
integer                                  :: auxinput6_begin_y
integer                                  :: auxinput6_begin_d
integer                                  :: auxinput6_begin_h
integer                                  :: auxinput6_begin_m
integer                                  :: auxinput6_begin_s
integer                                  :: auxinput6_begin
integer                                  :: auxinput6_end_y
integer                                  :: auxinput6_end_d
integer                                  :: auxinput6_end_h
integer                                  :: auxinput6_end_m
integer                                  :: auxinput6_end_s
integer                                  :: auxinput6_end
integer                                  :: io_form_auxinput6
integer                                  :: frames_per_auxinput6
integer                                  :: auxinput7_oid
character*256                               :: auxinput7_inname
character*256                               :: auxinput7_outname
integer                                  :: auxinput7_interval_y
integer                                  :: auxinput7_interval_d
integer                                  :: auxinput7_interval_h
integer                                  :: auxinput7_interval_m
integer                                  :: auxinput7_interval_s
integer                                  :: auxinput7_interval
integer                                  :: auxinput7_begin_y
integer                                  :: auxinput7_begin_d
integer                                  :: auxinput7_begin_h
integer                                  :: auxinput7_begin_m
integer                                  :: auxinput7_begin_s
integer                                  :: auxinput7_begin
integer                                  :: auxinput7_end_y
integer                                  :: auxinput7_end_d
integer                                  :: auxinput7_end_h
integer                                  :: auxinput7_end_m
integer                                  :: auxinput7_end_s
integer                                  :: auxinput7_end
integer                                  :: io_form_auxinput7
integer                                  :: frames_per_auxinput7
integer                                  :: auxinput8_oid
character*256                               :: auxinput8_inname
character*256                               :: auxinput8_outname
integer                                  :: auxinput8_interval_y
integer                                  :: auxinput8_interval_d
integer                                  :: auxinput8_interval_h
integer                                  :: auxinput8_interval_m
integer                                  :: auxinput8_interval_s
integer                                  :: auxinput8_interval
integer                                  :: auxinput8_begin_y
integer                                  :: auxinput8_begin_d
integer                                  :: auxinput8_begin_h
integer                                  :: auxinput8_begin_m
integer                                  :: auxinput8_begin_s
integer                                  :: auxinput8_begin
integer                                  :: auxinput8_end_y
integer                                  :: auxinput8_end_d
integer                                  :: auxinput8_end_h
integer                                  :: auxinput8_end_m
integer                                  :: auxinput8_end_s
integer                                  :: auxinput8_end
integer                                  :: io_form_auxinput8
integer                                  :: frames_per_auxinput8
integer                                  :: auxinput9_oid
character*256                               :: auxinput9_inname
character*256                               :: auxinput9_outname
integer                                  :: auxinput9_interval_y
integer                                  :: auxinput9_interval_d
integer                                  :: auxinput9_interval_h
integer                                  :: auxinput9_interval_m
integer                                  :: auxinput9_interval_s
integer                                  :: auxinput9_interval
integer                                  :: auxinput9_begin_y
integer                                  :: auxinput9_begin_d
integer                                  :: auxinput9_begin_h
integer                                  :: auxinput9_begin_m
integer                                  :: auxinput9_begin_s
integer                                  :: auxinput9_begin
integer                                  :: auxinput9_end_y
integer                                  :: auxinput9_end_d
integer                                  :: auxinput9_end_h
integer                                  :: auxinput9_end_m
integer                                  :: auxinput9_end_s
integer                                  :: auxinput9_end
integer                                  :: io_form_auxinput9
integer                                  :: frames_per_auxinput9
integer                                  :: auxinput10_oid
character*256                               :: auxinput10_inname
character*256                               :: auxinput10_outname
integer                                  :: auxinput10_interval_y
integer                                  :: auxinput10_interval_d
integer                                  :: auxinput10_interval_h
integer                                  :: auxinput10_interval_m
integer                                  :: auxinput10_interval_s
integer                                  :: auxinput10_interval
integer                                  :: auxinput10_begin_y
integer                                  :: auxinput10_begin_d
integer                                  :: auxinput10_begin_h
integer                                  :: auxinput10_begin_m
integer                                  :: auxinput10_begin_s
integer                                  :: auxinput10_begin
integer                                  :: auxinput10_end_y
integer                                  :: auxinput10_end_d
integer                                  :: auxinput10_end_h
integer                                  :: auxinput10_end_m
integer                                  :: auxinput10_end_s
integer                                  :: auxinput10_end
integer                                  :: io_form_auxinput10
integer                                  :: frames_per_auxinput10
integer                                  :: auxinput11_oid
character*256                               :: auxinput11_inname
character*256                               :: auxinput11_outname
integer                                  :: auxinput11_interval_y
integer                                  :: auxinput11_interval_d
integer                                  :: auxinput11_interval_h
integer                                  :: auxinput11_interval_m
integer                                  :: auxinput11_interval_s
integer                                  :: auxinput11_interval
integer                                  :: auxinput11_begin_y
integer                                  :: auxinput11_begin_d
integer                                  :: auxinput11_begin_h
integer                                  :: auxinput11_begin_m
integer                                  :: auxinput11_begin_s
integer                                  :: auxinput11_begin
integer                                  :: auxinput11_end_y
integer                                  :: auxinput11_end_d
integer                                  :: auxinput11_end_h
integer                                  :: auxinput11_end_m
integer                                  :: auxinput11_end_s
integer                                  :: auxinput11_end
integer                                  :: io_form_auxinput11
integer                                  :: frames_per_auxinput11
integer                                  :: auxinput12_oid
character*256                               :: auxinput12_inname
character*256                               :: auxinput12_outname
integer                                  :: auxinput12_interval_y
integer                                  :: auxinput12_interval_d
integer                                  :: auxinput12_interval_h
integer                                  :: auxinput12_interval_m
integer                                  :: auxinput12_interval_s
integer                                  :: auxinput12_interval
integer                                  :: auxinput12_begin_y
integer                                  :: auxinput12_begin_d
integer                                  :: auxinput12_begin_h
integer                                  :: auxinput12_begin_m
integer                                  :: auxinput12_begin_s
integer                                  :: auxinput12_begin
integer                                  :: auxinput12_end_y
integer                                  :: auxinput12_end_d
integer                                  :: auxinput12_end_h
integer                                  :: auxinput12_end_m
integer                                  :: auxinput12_end_s
integer                                  :: auxinput12_end
integer                                  :: io_form_auxinput12
integer                                  :: frames_per_auxinput12
integer                                  :: auxinput13_oid
character*256                               :: auxinput13_inname
character*256                               :: auxinput13_outname
integer                                  :: auxinput13_interval_y
integer                                  :: auxinput13_interval_d
integer                                  :: auxinput13_interval_h
integer                                  :: auxinput13_interval_m
integer                                  :: auxinput13_interval_s
integer                                  :: auxinput13_interval
integer                                  :: auxinput13_begin_y
integer                                  :: auxinput13_begin_d
integer                                  :: auxinput13_begin_h
integer                                  :: auxinput13_begin_m
integer                                  :: auxinput13_begin_s
integer                                  :: auxinput13_begin
integer                                  :: auxinput13_end_y
integer                                  :: auxinput13_end_d
integer                                  :: auxinput13_end_h
integer                                  :: auxinput13_end_m
integer                                  :: auxinput13_end_s
integer                                  :: auxinput13_end
integer                                  :: io_form_auxinput13
integer                                  :: frames_per_auxinput13
integer                                  :: auxinput14_oid
character*256                               :: auxinput14_inname
character*256                               :: auxinput14_outname
integer                                  :: auxinput14_interval_y
integer                                  :: auxinput14_interval_d
integer                                  :: auxinput14_interval_h
integer                                  :: auxinput14_interval_m
integer                                  :: auxinput14_interval_s
integer                                  :: auxinput14_interval
integer                                  :: auxinput14_begin_y
integer                                  :: auxinput14_begin_d
integer                                  :: auxinput14_begin_h
integer                                  :: auxinput14_begin_m
integer                                  :: auxinput14_begin_s
integer                                  :: auxinput14_begin
integer                                  :: auxinput14_end_y
integer                                  :: auxinput14_end_d
integer                                  :: auxinput14_end_h
integer                                  :: auxinput14_end_m
integer                                  :: auxinput14_end_s
integer                                  :: auxinput14_end
integer                                  :: io_form_auxinput14
integer                                  :: frames_per_auxinput14
integer                                  :: auxinput15_oid
character*256                               :: auxinput15_inname
character*256                               :: auxinput15_outname
integer                                  :: auxinput15_interval_y
integer                                  :: auxinput15_interval_d
integer                                  :: auxinput15_interval_h
integer                                  :: auxinput15_interval_m
integer                                  :: auxinput15_interval_s
integer                                  :: auxinput15_interval
integer                                  :: auxinput15_begin_y
integer                                  :: auxinput15_begin_d
integer                                  :: auxinput15_begin_h
integer                                  :: auxinput15_begin_m
integer                                  :: auxinput15_begin_s
integer                                  :: auxinput15_begin
integer                                  :: auxinput15_end_y
integer                                  :: auxinput15_end_d
integer                                  :: auxinput15_end_h
integer                                  :: auxinput15_end_m
integer                                  :: auxinput15_end_s
integer                                  :: auxinput15_end
integer                                  :: io_form_auxinput15
integer                                  :: frames_per_auxinput15
integer                                  :: auxinput16_oid
character*256                               :: auxinput16_inname
character*256                               :: auxinput16_outname
integer                                  :: auxinput16_interval_y
integer                                  :: auxinput16_interval_d
integer                                  :: auxinput16_interval_h
integer                                  :: auxinput16_interval_m
integer                                  :: auxinput16_interval_s
integer                                  :: auxinput16_interval
integer                                  :: auxinput16_begin_y
integer                                  :: auxinput16_begin_d
integer                                  :: auxinput16_begin_h
integer                                  :: auxinput16_begin_m
integer                                  :: auxinput16_begin_s
integer                                  :: auxinput16_begin
integer                                  :: auxinput16_end_y
integer                                  :: auxinput16_end_d
integer                                  :: auxinput16_end_h
integer                                  :: auxinput16_end_m
integer                                  :: auxinput16_end_s
integer                                  :: auxinput16_end
integer                                  :: io_form_auxinput16
integer                                  :: frames_per_auxinput16
integer                                  :: auxinput17_oid
character*256                               :: auxinput17_inname
character*256                               :: auxinput17_outname
integer                                  :: auxinput17_interval_y
integer                                  :: auxinput17_interval_d
integer                                  :: auxinput17_interval_h
integer                                  :: auxinput17_interval_m
integer                                  :: auxinput17_interval_s
integer                                  :: auxinput17_interval
integer                                  :: auxinput17_begin_y
integer                                  :: auxinput17_begin_d
integer                                  :: auxinput17_begin_h
integer                                  :: auxinput17_begin_m
integer                                  :: auxinput17_begin_s
integer                                  :: auxinput17_begin
integer                                  :: auxinput17_end_y
integer                                  :: auxinput17_end_d
integer                                  :: auxinput17_end_h
integer                                  :: auxinput17_end_m
integer                                  :: auxinput17_end_s
integer                                  :: auxinput17_end
integer                                  :: io_form_auxinput17
integer                                  :: frames_per_auxinput17
integer                                  :: auxinput18_oid
character*256                               :: auxinput18_inname
character*256                               :: auxinput18_outname
integer                                  :: auxinput18_interval_y
integer                                  :: auxinput18_interval_d
integer                                  :: auxinput18_interval_h
integer                                  :: auxinput18_interval_m
integer                                  :: auxinput18_interval_s
integer                                  :: auxinput18_interval
integer                                  :: auxinput18_begin_y
integer                                  :: auxinput18_begin_d
integer                                  :: auxinput18_begin_h
integer                                  :: auxinput18_begin_m
integer                                  :: auxinput18_begin_s
integer                                  :: auxinput18_begin
integer                                  :: auxinput18_end_y
integer                                  :: auxinput18_end_d
integer                                  :: auxinput18_end_h
integer                                  :: auxinput18_end_m
integer                                  :: auxinput18_end_s
integer                                  :: auxinput18_end
integer                                  :: io_form_auxinput18
integer                                  :: frames_per_auxinput18
integer                                  :: auxinput19_oid
character*256                               :: auxinput19_inname
character*256                               :: auxinput19_outname
integer                                  :: auxinput19_interval_y
integer                                  :: auxinput19_interval_d
integer                                  :: auxinput19_interval_h
integer                                  :: auxinput19_interval_m
integer                                  :: auxinput19_interval_s
integer                                  :: auxinput19_interval
integer                                  :: auxinput19_begin_y
integer                                  :: auxinput19_begin_d
integer                                  :: auxinput19_begin_h
integer                                  :: auxinput19_begin_m
integer                                  :: auxinput19_begin_s
integer                                  :: auxinput19_begin
integer                                  :: auxinput19_end_y
integer                                  :: auxinput19_end_d
integer                                  :: auxinput19_end_h
integer                                  :: auxinput19_end_m
integer                                  :: auxinput19_end_s
integer                                  :: auxinput19_end
integer                                  :: io_form_auxinput19
integer                                  :: frames_per_auxinput19
integer                                  :: auxinput20_oid
character*256                               :: auxinput20_inname
character*256                               :: auxinput20_outname
integer                                  :: auxinput20_interval_y
integer                                  :: auxinput20_interval_d
integer                                  :: auxinput20_interval_h
integer                                  :: auxinput20_interval_m
integer                                  :: auxinput20_interval_s
integer                                  :: auxinput20_interval
integer                                  :: auxinput20_begin_y
integer                                  :: auxinput20_begin_d
integer                                  :: auxinput20_begin_h
integer                                  :: auxinput20_begin_m
integer                                  :: auxinput20_begin_s
integer                                  :: auxinput20_begin
integer                                  :: auxinput20_end_y
integer                                  :: auxinput20_end_d
integer                                  :: auxinput20_end_h
integer                                  :: auxinput20_end_m
integer                                  :: auxinput20_end_s
integer                                  :: auxinput20_end
integer                                  :: io_form_auxinput20
integer                                  :: frames_per_auxinput20
integer                                  :: auxinput21_oid
character*256                               :: auxinput21_inname
character*256                               :: auxinput21_outname
integer                                  :: auxinput21_interval_y
integer                                  :: auxinput21_interval_d
integer                                  :: auxinput21_interval_h
integer                                  :: auxinput21_interval_m
integer                                  :: auxinput21_interval_s
integer                                  :: auxinput21_interval
integer                                  :: auxinput21_begin_y
integer                                  :: auxinput21_begin_d
integer                                  :: auxinput21_begin_h
integer                                  :: auxinput21_begin_m
integer                                  :: auxinput21_begin_s
integer                                  :: auxinput21_begin
integer                                  :: auxinput21_end_y
integer                                  :: auxinput21_end_d
integer                                  :: auxinput21_end_h
integer                                  :: auxinput21_end_m
integer                                  :: auxinput21_end_s
integer                                  :: auxinput21_end
integer                                  :: io_form_auxinput21
integer                                  :: frames_per_auxinput21
integer                                  :: auxinput22_oid
character*256                               :: auxinput22_inname
character*256                               :: auxinput22_outname
integer                                  :: auxinput22_interval_y
integer                                  :: auxinput22_interval_d
integer                                  :: auxinput22_interval_h
integer                                  :: auxinput22_interval_m
integer                                  :: auxinput22_interval_s
integer                                  :: auxinput22_interval
integer                                  :: auxinput22_begin_y
integer                                  :: auxinput22_begin_d
integer                                  :: auxinput22_begin_h
integer                                  :: auxinput22_begin_m
integer                                  :: auxinput22_begin_s
integer                                  :: auxinput22_begin
integer                                  :: auxinput22_end_y
integer                                  :: auxinput22_end_d
integer                                  :: auxinput22_end_h
integer                                  :: auxinput22_end_m
integer                                  :: auxinput22_end_s
integer                                  :: auxinput22_end
integer                                  :: io_form_auxinput22
integer                                  :: frames_per_auxinput22
integer                                  :: auxinput23_oid
character*256                               :: auxinput23_inname
character*256                               :: auxinput23_outname
integer                                  :: auxinput23_interval_y
integer                                  :: auxinput23_interval_d
integer                                  :: auxinput23_interval_h
integer                                  :: auxinput23_interval_m
integer                                  :: auxinput23_interval_s
integer                                  :: auxinput23_interval
integer                                  :: auxinput23_begin_y
integer                                  :: auxinput23_begin_d
integer                                  :: auxinput23_begin_h
integer                                  :: auxinput23_begin_m
integer                                  :: auxinput23_begin_s
integer                                  :: auxinput23_begin
integer                                  :: auxinput23_end_y
integer                                  :: auxinput23_end_d
integer                                  :: auxinput23_end_h
integer                                  :: auxinput23_end_m
integer                                  :: auxinput23_end_s
integer                                  :: auxinput23_end
integer                                  :: io_form_auxinput23
integer                                  :: frames_per_auxinput23
integer                                  :: auxinput24_oid
character*256                               :: auxinput24_inname
character*256                               :: auxinput24_outname
integer                                  :: auxinput24_interval_y
integer                                  :: auxinput24_interval_d
integer                                  :: auxinput24_interval_h
integer                                  :: auxinput24_interval_m
integer                                  :: auxinput24_interval_s
integer                                  :: auxinput24_interval
integer                                  :: auxinput24_begin_y
integer                                  :: auxinput24_begin_d
integer                                  :: auxinput24_begin_h
integer                                  :: auxinput24_begin_m
integer                                  :: auxinput24_begin_s
integer                                  :: auxinput24_begin
integer                                  :: auxinput24_end_y
integer                                  :: auxinput24_end_d
integer                                  :: auxinput24_end_h
integer                                  :: auxinput24_end_m
integer                                  :: auxinput24_end_s
integer                                  :: auxinput24_end
integer                                  :: io_form_auxinput24
integer                                  :: frames_per_auxinput24
integer                                  :: oid
integer                                  :: history_interval
integer                                  :: frames_per_outfile
logical                                  :: restart
integer                                  :: restart_interval
integer                                  :: io_form_input
integer                                  :: io_form_history
integer                                  :: io_form_restart
integer                                  :: io_form_boundary
integer                                  :: debug_level
logical                                  :: self_test_domain
character*256                               :: history_outname
character*256                               :: history_inname
logical                                  :: use_netcdf_classic
integer                                  :: history_interval_d
integer                                  :: history_interval_h
integer                                  :: history_interval_m
integer                                  :: history_interval_s
integer                                  :: inputout_interval_d
integer                                  :: inputout_interval_h
integer                                  :: inputout_interval_m
integer                                  :: inputout_interval_s
integer                                  :: inputout_interval
integer                                  :: restart_interval_d
integer                                  :: restart_interval_h
integer                                  :: restart_interval_m
integer                                  :: restart_interval_s
integer                                  :: history_begin_y
integer                                  :: history_begin_d
integer                                  :: history_begin_h
integer                                  :: history_begin_m
integer                                  :: history_begin_s
integer                                  :: history_begin
integer                                  :: inputout_begin_y
integer                                  :: inputout_begin_d
integer                                  :: inputout_begin_h
integer                                  :: inputout_begin_m
integer                                  :: inputout_begin_s
integer                                  :: restart_begin_y
integer                                  :: restart_begin_d
integer                                  :: restart_begin_h
integer                                  :: restart_begin_m
integer                                  :: restart_begin_s
integer                                  :: restart_begin
integer                                  :: history_end_y
integer                                  :: history_end_d
integer                                  :: history_end_h
integer                                  :: history_end_m
integer                                  :: history_end_s
integer                                  :: history_end
integer                                  :: inputout_end_y
integer                                  :: inputout_end_d
integer                                  :: inputout_end_h
integer                                  :: inputout_end_m
integer                                  :: inputout_end_s
integer                                  :: simulation_start_year
integer                                  :: simulation_start_month
integer                                  :: simulation_start_day
integer                                  :: simulation_start_hour
integer                                  :: simulation_start_minute
integer                                  :: simulation_start_second
logical                                  :: reset_simulation_start
integer                                  :: sr_x
integer                                  :: sr_y
character*256                               :: sgfdda_inname
character*256                               :: gfdda_inname
integer                                  :: sgfdda_interval_d
integer                                  :: sgfdda_interval_h
integer                                  :: sgfdda_interval_m
integer                                  :: sgfdda_interval_s
integer                                  :: sgfdda_interval_y
integer                                  :: sgfdda_interval
integer                                  :: gfdda_interval_d
integer                                  :: gfdda_interval_h
integer                                  :: gfdda_interval_m
integer                                  :: gfdda_interval_s
integer                                  :: gfdda_interval_y
integer                                  :: gfdda_interval
integer                                  :: sgfdda_begin_y
integer                                  :: sgfdda_begin_d
integer                                  :: sgfdda_begin_h
integer                                  :: sgfdda_begin_m
integer                                  :: sgfdda_begin_s
integer                                  :: gfdda_begin_y
integer                                  :: gfdda_begin_d
integer                                  :: gfdda_begin_h
integer                                  :: gfdda_begin_m
integer                                  :: gfdda_begin_s
integer                                  :: sgfdda_end_y
integer                                  :: sgfdda_end_d
integer                                  :: sgfdda_end_h
integer                                  :: sgfdda_end_m
integer                                  :: sgfdda_end_s
integer                                  :: gfdda_end_y
integer                                  :: gfdda_end_d
integer                                  :: gfdda_end_h
integer                                  :: gfdda_end_m
integer                                  :: gfdda_end_s
integer                                  :: io_form_sgfdda
integer                                  :: io_form_gfdda
character*256                               :: iofields_filename
logical                                  :: ignore_iofields_warning
logical                                  :: ncd_nofill
integer                                  :: ifire
integer                                  :: fire_boundary_guard
integer                                  :: fire_num_ignitions
real                                     :: fire_ignition_ros1
real                                     :: fire_ignition_start_lon1
real                                     :: fire_ignition_start_lat1
real                                     :: fire_ignition_end_lon1
real                                     :: fire_ignition_end_lat1
real                                     :: fire_ignition_radius1
real                                     :: fire_ignition_start_time1
real                                     :: fire_ignition_end_time1
real                                     :: fire_ignition_ros2
real                                     :: fire_ignition_start_lon2
real                                     :: fire_ignition_start_lat2
real                                     :: fire_ignition_end_lon2
real                                     :: fire_ignition_end_lat2
real                                     :: fire_ignition_radius2
real                                     :: fire_ignition_start_time2
real                                     :: fire_ignition_end_time2
real                                     :: fire_ignition_ros3
real                                     :: fire_ignition_start_lon3
real                                     :: fire_ignition_start_lat3
real                                     :: fire_ignition_end_lon3
real                                     :: fire_ignition_end_lat3
real                                     :: fire_ignition_radius3
real                                     :: fire_ignition_start_time3
real                                     :: fire_ignition_end_time3
real                                     :: fire_ignition_ros4
real                                     :: fire_ignition_start_lon4
real                                     :: fire_ignition_start_lat4
real                                     :: fire_ignition_end_lon4
real                                     :: fire_ignition_end_lat4
real                                     :: fire_ignition_radius4
real                                     :: fire_ignition_start_time4
real                                     :: fire_ignition_end_time4
real                                     :: fire_ignition_ros5
real                                     :: fire_ignition_start_lon5
real                                     :: fire_ignition_start_lat5
real                                     :: fire_ignition_end_lon5
real                                     :: fire_ignition_end_lat5
real                                     :: fire_ignition_radius5
real                                     :: fire_ignition_start_time5
real                                     :: fire_ignition_end_time5
real                                     :: fire_ignition_start_x1
real                                     :: fire_ignition_start_y1
real                                     :: fire_ignition_end_x1
real                                     :: fire_ignition_end_y1
real                                     :: fire_ignition_start_x2
real                                     :: fire_ignition_start_y2
real                                     :: fire_ignition_end_x2
real                                     :: fire_ignition_end_y2
real                                     :: fire_ignition_start_x3
real                                     :: fire_ignition_start_y3
real                                     :: fire_ignition_end_x3
real                                     :: fire_ignition_end_y3
real                                     :: fire_ignition_start_x4
real                                     :: fire_ignition_start_y4
real                                     :: fire_ignition_end_x4
real                                     :: fire_ignition_end_y4
real                                     :: fire_ignition_start_x5
real                                     :: fire_ignition_start_y5
real                                     :: fire_ignition_end_x5
real                                     :: fire_ignition_end_y5
real                                     :: fire_lat_init
real                                     :: fire_lon_init
real                                     :: fire_ign_time
integer                                  :: fire_shape
integer                                  :: fire_sprd_mdl
real                                     :: fire_crwn_hgt
real                                     :: fire_ext_grnd
real                                     :: fire_ext_crwn
real                                     :: fire_wind_height
integer                                  :: fire_fuel_read
integer                                  :: fire_fuel_cat
integer                                  :: fire_print_msg
integer                                  :: fire_print_file
integer                                  :: fire_fuel_left_method
integer                                  :: fire_fuel_left_irl
integer                                  :: fire_fuel_left_jrl
real                                     :: fire_back_weight
integer                                  :: fire_grows_only
integer                                  :: fire_upwinding
integer                                  :: fire_upwind_split
real                                     :: fire_viscosity
real                                     :: fire_lfn_ext_up
integer                                  :: fire_topo_from_atm
integer                                  :: fire_advection
integer                                  :: fire_test_steps
real                                     :: fire_const_time
real                                     :: fire_const_grnhfx
real                                     :: fire_const_grnqfx
real                                     :: fire_atm_feedback
integer                                  :: fire_mountain_type
real                                     :: fire_mountain_height
real                                     :: fire_mountain_start_x
real                                     :: fire_mountain_start_y
real                                     :: fire_mountain_end_x
real                                     :: fire_mountain_end_y
real                                     :: delt_perturbation
real                                     :: xrad_perturbation
real                                     :: yrad_perturbation
real                                     :: zrad_perturbation
real                                     :: hght_perturbation
logical                                  :: stretch_grd
logical                                  :: stretch_hyp
real                                     :: z_grd_scale
logical                                  :: sfc_full_init
integer                                  :: sfc_lu_index
real                                     :: sfc_tsk
real                                     :: sfc_tmn
logical                                  :: fire_read_lu
logical                                  :: fire_read_tsk
logical                                  :: fire_read_tmn
logical                                  :: fire_read_atm_ht
logical                                  :: fire_read_fire_ht
logical                                  :: fire_read_atm_grad
logical                                  :: fire_read_fire_grad
real                                     :: sfc_vegfra
real                                     :: sfc_canwat
integer                                  :: sfc_ivgtyp
integer                                  :: sfc_isltyp
integer                                  :: avgflx_count
integer                                  :: do_avgflx_em
integer                                  :: do_avgflx_cugd
integer                                  :: seed1
integer                                  :: seed2
logical                                  :: did_stoch
integer                                  :: stoch_force_opt
integer                                  :: stoch_vertstruc_opt
integer                                  :: nens
real                                     :: tot_backscat_psi
real                                     :: tot_backscat_t
real                                     :: ztau_psi
real                                     :: ztau_t
real                                     :: rexponent_psi
real                                     :: rexponent_t
real                                     :: zsigma2_eps
real                                     :: zsigma2_eta
integer                                  :: kminforc
integer                                  :: lminforc
integer                                  :: kminforct
integer                                  :: lminforct
integer                                  :: kmaxforc
integer                                  :: lmaxforc
integer                                  :: kmaxforct
integer                                  :: lmaxforct
integer                                  :: kmaxforch
integer                                  :: lmaxforch
integer                                  :: kmaxforcth
integer                                  :: lmaxforcth
real                                     :: gridpointvariance
real                                     :: sppt_thresh_fact
real                                     :: l_sppt
real                                     :: tau_sppt
integer                                  :: stoch_force_global_opt
integer                                  :: sfs_opt
integer                                  :: m_opt
logical                                  :: is_cammgmp_used
integer                                  :: lakeflag
integer                                  :: lake_depth_flag
real                                     :: lakedepth_default
real                                     :: lake_min_elev
integer                                  :: use_lakedepth
integer                                  :: p_lev_diags
integer                                  :: p_lev_diags_dfi
integer                                  :: num_press_levels
real                                     :: press_levels
integer                                  :: use_tot_or_hyd_p
real                                     :: p_lev_missing
real                                     :: p_lev_interval
integer                                  :: perturb_bdy
integer                                  :: chem_opt
real      ,DIMENSION(:,:)     ,POINTER   :: xlat
real      ,DIMENSION(:,:)     ,POINTER   :: xlong
real      ,DIMENSION(:,:)     ,POINTER   :: lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: lu_mask
real      ,DIMENSION(:)       ,POINTER   :: znu
real      ,DIMENSION(:)       ,POINTER   :: znw
real      ,DIMENSION(:)       ,POINTER   :: zs
real      ,DIMENSION(:)       ,POINTER   :: dzs
real      ,DIMENSION(:)       ,POINTER   :: traj_i
real      ,DIMENSION(:)       ,POINTER   :: traj_j
real      ,DIMENSION(:)       ,POINTER   :: traj_k
real      ,DIMENSION(:)       ,POINTER   :: traj_long
real      ,DIMENSION(:)       ,POINTER   :: traj_lat
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: rh_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: ght_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: p_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: prho_gc
real      ,DIMENSION(:,:)     ,POINTER   :: xlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: xlong_gc
real      ,DIMENSION(:,:)     ,POINTER   :: ht_gc
real      ,DIMENSION(:,:)     ,POINTER   :: var_sso
real      ,DIMENSION(:,:)     ,POINTER   :: lap_hgt
real      ,DIMENSION(:,:)     ,POINTER   :: tsk_gc
real      ,DIMENSION(:,:)     ,POINTER   :: tavgsfc
real      ,DIMENSION(:,:)     ,POINTER   :: tmn_gc
real      ,DIMENSION(:,:)     ,POINTER   :: pslv_gc
real      ,DIMENSION(:,:)     ,POINTER   :: sct_dom_gc
real      ,DIMENSION(:,:)     ,POINTER   :: scb_dom_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: greenfrac
real      ,DIMENSION(:,:,:)   ,POINTER   :: albedo12m
real      ,DIMENSION(:,:,:)   ,POINTER   :: lai12m
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: pdrho_gc
real      ,DIMENSION(:,:)     ,POINTER   :: psfc_gc
real      ,DIMENSION(:,:)     ,POINTER   :: intq_gc
real      ,DIMENSION(:,:)     ,POINTER   :: pdhs
real      ,DIMENSION(:,:,:)   ,POINTER   :: qv_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh_gc
real      ,DIMENSION(:,:)     ,POINTER   :: icefrac_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qr_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qc_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qs_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qi_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qg_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qh_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qni_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnr_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_now
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_jan
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_feb
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_mar
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_apr
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_may
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_jun
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_jul
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_aug
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_sep
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_oct
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_nov
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnwfa_dec
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_now
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_jan
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_feb
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_mar
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_apr
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_may
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_jun
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_jul
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_aug
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_sep
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_oct
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_nov
real      ,DIMENSION(:,:,:)   ,POINTER   :: qnifa_dec
real      ,DIMENSION(:,:,:)   ,POINTER   :: qntemp
real      ,DIMENSION(:,:)     ,POINTER   :: qntemp2
real      ,DIMENSION(:,:)     ,POINTER   :: t_max_p
real      ,DIMENSION(:,:)     ,POINTER   :: ght_max_p
real      ,DIMENSION(:,:)     ,POINTER   :: max_p
real      ,DIMENSION(:,:)     ,POINTER   :: t_min_p
real      ,DIMENSION(:,:)     ,POINTER   :: ght_min_p
real      ,DIMENSION(:,:)     ,POINTER   :: min_p
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_save
real      ,DIMENSION(:)       ,POINTER   :: z_force
real      ,DIMENSION(:)       ,POINTER   :: z_force_tend
real      ,DIMENSION(:)       ,POINTER   :: u_g
real      ,DIMENSION(:)       ,POINTER   :: u_g_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_m
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_save
real      ,DIMENSION(:)       ,POINTER   :: v_g
real      ,DIMENSION(:)       ,POINTER   :: v_g_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: rw
real      ,DIMENSION(:,:,:)   ,POINTER   :: ww_m
real      ,DIMENSION(:)       ,POINTER   :: w_subs
real      ,DIMENSION(:)       ,POINTER   :: w_subs_tend
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: phb_fine
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph0
real      ,DIMENSION(:,:,:)   ,POINTER   :: php
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_init
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_save
real      ,DIMENSION(:)       ,POINTER   :: th_upstream_x
real      ,DIMENSION(:)       ,POINTER   :: th_upstream_x_tend
real      ,DIMENSION(:)       ,POINTER   :: th_upstream_y
real      ,DIMENSION(:)       ,POINTER   :: th_upstream_y_tend
real      ,DIMENSION(:)       ,POINTER   :: qv_upstream_x
real      ,DIMENSION(:)       ,POINTER   :: qv_upstream_x_tend
real      ,DIMENSION(:)       ,POINTER   :: qv_upstream_y
real      ,DIMENSION(:)       ,POINTER   :: qv_upstream_y_tend
real      ,DIMENSION(:)       ,POINTER   :: ql_upstream_x
real      ,DIMENSION(:)       ,POINTER   :: ql_upstream_x_tend
real      ,DIMENSION(:)       ,POINTER   :: ql_upstream_y
real      ,DIMENSION(:)       ,POINTER   :: ql_upstream_y_tend
real      ,DIMENSION(:)       ,POINTER   :: u_upstream_x
real      ,DIMENSION(:)       ,POINTER   :: u_upstream_x_tend
real      ,DIMENSION(:)       ,POINTER   :: u_upstream_y
real      ,DIMENSION(:)       ,POINTER   :: u_upstream_y_tend
real      ,DIMENSION(:)       ,POINTER   :: v_upstream_x
real      ,DIMENSION(:)       ,POINTER   :: v_upstream_x_tend
real      ,DIMENSION(:)       ,POINTER   :: v_upstream_y
real      ,DIMENSION(:)       ,POINTER   :: v_upstream_y_tend
real      ,DIMENSION(:)       ,POINTER   :: th_t_tend
real      ,DIMENSION(:)       ,POINTER   :: qv_t_tend
real      ,DIMENSION(:)       ,POINTER   :: th_largescale
real      ,DIMENSION(:)       ,POINTER   :: th_largescale_tend
real      ,DIMENSION(:)       ,POINTER   :: qv_largescale
real      ,DIMENSION(:)       ,POINTER   :: qv_largescale_tend
real      ,DIMENSION(:)       ,POINTER   :: ql_largescale
real      ,DIMENSION(:)       ,POINTER   :: ql_largescale_tend
real      ,DIMENSION(:)       ,POINTER   :: u_largescale
real      ,DIMENSION(:)       ,POINTER   :: u_largescale_tend
real      ,DIMENSION(:)       ,POINTER   :: v_largescale
real      ,DIMENSION(:)       ,POINTER   :: v_largescale_tend
real      ,DIMENSION(:)       ,POINTER   :: tau_largescale
real      ,DIMENSION(:)       ,POINTER   :: tau_largescale_tend
real      ,DIMENSION(:)       ,POINTER   :: tau_x
real      ,DIMENSION(:)       ,POINTER   :: tau_x_tend
real      ,DIMENSION(:)       ,POINTER   :: tau_y
real      ,DIMENSION(:)       ,POINTER   :: tau_y_tend
real      ,DIMENSION(:)       ,POINTER   :: t_soil_forcing_val
real      ,DIMENSION(:)       ,POINTER   :: t_soil_forcing_tend
real      ,DIMENSION(:)       ,POINTER   :: q_soil_forcing_val
real      ,DIMENSION(:)       ,POINTER   :: q_soil_forcing_tend
real      ,DIMENSION(:)       ,POINTER   :: tau_soil
real      ,DIMENSION(:)       ,POINTER   :: soil_depth_force
real      ,DIMENSION(:,:)     ,POINTER   :: mu_1
real      ,DIMENSION(:,:)     ,POINTER   :: mu_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu_btye
real      ,DIMENSION(:,:)     ,POINTER   :: mub
real      ,DIMENSION(:,:)     ,POINTER   :: mub_fine
real      ,DIMENSION(:,:)     ,POINTER   :: mub_save
real      ,DIMENSION(:,:)     ,POINTER   :: mu0
real      ,DIMENSION(:,:)     ,POINTER   :: mudf
real      ,DIMENSION(:,:)     ,POINTER   :: muu
real      ,DIMENSION(:,:)     ,POINTER   :: muv
real      ,DIMENSION(:,:)     ,POINTER   :: mut
real      ,DIMENSION(:,:)     ,POINTER   :: muts
real      ,DIMENSION(:,:)     ,POINTER   :: nest_pos
real      ,DIMENSION(:,:)     ,POINTER   :: nest_mask
real      ,DIMENSION(:,:)     ,POINTER   :: ht_coarse
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_1
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_2
real      ,DIMENSION(:,:,:)   ,POINTER   :: p
real      ,DIMENSION(:,:,:)   ,POINTER   :: al
real      ,DIMENSION(:,:,:)   ,POINTER   :: alt
real      ,DIMENSION(:,:,:)   ,POINTER   :: alb
real      ,DIMENSION(:,:,:)   ,POINTER   :: zx
real      ,DIMENSION(:,:,:)   ,POINTER   :: zy
real      ,DIMENSION(:,:,:)   ,POINTER   :: rdz
real      ,DIMENSION(:,:,:)   ,POINTER   :: rdzw
real      ,DIMENSION(:,:,:)   ,POINTER   :: pb
real      ,DIMENSION(:,:,:)   ,POINTER   :: rho
real      ,DIMENSION(:)       ,POINTER   :: fnm
real      ,DIMENSION(:)       ,POINTER   :: fnp
real      ,DIMENSION(:)       ,POINTER   :: rdnw
real      ,DIMENSION(:)       ,POINTER   :: rdn
real      ,DIMENSION(:)       ,POINTER   :: dnw
real      ,DIMENSION(:)       ,POINTER   :: dn
real      ,DIMENSION(:)       ,POINTER   :: t_base
real      ,DIMENSION(:,:,:)   ,POINTER   :: z
real      ,DIMENSION(:,:,:)   ,POINTER   :: z_at_w
real      ,DIMENSION(:,:,:)   ,POINTER   :: p_hyd
real      ,DIMENSION(:,:,:)   ,POINTER   :: p_hyd_w
real      ,DIMENSION(:,:)     ,POINTER   :: q2
real      ,DIMENSION(:,:)     ,POINTER   :: t2
real      ,DIMENSION(:,:)     ,POINTER   :: th2
real      ,DIMENSION(:,:)     ,POINTER   :: psfc
real      ,DIMENSION(:,:)     ,POINTER   :: u10
real      ,DIMENSION(:,:)     ,POINTER   :: v10
real      ,DIMENSION(:,:)     ,POINTER   :: uratx
real      ,DIMENSION(:,:)     ,POINTER   :: vratx
real      ,DIMENSION(:,:)     ,POINTER   :: tratx
real      ,DIMENSION(:,:,:,:) ,POINTER   :: obs_savwt
real      ,DIMENSION(:,:)     ,POINTER   :: power
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_nostag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xstag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_ystag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xystag
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: rimi
real      ,DIMENSION(:,:)     ,POINTER   :: qnwfa2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: re_cloud
real      ,DIMENSION(:,:,:)   ,POINTER   :: re_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: re_snow
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_re_cloud
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_re_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_re_snow
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btye
real      ,DIMENSION(:)       ,POINTER   :: fcx
real      ,DIMENSION(:)       ,POINTER   :: gcx
real      ,DIMENSION(:,:,:)   ,POINTER   :: soil_layers
real      ,DIMENSION(:,:,:)   ,POINTER   :: soil_levels
real      ,DIMENSION(:,:,:)   ,POINTER   :: st
real      ,DIMENSION(:,:,:)   ,POINTER   :: sm
real      ,DIMENSION(:,:,:)   ,POINTER   :: sw
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilt
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilm
real      ,DIMENSION(:,:)     ,POINTER   :: sm000007
real      ,DIMENSION(:,:)     ,POINTER   :: sm007028
real      ,DIMENSION(:,:)     ,POINTER   :: sm028100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100255
real      ,DIMENSION(:,:)     ,POINTER   :: st000007
real      ,DIMENSION(:,:)     ,POINTER   :: st007028
real      ,DIMENSION(:,:)     ,POINTER   :: st028100
real      ,DIMENSION(:,:)     ,POINTER   :: st100255
real      ,DIMENSION(:,:)     ,POINTER   :: sm000010
real      ,DIMENSION(:,:)     ,POINTER   :: sm010040
real      ,DIMENSION(:,:)     ,POINTER   :: sm040100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100200
real      ,DIMENSION(:,:)     ,POINTER   :: sm010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilm000
real      ,DIMENSION(:,:)     ,POINTER   :: soilm005
real      ,DIMENSION(:,:)     ,POINTER   :: soilm020
real      ,DIMENSION(:,:)     ,POINTER   :: soilm040
real      ,DIMENSION(:,:)     ,POINTER   :: soilm160
real      ,DIMENSION(:,:)     ,POINTER   :: soilm300
real      ,DIMENSION(:,:)     ,POINTER   :: sw000010
real      ,DIMENSION(:,:)     ,POINTER   :: sw010040
real      ,DIMENSION(:,:)     ,POINTER   :: sw040100
real      ,DIMENSION(:,:)     ,POINTER   :: sw100200
real      ,DIMENSION(:,:)     ,POINTER   :: sw010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilw000
real      ,DIMENSION(:,:)     ,POINTER   :: soilw005
real      ,DIMENSION(:,:)     ,POINTER   :: soilw020
real      ,DIMENSION(:,:)     ,POINTER   :: soilw040
real      ,DIMENSION(:,:)     ,POINTER   :: soilw160
real      ,DIMENSION(:,:)     ,POINTER   :: soilw300
real      ,DIMENSION(:,:)     ,POINTER   :: st000010
real      ,DIMENSION(:,:)     ,POINTER   :: st010040
real      ,DIMENSION(:,:)     ,POINTER   :: st040100
real      ,DIMENSION(:,:)     ,POINTER   :: st100200
real      ,DIMENSION(:,:)     ,POINTER   :: st010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilt000
real      ,DIMENSION(:,:)     ,POINTER   :: soilt005
real      ,DIMENSION(:,:)     ,POINTER   :: soilt020
real      ,DIMENSION(:,:)     ,POINTER   :: soilt040
real      ,DIMENSION(:,:)     ,POINTER   :: soilt160
real      ,DIMENSION(:,:)     ,POINTER   :: soilt300
real      ,DIMENSION(:,:)     ,POINTER   :: topostdv
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpx
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpy
real      ,DIMENSION(:,:)     ,POINTER   :: slope
real      ,DIMENSION(:,:)     ,POINTER   :: slp_azi
real      ,DIMENSION(:,:)     ,POINTER   :: shdmax
real      ,DIMENSION(:,:)     ,POINTER   :: shdmin
real      ,DIMENSION(:,:)     ,POINTER   :: snoalb
real      ,DIMENSION(:,:)     ,POINTER   :: slopecat
real      ,DIMENSION(:,:)     ,POINTER   :: toposoil
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot
real      ,DIMENSION(:,:)     ,POINTER   :: soilcat
real      ,DIMENSION(:,:)     ,POINTER   :: vegcat
real      ,DIMENSION(:,:,:)   ,POINTER   :: tslb
real      ,DIMENSION(:,:)     ,POINTER   :: ts_hour
real      ,DIMENSION(:,:)     ,POINTER   :: ts_u
real      ,DIMENSION(:,:)     ,POINTER   :: ts_v
real      ,DIMENSION(:,:)     ,POINTER   :: ts_q
real      ,DIMENSION(:,:)     ,POINTER   :: ts_t
real      ,DIMENSION(:,:)     ,POINTER   :: ts_psfc
real      ,DIMENSION(:,:)     ,POINTER   :: ts_glw
real      ,DIMENSION(:,:)     ,POINTER   :: ts_gsw
real      ,DIMENSION(:,:)     ,POINTER   :: ts_hfx
real      ,DIMENSION(:,:)     ,POINTER   :: ts_lh
real      ,DIMENSION(:,:)     ,POINTER   :: ts_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: ts_tslb
real      ,DIMENSION(:,:)     ,POINTER   :: ts_clw
real      ,DIMENSION(:,:)     ,POINTER   :: ts_rainc
real      ,DIMENSION(:,:)     ,POINTER   :: ts_rainnc
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_u_profile
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_v_profile
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_gph_profile
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_th_profile
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_qv_profile
real      ,DIMENSION(:)       ,POINTER   :: dzr
real      ,DIMENSION(:)       ,POINTER   :: dzb
real      ,DIMENSION(:)       ,POINTER   :: dzg
real      ,DIMENSION(:,:,:)   ,POINTER   :: urb_param
real      ,DIMENSION(:,:)     ,POINTER   :: lp_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: hi_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: lb_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: hgt_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: fad0_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: fad135_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: fad45_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: pad_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: fad90_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: rad_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: mh_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: stdh_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: lf_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: car_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: h2w_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: svf_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: z0s_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: z0r_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: z0m_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: zds_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: zdm_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: zdr_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh2o
real      ,DIMENSION(:,:,:)   ,POINTER   :: smcrel
real      ,DIMENSION(:,:)     ,POINTER   :: xice
real      ,DIMENSION(:,:)     ,POINTER   :: icedepth
real      ,DIMENSION(:,:)     ,POINTER   :: xicem
real      ,DIMENSION(:,:)     ,POINTER   :: albsi
real      ,DIMENSION(:,:)     ,POINTER   :: snowsi
real      ,DIMENSION(:,:)     ,POINTER   :: smstav
real      ,DIMENSION(:,:)     ,POINTER   :: smstot
real      ,DIMENSION(:,:)     ,POINTER   :: soldrain
real      ,DIMENSION(:,:)     ,POINTER   :: sfcheadrt
real      ,DIMENSION(:,:)     ,POINTER   :: infxsrt
real      ,DIMENSION(:,:)     ,POINTER   :: sfcrunoff
real      ,DIMENSION(:,:)     ,POINTER   :: udrunoff
integer   ,DIMENSION(:,:)     ,POINTER   :: ivgtyp
integer   ,DIMENSION(:,:)     ,POINTER   :: isltyp
real      ,DIMENSION(:,:)     ,POINTER   :: vegfra
real      ,DIMENSION(:,:)     ,POINTER   :: sfcevp
real      ,DIMENSION(:,:)     ,POINTER   :: grdflx
real      ,DIMENSION(:,:)     ,POINTER   :: acgrdflx
real      ,DIMENSION(:,:)     ,POINTER   :: sfcexc
real      ,DIMENSION(:,:)     ,POINTER   :: acsnow
real      ,DIMENSION(:,:)     ,POINTER   :: acsnom
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: snowh
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
real      ,DIMENSION(:,:)     ,POINTER   :: sstsk
real      ,DIMENSION(:,:)     ,POINTER   :: lake_depth
real      ,DIMENSION(:,:)     ,POINTER   :: dtw
real      ,DIMENSION(:,:)     ,POINTER   :: uoce
real      ,DIMENSION(:,:)     ,POINTER   :: voce
real      ,DIMENSION(:)       ,POINTER   :: hcoeff
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_p
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_al
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_mu
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_phb
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_ph0
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_php
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_u
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_v
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_w
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_ww
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_t
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_rh
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_ph
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_pb
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_alt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_tke
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_tten_rad
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_tslb
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_smois
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snow
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snowh
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_canwat
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_smfr3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_keepfr3dflag
real      ,DIMENSION(:,:)     ,POINTER   :: tr_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: tb_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: tg_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: tc_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: qc_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: uc_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxr_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxb_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxg_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: xxxc_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: trl_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tbl_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tgl_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: sh_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: lh_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: g_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: rn_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: ts_urb2d
real      ,DIMENSION(:,:)     ,POINTER   :: frc_urb2d
integer   ,DIMENSION(:,:)     ,POINTER   :: utype_urb2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: trb_urb4d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tw1_urb4d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tw2_urb4d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tgb_urb4d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tlev_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: qlev_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tw1lev_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tw2lev_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tglev_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tflev_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: sf_ac_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: lf_ac_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: cm_ac_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: sfvent_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: lfvent_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: sfwin1_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: sfwin2_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: sfw1_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: sfw2_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: sfr_urb3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: sfg_urb3d
real      ,DIMENSION(:,:)     ,POINTER   :: cmr_sfcdif
real      ,DIMENSION(:,:)     ,POINTER   :: chr_sfcdif
real      ,DIMENSION(:,:)     ,POINTER   :: cmc_sfcdif
real      ,DIMENSION(:,:)     ,POINTER   :: chc_sfcdif
real      ,DIMENSION(:,:)     ,POINTER   :: coszen
real      ,DIMENSION(:,:)     ,POINTER   :: hrang
real      ,DIMENSION(:,:,:)   ,POINTER   :: smfr3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: keepfr3dflag
real      ,DIMENSION(:,:)     ,POINTER   :: swvisdir
real      ,DIMENSION(:,:)     ,POINTER   :: swvisdif
real      ,DIMENSION(:,:)     ,POINTER   :: swnirdir
real      ,DIMENSION(:,:)     ,POINTER   :: swnirdif
real      ,DIMENSION(:,:)     ,POINTER   :: alswvisdir
real      ,DIMENSION(:,:)     ,POINTER   :: alswvisdif
real      ,DIMENSION(:,:)     ,POINTER   :: alswnirdir
real      ,DIMENSION(:,:)     ,POINTER   :: alswnirdif
real      ,DIMENSION(:,:)     ,POINTER   :: ra
real      ,DIMENSION(:,:)     ,POINTER   :: rs
real      ,DIMENSION(:,:)     ,POINTER   :: lai
real      ,DIMENSION(:,:)     ,POINTER   :: vegf_px
real      ,DIMENSION(:,:)     ,POINTER   :: t2obs
real      ,DIMENSION(:,:)     ,POINTER   :: q2obs
real      ,DIMENSION(:,:)     ,POINTER   :: fm
real      ,DIMENSION(:,:)     ,POINTER   :: fh
real      ,DIMENSION(:,:)     ,POINTER   :: wstar_ysu
real      ,DIMENSION(:,:)     ,POINTER   :: delta_ysu
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_h
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_m
real      ,DIMENSION(:,:)     ,POINTER   :: ct
real      ,DIMENSION(:,:)     ,POINTER   :: thz0
real      ,DIMENSION(:,:)     ,POINTER   :: z0
real      ,DIMENSION(:,:)     ,POINTER   :: qz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0
real      ,DIMENSION(:,:)     ,POINTER   :: vz0
real      ,DIMENSION(:,:)     ,POINTER   :: qsfc
real      ,DIMENSION(:,:)     ,POINTER   :: akhs
real      ,DIMENSION(:,:)     ,POINTER   :: akms
integer   ,DIMENSION(:,:)     ,POINTER   :: kpbl
real      ,DIMENSION(:,:)     ,POINTER   :: tshltr
real      ,DIMENSION(:,:)     ,POINTER   :: qshltr
real      ,DIMENSION(:,:)     ,POINTER   :: pshltr
real      ,DIMENSION(:,:)     ,POINTER   :: th10
real      ,DIMENSION(:,:)     ,POINTER   :: q10
real      ,DIMENSION(:,:,:)   ,POINTER   :: massflux_edkf
real      ,DIMENSION(:,:,:)   ,POINTER   :: entr_edkf
real      ,DIMENSION(:,:,:)   ,POINTER   :: detr_edkf
real      ,DIMENSION(:,:,:)   ,POINTER   :: thl_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: thv_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: rc_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: frac_up
real      ,DIMENSION(:,:,:)   ,POINTER   :: rc_mf
real      ,DIMENSION(:,:,:)   ,POINTER   :: te_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: kh_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: km_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: shf_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: qf_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: uw_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: vw_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: wupd_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: mf_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: thup_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: qtup_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: qlup_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: cf3d_temf
real      ,DIMENSION(:,:)     ,POINTER   :: hd_temf
real      ,DIMENSION(:,:)     ,POINTER   :: lcl_temf
real      ,DIMENSION(:,:)     ,POINTER   :: hct_temf
real      ,DIMENSION(:,:)     ,POINTER   :: cfm_temf
real      ,DIMENSION(:,:)     ,POINTER   :: wm_temf
real      ,DIMENSION(:,:,:)   ,POINTER   :: qke
real      ,DIMENSION(:,:,:)   ,POINTER   :: qshear
real      ,DIMENSION(:,:,:)   ,POINTER   :: qbuoy
real      ,DIMENSION(:,:,:)   ,POINTER   :: qdiss
real      ,DIMENSION(:,:,:)   ,POINTER   :: qwt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dqke
real      ,DIMENSION(:,:,:)   ,POINTER   :: tsq
real      ,DIMENSION(:,:,:)   ,POINTER   :: qsq
real      ,DIMENSION(:,:,:)   ,POINTER   :: cov
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh3d
real      ,DIMENSION(:,:)     ,POINTER   :: ch
real      ,DIMENSION(:,:)     ,POINTER   :: fgdp
real      ,DIMENSION(:,:)     ,POINTER   :: dfgdp
real      ,DIMENSION(:,:)     ,POINTER   :: vdfg
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_tke
real      ,DIMENSION(:,:,:)   ,POINTER   :: dtaux3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dtauy3d
real      ,DIMENSION(:,:)     ,POINTER   :: dusfcg
real      ,DIMENSION(:,:)     ,POINTER   :: dvsfcg
real      ,DIMENSION(:,:)     ,POINTER   :: var2d
real      ,DIMENSION(:,:)     ,POINTER   :: oc12d
real      ,DIMENSION(:,:)     ,POINTER   :: oa1
real      ,DIMENSION(:,:)     ,POINTER   :: oa2
real      ,DIMENSION(:,:)     ,POINTER   :: oa3
real      ,DIMENSION(:,:)     ,POINTER   :: oa4
real      ,DIMENSION(:,:)     ,POINTER   :: ol1
real      ,DIMENSION(:,:)     ,POINTER   :: ol2
real      ,DIMENSION(:,:)     ,POINTER   :: ol3
real      ,DIMENSION(:,:)     ,POINTER   :: ol4
real      ,DIMENSION(:,:)     ,POINTER   :: ctopo
real      ,DIMENSION(:,:)     ,POINTER   :: ctopo2
real      ,DIMENSION(:,:,:)   ,POINTER   :: a_u_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: a_v_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: a_t_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: a_q_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: a_e_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: b_u_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: b_v_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: b_t_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: b_q_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: b_e_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: dlg_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: dl_u_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: sf_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: vl_bep
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_pbl
real      ,DIMENSION(:,:,:)   ,POINTER   :: el_pbl
real      ,DIMENSION(:,:,:)   ,POINTER   :: wu_tur
real      ,DIMENSION(:,:,:)   ,POINTER   :: wv_tur
real      ,DIMENSION(:,:,:)   ,POINTER   :: wt_tur
real      ,DIMENSION(:,:,:)   ,POINTER   :: wq_tur
real      ,DIMENSION(:,:)     ,POINTER   :: htop
real      ,DIMENSION(:,:)     ,POINTER   :: hbot
real      ,DIMENSION(:,:)     ,POINTER   :: htopr
real      ,DIMENSION(:,:)     ,POINTER   :: hbotr
real      ,DIMENSION(:,:)     ,POINTER   :: cutop
real      ,DIMENSION(:,:)     ,POINTER   :: cubot
real      ,DIMENSION(:,:)     ,POINTER   :: cuppt
real      ,DIMENSION(:,:)     ,POINTER   :: rswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: rlwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: czmean
real      ,DIMENSION(:,:)     ,POINTER   :: cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: cfracm
real      ,DIMENSION(:,:)     ,POINTER   :: cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: acfrst
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrst
real      ,DIMENSION(:,:)     ,POINTER   :: acfrcv
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrcv
real      ,DIMENSION(:,:,:)   ,POINTER   :: o3rad
real      ,DIMENSION(:,:,:,:,:),POINTER   :: aerodm
real      ,DIMENSION(:)       ,POINTER   :: pina
real      ,DIMENSION(:,:,:,:) ,POINTER   :: aerod
real      ,DIMENSION(:,:)     ,POINTER   :: aodtot
real      ,DIMENSION(:,:,:,:) ,POINTER   :: ozmixm
real      ,DIMENSION(:)       ,POINTER   :: pin
real      ,DIMENSION(:,:)     ,POINTER   :: m_ps_1
real      ,DIMENSION(:,:)     ,POINTER   :: m_ps_2
real      ,DIMENSION(:,:,:,:) ,POINTER   :: aerosolc_1
real      ,DIMENSION(:,:,:,:) ,POINTER   :: aerosolc_2
real      ,DIMENSION(:)       ,POINTER   :: m_hybi
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: qndropsource
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_tmp
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_s
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_depth
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_u
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_v
real      ,DIMENSION(:,:)     ,POINTER   :: om_lat
real      ,DIMENSION(:,:)     ,POINTER   :: om_lon
real      ,DIMENSION(:,:)     ,POINTER   :: om_ml
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_tini
real      ,DIMENSION(:,:,:)   ,POINTER   :: om_sini
real      ,DIMENSION(:,:,:)   ,POINTER   :: h_diabatic
real      ,DIMENSION(:,:)     ,POINTER   :: msft
real      ,DIMENSION(:,:)     ,POINTER   :: msfu
real      ,DIMENSION(:,:)     ,POINTER   :: msfv
real      ,DIMENSION(:,:)     ,POINTER   :: msftx
real      ,DIMENSION(:,:)     ,POINTER   :: msfty
real      ,DIMENSION(:,:)     ,POINTER   :: msfux
real      ,DIMENSION(:,:)     ,POINTER   :: msfuy
real      ,DIMENSION(:,:)     ,POINTER   :: msfvx
real      ,DIMENSION(:,:)     ,POINTER   :: msfvx_inv
real      ,DIMENSION(:,:)     ,POINTER   :: msfvy
real      ,DIMENSION(:,:)     ,POINTER   :: f
real      ,DIMENSION(:,:)     ,POINTER   :: e
real      ,DIMENSION(:,:)     ,POINTER   :: sina
real      ,DIMENSION(:,:)     ,POINTER   :: cosa
real      ,DIMENSION(:,:)     ,POINTER   :: ht
real      ,DIMENSION(:,:)     ,POINTER   :: ht_fine
real      ,DIMENSION(:,:)     ,POINTER   :: ht_int
real      ,DIMENSION(:,:)     ,POINTER   :: ht_input
real      ,DIMENSION(:,:)     ,POINTER   :: ht_smooth
real      ,DIMENSION(:,:)     ,POINTER   :: ht_shad
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: ht_shad_btye
integer   ,DIMENSION(:,:)     ,POINTER   :: shadowmask
real      ,DIMENSION(:,:)     ,POINTER   :: tsk
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: tsk_save
real      ,DIMENSION(:)       ,POINTER   :: u_base
real      ,DIMENSION(:)       ,POINTER   :: v_base
real      ,DIMENSION(:)       ,POINTER   :: qv_base
real      ,DIMENSION(:)       ,POINTER   :: z_base
real      ,DIMENSION(:,:)     ,POINTER   :: tlwdn
real      ,DIMENSION(:,:)     ,POINTER   :: tlwup
real      ,DIMENSION(:,:)     ,POINTER   :: slwdn
real      ,DIMENSION(:,:)     ,POINTER   :: slwup
real      ,DIMENSION(:,:)     ,POINTER   :: tswdn
real      ,DIMENSION(:,:)     ,POINTER   :: tswup
real      ,DIMENSION(:,:)     ,POINTER   :: sswdn
real      ,DIMENSION(:,:)     ,POINTER   :: sswup
real      ,DIMENSION(:,:,:)   ,POINTER   :: rushten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqrshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqcshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqsshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqishten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqgshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqcnshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqinshten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rucuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqrcuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqccuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqscuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqicuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqcncuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqincuten
real      ,DIMENSION(:,:,:)   ,POINTER   :: w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: rainc
real      ,DIMENSION(:,:)     ,POINTER   :: rainsh
real      ,DIMENSION(:,:)     ,POINTER   :: rainnc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_rainc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_rainnc
real      ,DIMENSION(:,:)     ,POINTER   :: pratec
real      ,DIMENSION(:,:)     ,POINTER   :: pratesh
real      ,DIMENSION(:,:)     ,POINTER   :: raincv
real      ,DIMENSION(:,:)     ,POINTER   :: rainshv
real      ,DIMENSION(:,:)     ,POINTER   :: rainncv
real      ,DIMENSION(:,:)     ,POINTER   :: rainbl
real      ,DIMENSION(:,:)     ,POINTER   :: snownc
real      ,DIMENSION(:,:)     ,POINTER   :: graupelnc
real      ,DIMENSION(:,:)     ,POINTER   :: hailnc
real      ,DIMENSION(:,:)     ,POINTER   :: snowncv
real      ,DIMENSION(:,:)     ,POINTER   :: graupelncv
real      ,DIMENSION(:,:)     ,POINTER   :: hailncv
real      ,DIMENSION(:,:,:)   ,POINTER   :: refl_10cm
real      ,DIMENSION(:,:)     ,POINTER   :: nca
integer   ,DIMENSION(:,:)     ,POINTER   :: lowlyr
real      ,DIMENSION(:,:)     ,POINTER   :: mass_flux
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_dp
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_sh
real      ,DIMENSION(:,:)     ,POINTER   :: apr_gr
real      ,DIMENSION(:,:)     ,POINTER   :: apr_w
real      ,DIMENSION(:,:)     ,POINTER   :: apr_mc
real      ,DIMENSION(:,:)     ,POINTER   :: apr_st
real      ,DIMENSION(:,:)     ,POINTER   :: apr_as
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capma
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capme
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capmi
real      ,DIMENSION(:,:)     ,POINTER   :: edt_out
real      ,DIMENSION(:,:)     ,POINTER   :: xmb_shallow
integer   ,DIMENSION(:,:)     ,POINTER   :: k22_shallow
integer   ,DIMENSION(:,:)     ,POINTER   :: kbcon_shallow
integer   ,DIMENSION(:,:)     ,POINTER   :: ktop_shallow
integer   ,DIMENSION(:,:)     ,POINTER   :: k22_deep
integer   ,DIMENSION(:,:)     ,POINTER   :: kbcon_deep
integer   ,DIMENSION(:,:)     ,POINTER   :: ktop_deep
real      ,DIMENSION(:,:,:)   ,POINTER   :: xf_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: pr_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: cugd_tten
real      ,DIMENSION(:,:,:)   ,POINTER   :: cugd_qvten
real      ,DIMENSION(:,:,:)   ,POINTER   :: cugd_ttens
real      ,DIMENSION(:,:,:)   ,POINTER   :: cugd_qvtens
real      ,DIMENSION(:,:,:)   ,POINTER   :: cugd_qcten
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud2
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cldfr
real      ,DIMENSION(:,:)     ,POINTER   :: raincv_a
real      ,DIMENSION(:,:)     ,POINTER   :: raincv_b
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud2_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: qc_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qi_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthften
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvften
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthraten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthratenlw
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthratensw
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_old
real      ,DIMENSION(:,:)     ,POINTER   :: swdown
real      ,DIMENSION(:,:)     ,POINTER   :: swdownc
real      ,DIMENSION(:,:)     ,POINTER   :: gsw
real      ,DIMENSION(:,:)     ,POINTER   :: glw
real      ,DIMENSION(:,:)     ,POINTER   :: swnorm
real      ,DIMENSION(:,:)     ,POINTER   :: swddir
real      ,DIMENSION(:,:)     ,POINTER   :: swddni
real      ,DIMENSION(:,:)     ,POINTER   :: swddif
real      ,DIMENSION(:,:)     ,POINTER   :: gx
real      ,DIMENSION(:,:)     ,POINTER   :: bx
real      ,DIMENSION(:,:)     ,POINTER   :: gg
real      ,DIMENSION(:,:)     ,POINTER   :: bb
real      ,DIMENSION(:,:)     ,POINTER   :: coszen_ref
real      ,DIMENSION(:,:)     ,POINTER   :: swdown_ref
real      ,DIMENSION(:,:)     ,POINTER   :: swddir_ref
real      ,DIMENSION(:,:)     ,POINTER   :: aod5502d
real      ,DIMENSION(:,:)     ,POINTER   :: angexp2d
real      ,DIMENSION(:,:)     ,POINTER   :: aerssa2d
real      ,DIMENSION(:,:)     ,POINTER   :: aerasy2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: aod5503d
real      ,DIMENSION(:,:)     ,POINTER   :: t2min
real      ,DIMENSION(:,:)     ,POINTER   :: t2max
real      ,DIMENSION(:,:)     ,POINTER   :: tt2min
real      ,DIMENSION(:,:)     ,POINTER   :: tt2max
real      ,DIMENSION(:,:)     ,POINTER   :: t2mean
real      ,DIMENSION(:,:)     ,POINTER   :: t2std
real      ,DIMENSION(:,:)     ,POINTER   :: q2min
real      ,DIMENSION(:,:)     ,POINTER   :: q2max
real      ,DIMENSION(:,:)     ,POINTER   :: tq2min
real      ,DIMENSION(:,:)     ,POINTER   :: tq2max
real      ,DIMENSION(:,:)     ,POINTER   :: q2mean
real      ,DIMENSION(:,:)     ,POINTER   :: q2std
real      ,DIMENSION(:,:)     ,POINTER   :: skintempmin
real      ,DIMENSION(:,:)     ,POINTER   :: skintempmax
real      ,DIMENSION(:,:)     ,POINTER   :: tskintempmin
real      ,DIMENSION(:,:)     ,POINTER   :: tskintempmax
real      ,DIMENSION(:,:)     ,POINTER   :: skintempmean
real      ,DIMENSION(:,:)     ,POINTER   :: skintempstd
real      ,DIMENSION(:,:)     ,POINTER   :: u10max
real      ,DIMENSION(:,:)     ,POINTER   :: v10max
real      ,DIMENSION(:,:)     ,POINTER   :: spduv10max
real      ,DIMENSION(:,:)     ,POINTER   :: tspduv10max
real      ,DIMENSION(:,:)     ,POINTER   :: u10mean
real      ,DIMENSION(:,:)     ,POINTER   :: v10mean
real      ,DIMENSION(:,:)     ,POINTER   :: spduv10mean
real      ,DIMENSION(:,:)     ,POINTER   :: u10std
real      ,DIMENSION(:,:)     ,POINTER   :: v10std
real      ,DIMENSION(:,:)     ,POINTER   :: spduv10std
real      ,DIMENSION(:,:)     ,POINTER   :: raincvmax
real      ,DIMENSION(:,:)     ,POINTER   :: rainncvmax
real      ,DIMENSION(:,:)     ,POINTER   :: traincvmax
real      ,DIMENSION(:,:)     ,POINTER   :: trainncvmax
real      ,DIMENSION(:,:)     ,POINTER   :: raincvmean
real      ,DIMENSION(:,:)     ,POINTER   :: rainncvmean
real      ,DIMENSION(:,:)     ,POINTER   :: raincvstd
real      ,DIMENSION(:,:)     ,POINTER   :: rainncvstd
real      ,DIMENSION(:,:)     ,POINTER   :: acswupt
real      ,DIMENSION(:,:)     ,POINTER   :: acswuptc
real      ,DIMENSION(:,:)     ,POINTER   :: acswdnt
real      ,DIMENSION(:,:)     ,POINTER   :: acswdntc
real      ,DIMENSION(:,:)     ,POINTER   :: acswupb
real      ,DIMENSION(:,:)     ,POINTER   :: acswupbc
real      ,DIMENSION(:,:)     ,POINTER   :: acswdnb
real      ,DIMENSION(:,:)     ,POINTER   :: acswdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwupt
real      ,DIMENSION(:,:)     ,POINTER   :: aclwuptc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdnt
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdntc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwupb
real      ,DIMENSION(:,:)     ,POINTER   :: aclwupbc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdnb
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdnbc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswupt
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswuptc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswdnt
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswdntc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswupb
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswupbc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswdnb
integer   ,DIMENSION(:,:)     ,POINTER   :: i_acswdnbc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwupt
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwuptc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwdnt
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwdntc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwupb
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwupbc
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwdnb
integer   ,DIMENSION(:,:)     ,POINTER   :: i_aclwdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: swupt
real      ,DIMENSION(:,:)     ,POINTER   :: swuptc
real      ,DIMENSION(:,:)     ,POINTER   :: swdnt
real      ,DIMENSION(:,:)     ,POINTER   :: swdntc
real      ,DIMENSION(:,:)     ,POINTER   :: swupb
real      ,DIMENSION(:,:)     ,POINTER   :: swupbc
real      ,DIMENSION(:,:)     ,POINTER   :: swdnb
real      ,DIMENSION(:,:)     ,POINTER   :: swdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: lwupt
real      ,DIMENSION(:,:)     ,POINTER   :: lwuptc
real      ,DIMENSION(:,:)     ,POINTER   :: lwdnt
real      ,DIMENSION(:,:)     ,POINTER   :: lwdntc
real      ,DIMENSION(:,:)     ,POINTER   :: lwupb
real      ,DIMENSION(:,:)     ,POINTER   :: lwupbc
real      ,DIMENSION(:,:)     ,POINTER   :: lwdnb
real      ,DIMENSION(:,:)     ,POINTER   :: lwdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: swcf
real      ,DIMENSION(:,:)     ,POINTER   :: lwcf
real      ,DIMENSION(:,:)     ,POINTER   :: olr
real      ,DIMENSION(:,:)     ,POINTER   :: xlat_u
real      ,DIMENSION(:,:)     ,POINTER   :: xlong_u
real      ,DIMENSION(:,:)     ,POINTER   :: xlat_v
real      ,DIMENSION(:,:)     ,POINTER   :: xlong_v
real      ,DIMENSION(:,:)     ,POINTER   :: albedo
real      ,DIMENSION(:,:)     ,POINTER   :: clat
real      ,DIMENSION(:,:)     ,POINTER   :: albbck
real      ,DIMENSION(:,:)     ,POINTER   :: embck
real      ,DIMENSION(:,:)     ,POINTER   :: emiss
real      ,DIMENSION(:,:)     ,POINTER   :: snotime
real      ,DIMENSION(:,:)     ,POINTER   :: noahres
real      ,DIMENSION(:,:)     ,POINTER   :: cldefi
real      ,DIMENSION(:,:,:)   ,POINTER   :: rublten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqcblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqiblten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqniblten
real      ,DIMENSION(:,:)     ,POINTER   :: flx4
real      ,DIMENSION(:,:)     ,POINTER   :: fvb
real      ,DIMENSION(:,:)     ,POINTER   :: fbur
real      ,DIMENSION(:,:)     ,POINTER   :: fgsn
integer   ,DIMENSION(:,:)     ,POINTER   :: isnowxy
real      ,DIMENSION(:,:)     ,POINTER   :: tvxy
real      ,DIMENSION(:,:)     ,POINTER   :: tgxy
real      ,DIMENSION(:,:)     ,POINTER   :: canicexy
real      ,DIMENSION(:,:)     ,POINTER   :: canliqxy
real      ,DIMENSION(:,:)     ,POINTER   :: eahxy
real      ,DIMENSION(:,:)     ,POINTER   :: tahxy
real      ,DIMENSION(:,:)     ,POINTER   :: cmxy
real      ,DIMENSION(:,:)     ,POINTER   :: chxy
real      ,DIMENSION(:,:)     ,POINTER   :: fwetxy
real      ,DIMENSION(:,:)     ,POINTER   :: sneqvoxy
real      ,DIMENSION(:,:)     ,POINTER   :: alboldxy
real      ,DIMENSION(:,:)     ,POINTER   :: qsnowxy
real      ,DIMENSION(:,:)     ,POINTER   :: wslakexy
real      ,DIMENSION(:,:)     ,POINTER   :: zwtxy
real      ,DIMENSION(:,:)     ,POINTER   :: waxy
real      ,DIMENSION(:,:)     ,POINTER   :: wtxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: tsnoxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: zsnsoxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: snicexy
real      ,DIMENSION(:,:,:)   ,POINTER   :: snliqxy
real      ,DIMENSION(:,:)     ,POINTER   :: lfmassxy
real      ,DIMENSION(:,:)     ,POINTER   :: rtmassxy
real      ,DIMENSION(:,:)     ,POINTER   :: stmassxy
real      ,DIMENSION(:,:)     ,POINTER   :: woodxy
real      ,DIMENSION(:,:)     ,POINTER   :: stblcpxy
real      ,DIMENSION(:,:)     ,POINTER   :: fastcpxy
real      ,DIMENSION(:,:)     ,POINTER   :: xsaixy
real      ,DIMENSION(:,:)     ,POINTER   :: taussxy
real      ,DIMENSION(:,:)     ,POINTER   :: t2mvxy
real      ,DIMENSION(:,:)     ,POINTER   :: t2mbxy
real      ,DIMENSION(:,:)     ,POINTER   :: q2mvxy
real      ,DIMENSION(:,:)     ,POINTER   :: q2mbxy
real      ,DIMENSION(:,:)     ,POINTER   :: tradxy
real      ,DIMENSION(:,:)     ,POINTER   :: neexy
real      ,DIMENSION(:,:)     ,POINTER   :: gppxy
real      ,DIMENSION(:,:)     ,POINTER   :: nppxy
real      ,DIMENSION(:,:)     ,POINTER   :: fvegxy
real      ,DIMENSION(:,:)     ,POINTER   :: qinxy
real      ,DIMENSION(:,:)     ,POINTER   :: runsfxy
real      ,DIMENSION(:,:)     ,POINTER   :: runsbxy
real      ,DIMENSION(:,:)     ,POINTER   :: ecanxy
real      ,DIMENSION(:,:)     ,POINTER   :: edirxy
real      ,DIMENSION(:,:)     ,POINTER   :: etranxy
real      ,DIMENSION(:,:)     ,POINTER   :: fsaxy
real      ,DIMENSION(:,:)     ,POINTER   :: firaxy
real      ,DIMENSION(:,:)     ,POINTER   :: aparxy
real      ,DIMENSION(:,:)     ,POINTER   :: psnxy
real      ,DIMENSION(:,:)     ,POINTER   :: savxy
real      ,DIMENSION(:,:)     ,POINTER   :: sagxy
real      ,DIMENSION(:,:)     ,POINTER   :: rssunxy
real      ,DIMENSION(:,:)     ,POINTER   :: rsshaxy
real      ,DIMENSION(:,:)     ,POINTER   :: bgapxy
real      ,DIMENSION(:,:)     ,POINTER   :: wgapxy
real      ,DIMENSION(:,:)     ,POINTER   :: tgvxy
real      ,DIMENSION(:,:)     ,POINTER   :: tgbxy
real      ,DIMENSION(:,:)     ,POINTER   :: chvxy
real      ,DIMENSION(:,:)     ,POINTER   :: chbxy
real      ,DIMENSION(:,:)     ,POINTER   :: shgxy
real      ,DIMENSION(:,:)     ,POINTER   :: shcxy
real      ,DIMENSION(:,:)     ,POINTER   :: shbxy
real      ,DIMENSION(:,:)     ,POINTER   :: evgxy
real      ,DIMENSION(:,:)     ,POINTER   :: evbxy
real      ,DIMENSION(:,:)     ,POINTER   :: ghvxy
real      ,DIMENSION(:,:)     ,POINTER   :: ghbxy
real      ,DIMENSION(:,:)     ,POINTER   :: irgxy
real      ,DIMENSION(:,:)     ,POINTER   :: ircxy
real      ,DIMENSION(:,:)     ,POINTER   :: irbxy
real      ,DIMENSION(:,:)     ,POINTER   :: trxy
real      ,DIMENSION(:,:)     ,POINTER   :: evcxy
real      ,DIMENSION(:,:)     ,POINTER   :: chleafxy
real      ,DIMENSION(:,:)     ,POINTER   :: chucxy
real      ,DIMENSION(:,:)     ,POINTER   :: chv2xy
real      ,DIMENSION(:,:)     ,POINTER   :: chb2xy
real      ,DIMENSION(:,:)     ,POINTER   :: chstarxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: smoiseq
real      ,DIMENSION(:,:)     ,POINTER   :: smcwtdxy
real      ,DIMENSION(:,:)     ,POINTER   :: rechxy
real      ,DIMENSION(:,:)     ,POINTER   :: deeprechxy
real      ,DIMENSION(:,:)     ,POINTER   :: fdepthxy
real      ,DIMENSION(:,:)     ,POINTER   :: areaxy
real      ,DIMENSION(:,:)     ,POINTER   :: rivercondxy
real      ,DIMENSION(:,:)     ,POINTER   :: riverbedxy
real      ,DIMENSION(:,:)     ,POINTER   :: eqzwt
real      ,DIMENSION(:,:)     ,POINTER   :: pexpxy
real      ,DIMENSION(:,:)     ,POINTER   :: qrfxy
real      ,DIMENSION(:,:)     ,POINTER   :: qrfsxy
real      ,DIMENSION(:,:)     ,POINTER   :: qspringxy
real      ,DIMENSION(:,:)     ,POINTER   :: qspringsxy
real      ,DIMENSION(:,:)     ,POINTER   :: qslatxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: tsk_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: qsfc_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tslb_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh2o_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: canwat_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: snow_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowh_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowc_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: albedo_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: albbck_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: emiss_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: embck_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: znt_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: z0_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: hfx_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: qfx_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: lh_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: grdflx_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: snotime_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tr_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tb_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tg_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tc_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: ts_rul2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: qc_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: uc_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: trl_urb3d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tbl_urb3d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: tgl_urb3d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: lh_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: g_urb2d_mosaic
real      ,DIMENSION(:,:,:)   ,POINTER   :: rn_urb2d_mosaic
integer   ,DIMENSION(:,:,:)   ,POINTER   :: mosaic_cat_index
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef2
real      ,DIMENSION(:)       ,POINTER   :: mp_restart_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs0_state
real      ,DIMENSION(:)       ,POINTER   :: lu_state
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_phy
real      ,DIMENSION(:,:)     ,POINTER   :: tmn
real      ,DIMENSION(:,:)     ,POINTER   :: tyr
real      ,DIMENSION(:,:)     ,POINTER   :: tyra
real      ,DIMENSION(:,:)     ,POINTER   :: tdly
real      ,DIMENSION(:,:,:)   ,POINTER   :: tlag
real      ,DIMENSION(:,:)     ,POINTER   :: xland
real      ,DIMENSION(:,:,:)   ,POINTER   :: cplmask
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:)     ,POINTER   :: ck
real      ,DIMENSION(:,:)     ,POINTER   :: cka
real      ,DIMENSION(:,:)     ,POINTER   :: cd
real      ,DIMENSION(:,:)     ,POINTER   :: cda
real      ,DIMENSION(:,:)     ,POINTER   :: ust
real      ,DIMENSION(:,:)     ,POINTER   :: ustm
real      ,DIMENSION(:,:)     ,POINTER   :: rmol
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:)     ,POINTER   :: pblh
real      ,DIMENSION(:,:)     ,POINTER   :: capg
real      ,DIMENSION(:,:)     ,POINTER   :: thc
real      ,DIMENSION(:,:)     ,POINTER   :: hfx
real      ,DIMENSION(:,:)     ,POINTER   :: qfx
real      ,DIMENSION(:,:)     ,POINTER   :: lh
real      ,DIMENSION(:,:)     ,POINTER   :: achfx
real      ,DIMENSION(:,:)     ,POINTER   :: aclhf
real      ,DIMENSION(:,:)     ,POINTER   :: flhc
real      ,DIMENSION(:,:)     ,POINTER   :: flqc
real      ,DIMENSION(:,:)     ,POINTER   :: qsg
real      ,DIMENSION(:,:)     ,POINTER   :: qvg
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_qvg
real      ,DIMENSION(:,:)     ,POINTER   :: qcg
real      ,DIMENSION(:,:)     ,POINTER   :: dew
real      ,DIMENSION(:,:)     ,POINTER   :: soilt1
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_soilt1
real      ,DIMENSION(:,:)     ,POINTER   :: tsnav
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_tsnav
real      ,DIMENSION(:,:)     ,POINTER   :: regime
real      ,DIMENSION(:,:)     ,POINTER   :: snowc
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snowc
real      ,DIMENSION(:,:)     ,POINTER   :: mavail
real      ,DIMENSION(:,:)     ,POINTER   :: tkesfcf
real      ,DIMENSION(:,:)     ,POINTER   :: sr
real      ,DIMENSION(:,:)     ,POINTER   :: potevp
real      ,DIMENSION(:,:)     ,POINTER   :: snopcx
real      ,DIMENSION(:,:)     ,POINTER   :: soiltb
real      ,DIMENSION(:,:,:)   ,POINTER   :: taucldi
real      ,DIMENSION(:,:,:)   ,POINTER   :: taucldc
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor11
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor22
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor12
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor33
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor13
real      ,DIMENSION(:,:,:)   ,POINTER   :: defor23
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmv
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkmh
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkhv
real      ,DIMENSION(:,:,:)   ,POINTER   :: xkhh
real      ,DIMENSION(:,:,:)   ,POINTER   :: div
real      ,DIMENSION(:,:,:)   ,POINTER   :: bn2
real      ,DIMENSION(:,:,:)   ,POINTER   :: rundgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rvndgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthndgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rphndgdten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvndgdten
real      ,DIMENSION(:,:)     ,POINTER   :: rmundgdten
real      ,DIMENSION(:,:,:,:) ,POINTER   :: fdda3d
real      ,DIMENSION(:,:,:,:) ,POINTER   :: fdda2d
real      ,DIMENSION(:,:)     ,POINTER   :: u10_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: u10_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: v10_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: v10_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: t2_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: t2_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: th2_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: th2_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: q2_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: q2_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: rh_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: rh_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: psl_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: psl_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: ps_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: ps_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: tob_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: odis_ndg_old
real      ,DIMENSION(:,:)     ,POINTER   :: tob_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: odis_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: sn_ndg_new
real      ,DIMENSION(:,:)     ,POINTER   :: sn_ndg_old
real      ,DIMENSION(:,:,:,:) ,POINTER   :: abstot
real      ,DIMENSION(:,:,:,:) ,POINTER   :: absnxt
real      ,DIMENSION(:,:,:)   ,POINTER   :: emstot
real      ,DIMENSION(:,:)     ,POINTER   :: dpsdt
real      ,DIMENSION(:,:)     ,POINTER   :: dmudt
real      ,DIMENSION(:,:)     ,POINTER   :: pk1m
real      ,DIMENSION(:,:)     ,POINTER   :: mu_2m
real      ,DIMENSION(:,:)     ,POINTER   :: wspd10max
real      ,DIMENSION(:,:)     ,POINTER   :: w_up_max
real      ,DIMENSION(:,:)     ,POINTER   :: w_dn_max
real      ,DIMENSION(:,:)     ,POINTER   :: refd_max
real      ,DIMENSION(:,:)     ,POINTER   :: up_heli_max
real      ,DIMENSION(:,:)     ,POINTER   :: w_mean
real      ,DIMENSION(:,:)     ,POINTER   :: grpl_max
real      ,DIMENSION(:,:)     ,POINTER   :: uh
real      ,DIMENSION(:,:)     ,POINTER   :: w_colmean
real      ,DIMENSION(:,:)     ,POINTER   :: numcolpts
real      ,DIMENSION(:,:)     ,POINTER   :: grpl_colint
real      ,DIMENSION(:,:)     ,POINTER   :: prec_acc_c
real      ,DIMENSION(:,:)     ,POINTER   :: prec_acc_nc
real      ,DIMENSION(:,:)     ,POINTER   :: snow_acc_nc
real      ,DIMENSION(:,:)     ,POINTER   :: tcoli_max
real      ,DIMENSION(:,:)     ,POINTER   :: refd_com
real      ,DIMENSION(:,:)     ,POINTER   :: refd
real      ,DIMENSION(:,:)     ,POINTER   :: vil
real      ,DIMENSION(:,:)     ,POINTER   :: radarvil
real      ,DIMENSION(:,:)     ,POINTER   :: echotop
real      ,DIMENSION(:,:)     ,POINTER   :: fzlev
real      ,DIMENSION(:,:)     ,POINTER   :: icingtop
real      ,DIMENSION(:,:)     ,POINTER   :: icingbot
real      ,DIMENSION(:,:,:)   ,POINTER   :: qicing_lg
real      ,DIMENSION(:,:,:)   ,POINTER   :: qicing_sm
real      ,DIMENSION(:,:)     ,POINTER   :: qicing_lg_max
real      ,DIMENSION(:,:)     ,POINTER   :: qicing_sm_max
real      ,DIMENSION(:,:)     ,POINTER   :: icing_lg
real      ,DIMENSION(:,:)     ,POINTER   :: icing_sm
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_precip
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_rain
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_snow
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_ice
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_fzra
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_snowfall
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_vis
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_vis_dust
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_cloud
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_cloud_ceil
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_cape
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_zlfc
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_plfc
real      ,DIMENSION(:,:)     ,POINTER   :: midrh_min
real      ,DIMENSION(:,:)     ,POINTER   :: midrh_min_old
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_hail
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_llws
real      ,DIMENSION(:,:)     ,POINTER   :: afwa_tornado
real      ,DIMENSION(:,:,:,:) ,POINTER   :: advh_t
real      ,DIMENSION(:,:,:,:) ,POINTER   :: advz_t
real      ,DIMENSION(:,:)     ,POINTER   :: tml
real      ,DIMENSION(:,:)     ,POINTER   :: t0ml
real      ,DIMENSION(:,:)     ,POINTER   :: hml
real      ,DIMENSION(:,:)     ,POINTER   :: h0ml
real      ,DIMENSION(:,:)     ,POINTER   :: huml
real      ,DIMENSION(:,:)     ,POINTER   :: hvml
real      ,DIMENSION(:,:)     ,POINTER   :: tmoml
real      ,DIMENSION(:,:)     ,POINTER   :: track_z
real      ,DIMENSION(:,:)     ,POINTER   :: track_t
real      ,DIMENSION(:,:)     ,POINTER   :: track_p
real      ,DIMENSION(:,:)     ,POINTER   :: track_u
real      ,DIMENSION(:,:)     ,POINTER   :: track_v
real      ,DIMENSION(:,:)     ,POINTER   :: track_w
real      ,DIMENSION(:,:)     ,POINTER   :: track_rh
real      ,DIMENSION(:,:)     ,POINTER   :: track_alt
real      ,DIMENSION(:)       ,POINTER   :: track_ele
real      ,DIMENSION(:)       ,POINTER   :: track_aircraft
real      ,DIMENSION(:,:)     ,POINTER   :: track_qcloud
real      ,DIMENSION(:,:)     ,POINTER   :: track_qrain
real      ,DIMENSION(:,:)     ,POINTER   :: track_qice
real      ,DIMENSION(:,:)     ,POINTER   :: track_qsnow
real      ,DIMENSION(:,:)     ,POINTER   :: track_qgraup
real      ,DIMENSION(:,:)     ,POINTER   :: track_qvapor
real      ,DIMENSION(:,:)     ,POINTER   :: ic_flashcount
real      ,DIMENSION(:,:)     ,POINTER   :: ic_flashrate
real      ,DIMENSION(:,:)     ,POINTER   :: cg_flashcount
real      ,DIMENSION(:,:)     ,POINTER   :: cg_flashrate
real      ,DIMENSION(:,:)     ,POINTER   :: iccg_in_num
real      ,DIMENSION(:,:)     ,POINTER   :: iccg_in_den
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: ww_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: ph_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: dum_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: fourd_xxx
real      ,DIMENSION(:,:)     ,POINTER   :: clat_xxx
real      ,DIMENSION(:,:)     ,POINTER   :: ht_xxx
real      ,DIMENSION(:,:)     ,POINTER   :: mf_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: dif_analysis
real      ,DIMENSION(:,:,:)   ,POINTER   :: dif_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: dif_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: lfn_hist
real      ,DIMENSION(:)       ,POINTER   :: lfn_time
real      ,DIMENSION(:,:)     ,POINTER   :: nfuel_cat
real      ,DIMENSION(:,:)     ,POINTER   :: zsf
real      ,DIMENSION(:,:)     ,POINTER   :: dzdxf
real      ,DIMENSION(:,:)     ,POINTER   :: dzdyf
real      ,DIMENSION(:,:)     ,POINTER   :: tign_g
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthfrten
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvfrten
real      ,DIMENSION(:,:)     ,POINTER   :: avg_fuel_frac
real      ,DIMENSION(:,:)     ,POINTER   :: grnhfx
real      ,DIMENSION(:,:)     ,POINTER   :: grnqfx
real      ,DIMENSION(:,:)     ,POINTER   :: canhfx
real      ,DIMENSION(:,:)     ,POINTER   :: canqfx
real      ,DIMENSION(:,:)     ,POINTER   :: uah
real      ,DIMENSION(:,:)     ,POINTER   :: vah
real      ,DIMENSION(:,:)     ,POINTER   :: lfn
real      ,DIMENSION(:,:)     ,POINTER   :: fuel_frac
real      ,DIMENSION(:,:)     ,POINTER   :: fire_area
real      ,DIMENSION(:,:)     ,POINTER   :: uf
real      ,DIMENSION(:,:)     ,POINTER   :: vf
real      ,DIMENSION(:,:)     ,POINTER   :: fgrnhfx
real      ,DIMENSION(:,:)     ,POINTER   :: fgrnqfx
real      ,DIMENSION(:,:)     ,POINTER   :: fcanhfx
real      ,DIMENSION(:,:)     ,POINTER   :: fcanqfx
real      ,DIMENSION(:,:)     ,POINTER   :: ros
real      ,DIMENSION(:,:)     ,POINTER   :: fxlong
real      ,DIMENSION(:,:)     ,POINTER   :: fxlat
real      ,DIMENSION(:,:)     ,POINTER   :: fuel_time
real      ,DIMENSION(:,:)     ,POINTER   :: bbb
real      ,DIMENSION(:,:)     ,POINTER   :: betafl
real      ,DIMENSION(:,:)     ,POINTER   :: phiwc
real      ,DIMENSION(:,:)     ,POINTER   :: r_0
real      ,DIMENSION(:,:)     ,POINTER   :: fgip
real      ,DIMENSION(:,:)     ,POINTER   :: ischap
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_rum
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_rvm
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_wwm
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_cfu1
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_cfd1
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_dfu1
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_efu1
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_dfd1
real      ,DIMENSION(:,:,:)   ,POINTER   :: avgflx_efd1
real      ,DIMENSION(:,:,:)   ,POINTER   :: cfu1
real      ,DIMENSION(:,:,:)   ,POINTER   :: cfd1
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfu1
real      ,DIMENSION(:,:,:)   ,POINTER   :: efu1
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfd1
real      ,DIMENSION(:,:,:)   ,POINTER   :: efd1
real      ,DIMENSION(:,:,:)   ,POINTER   :: vertstrucc
real      ,DIMENSION(:,:,:)   ,POINTER   :: vertstrucs
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_tendf_stoch
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_tendf_stoch
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_tendf_stoch
real      ,DIMENSION(:,:)     ,POINTER   :: spstreamforcc
real      ,DIMENSION(:,:)     ,POINTER   :: spstreamforcs
real      ,DIMENSION(:,:)     ,POINTER   :: sptforcc
real      ,DIMENSION(:,:)     ,POINTER   :: sptforcs
real      ,DIMENSION(:,:)     ,POINTER   :: spstream_amp
real      ,DIMENSION(:,:)     ,POINTER   :: spt_amp
real      ,DIMENSION(:)       ,POINTER   :: vertampt
real      ,DIMENSION(:)       ,POINTER   :: vertampuv
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_real
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_imag
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_real_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_real_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_imag_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: ru_imag_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_real
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_imag
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_real_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_real_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_imag_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: rv_imag_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_real
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_imag
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_real_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_real_yyy
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_imag_xxx
real      ,DIMENSION(:,:,:)   ,POINTER   :: rt_imag_yyy
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nba_mij
real      ,DIMENSION(:,:,:,:) ,POINTER   :: nba_rij
real      ,DIMENSION(:,:)     ,POINTER   :: tauresx2d
real      ,DIMENSION(:,:)     ,POINTER   :: tauresy2d
real      ,DIMENSION(:,:)     ,POINTER   :: tpert2d
real      ,DIMENSION(:,:)     ,POINTER   :: qpert2d
real      ,DIMENSION(:,:)     ,POINTER   :: wpert2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: turbtype3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: smaw3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: wsedl3d
real      ,DIMENSION(:,:)     ,POINTER   :: rliq
real      ,DIMENSION(:,:,:)   ,POINTER   :: dlf
real      ,DIMENSION(:,:)     ,POINTER   :: precz
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmdq
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmdice
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmdliq
real      ,DIMENSION(:,:,:)   ,POINTER   :: evaptzm
real      ,DIMENSION(:,:,:)   ,POINTER   :: fzsntzm
real      ,DIMENSION(:,:,:)   ,POINTER   :: evsntzm
real      ,DIMENSION(:,:,:)   ,POINTER   :: evapqzm
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmflxprc
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmflxsnw
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmntprpd
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmntsnpd
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmeiheat
real      ,DIMENSION(:,:,:)   ,POINTER   :: cmfmcdzm
real      ,DIMENSION(:,:)     ,POINTER   :: preccdzm
real      ,DIMENSION(:,:)     ,POINTER   :: pconvb
real      ,DIMENSION(:,:)     ,POINTER   :: pconvt
real      ,DIMENSION(:,:)     ,POINTER   :: cape
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmmtu
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmmtv
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmmu
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmmd
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmupgu
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmupgd
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmvpgu
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmvpgd
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmicuu
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmicud
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmicvu
real      ,DIMENSION(:,:,:)   ,POINTER   :: zmicvd
real      ,DIMENSION(:,:,:)   ,POINTER   :: evapcdp3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: icwmrdp3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: rprddp3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dp3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: du3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: ed3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: eu3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: md3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: mu3d
real      ,DIMENSION(:,:)     ,POINTER   :: dsubcld2d
integer   ,DIMENSION(:,:)     ,POINTER   :: ideep2d
integer   ,DIMENSION(:,:)     ,POINTER   :: jt2d
integer   ,DIMENSION(:,:)     ,POINTER   :: maxg2d
integer   ,DIMENSION(:,:)     ,POINTER   :: lengath2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: cmfsl
real      ,DIMENSION(:,:,:)   ,POINTER   :: cmflq
real      ,DIMENSION(:,:,:)   ,POINTER   :: cmfmc
real      ,DIMENSION(:,:,:)   ,POINTER   :: cmfmc2
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfrash
real      ,DIMENSION(:,:)     ,POINTER   :: cush
real      ,DIMENSION(:,:,:)   ,POINTER   :: evapcsh
real      ,DIMENSION(:,:,:)   ,POINTER   :: icwmrsh
real      ,DIMENSION(:,:)     ,POINTER   :: snowsh
real      ,DIMENSION(:,:,:)   ,POINTER   :: rprdsh
real      ,DIMENSION(:,:)     ,POINTER   :: rliq2
real      ,DIMENSION(:,:,:)   ,POINTER   :: dlf2
real      ,DIMENSION(:,:,:)   ,POINTER   :: shfrc3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: evapcsh3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: qtflx_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: slflx_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: uflx_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: vflx_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qtten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: slten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: uten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: vten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qvten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qlten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qiten_cu
real      ,DIMENSION(:,:)     ,POINTER   :: cbmf_cu
real      ,DIMENSION(:,:)     ,POINTER   :: ufrcinvbase_cu
real      ,DIMENSION(:,:)     ,POINTER   :: ufrclcl_cu
real      ,DIMENSION(:,:)     ,POINTER   :: winvbase_cu
real      ,DIMENSION(:,:)     ,POINTER   :: wlcl_cu
real      ,DIMENSION(:,:)     ,POINTER   :: plcl_cu
real      ,DIMENSION(:,:)     ,POINTER   :: pinv_cu
real      ,DIMENSION(:,:)     ,POINTER   :: plfc_cu
real      ,DIMENSION(:,:)     ,POINTER   :: pbup_cu
real      ,DIMENSION(:,:)     ,POINTER   :: ppen_cu
real      ,DIMENSION(:,:)     ,POINTER   :: qtsrc_cu
real      ,DIMENSION(:,:)     ,POINTER   :: thlsrc_cu
real      ,DIMENSION(:,:)     ,POINTER   :: thvlsrc_cu
real      ,DIMENSION(:,:)     ,POINTER   :: emkfbup_cu
real      ,DIMENSION(:,:)     ,POINTER   :: cin_cu
real      ,DIMENSION(:,:)     ,POINTER   :: cinlcl_cu
real      ,DIMENSION(:,:)     ,POINTER   :: cbmflimit_cu
real      ,DIMENSION(:,:)     ,POINTER   :: tkeavg_cu
real      ,DIMENSION(:,:)     ,POINTER   :: zinv_cu
real      ,DIMENSION(:,:)     ,POINTER   :: rcwp_cu
real      ,DIMENSION(:,:)     ,POINTER   :: rlwp_cu
real      ,DIMENSION(:,:)     ,POINTER   :: riwp_cu
real      ,DIMENSION(:,:)     ,POINTER   :: tophgt_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: wu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: ufrc_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qtu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: thlu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: thvu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: uu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: vu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qtu_emf_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: thlu_emf_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: uu_emf_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: vu_emf_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: umf_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: uemf_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qcu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qlu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qiu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: cufrc_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: fer_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: fdr_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: dwten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: diten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qrten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qsten_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: flxrain_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: flxsnow_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: ntraprd_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: ntsnprd_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: excessu_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: excessu0_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: xc_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: aquad_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: bquad_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: cquad_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: bogbot_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: bogtop_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_uwcu_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_conden_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_klclmkx_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_klfcmkx_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_ufrc_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_wtw_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_drycore_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_wu_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_cufliter_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_kinv1_cu
real      ,DIMENSION(:,:)     ,POINTER   :: exit_rei_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_shcu_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_negcon_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_ufrc_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_ppen_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_emf_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_cinlcl_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_cin_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_cbmf_cu
real      ,DIMENSION(:,:)     ,POINTER   :: limit_rei_cu
real      ,DIMENSION(:,:)     ,POINTER   :: ind_delcin_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: rh_old_mp
real      ,DIMENSION(:,:,:)   ,POINTER   :: lcd_old_mp
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_old_mp
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_mp
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_mp_all
real      ,DIMENSION(:,:,:)   ,POINTER   :: iradius
real      ,DIMENSION(:,:,:)   ,POINTER   :: lradius
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra_conv
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfrai
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfral
integer   ,DIMENSION(:,:)     ,POINTER   :: numc
integer   ,DIMENSION(:,:)     ,POINTER   :: nump
real      ,DIMENSION(:,:)     ,POINTER   :: sabv
real      ,DIMENSION(:,:)     ,POINTER   :: sabg
real      ,DIMENSION(:,:)     ,POINTER   :: lwup
real      ,DIMENSION(:,:,:)   ,POINTER   :: lhsoi
real      ,DIMENSION(:,:,:)   ,POINTER   :: lhveg
real      ,DIMENSION(:,:,:)   ,POINTER   :: lhtran
integer   ,DIMENSION(:,:,:)   ,POINTER   :: snl
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowdp
real      ,DIMENSION(:,:,:)   ,POINTER   :: wtc
real      ,DIMENSION(:,:,:)   ,POINTER   :: wtp
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osno
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_grnd
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_veg
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2ocan
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2ocan_col
real      ,DIMENSION(:,:)     ,POINTER   :: t2m_max
real      ,DIMENSION(:,:)     ,POINTER   :: t2m_min
real      ,DIMENSION(:,:)     ,POINTER   :: t2clm
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_ref2m
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq_s1
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq_s2
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq_s3
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq_s4
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq_s5
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq1
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq2
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq3
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq4
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq5
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq6
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq7
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq8
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq9
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq10
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice_s1
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice_s2
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice_s3
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice_s4
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice_s5
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice1
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice2
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice3
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice4
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice5
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice6
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice7
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice8
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice9
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice10
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno_s1
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno_s2
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno_s3
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno_s4
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno_s5
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno1
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno2
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno3
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno4
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno5
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno6
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno7
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno8
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno9
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno10
real      ,DIMENSION(:,:,:)   ,POINTER   :: dzsnow1
real      ,DIMENSION(:,:,:)   ,POINTER   :: dzsnow2
real      ,DIMENSION(:,:,:)   ,POINTER   :: dzsnow3
real      ,DIMENSION(:,:,:)   ,POINTER   :: dzsnow4
real      ,DIMENSION(:,:,:)   ,POINTER   :: dzsnow5
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowrds1
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowrds2
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowrds3
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowrds4
real      ,DIMENSION(:,:,:)   ,POINTER   :: snowrds5
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake1
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake2
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake3
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake4
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake5
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake6
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake7
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake8
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake9
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake10
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol1
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol2
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol3
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol4
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol5
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol6
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol7
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol8
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol9
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol10
real      ,DIMENSION(:,:,:)   ,POINTER   :: albedosubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: lhsubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: hfxsubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: lwupsubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2subgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: sabvsubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: sabgsubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: nrasubgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: swupsubgrid
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_fm
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_fh
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_cm
real      ,DIMENSION(:,:)     ,POINTER   :: ssibxdd
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_br
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_lhf
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_shf
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_ghf
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_egs
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_eci
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_ect
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_egi
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_egt
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_sdn
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_sup
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_ldn
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_lup
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_wat
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_shc
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_shg
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_lai
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_vcf
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_z00
real      ,DIMENSION(:,:)     ,POINTER   :: ssib_veg
integer   ,DIMENSION(:,:)     ,POINTER   :: isnow
real      ,DIMENSION(:,:)     ,POINTER   :: swe
real      ,DIMENSION(:,:)     ,POINTER   :: snowden
real      ,DIMENSION(:,:)     ,POINTER   :: snowdepth
real      ,DIMENSION(:,:)     ,POINTER   :: tkair
real      ,DIMENSION(:,:)     ,POINTER   :: dzo1
real      ,DIMENSION(:,:)     ,POINTER   :: wo1
real      ,DIMENSION(:,:)     ,POINTER   :: tssn1
real      ,DIMENSION(:,:)     ,POINTER   :: tssno1
real      ,DIMENSION(:,:)     ,POINTER   :: bwo1
real      ,DIMENSION(:,:)     ,POINTER   :: bto1
real      ,DIMENSION(:,:)     ,POINTER   :: cto1
real      ,DIMENSION(:,:)     ,POINTER   :: fio1
real      ,DIMENSION(:,:)     ,POINTER   :: flo1
real      ,DIMENSION(:,:)     ,POINTER   :: bio1
real      ,DIMENSION(:,:)     ,POINTER   :: blo1
real      ,DIMENSION(:,:)     ,POINTER   :: ho1
real      ,DIMENSION(:,:)     ,POINTER   :: dzo2
real      ,DIMENSION(:,:)     ,POINTER   :: wo2
real      ,DIMENSION(:,:)     ,POINTER   :: tssn2
real      ,DIMENSION(:,:)     ,POINTER   :: tssno2
real      ,DIMENSION(:,:)     ,POINTER   :: bwo2
real      ,DIMENSION(:,:)     ,POINTER   :: bto2
real      ,DIMENSION(:,:)     ,POINTER   :: cto2
real      ,DIMENSION(:,:)     ,POINTER   :: fio2
real      ,DIMENSION(:,:)     ,POINTER   :: flo2
real      ,DIMENSION(:,:)     ,POINTER   :: bio2
real      ,DIMENSION(:,:)     ,POINTER   :: blo2
real      ,DIMENSION(:,:)     ,POINTER   :: ho2
real      ,DIMENSION(:,:)     ,POINTER   :: dzo3
real      ,DIMENSION(:,:)     ,POINTER   :: wo3
real      ,DIMENSION(:,:)     ,POINTER   :: tssn3
real      ,DIMENSION(:,:)     ,POINTER   :: tssno3
real      ,DIMENSION(:,:)     ,POINTER   :: bwo3
real      ,DIMENSION(:,:)     ,POINTER   :: bto3
real      ,DIMENSION(:,:)     ,POINTER   :: cto3
real      ,DIMENSION(:,:)     ,POINTER   :: fio3
real      ,DIMENSION(:,:)     ,POINTER   :: flo3
real      ,DIMENSION(:,:)     ,POINTER   :: bio3
real      ,DIMENSION(:,:)     ,POINTER   :: blo3
real      ,DIMENSION(:,:)     ,POINTER   :: ho3
real      ,DIMENSION(:,:)     ,POINTER   :: dzo4
real      ,DIMENSION(:,:)     ,POINTER   :: wo4
real      ,DIMENSION(:,:)     ,POINTER   :: tssn4
real      ,DIMENSION(:,:)     ,POINTER   :: tssno4
real      ,DIMENSION(:,:)     ,POINTER   :: bwo4
real      ,DIMENSION(:,:)     ,POINTER   :: bto4
real      ,DIMENSION(:,:)     ,POINTER   :: cto4
real      ,DIMENSION(:,:)     ,POINTER   :: fio4
real      ,DIMENSION(:,:)     ,POINTER   :: flo4
real      ,DIMENSION(:,:)     ,POINTER   :: bio4
real      ,DIMENSION(:,:)     ,POINTER   :: blo4
real      ,DIMENSION(:,:)     ,POINTER   :: ho4
logical   ,DIMENSION(:,:)     ,POINTER   :: lake2d
real      ,DIMENSION(:,:)     ,POINTER   :: lakedepth2d
real      ,DIMENSION(:,:)     ,POINTER   :: savedtke12d
real      ,DIMENSION(:,:)     ,POINTER   :: snowdp2d
real      ,DIMENSION(:,:)     ,POINTER   :: h2osno2d
real      ,DIMENSION(:,:)     ,POINTER   :: snl2d
real      ,DIMENSION(:,:)     ,POINTER   :: t_grnd2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: lake_icefrac3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: z_lake3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dz_lake3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: z3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dz3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: zi3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: watsat3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: csol3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tkmg3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tkdry3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tksatu3d
real      ,DIMENSION(:)       ,POINTER   :: p_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: rh_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: ght_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: s_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: td_pl
real      ,DIMENSION(:,:,:)   ,POINTER   :: th_old
real      ,DIMENSION(:,:,:)   ,POINTER   :: qv_old
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_ql
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qic
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qip
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qid
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qs
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qg
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qh
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_qa
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_ft_qic
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_ft_qip
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_ft_qid
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_ft_qs
real      ,DIMENSION(:,:,:)   ,POINTER   :: kext_ft_qg
real      ,DIMENSION(:,:,:)   ,POINTER   :: height
real      ,DIMENSION(:,:,:)   ,POINTER   :: tempc
real      ,DIMENSION(:,:,:)   ,POINTER   :: field_u_tend_perturb
real      ,DIMENSION(:,:,:)   ,POINTER   :: field_v_tend_perturb
real      ,DIMENSION(:,:,:)   ,POINTER   :: field_t_tend_perturb
real      ,DIMENSION(:,:)     ,POINTER   :: landmask
real      ,DIMENSION(:,:)     ,POINTER   :: lakemask
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:)     ,POINTER   :: sst_input
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tracer_btye
!ENDOFREGISTRYGENERATEDINCLUDE
