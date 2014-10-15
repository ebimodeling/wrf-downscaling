

SUBROUTINE solve_em ( grid , config_flags  &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


                    )

   USE module_state_description
   USE module_domain, ONLY : &
                  domain, get_ijk_from_grid, get_ijk_from_subgrid                          &
                 ,domain_get_current_time, domain_get_start_time                           &
                 ,domain_get_sim_start_time, domain_clock_get,is_alarm_tstep
   USE module_domain_type, ONLY : history_alarm, restart_alarm, auxinput4_alarm
   USE module_configure, ONLY : grid_config_rec_type
   USE module_driver_constants
   USE module_machine
   USE module_tiles, ONLY : set_tiles
   USE module_dm, ONLY : &
                  local_communicator, mytask, ntasks, ntasks_x, ntasks_y                   &
                 ,local_communicator_periodic, wrf_dm_maxval
   USE module_comm_dm, ONLY : &
                  halo_em_a_sub,halo_em_b_sub,halo_em_c2_sub,halo_em_chem_e_3_sub          &
                 ,halo_em_chem_e_5_sub,halo_em_chem_e_7_sub,halo_em_chem_old_e_5_sub       &
                 ,halo_em_chem_old_e_7_sub,halo_em_c_sub,halo_em_d2_3_sub                  &
                 ,halo_em_d2_5_sub,halo_em_d3_3_sub,halo_em_d3_5_sub,halo_em_d_sub         &
                 ,halo_em_e_3_sub,halo_em_e_5_sub,halo_em_hydro_uv_sub                     &
                 ,halo_em_moist_e_3_sub,halo_em_moist_e_5_sub,halo_em_moist_e_7_sub        &
                 ,halo_em_moist_old_e_5_sub,halo_em_moist_old_e_7_sub                      &
                 ,halo_em_scalar_e_3_sub,halo_em_scalar_e_5_sub,halo_em_scalar_e_7_sub     &
                 ,halo_em_scalar_old_e_5_sub,halo_em_scalar_old_e_7_sub,halo_em_tke_3_sub  &
                 ,halo_em_tke_5_sub,halo_em_tke_7_sub,halo_em_tke_advect_3_sub             &
                 ,halo_em_tke_advect_5_sub,halo_em_tke_old_e_5_sub                         &
                 ,halo_em_tke_old_e_7_sub,halo_em_tracer_e_3_sub,halo_em_tracer_e_5_sub    &
                 ,halo_em_tracer_e_7_sub,halo_em_tracer_old_e_5_sub                        &
                 ,halo_em_tracer_old_e_7_sub,halo_em_sbm_sub,period_bdy_em_a_sub                           &
                 ,period_bdy_em_b3_sub,period_bdy_em_b_sub,period_bdy_em_chem2_sub         &
                 ,period_bdy_em_chem_old_sub,period_bdy_em_chem_sub,period_bdy_em_d3_sub   &
                 ,period_bdy_em_d_sub,period_bdy_em_e_sub,period_bdy_em_moist2_sub         &
                 ,period_bdy_em_moist_old_sub,period_bdy_em_moist_sub                      &
                 ,period_bdy_em_scalar2_sub,period_bdy_em_scalar_old_sub                   &
                 ,period_bdy_em_scalar_sub,period_bdy_em_tke_old_sub                       &
                 ,period_bdy_em_tracer2_sub,period_bdy_em_tracer_old_sub                   &
                 ,period_bdy_em_tracer_sub,period_em_da_sub,period_em_hydro_uv_sub         &
                 ,halo_em_f_sub,halo_em_init_4_sub
   USE module_utility


   USE module_model_constants
   USE module_small_step_em
   USE module_em
   USE module_big_step_utilities_em
   USE module_bc
   USE module_bc_em
   USE module_solvedebug_em
   USE module_physics_addtendc
   USE module_diffusion_em
   USE module_polarfft
   USE module_microphysics_driver
   USE module_microphysics_zero_out

   USE module_fddaobs_driver

   USE module_first_rk_step_part1
   USE module_first_rk_step_part2
   USE module_after_all_rk_steps
   USE module_llxy, ONLY : proj_cassini
   USE module_avgflx_em, ONLY : zero_avgflx, upd_avgflx
   USE module_cpl, ONLY : coupler_on, cpl_settime, cpl_store_input

   IMPLICIT NONE

   

   TYPE(domain) , TARGET          :: grid

   






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye


   
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   

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

   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c , is , num_3d_s
   INTEGER                         :: loop
   INTEGER                         :: sz
   INTEGER                         :: iswater

   LOGICAL                         :: specified_bdy, channel_bdy

   REAL                            :: t_new
   
   
   real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: h_tendency, &
                                                                                   z_tendency
                                                                                   
   
   LOGICAL                        :: tenddec
   
   
   LOGICAL                        :: diag_flag
   









real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ru_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rv_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ww1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: wwp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rw_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ph_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_2save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muus
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muvs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: muave
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_save
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: mu_tendf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: advect_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: alpha
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: gamma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: c2a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: phm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cqw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pm1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psim
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: psih
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: wspd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: gz1oz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: br
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: chklowq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: ht_loc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: th_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pi_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dz8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: p8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t8w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rho_phy
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: cu_act_flag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33)           :: hol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer_tend
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer_old







   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dts, dtm, wmax
   REAL , ALLOCATABLE , DIMENSION(:)  :: max_vert_cfl_tmp, max_horiz_cfl_tmp
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk
   LOGICAL :: f_flux  
   REAL    :: curr_secs, curr_secs2
   INTEGER :: num_sound_steps
   INTEGER :: idex, jdex
   REAL    :: max_msft
   REAL    :: spacing

   INTEGER :: ii, jj 
   REAL    :: dclat
   INTEGER :: debug_level


   INTEGER :: NUM_ROOF_LAYERS, NUM_WALL_LAYERS, NUM_ROAD_LAYERS   

   TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval, tmpTimeInterval2
   REAL                                       :: real_time
   LOGICAL                                    :: adapt_step_flag
   LOGICAL                                    :: fill_w_flag


   CHARACTER*256                              :: message, message2
   REAL                                       :: old_dt
   TYPE(WRFU_Time)                            :: temp_time, CurrTime, restart_time
   INTEGER, PARAMETER                         :: precision = 100
   INTEGER                                    :: num, den
   TYPE(WRFU_TimeInterval)                    :: dtInterval, intervaltime,restartinterval

































































































   dynamics_option = config_flags%rk_ord



   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe,    &
                             imsx, imex, jmsx, jmex, kmsx, kmex,    &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                             imsy, imey, jmsy, jmey, kmsy, kmey,    &
                             ipsy, ipey, jpsy, jpey, kpsy, kpey )
 
   CALL get_ijk_from_subgrid (  grid ,                   &
                             sids, side, sjds, sjde, skds, skde,    &
                             sims, sime, sjms, sjme, skms, skme,    &
                             sips, sipe, sjps, sjpe, skps, skpe    )
   k_start         = kps
   k_end           = kpe

   num_3d_m        = num_moist
   num_3d_c        = num_chem
   num_3d_s        = num_scalar

   f_flux = config_flags%do_avgflx_cugd .EQ. 1



   CALL set_tiles ( ZONE_SOLVE_EM, grid , ids , ide , jds , jde , ips , ipe , jps , jpe )




   ALLOCATE (max_vert_cfl_tmp(grid%num_tiles))
   ALLOCATE (max_horiz_cfl_tmp(grid%num_tiles))

  
  
  
  
  
  
   tmpTimeInterval  = domain_get_current_time ( grid ) - domain_get_sim_start_time ( grid )
   tmpTimeInterval2 = domain_get_current_time ( grid ) - domain_get_start_time ( grid )
   curr_secs  = real_time(tmpTimeInterval)
   curr_secs2 = real_time(tmpTimeInterval2)

   old_dt = grid%dt   





   IF ( (config_flags%use_adaptive_time_step) .and. &
        ( (.not. grid%nested) .or. &
        ( (grid%nested) .and. (abs(grid%dtbc) < 0.0001) ) ) )THEN
      CALL adapt_timestep(grid, config_flags)
      adapt_step_flag = .TRUE.
   ELSE
      adapt_step_flag = .FALSE.
   ENDIF






      diag_flag = .false.
   if ( Is_alarm_tstep(grid%domain_clock, grid%alarms(HISTORY_ALARM)) ) then
      diag_flag = .true.
   endif

   grid%itimestep = grid%itimestep + 1

   IF( coupler_on ) CALL cpl_store_input( grid, config_flags )

   IF (config_flags%polar) dclat = 90./REAL(jde-jds) 


   rk_order = config_flags%rk_ord

   IF ( grid%time_step_sound == 0 ) THEN

     spacing = min(grid%dx, grid%dy)
     IF ( ( config_flags%use_adaptive_time_step ) .AND. ( config_flags%map_proj == PROJ_CASSINI ) ) THEN
       max_msft=MIN ( MAX(grid%max_msftx, grid%max_msfty) , &
                      1.0/COS(config_flags%fft_filter_lat*degrad) )
       num_sound_steps = max ( 2 * ( INT (300. * grid%dt / (spacing / max_msft) - 0.01 ) + 1 ), 4 )
     ELSE IF  ( config_flags%use_adaptive_time_step ) THEN
       max_msft= MAX(grid%max_msftx, grid%max_msfty)
       num_sound_steps = max ( 2 * ( INT (300. * grid%dt / (spacing / max_msft) - 0.01 ) + 1 ), 4 )
     ELSE
       num_sound_steps = max ( 2 * ( INT (300. * grid%dt /  spacing             - 0.01 ) + 1 ), 4 )
     END IF
     WRITE(wrf_err_message,*)'grid spacing, dt, time_step_sound=',spacing,grid%dt,num_sound_steps
     CALL wrf_debug ( 50 , wrf_err_message )
   ELSE
     num_sound_steps = grid%time_step_sound
   ENDIF

   dts = grid%dt/float(num_sound_steps)

   IF (config_flags%use_adaptive_time_step) THEN
  
     CALL get_wrf_debug_level( debug_level )
     IF ((config_flags%time_step < 0) .AND. (debug_level.GE.50)) THEN
       CALL wrf_dm_maxval(grid%max_vert_cfl, idex, jdex)
       WRITE(wrf_err_message,*)'variable dt, max horiz cfl, max vert cfl: ',&
            grid%dt, grid%max_horiz_cfl, grid%max_vert_cfl
       CALL wrf_debug ( 0 , wrf_err_message )
     ENDIF

     grid%max_cfl_val = 0
     grid%max_horiz_cfl = 0
     grid%max_vert_cfl = 0
   ENDIF



     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles



       IF( config_flags%specified .AND. config_flags%constant_bc ) THEN

       CALL zero_bdytend (grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye,     &
                          grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye,     &
                          grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                          grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye,     &
                          grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye,     &
                          grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                          moist_btxs,moist_btxe,                               &
                          moist_btys,moist_btye,                               &
                          scalar_btxs,scalar_btxe,                              &
                          scalar_btys,scalar_btye,                              &
                          grid%spec_bdy_width,num_3d_m,num_3d_s,                &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          ips,ipe, jps,jpe, kps,kpe,                   &
                          grid%i_start(ij), grid%i_end(ij),            &
                          grid%j_start(ij), grid%j_end(ij),            &
                          k_start, k_end                               )

       ENDIF
     ENDDO
     !$OMP END PARALLEL DO



























   Runge_Kutta_loop:  DO rk_step = 1, rk_order

   
   

     dtm = grid%dt
     IF ( rk_order == 1 ) THEN   

       write(wrf_err_message,*)' leapfrog removed, error exit for dynamics_option = ',dynamics_option
       CALL wrf_error_fatal3("<stdin>",585,&
wrf_err_message )

     ELSE IF ( rk_order == 2 ) THEN   

       IF ( rk_step == 1) THEN
         dt_rk  = 0.5*grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps/2
       ELSE
         dt_rk = grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps
       ENDIF

     ELSE IF ( rk_order == 3 ) THEN 

       IF ( rk_step == 1) THEN
         dt_rk = grid%dt/3.
         dts_rk = dt_rk
         number_of_small_timesteps = 1
       ELSE IF (rk_step == 2) THEN
         dt_rk  = 0.5*grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps/2
       ELSE
         dt_rk = grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps
       ENDIF

     ELSE

       write(wrf_err_message,*)' unknown solver, error exit for dynamics_option = ',dynamics_option
       CALL wrf_error_fatal3("<stdin>",619,&
wrf_err_message )

     END IF


     IF (config_flags%polar) THEN 
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL zero_pole ( grid%v_1,                      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                   )
         CALL zero_pole ( grid%v_2,                      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                   )
       END DO
       !$OMP END PARALLEL DO
     END IF





     CALL wrf_debug ( 200 , ' call rk_step_prep ' )


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL rk_step_prep  ( config_flags, rk_step,            &
                            grid%u_2, grid%v_2, grid%w_2, grid%t_2, grid%ph_2, grid%mu_2,   &
                            moist,                            &
                            grid%ru, grid%rv, grid%rw, grid%ww, grid%php, grid%alt, grid%muu, grid%muv,   &
                            grid%mub, grid%mut, grid%phb, grid%pb, grid%p, grid%al, grid%alb,    &
                            cqu, cqv, cqw,                    &
                            grid%msfux, grid%msfuy, grid%msfvx, grid%msfvx_inv,        &
                            grid%msfvy, grid%msftx, grid%msfty,                        &
                            grid%fnm, grid%fnp, grid%dnw, grid%rdx, grid%rdy,          &
                            num_3d_m,                         &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start, k_end                   )

     END DO
     !$OMP END PARALLEL DO






































CALL HALO_EM_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )











CALL PERIOD_BDY_EM_A_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_1' )

       CALL rk_phys_bc_dry_1( config_flags, grid%ru, grid%rv, grid%rw, grid%ww,      & 
                              grid%muu, grid%muv, grid%mut, grid%php, grid%alt, grid%p,        &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                )
       CALL set_physical_bc3d( grid%al, 'p', config_flags,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )
       CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              grid%i_start(ij), grid%i_end(ij),        &
                              grid%j_start(ij), grid%j_end(ij),        &
                              k_start, k_end                )

       IF (config_flags%polar) THEN 





         CALL pole_point_bc ( grid%v_1,                      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )
 
         CALL pole_point_bc ( grid%v_2,                      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )
 




       ENDIF
     END DO
     !$OMP END PARALLEL DO


     rk_step_is_one : IF (rk_step == 1) THEN 

















       IF (coupler_on) CALL cpl_settime( curr_secs2 )

       CALL first_rk_step_part1 (    grid, config_flags         &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , aerod                            &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , config_flags%use_adaptive_time_step &
                             , curr_secs                        &
                             , psim , psih , wspd , gz1oz0      &
                             , br , chklowq                     &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , grid%t_phy      &
                             , dz8w , p8w , t8w                 &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx, imex, jmsx, jmex, kmsx, kmex    &
                             , ipsx, ipex, jpsx, jpex, kpsx, kpex    &
                             , imsy, imey, jmsy, jmey, kmsy, kmey    &
                             , ipsy, ipey, jpsy, jpey, kpsy, kpey    &
                             , k_start , k_end                  &
                             , f_flux                           &
                            )

       IF ( config_flags%bl_pbl_physics == MYNNPBLSCHEME2 .OR. &
            config_flags%bl_pbl_physics == MYNNPBLSCHEME3 ) THEN






CALL HALO_EM_SCALAR_E_5_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

       ENDIF

       CALL first_rk_step_part2 (    grid, config_flags         &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , wspd , gz1oz0      &
                             , br , chklowq                     &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , grid%t_phy      &
                             , dz8w , p8w , t8w                 &
                             , nba_mij, num_nba_mij             & 
                             , nba_rij, num_nba_rij             & 
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx, imex, jmsx, jmex, kmsx, kmex    &
                             , ipsx, ipex, jpsx, jpex, kpsx, kpex    &
                             , imsy, imey, jmsy, jmey, kmsy, kmey    &
                             , ipsy, ipey, jpsy, jpey, kpsy, kpey    &
                             , k_start , k_end                  &
                            )

     END IF rk_step_is_one


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_tendency' )
       CALL rk_tendency ( config_flags, rk_step                                                                &
                         ,grid%ru_tend, grid%rv_tend, rw_tend, ph_tend, t_tend                                 &
                         ,ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf                                      &
                         ,mu_tend, grid%u_save, grid%v_save, w_save, ph_save                                   &
                         ,grid%t_save, mu_save, grid%rthften                                                   &
                         ,grid%ru, grid%rv, grid%rw, grid%ww                                                   &
                         ,grid%u_2, grid%v_2, grid%w_2, grid%t_2, grid%ph_2                                    &
                         ,grid%u_1, grid%v_1, grid%w_1, grid%t_1, grid%ph_1                                    &
                         ,grid%h_diabatic, grid%phb, grid%t_init                                               &
                         ,grid%mu_2, grid%mut, grid%muu, grid%muv, grid%mub                                    &
                         ,grid%al, grid%alt, grid%p, grid%pb, grid%php, cqu, cqv, cqw                          &
                         ,grid%u_base, grid%v_base, grid%t_base, grid%qv_base, grid%z_base                     &
                         ,grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv                                    &
                         ,grid%msfvy, grid%msftx,grid%msfty, grid%clat, grid%f, grid%e, grid%sina, grid%cosa   &
                         ,grid%fnm, grid%fnp, grid%rdn, grid%rdnw                                              &
                         ,grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, grid%xkmh, grid%xkhh            &
                         ,grid%diff_6th_opt, grid%diff_6th_factor                                              &
                         ,config_flags%momentum_adv_opt                                                        &
                         ,grid%dampcoef,grid%zdamp,config_flags%damp_opt,config_flags%rad_nudge                &
                         ,grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m                          &
                         ,config_flags%non_hydrostatic, config_flags%top_lid                                   &
                         ,grid%u_frame, grid%v_frame                                                           &
                         ,ids, ide, jds, jde, kds, kde                                                         &
                         ,ims, ime, jms, jme, kms, kme                                                         &
                         ,grid%i_start(ij), grid%i_end(ij)                                                     &
                         ,grid%j_start(ij), grid%j_end(ij)                                                     &
                         ,k_start, k_end                                                                       &
                         ,max_vert_cfl_tmp(ij), max_horiz_cfl_tmp(ij)                                         )
     END DO
     !$OMP END PARALLEL DO


     IF (config_flags%use_adaptive_time_step) THEN
       DO ij = 1 , grid%num_tiles
         IF (max_horiz_cfl_tmp(ij) .GT. grid%max_horiz_cfl) THEN
           grid%max_horiz_cfl = max_horiz_cfl_tmp(ij)
         ENDIF
         IF (max_vert_cfl_tmp(ij) .GT. grid%max_vert_cfl) THEN
           grid%max_vert_cfl = max_vert_cfl_tmp(ij)
         ENDIF
       END DO
     
       IF (grid%max_horiz_cfl .GT. grid%max_cfl_val) THEN
         grid%max_cfl_val = grid%max_horiz_cfl
       ENDIF
       IF (grid%max_vert_cfl .GT. grid%max_cfl_val) THEN
         grid%max_cfl_val = grid%max_vert_cfl
       ENDIF
     ENDIF


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

         CALL relax_bdy_dry ( config_flags,                                &
                              grid%u_save, grid%v_save, ph_save, grid%t_save,             &
                              w_save, mu_tend,                             & 
                              grid%ru, grid%rv, grid%ph_2, grid%t_2,                           &
                              grid%w_2, grid%mu_2, grid%mut,                              &
                              grid%u_bxs,grid%u_bxe,grid%u_bys,grid%u_bye, &
                              grid%v_bxs,grid%v_bxe,grid%v_bys,grid%v_bye, &
                              grid%ph_bxs,grid%ph_bxe,grid%ph_bys,grid%ph_bye, &
                              grid%t_bxs,grid%t_bxe,grid%t_bys,grid%t_bye, &
                              grid%w_bxs,grid%w_bxe,grid%w_bys,grid%w_bye, &
                              grid%mu_bxs,grid%mu_bxe,grid%mu_bys,grid%mu_bye, &
                              grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye, &
                              grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye, &
                              grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                              grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye, &
                              grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye, &
                              grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                              config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone,       &
                              grid%dtbc, grid%fcx, grid%gcx,                              &
                              ids,ide, jds,jde, kds,kde,                   &
                              ims,ime, jms,jme, kms,kme,                   &
                              ips,ipe, jps,jpe, kps,kpe,                   &
                              grid%i_start(ij), grid%i_end(ij),            &
                              grid%j_start(ij), grid%j_end(ij),            &
                              k_start, k_end                              )

       ENDIF

       CALL rk_addtend_dry( grid%ru_tend,  grid%rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                            ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                            grid%u_save, grid%v_save, w_save, ph_save, grid%t_save, &
                            mu_tend, mu_tendf, rk_step,                      &
                            grid%h_diabatic, grid%mut, grid%msftx,        &
                            grid%msfty, grid%msfux,grid%msfuy,               &
                            grid%msfvx, grid%msfvx_inv, grid%msfvy,          &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            grid%i_start(ij), grid%i_end(ij),                &
                            grid%j_start(ij), grid%j_end(ij),                &
                            k_start, k_end                                  )

       IF( config_flags%specified .or. config_flags%nested ) THEN 
         CALL spec_bdy_dry ( config_flags,                                    &
                             grid%ru_tend, grid%rv_tend, ph_tend, t_tend,               &
                             rw_tend, mu_tend,                                &
                             grid%u_bxs,grid%u_bxe,grid%u_bys,grid%u_bye, &
                             grid%v_bxs,grid%v_bxe,grid%v_bys,grid%v_bye, &
                             grid%ph_bxs,grid%ph_bxe,grid%ph_bys,grid%ph_bye, &
                             grid%t_bxs,grid%t_bxe,grid%t_bys,grid%t_bye, &
                             grid%w_bxs,grid%w_bxe,grid%w_bys,grid%w_bye, &
                             grid%mu_bxs,grid%mu_bxe,grid%mu_bys,grid%mu_bye, &
                             grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye, &
                             grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye, &
                             grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                             grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye, &
                             grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye, &
                             grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                             config_flags%spec_bdy_width, grid%spec_zone,                       &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )
     
       ENDIF






       IF( config_flags%specified .and. config_flags%perturb_bdy==1 ) THEN 
         CALL spec_bdy_dry_perturb ( config_flags,                                 &
                             grid%ru_tend, grid%rv_tend, t_tend,                   &
                             grid%mu_2, grid%mub,                                  &
                             grid%msfu, grid%msfv, grid%msft,	                   &
                             grid%ru_tendf_stoch, grid%rv_tendf_stoch, grid%rt_tendf_stoch, &
                             config_flags%spec_bdy_width, grid%spec_zone,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )
     
       ENDIF

       IF( config_flags%specified .and. config_flags%perturb_bdy==2 ) THEN
         CALL spec_bdy_dry_perturb ( config_flags,                                 &
                             grid%ru_tend, grid%rv_tend, t_tend,                   &
                             grid%mu_2, grid%mub,                                  &
                             grid%msfu, grid%msfv, grid%msft,                      &
                             grid%field_u_tend_perturb, grid%field_v_tend_perturb, grid%field_t_tend_perturb, &
                             config_flags%spec_bdy_width, grid%spec_zone,                   &
                             ids,ide, jds,jde, kds,kde,  & 
                             ims,ime, jms,jme, kms,kme,  & 
                             ips,ipe, jps,jpe, kps,kpe,  & 
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )
  
       ENDIF

     END DO
     !$OMP END PARALLEL DO



























     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

    
    
    
    
    

       CALL wrf_debug ( 200 , ' call small_step_prep ' )

       CALL small_step_prep( grid%u_1,grid%u_2,grid%v_1,grid%v_2,grid%w_1,grid%w_2,   &
                             grid%t_1,grid%t_2,grid%ph_1,grid%ph_2,                   &
                             grid%mub, grid%mu_1, grid%mu_2,                          &
                             grid%muu, muus, grid%muv, muvs,                          &
                             grid%mut, grid%muts, grid%mudf,                          &
                             grid%u_save, grid%v_save, w_save,                        &
                             grid%t_save, ph_save, mu_save,                           &
                             grid%ww, ww1,                                            &
                             grid%dnw, c2a, grid%pb, grid%p, grid%alt,                &
                             grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,       &
                             grid%msfvy, grid%msftx,grid%msfty,                       &
                             grid%rdx, grid%rdy, rk_step,                             &
                             ids, ide, jds, jde, kds, kde,                            &
                             ims, ime, jms, jme, kms, kme,                            &
                             grid%i_start(ij), grid%i_end(ij),                        &
                             grid%j_start(ij), grid%j_end(ij),                        &
                             k_start    , k_end                                       )
 
       CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                 &
                        grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                        grid%mu_2, grid%muts, grid%znu, t0,         &
                        grid%rdnw, grid%dnw, grid%smdiv,            &
                        config_flags%non_hydrostatic, 0,            &
                        ids, ide, jds, jde, kds, kde,               &
                        ims, ime, jms, jme, kms, kme,               &
                        grid%i_start(ij), grid%i_end(ij),           &
                        grid%j_start(ij), grid%j_end(ij),           &
                        k_start    , k_end                          )

       IF (config_flags%non_hydrostatic) THEN
         CALL calc_coef_w( a,alpha,gamma,                    &
                           grid%mut, cqw,                    &
                           grid%rdn, grid%rdnw, c2a,         &
                           dts_rk, g, grid%epssm,            &
                           config_flags%top_lid,             &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
       ENDIF

     ENDDO
     !$OMP END PARALLEL DO









































CALL HALO_EM_B_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_B_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL set_physical_bc3d( grid%ru_tend, 'u', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%rv_tend, 'v', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,         &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%al, 'p', config_flags,           &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%p, 'p', config_flags,            &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%t_1, 'p', config_flags,          &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%t_save, 't', config_flags,       &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc2d( grid%mu_1, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

       CALL set_physical_bc2d( grid%mu_2, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

       CALL set_physical_bc2d( grid%mudf, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

     END DO
     !$OMP END PARALLEL DO

     small_steps : DO iteration = 1 , number_of_small_timesteps

       






CALL PERIOD_BDY_EM_B_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles


         CALL advance_uv ( grid%u_2, grid%ru_tend, grid%v_2, grid%rv_tend,        &
                           grid%p, grid%pb,                                       &
                           grid%ph_2, grid%php, grid%alt,  grid%al,               &
                           grid%mu_2,                                             &
                           grid%muu, cqu, grid%muv, cqv, grid%mudf,               &
                           grid%msfux, grid%msfuy, grid%msfvx,                    &
                           grid%msfvx_inv, grid%msfvy,                            &
                           grid%rdx, grid%rdy, dts_rk,                            &
                           grid%cf1, grid%cf2, grid%cf3, grid%fnm, grid%fnp,      &
                           grid%emdiv,                                            &
                           grid%rdnw, config_flags,grid%spec_zone,                &
                           config_flags%non_hydrostatic, config_flags%top_lid,    &
                           ids, ide, jds, jde, kds, kde,                          &
                           ims, ime, jms, jme, kms, kme,                          &
                           grid%i_start(ij), grid%i_end(ij),                      &
                           grid%j_start(ij), grid%j_end(ij),                      &
                           k_start    , k_end                                     )


       END DO
       !$OMP END PARALLEL DO





       IF (config_flags%polar) THEN

         CALL pxft ( grid=grid                                              &
               ,lineno=1169                                             &
               ,flag_uv            = 1                                      &
               ,flag_rurv          = 0                                      &
               ,flag_wph           = 0                                      &
               ,flag_ww            = 0                                      &
               ,flag_t             = 0                                      &
               ,flag_mu            = 0                                      &
               ,flag_mut           = 0                                      &
               ,flag_moist         = 0                                      &
               ,flag_chem          = 0                                      &
               ,flag_tracer        = 0                                      &
               ,flag_scalar        = 0                                      &
               ,positive_definite  = .FALSE.                                &
               ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
               ,fft_filter_lat = config_flags%fft_filter_lat                &
               ,dclat = dclat                                               &
               ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
               ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
               ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
               ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
               ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

       END IF





       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles


         IF( config_flags%specified .or. config_flags%nested ) THEN
           CALL spec_bdyupdate(grid%u_2, grid%ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                                grid%spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )

           CALL spec_bdyupdate(grid%v_2, grid%rv_tend, dts_rk,      &
                                'v'         , config_flags, &
                                grid%spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )

         ENDIF


       END DO
       !$OMP END PARALLEL DO

















CALL HALO_EM_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

        


         CALL advance_mu_t( grid%ww, ww1, grid%u_2, grid%u_save, grid%v_2, grid%v_save, &
                          grid%mu_2, grid%mut, muave, grid%muts, grid%muu, grid%muv,    &
                          grid%mudf, grid%ru_m, grid%rv_m, grid%ww_m,                   &
                          grid%t_2, grid%t_save, t_2save, t_tend,                       &
                          mu_tend,                                                      &
                          grid%rdx, grid%rdy, dts_rk, grid%epssm,                       &
                          grid%dnw, grid%fnm, grid%fnp, grid%rdnw,                      &
                          grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,            &
                          grid%msfvy, grid%msftx,grid%msfty,                            &
                          iteration, config_flags,                                      &
                          ids, ide, jds, jde, kds, kde,      &
                          ims, ime, jms, jme, kms, kme,      &
                          grid%i_start(ij), grid%i_end(ij),  &
                          grid%j_start(ij), grid%j_end(ij),  &
                          k_start    , k_end                )

       ENDDO
       !$OMP END PARALLEL DO





       IF ( (config_flags%polar) ) THEN

         CALL pxft ( grid=grid                                               &
                ,lineno=1276                                             &
                ,flag_uv            = 0                                      &
                ,flag_rurv          = 0                                      &
                ,flag_wph           = 0                                      &
                ,flag_ww            = 0                                      &
                ,flag_t             = 1                                      &
                ,flag_mu            = 1                                      &
                ,flag_mut           = 0                                      &
                ,flag_moist         = 0                                      &
                ,flag_chem          = 0                                      &
                ,flag_tracer        = 0                                      &
                ,flag_scalar        = 0                                      &
                ,positive_definite  = .FALSE.                                &
                ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                ,fft_filter_lat = config_flags%fft_filter_lat                &
                ,dclat = dclat                                               &
                ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         grid%muts = grid%mut + grid%mu_2  
 
       END IF







       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

         IF( config_flags%specified .or. config_flags%nested ) THEN

           CALL spec_bdyupdate(grid%t_2, t_tend, dts_rk,        &
                               't'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,       &
                               ims,ime, jms,jme, kms,kme,       &
                               ips,ipe, jps,jpe, kps,kpe,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               k_start    , k_end              )

           CALL spec_bdyupdate(grid%mu_2, mu_tend, dts_rk,       &
                               'm'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,       &
                               ims,ime, jms,jme, 1  ,1  ,       &
                               ips,ipe, jps,jpe, 1  ,1  ,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               1    , 1             )

           CALL spec_bdyupdate(grid%muts, mu_tend, dts_rk,      &
                              'm'         , config_flags, &
                              grid%spec_zone,                  &
                              ids,ide, jds,jde, 1  ,1  ,  & 
                              ims,ime, jms,jme, 1  ,1  ,  & 
                              ips,ipe, jps,jpe, 1  ,1  ,  & 
                              grid%i_start(ij), grid%i_end(ij),         &
                              grid%j_start(ij), grid%j_end(ij),         &
                              1    , 1             )
         ENDIF


         
         



         IF ( config_flags%non_hydrostatic ) THEN
           CALL advance_w( grid%w_2, rw_tend, grid%ww, w_save,         &
                           grid%u_2, grid%v_2,                         &
                           grid%mu_2, grid%mut, muave, grid%muts,      &
                           t_2save, grid%t_2, grid%t_save,             &
                           grid%ph_2, ph_save, grid%phb, ph_tend,      &
                           grid%ht, c2a, cqw, grid%alt, grid%alb,      &
                           a, alpha, gamma,                            &
                           grid%rdx, grid%rdy, dts_rk, t0, grid%epssm, &
                           grid%dnw, grid%fnm, grid%fnp, grid%rdnw,    &
                           grid%rdn, grid%cf1, grid%cf2, grid%cf3,     &
                           grid%msftx, grid%msfty,                     &
                           config_flags,  config_flags%top_lid,        &
                           ids,ide, jds,jde, kds,kde,                  &
                           ims,ime, jms,jme, kms,kme,                  &
                           grid%i_start(ij), grid%i_end(ij),           &
                           grid%j_start(ij), grid%j_end(ij),           &
                           k_start    , k_end                          )
         ENDIF


       ENDDO
       !$OMP END PARALLEL DO





       IF ( (config_flags%polar) .AND. (config_flags%non_hydrostatic) ) THEN

         CALL pxft ( grid=grid                                               &
                ,lineno=1382                                             &
                ,flag_uv            = 0                                      &
                ,flag_rurv          = 0                                      &
                ,flag_wph           = 1                                      &
                ,flag_ww            = 0                                      &
                ,flag_t             = 0                                      &
                ,flag_mu            = 0                                      &
                ,flag_mut           = 0                                      &
                ,flag_moist         = 0                                      &
                ,flag_chem          = 0                                      &
                ,flag_tracer        = 0                                      &
                ,flag_scalar        = 0                                      &
                ,positive_definite  = .FALSE.                                &
                ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                ,fft_filter_lat = config_flags%fft_filter_lat                &
                ,dclat = dclat                                               &
                ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

       END IF





       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles


         CALL sumflux ( grid%u_2, grid%v_2, grid%ww,          &
                        grid%u_save, grid%v_save, ww1,        &
                        grid%muu, grid%muv,                   &
                        grid%ru_m, grid%rv_m, grid%ww_m, grid%epssm,  &
                        grid%msfux, grid% msfuy, grid%msfvx,  &
                        grid%msfvx_inv, grid%msfvy,           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )


         IF( config_flags%specified .or. config_flags%nested ) THEN


           IF (config_flags%non_hydrostatic)  THEN
             CALL spec_bdyupdate_ph( ph_save, grid%ph_2, ph_tend,     &
                                     mu_tend, grid%muts, dts_rk,      &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )
             IF( config_flags%specified ) THEN
               CALL zero_grad_bdy ( grid%w_2,                         &
                                    'w'         , config_flags,       &
                                    grid%spec_zone,                   &
                                    ids,ide, jds,jde, kds,kde,        &
                                    ims,ime, jms,jme, kms,kme,        &
                                    ips,ipe, jps,jpe, kps,kpe,        &
                                    grid%i_start(ij), grid%i_end(ij), &
                                    grid%j_start(ij), grid%j_end(ij), &
                                    k_start    , k_end                )
             ELSE
               CALL spec_bdyupdate ( grid%w_2, rw_tend, dts_rk,       &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )
             ENDIF
           ENDIF

         ENDIF


         CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                 &
                          grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                          grid%mu_2, grid%muts, grid%znu, t0,         &
                          grid%rdnw, grid%dnw, grid%smdiv,            &
                          config_flags%non_hydrostatic, iteration,    &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )


       ENDDO
       !$OMP END PARALLEL DO
























CALL HALO_EM_C2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_B3_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

       

         CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( grid%al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( grid%p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc2d( grid%muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

       END DO
       !$OMP END PARALLEL DO


     END DO small_steps

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_small_finish' )

      
      
      


       CALL calc_mu_uv_1 ( config_flags,                     &
                           grid%muts, muus, muvs,                 &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )


       CALL small_step_finish( grid%u_2, grid%u_1, grid%v_2, grid%v_1, grid%w_2, grid%w_1,     &
                               grid%t_2, grid%t_1, grid%ph_2, grid%ph_1, grid%ww, ww1,    &
                               grid%mu_2, grid%mu_1,                       &
                               grid%mut, grid%muts, grid%muu, muus, grid%muv, muvs,  & 
                               grid%u_save, grid%v_save, w_save,           &
                               grid%t_save, ph_save, mu_save,         &
                               grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,grid%msfty, &
                               grid%h_diabatic,                       &
                               number_of_small_timesteps,dts_rk, &
                               rk_step, rk_order,                &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start    , k_end               )


       IF (rk_step == rk_order) THEN

         CALL set_physical_bc3d( grid%ru_m, 'u', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL set_physical_bc3d( grid%rv_m, 'v', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL set_physical_bc3d( grid%ww_m, 'w', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL set_physical_bc2d( grid%mut, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%muts, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )
 
       END IF



     END DO
     !$OMP END PARALLEL DO





     IF (config_flags%polar) THEN

       CALL pxft ( grid=grid                                                   &
                  ,lineno=1655                                             &
                  ,flag_uv            = 1                                      &
                  ,flag_rurv          = 1                                      &
                  ,flag_wph           = 1                                      &
                  ,flag_ww            = 1                                      &
                  ,flag_t             = 1                                      &
                  ,flag_mu            = 1                                      &
                  ,flag_mut           = 1                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = 0                                      &
                  ,positive_definite  = .FALSE.                                &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

     END IF











     IF ((config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. &
         (rk_step == rk_order)) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
         DO im = PARAM_FIRST_SCALAR, num_3d_m
           CALL rk_update_scalar_pd( im, im,                                   &
                                     moist_old(ims,kms,jms,im),                &
                                     moist_tend(ims,kms,jms,im),               &
                                     grid%mu_1, grid%mu_1, grid%mub,  &
                                     rk_step, dt_rk, grid%spec_zone,           &
                                     config_flags,                             &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end                       )
         ENDDO
       END DO
       !$OMP END PARALLEL DO


       IF (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN






CALL HALO_EM_MOIST_OLD_E_5_sub ( grid, &
  num_moist, &
  moist_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN






CALL HALO_EM_MOIST_OLD_E_7_sub ( grid, &
  num_moist, &
  moist_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal3("<stdin>",1964,&
TRIM(wrf_err_message))
         ENDIF
       ENDIF







CALL PERIOD_BDY_EM_MOIST_OLD_sub ( grid, &
  config_flags, &
  num_moist, &
  moist_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
           DO im = PARAM_FIRST_SCALAR , num_3d_m
             CALL set_physical_bc3d( moist_old(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,                  &
                                     ims, ime, jms, jme, kms, kme,                  &
                                     ips, ipe, jps, jpe, kps, kpe,                  &
                                     grid%i_start(ij), grid%i_end(ij),              &
                                     grid%j_start(ij), grid%j_end(ij),              &
                                     k_start    , k_end                            )
           END DO
         ENDIF
       END DO
       !$OMP END PARALLEL DO

     END IF  



     IF ((config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. &
         (rk_step == rk_order)) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
         DO im = PARAM_FIRST_SCALAR, num_3d_s
           CALL rk_update_scalar_pd( im, im,                                  &
                                     scalar_old(ims,kms,jms,im),              &
                                     scalar_tend(ims,kms,jms,im),             &
                                     grid%mu_1, grid%mu_1, grid%mub, &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                      )
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO


       IF (config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN






CALL HALO_EM_SCALAR_OLD_E_5_sub ( grid, &
  num_scalar, &
  scalar_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN






CALL HALO_EM_SCALAR_OLD_E_7_sub ( grid, &
  num_scalar, &
  scalar_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal3("<stdin>",2066,&
TRIM(wrf_err_message))
         ENDIF
  endif







CALL PERIOD_BDY_EM_SCALAR_OLD_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles
           IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_3d_s
               CALL set_physical_bc3d(  scalar_old(ims,kms,jms,im), 'p', config_flags, &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )
             END DO
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       END IF  



       IF ((config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order)) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c
             CALL rk_update_scalar_pd( im, im,                                  &
                                       chem_old(ims,kms,jms,im),                &
                                       chem_tend(ims,kms,jms,im),               &
                                       grid%mu_1, grid%mu_1, grid%mub, &
                                       rk_step, dt_rk, grid%spec_zone,          &
                                       config_flags,                            &
                                       ids, ide, jds, jde, kds, kde,            &
                                       ims, ime, jms, jme, kms, kme,            &
                                       grid%i_start(ij), grid%i_end(ij),        &
                                       grid%j_start(ij), grid%j_end(ij),        &
                                       k_start    , k_end                      )
           ENDDO
         END DO
         !$OMP END PARALLEL DO


         IF (config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN






CALL HALO_EM_CHEM_OLD_E_5_sub ( grid, &
  num_chem, &
  chem_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN






CALL HALO_EM_CHEM_OLD_E_7_sub ( grid, &
  num_chem, &
  chem_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE
             WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
             CALL wrf_error_fatal3("<stdin>",2167,&
TRIM(wrf_err_message))
           ENDIF
         ENDIF







CALL PERIOD_BDY_EM_CHEM_OLD_sub ( grid, &
  config_flags, &
  num_chem, &
  chem_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_3d_c
               CALL set_physical_bc3d(  chem_old(ims,kms,jms,im), 'p', config_flags,     &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )
             END DO 
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       ENDIF  



       IF ((config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order)) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           DO im = PARAM_FIRST_SCALAR, num_tracer
             CALL rk_update_scalar_pd( im, im,                                  &
                                       tracer_old(ims,kms,jms,im),                &
                                       tracer_tend(ims,kms,jms,im),               &
                                       grid%mu_1, grid%mu_1, grid%mub, &
                                       rk_step, dt_rk, grid%spec_zone,          &
                                       config_flags,                            &
                                       ids, ide, jds, jde, kds, kde,            &
                                       ims, ime, jms, jme, kms, kme,            &
                                       grid%i_start(ij), grid%i_end(ij),        &
                                       grid%j_start(ij), grid%j_end(ij),        &
                                       k_start    , k_end                      )
           ENDDO
         END DO
         !$OMP END PARALLEL DO


         IF (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN






CALL HALO_EM_TRACER_OLD_E_5_sub ( grid, &
  num_tracer, &
  tracer_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN






CALL HALO_EM_TRACER_OLD_E_7_sub ( grid, &
  num_tracer, &
  tracer_old, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE
             WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
             CALL wrf_error_fatal3("<stdin>",2268,&
TRIM(wrf_err_message))
           ENDIF
         ENDIF







CALL PERIOD_BDY_EM_TRACER_OLD_sub ( grid, &
  config_flags, &
  num_tracer, &
  tracer_old, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           IF (num_tracer >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_tracer
               CALL set_physical_bc3d(  tracer_old(ims,kms,jms,im), 'p', config_flags,   &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )
             END DO 
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       ENDIF  



       IF ((config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order) &
           .and. (config_flags%km_opt .eq. 2)                ) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           CALL rk_update_scalar_pd( 1, 1,                                    &
                                     grid%tke_1,                              &
                                     tke_tend(ims,kms,jms),                   &
                                     grid%mu_1, grid%mu_1, grid%mub,          &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                       )
         ENDDO
         !$OMP END PARALLEL DO


         IF (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN






CALL HALO_EM_TKE_OLD_E_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN






CALL HALO_EM_TKE_OLD_E_7_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE
             WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
             CALL wrf_error_fatal3("<stdin>",2364,&
TRIM(wrf_err_message))
           ENDIF
         ENDIF







CALL PERIOD_BDY_EM_TKE_OLD_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL set_physical_bc3d(  grid%tke_1, 'p', config_flags,  &
                                    ids, ide, jds, jde, kds, kde,      &
                                    ims, ime, jms, jme, kms, kme,      &
                                    ips, ipe, jps, jpe, kps, kpe,      &
                                    grid%i_start(ij), grid%i_end(ij),  &
                                    grid%j_start(ij), grid%j_end(ij),  &
                                    k_start    , k_end                )
         END DO
         !$OMP END PARALLEL DO



       END IF  























CALL HALO_EM_D_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL PERIOD_EM_DA_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
















       moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

         moist_variable_loop: DO im = PARAM_FIRST_SCALAR, num_3d_m




           IF (grid%adv_moist_cond .or. im==p_qv ) THEN

             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij, tenddec )
             moist_tile_loop_1: DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
               tenddec = .false.


               CALL rk_scalar_tend (  im, im, config_flags, tenddec,         & 
                           rk_step, dt_rk,                                   &
                           grid%ru_m, grid%rv_m, grid%ww_m,                  &
                           grid%muts, grid%mub, grid%mu_1,                   &
                           grid%alt,                                         &
                           moist_old(ims,kms,jms,im),                        &
                           moist(ims,kms,jms,im),                            &
                           moist_tend(ims,kms,jms,im),                       &
                           advect_tend,h_tendency,z_tendency,grid%rqvften,   & 
                           grid%qv_base, .true., grid%fnm, grid%fnp,         &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,&
                           grid%msfvy, grid%msftx,grid%msfty,                & 
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                           grid%kvdif, grid%xkhh,                            &
                           grid%diff_6th_opt, grid%diff_6th_factor,          &
                           config_flags%moist_adv_opt,                       &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )




               IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN 
                 IF ( im .EQ. P_QV .OR. config_flags%nested .OR. &
                    ( config_flags%specified .AND. config_flags%have_bcs_moist ) ) THEN
                   CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                     moist(ims,kms,jms,im),  grid%mut,         &
                                     moist_bxs(jms,kms,1,im),moist_bxe(jms,kms,1,im), &
                                     moist_bys(ims,kms,1,im),moist_bye(ims,kms,1,im), &
                                     moist_btxs(jms,kms,1,im),moist_btxe(jms,kms,1,im), &
                                     moist_btys(ims,kms,1,im),moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,             &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & 
                                     ims,ime, jms,jme, kms,kme,  & 
                                     ips,ipe, jps,jpe, kps,kpe,  & 
                                     grid%i_start(ij), grid%i_end(ij),      &
                                     grid%j_start(ij), grid%j_end(ij),      &
                                     k_start, k_end                        )

                   CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                     moist_bxs(jms,kms,1,im),moist_bxe(jms,kms,1,im), &
                                     moist_bys(ims,kms,1,im),moist_bye(ims,kms,1,im), &
                                     moist_btxs(jms,kms,1,im),moist_btxe(jms,kms,1,im), &
                                     moist_btys(ims,kms,1,im),moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone,                 &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & 
                                     ims,ime, jms,jme, kms,kme,  & 
                                     ips,ipe, jps,jpe, kps,kpe,  & 
                                     grid%i_start(ij), grid%i_end(ij),          &
                                     grid%j_start(ij), grid%j_end(ij),          &
                                     k_start, k_end                               )
                 ENDIF
               ENDIF


             ENDDO moist_tile_loop_1
             !$OMP END PARALLEL DO

             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij, tenddec )
             moist_tile_loop_2: DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 200 , ' call rk_update_scalar' )
               tenddec = .false.


               CALL rk_update_scalar( scs=im, sce=im,                                  &
                               scalar_1=moist_old(ims,kms,jms,im),                     &
                               scalar_2=moist(ims,kms,jms,im),                         &
                               sc_tend=moist_tend(ims,kms,jms,im),                     &
                               advect_tend=advect_tend,                                &
                               h_tendency=h_tendency, z_tendency=z_tendency,           & 
                               msftx=grid%msftx,msfty=grid%msfty,                      &
                               mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                               rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                               config_flags=config_flags, tenddec=tenddec,             & 
                               ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                               ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                               its=grid%i_start(ij), ite=grid%i_end(ij),               &
                               jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                               kts=k_start    , kte=k_end                              )



               IF( config_flags%specified .AND. ( .NOT. config_flags%have_bcs_moist ) ) THEN
                 IF(im .ne. P_QV)THEN
                   CALL flow_dep_bdy  (  moist(ims,kms,jms,im),                 &
                                grid%ru_m, grid%rv_m, config_flags,             &
                                grid%spec_zone,                                 &
                                ids,ide, jds,jde, kds,kde,                      &
                                ims,ime, jms,jme, kms,kme,                      &
                                ips,ipe, jps,jpe, kps,kpe,                      &
                                grid%i_start(ij), grid%i_end(ij),               &
                                grid%j_start(ij), grid%j_end(ij),               &
                                k_start, k_end                               )
                 ENDIF
               ENDIF


             ENDDO moist_tile_loop_2
             !$OMP END PARALLEL DO

           ENDIF  

         ENDDO moist_variable_loop

       ENDIF moist_scalar_advance


       TKE_advance: IF (config_flags%km_opt .eq. 2) then
         IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_TKE_ADVECT_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_TKE_ADVECT_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE
          WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
          CALL wrf_error_fatal3("<stdin>",2623,&
TRIM(wrf_err_message))
         ENDIF
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         tke_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
           tenddec = .false.
           CALL rk_scalar_tend ( 1, 1, config_flags, tenddec,                      & 
                            rk_step, dt_rk,                                        &
                            grid%ru_m, grid%rv_m, grid%ww_m,                       &
                            grid%muts, grid%mub, grid%mu_1,                        &
                            grid%alt,                                              &
                            grid%tke_1,                                            &
                            grid%tke_2,                                            &
                            tke_tend(ims,kms,jms),                                 &
                            advect_tend,h_tendency,z_tendency,grid%rqvften,        & 
                            grid%qv_base, .false., grid%fnm, grid%fnp,             &
                            grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,     &
                            grid%msfvy, grid%msftx,grid%msfty,                     &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif,   &
                            grid%kvdif, grid%xkhh,                                 &
                            grid%diff_6th_opt, grid%diff_6th_factor,               &
                            config_flags%tke_adv_opt,                              &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

         ENDDO tke_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         tke_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = .false.
           CALL rk_update_scalar( scs=1,  sce=1,                                          &
                                  scalar_1=grid%tke_1,                                    &
                                  scalar_2=grid%tke_2,                                    &
                                  sc_tend=tke_tend(ims,kms,jms),                          &
                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )



           CALL bound_tke( grid%tke_2, grid%tke_upper_bound,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           grid%i_start(ij), grid%i_end(ij),    &
                           grid%j_start(ij), grid%j_end(ij),    &
                           k_start    , k_end                  )

           IF( config_flags%specified .or. config_flags%nested ) THEN
              CALL flow_dep_bdy (  grid%tke_2,                     &
                                   grid%ru_m, grid%rv_m, config_flags,               &
                                   grid%spec_zone,                              &
                                   ids,ide, jds,jde, kds,kde,  & 
                                   ims,ime, jms,jme, kms,kme,  & 
                                   ips,ipe, jps,jpe, kps,kpe,  & 
                                   grid%i_start(ij), grid%i_end(ij),       &
                                   grid%j_start(ij), grid%j_end(ij),       &
                                   k_start, k_end                               )
           ENDIF
         ENDDO tke_tile_loop_2
         !$OMP END PARALLEL DO

       ENDIF TKE_advance




       tracer_advance: IF (num_tracer >= PARAM_FIRST_SCALAR)  THEN

         tracer_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_tracer

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij, tenddec )
           tracer_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 15 , ' call rk_scalar_tend in tracer_tile_loop_1' )
             tenddec = .false.
             CALL rk_scalar_tend ( ic, ic, config_flags, tenddec,                & 
                              rk_step, dt_rk,                                    &
                              grid%ru_m, grid%rv_m, grid%ww_m,                   &
                              grid%muts, grid%mub, grid%mu_1,                    &
                              grid%alt,                                          &
                              tracer_old(ims,kms,jms,ic),                          &
                              tracer(ims,kms,jms,ic),                              &
                              tracer_tend(ims,kms,jms,ic),                         &
                              advect_tend,h_tendency,z_tendency,grid%rqvften,    & 
                              grid%qv_base, .false., grid%fnm, grid%fnp,         &
                              grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                              grid%msfvy, grid%msftx,grid%msfty,                 &
                              grid%rdx, grid%rdy, grid%rdn, grid%rdnw,           &
                              grid%khdif, grid%kvdif, grid%xkhh,                 &
                              grid%diff_6th_opt, grid%diff_6th_factor,           &
                              config_flags%tracer_adv_opt,                         &
                              ids, ide, jds, jde, kds, kde,                      &
                              ims, ime, jms, jme, kms, kme,                      &
                              grid%i_start(ij), grid%i_end(ij),                  &
                              grid%j_start(ij), grid%j_end(ij),                  &
                              k_start    , k_end                                )






           IF( ( config_flags%nested ) .and. rk_step == 1 ) THEN
             IF(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_tracer' )
             CALL relax_bdy_scalar ( tracer_tend(ims,kms,jms,ic),                                    &
                                     tracer(ims,kms,jms,ic),  grid%mut,                              &
                                     tracer_bxs(jms,kms,1,ic),tracer_bxe(jms,kms,1,ic),                &
                                     tracer_bys(ims,kms,1,ic),tracer_bye(ims,kms,1,ic),                &
                                     tracer_btxs(jms,kms,1,ic),tracer_btxe(jms,kms,1,ic),              &
                                     tracer_btys(ims,kms,1,ic),tracer_btye(ims,kms,1,ic),              &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,                                &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )
             CALL spec_bdy_scalar  ( tracer_tend(ims,kms,jms,ic),                 &
                                     tracer_bxs(jms,kms,1,ic),tracer_bxe(jms,kms,1,ic),                &
                                     tracer_bys(ims,kms,1,ic),tracer_bye(ims,kms,1,ic),                &
                                     tracer_btxs(jms,kms,1,ic),tracer_btxe(jms,kms,1,ic),              &
                                     tracer_btys(ims,kms,1,ic),tracer_btye(ims,kms,1,ic),              &
                                     config_flags%spec_bdy_width, grid%spec_zone,                  &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )
           ENDIF

         ENDDO tracer_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )

         tracer_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = .false.
           CALL rk_update_scalar( scs=ic, sce=ic,                                         &
                                  scalar_1=tracer_old(ims,kms,jms,ic),                    &
                                  scalar_2=tracer(ims,kms,jms,ic),                        &
                                  sc_tend=tracer_tend(ims,kms,jms,ic),                    &


                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF( config_flags%specified  ) THEN
             CALL flow_dep_bdy  ( tracer(ims,kms,jms,ic),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  ids,ide, jds,jde, kds,kde,  & 
                                  ims,ime, jms,jme, kms,kme,  & 
                                  ips,ipe, jps,jpe, kps,kpe,  & 
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
           ENDIF
         ENDDO tracer_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO tracer_variable_loop
     ENDIF tracer_advance



     other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

       scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
           tenddec = .false.
           CALL rk_scalar_tend ( is, is, config_flags, tenddec,                   & 
                                 rk_step, dt_rk,                                  &
                                 grid%ru_m, grid%rv_m, grid%ww_m,                 &
                                 grid%muts, grid%mub, grid%mu_1,                  &
                                 grid%alt,                                        &
                                 scalar_old(ims,kms,jms,is),                      &
                                 scalar(ims,kms,jms,is),                          &
                                 scalar_tend(ims,kms,jms,is),                     &
                                 advect_tend,h_tendency,z_tendency,grid%rqvften,  & 
                                 grid%qv_base, .false., grid%fnm, grid%fnp,       &
                                 grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                                 grid%msfvy, grid%msftx,grid%msfty,               &
                                 grid%rdx, grid%rdy, grid%rdn, grid%rdnw,         &
                                 grid%khdif, grid%kvdif, grid%xkhh,               &
                                 grid%diff_6th_opt, grid%diff_6th_factor,         &
                                 config_flags%scalar_adv_opt,                     &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

           IF( rk_step == 1 ) THEN
             IF ( config_flags%nested .OR. &
                ( config_flags%specified .AND. config_flags%have_bcs_scalar ) .OR. &
                ( ( is .EQ. P_QNWFA .OR. is .EQ. P_QNIFA) .AND. config_flags%use_aero_icbc ) ) THEN

               CALL relax_bdy_scalar ( scalar_tend(ims,kms,jms,is),                            &
                                       scalar(ims,kms,jms,is),  grid%mut,                      &
                                       scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is),      &
                                       scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is),      &
                                       scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is),    &
                                       scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is),    &
                                       config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                       grid%dtbc, grid%fcx, grid%gcx,                          &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

               CALL spec_bdy_scalar  ( scalar_tend(ims,kms,jms,is),                            &
                                       scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is),      &
                                       scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is),      &
                                       scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is),    &
                                       scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is),    &
                                       config_flags%spec_bdy_width, grid%spec_zone,            &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

             ENDIF
           ENDIF 

         ENDDO scalar_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = .false.
           CALL rk_update_scalar( scs=is, sce=is,                                         &
                                  scalar_1=scalar_old(ims,kms,jms,is),                    &
                                  scalar_2=scalar(ims,kms,jms,is),                        &
                                  sc_tend=scalar_tend(ims,kms,jms,is),                    &


                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF ( config_flags%specified ) THEN

             IF ( is .EQ. P_QNN ) THEN
               CALL flow_dep_bdy_qnn  ( scalar(ims,kms,jms,is),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  ids,ide, jds,jde, kds,kde,  & 
                                  ims,ime, jms,jme, kms,kme,  & 
                                  ips,ipe, jps,jpe, kps,kpe,  & 
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
             ELSE IF ( ( ( ( is .EQ. P_QNWFA ) .OR. ( is .EQ. P_QNIFA ) ) .AND. &
                         ( .NOT. config_flags%use_aero_icbc ) ) &
                         .OR. &
                       ( ( .NOT. ( ( is .EQ. P_QNWFA ) .OR. ( is .EQ. P_QNIFA ) ) ) .AND. &
                         ( .NOT. config_flags%have_bcs_scalar ) ) ) THEN























               CALL flow_dep_bdy  ( scalar(ims,kms,jms,is),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  ids,ide, jds,jde, kds,kde,  & 
                                  ims,ime, jms,jme, kms,kme,  & 
                                  ips,ipe, jps,jpe, kps,kpe,  & 
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
             ENDIF

           ENDIF

         ENDDO scalar_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO scalar_variable_loop

     ENDIF other_scalar_advance

 

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles



       CALL calc_p_rho_phi( moist, num_3d_m, config_flags%hypsometric_opt,             &
                            grid%al, grid%alb, grid%mu_2, grid%muts,              &
                            grid%ph_2, grid%phb, grid%p, grid%pb, grid%t_2,                 &
                            p0, t0, grid%p_top, grid%znu, grid%znw, grid%dnw, grid%rdnw,           &
                            grid%rdn, config_flags%non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )



     ENDDO
     !$OMP END PARALLEL DO





     rk_step_1_check: IF ( rk_step < rk_order ) THEN





       IF (config_flags%polar) THEN 
         IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter moist ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_m
             CALL couple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)              &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             CALL pxft ( grid=grid                                               &
                    ,lineno=2720                                             &
                    ,flag_uv            = 0                                      &
                    ,flag_rurv          = 0                                      &
                    ,flag_wph           = 0                                      &
                    ,flag_ww            = 0                                      &
                    ,flag_t             = 0                                      &
                    ,flag_mu            = 0                                      &
                    ,flag_mut           = 0                                      &
                    ,flag_moist         = im                                     &
                    ,flag_chem          = 0                                      &
                    ,flag_scalar        = 0                                      &
                    ,flag_tracer        = 0                                      &
                    ,positive_definite=.FALSE.                                   &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)            &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
           END DO
         END IF
   
         IF ( num_3d_c >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter chem ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c
             CALL couple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)               &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )
             CALL pxft ( grid=grid                                               &
                    ,lineno=2758                                             &
                    ,flag_uv            = 0                                      &
                    ,flag_rurv          = 0                                      &
                    ,flag_wph           = 0                                      &
                    ,flag_ww            = 0                                      &
                    ,flag_t             = 0                                      &
                    ,flag_mu            = 0                                      &
                    ,flag_mut           = 0                                      &
                    ,flag_moist         = 0                                      &
                    ,flag_chem          = im                                     &
                    ,flag_tracer        = 0                                      &
                    ,flag_scalar        = 0                                      &
                    ,positive_definite=.FALSE.                                   &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)             &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
           END DO
         END IF
         IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter tracer ' )
           DO im = PARAM_FIRST_SCALAR, num_tracer
             CALL couple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)               &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )
             CALL pxft ( grid=grid                                               &
                    ,lineno=2795                                             &
                    ,flag_uv            = 0                                      &
                    ,flag_rurv          = 0                                      &
                    ,flag_wph           = 0                                      &
                    ,flag_ww            = 0                                      &
                    ,flag_t             = 0                                      &
                    ,flag_mu            = 0                                      &
                    ,flag_mut           = 0                                      &
                    ,flag_moist         = 0                                      &
                    ,flag_chem          = 0                                      &
                    ,flag_tracer        = im                                      &
                    ,flag_scalar        = 0                                      &
                    ,positive_definite=.FALSE.                                   &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)             &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
           END DO
         END IF
   
         IF ( num_3d_s >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter scalar ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_s
             CALL couple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)           &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             CALL pxft ( grid=grid                                             &
                  ,lineno=2833                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = im                                     &
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)   &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
           END DO
         END IF
       END IF 













































       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_D2_3_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_tracer, &
  tracer, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_D2_5_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_tracer, &
  tracer, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

       ELSE 
         WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal3("<stdin>",3256,&
TRIM(wrf_err_message))
       ENDIF






CALL PERIOD_BDY_EM_D_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_MOIST2_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_CHEM2_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_TRACER2_sub ( grid, &
  config_flags, &
  num_tracer, &
  tracer, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_SCALAR2_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       tile_bc_loop_1: DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

         CALL rk_phys_bc_dry_2( config_flags,                     &
                                grid%u_2, grid%v_2, grid%w_2,     &
                                grid%t_2, grid%ph_2, grid%mu_2,   &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end               )


         IF (.not. config_flags%non_hydrostatic) THEN
           CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, dt_rk,  &
                            grid%u_2, grid%v_2, grid%ht,                           &
                            grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                            ids, ide, jds, jde, kds, kde,           &
                            ims, ime, jms, jme, kms, kme,           &
                            grid%i_start(ij), grid%i_end(ij),       &
                            grid%j_start(ij), grid%j_end(ij),       &
                            k_start    , k_end                     )
         ENDIF


         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

           moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m
  
             CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     ips, ipe, jps, jpe, kps, kpe,             &
                                     grid%i_start(ij), grid%i_end(ij),                   &
                                     grid%j_start(ij), grid%j_end(ij),                   &
                                     k_start    , k_end                       )
           END DO moisture_loop_bdy_1

         ENDIF

         IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN

           chem_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

             CALL set_physical_bc3d( chem(ims,kms,jms,ic), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO chem_species_bdy_loop_1

         END IF

         IF (num_tracer >= PARAM_FIRST_SCALAR) THEN

           tracer_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_tracer

             CALL set_physical_bc3d( tracer(ims,kms,jms,ic), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO tracer_species_bdy_loop_1

         END IF

         IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN

           scalar_species_bdy_loop_1 : DO is = PARAM_FIRST_SCALAR , num_3d_s

             CALL set_physical_bc3d( scalar(ims,kms,jms,is), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO scalar_species_bdy_loop_1

         END IF

         IF (config_flags%km_opt .eq. 2) THEN

           CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,            &
                                   ims, ime, jms, jme, kms, kme,            &
                                   ips, ipe, jps, jpe, kps, kpe,            &
                                   grid%i_start(ij), grid%i_end(ij),        &
                                   grid%j_start(ij), grid%j_end(ij),        &
                                   k_start    , k_end                      )
         END IF

       END DO tile_bc_loop_1
       !$OMP END PARALLEL DO













       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_TKE_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE






CALL HALO_EM_TKE_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ENDIF
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_TKE_7_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ELSE






CALL HALO_EM_TKE_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

         ENDIF
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
         CALL wrf_error_fatal3("<stdin>",3518,&
TRIM(wrf_err_message))
       ENDIF

       IF ( num_moist .GE. PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_MOIST_E_5_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_MOIST_E_3_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           END IF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_MOIST_E_7_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_MOIST_E_5_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           END IF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal3("<stdin>",3593,&
TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_chem >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_CHEM_E_5_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_CHEM_E_3_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_CHEM_E_7_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_CHEM_E_5_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal3("<stdin>",3668,&
TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_TRACER_E_5_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_TRACER_E_3_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_TRACER_E_7_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_TRACER_E_5_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal3("<stdin>",3743,&
TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_scalar >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_SCALAR_E_5_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_SCALAR_E_3_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN






CALL HALO_EM_SCALAR_E_7_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ELSE






CALL HALO_EM_SCALAR_E_5_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal3("<stdin>",3818,&
TRIM(wrf_err_message))
         ENDIF
       ENDIF

     ENDIF rk_step_1_check








   END DO Runge_Kutta_loop

   IF      ( config_flags%traj_opt .EQ. UM_TRAJECTORY ) THEN






CALL HALO_EM_F_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_D_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_4_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

           call trajectory (grid,config_flags,  &
                            grid%dt,grid%itimestep,grid%ru_m, grid%rv_m, grid%ww_m,&
                            grid%mut,grid%muu,grid%muv,                            &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw,grid%rdzw,     &
                            grid%traj_i,grid%traj_j,grid%traj_k,                   &
                            grid%traj_long,grid%traj_lat,                          &
                            grid%xlong,grid%xlat,            &
                            grid%msftx,grid%msfux,grid%msfvy,                      &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start  , k_end )
        ENDDO
     !$OMP END PARALLEL DO
   ENDIF


   IF (config_flags%do_avgflx_em .EQ. 1) THEN

      CALL WRFU_ALARMGET(grid%alarms( HISTORY_ALARM ),prevringtime=temp_time)
      CALL domain_clock_get ( grid, current_time=CurrTime, &
           current_timestr=message2 )



      WRITE ( message , FMT = '("solve_em: old_dt =",g15.6,", dt=",g15.6," on domain ",I3)' ) &
           & old_dt,grid%dt,grid%id
      CALL wrf_debug(200,message)
      old_dt=min(old_dt,grid%dt)
      num = INT(old_dt * precision)
      den = precision
      CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)
      IF (CurrTime .lt. temp_time + dtInterval) THEN
         WRITE ( message , FMT = '("solve_em: initializing avgflx at time ",A," on domain ",I3)' ) &
              & TRIM(message2), grid%id
         CALL wrf_message(trim(message)) 
         grid%avgflx_count = 0

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles
            CALL wrf_debug(200,'In solve_em, before zero_avgflx call')
            CALL zero_avgflx(grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
                 &   ids, ide, jds, jde, kds, kde,           &
                 &   ims, ime, jms, jme, kms, kme,           &
                 &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
                 &   k_start    , k_end, f_flux, &
                 &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
                 &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
            CALL wrf_debug(200,'In solve_em, after zero_avgflx call')
         ENDDO
      ENDIF



   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         CALL wrf_debug(200,'In solve_em, before upd_avgflx call')
         CALL upd_avgflx(grid%avgflx_count,grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
              &   grid%ru_m, grid%rv_m, grid%ww_m, &
              &   ids, ide, jds, jde, kds, kde,           &
              &   ims, ime, jms, jme, kms, kme,           &
              &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
              &   k_start    , k_end, f_flux, &
              &   grid%cfu1,grid%cfd1,grid%dfu1,grid%efu1,grid%dfd1,grid%efd1,          &
              &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
              &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
         CALL wrf_debug(200,'In solve_em, after upd_avgflx call')
         
      ENDDO
      grid%avgflx_count = grid%avgflx_count + 1
   ENDIF

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles


     CALL wrf_debug ( 200 , ' call advance_ppt' )
     CALL advance_ppt(grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                      grid%rqicuten,grid%rqscuten,           &
                      grid%rainc,grid%raincv,grid%rainsh,grid%pratec,grid%pratesh, &
                      grid%nca,grid%htop,grid%hbot,grid%cutop,grid%cubot,  &
                      grid%cuppt, grid%dt, config_flags,                   &
                      ids,ide, jds,jde, kds,kde,             &
                      ims,ime, jms,jme, kms,kme,             &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start    , k_end                    )


   ENDDO
  !$OMP END PARALLEL DO















   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = grid%spec_zone
   ELSE
     sz = 0
   ENDIF

   IF (config_flags%mp_physics /= 0)  then

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )

     scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL wrf_debug ( 200 , ' call moist_physics_prep' )

       CALL moist_physics_prep_em( grid%t_2, grid%t_1, t0, grid%rho,           &
                                   grid%al, grid%alb, grid%p, p8w, p0, grid%pb,          &
                                   grid%ph_2, grid%phb, th_phy, pi_phy , p_phy, &
                                   grid%z, grid%z_at_w, dz8w,        &
                                   dtm, grid%h_diabatic,                  &
                                   config_flags,grid%fnm, grid%fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )

     END DO scalar_tile_loop_1a
     !$OMP END PARALLEL DO

     CALL wrf_debug ( 200 , ' call microphysics_driver' )

     grid%sr = 0.
     specified_bdy = config_flags%specified .OR. config_flags%nested
     channel_bdy = config_flags%specified .AND. config_flags%periodic_x



























CALL HALO_EM_SBM_sub ( grid, &
  p_phy, &
  pi_phy, &
  dz8w, &
  th_phy, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



     CALL microphysics_driver(                                            &
      &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy   &
      &        ,DZ8W=dz8w          ,F_ICE_PHY=grid%f_ice_phy              &
      &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr  &
      &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy     &
      &        ,RHO=grid%rho       ,SPEC_ZONE=grid%spec_zone              &
      &        ,SR=grid%sr              ,TH=th_phy                        &
      &        ,refl_10cm=grid%refl_10cm                                  & 
      &        ,WARM_RAIN=grid%warm_rain                                  &
      &        ,T8W=t8w                                                   &
      &        ,CLDFRA=grid%cldfra, EXCH_H=grid%exch_h &
      &        ,NSOURCE=grid%qndropsource                                 &
      &        ,XLAND=grid%xland,SNOWH=grid%SNOW                           &  
      &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy       &
      &        ,F_RAIN_PHY=grid%f_rain_phy                                &
      &        ,F_RIMEF_PHY=grid%f_rimef_phy                              &
      &        ,MP_PHYSICS=config_flags%mp_physics                        &
      &        ,ID=grid%id                                                &
      &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde         &
      &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme         &
      &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe         &
      &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
      &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
      &        ,KTS=k_start, KTE=min(k_end,kde-1)                         &
      &        ,NUM_TILES=grid%num_tiles                                  &
      &        ,NAER=grid%naer                                            &

      
      &        ,DLF=grid%dlf,DLF2=grid%dlf2,T_PHY=grid%t_phy,P_HYD=grid%p_hyd  &
      &        ,P8W_HYD=grid%p_hyd_w,TKE_PBL=grid%tke_pbl                 &
      &        ,Z_AT_W=grid%z_at_w,QFX=grid%qfx,RLIQ=grid%rliq            &
      &        ,TURBTYPE3D=grid%turbtype3d,SMAW3D=grid%smaw3d             &
      &        ,WSEDL3D=grid%wsedl3d,CLDFRA_OLD_MP=grid%cldfra_old_mp     &
      &        ,CLDFRA_MP=grid%cldfra_mp,CLDFRA_MP_ALL=grid%cldfra_mp_ALL &
      &        ,LRADIUS=grid%LRADIUS, IRADIUS=grid%IRADIUS                & 
      &        ,CLDFRAI=grid%cldfrai                                      &
      &        ,CLDFRAL=grid%cldfral,CLDFRA_CONV=grid%CLDFRA_CONV         &
      &        ,ALT=grid%alt                                              &
      &        ,ACCUM_MODE=config_flags%accum_mode                        &
      &        ,AITKEN_MODE=config_flags%aitken_mode                      &
      &        ,COARSE_MODE=config_flags%coarse_mode                      &
      &        ,ICWMRSH3D=grid%icwmrsh,ICWMRDP3D=grid%icwmrdp3d           &
      &        ,SHFRC3D=grid%shfrc3d,CMFMC3D=grid%cmfmc                   &
      &        ,CMFMC2_3D=grid%cmfmc2,CONFIG_FLAGS=config_flags           &
      &        ,FNM=grid%fnm,FNP=grid%fnp,RH_OLD_MP=grid%rh_old_mp        &
      &        ,LCD_OLD_MP=grid%lcd_old_mp                                &

                 
      &        , RAINNC=grid%rainnc, RAINNCV=grid%rainncv                 &
      &        , SNOWNC=grid%snownc, SNOWNCV=grid%snowncv                 &
      &        , GRAUPELNC=grid%graupelnc, GRAUPELNCV=grid%graupelncv     & 
      &        , HAILNC=grid%hailnc, HAILNCV=grid%hailncv                 &
      &        , W=grid%w_2, Z=grid%z, HT=grid%ht                         &
      &        , MP_RESTART_STATE=grid%mp_restart_state                   &
      &        , TBPVS_STATE=grid%tbpvs_state                             & 
      &        , TBPVS0_STATE=grid%tbpvs0_state                           & 
      &        , QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV               &
      &        , QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC               &
      &        , QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR               &
      &        , QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI               &
      &        , QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS               &
      &        , QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG               &
      &        , QH_CURR=moist(ims,kms,jms,P_QH), F_QH=F_QH               & 
      &        , QIC_CURR=moist(ims,kms,jms,P_QIC), F_QIC=F_QIC               &
      &        , QIP_CURR=moist(ims,kms,jms,P_QIP), F_QIP=F_QIP               &
      &        , QID_CURR=moist(ims,kms,jms,P_QID), F_QID=F_QID               &
      &        , QNDROP_CURR=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
      &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT              &
      &        , QNN_CURR=scalar(ims,kms,jms,P_QNN), F_QNN=F_QNN          &
      &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI          &
      &        , QNC_CURR=scalar(ims,kms,jms,P_QNC), F_QNC=F_QNC          &
      &        , QNR_CURR=scalar(ims,kms,jms,P_QNR), F_QNR=F_QNR          &
      &        , QNS_CURR=scalar(ims,kms,jms,P_QNS), F_QNS=F_QNS          &
      &        , QNG_CURR=scalar(ims,kms,jms,P_QNG), F_QNG=F_QNG          &
      &        , QNWFA_CURR=scalar(ims,kms,jms,P_QNWFA), F_QNWFA=F_QNWFA  & 
      &        , QNIFA_CURR=scalar(ims,kms,jms,P_QNIFA), F_QNIFA=F_QNIFA  & 
      &        , QNH_CURR=scalar(ims,kms,jms,P_QNH), F_QNH=F_QNH          & 
      &        , QNIC_CURR=scalar(ims,kms,jms,P_QNIC), F_QNIC=F_QNIC          &
      &        , QNIP_CURR=scalar(ims,kms,jms,P_QNIP), F_QNIP=F_QNIP          &
      &        , QNID_CURR=scalar(ims,kms,jms,P_QNID), F_QNID=F_QNID          &






      &        , QVOLG_CURR=scalar(ims,kms,jms,P_QVOLG), F_QVOLG=F_QVOLG      & 
      &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten             &
      &        , qicuten=grid%rqicuten,mu=grid%mut                        &
      &        , HAIL=config_flags%gsfcgce_hail                           & 
      &        , ICE2=config_flags%gsfcgce_2ice                           & 



      &        , RI_CURR=grid%rimi                                          &
      &        , re_cloud=grid%re_cloud, re_ice=grid%re_ice, re_snow=grid%re_snow & 
      &        , has_reqc=grid%has_reqc, has_reqi=grid%has_reqi, has_reqs=grid%has_reqs & 
      &        , qnwfa2d=grid%qnwfa2d                                                   & 
      &        , diagflag=diag_flag, do_radar_ref=config_flags%do_radar_ref &
      &        ,u=grid%u_phy,v=grid%v_phy &
      &        ,scalar=scalar,num_scalar=num_scalar                             &
      &        ,TH_OLD=grid%th_old                                        &
      &        ,QV_OLD=grid%qv_old                                        &
      &        ,xlat=grid%xlat,xlong=grid%xlong,IVGTYP=grid%ivgtyp  &
      &        , EFFR_CURR=scalar(ims,kms,jms,P_EFFR), F_EFFR=F_EFFR          & 
      &        , ICE_EFFR_CURR=scalar(ims,kms,jms,P_ICE_EFFR), F_ICE_EFFR=F_ICE_EFFR          & 
      &        , TOT_EFFR_CURR=scalar(ims,kms,jms,P_TOT_EFFR), F_TOT_EFFR=F_TOT_EFFR          & 
      &        , QIC_EFFR_CURR=scalar(ims,kms,jms,P_QIC_EFFR), F_QIC_EFFR=F_QIC_EFFR          & 
      &        , QIP_EFFR_CURR=scalar(ims,kms,jms,P_QIP_EFFR), F_QIP_EFFR=F_QIP_EFFR          & 
      &        , QID_EFFR_CURR=scalar(ims,kms,jms,P_QID_EFFR), F_QID_EFFR=F_QID_EFFR          & 
      &        ,kext_ql=grid%kext_ql                                       &
      &        ,kext_qs=grid%kext_qs                                       &
      &        ,kext_qg=grid%kext_qg                                       &
      &        ,kext_qh=grid%kext_qh                                       &
      &        ,kext_qa=grid%kext_qa                                       &
      &        ,kext_qic=grid%kext_qic                                       &
      &        ,kext_qip=grid%kext_qip                                       &
      &        ,kext_qid=grid%kext_qid                                       &
      &        ,kext_ft_qic=grid%kext_ft_qic                                       &
      &        ,kext_ft_qip=grid%kext_ft_qip                                       &
      &        ,kext_ft_qid=grid%kext_ft_qid                                       &
      &        ,kext_ft_qs=grid%kext_ft_qs                                       &
      &        ,kext_ft_qg=grid%kext_ft_qg         &
      &        ,height=grid%height                                         &
      &        ,tempc=grid%tempc                                         &

                                                                          )



     CALL wrf_debug ( 200 , ' call moist_physics_finish' )


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)

       CALL microphysics_zero_outb (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outb (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outb (                                    &
                      chem , num_chem , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )
       CALL microphysics_zero_outb (                                    &
                      tracer , num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL microphysics_zero_outa (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outa (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outa (                                    &
                      chem , num_chem , config_flags ,                  &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outa (                                    &
                      tracer , num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL moist_physics_finish_em( grid%t_2, grid%t_1, t0, grid%muts, th_phy,       &
                                      grid%h_diabatic, dtm, config_flags,    &
                                      ids, ide, jds, jde, kds, kde,     &
                                      ims, ime, jms, jme, kms, kme,     &
                                      its, ite, jts, jte,               &
                                      k_start    , k_end               )

     END DO
     !$OMP END PARALLEL DO

   ENDIF  





   IF (config_flags%polar) THEN
     IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
       CALL wrf_debug ( 200 , ' call filter moist' )
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               moist(ii,kk,jj,im)=moist(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
 
         CALL pxft ( grid=grid                                                 &
                  ,lineno=3604                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = im                                     &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = 0                                      &
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
 
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               moist(ii,kk,jj,im)=moist(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO
     ENDIF
   ENDIF






   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )
   scalar_tile_loop_1ba: DO ij = 1 , grid%num_tiles

     IF ( config_flags%periodic_x ) THEN
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
     ELSE
       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
     ENDIF
     jts = max(grid%j_start(ij),jds+sz)
     jte = min(grid%j_end(ij),jde-1-sz)

     CALL calc_p_rho_phi( moist, num_3d_m, config_flags%hypsometric_opt,             &
                          grid%al, grid%alb, grid%mu_2, grid%muts,              &
                          grid%ph_2, grid%phb, grid%p, grid%pb, grid%t_2,                 &
                          p0, t0, grid%p_top, grid%znu, grid%znw, grid%dnw, grid%rdnw,           &
                          grid%rdn, config_flags%non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          its, ite, jts, jte,               &
                          k_start    , k_end               )

   END DO scalar_tile_loop_1ba
   !$OMP END PARALLEL DO


   IF (.not. config_flags%non_hydrostatic) THEN






CALL HALO_EM_HYDRO_UV_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_HYDRO_UV_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles
       CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, dt_rk,  &
                       grid%u_2, grid%v_2, grid%ht,                           &
                       grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                       ids, ide, jds, jde, kds, kde,           &
                       ims, ime, jms, jme, kms, kme,           &
                       grid%i_start(ij), grid%i_end(ij),       &
                       grid%j_start(ij), grid%j_end(ij),       &
                       k_start    , k_end                     )

     END DO
     !$OMP END PARALLEL DO

   END IF

   CALL wrf_debug ( 200 , ' call chem polar filter ' )





   IF (config_flags%polar) THEN

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then
       chem_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_c
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               chem(ii,kk,jj,im)=chem(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO

         CALL pxft ( grid=grid                                                 &
                  ,lineno=3711                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = im                                     &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = 0                                      &
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               chem(ii,kk,jj,im)=chem(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO chem_filter_loop
     ENDIF
     IF ( num_tracer >= PARAM_FIRST_SCALAR ) then
       tracer_filter_loop: DO im = PARAM_FIRST_SCALAR, num_tracer
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               tracer(ii,kk,jj,im)=tracer(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO

         CALL pxft ( grid=grid                                                 &
                  ,lineno=3753                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = im                                    &
                  ,flag_scalar        = 0                                      &
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               tracer(ii,kk,jj,im)=tracer(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO tracer_filter_loop
     ENDIF

     IF ( num_3d_s >= PARAM_FIRST_SCALAR ) then
       scalar_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_s
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO

         CALL pxft ( grid=grid                                                 &
                  ,lineno=3796                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = im                                     &
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO scalar_filter_loop
     ENDIF
   ENDIF





   

   
   

   

   
   

   






































   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_D3_3_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_tracer, &
  tracer, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_D3_5_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_tracer, &
  tracer, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

   ELSE 
      WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
      CALL wrf_error_fatal3("<stdin>",4660,&
TRIM(wrf_err_message))
   ENDIF






CALL PERIOD_BDY_EM_D3_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_MOIST_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_CHEM_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_TRACER_sub ( grid, &
  config_flags, &
  num_tracer, &
  tracer, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_SCALAR_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )





   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   tile_bc_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call set_phys_bc_dry_2' )

     CALL set_phys_bc_dry_2( config_flags,                           &
                             grid%u_1, grid%u_2, grid%v_1, grid%v_2, grid%w_1, grid%w_2,           &
                             grid%t_1, grid%t_2, grid%ph_1, grid%ph_2, grid%mu_1, grid%mu_2,       &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

     CALL set_physical_bc3d( grid%tke_1, 'p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )

     CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

       CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

       CALL set_physical_bc3d( chem(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO chem_species_bdy_loop_2

     tracer_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_tracer

       CALL set_physical_bc3d( tracer(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO tracer_species_bdy_loop_2

     scalar_species_bdy_loop_2 : DO is = PARAM_FIRST_SCALAR , num_3d_s

       CALL set_physical_bc3d( scalar(ims,kms,jms,is) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO scalar_species_bdy_loop_2

   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO


   IF( config_flags%specified .or. config_flags%nested ) THEN 
     grid%dtbc = grid%dtbc + grid%dt
   ENDIF









CALL HALO_EM_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_BDY_EM_E_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


   CALL wrf_debug ( 10 , ' call set_w_surface' )
   fill_w_flag = .false.

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
      CALL set_w_surface( config_flags, grid%znw, fill_w_flag,              &
                           grid%w_2, grid%ht,  grid%u_2, grid%v_2,          &
                           grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy,&
                           grid%msftx, grid%msfty,                          &
                           ids, ide, jds, jde, kds, kde,                    &
                           ims, ime, jms, jme, kms, kme,                    &
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                   )


   END DO
   !$OMP END PARALLEL DO






  CALL after_all_rk_steps ( grid, config_flags,                  &
                            moist, chem, tracer, scalar,         &
                            th_phy, pi_phy, p_phy, rho_phy,      &   
                            p8w, t8w, dz8w,                      &
                            curr_secs2,                          &
                            diag_flag,                           &
                            ids,  ide,  jds,  jde,  kds,  kde,   &
                            ims,  ime,  jms,  jme,  kms,  kme,   &
                            ips,  ipe,  jps,  jpe,  kps,  kpe,   &
                            imsx, imex, jmsx, jmex, kmsx, kmex,  &
                            ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                            imsy, imey, jmsy, jmey, kmsy, kmey,  &
                            ipsy, ipey, jpsy, jpey, kpsy, kpey   )






   CALL wrf_debug ( 200 , ' call HALO_RK_E' )
   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_E_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_E_5_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

   ELSE
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
     CALL wrf_error_fatal3("<stdin>",4939,&
TRIM(wrf_err_message))
   ENDIF

   IF ( num_moist >= PARAM_FIRST_SCALAR  ) THEN



     CALL wrf_debug ( 200 , ' call HALO_RK_MOIST' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_MOIST_E_3_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_MOIST_E_5_sub ( grid, &
  num_moist, &
  moist, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3("<stdin>",4982,&
TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_chem >= PARAM_FIRST_SCALAR ) THEN



     CALL wrf_debug ( 200 , ' call HALO_RK_CHEM' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_CHEM_E_3_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_CHEM_E_5_sub ( grid, &
  num_chem, &
  chem, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3("<stdin>",5025,&
TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN



     CALL wrf_debug ( 200 , ' call HALO_RK_TRACER' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_TRACER_E_3_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_TRACER_E_5_sub ( grid, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3("<stdin>",5068,&
TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_scalar >= PARAM_FIRST_SCALAR ) THEN



     CALL wrf_debug ( 200 , ' call HALO_RK_SCALAR' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN






CALL HALO_EM_SCALAR_E_3_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN






CALL HALO_EM_SCALAR_E_5_sub ( grid, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal3("<stdin>",5111,&
TRIM(wrf_err_message))
     ENDIF
   ENDIF



   DEALLOCATE(max_vert_cfl_tmp)
   DEALLOCATE(max_horiz_cfl_tmp)

   CALL wrf_debug ( 200 , ' call end of solve_em' )



   IF ( coupler_on )   grid%just_read_auxinput4 = Is_alarm_tstep(grid%domain_clock, grid%alarms(AUXINPUT4_ALARM))



























































   RETURN

END SUBROUTINE solve_em
