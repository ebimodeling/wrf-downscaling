







MODULE module_initialize_real

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
   USE module_state_description
   USE module_timing
   USE module_soil_pre
   USE module_date_time
   USE module_llxy
   USE module_polarfft

   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   &
                          ,HALO_EM_INIT_6_sub   &
                          ,HALO_EM_VINTERP_UV_1_sub


   REAL , SAVE :: p_top_save
   INTEGER :: internal_time_loop

CONTAINS



   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      


      TYPE (domain)          :: grid

      

      INTEGER :: idum1, idum2

      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

        CALL init_domain_rk( grid &







,grid%moist,grid%moist_bxs,grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys, &
grid%moist_btye,grid%dfi_moist,grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs, &
grid%dfi_moist_btxe,grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys, &
grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs, &
grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys, &
grid%dfi_scalar_btye,grid%aerod,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t, &
grid%nba_mij,grid%nba_rij,grid%chem,grid%tracer,grid%tracer_bxs,grid%tracer_bxe,grid%tracer_bys,grid%tracer_bye, &
grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye &


      )
   END SUBROUTINE init_domain



   SUBROUTINE init_domain_rk ( grid &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


   )

      USE module_optional_input
      IMPLICIT NONE

      


      TYPE (domain)          :: grid







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


      TYPE (grid_config_rec_type)              :: config_flags

      

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      INTEGER :: loop , num_seaice_changes

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte, &
                 ips, ipe, jps, jpe, kps, kpe, &
                 i, j, k

      INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex,    &
                 ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                 imsy, imey, jmsy, jmey, kmsy, kmey,    &
                 ipsy, ipey, jpsy, jpey, kpsy, kpey

      INTEGER :: ns

      

      INTEGER :: error
      INTEGER :: im, num_3d_m, num_3d_s
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , qtot, pd_surf
      REAL    :: p00 , t00 , a , tiso
      REAL    :: hold_znw , ptemp
      REAL    :: vap_pres_mb , sat_vap_pres_mb
      LOGICAL :: were_bad

      LOGICAL :: stretch_grid, dry_sounding, debug
      INTEGER :: IICOUNT, icount

      REAL :: p_top_requested , temp
      INTEGER :: num_metgrid_levels
      REAL , DIMENSION(max_eta) :: eta_levels
      REAL :: max_dz

      REAL :: dclat




integer::oops1,oops2

      REAL    :: zap_close_levels
      INTEGER :: force_sfc_in_vinterp
      INTEGER :: interp_type , lagrange_order , extrap_type , t_extrap_type
      LOGICAL :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      LOGICAL :: we_have_tavgsfc , we_have_tsk

      INTEGER :: lev500 , loop_count
      REAL    :: zl , zu , pl , pu , z500 , dz500 , tvsfc , dpmu
      REAL    :: pfu, pfd, phm

      LOGICAL , PARAMETER :: want_full_levels = .TRUE.
      LOGICAL , PARAMETER :: want_half_levels = .FALSE.

      CHARACTER (LEN=256) :: a_message, mminlu
      REAL :: max_mf
    
      

      LOGICAL :: any_valid_points
      INTEGER :: i_valid , j_valid
      
      

      INTEGER :: k_max_p , k_min_p


        REAL , DIMENSION(100) :: lqmi

      REAL :: t_start , t_end
      REAL , ALLOCATABLE , DIMENSION(:,:) :: clat_glob

      

      CALL cpu_time(t_start)
      CALL get_ijk_from_grid (  grid ,                   &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe,    &
                                imsx, imex, jmsx, jmex, kmsx, kmex,    &
                                ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                                imsy, imey, jmsy, jmey, kmsy, kmey,    &
                                ipsy, ipey, jpsy, jpey, kpsy, kpey )
      its = ips ; ite = ipe ; jts = jps ; jte = jpe ; kts = kps ; kte = kpe


      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      

      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL nl_get_islake  ( grid%id , grid%islake )

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( grid%lu_index(i,j) .NE. grid%islake ) THEN
               grid%lakemask(i,j) = 0
            ELSE
               grid%lakemask(i,j) = 1
            END IF
         END DO
      END DO

      IF ( grid%sf_lake_physics .EQ. 1 ) THEN
         grid%lake_depth_flag = flag_lake_depth
         IF ( flag_lake_depth .EQ. 0 ) THEN
            CALL wrf_message ( " Warning: Please rerun WPS to get lake_depth information for lake model" )

         

         ELSE
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF      ( ( grid%lu_index(i,j) .NE. grid%islake ) .AND. ( grid%lu_index(i,j) .NE. grid%iswater ) ) THEN
                     grid%lake_depth(i,j) = -1
                  ELSE IF   ( grid%lu_index(i,j) .NE. grid%islake ) THEN
                     grid%lake_depth(i,j) = -2
                  END IF
               END DO
            END DO
         END IF
      END IF

      

      IF ( ( internal_time_loop .EQ. 1 ) .AND. ( grid%id .EQ. 1 ) .AND. &
           ( .NOT. config_flags%map_proj .EQ. PROJ_CASSINI ) ) THEN
         max_mf = grid%msft(its,jts)
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               max_mf = MAX ( max_mf , grid%msft(i,j) )
            END DO
         END DO
         max_mf = wrf_dm_max_real ( max_mf )
         WRITE ( a_message , FMT='(A,F5.2,A)' ) 'Max map factor in domain 1 = ',max_mf, &
                                                '. Scale the dt in the model accordingly.'
         CALL wrf_message ( a_message ) 
      END IF

      
      

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      

      grid%step_number = 0
      grid%itimestep=0

      

      grid%real_data_init_type = model_config_rec%real_data_init_type

      
      
      
   
      CALL const_module_initialize ( p00 , t00 , a , tiso ) 

      

      grid%t00  = t00
      grid%p00  = p00
      grid%tlp  = a
      grid%tiso = tiso

      
      
      
      
      
      
      
      
      
      

      
      
      

      hold_ups = ( internal_time_loop .EQ. 1 ) .OR. &
                 ( config_flags%grid_fdda .NE. 0 ) .OR. &
                 ( config_flags%sst_update .EQ. 1 ) .OR. &
                 ( config_flags%all_ic_times ) .OR. &
                 ( config_flags%map_proj .EQ. PROJ_CASSINI )

      
      

      IF      ( flag_excluded_middle .NE. 0 ) THEN

         
         

         IF ( hold_ups ) THEN
            WRITE ( a_message,* ) 'None of the following are allowed to be TRUE : '
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) ' ( internal_time_loop .EQ. 1 )               ', ( internal_time_loop .EQ. 1 )
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) ' ( config_flags%grid_fdda .NE. 0 )           ', ( config_flags%grid_fdda .NE. 0 )
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) ' ( config_flags%sst_update .EQ. 1 )          ', ( config_flags%sst_update .EQ. 1 )
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) ' ( config_flags%all_ic_times )               ', ( config_flags%all_ic_times )
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) ' ( config_flags%smooth_cg_topo )             ', ( config_flags%smooth_cg_topo )
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) ' ( config_flags%map_proj .EQ. PROJ_CASSINI ) ', ( config_flags%map_proj .EQ. PROJ_CASSINI )
            CALL wrf_message ( a_message ) 

            WRITE ( a_message,* ) 'Problems, we cannot have excluded middle data from WPS'
            CALL wrf_error_fatal3("<stdin>",387,&
a_message )
         END IF

         
         

         IF ( config_flags%spec_bdy_width .GT. flag_excluded_middle ) THEN
            WRITE ( a_message,* ) 'The WRF &bdy_control namelist.input spec_bdy_width = ', config_flags%spec_bdy_width
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) 'The WPS &metgrid namelist.wps process_only_bdy width = ',flag_excluded_middle
            CALL wrf_message ( a_message ) 
            WRITE ( a_message,* ) 'WPS process_only_bdy must be >= WRF spec_bdy_width'
            CALL wrf_error_fatal3("<stdin>",400,&
a_message )
         END IF
      END IF
      em_width = config_flags%spec_bdy_width

      
      

      any_valid_points = .false.
      find_valid : DO j = jts,jte
         DO i = its,ite
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            any_valid_points = .true.
            i_valid = i
            j_valid = j
            EXIT find_valid
         END DO 
      END DO find_valid

      

      IF ( flag_icefrac .EQ. 1 ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%xice(i,j) = grid%icefrac_gc(i,j)
            END DO
         END DO
      END IF

      
      

      IF      ( ( flag_snow .EQ. 0 ) .AND. ( flag_snowh .EQ. 0 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%snow(i,j)  = 0.
               grid%snowh(i,j) = 0.
            END DO
         END DO

      ELSE IF ( ( flag_snow .EQ. 0 ) .AND. ( flag_snowh .EQ. 1 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)

               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%snow(i,j)  = grid%snowh(i,j) * 1000. / 5.
            END DO
         END DO

      ELSE IF ( ( flag_snow .EQ. 1 ) .AND. ( flag_snowh .EQ. 0 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)

               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%snowh(i,j) = grid%snow(i,j) / 1000. * 5.
            END DO
         END DO

      END IF

      
      

      IF      ( ( config_flags%polar ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         DO j=max(jds+1,jts),min(jde-1,jte)
            DO i=its,min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
         IF(jts == jds) THEN
            DO i=its,ite
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx(i,jts) = 0.
               grid%msfvx_inv(i,jts) = 0.
            END DO
         END IF
         IF(jte == jde) THEN
            DO i=its,ite
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx(i,jte) = 0.
               grid%msfvx_inv(i,jte) = 0.
            END DO
         END IF
      ELSE IF ( ( config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         DO j=jts,jte
            DO i=its,min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( .NOT. config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .NE. 1 ) ) THEN
         DO j=jts,jte
            DO i=its,ite
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx(i,j) = grid%msfv(i,j)
               grid%msfvy(i,j) = grid%msfv(i,j)
               grid%msfux(i,j) = grid%msfu(i,j)
               grid%msfuy(i,j) = grid%msfu(i,j)
               grid%msftx(i,j) = grid%msft(i,j)
               grid%msfty(i,j) = grid%msft(i,j)
            ENDDO
         ENDDO
         DO j=jts,min(jde,jte)
            DO i=its,min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( .NOT. config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         IF ( grid%msfvx(its,jts) .EQ. 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",514,&
'Maybe you do not have the new map factors, try re-running geogrid' )
         END IF
         DO j=jts,min(jde,jte)
            DO i=its,min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( flag_mf_xy .NE. 1 ) ) THEN
         CALL wrf_error_fatal3("<stdin>",524,&
'Neither SI data nor older metgrid data can initialize a global domain' )
      ENDIF

      

      IF ( flag_tavgsfc .EQ. 1 ) THEN
         we_have_tavgsfc = .TRUE.
      ELSE
         we_have_tavgsfc = .FALSE.
      END IF

      IF ( flag_tsk     .EQ. 1 ) THEN
         we_have_tsk     = .TRUE.
      ELSE
         we_have_tsk     = .FALSE.
      END IF
   
      IF ( config_flags%use_tavg_for_tsk ) THEN
         IF ( we_have_tsk .OR. we_have_tavgsfc ) THEN
           
         ELSE
            CALL wrf_error_fatal3("<stdin>",546,&
'We either need TSK or TAVGSFC, verify these fields are coming from WPS' )
         END IF
   
         
   
         IF ( we_have_tavgsfc ) THEN
            DO j=jts,min(jde,jte)
               DO i=its,min(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%tsk(i,j) = grid%tavgsfc(i,j)
               END DO
            END DO
         END IF
      END IF

      
      

      IF ( flag_metgrid .EQ. 1 ) THEN  

         num_metgrid_levels = grid%num_metgrid_levels

         IF ( config_flags%nest_interp_coord .EQ. 1 ) THEN

            
            
            
   
            DO j=jts,jte
               DO i=its,ite
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%max_p(i,j) = grid%p_gc(i,1,j) 
                  k_max_p = 1
                  IF      ( grid%p_gc(i,2,j) .GT. grid%max_p(i,j) ) THEN
                     grid%max_p(i,j) = grid%p_gc(i,2,j)
                     k_max_p = 2
                  ELSE IF ( grid%p_gc(i,num_metgrid_levels,j) .GT. grid%max_p(i,j) ) THEN
                     grid%max_p(i,j) = grid%p_gc(i,num_metgrid_levels,j)
                     k_max_p = num_metgrid_levels
                  END IF
                  grid%t_max_p(i,j) = grid%t_gc(i,k_max_p,j)
                  grid%ght_max_p(i,j) = grid%ght_gc(i,k_max_p,j)
   
                  grid%min_p(i,j) = grid%p_gc(i,num_metgrid_levels,j) 
                  k_min_p = num_metgrid_levels
                  IF      ( grid%p_gc(i,2,j) .LT. grid%min_p(i,j) ) THEN
                     grid%min_p(i,j) = grid%p_gc(i,2,j)
                     k_min_p = 2
                  END IF
                  grid%t_min_p(i,j) = grid%t_gc(i,k_min_p,j)
                  grid%ght_min_p(i,j) = grid%ght_gc(i,k_min_p,j)
               END DO
            END DO
         END IF

         
         
         
         
         
         
         
         
         

         IF ( flag_pinterp .EQ. 1 ) THEN

            WRITE ( a_message , * ) 'Data from P_INTERP program, filling k=1 level with artificial surface fields.'
            CALL wrf_message ( a_message )
            DO j=jts,jte
               DO i=its,ite
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%u_gc(i,1,j) = grid%u_gc(i,2,j)
                  grid%v_gc(i,1,j) = grid%v_gc(i,2,j)
                  grid%rh_gc(i,1,j) = grid%rh_gc(i,2,j)
                  grid%t_gc(i,1,j) = grid%t2(i,j)
                  grid%ght_gc(i,1,j) = grid%ht(i,j)
                  grid%p_gc(i,1,j) = grid%psfc(i,j)
               END DO
            END DO
            flag_psfc = 0

         END IF

         

         DO j = jts, MIN(jte,jde-1)
            DO i = its, MIN(ite,ide-1)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%tsk(i,j) = grid%tsk_gc(i,j)
               grid%tmn(i,j) = grid%tmn_gc(i,j)
               grid%xlat(i,j) = grid%xlat_gc(i,j)
               grid%xlong(i,j) = grid%xlong_gc(i,j)
               grid%ht(i,j) = grid%ht_gc(i,j)
            END DO
         END DO

         
         
         
         
         
         
         
         

         IF ( ( config_flags%smooth_cg_topo ) .AND. &
              ( internal_time_loop .EQ. 1 ) .AND. &
              ( grid%id .EQ. 1 ) .AND. &
              ( flag_soilhgt .EQ. 1) ) THEN
            CALL blend_terrain ( grid%toposoil  , grid%ht , &
                                 ids , ide , jds , jde , 1   , 1   , &
                                 ims , ime , jms , jme , 1   , 1   , &
                                 ips , ipe , jps , jpe , 1   , 1   )
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%ht_smooth(i,j) = grid%ht(i,j)
               END DO
            END DO

         ELSE IF ( ( config_flags%smooth_cg_topo ) .AND. &
              ( internal_time_loop .NE. 1 ) .AND. &
              ( grid%id .EQ. 1 ) .AND. &
              ( flag_soilhgt .EQ. 1) ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%ht(i,j) = grid%ht_smooth(i,j)
               END DO
            END DO
            
         END IF

         

         IF ( ( config_flags%polar ) .AND. ( grid%fft_filter_lat .GT. 90 ) ) THEN
            CALL wrf_error_fatal3("<stdin>",682,&
'If the polar boundary condition is used, then fft_filter_lat must be set in namelist.input' )
         END IF

         IF ( config_flags%map_proj .EQ. PROJ_CASSINI ) THEN
            dclat = 90./REAL(jde-jds) 
            DO j = jts, MIN(jte,jde-1)
              DO k = kts, kte
                 DO i = its, MIN(ite,ide-1)
                    grid%t_2(i,k,j) = 1.
                 END DO
              END DO
              DO i = its, MIN(ite,ide-1)
                 grid%t_2(i,1,j) = grid%ht(i,j)
                 grid%sr(i,j) = grid%ht(i,j)
              END DO
            END DO

     ALLOCATE( clat_glob(ids:ide,jds:jde) )

     CALL wrf_patch_to_global_real ( grid%clat, clat_glob, grid%domdesc, 'xy', 'xy', &
                                     ids, ide, jds, jde, 1, 1, &
                                     ims, ime, jms, jme, 1, 1, &
                                     its, ite, jts, jte, 1, 1 )

     CALL wrf_dm_bcast_real ( clat_glob , (ide-ids+1)*(jde-jds+1) )

     grid%clat_xxx(ipsx:ipex,jpsx:jpex) = clat_glob(ipsx:ipex,jpsx:jpex)

     DEALLOCATE( clat_glob )

         CALL pxft ( grid=grid                                              &
               ,lineno=625                                             &
               ,flag_uv            = 0                                      &
               ,flag_rurv          = 0                                      &
               ,flag_wph           = 0                                      &
               ,flag_ww            = 0                                      &
               ,flag_t             = 1                                      &
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
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 grid%ht(i,j) = grid%t_2(i,1,j)
                 grid%sr(i,j) = grid%sr(i,j) - grid%ht(i,j)
              END DO
            END DO

         END IF

         

         IF ( flag_psfc .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                 grid%psfc_gc(i,j) = grid%psfc(i,j)
                 grid%p_gc(i,1,j) = grid%psfc(i,j)
              END DO
            END DO
         END IF

         
         
         
         

         IF ( flag_soilhgt .EQ. 1) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)

                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     grid%ght_gc(i,1,j) = grid%toposoil(i,j)
                     grid%ht_gc(i,j)= grid%toposoil(i,j)

               END DO
           END DO
         END IF

         
         

         num_metgrid_levels = grid%num_metgrid_levels

         
         

         IF ( flag_ptheta .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     ptemp = grid%p_gc(i,k,j)
                     grid%p_gc(i,k,j) = grid%prho_gc(i,k,j)
                     grid%prho_gc(i,k,j) = ptemp
                  END DO
               END DO
            END DO

            
            
            
            

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%  p_gc(i,num_metgrid_levels,j) = ( grid%  p_gc(i,1,j) + grid%  p_gc(i,num_metgrid_levels-1,j) ) * 0.5
                  grid%  t_gc(i,num_metgrid_levels,j) = ( grid%  t_gc(i,1,j) + grid%  t_gc(i,num_metgrid_levels-1,j) ) * 0.5
                  grid% sh_gc(i,num_metgrid_levels,j) = ( grid% sh_gc(i,1,j) + grid% sh_gc(i,num_metgrid_levels-1,j) ) * 0.5
                  grid%ght_gc(i,num_metgrid_levels,j) = ( grid%ght_gc(i,1,j) + grid%ght_gc(i,num_metgrid_levels-1,j) ) * 0.5
               END DO
            END DO
         END IF

         IF ( any_valid_points ) THEN
         

         IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
            k = 2
         ELSE
            k = num_metgrid_levels
         END IF

         IF ( grid%t_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%t_gc(i,1,j) = grid%t_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface temp, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         IF ( grid%rh_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%rh_gc(i,1,j) = grid%rh_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface RH, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         IF ( grid%u_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, ite
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%u_gc(i,1,j) = grid%u_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface u wind, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         IF ( grid%v_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, jte
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%v_gc(i,1,j) = grid%v_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface v wind, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message ) 
         END IF

         

         IF ( ( flag_qv .NE. 1 ) .AND. ( flag_sh .NE. 1 ) ) THEN
            IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF

            IF      ( config_flags%rh2qv_method .eq. 1 ) THEN
               CALL rh_to_mxrat1(grid%rh_gc, grid%t_gc, grid%p_gc, grid%qv_gc ,         &
                                 config_flags%rh2qv_wrt_liquid ,                        &
                                 config_flags%qv_max_p_safe ,                           &
                                 config_flags%qv_max_flag , config_flags%qv_max_value , &
                                 config_flags%qv_min_p_safe ,                           &
                                 config_flags%qv_min_flag , config_flags%qv_min_value , &
                                 ids , ide , jds , jde , 1   , num_metgrid_levels ,     &
                                 ims , ime , jms , jme , 1   , num_metgrid_levels ,     &
                                 its , ite , jts , jte , 1   , num_metgrid_levels )
            ELSE IF ( config_flags%rh2qv_method .eq. 2 ) THEN
               CALL rh_to_mxrat2(grid%rh_gc, grid%t_gc, grid%p_gc, grid%qv_gc ,         &
                                 config_flags%rh2qv_wrt_liquid ,                        &
                                 config_flags%qv_max_p_safe ,                           &
                                 config_flags%qv_max_flag , config_flags%qv_max_value , &
                                 config_flags%qv_min_p_safe ,                           &
                                 config_flags%qv_min_flag , config_flags%qv_min_value , &
                                 ids , ide , jds , jde , 1   , num_metgrid_levels ,     &
                                 ims , ime , jms , jme , 1   , num_metgrid_levels ,     &
                                 its , ite , jts , jte , 1   , num_metgrid_levels )
            END IF


         ELSE IF ( flag_sh .EQ. 1 ) THEN
            IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF
            IF ( grid%sh_gc(i_valid,kts,j_valid) .LT. 1.e-6 ) THEN
               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     grid%sh_gc(i,1,j) = grid%sh_gc(i,k,j)
                  END DO
               END DO
            END IF

            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     grid%qv_gc(i,k,j) = grid%sh_gc(i,k,j) /( 1. - grid%sh_gc(i,k,j) )
                     sat_vap_pres_mb = 0.6112*10.*EXP(17.67*(grid%t_gc(i,k,j)-273.15)/(grid%t_gc(i,k,j)-29.65))
                     vap_pres_mb = grid%qv_gc(i,k,j) * grid%p_gc(i,k,j)/100. / (grid%qv_gc(i,k,j) + 0.622 )
                     IF ( sat_vap_pres_mb .GT. 0 ) THEN
                        grid%rh_gc(i,k,j) = ( vap_pres_mb / sat_vap_pres_mb ) * 100.
                     ELSE
                        grid%rh_gc(i,k,j) = 0.
                     END IF
                  END DO
               END DO
            END DO

         END IF

         

         IF ( grid%ght_gc(i_valid,grid%num_metgrid_levels/2,j_valid) .LT. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = kts+1 , grid%num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     grid%ght_gc(i,k,j) = grid%ght_gc(i,k-1,j) - &
                        R_d / g * 0.5 * ( grid%t_gc(i,k  ,j) * ( 1 + 0.608 * grid%qv_gc(i,k  ,j) ) +   &
                                          grid%t_gc(i,k-1,j) * ( 1 + 0.608 * grid%qv_gc(i,k-1,j) ) ) * &
                        LOG ( grid%p_gc(i,k,j) / grid%p_gc(i,k-1,j) )
                  END DO
               END DO
            END DO
         END IF

         
         

         IF ( grid%p_gc(i_valid,num_metgrid_levels/2,j_valid) .LT. grid%p_gc(i_valid,num_metgrid_levels/2+1,j_valid) ) THEN
            config_flags%sfcp_to_sfcp = .TRUE.
         END IF
         END IF

         
         
         
         

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%u10(i,j)=grid%u_gc(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%v10(i,j)=grid%v_gc(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%t2(i,j)=grid%t_gc(i,1,j)
            END DO
         END DO

         IF ( flag_qv .EQ. 1 ) THEN
            DO j = jts, min(jde-1,jte)
               DO i = its, min(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%q2(i,j)=grid%qv_gc(i,1,j)
               END DO
            END DO
         END IF

         

         p_top_requested = grid%p_top_requested

         
         
         
         
         

         IF ( internal_time_loop .EQ. 1 ) THEN
            CALL find_p_top ( grid%p_gc , grid%p_top , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

            grid%p_top = wrf_dm_max_real ( grid%p_top )

            

            IF ( p_top_requested .LT. grid%p_top ) THEN
               print *,'p_top_requested = ',p_top_requested
               print *,'allowable grid%p_top in data   = ',grid%p_top
               CALL wrf_error_fatal3("<stdin>",1017,&
'p_top_requested < grid%p_top possible from data' )
            END IF

            
            
            

            grid%p_top = p_top_requested

            
            
            

            p_top_save = grid%p_top

         ELSE
            CALL find_p_top ( grid%p_gc , grid%p_top , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

            grid%p_top = wrf_dm_max_real ( grid%p_top )
            IF ( grid%p_top .GT. p_top_save ) THEN
               print *,'grid%p_top from last time period = ',p_top_save
               print *,'grid%p_top from this time period = ',grid%p_top
               CALL wrf_error_fatal3("<stdin>",1043,&
'grid%p_top > previous value' )
            END IF
            grid%p_top = p_top_save
         ENDIF

         
         

         CALL monthly_interp_to_date ( grid%greenfrac , current_date , grid%vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( grid%albedo12m , current_date , grid%albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         IF ( config_flags%rdlai2d ) THEN
         CALL monthly_interp_to_date ( grid%lai12m , current_date , grid%lai , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )
         ENDIF

         

         CALL monthly_min_max ( grid%greenfrac , grid%shdmin , grid%shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              grid%vegfra(i,j) = grid%vegfra(i,j) * 100.
              grid%shdmax(i,j) = grid%shdmax(i,j) * 100.
              grid%shdmin(i,j) = grid%shdmin(i,j) * 100.
           END DO
         END DO

         
         

         DO j = jts, MIN(jte,jde-1)
            DO i = its, MIN(ite,ide-1)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%albbck(i,j) = grid%albbck(i,j) / 100.
               grid%snoalb(i,j) = grid%snoalb(i,j) / 100.
               IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                  grid%albbck(i,j) = 0.08
                  grid%snoalb(i,j) = 0.08
               END IF
            END DO
         END DO

         
         
         

         IF (config_flags%mp_physics.eq.THOMPSONAERO .and. P_QNWFA.gt.1 .and. config_flags%use_aero_icbc) then
         CALL wrf_debug ( 0 , 'Using Thompson_aerosol_aware so using QNWFA monthly climo arrays to create QNWFA_now')
         DO k = 1, num_metgrid_levels
         WRITE(a_message,*) '  transferring each K-level ', k, ' to qntemp, sample Jan data, ', grid%QNWFA_jan(10,k,10)
         CALL wrf_debug ( 0 , a_message)
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%qntemp(i, 1, j) = grid%QNWFA_jan(i,k,j)
                  grid%qntemp(i, 2, j) = grid%QNWFA_feb(i,k,j)
                  grid%qntemp(i, 3, j) = grid%QNWFA_mar(i,k,j)
                  grid%qntemp(i, 4, j) = grid%QNWFA_apr(i,k,j)
                  grid%qntemp(i, 5, j) = grid%QNWFA_may(i,k,j)
                  grid%qntemp(i, 6, j) = grid%QNWFA_jun(i,k,j)
                  grid%qntemp(i, 7, j) = grid%QNWFA_jul(i,k,j)
                  grid%qntemp(i, 8, j) = grid%QNWFA_aug(i,k,j)
                  grid%qntemp(i, 9, j) = grid%QNWFA_sep(i,k,j)
                  grid%qntemp(i,10, j) = grid%QNWFA_oct(i,k,j)
                  grid%qntemp(i,11, j) = grid%QNWFA_nov(i,k,j)
                  grid%qntemp(i,12, j) = grid%QNWFA_dec(i,k,j)
               ENDDO
            ENDDO
         if (k.eq.1) then
         write(a_message,*) ' DEBUG-GT qntemp(10,jan-feb,10) ', grid%qntemp(10, 1, 10),grid%qntemp(10, 2, 10)
         CALL wrf_debug ( 0 , a_message)
         endif
            CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                          ids , ide , jds , jde , kds , kde , &
                                          ims , ime , jms , jme , kms , kme , &
                                          its , ite , jts , jte , kts , kte )
         if (k.eq.1) then
         write(a_message,*) ' DEBUG-GT qntemp2(10,10) ', grid%qntemp2(10,10)
         CALL wrf_debug ( 0 , a_message)
         endif
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%QNWFA_now(i,k,j) = grid%qntemp2(i,j)
               ENDDO
            ENDDO
         ENDDO
         ENDIF

         IF (config_flags%mp_physics.eq.THOMPSONAERO .and. P_QNIFA.gt.1 .and. config_flags%use_aero_icbc) then
         DO k = 1, num_metgrid_levels
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%qntemp(i, 1, j) = grid%QNIFA_jan(i,k,j)
                  grid%qntemp(i, 2, j) = grid%QNIFA_feb(i,k,j)
                  grid%qntemp(i, 3, j) = grid%QNIFA_mar(i,k,j)
                  grid%qntemp(i, 4, j) = grid%QNIFA_apr(i,k,j)
                  grid%qntemp(i, 5, j) = grid%QNIFA_may(i,k,j)
                  grid%qntemp(i, 6, j) = grid%QNIFA_jun(i,k,j)
                  grid%qntemp(i, 7, j) = grid%QNIFA_jul(i,k,j)
                  grid%qntemp(i, 8, j) = grid%QNIFA_aug(i,k,j)
                  grid%qntemp(i, 9, j) = grid%QNIFA_sep(i,k,j)
                  grid%qntemp(i,10, j) = grid%QNIFA_oct(i,k,j)
                  grid%qntemp(i,11, j) = grid%QNIFA_nov(i,k,j)
                  grid%qntemp(i,12, j) = grid%QNIFA_dec(i,k,j)
               ENDDO
            ENDDO
            CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                          ids , ide , jds , jde , kds , kde , &
                                          ims , ime , jms , jme , kms , kme , &
                                          its , ite , jts , jte , kts , kte )
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%QNIFA_now(i,k,j) = grid%qntemp2(i,j)
               ENDDO
            ENDDO
         ENDDO
         ENDIF

         
         
         
         
         
         

         IF      ( ( flag_psfc    .EQ. 1 ) .AND. &
                   ( flag_soilhgt .EQ. 1 ) .AND. &
                   ( flag_slp     .EQ. 1 ) .AND. &
                   ( .NOT. config_flags%sfcp_to_sfcp ) ) THEN
            WRITE(a_message,FMT='(A)') 'Using sfcprs3 to compute psfc'
            CALL wrf_message ( a_message )
            CALL sfcprs3(grid%ght_gc, grid%p_gc, grid%ht, &
                         grid%pslv_gc, grid%psfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE IF ( ( flag_psfc    .EQ. 1 ) .AND. &
                   ( flag_soilhgt .EQ. 1 ) .AND. &
                   ( config_flags%sfcp_to_sfcp ) ) THEN
            WRITE(a_message,FMT='(A)') 'Using sfcprs2 to compute psfc'
            CALL wrf_message ( a_message )
            CALL sfcprs2(grid%t_gc, grid%qv_gc, grid%ght_gc, grid%psfc_gc, grid%ht, &
                         grid%tavgsfc, grid%p_gc, grid%psfc, we_have_tavgsfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE IF ( flag_slp     .EQ. 1 ) THEN
            WRITE(a_message,FMT='(A)') 'Using sfcprs  to compute psfc'
            CALL wrf_message ( a_message )
            CALL sfcprs (grid%t_gc, grid%qv_gc, grid%ght_gc, grid%pslv_gc, grid%ht, &
                         grid%tavgsfc, grid%p_gc, grid%psfc, we_have_tavgsfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE
            WRITE(a_message,FMT='(3(A,I2),A,L1)') 'ERROR in psfc: flag_psfc = ',flag_psfc, &
                                               ', flag_soilhgt = ',flag_soilhgt , &
                                               ', flag_slp = ',flag_slp , & 
                                               ', sfcp_to_sfcp = ',config_flags%sfcp_to_sfcp 
            CALL wrf_message ( a_message ) 
            CALL wrf_error_fatal3("<stdin>",1218,&
'not enough info for a p sfc computation' )
         END IF

         

         IF ( flag_psfc .NE. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                 grid%psfc_gc(i,j) = grid%psfc(i,j)
                 grid%p_gc(i,1,j) = grid%psfc(i,j)
              END DO
            END DO
         END IF

         

         CALL integ_moist ( grid%qv_gc , grid%p_gc , grid%pd_gc , grid%t_gc , grid%ght_gc , grid%intq_gc , &
                            ids , ide , jds , jde , 1   , num_metgrid_levels , &
                            ims , ime , jms , jme , 1   , num_metgrid_levels , &
                            its , ite , jts , jte , 1   , num_metgrid_levels )

         
         
         

         IF ( flag_ptheta .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = num_metgrid_levels-1 , 1 , -1
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     ptemp = ((grid%p_gc(i,k,j) - grid%pd_gc(i,k,j)) + (grid%p_gc(i,k+1,j) - grid%pd_gc(i,k+1,j)))/2
                     grid%pdrho_gc(i,k,j) = grid%prho_gc(i,k,j) - ptemp
                  END DO
               END DO
            END DO
         END IF


         
         

         CALL p_dts ( grid%mu0 , grid%intq_gc , grid%psfc , grid%p_top , &
                      ids , ide , jds , jde , 1   , num_metgrid_levels , &
                      ims , ime , jms , jme , 1   , num_metgrid_levels , &
                      its , ite , jts , jte , 1   , num_metgrid_levels )

         

         CALL p_dhs ( grid%pdhs , grid%ht , p00 , t00 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

         

         IF ( grid%znw(1) .NE. 1.0 ) THEN

            eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
            max_dz            = model_config_rec%max_dz

            CALL compute_eta ( grid%znw , &
                               eta_levels , max_eta , max_dz , &
                               grid%p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )
         END IF

         IF ( config_flags%interp_theta ) THEN

            

            CALL t_to_theta ( grid%t_gc , grid%p_gc , p00 , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )
         END IF

         IF ( flag_slp .EQ. 1 ) THEN

            
            
            
            

            CALL p_dry ( grid%mu0 , grid%znw , grid%p_top , grid%pb , want_full_levels , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )

            
            
            
            

            interp_type = 2
            lagrange_order = grid%lagrange_order
            lowest_lev_from_sfc = .FALSE.
            use_levels_below_ground = .TRUE.
            use_surface = .TRUE.
            zap_close_levels = grid%zap_close_levels
            force_sfc_in_vinterp = 0
            t_extrap_type = grid%t_extrap_type
            extrap_type = 1

            
            
            

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%psfc_gc(i,j) = grid%pd_gc(i,1,j)
                  grid%pd_gc(i,1,j) = grid%pslv_gc(i,j) - ( grid%p_gc(i,1,j) - grid%pd_gc(i,1,j) )
                  grid%ht_gc(i,j) = grid%ght_gc(i,1,j)
                  grid%ght_gc(i,1,j) = 0.
               END DO
            END DO

            CALL vert_interp ( grid%ght_gc , grid%pd_gc , grid%ph0 , grid%pb , &
                               num_metgrid_levels , 'Z' , &
                               interp_type , lagrange_order , extrap_type , &
                               lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                               zap_close_levels , force_sfc_in_vinterp , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )

            

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%pd_gc(i,1,j) = grid%psfc_gc(i,j)
                  grid%ght_gc(i,1,j) = grid%ht_gc(i,j)
               END DO
            END DO

         END IF

         

         CALL p_dry ( grid%mu0 , grid%znw , grid%p_top , grid%pb , want_half_levels , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

         interp_type = grid%interp_type
         lagrange_order = grid%lagrange_order
         lowest_lev_from_sfc = grid%lowest_lev_from_sfc
         use_levels_below_ground = grid%use_levels_below_ground
         use_surface = grid%use_surface
         zap_close_levels = grid%zap_close_levels
         force_sfc_in_vinterp = grid%force_sfc_in_vinterp
         t_extrap_type = grid%t_extrap_type
         extrap_type = grid%extrap_type

         
         

         CALL vert_interp ( grid%rh_gc , grid%pd_gc , grid%u_1 , grid%pb , &
                            num_metgrid_levels , 'Q' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

         
         
         
         

         interp_type = 2
         CALL vert_interp ( grid%t_gc , grid%pd_gc , grid%t_2               , grid%pb , &
                            num_metgrid_levels , 'T' , &
                            interp_type , lagrange_order , t_extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
         interp_type = grid%interp_type
     
         

         interp_type = 1
         CALL vert_interp ( grid%p_gc , grid%pd_gc , grid%p               , grid%pb , &
                            num_metgrid_levels , 'T' , &
                            interp_type , lagrange_order , t_extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
         interp_type = grid%interp_type

         
         
         
         

         grid%v_1 = grid%t_2

         IF ( config_flags%interp_theta ) THEN
            CALL theta_to_t ( grid%v_1 , grid%p  , p00 , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte )
         END IF

         IF      ( config_flags%rh2qv_method .eq. 1 ) THEN
            CALL rh_to_mxrat1(grid%u_1, grid%v_1, grid%p , moist(:,:,:,P_QV) ,       &
                              config_flags%rh2qv_wrt_liquid ,                        &
                              config_flags%qv_max_p_safe ,                           &
                              config_flags%qv_max_flag , config_flags%qv_max_value , &
                              config_flags%qv_min_p_safe ,                           &
                              config_flags%qv_min_flag , config_flags%qv_min_value , &
                              ids , ide , jds , jde , kds , kde ,                    &
                              ims , ime , jms , jme , kms , kme ,                    &
                              its , ite , jts , jte , kts , kte-1 )
         ELSE IF ( config_flags%rh2qv_method .eq. 2 ) THEN
            CALL rh_to_mxrat2(grid%u_1, grid%v_1, grid%p , moist(:,:,:,P_QV) ,       &
                              config_flags%rh2qv_wrt_liquid ,                        &
                              config_flags%qv_max_p_safe ,                           &
                              config_flags%qv_max_flag , config_flags%qv_max_value , &
                              config_flags%qv_min_p_safe ,                           &
                              config_flags%qv_min_flag , config_flags%qv_min_value , &
                              ids , ide , jds , jde , kds , kde ,                    &
                              ims , ime , jms , jme , kms , kme ,                    &
                              its , ite , jts , jte , kts , kte-1 )
         END IF

         IF ( .NOT. config_flags%interp_theta ) THEN
            CALL t_to_theta ( grid%t_2 , grid%p , p00 , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte-1 )
         END IF

         num_3d_m = num_moist
         num_3d_s = num_scalar

         IF ( flag_qr .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QR ) THEN
                  CALL vert_interp ( grid%qr_gc , grid%pd_gc , moist(:,:,:,P_QR) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qc .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QC ) THEN
                  CALL vert_interp ( grid%qc_gc , grid%pd_gc , moist(:,:,:,P_QC) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qi .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QI ) THEN
                  CALL vert_interp ( grid%qi_gc , grid%pd_gc , moist(:,:,:,P_QI) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qs .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QS ) THEN
                  CALL vert_interp ( grid%qs_gc , grid%pd_gc , moist(:,:,:,P_QS) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qg .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QG ) THEN
                  CALL vert_interp ( grid%qg_gc , grid%pd_gc , moist(:,:,:,P_QG) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qh .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QH ) THEN
                  CALL vert_interp ( grid%qh_gc , grid%pd_gc , moist(:,:,:,P_QH) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qni .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNI ) THEN
                  CALL vert_interp ( grid%qni_gc , grid%pd_gc , scalar(:,:,:,P_QNI) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
    
         IF ( flag_qnr .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNR ) THEN
                  CALL vert_interp ( grid%qnr_gc , grid%pd_gc , scalar(:,:,:,P_QNR) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF


         WRITE(a_message,*) ' flag value of flag_qnwfa is ', flag_qnwfa
         CALL wrf_debug ( 0 , a_message)
         IF ( flag_qnwfa .EQ. 1 .and. config_flags%use_aero_icbc) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNWFA ) THEN
                  CALL wrf_debug ( 0 , 'Using Thompson_aerosol_aware so vertically-interpolating QNWFA monthly climo arrays to fill scalar')
                  CALL vert_interp ( grid%QNWFA_now , grid%pd_gc , scalar(:,:,:,P_QNWFA) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         ELSEIF ( flag_qnwfa .EQ. 1 .and. (.NOT. config_flags%use_aero_icbc)) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNWFA ) THEN
                  DO j = jts, MIN(jte,jde-1)
                     DO k = kts, kte
                        DO i = its, MIN(ite,ide-1)
                           scalar(i,k,j,P_QNWFA) = 0.
                        END DO
                     END DO
                  END DO
               END IF
            END DO
         END IF


         IF ( flag_qnifa .EQ. 1 .and. config_flags%use_aero_icbc) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNIFA ) THEN
                  CALL vert_interp ( grid%QNIFA_now , grid%pd_gc , scalar(:,:,:,P_QNIFA) , grid%pb , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , lagrange_order , extrap_type , &
                                     lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         ELSEIF ( flag_qnifa .EQ. 1 .and. (.NOT. config_flags%use_aero_icbc)) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNIFA ) THEN
                  DO j = jts, MIN(jte,jde-1)
                     DO k = kts, kte
                        DO i = its, MIN(ite,ide-1)
                           scalar(i,k,j,P_QNIFA) = 0.
                        END DO
                     END DO
                  END DO
               END IF
            END DO
         END IF

         
         

         IF ( flag_ptheta .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                     grid%pd_gc(i,k,j) = grid%prho_gc(i,k,j)
                  END DO
               END DO
            END DO
         END IF

         ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte

         
         
         
         
         







CALL HALO_EM_VINTERP_UV_1_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


         CALL vert_interp ( grid%u_gc , grid%pd_gc , grid%u_2               , grid%pb , &
                            num_metgrid_levels , 'U' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

         CALL vert_interp ( grid%v_gc , grid%pd_gc , grid%v_2               , grid%pb , &
                            num_metgrid_levels , 'V' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      END IF     

      
      

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL nl_get_islake  ( grid%id , grid%islake )


      IF ( grid%islake < 0 ) THEN
         grid%lakeflag=0
         CALL wrf_debug ( 0 , 'Old data, no inland lake information')

            DO j=jts,MIN(jde-1,jte)
               DO i=its,MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( ( grid%landusef(i,grid%iswater,j) >= 0.5 ) .OR. ( grid%lu_index(i,j) == grid%iswater ) ) .AND. &
                       ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) ) THEN
                     IF ( we_have_tavgsfc ) THEN
                        grid%sst(i,j) = grid%tavgsfc(i,j)
                     END IF
                     IF ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) THEN
                        grid%sst(i,j) = grid%tsk(i,j)
                     END IF
                     IF ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) THEN
                        grid%sst(i,j) = grid%t2(i,j)
                     END IF
                  END IF
               END DO
            END DO
      ELSE
         grid%lakeflag=1
         IF ( we_have_tavgsfc ) THEN

            CALL wrf_debug ( 0 , 'Using inland lakes with average surface temperature')
            DO j=jts,MIN(jde-1,jte)
               DO i=its,MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( grid%landusef(i,grid%islake,j) >= 0.5 ) .OR. ( grid%lu_index(i,j) == grid%islake ) )  THEN
                     grid%sst(i,j) = grid%tavgsfc(i,j)
                     grid%tsk(i,j) = grid%tavgsfc(i,j)
                  END IF
                  IF ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) THEN
                     grid%sst(i,j) = grid%t2(i,j)
                  END IF
               END DO
            END DO

         ELSE     

            CALL wrf_debug ( 0 , 'No average surface temperature for use with inland lakes')

         END IF
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%landusef(i,grid%iswater,j) = grid%landusef(i,grid%iswater,j) + &
                                                 grid%landusef(i,grid%islake,j)
               grid%landusef(i,grid%islake,j) = 0.
            END DO
         END DO

      END IF

      
      

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            grid%tsk_save(i,j) = grid%tsk(i,j)
         END DO
      END DO

      
      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                 ( grid%sst(i,j) .GT. 170. ) .AND. ( grid%sst(i,j) .LT. 400. ) ) THEN
               grid%tsk(i,j) = grid%sst(i,j)
            ENDIF
         END DO
      END DO

      
      

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( grid%snow(i,j) .GE. 10. ) then
               grid%snowc(i,j) = 1.
            ELSE
               grid%snowc(i,j) = 0.0
            END IF
         END DO
      END DO

      

      grid%ifndsnowh = flag_snowh
      IF (num_sw_levels_input .GE. 1) THEN
         grid%ifndsoilw = 1
      ELSE
         grid%ifndsoilw = 0
      END IF

      

      IF ( config_flags%seaice_albedo_opt == 2 ) THEN
          grid%ifndalbsi = flag_albsi
      ELSE
          grid%ifndalbsi = 0
      ENDIF
          
      IF ( config_flags%seaice_snowdepth_opt == 1 ) THEN
          grid%ifndsnowsi = flag_snowsi
      ELSE
          grid%ifndsnowsi = 0
      ENDIF
          
      IF ( config_flags%seaice_thickness_opt == 1 ) THEN
          grid%ifndicedepth = flag_icedepth
      ELSE
          grid%ifndicedepth = 0
      ENDIF

      

      CALL nl_get_mminlu ( grid%id , mminlu )
      write(a_message,*) 'MMINLU = ',trim(mminlu)
      CALL wrf_debug ( 1 , a_message )
      write(a_message,*) 'sf_surface_physics = ',model_config_rec%sf_surface_physics(grid%id)
      CALL wrf_debug ( 1, a_message )

      probs_with_nlcd : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( RUCLSMSCHEME, NOAHMPSCHEME, CLMSCHEME, SSIBSCHEME )
            IF ( TRIM(mminlu) .EQ. 'NLCD40' ) THEN
               CALL wrf_message ( 'NLCD40 data may be used with SLABSCHEME, LSMSCHEME, PXLSMSCHEME' )
               CALL wrf_message ( 'Re-run geogrid and choose a different land cover source, or select a different sf_surface_physics option' )
               CALL wrf_error_fatal3("<stdin>",1842,&
'NLCD40 data may not be used with: RUCLSMSCHEME, NOAHMPSCHEME, CLMSCHEME, SSIBSCHEME' )
            END IF

         CASE ( SLABSCHEME, LSMSCHEME, PXLSMSCHEME )
               CALL wrf_debug ( 1, 'NLCD40 being used with an OK scheme' )

      END SELECT probs_with_nlcd

      

      enough_data : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( LSMSCHEME, NOAHMPSCHEME )
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal3("<stdin>",1857,&
'Not enough soil temperature data for Noah LSM scheme.')
            END IF

         CASE (RUCLSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal3("<stdin>",1863,&
'Not enough soil temperature data for RUC LSM scheme.')
            END IF

         CASE (PXLSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal3("<stdin>",1869,&
'Not enough soil temperature data for P-X LSM scheme.')
            END IF
         CASE (CLMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal3("<stdin>",1874,&
'Not enough soil temperature data for CLM LSM scheme.')
            END IF

         CASE (SSIBSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal3("<stdin>",1880,&
'Not enough soil temperature data for SSIB LSM scheme.')
            END IF


      END SELECT enough_data

      interpolate_soil_tmw : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME,LSMSCHEME,NOAHMPSCHEME,RUCLSMSCHEME,PXLSMSCHEME,CLMSCHEME,SSIBSCHEME )
            CALL process_soil_real ( grid%tsk , grid%tmn , grid%tavgsfc,  &
                                  grid%landmask , grid%sst , grid%ht, grid%toposoil, &
                                  st_input , sm_input , sw_input , &
                                  st_levels_input , sm_levels_input , sw_levels_input , &
                                  grid%zs , grid%dzs , grid%tslb , grid%smois , grid%sh2o , &
                                  flag_sst , flag_tavgsfc, flag_soilhgt,&
                                  flag_soil_layers, flag_soil_levels, &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  model_config_rec%sf_surface_physics(grid%id) , &
                                  model_config_rec%num_soil_layers , &
                                  model_config_rec%real_data_init_type , &
                                  num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                  num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      END SELECT interpolate_soil_tmw

      
      
      

      IF ( any_valid_points ) THEN
      IF ( config_flags%surface_input_source .EQ. 1 ) THEN

      

         IF ( ( config_flags%sf_urban_physics == 1 ) .OR. ( config_flags%sf_urban_physics == 2 ) .OR. ( config_flags%sf_urban_physics == 3 ) ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%LP_URB2D(i,j)  = grid%URB_PARAM(i,91,j)
                  grid%LB_URB2D(i,j)  = grid%URB_PARAM(i,95,j)
                  grid%HGT_URB2D(i,j)  = grid%URB_PARAM(i,94,j)
               END DO
            END DO
         ENDIF

         IF ( ( config_flags%sf_urban_physics == 2 ) .OR. ( config_flags%sf_urban_physics == 3 ) ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  DO k = 1, 15
                     grid%HI_URB2D(i,k,j)  = grid%URB_PARAM(i,k+117,j)
                  END DO
               END DO
            END DO
         ENDIF

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( config_flags%sf_urban_physics==1 ) THEN
                  grid%MH_URB2D(i,j)  = grid%URB_PARAM(i,92,j)
                  grid%STDH_URB2D(i,j)  = grid%URB_PARAM(i,93,j)
               ENDIF
               grid%H2W_URB2D(i,j)  = grid%URB_PARAM(i,101,j)
               grid%Z0S_URB2D(i,j)  = grid%URB_PARAM(i,103,j)
               grid%ZDS_URB2D(i,j)  = grid%URB_PARAM(i,104,j)
               grid%ZDM_URB2D(i,j)  = grid%URB_PARAM(i,117,j)
               IF(grid%URB_PARAM(i,100,j).eq.0)THEN
                  grid%CAR_URB2D(i,j) = 1.0
               ELSE
                  grid%CAR_URB2D(i,j) = grid%URB_PARAM(i,100,j)
               END IF
               IF(grid%URB_PARAM(i,102,j).eq.0)THEN
                  grid%SVF_URB2D(i,j) = 1.0
               ELSE
                  grid%SVF_URB2D(i,j) = grid%URB_PARAM(i,102,j)
               END IF
            END DO
         END DO

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               DO k = 1, 15
                  grid%FAD0_URB2D(i,k,j)  = grid%URB_PARAM(i,k,j)
                  grid%FAD135_URB2D(i,k,j)  = grid%URB_PARAM(i,k+15,j)
                  grid%FAD45_URB2D(i,k,j)  = grid%URB_PARAM(i,k+30,j)
                  grid%FAD90_URB2D(i,k,j)  = grid%URB_PARAM(i,k+45,j)
                  grid%PAD_URB2D(i,k,j)  = grid%URB_PARAM(i,k+60,j)
                  grid%RAD_URB2D(i,k,j)  = grid%URB_PARAM(i,k+75,j)
               END DO
            END DO
         END DO

         DO  j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               DO k = 1, 4
                  IF ( config_flags%sf_urban_physics==1 ) THEN
                     grid%LF_URB2D(i,k,j)  = grid%URB_PARAM(i,k+95,j)
                  ENDIF
                  grid%Z0M_URB2D(i,k,j)  = grid%URB_PARAM(i,k+112,j)
               END DO
            END DO
         END DO

         DO  j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%Z0R_URB2D(i,1,j)  = grid%URB_PARAM(i,105,j)
               grid%Z0R_URB2D(i,2,j)  = grid%URB_PARAM(i,107,j)
               grid%Z0R_URB2D(i,3,j)  = grid%URB_PARAM(i,109,j)
               grid%Z0R_URB2D(i,4,j)  = grid%URB_PARAM(i,111,j)
               grid%ZDR_URB2D(i,1,j)  = grid%URB_PARAM(i,106,j)
               grid%ZDR_URB2D(i,2,j)  = grid%URB_PARAM(i,108,j)
               grid%ZDR_URB2D(i,3,j)  = grid%URB_PARAM(i,110,j)
               grid%ZDR_URB2D(i,4,j)  = grid%URB_PARAM(i,112,j)
            END DO
         END DO

      
      

         grid%vegcat (its,jts) = 0
         grid%soilcat(its,jts) = 0

         num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
         num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )

         CALL process_percent_cat_new ( grid%landmask , &
                                    grid%landusef , grid%soilctop , grid%soilcbot , &
                                    grid%isltyp , grid%ivgtyp , &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte , &
                                    model_config_rec%iswater(grid%id) )

         


         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%vegcat(i,j)  = grid%ivgtyp(i,j)
               grid%soilcat(i,j) = grid%isltyp(i,j)
            END DO
         END DO

      ELSE IF ( config_flags%surface_input_source .EQ. 2 ) THEN

         

         IF ( grid%soilcat(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%isltyp(i,j) = NINT( grid%soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( grid%vegcat(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%ivgtyp(i,j) = NINT( grid%vegcat(i,j) )
               END DO
            END DO
         END IF

      ELSE IF ( config_flags%surface_input_source .EQ. 3 ) THEN

         

         IF ( grid%sct_dom_gc(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%isltyp(i,j) = NINT( grid%sct_dom_gc(i,j) )
                  grid%soilcat(i,j) = grid%isltyp(i,j)
               END DO
            END DO
         ELSE
            WRITE ( a_message , * ) 'You have set surface_input_source = 3,'// &
                                    ' but your geogrid data does not have valid dominant soil data.'
            CALL wrf_error_fatal3("<stdin>",2069,&
a_message ) 
         END IF
         IF ( grid%lu_index(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  grid%ivgtyp(i,j) = NINT( grid%lu_index(i,j) )
                  grid%vegcat(i,j) = grid%ivgtyp(i,j)
               END DO
            END DO
         ELSE
            WRITE ( a_message , * ) 'You have set surface_input_source = 3,'//&
                                    ' but your geogrid data does not have valid dominant land use data.'
            CALL wrf_error_fatal3("<stdin>",2083,&
a_message ) 
         END IF

      END IF
      END IF

      
      


      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold )
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_pre ( grid%xice , grid%landmask , grid%tsk , grid%ivgtyp , grid%vegcat , grid%lu_index , &
                                   grid%xland , grid%landusef , grid%isltyp , grid%soilcat , grid%soilctop , &
                                   grid%soilcbot , grid%tmn , &
                                   grid%seaice_threshold , &
                                   config_flags%fractional_seaice, &
                                   num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                   grid%iswater , grid%isice , &
                                   model_config_rec%sf_surface_physics(grid%id) , &
                                   ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte )

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            grid%lu_index(i,j) = grid%ivgtyp(i,j)
            IF ( grid%lu_index(i,j) .NE. model_config_rec%iswater(grid%id) ) THEN
               grid%landmask(i,j) = 1
               grid%xland(i,j)    = 1
            ELSE
               grid%landmask(i,j) = 0
               grid%xland(i,j)    = 2
            END IF
         END DO
      END DO


      

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME , LSMSCHEME , NOAHMPSCHEME , RUCLSMSCHEME, PXLSMSCHEME,CLMSCHEME, SSIBSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                       ( grid%sst(i,j) .GT. 170. ) .AND. ( grid%sst(i,j) .LT. 400. ) ) THEN
                     grid%tmn(i,j) = grid%sst(i,j)
                     grid%tsk(i,j) = grid%sst(i,j)
                  ELSE IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                     grid%tmn(i,j) = grid%tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      

      IF ( internal_time_loop .NE. 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( grid%tsk(i,j) .LT. 170 .or. grid%tsk(i,j) .GT. 400. ) THEN
                  grid%tsk(i,j) = grid%t_2(i,1,j)
               END IF
            END DO
         END DO
      ELSE
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( grid%tsk(i,j) .LT. 170 .or. grid%tsk(i,j) .GT. 400. ) THEN
                  print *,'error in the grid%tsk'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
                  if(grid%tmn(i,j).gt.170. .and. grid%tmn(i,j).lt.400.)then
                     grid%tsk(i,j)=grid%tmn(i,j)
                  else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                     grid%tsk(i,j)=grid%sst(i,j)
                  else
                     CALL wrf_error_fatal3("<stdin>",2173,&
'grid%tsk unreasonable' )
                  end if
               END IF
            END DO
         END DO
      END IF

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( ( ( grid%tmn(i,j) .LT. 170. ) .OR. ( grid%tmn(i,j) .GT. 400. ) ) &
               .AND. ( grid%landmask(i,j) .GT. 0.5 ) ) THEN
               IF ( ( model_config_rec%sf_surface_physics(grid%id) .NE. LSMSCHEME ) .and. &
                    ( model_config_rec%sf_surface_physics(grid%id) .NE. NOAHMPSCHEME ) ) THEN
                  print *,'error in the grid%tmn'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
               END IF

               if(grid%tsk(i,j).gt.170. .and. grid%tsk(i,j).lt.400.)then
                  grid%tmn(i,j)=grid%tsk(i,j)
               else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                  grid%tmn(i,j)=grid%sst(i,j)
               else
                  CALL wrf_error_fatal3("<stdin>",2201,&
'grid%tmn unreasonable' )
               endif
            END IF
         END DO
      END DO
   

      
      
      
      

      lqmi(1:num_soil_top_cat) = &
      (/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
        0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
        0.004, 0.065 /)


      
      

      IF ( domain_ClockIsStartTime(grid) ) THEN
         account_for_zero_soil_moisture : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

            CASE ( LSMSCHEME , NOAHMPSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'Noah -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1

                           grid%smois(i,:,j) = 0.005













                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'Noah -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) , 0.005 )

                     END DO
                  END DO
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'RUC -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1



                           grid%smois(i,:,j) = 0.499
                        ELSEIF ( (grid%landmask(i,j).gt.0.5) .and. (grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. (grid%smois(i,1,j) .gt. 1.005 ) ) then
                           print *,'Noah -> Noah: bad soil moisture at i,j =',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.499

                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'RUC -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               END IF


               
               
















            CASE ( RUCLSMSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j)  , 0.005 )

                     END DO
                  END DO
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  
               END IF

             CASE ( PXLSMSCHEME )
               iicount = 0
               IF ( flag_soil_layers == 1 ) THEN
                  
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) , 0.005 )

                     END DO
                  END DO
               END IF
            CASE ( CLMSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'CLM -> CLM: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'CLM -> CLM: total number of small soil moisture locations = ',iicount
                  END IF
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) + lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'CLM -> CLM: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'CLM -> CLM: total number of small soil moisture locations = ',iicount
                  END IF
               END IF

         END SELECT account_for_zero_soil_moisture
      END IF

      

      IF ( internal_time_loop .NE. 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO ns = 1 , model_config_rec%num_soil_layers
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  IF ( grid%tslb(i,ns,j) .LT. 170 .or. grid%tslb(i,ns,j) .GT. 400. ) THEN
                     grid%tslb(i,ns,j) = grid%t_2(i,1,j)
                     grid%smois(i,ns,j) = 0.3
                  END IF
               END DO
            END DO
         END DO
      ELSE
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( ( ( grid%tslb(i,1,j) .LT. 170. ) .OR. ( grid%tslb(i,1,j) .GT. 400. ) ) .AND. &
                       ( grid%landmask(i,j) .GT. 0.5 ) ) THEN
                     IF ( ( model_config_rec%sf_surface_physics(grid%id) .NE. LSMSCHEME    ) .AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. NOAHMPSCHEME ) .AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. RUCLSMSCHEME ).AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. SSIBSCHEME ).AND. & 
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. CLMSCHEME ).AND. & 
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. PXLSMSCHEME ) ) THEN
                        print *,'error in the grid%tslb'
                        print *,'i,j=',i,j
                        print *,'grid%landmask=',grid%landmask(i,j)
                        print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
                        print *,'grid%tslb = ',grid%tslb(i,:,j)
                        print *,'old grid%smois = ',grid%smois(i,:,j)
                        grid%smois(i,1,j) = 0.3
                        grid%smois(i,2,j) = 0.3
                        grid%smois(i,3,j) = 0.3
                        grid%smois(i,4,j) = 0.3
                     END IF

                     IF ( (grid%tsk(i,j).GT.170. .AND. grid%tsk(i,j).LT.400.) .AND. &
                          (grid%tmn(i,j).GT.170. .AND. grid%tmn(i,j).LT.400.) ) THEN
                        fake_soil_temp : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )
                           CASE ( SLABSCHEME )
                              DO ns = 1 , model_config_rec%num_soil_layers
                                 grid%tslb(i,ns,j) = ( grid%tsk(i,j)*(3.0 - grid%zs(ns)) + &
                                                       grid%tmn(i,j)*(0.0 - grid%zs(ns)) ) /(3.0 - 0.0)
                              END DO
                           CASE ( LSMSCHEME , NOAHMPSCHEME , RUCLSMSCHEME, PXLSMSCHEME,CLMSCHEME,SSIBSCHEME )

                              DO ns = 1 , model_config_rec%num_soil_layers
                                 grid%tslb(i,ns,j) = ( grid%tsk(i,j)*(3.0 - grid%zs(ns)) + &
                                                       grid%tmn(i,j)*(0.0 - grid%zs(ns)) ) /(3.0 - 0.0)
                              END DO
                        END SELECT fake_soil_temp
                     else if(grid%tsk(i,j).gt.170. .and. grid%tsk(i,j).lt.400.)then
                        CALL wrf_error_fatal3("<stdin>",2433,&
'grid%tslb unreasonable 1' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%tsk(i,j)
                        END DO
                     else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                        CALL wrf_error_fatal3("<stdin>",2439,&
'grid%tslb unreasonable 2' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%sst(i,j)
                        END DO
                     else if(grid%tmn(i,j).gt.170. .and. grid%tmn(i,j).lt.400.)then
                        CALL wrf_error_fatal3("<stdin>",2445,&
'grid%tslb unreasonable 3' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%tmn(i,j)
                        END DO
                     else
                        CALL wrf_error_fatal3("<stdin>",2451,&
'grid%tslb unreasonable 4' )
                     endif
               END IF
            END DO
         END DO
      END IF

      
      

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold )
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_post ( grid%xice , grid%landmask , grid%tsk , grid%tsk_save , &
                                    grid%ivgtyp , grid%vegcat , grid%lu_index , &
                                    grid%xland , grid%landusef , grid%isltyp , grid%soilcat ,  &
                                    grid%soilctop , &
                                    grid%soilcbot , grid%tmn , grid%vegfra , &
                                    grid%tslb , grid%smois , grid%sh2o , &
                                    grid%seaice_threshold , &
                                    grid%sst,flag_sst, &
                                    config_flags%fractional_seaice, &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    model_config_rec%num_soil_layers , &
                                    grid%iswater , grid%isice , &
                                    model_config_rec%sf_surface_physics(grid%id) , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte )

      

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. &
                   ( grid%ivgtyp(i,j) .NE. config_flags%iswater .OR. grid%isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( grid%landmask(i,j) .GT. 0.5 ) .AND. &
                   ( grid%ivgtyp(i,j) .EQ. config_flags%iswater .OR. grid%isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( grid%tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  grid%ivgtyp(i,j) = 5
                  grid%isltyp(i,j) = 8
                  grid%landmask(i,j) = 1
                  grid%xland(i,j) = 1
               ELSE IF ( grid%sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  grid%ivgtyp(i,j) = config_flags%iswater
                  grid%isltyp(i,j) = 14
                  grid%landmask(i,j) = 0
                  grid%xland(i,j) = 2
               ELSE
                  print *,'the grid%landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%ivgtyp=',grid%ivgtyp(i,j)
                  print *,'grid%isltyp=',grid%isltyp(i,j)
                  print *,'iswater=', config_flags%iswater
                  print *,'grid%tslb=',grid%tslb(i,:,j)
                  print *,'grid%sst=',grid%sst(i,j)
                  CALL wrf_error_fatal3("<stdin>",2517,&
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

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
           IF ( flag_sst .NE. 1 ) THEN
             grid%sst(i,j) = grid%tsk(i,j)
           ENDIF
         END DO
      END DO

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
           IF ( grid%ivgtyp(i,j) .EQ. config_flags%isice) THEN
             grid%snoalb(i,j) = 0.75
           ENDIF
         END DO
      END DO

      
      
      
      

      
      
      

      were_bad = .false.
      IF ( ( (grid%znw(1).LT.(1-1.E-5) ) .OR. ( grid%znw(1).GT.(1+1.E-5) ) ).AND. &
           ( (grid%znw(1).LT.(0-1.E-5) ) .OR. ( grid%znw(1).GT.(0+1.E-5) ) ) ) THEN
         were_bad = .true.
         print *,'Your grid%znw input values are probably half-levels. '
         print *,grid%znw
         print *,'WRF expects grid%znw values to be full levels. '
         print *,'Adjusting now to full levels...'
         
         IF (grid%znw(1).LT.0) THEN
            grid%znw(1)=0
         END IF
         DO k=2,kde
            grid%znw(k)=2*grid%znw(k)-grid%znw(k-1)
         END DO
      END IF

      

      IF ( ( ( grid%znw(1) .LT. (1-1.E-5) ) .OR. ( grid%znw(1) .GT. (1+1.E-5) ) ).AND. &
           ( ( grid%znw(1) .LT. (0-1.E-5) ) .OR. ( grid%znw(1) .GT. (0+1.E-5) ) ) ) THEN
         print *,'The input grid%znw height values were half-levels or erroneous. '
         print *,'Attempts to treat the values as half-levels and change them '
         print *,'to valid full levels failed.'
         CALL wrf_error_fatal3("<stdin>",2581,&
"bad grid%znw values from input files")
      ELSE IF ( were_bad ) THEN
         print *,'...adjusted. grid%znw array now contains full eta level values. '
      ENDIF

      IF ( grid%znw(1) .LT. grid%znw(kde) ) THEN
         DO k=1, kde/2
            hold_znw = grid%znw(k)
            grid%znw(k)=grid%znw(kde+1-k)
            grid%znw(kde+1-k)=hold_znw
         END DO
      END IF

      DO k=1, kde-1
         grid%dnw(k) = grid%znw(k+1) - grid%znw(k)
         grid%rdnw(k) = 1./grid%dnw(k)
         grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
      END DO

      
      

      DO k=2, kde-1
         grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
         grid%rdn(k) = 1./grid%dn(k)
         grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
         grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
      END DO

      

      cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2)
      cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3)

      grid%cf1  = grid%fnp(2) + cof1
      grid%cf2  = grid%fnm(2) - cof1 - cof2
      grid%cf3  = cof2

      grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
      grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)

      

      grid%rdx = 1./config_flags%dx
      grid%rdy = 1./config_flags%dy

      
      
      

      DO j=jts,jte
         DO i=its,ite
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            grid%ph0(i,1,j) = grid%ht(i,j) * g
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      
      
      

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
    
            
            
            

            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )


            DO k = 1, kte-1
               grid%php(i,k,j) = grid%znw(k)*(p_surf - grid%p_top) + grid%p_top 
               grid%pb(i,k,j) = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )

               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO

            

            grid%mub(i,j) = p_surf - grid%p_top

            
            

            pd_surf = grid%mu0(i,j) + grid%p_top

            
            
            

            grid%phb(i,1,j) = grid%ht(i,j) * g
            IF (grid%hypsometric_opt == 1) THEN
               DO k  = 2,kte
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
               END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
               DO k = 2,kte
                  pfu = grid%mub(i,j)*grid%znw(k)   + grid%p_top
                  pfd = grid%mub(i,j)*grid%znw(k-1) + grid%p_top
                  phm = grid%mub(i,j)*grid%znu(k-1) + grid%p_top
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO
            ELSE
               CALL wrf_error_fatal3("<stdin>",2691,&
'initialize_real: hypsometric_opt should be 1 or 2' )
            END IF

         END DO
      END DO


      







      

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         grid%mub(i,j) = grid%mub(i-1,j)
         grid%mu_2(i,j) = grid%mu_2(i-1,j)
         DO k = 1, kte-1
            grid%pb(i,k,j) = grid%pb(i-1,k,j)
            grid%t_init(i,k,j) = grid%t_init(i-1,k,j)
            grid%alb(i,k,j) = grid%alb(i-1,k,j)
         END DO
         DO k = 1, kte
            grid%phb(i,k,j) = grid%phb(i-1,k,j)
         END DO
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         grid%mub(i,j) = grid%mub(i,j-1)
         grid%mu_2(i,j) = grid%mu_2(i,j-1)
         DO k = 1, kte-1
            grid%pb(i,k,j) = grid%pb(i,k,j-1)
            grid%t_init(i,k,j) = grid%t_init(i,k,j-1)
            grid%alb(i,k,j) = grid%alb(i,k,j-1)
         END DO
         DO k = 1, kte
            grid%phb(i,k,j) = grid%phb(i,k,j-1)
         END DO
      END DO
      END IF

      

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            grid%mu_2(i,j) = grid%mu0(i,j) - grid%mub(i,j)
         END DO
      END DO

      

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         grid%mu_2(i,j) = grid%mu_2(i-1,j)
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         grid%mu_2(i,j) = grid%mu_2(i,j-1)
      END DO
      END IF

      lev500 = 0
      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 

            
            

            DO k =  1 , kde-1
               grid%t_2(i,k,j)          = grid%t_2(i,k,j) - t0
            END DO

            dpmu = 10001.
            loop_count = 0

            DO WHILE ( ( ABS(dpmu) .GT. 10. ) .AND. &
                       ( loop_count .LT. 5 ) )

               loop_count = loop_count + 1

               
               
               

               k = kte-1

               qtot=0.
               DO im = PARAM_FIRST_SCALAR, num_3d_m
                 qtot = qtot + moist(i,k,j,im)
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2

               grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf&
                                 *(((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)

               
               

               DO k=kte-2,1,-1
                  qtot=0.
                  DO im = PARAM_FIRST_SCALAR, num_3d_m
                    qtot = qtot + 0.5*(moist(i,k,j,im)+moist(i,k+1,j,im))
                  ENDDO
                  qvf2 = 1./(1.+qtot)
                  qvf1 = qtot*qvf2
                  grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
                  qvf = 1. + rvovrd*moist(i,k,j,P_QV)
                  grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                              (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
                  grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
                  grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
               END DO

               
               

               IF (grid%hypsometric_opt == 1) THEN
                  DO k  = 2,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                   grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                                 + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
                     grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
                  END DO
               ELSE IF (grid%hypsometric_opt == 2) THEN
                
                
                

                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kte
                     pfu = grid%mu0(i,j)*grid%znw(k)   + grid%p_top
                     pfd = grid%mu0(i,j)*grid%znw(k-1) + grid%p_top
                     phm = grid%mu0(i,j)*grid%znu(k-1) + grid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO

                  DO k = 1,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO
               END IF

               
               

               IF ( ( flag_metgrid .EQ. 1 ) .AND. ( i .EQ. i_valid ) .AND. ( j .EQ. j_valid ) ) THEN
                  DO k = 1 , num_metgrid_levels
                     IF ( ABS ( grid%p_gc(i,k,j) - 50000. ) .LT. 1. ) THEN
                        lev500 = k
                        EXIT
                     END IF
                  END DO
               END IF

               
               

               IF ( ( flag_metgrid .EQ. 1 ) .AND. &
                    ( flag_ptheta  .EQ. 0 ) .AND. &
                    ( config_flags%adjust_heights ) .AND. &
                    ( lev500 .NE. 0 ) ) THEN

                  DO k = 2 , kte-1

                     
                     

                     pl = grid%php(i,k  ,j) + &
                          ( grid%p(i,k-1  ,j) * ( grid%znw(k    ) - grid%znu(k  ) ) + &
                            grid%p(i,k    ,j) * ( grid%znu(k-1  ) - grid%znw(k  ) ) ) / &
                          ( grid%znu(k-1  ) - grid%znu(k  ) )
                     pu = grid%php(i,k+1,j) + &
                          ( grid%p(i,k-1+1,j) * ( grid%znw(k  +1) - grid%znu(k+1) ) + &
                            grid%p(i,k  +1,j) * ( grid%znu(k-1+1) - grid%znw(k+1) ) ) / &
                          ( grid%znu(k-1+1) - grid%znu(k+1) )

                     
                     

                     IF ( ( pl .GE. 50000. ) .AND. ( pu .LT. 50000. ) ) THEN
                        zl = ( grid%ph_2(i,k  ,j) + grid%phb(i,k  ,j) ) / g
                        zu = ( grid%ph_2(i,k+1,j) + grid%phb(i,k+1,j) ) / g

                        z500 = ( zl * ( LOG(50000.) - LOG(pu    ) ) + &
                                 zu * ( LOG(pl    ) - LOG(50000.) ) ) / &
                               ( LOG(pl) - LOG(pu) )




                        
                        

                        dz500 = z500 - grid%ght_gc(i,lev500,j)
                        tvsfc = ((grid%t_2(i,1,j)+t0)*((grid%p(i,1,j)+grid%php(i,1,j))/p1000mb)**(r_d/cp)) * &
                                (1.+0.6*moist(i,1,j,P_QV))
                        dpmu = ( grid%php(i,1,j) + grid%p(i,1,j) ) * EXP ( g * dz500 / ( r_d * tvsfc ) )
                        dpmu = dpmu - ( grid%php(i,1,j) + grid%p(i,1,j) )
                        grid%mu_2(i,j) = grid%mu_2(i,j) - dpmu
                        EXIT
                     END IF

                  END DO
               ELSE
                  dpmu = 0.
               END IF

            END DO

         END DO
      END DO

      
      

      grid%v_1 = grid%t_2+t0

      CALL theta_to_t ( grid%v_1 , grid%p_hyd  , p00 , &
                        ids , ide , jds , jde , kds , kde , &
                        ims , ime , jms , jme , kms , kme , &
                        its , ite , jts , jte , kts , kte )

      IF      ( config_flags%rh2qv_method .eq. 1 ) THEN
         CALL rh_to_mxrat1(grid%u_1, grid%v_1, grid%p_hyd , moist(:,:,:,P_QV) ,       &
                           config_flags%rh2qv_wrt_liquid ,                        &
                           config_flags%qv_max_p_safe ,                           &
                           config_flags%qv_max_flag , config_flags%qv_max_value , &
                           config_flags%qv_min_p_safe ,                           &
                           config_flags%qv_min_flag , config_flags%qv_min_value , &
                           ids , ide , jds , jde , kds , kde ,                    &
                           ims , ime , jms , jme , kms , kme ,                    &
                           its , ite , jts , jte , kts , kte-1 )
      ELSE IF ( config_flags%rh2qv_method .eq. 2 ) THEN
         CALL rh_to_mxrat2(grid%u_1, grid%v_1, grid%p_hyd , moist(:,:,:,P_QV) ,       &
                           config_flags%rh2qv_wrt_liquid ,                        &
                           config_flags%qv_max_p_safe ,                           &
                           config_flags%qv_max_flag , config_flags%qv_max_value , &
                           config_flags%qv_min_p_safe ,                           &
                           config_flags%qv_min_flag , config_flags%qv_min_value , &
                           ids , ide , jds , jde , kds , kde ,                    &
                           ims , ime , jms , jme , kms , kme ,                    &
                           its , ite , jts , jte , kts , kte-1 )
      END IF

      
      
      
      
      
      

      IF ( flag_metgrid .NE. 1 ) THEN
         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%u10(i,j)=grid%u_2(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%v10(i,j)=grid%v_2(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
               grid%psfc(i,j)=p_surf + grid%p(i,1,j)
               grid%q2(i,j)=moist(i,1,j,P_QV)
               grid%th2(i,j)=grid%t_2(i,1,j)+300.
               grid%t2(i,j)=grid%th2(i,j)*(((grid%p(i,1,j)+grid%pb(i,1,j))/p00)**(r_d/cp))
            END DO
         END DO

      
      
      
      

      ELSE IF ( flag_metgrid .EQ. 1 ) THEN

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 


               grid%th2(i,j)=grid%t2(i,j)*(p00/(grid%p(i,1,j)+grid%pb(i,1,j)))**(r_d/cp)
            END DO
         END DO
         IF ( flag_qv .NE. 1 ) THEN
            DO j = jts, min(jde-1,jte)
               DO i = its, min(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 

                  grid%q2(i,j)=grid%qv_gc(i,1,j)
               END DO
            END DO
         END IF

      END IF
      CALL cpu_time(t_end)




      grid%save_topo_from_real=1

      
      
      

      IF (config_flags%tracer_opt .eq. 2) THEN
          DO j = (jde + jds)/2 - 4, (jde + jds)/2 + 4, 1
            DO i = (ide + ids)/2 - 4, (ide + ids)/2 + 4, 1
               tracer(i, 1, j, im) = 1.
               IF ( its .LE. i .and. ite .GE. i .and. jts .LE. j .and. jte .GE. j ) THEN
                 tracer(i, 1, j, P_tr17_1) = 1.
                 tracer(i, 1, j, P_tr17_2) = 1.
                 tracer(i, 1, j, P_tr17_3) = 1.
                 tracer(i, 1, j, P_tr17_4) = 1.




               END IF
            END DO
          END DO
      END IF

      
      
      
      
      grid%traj_i    = -9999
      grid%traj_j    = -9999
      grid%traj_k    = -9999
      grid%traj_lat  = -9999
      grid%traj_long = -9999
    
      IF (config_flags%num_traj .gt. 0 .and. config_flags%traj_opt .gt. 0) THEN
         icount = 1
         DO j = (jde + jds)/2 - 2, (jde + jds)/2 + 2, 1 
            DO i = (ide + ids)/2 - 2, (ide + ids)/2 + 2, 1
               IF ( its .LE. i    .and. ite .GE. i   .and.  jts .LE. j    .and. jte .GE. j ) THEN
                  grid%traj_i   (icount) = i
                  grid%traj_j   (icount) = j
                  grid%traj_k   (icount) = 10
                  grid%traj_lat (icount) = grid%xlat(i,j)
                  grid%traj_long(icount) = grid%xlong(i,j)
               END IF
         
               grid%traj_i   (icount) = wrf_dm_max_real ( grid%traj_i   (icount) )
               grid%traj_j   (icount) = wrf_dm_max_real ( grid%traj_j   (icount) )
               grid%traj_k   (icount) = wrf_dm_max_real ( grid%traj_k   (icount) )
               grid%traj_lat (icount) = wrf_dm_max_real ( grid%traj_lat (icount) )
               grid%traj_long(icount) = wrf_dm_max_real ( grid%traj_long(icount) )
         	      
               icount = icount + 1
               IF (icount .GT. config_flags%num_traj) THEN
                  EXIT
               END IF
            END DO
         END DO  
      END IF

      

      IF ( config_flags%sf_ocean_physics .EQ. PWP3DSCHEME ) THEN

         
         
         
         

         DO k = 1,model_config_rec%ocean_levels
            grid%om_depth(:,k,:) = model_config_rec%ocean_z(k)
            grid%om_tmp  (:,k,:) = model_config_rec%ocean_t(k)
            grid%om_s    (:,k,:) = model_config_rec%ocean_s(k)
            grid%om_tini (:,k,:) = model_config_rec%ocean_t(k)
            grid%om_sini (:,k,:) = model_config_rec%ocean_s(k)
            grid%om_u    (:,k,:) = 0.
            grid%om_v    (:,k,:) = 0.
         END DO

         

         grid%om_ml = 5    

         

         grid%om_lon = grid%xlong
         grid%om_lat = grid%xlat

         
         
         
         

         IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
            DO j = jts, min(jde-1,jte)
               DO k = 1,model_config_rec%ocean_levels
                  DO i = its, min(ide-1,ite)
                     grid%om_tmp(i,k,j) = grid%sst(i,j) - ( grid%om_tini(i,1,j) - grid%om_tini(i,k,j) )
                  END DO
               END DO
            END DO
   
            DO j = jts, min(jde-1,jte)
               DO k = 1,model_config_rec%ocean_levels
                  DO i = its, min(ide-1,ite)
                     grid%om_tini(i,k,j) = grid%om_tmp(i,k,j)
                  END DO
               END DO
            END DO
         END IF

      END IF

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte


      



















CALL HALO_EM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_3_sub ( grid, &
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







CALL HALO_EM_INIT_5_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_scalar, &
  scalar, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      IF ( config_flags%sf_ocean_physics .EQ. PWP3DSCHEME ) THEN






CALL HALO_EM_INIT_6_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      END IF

      RETURN

   END SUBROUTINE init_domain_rk



   SUBROUTINE const_module_initialize ( p00 , t00 , a , tiso ) 
      USE module_configure
      IMPLICIT NONE
      
      REAL , INTENT(OUT) :: p00 , t00 , a , tiso
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
      CALL nl_get_iso_temp   ( 1 , tiso )
   END SUBROUTINE const_module_initialize



   SUBROUTINE rebalance_driver ( grid )

      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance( grid &







,grid%moist,grid%moist_bxs,grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys, &
grid%moist_btye,grid%dfi_moist,grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs, &
grid%dfi_moist_btxe,grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys, &
grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs, &
grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys, &
grid%dfi_scalar_btye,grid%aerod,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t, &
grid%nba_mij,grid%nba_rij,grid%chem,grid%tracer,grid%tracer_bxs,grid%tracer_bxe,grid%tracer_bys,grid%tracer_bye, &
grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye &


      )

   END SUBROUTINE rebalance_driver



   SUBROUTINE rebalance ( grid  &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


                        )
      IMPLICIT NONE

      TYPE (domain)          :: grid







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


      TYPE (grid_config_rec_type)              :: config_flags

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2
      REAL :: p00 , t00 , a , tiso
      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: t_init_int

      

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k

      REAL    :: temp, temp_int
      REAL    :: pfu, pfd, phm
      REAL    :: w1, w2, z0, z1, z2

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   
            its = grid%sp32 ; ite = grid%ep32 ;   
            jts = grid%sp33 ; jte = grid%ep33 ;   

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   
            jts = grid%sp32 ; jte = grid%ep32 ;   
            kts = grid%sp33 ; kte = grid%ep33 ;   

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   
            kts = grid%sp32 ; kte = grid%ep32 ;   
            jts = grid%sp33 ; jte = grid%ep33 ;   

      END SELECT

      ALLOCATE ( t_init_int(ims:ime,kms:kme,jms:jme) )

      

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      
      
      

      DO j=jts,jte
         DO i=its,ite
            grid%ph0(i,1,j) = grid%ht_fine(i,j) * g
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      
      
      
      

      IF ( config_flags%use_baseparam_fr_nml ) then
      
         CALL wrf_message('ndown: using namelist constants')
         CALL const_module_initialize ( p00 , t00 , a , tiso ) 
      ELSE
      
         CALL wrf_debug(99,'ndown: using base-state profile constants from input file')
         t00  = grid%t00
         p00  = grid%p00
         a    = grid%tlp
         tiso = grid%tiso

         IF (t00 .LT. 100. .or. p00 .LT. 10000.) THEN
            WRITE(wrf_err_message,*)&
      'ndown_em: did not find base state parameters in wrfout. Add use_baseparam_fr_nml = .t. in &dynamics and rerun'
            CALL wrf_error_fatal3("<stdin>",3473,&
TRIM(wrf_err_message))
         ENDIF
      ENDIF
 
      hold_ups = .true.

      
      
      

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            
            
            
            

            p_surf     = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht_fine(i,j)/a/r_d ) **0.5 )
            p_surf_int = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)     /a/r_d ) **0.5 )

            DO k = 1, kte-1
               grid%pb(i,k,j) = grid%znu(k)*(p_surf     - grid%p_top) + grid%p_top
               pb_int    = grid%znu(k)*(p_surf_int - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )

               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0

               temp_int = MAX ( tiso, t00 + A*LOG(pb_int   /p00) )
               t_init_int(i,k,j)= temp_int*(p00/pb_int   )**(r_d/cp) - t0

               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO

            

            grid%mub(i,j) = p_surf - grid%p_top

            
            

            pd_surf = ( grid%mub(i,j) + grid%mu_2(i,j) ) + grid%p_top

            
            
            

            grid%phb(i,1,j) = grid%ht_fine(i,j) * g
            IF (grid%hypsometric_opt == 1) THEN
              DO k = 2,kte
                 grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
              END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
              DO k = 2,kte
                 pfu = grid%mub(i,j)*grid%znw(k)   + grid%p_top
                 pfd = grid%mub(i,j)*grid%znw(k-1) + grid%p_top
                 phm = grid%mub(i,j)*grid%znu(k-1) + grid%p_top
                 grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
              END DO
            ELSE
              CALL wrf_error_fatal3("<stdin>",3534,&
'initialize_real: hypsometric_opt should be 1 or 2' )
            END IF
         END DO
      END DO

      

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%ht(i,j) = grid%ht_fine(i,j)
         END DO
      END DO

      

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)

            

            DO k =  1 , kde-1
               grid%t_2(i,k,j) = grid%t_2(i,k,j) + ( grid%t_init(i,k,j) - t_init_int(i,k,j) )
            END DO

            
            
            

            k = kte-1

            qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
            qvf2 = 1./(1.+qvf1)
            qvf1 = qvf1*qvf2

            grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
            qvf = 1. + rvovrd*moist(i,k,j,P_QV)
            grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                                 (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
            grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
            grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)

            
            

            DO k=kte-2,1,-1
               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                           (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
            END DO

            
            

            IF (grid%hypsometric_opt == 1) THEN
               DO k  = 2,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                              + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN

             
             
             

               grid%ph_2(i,1,j) = grid%phb(i,1,j)
               DO k = 2,kte
                  pfu = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k)   + grid%p_top
                  pfd = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k-1) + grid%p_top
                  phm = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znu(k-1) + grid%p_top
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO

               DO k = 1,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
               END DO

               DO k = 1,kte
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO

            END IF



            z0 = grid%ph0(i,1,j)/g
            z1 = 0.5*(grid%ph0(i,1,j)+grid%ph0(i,2,j))/g
            z2 = 0.5*(grid%ph0(i,2,j)+grid%ph0(i,3,j))/g
            w1 = (z0 - z2)/(z1 - z2)
            w2 = 1. - w1
            grid%psfc(i,j) = w1*(grid%p(i,1,j)+grid%pb(i,1,j))+w2*(grid%p(i,2,j)+grid%pb(i,2,j))

         END DO
      END DO

      DEALLOCATE ( t_init_int )

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte






CALL HALO_EM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_INIT_3_sub ( grid, &
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







CALL HALO_EM_INIT_5_sub ( grid, &
  num_moist, &
  moist, &
  num_chem, &
  chem, &
  num_scalar, &
  scalar, &
  num_tracer, &
  tracer, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

   END SUBROUTINE rebalance



   RECURSIVE SUBROUTINE find_my_parent ( grid_ptr_in , grid_ptr_out , id_i_am , id_wanted , found_the_id )







      USE module_domain

      TYPE(domain) , POINTER :: grid_ptr_in , grid_ptr_out
      TYPE(domain) , POINTER :: grid_ptr_sibling
      INTEGER :: id_wanted , id_i_am
      INTEGER :: nest                    
      LOGICAL :: found_the_id

      found_the_id = .FALSE.
      grid_ptr_sibling => grid_ptr_in
      nest = 0                           

      DO WHILE ( ASSOCIATED ( grid_ptr_sibling ) )

         IF ( grid_ptr_sibling%grid_id .EQ. id_wanted ) THEN
            found_the_id = .TRUE.
            grid_ptr_out => grid_ptr_sibling
            RETURN

         ELSE IF ( grid_ptr_sibling%num_nests .GT. 0 .AND. nest .LT. grid_ptr_sibling%num_nests ) THEN
            nest = nest + 1               
            grid_ptr_sibling => grid_ptr_sibling%nests(nest)%ptr 
            CALL find_my_parent ( grid_ptr_sibling , grid_ptr_out , id_i_am , id_wanted , found_the_id )
            IF (.NOT. found_the_id) grid_ptr_sibling => grid_ptr_sibling%parents(1)%ptr   
         ELSE
            grid_ptr_sibling => grid_ptr_sibling%sibling
         END IF

      END DO

   END SUBROUTINE find_my_parent



   RECURSIVE SUBROUTINE find_my_parent2 ( grid_ptr_in , grid_ptr_out , id_wanted , found_the_id )

      USE module_domain

      TYPE(domain) , POINTER               :: grid_ptr_in
      TYPE(domain) , POINTER               :: grid_ptr_out
      INTEGER                , INTENT(IN ) :: id_wanted
      LOGICAL                , INTENT(OUT) :: found_the_id

      

      TYPE(domain) , POINTER :: grid_ptr_holder
      INTEGER :: kid

      

      found_the_id = .FALSE.
      grid_ptr_holder => grid_ptr_in


      
      

      IF ( id_wanted .EQ. grid_ptr_in%grid_id ) THEN

         found_the_id = .TRUE.
         grid_ptr_out => grid_ptr_in


      

      ELSE

      
      
      

         loop_over_all_kids : DO kid = 1 , grid_ptr_in%num_nests

            IF ( ASSOCIATED ( grid_ptr_in%nests(kid)%ptr ) ) THEN

               CALL find_my_parent2 ( grid_ptr_in%nests(kid)%ptr , grid_ptr_out , id_wanted , found_the_id )
               IF ( found_the_id ) THEN
                  EXIT loop_over_all_kids
               END IF

            END IF
         END DO loop_over_all_kids

      END IF

   END SUBROUTINE find_my_parent2







   SUBROUTINE vert_interp ( fo , po , fnew , pnu , &
                            generic , var_type , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   
   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order , extrap_type
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      REAL    , INTENT(IN)        :: zap_close_levels
      INTEGER , INTENT(IN)        :: force_sfc_in_vinterp
      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      INTEGER , INTENT(IN)        :: generic

      CHARACTER (LEN=1) :: var_type

      REAL , DIMENSION(ims:ime,generic,jms:jme) , INTENT(IN)     :: fo , po
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: pnu
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: fnew

      REAL , DIMENSION(ims:ime,generic,jms:jme)                  :: forig , porig
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)                  :: pnew

      

      INTEGER :: i , j , k , ko , kn , k1 , k2 , ko_1 , ko_2 , knext
      INTEGER :: istart , iend , jstart , jend , kstart , kend
      INTEGER , DIMENSION(ims:ime,kms:kme        )               :: k_above , k_below
      INTEGER , DIMENSION(ims:ime                )               :: ks
      INTEGER , DIMENSION(ims:ime                )               :: ko_above_sfc
      INTEGER :: count , zap , zap_below , zap_above , kst , kcount
      INTEGER :: kinterp_start , kinterp_end , sfc_level

      LOGICAL :: any_below_ground

      REAL :: p1 , p2 , pn, hold
      REAL , DIMENSION(1:generic) :: ordered_porig , ordered_forig
      REAL , DIMENSION(kts:kte) :: ordered_pnew , ordered_fnew
    
      

      LOGICAL :: any_valid_points
      INTEGER :: i_valid , j_valid
      LOGICAL :: flip_data_required

      

      IF      ( var_type .EQ. 'U' ) THEN
         istart = its
         iend   = ite
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  porig(i,k,j) = ( po(i,k,j) + po(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = 1,generic
                  porig(its,k,j) =  po(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = 1,generic
                  porig(ite,k,j) =  po(ite-1,k,j)
               END DO
            END IF

            DO k = kstart,kend
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = kstart,kend
                  pnew(its,k,j) =  pnu(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = kstart,kend
                  pnew(ite,k,j) =  pnu(ite-1,k,j)
               END DO
            END IF
         END DO
      ELSE IF ( var_type .EQ. 'V' ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = jte
         kstart = kts
         kend   = kte-1
         DO i = istart,iend
            DO k = 1,generic
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  porig(i,k,j) = ( po(i,k,j) + po(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = 1,generic
                  porig(i,k,jts) =  po(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
               DO k = 1,generic
                  porig(i,k,jte) =  po(i,k,jte-1)
               END DO
            END IF

            DO k = kstart,kend
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = kstart,kend
                  pnew(i,k,jts) =  pnu(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
              DO k = kstart,kend
                  pnew(i,k,jte) =  pnu(i,k,jte-1)
               END DO
            END IF
         END DO
      ELSE IF ( ( var_type .EQ. 'W' ) .OR.  ( var_type .EQ. 'Z' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE IF ( ( var_type .EQ. 'T' ) .OR. ( var_type .EQ. 'Q' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      END IF

      
      

      any_valid_points = .false.
      find_valid : DO j = jstart , jend
         DO i = istart , iend
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            any_valid_points = .true.
            i_valid = i
            j_valid = j
            EXIT find_valid
         END DO 
      END DO find_valid
      IF ( .NOT. any_valid_points ) THEN
         RETURN
      END IF

      IF ( porig(i_valid,2,j_valid) .LT. porig(i_valid,generic,j_valid) ) THEN
         flip_data_required = .true.
      ELSE
         flip_data_required = .false.
      END IF

      DO j = jstart , jend

         
         
         

         IF ( flip_data_required ) THEN
            DO kn = 2 , ( generic + 1 ) / 2
               DO i = istart , iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  hold                    = porig(i,kn,j)
                  porig(i,kn,j)           = porig(i,generic+2-kn,j)
                  porig(i,generic+2-kn,j) = hold
                  forig(i,kn,j)           = fo   (i,generic+2-kn,j)
                  forig(i,generic+2-kn,j) = fo   (i,kn,j)
               END DO
            END DO
            DO i = istart , iend
               forig(i,1,j)               = fo   (i,1,j)
            END DO
            IF ( MOD(generic,2) .EQ. 0 ) THEN
               k=generic/2 + 1
               DO i = istart , iend
                  forig(i,k,j)            = fo   (i,k,j)
               END DO
            END IF
         ELSE
            DO kn = 1 , generic
               DO i = istart , iend
                  forig(i,kn,j)           = fo   (i,kn,j)
               END DO
            END DO
         END IF

         
         
         
         

         DO i = istart , iend
            ko_above_sfc(i) = -1
         END DO
         DO ko = kstart+1 , generic
            DO i = istart , iend
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( ko_above_sfc(i) .EQ. -1 ) THEN
                  IF ( porig(i,1,j) .GT. porig(i,ko,j) ) THEN
                     ko_above_sfc(i) = ko
                  END IF
               END IF
            END DO
         END DO

         
         

         DO i = istart , iend
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 

            
            
            
            
            
            
            

            
            
            

            

            zap = 0
            zap_below = 0
            zap_above = 0

            IF (  ko_above_sfc(i) .GT. 2 ) THEN
               count = 1
               DO ko = 2 , ko_above_sfc(i)-1
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

               
               
               
               

               IF ( ordered_porig(count-1) - porig(i,1,j) .LT. zap_close_levels ) THEN
                  count = count -1
                  zap = 1
                  zap_below = 1
               END IF

               

               ordered_porig(count) = porig(i,1,j)
               ordered_forig(count) = forig(i,1,j)
               count = count + 1

               
               
               
               
               
               
               
               

               IF ( force_sfc_in_vinterp .GT. 0 ) THEN

                  
                  
                  
                  
                  

                  knext = ko_above_sfc(i)
                  find_level : DO ko = ko_above_sfc(i) , generic
                     IF ( porig(i,ko,j) .LE. pnew(i,force_sfc_in_vinterp,j) ) THEN
                        knext = ko
                        exit find_level
                     ELSE
                        zap = zap + 1
                        zap_above = zap_above + 1
                     END IF
                  END DO find_level

               
               

               ELSE
                  knext = ko_above_sfc(i)
               END IF

               
               
               
               
               

               IF ( ordered_porig(count-1) - porig(i,knext,j) .LT. zap_close_levels ) THEN
                  kst = knext+1
                  zap = zap + 1
                  zap_above = zap_above + 1
               ELSE
                  kst = knext
               END IF

               DO ko = kst , generic
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            
            
            
            

            ELSE

               

               zap = 0

               

               ordered_porig(1) = porig(i,1,j)
               ordered_forig(1) = forig(i,1,j)

               

               count = 2

               
               

               IF ( force_sfc_in_vinterp .GT. 0 ) THEN
                  knext = 2
                  find_level2: DO ko = 2 , generic
                     IF ( porig(i,ko,j) .LE. pnew(i,force_sfc_in_vinterp,j) ) THEN
                        knext = ko
                        exit find_level2
                     ELSE
                        zap = zap + 1
                        zap_above = zap_above + 1
                     END IF
                  END DO find_level2
               ELSE
                  knext = 2
               END IF

               
               
               
               

               DO ko = knext , generic
                  IF ( ( ordered_porig(count-1) - porig(i,ko,j) .LT. zap_close_levels ) .AND. &
                       ( ko .LT. generic ) ) THEN
                     zap = zap + 1
                     zap_above = zap_above + 1
                     CYCLE
                  END IF
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            END IF

            

            DO kn = kstart , kend
               ordered_pnew(kn) = pnew(i,kn,j)
            END DO

            

            IF      ( ( use_levels_below_ground ) .AND. ( use_surface ) ) THEN

               
               
               

               count = 0
               find_how_many_1 : DO ko = 1 , generic
                  IF ( porig(i,generic,j) .EQ. ordered_porig(ko) ) THEN
                     count = count + 1
                     EXIT find_how_many_1
                  ELSE
                     count = count + 1
                  END IF
               END DO find_how_many_1
               kinterp_start = 1
               kinterp_end = kinterp_start + count - 1

            ELSE IF ( ( use_levels_below_ground ) .AND. ( .NOT. use_surface ) ) THEN

               
               
               

               count = 0
               find_sfc_2 : DO ko = 1 , generic
                  IF ( porig(i,1,j) .EQ. ordered_porig(ko) ) THEN
                     sfc_level = ko
                     EXIT find_sfc_2
                  END IF
               END DO find_sfc_2

               DO ko = sfc_level , generic-1
                  ordered_porig(ko) = ordered_porig(ko+1)
                  ordered_forig(ko) = ordered_forig(ko+1)
               END DO
               ordered_porig(generic) = 1.E-5
               ordered_forig(generic) = 1.E10

               count = 0
               find_how_many_2 : DO ko = 1 , generic
                  IF ( porig(i,generic,j) .EQ. ordered_porig(ko) ) THEN
                     count = count + 1
                     EXIT find_how_many_2
                  ELSE
                     count = count + 1
                  END IF
               END DO find_how_many_2
               kinterp_start = 1
               kinterp_end = kinterp_start + count - 1

            ELSE IF ( ( .NOT. use_levels_below_ground ) .AND. ( use_surface ) ) THEN

               

               kcount = ko_above_sfc(i)-1-zap_below
               count = 0
               DO ko = 1 , generic
                  IF ( porig(i,ko,j) .EQ. ordered_porig(kcount) ) THEN

                     kcount = kcount + 1
                     count = count + 1
                  ELSE

                  END IF
               END DO
               kinterp_start = ko_above_sfc(i)-1-zap_below
               kinterp_end = kinterp_start + count - 1

            END IF

            

            IF ( interp_type .EQ. 1 ) THEN
               CALL lagrange_setup ( var_type , interp_type , &
                    ordered_porig(kinterp_start:kinterp_end) , &
                    ordered_forig(kinterp_start:kinterp_end) , &
                    count , lagrange_order , extrap_type , &
                    ordered_pnew(kstart:kend)  , ordered_fnew  , kend-kstart+1 ,i,j)
            ELSE
               CALL lagrange_setup ( var_type , interp_type , &
                LOG(ordered_porig(kinterp_start:kinterp_end)) , &
                    ordered_forig(kinterp_start:kinterp_end) , &
                    count , lagrange_order , extrap_type , &
                LOG(ordered_pnew(kstart:kend)) , ordered_fnew  , kend-kstart+1 ,i,j)
            END IF

            

            DO kn = kstart , kend
               fnew(i,kn,j) = ordered_fnew(kn)
            END DO

            
            
            

            IF ( lowest_lev_from_sfc ) THEN
               fnew(i,1,j) = forig(i,1,j)
            END IF

         END DO

      END DO

   END SUBROUTINE vert_interp



   SUBROUTINE vert_interp_old ( forig , po , fnew , pnu , &
                            generic , var_type , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   
   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order , extrap_type
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      REAL    , INTENT(IN)        :: zap_close_levels
      INTEGER , INTENT(IN)        :: force_sfc_in_vinterp
      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      INTEGER , INTENT(IN)        :: generic

      CHARACTER (LEN=1) :: var_type

      REAL , DIMENSION(ims:ime,generic,jms:jme) , INTENT(IN)     :: forig , po
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: pnu
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: fnew

      REAL , DIMENSION(ims:ime,generic,jms:jme)                  :: porig
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)                  :: pnew

      

      INTEGER :: i , j , k , ko , kn , k1 , k2 , ko_1 , ko_2
      INTEGER :: istart , iend , jstart , jend , kstart , kend
      INTEGER , DIMENSION(ims:ime,kms:kme        )               :: k_above , k_below
      INTEGER , DIMENSION(ims:ime                )               :: ks
      INTEGER , DIMENSION(ims:ime                )               :: ko_above_sfc

      LOGICAL :: any_below_ground

      REAL :: p1 , p2 , pn
integer vert_extrap
vert_extrap = 0

      

      IF      ( var_type .EQ. 'U' ) THEN
         istart = its
         iend   = ite
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  porig(i,k,j) = ( po(i,k,j) + po(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = 1,generic
                  porig(its,k,j) =  po(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = 1,generic
                  porig(ite,k,j) =  po(ite-1,k,j)
               END DO
            END IF

            DO k = kstart,kend
               DO i = MAX(ids+1,its) , MIN(ide-1,ite)
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = kstart,kend
                  pnew(its,k,j) =  pnu(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = kstart,kend
                  pnew(ite,k,j) =  pnu(ite-1,k,j)
               END DO
            END IF
         END DO
      ELSE IF ( var_type .EQ. 'V' ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = jte
         kstart = kts
         kend   = kte-1
         DO i = istart,iend
            DO k = 1,generic
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  porig(i,k,j) = ( po(i,k,j) + po(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = 1,generic
                  porig(i,k,jts) =  po(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
               DO k = 1,generic
                  porig(i,k,jte) =  po(i,k,jte-1)
               END DO
            END IF

            DO k = kstart,kend
               DO j = MAX(jds+1,jts) , MIN(jde-1,jte)
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = kstart,kend
                  pnew(i,k,jts) =  pnu(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
              DO k = kstart,kend
                  pnew(i,k,jte) =  pnu(i,k,jte-1)
               END DO
            END IF
         END DO
      ELSE IF ( ( var_type .EQ. 'W' ) .OR.  ( var_type .EQ. 'Z' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE IF ( ( var_type .EQ. 'T' ) .OR. ( var_type .EQ. 'Q' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      END IF

      DO j = jstart , jend

         
         
         
         

         DO i = istart , iend
            ko_above_sfc(i) = -1
         END DO
         DO ko = kstart+1 , kend
            DO i = istart , iend
               IF ( ko_above_sfc(i) .EQ. -1 ) THEN
                  IF ( porig(i,1,j) .GT. porig(i,ko,j) ) THEN
                     ko_above_sfc(i) = ko
                  END IF
               END IF
            END DO
         END DO

         
         

         DO kn = kts , kte
            DO i = its , ite
               k_above(i,kn) = -1
               k_below(i,kn) = -2
            END DO
         END DO

         
         

         DO i = its , ite
            ks(i) = 1
         END DO

         
         

         DO kn = kstart , kend

            DO i = istart , iend

               
               
               

               found_trap_above : DO ko = ks(i) , generic-1

                  
                  
                  
                  
                  
                  
                  

                  
                  

                  IF      ( ( ko .LT. ko_above_sfc(i) ) .AND. ( ko .EQ. 1 ) ) THEN
                     ko_1 = ko
                     ko_2 = ko_above_sfc(i)

                  
                  

                  ELSE IF ( ( ko .LT. ko_above_sfc(i) ) .AND. ( ko .NE. 1 ) ) THEN
                     CYCLE found_trap_above

                  
                  

                  ELSE
                     ko_1 = ko
                     ko_2 = ko+1

                  END IF

                  
                  

                  
                  

                  IF      ( ( porig(i,ko_1,j) .GE. pnew(i,kn,j) ) .AND. &
                            ( porig(i,ko_2,j) .LT. pnew(i,kn,j) ) ) THEN
                     k_above(i,kn) = ko_2
                     k_below(i,kn) = ko_1
                     ks(i) = ko_1
                     EXIT found_trap_above

                  
                  
                  

                  ELSE IF   ( porig(i,1,j) .LT. pnew(i,kn,j) ) THEN

                     

                     IF      ( ( var_type .EQ. 'U' ) .OR. &
                               ( var_type .EQ. 'V' ) .OR. &
                               ( var_type .EQ. 'Q' ) ) THEN
                        k_above(i,kn) = 1
                        ks(i) = 1

                     
                     
                     
                     

                     ELSE IF ( ( var_type .EQ. 'Z' ) .OR. &
                               ( var_type .EQ. 'T' ) ) THEN
                        k_above(i,kn) = ko_above_sfc(i)
                        k_below(i,kn) = 1
                        ks(i) = 1

                     

                     ELSE
                        k_above(i,kn) = 1
                        ks(i) = 1
                     END IF

                     EXIT found_trap_above

                  
                  
                  

                  ELSE IF   ( porig(i,generic,j) .GT. pnew(i,kn,j) ) THEN
                     print *,'data is too high, try a lower p_top'
                     print *,'pnew=',pnew(i,kn,j)
                     print *,'porig=',porig(i,:,j)
                     CALL wrf_error_fatal3("<stdin>",4705,&
'requested p_top is higher than input data, lower p_top')

                  END IF
               END DO found_trap_above
            END DO
         END DO

         

         DO kn = kstart , kend
            DO i = istart , iend
               IF ( k_above(i,kn) .EQ. 1 ) THEN
                  fnew(i,kn,j) = forig(i,1,j)
               ELSE
                  k2 = MAX ( k_above(i,kn) , 2)
                  k1 = MAX ( k_below(i,kn) , 1)
                  IF ( k1 .EQ. k2 ) THEN
                     CALL wrf_error_fatal3("<stdin>",4723,&
'identical values in the interp, bad for divisions' )
                  END IF
                  IF      ( interp_type .EQ. 1 ) THEN
                     p1 = porig(i,k1,j)
                     p2 = porig(i,k2,j)
                     pn = pnew(i,kn,j)
                  ELSE IF ( interp_type .EQ. 2 ) THEN
                     p1 = ALOG(porig(i,k1,j))
                     p2 = ALOG(porig(i,k2,j))
                     pn = ALOG(pnew(i,kn,j))
                  END IF
                  IF ( ( p1-pn) * (p2-pn) > 0. ) THEN


vert_extrap = vert_extrap + 1
                  END IF
                  fnew(i,kn,j) = ( forig(i,k1,j) * ( p2 - pn )   + &
                                   forig(i,k2,j) * ( pn - p1 ) ) / &
                                   ( p2 - p1 )
               END IF
            END DO
         END DO

         search_below_ground : DO kn = kstart , kend
            any_below_ground = .FALSE.
            DO i = istart , iend
               IF ( k_above(i,kn) .EQ. 1 ) THEN
                  fnew(i,kn,j) = forig(i,1,j)
                  any_below_ground = .TRUE.
               END IF
            END DO
            IF ( .NOT. any_below_ground ) THEN
               EXIT search_below_ground
            END IF
         END DO search_below_ground

         
         
         

         DO i = istart , iend
            IF ( lowest_lev_from_sfc ) THEN
               fnew(i,1,j) = forig(i,ko_above_sfc(i),j)
            END IF
         END DO

      END DO
print *,'VERT EXTRAP = ', vert_extrap

   END SUBROUTINE vert_interp_old



   SUBROUTINE lagrange_setup ( var_type , interp_type , all_x , all_y , all_dim , n , extrap_type , &
                               target_x , target_y , target_dim ,i,j)

      
      
      
      
      
      
      
      

      IMPLICIT NONE

      CHARACTER (LEN=1) :: var_type
      INTEGER , INTENT(IN) :: interp_type , all_dim , n , extrap_type , target_dim
      REAL, DIMENSION(all_dim) , INTENT(IN) :: all_x , all_y
      REAL , DIMENSION(target_dim) , INTENT(IN) :: target_x
      REAL , DIMENSION(target_dim) , INTENT(OUT) :: target_y


      INTEGER :: K
      REAL :: DX, ALPHA, BETA, GAMMA, ETA
      REAL , DIMENSION(all_dim) :: P2


      

      INTEGER , INTENT(IN) :: i,j

      

      REAL , DIMENSION(n+1) :: x , y
      REAL :: a , b
      REAL :: target_y_1 , target_y_2
      LOGICAL :: found_loc
      INTEGER :: loop , loc_center_left , loc_center_right , ist , iend , target_loop
      INTEGER :: vboundb , vboundt

      

      REAL :: temp_1 , temp_2 , temp_3 , temp_y
      REAL :: depth_of_extrap_in_p , avg_of_extrap_p , temp_extrap_starting_point , dhdp , dh , dt
      REAL , PARAMETER :: RovCp      = rcp
      REAL , PARAMETER :: CRC_const1 = 11880.516      
      REAL , PARAMETER :: CRC_const2 =     0.1902632  
      REAL , PARAMETER :: CRC_const3 =     0.0065     
      REAL, DIMENSION(all_dim) :: all_x_full
      REAL , DIMENSION(target_dim) :: target_x_full

      IF ( all_dim .LT. n+1 ) THEN
print *,'all_dim = ',all_dim
print *,'order = ',n
print *,'i,j = ',i,j
print *,'p array = ',all_x
print *,'f array = ',all_y
print *,'p target= ',target_x
         CALL wrf_error_fatal3("<stdin>",4834,&
'troubles, the interpolating order is too large for this few input values' )
      END IF

      IF ( n .LT. 1 ) THEN
         CALL wrf_error_fatal3("<stdin>",4839,&
'pal, linear is about as low as we go' )
      END IF

      
      
      

      vboundb = 4
      vboundt = 0

      

      DO target_loop = 1 , target_dim

         

         found_loc = .FALSE.
         find_trap : DO loop = 1 , all_dim -1
            a = target_x(target_loop) - all_x(loop)
            b = target_x(target_loop) - all_x(loop+1)
            IF ( a*b .LE. 0.0 ) THEN
               loc_center_left  = loop
               loc_center_right = loop+1
               found_loc = .TRUE.
               EXIT find_trap
            END IF
         END DO find_trap

         IF ( ( .NOT. found_loc ) .AND. ( target_x(target_loop) .GT. all_x(1) ) ) THEN

            

            IF ( interp_type .EQ. 1 ) THEN
               all_x_full    =       all_x
               target_x_full =       target_x
            ELSE
               all_x_full    = EXP ( all_x )
               target_x_full = EXP ( target_x )
            END IF
            

            IF      ( ( extrap_type .EQ. 1 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               temp_1 = all_y(1) * ( all_x_full(1) / 100000. ) ** RovCp
               target_y(target_loop) = temp_1 * ( 100000. / target_x_full(target_loop) ) ** RovCp

            

            ELSE IF ( ( extrap_type .EQ. 2 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               depth_of_extrap_in_p = target_x_full(target_loop) - all_x_full(1)
               avg_of_extrap_p = ( target_x_full(target_loop) + all_x_full(1) ) * 0.5
               temp_extrap_starting_point = all_y(1) * ( all_x_full(1) / 100000. ) ** RovCp
               dhdp = CRC_const1 * CRC_const2 * ( avg_of_extrap_p / 100. ) ** ( CRC_const2 - 1. )
               dh = dhdp * ( depth_of_extrap_in_p / 100. )
               dt = dh * CRC_const3
               target_y(target_loop) = ( temp_extrap_starting_point + dt ) * ( 100000. / target_x_full(target_loop) ) ** RovCp

            

            ELSE IF ( ( extrap_type .EQ. 3 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               target_y(target_loop) = all_y(1)


            

            ELSE IF ( extrap_type .EQ. 1 ) THEN

               target_y(target_loop) = ( all_y(2) * ( target_x(target_loop) - all_x(3) ) + &
                                         all_y(3) * ( all_x(2) - target_x(target_loop) ) ) / &
                                       ( all_x(2) - all_x(3) )

            

            ELSE IF ( extrap_type .EQ. 2 ) THEN

               target_y(target_loop) = all_y(1)

            ELSE IF ( extrap_type .EQ. 3 ) THEN
               CALL wrf_error_fatal3("<stdin>",4920,&
'You are not allowed to use extrap_option #3 for any var except for theta.' )

            END IF
            CYCLE
         ELSE IF ( .NOT. found_loc ) THEN
            print *,'i,j = ',i,j
            print *,'target pressure and value = ',target_x(target_loop),target_y(target_loop)
            DO loop = 1 , all_dim
               print *,'column of pressure and value = ',all_x(loop),all_y(loop)
            END DO
            CALL wrf_error_fatal3("<stdin>",4931,&
'troubles, could not find trapping x locations' )
         END IF

         
         
         


         IF ( n .EQ. 9 ) THEN
            CALL cubic_spline (all_dim-1, all_x, all_y, P2)



          DX = all_x(loc_center_right) - all_x(loc_center_left)
          ALPHA = P2(loc_center_right)/(6*DX)
          BETA = -P2(loc_center_left)/(6*DX)
          GAMMA = all_y(loc_center_right)/DX - DX*P2(loc_center_right)/6
          ETA = DX*P2(loc_center_left)/6 - all_y(loc_center_left)/DX
          target_y(target_loop) = ALPHA*(target_x(target_loop)-all_x(loc_center_left))*(target_x(target_loop)-all_x(loc_center_left)) &
                                  *(target_x(target_loop)-all_x(loc_center_left)) &
                     +BETA*(target_x(target_loop)-all_x(loc_center_right))*(target_x(target_loop)-all_x(loc_center_right)) &
                                  *(target_x(target_loop)-all_x(loc_center_right)) &
                     +GAMMA*(target_x(target_loop)-all_x(loc_center_left)) &
                     +ETA*(target_x(target_loop)-all_x(loc_center_right))




         ELSE IF ( MOD(n,2) .NE. 0 ) THEN
            IF ( ( loc_center_left -(((n+1)/2)-1) .GE.       1 ) .AND. &
                 ( loc_center_right+(((n+1)/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n+1)/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop) )
            ELSE
               IF ( .NOT. found_loc ) THEN
                  CALL wrf_error_fatal3("<stdin>",4968,&
'I doubt this will happen, I will only do 2nd order for now' )
               END IF
            END IF

         ELSE IF ( ( MOD(n,2) .EQ. 0 ) .AND. &
                   ( ( target_loop .GE. 1 + vboundb ) .AND. ( target_loop .LE. target_dim - vboundt ) ) ) THEN
            IF      ( ( loc_center_left -(((n  )/2)-1) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)  ) .LE. all_dim ) .AND. &
                      ( loc_center_left -(((n  )/2)  ) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y_1              )
               ist  = loc_center_left -(((n  )/2)  )
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y_2              )
               target_y(target_loop) = ( target_y_1 + target_y_2 ) * 0.5

            ELSE IF ( ( loc_center_left -(((n  )/2)-1) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)  ) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop)   )
            ELSE IF ( ( loc_center_left -(((n  )/2)  ) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)  )
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop)   )
            ELSE
               CALL wrf_error_fatal3("<stdin>",4998,&
'unauthorized area, you should not be here' )
            END IF

         ELSE IF ( MOD(n,2) .EQ. 0 ) THEN
               ist  = loc_center_left
               iend = loc_center_right
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , 1 , target_x(target_loop) , target_y(target_loop) )

         END IF

      END DO

   END SUBROUTINE lagrange_setup





      SUBROUTINE cubic_spline (N, XI, FI, P2)
      
      
      
      
      INTEGER :: I
      INTEGER, INTENT (IN) :: N
      REAL, INTENT (IN), DIMENSION (N):: XI, FI
      REAL, INTENT (OUT), DIMENSION (N):: P2
      REAL, DIMENSION (N):: G, H
      REAL, DIMENSION (N-1):: D, B, C



  DO I = 1, N
    H(I) = XI(I+1) - XI(I)
    G(I) = FI(I+1) - FI(I)
  END DO


  DO I = 1, N-1
    D(I) = 2*(H(I+1)+H(I))
    B(I) = 6*(G(I+1)/H(I+1)-G(I)/H(I))
    C(I) = H(I+1)
  END DO



  CALL TRIDIAGONAL_LINEAR_EQ (N-1, D, C, C, B, G)
  P2(1) = 0
  P2(N+1) = 0
  DO I = 2, N
    P2(I) = G(I-1)
  END DO

END SUBROUTINE cubic_spline
 


    SUBROUTINE TRIDIAGONAL_LINEAR_EQ (L, D, E, C, B, Z)



  INTEGER, INTENT (IN) :: L
  INTEGER :: I
  REAL, INTENT (IN), DIMENSION (L):: D, E, C, B
  REAL, INTENT (OUT), DIMENSION (L):: Z
  REAL, DIMENSION (L):: Y, W
  REAL, DIMENSION (L-1):: V, T



  W(1) = D(1)
  V(1)  = C(1)
  T(1)  = E(1)/W(1)
  DO I = 2, L - 1
    W(I) = D(I)-V(I-1)*T(I-1)
    V(I) = C(I)
    T(I) = E(I)/W(I)
  END DO
  W(L) = D(L)-V(L-1)*T(L-1)



  Y(1) = B(1)/W(1)
  DO I = 2, L
    Y(I) = (B(I)-V(I-1)*Y(I-1))/W(I)
  END DO


  Z(L) = Y(L)
  DO I = L-1, 1, -1
    Z(I) = Y(I) - T(I)*Z(I+1)
  END DO

END SUBROUTINE TRIDIAGONAL_LINEAR_EQ





   SUBROUTINE lagrange_interp ( x , y , n , target_x , target_y )

      
      
      
      
      

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: n
      REAL , DIMENSION(0:n) , INTENT(IN) :: x , y
      REAL , INTENT(IN) :: target_x

      REAL , INTENT(OUT) :: target_y

      

      INTEGER :: i , k
      REAL :: numer , denom , Px
      REAL , DIMENSION(0:n) :: Ln

      Px = 0.
      DO i = 0 , n
         numer = 1.
         denom = 1.
         DO k = 0 , n
            IF ( k .EQ. i ) CYCLE
            numer = numer * ( target_x  - x(k) )
            denom = denom * ( x(i)  - x(k) )
         END DO
         IF ( denom .NE. 0. ) THEN
            Ln(i) = y(i) * numer / denom
            Px = Px + Ln(i)
         ENDIF
      END DO
      target_y = Px

   END SUBROUTINE lagrange_interp



   SUBROUTINE p_dry ( mu0 , eta , pdht , pdry , full_levs , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL :: full_levs

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: mu0
      REAL , DIMENSION(        kms:kme        ) , INTENT(IN)     :: eta
      REAL                                                       :: pdht
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pdry

      

      INTEGER :: i , j , k
      REAL , DIMENSION(        kms:kme        )                  :: eta_h

      IF ( full_levs ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pdry(i,k,j) = eta(k) * mu0(i,j) + pdht
               END DO
            END DO
         END DO

      ELSE
         DO k = kts , kte-1
            eta_h(k) = ( eta(k) + eta(k+1) ) * 0.5
         END DO

         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte-1
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  pdry(i,k,j) = eta_h(k) * mu0(i,j) + pdht
               END DO
            END DO
         END DO
      END IF

   END SUBROUTINE p_dry



   SUBROUTINE p_dts ( pdts , intq , psfc , p_top , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p_top
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)     :: psfc
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)     :: intq
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT)    :: pdts

      

      INTEGER :: i , j , k

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            pdts(i,j) = psfc(i,j) - intq(i,j) - p_top
         END DO
      END DO

   END SUBROUTINE p_dts



   SUBROUTINE p_dhs ( pdhs , ht , p0 , t0 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: ht
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: pdhs

      REAL , INTENT(IN) :: p0 , t0 , a

      

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            pdhs(i,j) = p0 * EXP ( -t0/a + SQRT ( (t0/a)**2 - 2. * g * ht(i,j)/(a * Rd) ) )
         END DO
      END DO

   END SUBROUTINE p_dhs



   SUBROUTINE find_p_top ( p , p_top , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

   
   
   
   
   
   
   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL :: p_top
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: p

      

      INTEGER :: i , j , k, min_lev

      i = its
      j = jts
      p_top = p(i,2,j)
      min_lev = 2
      DO k = 2 , kte
         IF ( p_top .GT. p(i,k,j) ) THEN
            p_top = p(i,k,j)
            min_lev = k
         END IF
      END DO

      k = min_lev
      p_top = p(its,k,jts)
      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            p_top = MAX ( p_top , p(i,k,j) )
         END DO
      END DO

   END SUBROUTINE find_p_top



   SUBROUTINE t_to_theta ( t , p , p00 , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               t(i,k,j) = t(i,k,j) * ( p00 / p(i,k,j) ) ** (Rd / Cp)
            END DO
         END DO
      END DO

   END SUBROUTINE t_to_theta




   SUBROUTINE theta_to_t ( t , p , p00 , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d
      CHARACTER (LEN=80) :: mess

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte-1
            DO i = its , MIN (ide-1 , ite )
             IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
             if ( p(i,k,j) .NE. 0. ) then
               t(i,k,j) = t(i,k,j) / ( ( p00 / p(i,k,j) ) ** (Rd / Cp) )
             else
               WRITE(mess,*) 'Troubles in theta_to_t'
               CALL wrf_debug(0,mess)
               WRITE(mess,*) "i,j,k = ", i,j,k
               CALL wrf_debug(0,mess)
               WRITE(mess,*) "p(i,k,j) = ", p(i,k,j)
               CALL wrf_debug(0,mess)
               WRITE(mess,*) "t(i,k,j) = ", t(i,k,j)
               CALL wrf_debug(0,mess)
             endif
            END DO
         END DO
      END DO

   END SUBROUTINE theta_to_t



   SUBROUTINE integ_moist ( q_in , p_in , pd_out , t_in , ght_in , intq , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   
   
   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: q_in , p_in , t_in , ght_in
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pd_out
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: intq

      

      INTEGER :: i , j , k
      INTEGER , DIMENSION(ims:ime) :: level_above_sfc
      REAL , DIMENSION(ims:ime,jms:jme) :: psfc , tsfc , qsfc, zsfc
      REAL , DIMENSION(ims:ime,kms:kme) :: q , p , t , ght, pd

      REAL :: rhobar , qbar , dz
      REAL :: p1 , p2 , t1 , t2 , q1 , q2 , z1, z2

      LOGICAL :: upside_down
      LOGICAL :: already_assigned_upside_down

      REAL , PARAMETER :: Rd = r_d

      


      already_assigned_upside_down = .FALSE.
      find_valid : DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            IF ( p_in(i,kts+1,j) .LT. p_in(i,kte,j) ) THEN
               upside_down = .TRUE.
               already_assigned_upside_down = .TRUE.
            ELSE
               upside_down = .FALSE.
               already_assigned_upside_down = .TRUE.
            END IF
            EXIT find_valid
         END DO
      END DO find_valid

      IF ( .NOT. already_assigned_upside_down ) THEN
         upside_down = .FALSE.
      END IF

      

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            psfc(i,j) = p_in(i,kts,j)
            tsfc(i,j) = t_in(i,kts,j)
            qsfc(i,j) = q_in(i,kts,j)
            zsfc(i,j) = ght_in(i,kts,j)
         END DO
      END DO

      DO j = jts , MIN ( jde-1 , jte )

         

         DO i = its , MIN (ide-1 , ite )
            intq(i,j) = 0.
         END DO

         IF ( upside_down ) THEN
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               p(i,kts) = p_in(i,kts,j)
               t(i,kts) = t_in(i,kts,j)
               q(i,kts) = q_in(i,kts,j)
               ght(i,kts) = ght_in(i,kts,j)
               DO k = kts+1,kte
                  p(i,k) = p_in(i,kte+2-k,j)
                  t(i,k) = t_in(i,kte+2-k,j)
                  q(i,k) = q_in(i,kte+2-k,j)
                  ght(i,k) = ght_in(i,kte+2-k,j)
               END DO
            END DO
         ELSE
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               DO k = kts,kte
                  p(i,k) = p_in(i,k      ,j)
                  t(i,k) = t_in(i,k      ,j)
                  q(i,k) = q_in(i,k      ,j)
                  ght(i,k) = ght_in(i,k      ,j)
               END DO
            END DO
         END IF

         
         

         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            level_above_sfc(i) = -1
            IF ( p(i,kts+1) .LT. psfc(i,j) ) THEN
               level_above_sfc(i) = kts+1
            ELSE
               find_k : DO k = kts+1,kte-1
                  IF ( ( p(i,k  )-psfc(i,j) .GE. 0. ) .AND. &
                       ( p(i,k+1)-psfc(i,j) .LT. 0. ) ) THEN
                     level_above_sfc(i) = k+1
                     EXIT find_k
                  END IF
               END DO find_k
               IF ( level_above_sfc(i) .EQ. -1 ) THEN
print *,'i,j = ',i,j
print *,'p = ',p(i,:)
print *,'p sfc = ',psfc(i,j)
                  CALL wrf_error_fatal3("<stdin>",5519,&
'Could not find level above ground')
               END IF
            END IF
         END DO

         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 

            

            pd(i,kte) = p(i,kte)
            DO k = kte-1,level_above_sfc(i),-1
                  rhobar = ( p(i,k  ) / ( Rd * t(i,k  ) ) + &
                             p(i,k+1) / ( Rd * t(i,k+1) ) ) * 0.5
                  qbar   = ( q(i,k  ) + q(i,k+1) ) * 0.5
                  dz     = ght(i,k+1) - ght(i,k)
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
                  pd(i,k) = p(i,k) - intq(i,j)
            END DO

            

            IF ( ( p(i,level_above_sfc(i)-1)-psfc(i,j) .GE. 0. ) .AND. &
                 ( p(i,level_above_sfc(i)  )-psfc(i,j) .LT. 0. ) .AND. &
                 ( level_above_sfc(i) .GT. kts ) ) THEN
               p1 = psfc(i,j)
               p2 = p(i,level_above_sfc(i))
               t1 = tsfc(i,j)
               t2 = t(i,level_above_sfc(i))
               q1 = qsfc(i,j)
               q2 = q(i,level_above_sfc(i))
               z1 = zsfc(i,j)
               z2 = ght(i,level_above_sfc(i))
               rhobar = ( p1 / ( Rd * t1 ) + &
                          p2 / ( Rd * t2 ) ) * 0.5
               qbar   = ( q1 + q2 ) * 0.5
               dz     = z2 - z1
               IF ( dz .GT. 0.1 ) THEN
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
               END IF

               

               DO k = level_above_sfc(i)-1,kts+1,-1
                  pd(i,k) = p(i,k) - intq(i,j)
               END DO
            END IF
            pd(i,kts) = psfc(i,j) - intq(i,j)

         END DO

         IF ( upside_down ) THEN
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               pd_out(i,kts,j) = pd(i,kts)
               DO k = kts+1,kte
                  pd_out(i,kte+2-k,j) = pd(i,k)
               END DO
            END DO
         ELSE
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               DO k = kts,kte
                  pd_out(i,k,j) = pd(i,k)
               END DO
            END DO
         END IF

      END DO

   END SUBROUTINE integ_moist



   SUBROUTINE rh_to_mxrat2(rh, t, p, q , wrt_liquid , &
                           qv_max_p_safe , &
                           qv_max_flag , qv_max_value , &
                           qv_min_p_safe , &
                           qv_min_flag , qv_min_value , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      
      
      
      
      
      
      

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

      REAL , INTENT(IN)           :: qv_max_p_safe , qv_max_flag , qv_max_value
      REAL , INTENT(IN)           :: qv_min_p_safe , qv_min_flag , qv_min_value

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: q

      

      REAL,         PARAMETER     :: T0K = 273.16
      REAL,         PARAMETER     :: Tice = T0K - 23.0

      REAL,         PARAMETER     :: cfe = 1.0/(23.0*23.0)
      REAL,         PARAMETER     :: eps = 0.622

      
      REAL,         PARAMETER     :: cw1 = 10.79574
      REAL,         PARAMETER     :: cw2 = -5.02800
      REAL,         PARAMETER     :: cw3 = 1.50475E-4
      REAL,         PARAMETER     :: cw4 = 0.42873E-3
      REAL,         PARAMETER     :: cw5 = 0.78614

      
      REAL,         PARAMETER     :: ci1 = -9.09685
      REAL,         PARAMETER     :: ci2 = -3.56654
      REAL,         PARAMETER     :: ci3 = 0.87682
      REAL,         PARAMETER     :: ci4 = 0.78614

      REAL,         PARAMETER     :: Tn = 273.16

      
      REAL,         PARAMETER     :: QV_MIN = 1.e-6

      
      
      REAL,         PARAMETER     :: QV_MAX = 0.045 

      
      
      
      REAL,         PARAMETER     :: EP_MAX = QV_MAX/(eps+QV_MAX)

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1
      REAL                        :: ta, tb, pw3, pw4, pwr
      REAL                        :: es, esw, esi, wvp, pmb, wvpmax

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,  0. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  Ta=Tn/T(i,k,j)
                  Tb=T(i,k,j)/Tn
                  pw3 = -8.2969*(Tb-1.0)
                  pw4 = 4.76955*(1.0-Ta)
                  pwr = cw1*(1.0-Ta) + cw2*LOG10(Tb) + cw3*(1.0-10.0**pw3) + cw4*(10.0**pw4-1.0) + cw5
                  es = 10.0**pwr             
                  wvp = 0.01*rh(i,k,j)*es    
                  pmb = p(i,k,j)/100.
                  wvpmax = EP_MAX*pmb        
                  wvp = MIN(wvp,wvpmax)
                  q(i,k,j) = eps*wvp/(pmb-wvp)
               END DO
            END DO
         END DO

      ELSE
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  Ta=Tn/T(i,k,j)
                  Tb=T(i,k,j)/Tn
                  IF (t(i,k,j) >= T0K) THEN         
                     pw3 = -8.2969*(Tb-1.0)
                     pw4 = 4.76955*(1.0-Ta)
                     pwr = cw1*(1.0-Ta) + cw2*LOG10(Tb) + cw3*(1.0-10.0**pw3) + cw4*(10.0**pw4-1.0) + cw5
                     es = 10.0**pwr
                     wvp = 0.01*rh(i,k,j)*es
                  ELSE IF (t(i,k,j) <= Tice) THEN   
                     pwr = ci1*(Ta-1.0) + ci2*LOG10(Ta) + ci3*(1.0-Tb) + ci4
                     es = 10.0**pwr
                     wvp = 0.01*rh(i,k,j)*es
                  ELSE                              
                     pw3 = -8.2969*(Tb-1.0)
                     pw4 = 4.76955*(1.0-Ta)
                     pwr = cw1*(1.0-Ta) + cw2*LOG10(Tb) + cw3*(1.0-10.0**pw3) + cw4*(10.0**pw4-1.0) + cw5
                     esw = 10.0**pwr      

                     pwr = ci1*(Ta-1.0) + ci2*LOG10(Ta) + ci3*(1.0-Tb) + ci4
                     esi = 10.0**pwr      

                     es = esi + (esw-esi)*cfe*(T(i,k,j)-Tice)*(T(i,k,j)-Tice)
                     wvp = 0.01*rh(i,k,j)*es
                  END IF
                  pmb = p(i,k,j)/100.
                  wvpmax = EP_MAX*pmb     
                  wvp = MIN(wvp,wvpmax)
                  q(i,k,j) = eps*wvp/(pmb-wvp)
               END DO
            END DO
         END DO
      END IF

      
      
      
      
      
     
      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( p(i,k,j) .LT. qv_max_p_safe ) THEN
                  IF ( q(i,k,j) .GT. qv_max_flag ) THEN
                     q(i,k,j) = qv_max_value
                  END IF
               END IF
               IF ( p(i,k,j) .LT. qv_min_p_safe ) THEN
                  IF ( q(i,k,j) .LT. qv_min_flag ) THEN
                     q(i,k,j) = qv_min_value
                  END IF
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE rh_to_mxrat2



   SUBROUTINE rh_to_mxrat1(rh, t, p, q , wrt_liquid , &
                           qv_max_p_safe , &
                           qv_max_flag , qv_max_value , &
                           qv_min_p_safe , &
                           qv_min_flag , qv_min_value , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

      REAL , INTENT(IN)           :: qv_max_p_safe , qv_max_flag , qv_max_value
      REAL , INTENT(IN)           :: qv_min_p_safe , qv_min_flag , qv_min_value

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: q

      

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      CHARACTER (LEN=80) :: mess

      
      
      
      

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,  0. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 


                  if (t(i,k,j) .ne. 0.) then
                     es=.01*rh(i,k,j)*svp1*10.*EXP(svp2*(t(i,k,j)-svpt0)/(t(i,k,j)-svp3))
                     IF (es .ge. p(i,k,j)/100.)THEN
                       q(i,k,j)=1.E-6
                       WRITE(mess,*) 'Warning: vapor pressure exceeds total pressure, setting Qv to 1.E-6'
                       CALL wrf_debug(1,mess)
                     ELSE
                       q(i,k,j)=MAX(eps*es/(p(i,k,j)/100.-es),1.E-6)
                     ENDIF
                  else
                     q(i,k,j)=1.E-6
                     WRITE(mess,*) 't(i,j,k) was 0 at ', i,j,k,', setting Qv to 0'
                     CALL wrf_debug(0,mess)
                  endif
               END DO
            END DO
         END DO

      ELSE
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 

                  t1 = t(i,k,j) - 273.16

                  

                  IF ( t1 .lt. -200. ) THEN
                     q(i,k,j) = 0

                  ELSE

                     

                     

                     IF ( t1 .GE. t_ref ) THEN
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 * a6)))))

                     

                     ELSE IF ( ( t1 .LT. t_ref ) .AND. ( t1 .GE. -47. ) ) THEN
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     

                     ELSE IF ( t1 .LT. -47. ) THEN
                        tk = t(i,k,j)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  &
                               c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     

                     ew = MAX ( ew , 0. ) * rh(i,k,j) * 0.01

                     
                     
                     
                     

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,k,j)/100. - ew))

                     q(i,k,j) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO
      END IF

      
      
      
      
      
     
      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF ( p(i,k,j) .LT. qv_max_p_safe ) THEN
                  IF ( q(i,k,j) .GT. qv_max_flag ) THEN
                     q(i,k,j) = qv_max_value
                  END IF
               END IF
               IF ( p(i,k,j) .LT. qv_min_p_safe ) THEN
                  IF ( q(i,k,j) .LT. qv_min_flag ) THEN
                     q(i,k,j) = qv_min_value
                  END IF
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE rh_to_mxrat1



   SUBROUTINE compute_eta ( znw , &
                           eta_levels , max_eta , max_dz , &
                           p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      
      
      

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      REAL , INTENT(IN)           :: max_dz
      REAL , INTENT(IN)           :: p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso
      INTEGER , INTENT(IN)        :: max_eta
      REAL , DIMENSION (max_eta) , INTENT(IN)  :: eta_levels

      REAL , DIMENSION (kts:kte) , INTENT(OUT) :: znw

      

      INTEGER :: k
      REAL :: mub , t_init , p_surf , pb, ztop, ztop_pbl , dz , temp
      REAL , DIMENSION(kts:kte) :: dnw

      INTEGER , PARAMETER :: prac_levels = 17
      INTEGER :: loop , loop1
      REAL , DIMENSION(prac_levels) :: znw_prac , znu_prac , dnw_prac
      REAL , DIMENSION(kts:kte) :: alb , phb

      

      IF ( ABS(eta_levels(1)+1.) .GT. 0.0000001 ) THEN

         

         IF      ( ( ABS(eta_levels(1  )-1.) .LT. 0.0000001 ) .AND. &
                   ( ABS(eta_levels(kde)-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
	       znw(k) = eta_levels(k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE IF ( ( ABS(eta_levels(kde)-1.) .LT. 0.0000001 ) .AND. &
                   ( ABS(eta_levels(1  )-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
	       znw(k) = eta_levels(kde+1-k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE
            CALL wrf_error_fatal3("<stdin>",6004,&
'First eta level should be 1.0 and the last 0.0 in namelist' )
         END IF

         

         DO k = kds , kde-1
            IF ( znw(k) .LE. znw(k+1) ) THEN
               PRINT *,'eta on full levels is not monotonic'
               PRINT *,'eta (',k,') = ',znw(k)
               PRINT *,'eta (',k+1,') = ',znw(k+1)
               CALL wrf_error_fatal3("<stdin>",6015,&
'Fix non-monotonic "eta_levels" in the namelist.input file' )
            END IF
         END DO

      

      ELSE

         
         
         
         

         p_surf = p00

         znw_prac = (/ 1.000 , 0.993 , 0.983 , 0.970 , 0.954 , 0.934 , 0.909 , &
                       0.88 , 0.8 , 0.7 , 0.6 , 0.5 , 0.4 , 0.3 , 0.2 , 0.1 , 0.0 /)

         DO k = 1 , prac_levels - 1
            znu_prac(k) = ( znw_prac(k) + znw_prac(k+1) ) * 0.5
            dnw_prac(k) = znw_prac(k+1) - znw_prac(k)
         END DO

         DO k = 1, prac_levels-1
            pb = znu_prac(k)*(p_surf - p_top) + p_top
            temp = MAX ( tiso, t00 + A*LOG(pb/p00) )

            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
         END DO

         

         mub = p_surf - p_top

         

         phb(1) = 0.
         DO k  = 2,prac_levels
               phb(k) = phb(k-1) - dnw_prac(k-1)*mub*alb(k-1)
         END DO

         
         

         ztop     = phb(prac_levels) / g
         ztop_pbl = phb(8          ) / g
         dz = ( ztop - ztop_pbl ) / REAL ( kde - 8 )

         

         DO k = 1 , 8
            znw(k) = znw_prac(k)
         END DO

         
         
         

         DO k = 8, kte-1-2
            pb = znw(k) * (p_surf - p_top) + p_top
            temp = MAX ( tiso, t00 + A*LOG(pb/p00) )

            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            znw(k+1) = znw(k) - dz*g / ( mub*alb(k) )
         END DO
         znw(kte-2) = 0.000

         
         
         
         
         

         DO loop1 = 1 , 5
            DO loop = 1 , 10
               DO k = 8, kte-1-2
                  pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
                  temp = MAX ( tiso, t00 + A*LOG(pb/p00) )

                  t_init = temp*(p00/pb)**(r_d/cp) - t0
                  alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
                  znw(k+1) = znw(k) - dz*g / ( mub*alb(k) )
               END DO
               IF ( ( loop1 .EQ. 5 ) .AND. ( loop .EQ. 10 ) ) THEN
                  print *,'Converged znw(kte) should be about 0.0 = ',znw(kte-2)
               END IF
               znw(kte-2) = 0.000
            END DO

            

            DO k = 1, kde-1-2
               pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
               temp = MAX ( tiso, t00 + A*LOG(pb/p00) )

               t_init = temp*(p00/pb)**(r_d/cp) - t0
               alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            END DO

            phb(1) = 0.
            DO k  = 2,kde-2
                  phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
            END DO

            

            ztop = phb(kde-2)/g
            ztop_pbl = phb(8)/g
            dz = ( ztop - ztop_pbl ) / REAL ( (kde-2) - 8 )
         END DO

         IF ( dz .GT. max_dz ) THEN
print *,'z (m)            = ',phb(1)/g
do k = 2 ,kte-2
print *,'z (m) and dz (m) = ',phb(k)/g,(phb(k)-phb(k-1))/g
end do
print *,'dz (m) above fixed eta levels = ',dz
print *,'namelist max_dz (m) = ',max_dz
print *,'namelist p_top (Pa) = ',p_top
            CALL wrf_debug ( 0, 'You need one of three things:' )
            CALL wrf_debug ( 0, '1) More eta levels to reduce the dz: e_vert' )
            CALL wrf_debug ( 0, '2) A lower p_top so your total height is reduced: p_top_requested')
            CALL wrf_debug ( 0, '3) Increase the maximum allowable eta thickness: max_dz')
            CALL wrf_debug ( 0, 'All are namelist options')
            CALL wrf_error_fatal3("<stdin>",6142,&
'dz above fixed eta levels is too large')
         END IF

         
         
         
         
         
         
         

         DO k = kte-2 , 9 , -1
            znw(k+2) = znw(k)
         END DO

         znw( 9) = 0.75 * znw( 8) + 0.25 * znw(12)
         znw(10) = 0.50 * znw( 8) + 0.50 * znw(12)
         znw(11) = 0.25 * znw( 8) + 0.75 * znw(12)

         DO k = 8, kte-1
            pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
            temp = MAX ( tiso, t00 + A*LOG(pb/p00) )
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
         END DO
         phb(kte) = phb(kte-1) - (znw(kte)-znw(kte-1)) * mub*alb(kte-1)

k=1
print *,k,'  z (m)            = ',phb(1)/g
do k = 2 ,kte
print *,k,'  z (m) and dz (m) = ',phb(k)/g,(phb(k)-phb(k-1))/g
end do

      END IF

   END SUBROUTINE compute_eta



   SUBROUTINE monthly_min_max ( field_in , field_min , field_max , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,12,jms:jme) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_min , field_max

      

      INTEGER :: i , j , l
      REAL :: minner , maxxer

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            minner = field_in(i,1,j)
            maxxer = field_in(i,1,j)
            DO l = 2 , 12
               IF ( field_in(i,l,j) .LT. minner ) THEN
                  minner = field_in(i,l,j)
               END IF
               IF ( field_in(i,l,j) .GT. maxxer ) THEN
                  maxxer = field_in(i,l,j)
               END IF
            END DO
            field_min(i,j) = minner
            field_max(i,j) = maxxer
         END DO
      END DO

   END SUBROUTINE monthly_min_max



   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   
   

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,12,jms:jme) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month , month1 , month2
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 12
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(l) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 31

      l = 13
      middle(l) = middle(12) + 31

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  int_month = l
                  IF ( ( int_month .EQ. 0 ) .OR. ( int_month .EQ. 12 ) ) THEN
                     month1 = 12
                     month2 =  1
                  ELSE
                     month1 = int_month
                     month2 = month1 + 1
                  END IF
                  field_out(i,j) =  ( field_in(i,month2,j) * ( target_date - middle(l)   ) + &
                                      field_in(i,month1,j) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month

   END SUBROUTINE monthly_interp_to_date



   SUBROUTINE sfcprs (t, q, height, pslv, ter, avgsfct, p, &
                      psfc, ez_method, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      
      
      
      

      IMPLICIT NONE

      REAL, PARAMETER    :: gamma     = 6.5E-3
      REAL, PARAMETER    :: pconst    = 10000.0
      REAL, PARAMETER    :: Rd        = r_d
      REAL, PARAMETER    :: TC        = svpt0 + 17.5

      REAL, PARAMETER    :: gammarg   = gamma * Rd / g
      REAL, PARAMETER    :: rov2      = Rd / 2.

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      LOGICAL , INTENT ( IN ) :: ez_method

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: pslv ,  ter, avgsfct
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k
      INTEGER , DIMENSION (its:ite,jts:jte) :: k500 , k700 , k850

      LOGICAL                     :: l1
      LOGICAL                     :: l2
      LOGICAL                     :: l3
      LOGICAL                     :: OK

      REAL                        :: gamma78     ( its:ite,jts:jte )
      REAL                        :: gamma57     ( its:ite,jts:jte )
      REAL                        :: ht          ( its:ite,jts:jte )
      REAL                        :: p1          ( its:ite,jts:jte )
      REAL                        :: t1          ( its:ite,jts:jte )
      REAL                        :: t500        ( its:ite,jts:jte )
      REAL                        :: t700        ( its:ite,jts:jte )
      REAL                        :: t850        ( its:ite,jts:jte )
      REAL                        :: tfixed      ( its:ite,jts:jte )
      REAL                        :: tsfc        ( its:ite,jts:jte )
      REAL                        :: tslv        ( its:ite,jts:jte )

      
      
      
      

      IF ( ez_method ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               psfc(i,j) = pslv(i,j) * ( 1.0 + gamma * ter(i,j) / avgsfct(i,j) ) ** ( - g / ( Rd * gamma ) )
            END DO
         END DO

      ELSE

         

         k850 = 0                              
         k700 = 0                              
         k500 = 0                              

         i = its
         j = jts
         DO k = kts+1 , kte
            IF      (NINT(p(i,k,j)) .EQ. 85000) THEN
               k850(i,j) = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 70000) THEN
               k700(i,j) = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 50000) THEN
               k500(i,j) = k
            END IF
         END DO

         IF ( ( k850(i,j) .EQ. 0 ) .OR. ( k700(i,j) .EQ. 0 ) .OR. ( k500(i,j) .EQ. 0 ) ) THEN

            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  psfc(i,j) = pslv(i,j) * ( 1.0 + gamma * ter(i,j) / t(i,1,j) ) ** ( - g / ( Rd * gamma ) )
               END DO
            END DO

            RETURN

         
         

         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
                  k850(i,j) = k850(its,jts)
                  k700(i,j) = k700(its,jts)
                  k500(i,j) = k500(its,jts)
               END DO
            END DO
         END IF

         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               ht(i,j) = height(i,k850(i,j),j)
            END DO
         END DO

         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               ht(i,j) = -ter(i,j) / ht(i,j)
            END DO
         END DO

         
         
         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               psfc(i,j) = pslv(i,j) * (pslv(i,j) / p(i,k850(i,j),j)) ** ht(i,j)
            END DO
         END DO

         
         
         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  p1(i,j) = 85000.
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0. ) THEN
                  p1(i,j) = psfc(i,j) - pconst
               ELSE
                  p1(i,j) = 50000.
               END IF
            END DO
         END DO

         
         
         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               t850(i,j) = t(i,k850(i,j),j) * (1. + 0.608 * q(i,k850(i,j),j))
               t700(i,j) = t(i,k700(i,j),j) * (1. + 0.608 * q(i,k700(i,j),j))
               t500(i,j) = t(i,k500(i,j),j) * (1. + 0.608 * q(i,k500(i,j),j))
            END DO
         END DO

         
         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               gamma78(i,j) = ALOG(t850(i,j) / t700(i,j))  / ALOG (p(i,k850(i,j),j) / p(i,k700(i,j),j) )
               gamma57(i,j) = ALOG(t700(i,j) / t500(i,j))  / ALOG (p(i,k700(i,j),j) / p(i,k500(i,j),j) )
            END DO
         END DO

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  t1(i,j) = t850(i,j)
               ELSE IF ( ( psfc(i,j) - 85000. ) .GE. 0. ) THEN
                  t1(i,j) = t700(i,j) * (p1(i,j) / (p(i,k700(i,j),j))) ** gamma78(i,j)
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0.) THEN
                  t1(i,j) = t500(i,j) * (p1(i,j) / (p(i,k500(i,j),j))) ** gamma57(i,j)
               ELSE
                  t1(i,j) = t500(i,j)
               ENDIF
            END DO
         END DO

         
         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               tslv(i,j) = t1(i,j) * (pslv(i,j) / p1(i,j)) ** gammarg
            END DO
         END DO

         
         
         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               tsfc(i,j) = tslv(i,j) - gamma * ter(i,j)
            END DO
         END DO

         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               tfixed(i,j) = tc - 0.005 * (tsfc(i,j) - tc) ** 2
            END DO
         END DO

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               l1 = tslv(i,j) .LT. tc
               l2 = tsfc(i,j) .LE. tc
               l3 = .NOT. l1
               IF      ( l2 .AND. l3 ) THEN
                  tslv(i,j) = tc
               ELSE IF ( ( .NOT. l2 ) .AND. l3 ) THEN
                  tslv(i,j) = tfixed(i,j)
               END IF
            END DO
         END DO

         

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
            p1(i,j) = - ter(i,j) * g / ( rov2 * ( tsfc(i,j) + tslv(i,j) ) )
            psfc(i,j) = pslv(i,j) * EXP ( p1(i,j) )
            END DO
         END DO

      END IF

      










   END SUBROUTINE sfcprs



   SUBROUTINE sfcprs2(t, q, height, psfc_in, ter, avgsfct, p, &
                      psfc, ez_method, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      
      
      
      

      IMPLICIT NONE

      REAL, PARAMETER    :: Rd        = r_d

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      LOGICAL , INTENT ( IN ) :: ez_method

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: psfc_in ,  ter, avgsfct
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k

      REAL :: tv_sfc_avg , tv_sfc , del_z

      
      

      
      


      IF ( ez_method ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               tv_sfc_avg = avgsfct(i,j) * (1. + 0.608 * q(i,1,j))
               del_z = height(i,1,j) - ter(i,j)
               psfc(i,j) = psfc_in(i,j) * EXP ( g * del_z / ( Rd * tv_sfc_avg ) )
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               tv_sfc = t(i,1,j) * (1. + 0.608 * q(i,1,j))
               del_z = height(i,1,j) - ter(i,j)
               psfc(i,j) = psfc_in(i,j) * EXP ( g * del_z / ( Rd * tv_sfc     ) )
            END DO
         END DO
      END IF

   END SUBROUTINE sfcprs2



   SUBROUTINE sfcprs3( height , p , ter , slp , psfc , &
                       ids , ide , jds , jde , kds , kde , &
                       ims , ime , jms , jme , kms , kme , &
                       its , ite , jts , jte , kts , kte )

      
      

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: ter , slp
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k

      LOGICAL                     :: found_loc

      REAL :: zl , zu , pl , pu , zm

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 

            

            IF      ( ter(i,j) .LT. 50 ) THEN
               psfc(i,j) = slp(i,j) + ( p(i,2,j)-p(i,3,j) ) / ( height(i,2,j)-height(i,3,j) ) * ter(i,j)
               CYCLE
            END IF

            

            found_loc = .FALSE.

            
            

            found_k_loc : DO k = kts+1 , kte-2
               IF ( ( height(i,k  ,j) .LE. ter(i,j) ) .AND. &
                    ( height(i,k+1,j) .GT. ter(i,j) ) ) THEN
                  zl = height(i,k  ,j)
                  zu = height(i,k+1,j)
                  zm = ter(i,j)
                  pl = p(i,k  ,j)
                  pu = p(i,k+1,j)
                  psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                  found_loc = .TRUE.
                  EXIT found_k_loc
               END IF
            END DO found_k_loc

            
            

            IF ( .NOT. found_loc ) THEN
               IF ( slp(i,j) .GE. p(i,2,j) ) THEN
                  zl = 0.
                  zu = height(i,3,j)
                  zm = ter(i,j)
                  pl = slp(i,j)
                  pu = p(i,3,j)
                  psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                  found_loc = .TRUE.
               ELSE
                  found_slp_loc : DO k = kts+1 , kte-3
                     IF ( ( slp(i,j) .GE. p(i,k+1,j) ) .AND. &
                          ( slp(i,j) .LT. p(i,k  ,j) ) ) THEN
                        zl = 0.
                        zu = height(i,k+1,j)
                        zm = ter(i,j)
                        pl = slp(i,j)
                        pu = p(i,k+1,j)
                        psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                        found_loc = .TRUE.
                        EXIT found_slp_loc
                     END IF
                  END DO found_slp_loc
               END IF
            END IF

            

            IF ( .NOT. found_loc ) THEN
               print *,'i,j = ',i,j
               print *,'p column = ',p(i,2:,j)
               print *,'z column = ',height(i,2:,j)
               print *,'model topo = ',ter(i,j)
               CALL wrf_error_fatal3("<stdin>",6721,&
' probs with sfc p computation ' )
            END IF

         END DO
      END DO

   END SUBROUTINE sfcprs3


   SUBROUTINE filter_topo ( ht_in , xlat , msftx , fft_filter_lat , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: fft_filter_lat
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: ht_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx


      

      INTEGER :: i , j , j_lat_pos , j_lat_neg
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: ht_out

      
      
      

      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal3("<stdin>",6759,&
'filtering assumes all values on X' )
      END IF

      
      
      
      

      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .LT. fft_filter_lat ) THEN
               j_lat_neg = j - 1
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg

      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_out(i,j) = ht_in(i,j)
         END DO
      END DO

      

      DO j = j_lat_neg , jts , -1
         i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
         print *,'j = ' , j, ', kicker = ',i_kicker
         DO i = its , MIN(ide-1,ite)
            IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j) + ht_in(i-ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j)
               END DO
               i1 = i - i_kicker + ide -1
               i2 = ide-1
               i3 = ids
               i4 = i-1
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i-ik,j)
               END DO
               i1 = i+1
               i2 = ide-1
               i3 = ids
               i4 = ids + ( i_kicker+i ) - ide
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            END IF
         END DO
      END DO

      

      DO j = j_lat_pos , MIN(jde-1,jte)
         i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
         print *,'j = ' , j, ', kicker = ',i_kicker
         DO i = its , MIN(ide-1,ite)
            IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j) + ht_in(i-ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j)
               END DO
               i1 = i - i_kicker + ide -1
               i2 = ide-1
               i3 = ids
               i4 = i-1
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i-ik,j)
               END DO
               i1 = i+1
               i2 = ide-1
               i3 = ids
               i4 = ids + ( i_kicker+i ) - ide
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            END IF
         END DO
      END DO

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_in(i,j) = ht_out(i,j)
         END DO
      END DO

   END SUBROUTINE filter_topo
















   subroutine dry_stratos ( theta, qv, phb, &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: theta, phb
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: qv

      

      INTEGER :: i, j, k, kk, istart, iend, jstart, jend, kstart, kend
      REAL    :: ht1, ht2, theta1, theta2, htz, sat85, p_std_atmos
      CHARACTER*256:: str_debug
      
      DATA sat85 /0.0235755574/

      do i = 1, 256
         str_debug(i:i) = char(0)
      enddo

      istart = its
      iend   = MIN(ide-1,ite)
      jstart = jts
      jend   = MIN(jde-1,jte)
      kstart = kts
      kend   = kte-1
      DO j = jstart, jend
         DO i = istart, iend
            DO k = kend-3, kstart, -1
               ht1 = phb(i,k,j)/9.8
               ht2 = phb(i,k+2,j)/9.8
               theta1 = theta(i,k,j)
               theta2 = theta(i,k+2,j)
               if ( (((theta2-theta1)/(ht2-ht1)) .lt. 10./1500. ) .AND. (ht1.gt.4000.) ) then
                  DO kk = k+3, kend
                     htz = phb(i,kk,j)/9.8
                     p_std_atmos = exp(log(1.0-htz/44307.692)/0.19)*101325.0
                     qv(i,kk,j) = 0.622*sat85/(p_std_atmos-sat85)
                  END DO
                  goto 79
               end if
            END DO
 79         continue
         END DO
      END DO

   END SUBROUTINE dry_stratos










      real function snowHires (snow_in, latitude, elev, date_str, i,j)
      IMPLICIT NONE

      REAL, INTENT(IN):: latitude, elev, snow_in
      INTEGER, INTENT(IN):: i, j
      CHARACTER (LEN=24), INTENT(IN) :: date_str

      REAL :: snow_startz, del_lat, season_factor, snow_out
      REAL :: gmt
      INTEGER :: day_peak, day_of_year, julyr
      CHARACTER (LEN=256) :: dbg_msg

      CALL get_julgmt ( date_str , julyr , day_of_year , gmt )

      if (latitude .gt. 0.0) then
         del_lat = (65.-latitude)/(65.-35.)
         day_peak = 80
      else
         del_lat = (-65.-latitude)/(-65.+35.)
         day_peak = 264
      endif

      snow_startz = (3900.-250.)*del_lat + 250.
      snow_startz = max(250., min(3900., snow_startz))

      season_factor = 1.
      snow_out = 0.
      IF (elev .GT. snow_startz) THEN
         season_factor = ABS(COS((day_of_year - day_peak)*0.5*0.0174533))
         snow_out = 0.999*(elev-snow_startz)/(4000.-snow_startz)
         write(dbg_msg,*) 'DEBUG_GT_SNOW ', day_of_year, latitude, elev, snow_in, snow_startz, season_factor, snow_out,i, j
         CALL wrf_debug (150, dbg_msg)
      ENDIF

      snowHires = MAX(snow_in, season_factor * snow_out)

      END FUNCTION snowHires




   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize



END MODULE module_initialize_real
