

   SUBROUTINE start_domain_em ( grid, allowed_to_read &

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/dummy_new_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &
!ENDOFREGISTRYGENERATEDINCLUDE

)

   USE module_domain, ONLY : domain, wrfu_timeinterval, get_ijk_from_grid, &
        domain_setgmtetc
   USE module_state_description
   USE module_model_constants
   USE module_bc, ONLY : boundary_condition_check, set_physical_bc2d
   USE module_bc_em
   USE module_configure, ONLY : grid_config_rec_type
   USE module_tiles, ONLY : set_tiles
   USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_max_real, wrf_dm_maxval, &
        ntasks_x, ntasks_y, &
        local_communicator_periodic, local_communicator, mytask, ntasks 
   USE module_comm_dm

   USE module_physics_init
   USE module_lightning_driver, ONLY : lightning_init
   USE module_fr_fire_driver_wrf, ONLY : fire_driver_em_init
   USE module_stoch, ONLY : SETUP_STOCH_SPPT,SETUP_STOCH_SKEBS,rand_seed, update_stoch
   USE module_diag_pld, ONLY : pld




   USE module_model_constants
   USE module_avgflx_em, ONLY : zero_avgflx

   IMPLICIT NONE
   
   TYPE (domain)          :: grid

   LOGICAL , INTENT(IN)   :: allowed_to_read

   
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/dummy_new_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
!ENDOFREGISTRYGENERATEDINCLUDE

   
   TYPE (grid_config_rec_type)              :: config_flags

   
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  ips, ipe, jps, jpe, kps, kpe, &
                                  its, ite, jts, jte, kts, kte, &
                                  ij,i,j,k,ii,jj,kk,loop,error,l

   INTEGER ::              imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

   INTEGER     :: i_m

   REAL        :: p00, t00, a, tiso, p_surf, pd_surf, temp, tiso_tmp

   REAL :: qvf1, qvf2, qvf
   REAL :: pfu, pfd, phm
   REAL :: MPDT
   REAL :: spongeweight
   LOGICAL :: first_trip_for_this_domain, start_of_simulation, fill_w_flag
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: cldfra_old

   REAL :: lat1 , lat2 , lat3 , lat4
   REAL :: lon1 , lon2 , lon3 , lon4
   INTEGER :: num_points_lat_lon , iloc , jloc
   CHARACTER (LEN=132) :: message
   TYPE(WRFU_TimeInterval) :: stepTime
   REAL, DIMENSION(:,:), ALLOCATABLE :: clat_glob
   logical :: f_flux  

   INTEGER :: idex, jdex
   INTEGER :: im1,ip1,jm1,jp1
   REAL :: hx,hy,pi

   REAL :: w_max, w_min
   LOGICAL :: w_needs_to_be_set


   REAL    :: alpha, vfac


   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: z_at_q

   CALL get_ijk_from_grid ( grid ,                              &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe,        &
                           imsx, imex, jmsx, jmex, kmsx, kmex,  &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                           imsy, imey, jmsy, jmey, kmsy, kmey,  &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )

   kts = kps ; kte = kpe     
   its = ips ; ite = ipe     
   jts = jps ; jte = jpe    
         ALLOCATE(CLDFRA_OLD(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_OLD = 0.
   ALLOCATE(z_at_q(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; z_at_q = 0.
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   IF ( ( MOD (ide-ids,config_flags%parent_grid_ratio) .NE. 0 ) .OR. &
        ( MOD (jde-jds,config_flags%parent_grid_ratio) .NE. 0 ) ) THEN
      WRITE(message, FMT='(A,I2,":  Both MOD(",I4,"-",I1,",",I2,") and MOD(",I4,"-",I1,",",I2,") must = 0" )') &
         "Nested dimensions are illegal for domain ",grid%id,ide,ids,config_flags%parent_grid_ratio,&
         jde,jds,config_flags%parent_grid_ratio
      CALL wrf_error_fatal3("start_em.b",136,&
message )
   END IF

   IF ( config_flags%polar ) THEN






     ALLOCATE( clat_glob(ids:ide,jds:jde) )

     CALL wrf_patch_to_global_real ( grid%clat, clat_glob, grid%domdesc, 'xy', 'xy', &
                                     ids, ide, jds, jde, 1, 1, &
                                     ims, ime, jms, jme, 1, 1, &
                                     its, ite, jts, jte, 1, 1 )

     CALL wrf_dm_bcast_real ( clat_glob , (ide-ids+1)*(jde-jds+1) )

     grid%clat_xxx(ipsx:ipex,jpsx:jpex) = clat_glob(ipsx:ipex,jpsx:jpex)

     DEALLOCATE( clat_glob )
   ENDIF



   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )




   

   IF ( grid%itimestep .EQ. 0 ) THEN
      first_trip_for_this_domain = .TRUE.
   ELSE
      first_trip_for_this_domain = .FALSE.
   END IF

   IF ( .not. ( config_flags%restart .or. grid%moved ) ) THEN
       grid%itimestep=0
   ENDIF

   IF ( config_flags%restart .or. grid%moved ) THEN
       first_trip_for_this_domain = .TRUE.
   ENDIF



   IF ( first_trip_for_this_domain ) THEN
     grid%did_stoch = .FALSE.
   END IF

   IF ( ( grid%id == 1 ) .AND. &
        ( grid%stoch_force_global_opt == 1 ) .AND. &
        ( .NOT. grid%did_stoch ) ) THEN

     grid%did_stoch = .TRUE.
     IF ( wrf_dm_on_monitor () ) THEN
       CALL rand_seed ( config_flags, grid%SEED1, grid%SEED2, grid%NENS )
     ENDIF
     CALL wrf_dm_bcast_bytes ( grid%SEED1, 4 )
     CALL wrf_dm_bcast_bytes ( grid%SEED2, 4 )

     IF (grid%stoch_force_opt==1) then
     grid%SPTFORCC=0.0
     grid%SPTFORCS=0.0
     call SETUP_STOCH_SKEBS(grid%VERTSTRUCC,grid%VERTSTRUCS,                   &
                      grid%SPT_AMP,grid%SPSTREAM_AMP,                    &
                      grid%VERTAMPT,grid%VERTAMPUV,                      &
                      grid%stoch_vertstruc_opt,                          &
                      grid%SEED1,grid%SEED2,grid%time_step,              &
                      grid%DX,grid%DY,                                   &
                      grid%TOT_BACKSCAT_PSI,grid%TOT_BACKSCAT_T,         &
                      grid%ZTAU_PSI,grid%ZTAU_T,grid%REXPONENT_PSI,grid%REXPONENT_T,               &
                      grid%KMINFORC,grid%KMAXFORC,grid%LMINFORC,grid%LMAXFORC,     &
                      grid%KMINFORCT,grid%KMAXFORCT,grid%LMINFORCT,grid%LMAXFORCT, &
                      grid%KMAXFORCH,grid%LMAXFORCH,grid%KMAXFORCTH,grid%LMAXFORCTH, &
                      grid%ZSIGMA2_EPS,grid%ZSIGMA2_ETA,                          &
                      ids, ide, jds, jde, kds, kde,                      &
                      ims, ime, jms, jme, kms, kme,                      &
                      its, ite, jts, jte, kts, kte                       )
     END IF
     IF (grid%stoch_force_opt==2) then

        call SETUP_STOCH_SPPT(grid%VERTSTRUCC,grid%VERTSTRUCS,            &
                       grid%SPT_AMP,                                      &
                       grid%SPTFORCC,grid%SPTFORCS,                       &
                       grid%VERTAMPT,grid%VERTAMPUV,                      &
                       grid%stoch_vertstruc_opt,                          &
                       grid%SEED1,grid%SEED2,grid%time_step,              &
                       grid%DX,grid%DY,                                   &
                       grid%gridpointvariance,grid%l_sppt,grid%tau_sppt,  &
                       grid%KMINFORCT,grid%KMAXFORCT,grid%LMINFORCT,grid%LMAXFORCT, &
                       grid%KMAXFORCTH,grid%LMAXFORCTH, &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )
     ENDIF 
     do i=1,600
     CALL UPDATE_STOCH(grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,           &
                      grid%SPTFORCS,grid%SPTFORCC,                        &
                      grid%SPT_AMP,grid%SPSTREAM_AMP,                     &
                      ids, ide, jds, jde, kds, kde,                       &
                      ims, ime, jms, jme, kms, kme,                       &
                      its, ite, jts, jte, kts, kte                       )
     enddo


    END IF









   CALL lbc_fcx_gcx ( grid%fcx , grid%gcx , grid%spec_bdy_width , &
                      grid%spec_zone , grid%relax_zone , grid%dt , config_flags%spec_exp , &
                      config_flags%specified , config_flags%nested )

   IF ( config_flags%nested ) THEN
     grid%dtbc = 0.
   ENDIF

   IF ( ( grid%id .NE. 1 ) .AND. ( .NOT. config_flags%input_from_file ) ) THEN

      
      
      
      
      
      
      
      
      
      

      IF      ( ( MOD(ide,2) .EQ. 0 ) .AND. ( MOD(jde,2) .EQ. 0 ) ) THEN
         num_points_lat_lon = 1
         iloc = ide/2
         jloc = jde/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )
         CALL nl_set_cen_lat ( grid%id , lat1 )
         CALL nl_set_cen_lon ( grid%id , lon1 )
      ELSE IF ( ( MOD(ide,2) .NE. 0 ) .AND. ( MOD(jde,2) .EQ. 0 ) ) THEN
         num_points_lat_lon = 2
         iloc = (ide-1)/2
         jloc =  jde   /2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )

         iloc = (ide+1)/2
         jloc =  jde   /2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = grid%xlat (iloc,jloc)
            lon2 = grid%xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )

         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 ) * 0.50 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 ) * 0.50 )
      ELSE IF ( ( MOD(ide,2) .EQ. 0 ) .AND. ( MOD(jde,2) .NE. 0 ) ) THEN
         num_points_lat_lon = 2
         iloc =  ide   /2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )

         iloc =  ide   /2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = grid%xlat (iloc,jloc)
            lon2 = grid%xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )

         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 ) * 0.50 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 ) * 0.50 )
      ELSE IF ( ( MOD(ide,2) .NE. 0 ) .AND. ( MOD(jde,2) .NE. 0 ) ) THEN
         num_points_lat_lon = 4
         iloc = (ide-1)/2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )

         iloc = (ide+1)/2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = grid%xlat (iloc,jloc)
            lon2 = grid%xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )

         iloc = (ide-1)/2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat3 = grid%xlat (iloc,jloc)
            lon3 = grid%xlong(iloc,jloc)
         ELSE
            lat3 = 99999.
            lon3 = 99999.
         END IF
         lat3 = wrf_dm_min_real ( lat3 )
         lon3 = wrf_dm_min_real ( lon3 )

         iloc = (ide+1)/2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat4 = grid%xlat (iloc,jloc)
            lon4 = grid%xlong(iloc,jloc)
         ELSE
            lat4 = 99999.
            lon4 = 99999.
         END IF
         lat4 = wrf_dm_min_real ( lat4 )
         lon4 = wrf_dm_min_real ( lon4 )

         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 + lat3 + lat4 ) * 0.25 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 + lon3 + lon4 ) * 0.25 )
      END IF
   END IF

   IF ( config_flags%use_baseparam_fr_nml ) then
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
      CALL nl_get_iso_temp   ( 1 , tiso )
      IF ( ( t00 .LT. 100. .or. p00 .LT. 10000.) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
         WRITE(wrf_err_message,*) 'start_em: BAD BASE STATE for T00 or P00 in namelist.input file'
         CALL wrf_error_fatal3("start_em.b",423,&
TRIM(wrf_err_message))
      END IF

   ELSE
   

      t00  = grid%t00
      p00  = grid%p00
      a    = grid%tlp
      tiso = grid%tiso
      IF ( ( t00 .LT. 100. .or. p00 .LT. 10000.) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
         WRITE(wrf_err_message,*)&
         'start_em: did not find base state parameters in wrfinput. Add use_baseparam_fr_nml = .t. in &dynamics and rerun'
         CALL wrf_error_fatal3("start_em.b",437,&
TRIM(wrf_err_message))
      ENDIF

   ENDIF



   CALL nl_get_iso_temp   ( 1 , tiso_tmp )
   IF ( ( tiso_tmp .NE. tiso ) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
      WRITE(wrf_err_message,*)&
      'start_em: namelist iso_temp is not equal to iso_temp in wrfinput. Reset nml value and rerun'
      CALL wrf_error_fatal3("start_em.b",449,&
TRIM(wrf_err_message))
   ENDIF

   IF ( .NOT. config_flags%restart .AND. &
        (( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ))) THEN

      IF ( config_flags%map_proj .EQ. 0 ) THEN
         CALL wrf_error_fatal3("start_em.b",457,&
'start_domain: Idealized case cannot have a separate nested input file' )
      END IF

      
      
      

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            
            
            

            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )

            DO k = 1, kte-1
               grid%pb(i,k,j) = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0

               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO

            

            grid%mub(i,j) = p_surf - grid%p_top

            
            
            

            grid%phb(i,1,j) = grid%ht(i,j) * g

            IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
               DO k  = 2,kte
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
               END DO
            ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
               DO k  = 2,kte
                  pfu = grid%mub(i,j)*grid%znw(k)   + grid%p_top
                  pfd = grid%mub(i,j)*grid%znw(k-1) + grid%p_top
                  phm = grid%mub(i,j)*grid%znu(k-1) + grid%p_top
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO
            END IF
         END DO
      END DO

   ENDIF

   IF(.not.config_flags%restart)THEN



     IF ( first_trip_for_this_domain ) THEN




       DO j = jts,min(jte,jde-1)
       DO k = kts,kte-1
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%t_1(i,k,j)=grid%t_2(i,k,j)
           ENDIF
       ENDDO
       ENDDO
       ENDDO

       DO j = jts,min(jte,jde-1)
       DO k = kts,kte
       DO i = its, min(ite,ide-1)
          grid%ph_1(i,k,j)=grid%ph_2(i,k,j)
       ENDDO
       ENDDO
       ENDDO

       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%mu_1(i,j)=grid%mu_2(i,j)
           ENDIF
       ENDDO
       ENDDO
     END IF



    IF(config_flags%max_dom .EQ. 1)THEN

     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its, min(ite,ide-1)
       IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
         grid%pb(i,k,j) = grid%znu(k)*grid%mub(i,j)+grid%p_top
         grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
       ENDIF
     ENDDO
     ENDDO
     ENDDO
    ELSE

     IF ( .NOT. grid%this_is_an_ideal_run ) THEN
        DO j = jts,min(jte,jde-1)
        DO k = kts,kte-1
        DO i = its, min(ite,ide-1)
          IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
            grid%pb(i,k,j) = grid%znu(k)*grid%mub(i,j)+grid%p_top
            temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
            grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
            grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
          ENDIF
        ENDDO
        ENDDO
        ENDDO
     ELSE
        DO j = jts,min(jte,jde-1)
        DO k = kts,kte-1
        DO i = its, min(ite,ide-1)
          IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
            grid%pb(i,k,j) = grid%znu(k)*grid%mub(i,j)+grid%p_top
            grid%alb(i,k,j) = -grid%rdnw(k)*(grid%phb(i,k+1,j)-grid%phb(i,k,j))/grid%mub(i,j)
            grid%t_init(i,k,j) = grid%alb(i,k,j)*(p1000mb/r_d)/((grid%pb(i,k,j)/p1000mb)**cvpm) - t0
          ENDIF
        ENDDO
        ENDDO
        ENDDO
     END IF
    ENDIF



    IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
       DO j=jts,min(jte,jde-1)
       DO k=kts,kte-1
       DO i=its,min(ite,ide-1)
          grid%al(i,k,j)=-1./(grid%mub(i,j)+grid%mu_1(i,j))*(grid%alb(i,k,j)*grid%mu_1(i,j)  &
                     +grid%rdnw(k)*(grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)))
       ENDDO
       ENDDO
       ENDDO
    ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
       DO j=jts,min(jte,jde-1)
       DO k=kts,kte-1
       DO i=its,min(ite,ide-1)
          pfu = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znw(k+1)+grid%p_top
          pfd = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znw(k)  +grid%p_top
          phm = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znu(k)  +grid%p_top
          grid%al(i,k,j) = (grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)+grid%phb(i,k+1,j)-grid%phb(i,k,j)) &
                            /phm/LOG(pfd/pfu)-grid%alb(i,k,j)
       ENDDO
       ENDDO
       ENDDO
    END IF
    DO j=jts,min(jte,jde-1)
    DO k=kts,kte-1
    DO i=its,min(ite,ide-1)
       qvf = 1.+rvovrd*moist(i,k,j,P_QV)
       grid%p(i,k,j)=p1000mb*( (r_d*(t0+grid%t_1(i,k,j))*qvf)/                     &
                  (p1000mb*(grid%al(i,k,j)+grid%alb(i,k,j))) )**cpovcv  &
                  -grid%pb(i,k,j)
       grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
       grid%alt(i,k,j) = grid%al(i,k,j) + grid%alb(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    IF ( .NOT. grid%this_is_an_ideal_run ) THEN
       DO j=jts,min(jte,jde-1)
          DO i=its,min(ite,ide-1)
             p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
             grid%p_hyd_w(i,1,j) = grid%p(i,1,j) + grid%znw(1)*(p_surf - grid%p_top) + grid%p_top
             DO k=kts+1,kte
                grid%p_hyd_w(i,k,j) = ( 2.*(grid%p(i,k-1,j)+grid%pb(i,k-1,j)) - grid%p_hyd_w(i,k-1,j) )
             ENDDO
          ENDDO
       ENDDO
    ELSE
       DO j=jts,min(jte,jde-1)
          DO i=its,min(ite,ide-1)
             p_surf = grid%mub(i,j)+grid%p_top
             grid%p_hyd_w(i,1,j) = grid%p(i,1,j) + grid%znw(1)*(p_surf - grid%p_top) + grid%p_top
             DO k=kts+1,kte
                grid%p_hyd_w(i,k,j) = ( 2.*(grid%p(i,k-1,j)+grid%pb(i,k-1,j)) - grid%p_hyd_w(i,k-1,j) )
             ENDDO
          ENDDO
       ENDDO
    END IF

   ENDIF

   DO j=jts,min(jte,jde-1)
      DO k = kts,kte
         DO i = its, min(ite,ide-1)
            z_at_q(i,k,j)=(grid%ph_2(i,k,j)+grid%phb(i,k,j))/g
         ENDDO
      ENDDO
   ENDDO

   IF ( grid%press_adj .and. ( grid%id .NE. 1 ) .AND. .NOT. ( config_flags%restart ) .AND. &
       ( ( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ) ) ) THEN
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%mu_2(i,j) = grid%mu_2(i,j) + grid%al(i,1,j) / ( grid%alt(i,1,j) * grid%alb(i,1,j) ) * &
                                    g * ( grid%ht(i,j) - grid%ht_fine(i,j) )
         END DO
       END DO
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          grid%mu_1(i,j)=grid%mu_2(i,j)
       ENDDO
       ENDDO

   END IF




   CALL domain_setgmtetc( grid, start_of_simulation )
   IF ( first_trip_for_this_domain ) THEN

   CALL wrf_debug ( 100 , 'start_domain_em: Before call to phy_init' )



   MPDT = 0.


   IF(config_flags%cycling) start_of_simulation = .true.







   IF ( ( grid%use_adaptive_time_step ) .AND. &
        ( ( grid%dfi_opt .EQ. DFI_NODFI ) .OR. ( grid%dfi_stage .EQ. DFI_FST ) ) ) THEN



      if (grid%starting_time_step == -1) then
         grid%starting_time_step = NINT(6 * MIN(grid%dx,grid%dy) / 1000)
      endif

      if (grid%max_time_step == -1) then
         grid%max_time_step = 3*grid%starting_time_step
      endif

      if (grid%min_time_step == -1) then
         grid%min_time_step = 0.5*grid%starting_time_step
      endif



      grid%dt = grid%starting_time_step


      
      grid%last_max_vert_cfl = 0
      grid%last_max_horiz_cfl = 0



      CALL nl_set_time_step_sound ( 1 , 0 )
      grid%time_step_sound = 0

      grid%max_msftx=MAXVAL(grid%msftx)
      grid%max_msfty=MAXVAL(grid%msfty)
      CALL wrf_dm_maxval(grid%max_msftx, idex, jdex)
      CALL wrf_dm_maxval(grid%max_msfty, idex, jdex)




      IF ( .NOT. ( config_flags%restart ) ) then
         CALL adapt_timestep(grid, config_flags)
      END IF


   END IF





   CALL set_tiles ( grid , grid%imask_nostag, ims, ime, jms, jme, ips, ipe, jps, jpe )







   if ( allowed_to_read ) grid%num_tiles = max(1,grid%num_tiles)





if(config_flags%sf_surface_physics.eq.NOAHMPSCHEME.and.config_flags%opt_run.eq.5)then
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_HYDRO_NOAHMP_INIT.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_HYDRO_NOAHMP_INIT_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
endif

   DO ij = 1, grid%num_tiles


    IF ( ( grid%dfi_opt .EQ. DFI_NODFI ) .or. &
          ( ( grid%dfi_stage .NE. DFI_BCK ) .and. &
          ( grid%dfi_stage .NE. DFI_STARTBCK ) ) ) THEN    

     CALL phy_init (  grid%id , config_flags, grid%DT, grid%RESTART, grid%znw, grid%znu,   &
                      grid%p_top, grid%tsk, grid%RADT,grid%BLDT,grid%CUDT, MPDT, &
                      grid%rucuten, grid%rvcuten, grid%rthcuten,             &
                      grid%rqvcuten, grid%rqrcuten, grid%rqccuten,           &
                      grid%rqscuten, grid%rqicuten,                          &
                      grid%rushten, grid%rvshten, grid%rthshten,             &
                      grid%rqvshten, grid%rqrshten, grid%rqcshten,           &
                      grid%rqsshten, grid%rqishten, grid%rqgshten,           &
                      grid%rublten,grid%rvblten,grid%rthblten,               &
                      grid%rqvblten,grid%rqcblten,grid%rqiblten,             &
                      grid%rthraten,grid%rthratenlw,grid%rthratensw,         &
                      grid%stepbl,grid%stepra,grid%stepcu,                   &
                      grid%w0avg, grid%rainnc, grid%rainc, grid%raincv, grid%rainncv,  &
                      grid%snownc, grid%snowncv, grid%graupelnc, grid%graupelncv,  &
                      z_at_q, grid%qnwfa2d, scalar(ims,kms,jms,1), num_scalar,         & 
                      grid%re_cloud, grid%re_ice, grid%re_snow,         & 
                      grid%has_reqc, grid%has_reqi, grid%has_reqs,      & 
                      grid%nca,grid%swrad_scat,                    &
                      grid%cldefi,grid%lowlyr,                          &
                      grid%mass_flux,                              &
                      grid%rthften, grid%rqvften,                       &
                      grid%cldfra,                                      &
                      cldfra_old,                                  &
                      grid%glw,grid%gsw,grid%emiss,grid%embck,            &
                      grid%lu_index,                                      &
                      grid%landuse_ISICE, grid%landuse_LUCATS,            &
                      grid%landuse_LUSEAS, grid%landuse_ISN,              &
                      grid%lu_state,                                      &
                      grid%xlat,grid%xlong,grid%xlong_u,grid%xlat_v,grid%albedo,grid%albbck,grid%GMT,grid%JULYR,grid%JULDAY,     &
                      grid%levsiz, num_ozmixm, num_aerosolc, grid%paerlev,  &
                      grid%alevsiz, grid%no_src_types,                      &
                      grid%tmn,grid%xland,grid%znt,grid%z0,grid%ust,grid%mol,grid%pblh,grid%tke_pbl,    &
                      grid%exch_h,grid%thc,grid%snowc,grid%mavail,grid%hfx,grid%qfx,grid%rainbl, &
                      grid%tslb,grid%zs,grid%dzs,config_flags%num_soil_layers,grid%warm_rain,  &
                      grid%adv_moist_cond, grid%is_CAMMGMP_used,                               &
                      grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,      &
                      grid%apr_capma,grid%apr_capme,grid%apr_capmi,          &
                      grid%xice,grid%xicem,grid%vegfra,grid%snow,grid%canwat,grid%smstav,         &
                      grid%smstot, grid%sfcrunoff,grid%udrunoff,grid%grdflx,grid%acsnow,      &
                      grid%acsnom,grid%ivgtyp,grid%isltyp, grid%sfcevp,grid%smois,     &
                      grid%sh2o, grid%snowh, grid%smfr3d,                    &
                      grid%snoalb,                 &
                      grid%DX,grid%DY,grid%f_ice_phy,grid%f_rain_phy,grid%f_rimef_phy, &
                      grid%mp_restart_state,grid%tbpvs_state,grid%tbpvs0_state,&
                      allowed_to_read, grid%moved, start_of_simulation,               &
                      grid%LAGDAY, &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), kts, kte, &
                      config_flags%num_urban_layers,                        & 
                      config_flags%num_urban_hi,                            & 
                      grid%raincv_a,grid%raincv_b,                                    &
                      grid%gd_cloud, grid%gd_cloud2,                                  & 
                      grid%gd_cloud_a, grid%gd_cloud2_a,                              & 
                      grid%QC_CU, grid%QI_CU,                                         & 
                      ozmixm,grid%pin,                             &     
                      grid%aerodm,grid%pina,                       &     
                      grid%m_ps_1,grid%m_ps_2,grid%m_hybi,aerosolc_1,aerosolc_2,&  
                      grid%rundgdten,grid%rvndgdten,grid%rthndgdten,         &     
                      grid%rphndgdten,grid%rqvndgdten,grid%rmundgdten,       &     
                      grid%FGDT,grid%stepfg,                        &     
                      grid%cugd_tten,grid%cugd_ttens,grid%cugd_qvten,        &   
                      grid%cugd_qvtens,grid%cugd_qcten,                 &   
                      grid%ISNOWXY, grid%ZSNSOXY, grid%TSNOXY,                                  &   
                      grid%SNICEXY, grid%SNLIQXY, grid%TVXY, grid%TGXY, grid%CANICEXY,          &   
                      grid%CANLIQXY, grid%EAHXY, grid%TAHXY, grid%CMXY,                         &   
                      grid%CHXY, grid%FWETXY, grid%SNEQVOXY, grid%ALBOLDXY, grid%QSNOWXY,       &   
                      grid%WSLAKEXY, grid%ZWTXY, grid%WAXY, grid%WTXY, grid%LFMASSXY, grid%RTMASSXY, & 
                      grid%STMASSXY, grid%WOODXY, grid%STBLCPXY, grid%FASTCPXY,                 &   
                      grid%XSAIXY,                                                              &   
                      grid%T2MVXY, grid%T2MBXY, grid%CHSTARXY,                                  &   
                      grid%SMOISEQ  ,grid%SMCWTDXY ,grid%RECHXY, grid%DEEPRECHXY, grid%AREAXY,  & 
                      config_flags%wtddt ,grid%stepwtd ,grid%QRFSXY ,grid%QSPRINGSXY ,grid%QSLATXY, & 
                      grid%FDEPTHXY, grid%RIVERBEDXY, grid%EQZWT, grid%RIVERCONDXY, grid%PEXPXY, & 
                      grid%msftx, grid%msfty,                              &
                      grid%DZR, grid%DZB, grid%DZG,                          & 
                      grid%TR_URB2D,grid%TB_URB2D,grid%TG_URB2D,grid%TC_URB2D,    & 
                      grid%QC_URB2D, grid%XXXR_URB2D,grid%XXXB_URB2D,        & 
                      grid%XXXG_URB2D, grid%XXXC_URB2D,                 & 
                      grid%TRL_URB3D, grid%TBL_URB3D, grid%TGL_URB3D,        & 
                      grid%SH_URB2D, grid%LH_URB2D, grid%G_URB2D, grid%RN_URB2D,  & 
                      grid%TS_URB2D, grid%FRC_URB2D, grid%UTYPE_URB2D,      & 
                      grid%TRB_URB4D,grid%TW1_URB4D,grid%TW2_URB4D,grid%TGB_URB4D,grid%TLEV_URB3D,  & 
                      grid%QLEV_URB3D,grid%TW1LEV_URB3D,grid%TW2LEV_URB3D,        & 
                      grid%TGLEV_URB3D,grid%TFLEV_URB3D,grid%SF_AC_URB3D,         & 
                      grid%LF_AC_URB3D,grid%CM_AC_URB3D,grid%SFVENT_URB3D,grid%LFVENT_URB3D,   & 
                      grid%SFWIN1_URB3D,grid%SFWIN2_URB3D, & 
                      grid%SFW1_URB3D,grid%SFW2_URB3D,grid%SFR_URB3D,grid%SFG_URB3D,     & 
                      grid%LP_URB2D,grid%HI_URB2D,grid%LB_URB2D,grid%HGT_URB2D,        & 
                      grid%MH_URB2D,grid%STDH_URB2D,grid%LF_URB2D,                     & 
                      grid%A_U_BEP,grid%A_V_BEP,grid%A_T_BEP,grid%A_Q_BEP,             & 
                      grid%A_E_BEP,grid%B_U_BEP,grid%B_V_BEP,grid%B_T_BEP,             & 
                      grid%B_Q_BEP,grid%B_E_BEP,grid%DLG_BEP,                          & 
                      grid%DL_U_BEP,grid%SF_BEP,grid%VL_BEP,                           & 
                      grid%TML,grid%T0ML,grid%HML,grid%H0ML,grid%HUML,grid%HVML,grid%TMOML,     & 
                      grid%lakedepth2d,  grid%savedtke12d,  grid%snowdp2d,   grid%h2osno2d,       & 
                      grid%snl2d,        grid%t_grnd2d,     grid%t_lake3d,   grid%lake_icefrac3d, & 
                      grid%z_lake3d,     grid%dz_lake3d,    grid%t_soisno3d, grid%h2osoi_ice3d,   & 
                      grid%h2osoi_liq3d, grid%h2osoi_vol3d, grid%z3d,        grid%dz3d,           & 
                      grid%zi3d,         grid%watsat3d,     grid%csol3d,     grid%tkmg3d,         & 
                      grid%tkdry3d,      grid%tksatu3d,     grid%lake2d,                            & 
                      config_flags%lakedepth_default,        config_flags%lake_min_elev, grid%lake_depth,     & 
                      grid%lakemask,        grid%lakeflag,  grid%LAKE_DEPTH_FLAG, grid%use_lakedepth,     & 
                      config_flags%sf_surface_mosaic, config_flags%mosaic_cat, config_flags%num_land_cat, & 
                      config_flags%maxpatch,           &   
              grid%numc,grid%nump,grid%snl,grid%snowdp,&   
              grid%wtc,grid%wtp,&
              grid%h2osno,grid%t_grnd,grid%t_veg,grid%h2ocan, &
              grid%h2ocan_col,grid%t2m_max,grid%t2m_min,&
              grid%t_ref2m,&
              grid%h2osoi_liq_s1,&
              grid%h2osoi_liq_s2,grid%h2osoi_liq_s3,&
              grid%h2osoi_liq_s4, &
              grid%h2osoi_liq_s5, &
              grid%h2osoi_liq1,grid%h2osoi_liq2,&
              grid%h2osoi_liq3,grid%h2osoi_liq4,&
              grid%h2osoi_liq5,grid%h2osoi_liq6, &
              grid%h2osoi_liq7,grid%h2osoi_liq8,&
              grid%h2osoi_liq9,   &
              grid%h2osoi_liq10, &
              grid%h2osoi_ice_s1,grid%h2osoi_ice_s2,&
              grid%h2osoi_ice_s3, &
              grid%h2osoi_ice_s4, &
              grid%h2osoi_ice_s5, &
              grid%h2osoi_ice1,&
              grid%h2osoi_ice2,&
              grid%h2osoi_ice3,grid%h2osoi_ice4,&
              grid%h2osoi_ice5, &
              grid%h2osoi_ice6, &
              grid%h2osoi_ice7,grid%h2osoi_ice8,&
              grid%h2osoi_ice9,grid%h2osoi_ice10,&
              grid%t_soisno_s1,grid%t_soisno_s2, &
              grid%t_soisno_s3,grid%t_soisno_s4, &
              grid%t_soisno_s5,grid%t_soisno1,&
              grid%t_soisno2,grid%t_soisno3,&
              grid%t_soisno4,grid%t_soisno5, &
              grid%t_soisno6,grid%t_soisno7,&
              grid%t_soisno8,grid%t_soisno9,&
              grid%t_soisno10,grid%dzsnow1,grid%dzsnow2,grid%dzsnow3,grid%dzsnow4,&
              grid%dzsnow5,grid%snowrds1,grid%snowrds2,grid%snowrds3,grid%snowrds4,grid%snowrds5, &
              grid%t_lake1,grid%t_lake2,&
              grid%t_lake3,grid%t_lake4, &
              grid%t_lake5,grid%t_lake6, &
              grid%t_lake7,grid%t_lake8, &
              grid%t_lake9,grid%t_lake10,&
              grid%h2osoi_vol1,grid%h2osoi_vol2, &
              grid%h2osoi_vol3,grid%h2osoi_vol4, &
              grid%h2osoi_vol5,&
              grid%h2osoi_vol6,&
              grid%h2osoi_vol7,grid%h2osoi_vol8,&
              grid%h2osoi_vol9,grid%h2osoi_vol10,&
              grid%ht,                          &
              grid%ALBEDOsubgrid,grid%LHsubgrid,&
              grid%HFXsubgrid,grid%LWUPsubgrid,&
              grid%Q2subgrid,grid%SABVsubgrid,  &
              grid%SABGsubgrid,grid%NRAsubgrid,&
              grid%SWUPsubgrid,grid%lhsoi, &
              grid%lhveg, grid%lhtran, & 
                      grid%TSK_SAVE,                        & 
                      grid%itimestep, grid%fdob,            &
                      t00, p00, a,                      & 
                      grid%TYR, grid%TYRA, grid%TDLY, grid%TLAG, grid%NYEAR, grid%NDAY,grid%tmn_update, &
                      grid%achfx, grid%aclhf, grid%acgrdflx,                  &
                      config_flags%nssl_cccn,                                 &
                      config_flags%nssl_alphah, config_flags%nssl_alphahl,    &
                      config_flags%nssl_cnoh, config_flags%nssl_cnohl,        &
                      config_flags%nssl_cnor, config_flags%nssl_cnos,         &
                      config_flags%nssl_rho_qh, config_flags%nssl_rho_qhl,    &
                      config_flags%nssl_rho_qs                                &
                      ,grid%RQCNCUTEN, grid%RQINCUTEN,grid%rliq             &      
                      ,grid%cldfra_dp,grid%cldfra_sh                        & 
                      ,grid%te_temf,grid%cf3d_temf,grid%wm_temf        & 
                      ,grid%massflux_EDKF, grid%entr_EDKF, grid%detr_EDKF                & 
                      ,grid%thl_up,grid%thv_up,grid%rt_up                                &
                      ,grid%rv_up,grid%rc_up,grid%u_up,grid%v_up,grid%frac_up            &
                      ,grid%QKE                                               &
                      ,grid%landusef,grid%landusef2,grid%mosaic_cat_index                            & 
                      ,grid%TSK_mosaic,grid%TSLB_mosaic,grid%SMOIS_mosaic,grid%SH2O_mosaic           & 
                      ,grid%CANWAT_mosaic,grid%SNOW_mosaic,grid%SNOWH_mosaic,grid%SNOWC_mosaic       & 
                      ,grid%ALBEDO_mosaic,grid%ALBBCK_mosaic, grid%EMISS_mosaic                      & 
                      ,grid%EMBCK_mosaic, grid%ZNT_mosaic, grid%Z0_mosaic                            & 
                      ,grid%TR_URB2D_mosaic,grid%TB_URB2D_mosaic                                     & 
                      ,grid%TG_URB2D_mosaic,grid%TC_URB2D_mosaic                                     & 
                      ,grid%QC_URB2D_mosaic                                                          & 
                      ,grid%TRL_URB3D_mosaic,grid%TBL_URB3D_mosaic                                   & 
                      ,grid%TGL_URB3D_mosaic                                                         & 
                      ,grid%SH_URB2D_mosaic,grid%LH_URB2D_mosaic                                     & 
                      ,grid%G_URB2D_mosaic,grid%RN_URB2D_mosaic                                      & 
                      ,grid%TS_URB2D_mosaic                                                          & 
                      ,grid%TS_RUL2D_mosaic                                                          & 
                      )
       ENDIF   

   ENDDO

   CALL wrf_debug ( 100 , 'start_domain_em: After call to phy_init' )

   IF (config_flags%do_avgflx_em .EQ. 1) THEN
      WRITE ( message , FMT = '("start_em: initializing avgflx on domain ",I3)' ) &
           & grid%id
      CALL wrf_message(trim(message)) 
      grid%avgflx_count = 0
      f_flux = config_flags%do_avgflx_cugd .EQ. 1  
      DO ij = 1, grid%num_tiles
         call wrf_debug(200,'In start_em, before zero_avgflx call')
         if (.not. grid%restart) call zero_avgflx(grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
              &   ids, ide, jds, jde, kds, kde,           &
              &   ims, ime, jms, jme, kms, kme,           &
              &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), kts, kte, f_flux, &
              &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
              &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
         call wrf_debug(200,'In start_em, after zero_avgflx call')
      ENDDO
   ENDIF


   call wrf_debug(100,'start_em: calling lightning_init')
   CALL lightning_init ( itimestep=grid%itimestep, restart=grid%restart, dt=grid%dt, dx=grid%dx   &
                    
                        ,cu_physics=config_flags%cu_physics,mp_physics=config_flags%mp_physics    &
                        ,do_radar_ref=config_flags%do_radar_ref                                   &
                        ,lightning_option=config_flags%lightning_option                           &
                        ,lightning_dt=config_flags%lightning_dt                                   &
                        ,lightning_start_seconds=config_flags%lightning_start_seconds             &
                        ,iccg_prescribed_num=config_flags%iccg_prescribed_num                     &
                        ,iccg_prescribed_den=config_flags%iccg_prescribed_den                     &
                        ,cellcount_method=config_flags%cellcount_method                           &
                    
                        ,ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde             &
                        ,ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme             &
                        ,its=its, ite=ite, jts=jts, jte=jte, kts=kts, kte=kte             &
                    
                        ,ic_flashcount=grid%ic_flashcount, ic_flashrate=grid%ic_flashrate        &
                        ,cg_flashcount=grid%cg_flashcount, cg_flashrate=grid%cg_flashrate        &
                    )

    call wrf_debug(100,'start_em: after calling lightning_init')

   END IF





 






























































!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_1.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_4.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_4_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
!ENDOFREGISTRYGENERATEDINCLUDE
      IF ( config_flags%sf_ocean_physics .EQ. PWP3DSCHEME ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_6.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_6_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
      END IF
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_INIT.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_INIT_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_MOIST.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_MOIST_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_TKE.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_TKE_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_SCALAR.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_SCALAR_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_CHEM.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_CHEM_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
if(config_flags%sf_surface_physics.eq.NOAHMPSCHEME.and.config_flags%opt_run.eq.5)then
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_HYDRO_NOAHMP_INIT.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_HYDRO_NOAHMP_INIT_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
endif


   CALL set_physical_bc3d( grid%u_1 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%u_2 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%v_1 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%v_2 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )



   CALL set_physical_bc2d( grid%ht , 'r' , config_flags ,                &
                           ids , ide , jds , jde , &
                           ims , ime , jms , jme , &
                           its , ite , jts , jte , &
                           its , ite , jts , jte   )






   IF ( ( start_of_simulation .OR. config_flags%cycling ) .AND. ( .NOT. config_flags%restart ) ) THEN








      w_max = grid%w_2(its,1,jts)
      w_min = grid%w_2(its,1,jts)
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
         w_max = MAX ( w_max , grid%w_2(i,1,j) )
         w_min = MIN ( w_min , grid%w_2(i,1,j) )
         END DO
      END DO
      w_max = wrf_dm_max_real ( w_max )
      w_min = wrf_dm_min_real ( w_min )

      IF ( ( ABS(w_max) .LT. 1.E-6 ) .AND. &
           ( ABS(w_min) .LT. 1.E-6 ) ) THEN
         w_needs_to_be_set = .TRUE.
      ELSE
         IF ( config_flags%use_input_w ) THEN
            w_needs_to_be_set = .FALSE.
         ELSE
            w_needs_to_be_set = .TRUE.
         END IF
      END IF

      IF ( w_needs_to_be_set ) THEN

         

         fill_w_flag = .true.
         CALL set_w_surface(  config_flags, grid%znw, fill_w_flag,             &
                              grid%w_1, grid%ht, grid%u_1, grid%v_1, grid%cf1, &
                              grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                              ids, ide, jds, jde, kds, kde,                    &
                              ims, ime, jms, jme, kms, kme,                    &
                              its, ite, jts, jte, kts, kte                     )
         CALL set_w_surface(  config_flags, grid%znw, fill_w_flag,             &
                              grid%w_2, grid%ht, grid%u_2, grid%v_2, grid%cf1, &
                              grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                              ids, ide, jds, jde, kds, kde,                    &
                              ims, ime, jms, jme, kms, kme,                    &
                              its, ite, jts, jte, kts, kte                     ) 
      END IF
   END IF

   IF ( .NOT. config_flags%restart ) THEN



       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          im1 = max(i-1,ids)
          ip1 = min(i+1,ide-1)
          jm1 = max(j-1,jds)
          jp1 = min(j+1,jde-1)
          grid%toposlpx(i,j)=(grid%ht(ip1,j)-grid%ht(im1,j))*grid%msftx(i,j)*grid%rdx/(ip1-im1)
          grid%toposlpy(i,j)=(grid%ht(i,jp1)-grid%ht(i,jm1))*grid%msfty(i,j)*grid%rdy/(jp1-jm1)
          grid%lap_hgt(i,j)=(grid%ht(ip1,j)+grid%ht(im1,j)+grid%ht(i,jp1)+grid%ht(i,jm1)-grid%ht(i,j)*4.)/4.
             hx = grid%toposlpx(i,j)
             hy = grid%toposlpy(i,j)
             pi = 4.*atan(1.)
             grid%slope(i,j) = atan((hx**2+hy**2)**.5)
             if (grid%slope(i,j).lt.1.e-4) then
               grid%slope(i,j) = 0.
               grid%slp_azi(i,j) = 0.
             else
               grid%slp_azi(i,j) = atan2(hx,hy)+pi


               if (grid%cosa(i,j).ge.0) then
                 grid%slp_azi(i,j) = grid%slp_azi(i,j) - asin(grid%sina(i,j))
               else
                 grid%slp_azi(i,j) = grid%slp_azi(i,j) - (pi - asin(grid%sina(i,j)))
               endif
             endif
       ENDDO
       ENDDO



       grid%ctopo=1.
       grid%ctopo2=1.

       if (config_flags%topo_wind.eq.1) then
         DO j = jts,min(jte,jde-1)
         DO i = its, min(ite,ide-1)
            if(grid%xland(i,j).lt.1.5)grid%ctopo(i,j)=sqrt(grid%var_sso(i,j))
            if (grid%ctopo(i,j).le.2.718) then
            grid%ctopo(i,j)=1.
            else
            grid%ctopo(i,j)=alog(grid%ctopo(i,j))
            endif

            if (grid%lap_hgt(i,j).gt.-10.) then
              grid%ctopo(i,j)=grid%ctopo(i,j)
            else
               if (grid%lap_hgt(i,j).ge.-20) then
                 alpha=(grid%lap_hgt(i,j)+20.)/10.
                 grid%ctopo(i,j)=alpha*grid%ctopo(i,j)+(1-alpha)
               else
                  if (grid%lap_hgt(i,j).ge.-30.) then
                  grid%ctopo(i,j)=(grid%lap_hgt(i,j)+30.)/10.
                  grid%ctopo2(i,j)=(grid%lap_hgt(i,j)+30.)/10.
                  else
                  grid%ctopo(i,j)=0.
                  grid%ctopo2(i,j)=0.
                  endif
               endif
            endif
         ENDDO
         ENDDO

       else if (config_flags%topo_wind.eq.2) then
         DO j = jts,min(jte,jde-1)
         DO i = its, min(ite,ide-1)
           if (grid%xland(i,j).lt.1.5) then
              vfac = amin1(1.575,(grid%var2d(i,j)*0.4/200.+1.175))
              vfac = vfac * vfac
           else                      
              vfac = 1.
           endif

           grid%ctopo(i,j)=grid%ctopo(i,j)*vfac
         ENDDO
         ENDDO
       endif

   END IF

   CALL set_physical_bc3d( grid%w_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%w_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%ph_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%ph_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%t_1 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%t_2 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc2d( grid%mu_1, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%mu_2, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%mub , 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )







   CALL set_physical_bc3d( grid%phb , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%ph0 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%php , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%pb , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%al , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%alt , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%alb , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d(grid%t_init, 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d(grid%tke_2, 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   IF (num_moist > 0) THEN



      loop_3d_m   : DO loop = 1 , num_moist
         CALL set_physical_bc3d( moist(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_m

   ENDIF

   IF (num_scalar > 0) THEN



      loop_3d_s   : DO loop = 1 , num_scalar
         CALL set_physical_bc3d( scalar(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_s

   ENDIF



        
        if ( grid%itimestep .eq. 0 .and. config_flags%tenddiag .eq. USETENDDIAG ) then
          advh_t(:,:,:,:) = 0.
          advz_t(:,:,:,:) = 0.
        endif

   IF (num_chem >= PARAM_FIRST_SCALAR ) THEN


      loop_3d_c   : DO loop = PARAM_FIRST_SCALAR , num_chem
         CALL set_physical_bc3d( chem(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_c

   ENDIF

   CALL set_physical_bc2d( grid%msftx , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfty , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfux , 'x' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfuy , 'x' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfvx , 'y' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfvy , 'y' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%sina , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%cosa , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%e , 'r' , config_flags ,                 &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%f , 'r' , config_flags ,                 &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )

      DEALLOCATE(CLDFRA_OLD)
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_1.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_2.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_2_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_3.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_3_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_4.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL HALO_EM_INIT_4_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_INIT.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_INIT_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_MOIST.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_MOIST_sub ( grid, &
  config_flags, &
  num_moist, &
  moist, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_TKE.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_TKE_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_SCALAR.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_SCALAR_sub ( grid, &
  config_flags, &
  num_scalar, &
  scalar, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_CHEM.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL PERIOD_BDY_EM_CHEM_sub ( grid, &
  config_flags, &
  num_chem, &
  chem, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
!ENDOFREGISTRYGENERATEDINCLUDE

DEALLOCATE(z_at_q)

   IF (config_flags%p_lev_diags == PRESS_DIAGS ) THEN
    CALL wrf_debug ( 200 , ' PLD: pressure level diags' )
    CALL pld (                                  &
                 
                  U=grid%u_2                    &
                 ,V=grid%v_2                    &
                 ,W=grid%w_2                    &
                 ,t=grid%t_2                    &
                 ,qv=moist(:,:,:,P_QV)          &
                 ,zp=grid%ph_2                  &
                 ,zb=grid%phb                   &
                 ,pp=grid%p                     &
                 ,pb=grid%pb                    &
                 ,p=grid%p_hyd                  &
                 ,pw=grid%p_hyd_w               &
                 
                 ,msfux=grid%msfux              &
                 ,msfuy=grid%msfuy              &
                 ,msfvx=grid%msfvx              &
                 ,msfvy=grid%msfvy              &
                 ,msftx=grid%msftx              &
                 ,msfty=grid%msfty              &
                 ,f=grid%f                      &
                 ,e=grid%e                      &
                 
                 ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p &
                 ,missing=config_flags%p_lev_missing &
                 
                 ,num_press_levels=config_flags%num_press_levels &
                 ,max_press_levels=max_plevs    &
                 ,press_levels=model_config_rec%press_levels &
                 ,p_pl  = grid%p_pl             &  
                 ,u_pl  = grid%u_pl             &  
                 ,v_pl  = grid%v_pl             &  
                 ,t_pl  = grid%t_pl             &  
                 ,rh_pl = grid%rh_pl            & 
                 ,ght_pl= grid%ght_pl           &
                 ,s_pl  = grid%s_pl             &  
                 ,td_pl = grid%td_pl            &
                 
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde    &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme    &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte    )
   ENDIF



if(config_flags%ifire.eq.2)then

   call fire_driver_em_init ( grid , config_flags    &
            ,ids,ide, kds,kde, jds,jde                &
            ,ims,ime, kms,kme, jms,jme                &
            ,ips,ipe, kps,kpe, jps,jpe ) 

   CALL wrf_debug ( 100 , 'start_domain_em: After call to fire_driver_em_init' )
endif


     CALL wrf_debug ( 100 , 'start_domain_em: Returning' )

     RETURN

   END SUBROUTINE start_domain_em

