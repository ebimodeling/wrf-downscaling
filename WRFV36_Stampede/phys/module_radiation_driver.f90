

MODULE module_radiation_driver
CONTAINS




   SUBROUTINE radiation_driver (                                  &
               ACFRCV ,ACFRST ,ALBEDO  &
              ,CFRACH ,CFRACL ,CFRACM  &
              ,CUPPT  ,CZMEAN ,DT      &
              ,DZ8W   ,EMISS  ,GLW     &
              ,GMT    ,GSW    ,HBOT    &
              ,HTOP   ,HBOTR  ,HTOPR   &
              ,ICLOUD                  &
              ,ITIMESTEP,JULDAY, JULIAN &
              ,JULYR  ,LW_PHYSICS       &
              ,NCFRCV ,NCFRST ,NPHS     &
              ,O3INPUT, O3RAD           &
              ,AER_OPT, aerod           &
              ,swint_opt                &
              ,P8W    ,P ,PI            &
              ,RADT   ,RA_CALL_OFFSET   &
              ,RHO    ,RLWTOA           &
              ,RSWTOA ,RTHRATEN         &
              ,RTHRATENLW      ,RTHRATENSW    &
              ,SNOW   ,STEPRA ,SWDOWN  &
              ,SWDOWNC ,SW_PHYSICS     &
              ,T8W     ,T ,TAUCLDC &
              ,TAUCLDI ,TSK ,VEGFRA  &
              ,WARM_RAIN ,XICE ,XLAND   &
              ,XLAT ,XLONG ,YR          &

              ,DECLINX ,SOLCONX ,COSZEN ,HRANG    &
              , CEN_LAT                                      &
              ,Z                                             &
              ,ALEVSIZ, no_src_types               &
              ,LEVSIZ, N_OZMIXM                    &
              ,N_AEROSOLC                                    &
              ,PAERLEV   ,ID                                 &
              ,CAM_ABS_DIM1, CAM_ABS_DIM2 &
              ,CAM_ABS_FREQ_S                         &
              ,XTIME                                           &
              ,CURR_SECS, ADAPT_STEP_FLAG       &
            
              ,IDS,IDE, JDS,JDE, KDS,KDE          &
              ,IMS,IME, JMS,JME, KMS,KME          &
              ,i_start,i_end          &
              ,j_start,j_end          &
              ,kts, kte                          &
              ,num_tiles                                   &
            
              , TLWDN, TLWUP                        & 
              , SLWDN, SLWUP                        & 
              , TSWDN, TSWUP                        & 
              , SSWDN, SSWUP                        & 
              , CLDFRA,CLDFRA_MP_ALL                &

              , lradius,iradius                     &

              , cldfra_dp, cldfra_sh                & 
              , re_cloud, re_ice, re_snow           & 
              , has_reqc, has_reqi, has_reqs        & 
              , PB                                                &
              , F_ICE_PHY,F_RAIN_PHY       &
              , QV, F_QV                     &
              , QC, F_QC                     &
              , QR, F_QR                     &
              , QI, F_QI                     &
              , QS, F_QS                     &
              , QG, F_QG                     &
              , QNDROP, F_QNDROP    &
              ,ACSWUPT   ,ACSWUPTC            &
              ,ACSWDNT   ,ACSWDNTC            &
              ,ACSWUPB   ,ACSWUPBC            &
              ,ACSWDNB   ,ACSWDNBC            &
              ,ACLWUPT   ,ACLWUPTC            &
              ,ACLWDNT   ,ACLWDNTC            &
              ,ACLWUPB   ,ACLWUPBC            &
              ,ACLWDNB   ,ACLWDNBC            &
              ,SWUPT ,SWUPTC                  &
              ,SWDNT ,SWDNTC                  &
              ,SWUPB ,SWUPBC                  &
              ,SWDNB ,SWDNBC                  &
              ,LWUPT ,LWUPTC                  &
              ,LWDNT ,LWDNTC                  &
              ,LWUPB ,LWUPBC                  &
              ,LWDNB ,LWDNBC                  &
              ,LWCF                           &
              ,SWCF                           &
              ,OLR                            &
              ,aerodm, PINA, aodtot           &
              ,OZMIXM, PIN                    &
              ,M_PS_1, M_PS_2, AEROSOLC_1     &
              ,AEROSOLC_2, M_HYBI0            &
              ,ABSTOT, ABSNXT, EMSTOT         &
              ,ICLOUD_CU                      &
              ,AER_RA_FEEDBACK                &
              ,QC_CU , QI_CU                  &
              ,PM2_5_DRY, PM2_5_WATER         &
              ,PM2_5_DRY_EC                   &
              ,TAUAER300, TAUAER400 & 
              ,TAUAER600, TAUAER999 & 
              ,GAER300, GAER400, GAER600, GAER999 & 
              ,WAER300, WAER400, WAER600, WAER999 & 
              ,TAUAERlw1,  TAUAERlw2  &
              ,TAUAERlw3,  TAUAERlw4  &
              ,TAUAERlw5,  TAUAERlw6  &
              ,TAUAERlw7,  TAUAERlw8  &
              ,TAUAERlw9,  TAUAERlw10   &
              ,TAUAERlw11, TAUAERlw12   &
              ,TAUAERlw13, TAUAERlw14   &
              ,TAUAERlw15, TAUAERlw16  &
              ,progn                                            &
              ,slope_rad,topo_shading     &
              ,shadowmask,ht,dx,dy                 &
              ,SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC                          & 
              ,LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC                          & 
              ,radtacttime                                                &
              ,ALSWVISDIR, ALSWVISDIF, ALSWNIRDIR, ALSWNIRDIF             & 
              ,SWVISDIR, SWVISDIF, SWNIRDIR, SWNIRDIF                     & 
              ,SF_SURFACE_PHYSICS, IS_CAMMGMP_USED                        & 
              ,EXPLICIT_CONVECTION                                        & 
              ,swddir,swddni,swddif                                       & 
              ,swdown_ref,swddir_ref,coszen_ref,Gx,gg,Bx,bb               &
              ,aer_type                                                   & 
              ,aer_aod550_opt, aer_aod550_val                             &
              ,aer_angexp_opt, aer_angexp_val                             &
              ,aer_ssa_opt, aer_ssa_val                                   &
              ,aer_asy_opt, aer_asy_val                                   &
              ,aod5502d, angexp2d, aerssa2d, aerasy2d                     &
              ,aod5503d                                                   &
                                                                          )





   USE module_state_description, ONLY : RRTMSCHEME, GFDLLWSCHEME        &
                                       ,RRTMG_LWSCHEME, RRTMG_SWSCHEME  &
                                       ,SWRADSCHEME, GSFCSWSCHEME       &
                                       ,GFDLSWSCHEME, CAMLWSCHEME, CAMSWSCHEME &
                                       ,HELDSUAREZ                      &



                                       ,goddardlwscheme                 & 
                                       ,goddardswscheme                 &  

                                       ,CAMMGMPSCHEME                   &

                                       ,FLGLWSCHEME, FLGSWSCHEME

   USE module_model_constants

   USE module_wrf_error , ONLY : wrf_err_message




   USE module_ra_sw         , ONLY : swrad
   USE module_ra_gsfcsw     , ONLY : gsfcswrad
   USE module_ra_rrtm       , ONLY : rrtmlwrad
   USE module_ra_rrtmg_lw   , ONLY : rrtmg_lwrad
   USE module_ra_rrtmg_sw   , ONLY : rrtmg_swrad
   USE module_ra_cam        , ONLY : camrad
   USE module_ra_gfdleta    , ONLY : etara



   USE module_ra_hs         , ONLY : hsrad

   USE module_ra_goddard    , ONLY : goddardrad
   USE module_ra_flg        , ONLY : RAD_FLG

   USE module_ra_aerosol    , ONLY : calc_aerosol_goddard_sw, &
                                     calc_aerosol_rrtmg_sw

   
   
   
   
   
   
   
   
   
   

   IMPLICIT NONE

























































































































































   LOGICAL, OPTIONAL, INTENT(IN) :: explicit_convection
   LOGICAL :: expl_conv
   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                                         kts,kte, &
                                       num_tiles

   INTEGER, INTENT(IN)            :: lw_physics, sw_physics
   INTEGER, INTENT(IN)            :: o3input, aer_opt
   INTEGER, INTENT(IN)            :: id
   integer, intent(in)            :: swint_opt

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
                i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   STEPRA,ICLOUD,ra_call_offset
   INTEGER,      INTENT(IN   )    ::   alevsiz, no_src_types
   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc, cam_abs_dim1, cam_abs_dim2
   REAL,      INTENT(IN   )       ::   cam_abs_freq_s

   LOGICAL,      INTENT(IN   )    ::   warm_rain
   LOGICAL,      INTENT(IN   )    ::   is_CAMMGMP_used 

   REAL,      INTENT(IN   )       ::   RADT

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                 XLAND, &
                                                            XICE, &
                                                             TSK, &
                                                          VEGFRA, &
                                                            SNOW 
   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ),  OPTIONAL,    &
          INTENT(IN   ) ::                                  OZMIXM
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_src_types, n_ozmixm-1 ),  OPTIONAL,    &
          INTENT(IN   ) ::                                  AERODM
   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme, no_src_types ),  OPTIONAL,    &
          INTENT(INOUT) ::                                  AEROD
   REAL,  DIMENSION( ims:ime, jms:jme ), OPTIONAL,                &
          INTENT(INOUT) ::                                  AODTOT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ) :: OZFLG

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(IN )  ::     PIN
   REAL,  DIMENSION(alevsiz), OPTIONAL, INTENT(IN )  ::     PINA

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(IN )  ::      m_ps_1,m_ps_2
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL, &
          INTENT(IN   ) ::                       aerosolc_1, aerosolc_2
   REAL,  DIMENSION(paerlev), OPTIONAL, &
          INTENT(IN   ) ::                           m_hybi0

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  HTOP, &
                                                            HBOT, &
                                                           HTOPR, &
                                                           HBOTR, &
                                                           CUPPT

   INTEGER, INTENT(IN   )  ::   julyr

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                                     dz8w, &
                                                               z, &
                                                             p8w, &
                                                               p, &
                                                              pi, &
                                                               t, &
                                                             t8w, &
                                                             rho

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::  tauaer300,tauaer400,tauaer600,tauaer999, & 
                                 gaer300,gaer400,gaer600,gaer999, & 
                                 waer300,waer400,waer600,waer999

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::          qc_cu, qi_cu

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::  tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4, & 
                         tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8, & 
                         tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12, & 
                         tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16

   INTEGER, INTENT(IN) :: icloud_cu

   INTEGER, INTENT(IN   ), OPTIONAL  ::   aer_ra_feedback


   INTEGER, OPTIONAL, INTENT(IN   )    :: progn



   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::                                pm2_5_dry, &
                                                     pm2_5_water, &
                                                    pm2_5_dry_ec

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN, &
                                                      RTHRATENLW, &
                                                      RTHRATENSW











   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC


   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                        SWUPT,  SWUPTC,  SWDNT,  SWDNTC,          &
                        SWUPB,  SWUPBC,  SWDNB,  SWDNBC,          &
                        LWUPT,  LWUPTC,  LWDNT,  LWDNTC,          &
                        LWUPB,  LWUPBC,  LWDNB,  LWDNBC


   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(INOUT) ::                               &
                               SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC, &
                               LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC

   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(INOUT)  ::                                  SWCF, &
                                                            LWCF, &
                                                             OLR

   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(IN   )  ::                            ALSWVISDIR, &
                                                      ALSWVISDIF, &
                                                      ALSWNIRDIR, &
                                                      ALSWNIRDIF

   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(OUT  )  ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF
   INTEGER,    OPTIONAL, INTENT(IN   )    ::  sf_surface_physics

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                          ALBEDO, &
                                                           EMISS

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW, &
                                                             GLW

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)  ::   SWDOWN


   REAL, DIMENSION( ims:ime, jms:jme ),  INTENT(OUT) :: swddir, & 
                                                        swddni, & 
                                                        swddif    
   REAL, DIMENSION( ims:ime, jms:jme ),  INTENT(INOUT) :: Gx,Bx,gg,bb, & 
                                                          swdown_ref,  &
                                                          swddir_ref,  &
                                                          coszen_ref

    INTEGER,                             INTENT(IN)    :: aer_type,       & 
                                                          aer_aod550_opt, & 
                                                          aer_angexp_opt, & 
                                                          aer_ssa_opt,    & 
                                                          aer_asy_opt       
    REAL,                                INTENT(IN)    :: aer_aod550_val, & 
                                                          aer_angexp_val, & 
                                                          aer_ssa_val,    & 
                                                          aer_asy_val       
    REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL,               &
          INTENT(INOUT)                                :: aod5502d,       & 
                                                          angexp2d,       & 
                                                          aerssa2d,       & 
                                                          aerasy2d          
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,               &
          INTENT(OUT)                                  :: aod5503d   

   REAL, INTENT(IN  )   ::                                GMT,dt, &
                                                   julian, xtime
   INTEGER, INTENT(IN  ),OPTIONAL ::                          YR

   INTEGER, INTENT(IN  ) ::                    JULDAY, itimestep
   REAL, INTENT(IN ),OPTIONAL     ::                    CURR_SECS
   LOGICAL, INTENT(IN ),OPTIONAL  ::              ADAPT_STEP_FLAG

   INTEGER,INTENT(IN)                                       :: NPHS
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(OUT)          ::    &
                                                      CFRACH,     & 
                                                      CFRACL,     & 
                                                      CFRACM,     & 
                                                      CZMEAN        
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                        &
                                                      RLWTOA,     & 
                                                      RSWTOA,     & 
                                                      ACFRST,     & 
                                                      ACFRCV        

   INTEGER,DIMENSION( ims:ime, jms:jme ),INTENT(INOUT)        ::  &
                                                          NCFRST, &  
                                                          NCFRCV     


   REAL, DIMENSION(IMS:IME, JMS:JME, 1:8)       :: ERBE_out  
   REAL, DIMENSION(IMS:IME, JMS:JME), OPTIONAL, INTENT(INOUT) ::   & 
                                               TLWDN, TLWUP,     &
                                               SLWDN, SLWUP,     &
                                               TSWDN, TSWUP,     &
                                               SSWDN, SSWUP        



   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim2, jms:jme ), OPTIONAL ,&
         INTENT(INOUT)  ::                                  abstot
   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim1, jms:jme ), OPTIONAL ,&
         INTENT(INOUT)  ::                                  absnxt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               OPTIONAL ,&
         INTENT(INOUT)  ::                                  emstot




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) ::                                 CLDFRA   

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  & 
         OPTIONAL,                                                &
         INTENT(INOUT) ::                              cldfra_dp, &
                                                       cldfra_sh


   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN):: re_cloud, re_ice, re_snow
   INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY, &
                                                      CLDFRA_MP_ALL


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                            LRADIUS, &
                                                            IRADIUS


   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL,                                                &
         INTENT(OUT) ::                                   SWDOWNC

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT ) ::                                        &
                                                               pb &
                                        ,qv,qc,qr,qi,qs,qg,qndrop

   LOGICAL, OPTIONAL ::     f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT)  ::                       taucldi,taucldc



     REAL, OPTIONAL, INTENT(IN) :: dx,dy
     INTEGER, OPTIONAL, INTENT(IN) :: slope_rad,topo_shading
     REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)  :: ht
     INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN) :: shadowmask

   REAL , OPTIONAL, INTENT(INOUT) ::    radtacttime 
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT)  ::                       o3rad



   REAL, DIMENSION( ims:ime, jms:jme ) ::             GLAT,GLON
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    CEMISS
   REAL, DIMENSION( ims:ime, jms:jme ) ::             coszr
   REAL, DIMENSION( ims:ime, levsiz, jms:jme )  ::    ozmixt
   REAL, DIMENSION( ims:ime, alevsiz, jms:jme, 1:no_src_types )  ::    aerodt

   REAL    ::    DECLIN,SOLCON,XXLAT,TLOCTM,XT24, CEN_LAT
   INTEGER ::    i,j,k,its,ite,jts,jte,ij
   INTEGER ::    STEPABS
   LOGICAL ::    gfdl_lw,gfdl_sw
   LOGICAL ::    doabsems
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER ::    s

   REAL    ::    OBECL,SINOB,SXLONG,ARG,DECDEG,                  &
                 DJUL,RJUL,ECCFAC
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_temp,qc_temp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_save,qc_save

   REAL    ::    next_rad_time
   LOGICAL ::    run_param , doing_adapt_dt , decided
   LOGICAL ::    flg_lw, flg_sw

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    cldfra_cu



   REAL, OPTIONAL, INTENT(OUT) :: DECLINX,SOLCONX
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: COSZEN
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: HRANG



   real :: ioh,kt,airmass,kd
   real, dimension(ims:ime,jms:jme) :: coszen_loc,hrang_loc

   real, dimension(:,:,:,:), allocatable :: tauaer_sw, ssaaer_sw, asyaer_sw





   
   
   
   if(present(explicit_convection)) then
      expl_conv=explicit_convection
   else
      expl_conv=.true. 
   endif

   CALL wrf_debug (1, 'Top of Radiation Driver')


   if (lw_physics .eq. 0 .and. sw_physics .eq. 0)         return






   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
         IF ( radtacttime .eq. 0. ) THEN
            radtacttime = CURR_SECS + radt*60.
         END IF
      END IF
   END IF



















   run_param = .FALSE.
   decided = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( ( radt .EQ. 0. ) .OR. ( stepra .EQ. 1 ) ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepra) .EQ. 1+ra_call_offset ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. radtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      radtacttime = curr_secs + radt*60
   END IF

   if(swint_opt.eq.1) then
      DO ij = 1 , num_tiles
         its = i_start(ij)
         ite = i_end(ij)
         jts = j_start(ij)
         jte = j_end(ij)
         CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,               &
                       DEGRAD,DPD                                )
         call calc_coszen(ims,ime,jms,jme,its,ite,jts,jte, &
                          julian,xtime,gmt,declin,degrad,  &
                          xlong,xlat,coszen_loc,hrang_loc)
      end do
   end if

   Radiation_step: IF ( run_param ) then


     STEPABS = nint(cam_abs_freq_s/(dt*STEPRA))*STEPRA
     IF (itimestep .eq. 1 .or. mod(itimestep,STEPABS) .eq. 1 + ra_call_offset &
                                        .or. STEPABS .eq. 1 ) THEN
       doabsems = .true.
     ELSE
       doabsems = .false.
     ENDIF
   IF (PRESENT(adapt_step_flag)) THEN
     IF ((adapt_step_flag)) THEN
       IF ( (itimestep .EQ. 1) .OR. (cam_abs_freq_s .EQ. 0) .OR. &
           ( CURR_SECS + dt >= ( INT( CURR_SECS / ( cam_abs_freq_s ) + 1 ) * cam_abs_freq_s) ) ) THEN
         doabsems = .true.
       ELSE
         doabsems = .false.
       ENDIF
     ENDIF
   ENDIF

   gfdl_lw = .false.
   gfdl_sw = .false.


   IF ( PRESENT( AOD5502D ) ) THEN
     
     IF ( aer_opt .EQ. 2 ) THEN
        swrad_aerosol_select: select case(sw_physics)

           case(GODDARDSWSCHEME)
              allocate(tauaer_sw(ims:ime, kms:kme, jms:jme, 1:11))
              allocate(ssaaer_sw(ims:ime, kms:kme, jms:jme, 1:11))
              allocate(asyaer_sw(ims:ime, kms:kme, jms:jme, 1:11))

           case(RRTMG_SWSCHEME)
              allocate(tauaer_sw(ims:ime, kms:kme, jms:jme, 1:14))
              allocate(ssaaer_sw(ims:ime, kms:kme, jms:jme, 1:14))
              allocate(asyaer_sw(ims:ime, kms:kme, jms:jme, 1:14))

        end select swrad_aerosol_select
     ELSE
        swrad_aerosol_select_stub: select case(sw_physics)

           case(GODDARDSWSCHEME)
              allocate(tauaer_sw(1, 1, 1, 1))
              allocate(ssaaer_sw(1, 1, 1, 1))
              allocate(asyaer_sw(1, 1, 1, 1))

           case(RRTMG_SWSCHEME)
              allocate(tauaer_sw(1, 1, 1, 1))
              allocate(ssaaer_sw(1, 1, 1, 1))
              allocate(asyaer_sw(1, 1, 1, 1))

        end select swrad_aerosol_select_stub
     ENDIF
   ENDIF






   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)
     CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,               &
                   DEGRAD,DPD                                )

     IF(PRESENT(declinx).AND.PRESENT(solconx))THEN

       declinx=declin
       solconx=solcon
     ENDIF



     call calc_coszen(ims,ime,jms,jme,its,ite,jts,jte,  &
                      julian,xtime+radt*0.5,gmt, &
                      declin,degrad,xlong,xlat,coszen,hrang)
   ENDDO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)



     if ((itimestep.eq.1).and.(swint_opt.eq.1)) then
        do j=jts,jte
           do i=its,ite
              Bx(i,j)=0.
              bb(i,j)=0.
              Gx(i,j)=0.
              gg(i,j)=0.
           end do
        end do
     end if

     DO j=jts,jte
     DO i=its,ite
        GSW(I,J)=0.
        GLW(I,J)=0.
        SWDOWN(I,J)=0.
        swddir(i,j)=0.  
        swddni(i,j)=0.  
        swddif(i,j)=0.  
        GLAT(I,J)=XLAT(I,J)*DEGRAD
        GLON(I,J)=XLONG(I,J)*DEGRAD
     ENDDO
     ENDDO

     DO j=jts,jte
     DO k=kts,kte+1
     DO i=its,ite
        RTHRATEN(I,K,J)=0.
        RTHRATENLW(I,K,J)=0.
        RTHRATENSW(I,K,J)=0.








        CEMISS(I,K,J)=0.0
     ENDDO
     ENDDO
     ENDDO

     IF ( PRESENT( SWUPFLX ) ) THEN
        DO j=jts,jte
        DO k=kts,kte+2
        DO i=its,ite
           SWUPFLX(I,K,J) = 0.0
           SWDNFLX(I,K,J) = 0.0
           SWUPFLXC(I,K,J) = 0.0
           SWDNFLXC(I,K,J) = 0.0
           LWUPFLX(I,K,J) = 0.0
           LWDNFLX(I,K,J) = 0.0
           LWUPFLXC(I,K,J) = 0.0
           LWDNFLXC(I,K,J) = 0.0
        ENDDO
        ENDDO
        ENDDO
     ENDIF



       IF ( PRESENT( qc ) .AND. PRESENT( qc_cu ) .AND. icloud_cu .EQ. 1 ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qc_save(i,k,j) = qc(i,k,j)
            qc(i,k,j) = qc(i,k,j) + qc_cu(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
       IF ( PRESENT( qi ) .AND. PRESENT( qi_cu ) .AND. icloud_cu .EQ. 1 ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qi_save(i,k,j) = qi(i,k,j)
            qi(i,k,j) = qi(i,k,j) + qi_cu(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF


     if(PRESENT(qc) .and. PRESENT(F_QC)) then
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J)=qc(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     else
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
     endif













     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite
        CLDFRA(i,k,j) = 0.
     END DO
     END DO
     END DO

     IF ( ICLOUD == 1 ) THEN

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN


        CALL wrf_debug (1, 'CALL cldfra2')
        CALL cal_cldfra1(CLDFRA,qv,qc,qi,qs,               &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )

        IF ( PRESENT ( CLDFRA_DP ) ) THEN

          IF ( icloud_cu .EQ. 2 ) THEN
             CALL wrf_debug (1, 'use kf cldfra')
             DO j = jts,jte
             DO k = kts,kte
             DO i = its,ite
                cldfra_cu(i,k,j)=cldfra_dp(i,k,j)+cldfra_sh(i,k,j) 
                CLDFRA(i,k,j)=(1.-cldfra_cu(i,k,j))*CLDFRA(i,k,j)  
                CLDFRA(i,k,j)=CLDFRA(i,k,j)+cldfra_cu(i,k,j)       
                CLDFRA(i,k,j)=AMIN1(1.0,CLDFRA(i,k,j))
                qc_save(i,k,j)=qc(i,k,j)
                qc(i,k,j) = qc(i,k,j)+qc_cu(i,k,j)*cldfra_cu(i,k,j)
                qi_save(i,k,j)=qi(i,k,j)
                qi(i,k,j) = qi(i,k,j)+qi_cu(i,k,j)*cldfra_cu(i,k,j)
             ENDDO
             ENDDO
             ENDDO
          ENDIF
        ENDIF

        IF ( PRESENT (cldfra_mp_all) ) THEN
          IF (is_CAMMGMP_used) THEN
            
        CALL wrf_debug (1, 'use cammgmp')
            IF (itimestep .NE. 1) THEN
               DO j=jts,jte
               DO k=kts,kte
               DO i=its,ite
                  CLDFRA(i,k,j) = CLDFRA_MP_ALL(I,K,J) 
                  if (CLDFRA(i,k,j) .lt. 0.01) CLDFRA(i,k,j) = 0.
               ENDDO
               ENDDO
               ENDDO
            ENDIF 
          ENDIF
        ENDIF
     ENDIF
 
     ELSE IF ( ICLOUD == 2 ) THEN

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
       CALL cal_cldfra2(CLDFRA,qc,qi,F_QC,F_QI,              &
                       ids,ide, jds,jde, kds,kde,            &
                       ims,ime, jms,jme, kms,kme,            &
                       its,ite, jts,jte, kts,kte             )
     ENDIF

     END IF



     IF ( PRESENT( O3RAD ) ) THEN
     IF ( o3input .EQ. 2 .AND. id .EQ. 1 ) THEN







        call ozn_time_int(julday,julian,ozmixm,ozmixt,levsiz,n_ozmixm,    &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )


        call ozn_p_int(p ,pin, levsiz, ozmixt, o3rad, &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )
     ENDIF
     ENDIF

     IF ( PRESENT( AEROD ) ) THEN
     IF ( aer_opt .EQ. 1 .AND. id .EQ. 1 ) THEN
        call aer_time_int(julday,julian,aerodm,aerodt,alevsiz,n_ozmixm-1,no_src_types,    &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )

        call aer_p_int(p ,pina, alevsiz, aerodt, aerod, no_src_types, p8w, AODTOT, &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )
     ENDIF
     ENDIF

     lwrad_select: SELECT CASE(lw_physics)

        CASE (RRTMSCHEME)
             CALL wrf_debug (100, 'CALL rrtm')

             CALL RRTMLWRAD(                                        &
                  RTHRATEN=RTHRATEN,GLW=GLW,OLR=RLWTOA,EMISS=EMISS  &
                 ,QV3D=QV                                           &
                 ,QC3D=QC                                           &
                 ,QR3D=QR                                           &
                 ,QI3D=QI                                           &
                 ,QS3D=QS                                           &
                 ,QG3D=QG                                           &
                 ,P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t     &
                 ,T8W=t8w,RHO3D=rho, CLDFRA3D=CLDFRA,R=R_d,G=G      &
                 ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR                     &
                 ,F_QI=F_QI,F_QS=F_QS,F_QG=F_QG                     &
                 ,ICLOUD=icloud,WARM_RAIN=warm_rain                 &

                 ,YR=YR,JULIAN=JULIAN                               &

                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )

        CASE (goddardlwscheme)

             CALL wrf_debug (100, 'CALL goddard longwave radiation scheme ')
             IF (itimestep.eq.1) then
                call wrf_message('running goddard lw radiation')
             ENDIF
             CALL goddardrad(sw_or_lw='lw'                             &
                    ,rthraten=rthraten,gsf=glw,xlat=xlat,xlong=xlong   &
                    ,alb=albedo,t3d=t,p3d=p,p8w3d=p8w,pi3d=pi          &
                    ,dz8w=dz8w,rho_phy=rho,emiss=emiss                 &
                    ,cldfra3d=cldfra                                   &
                    ,gmt=gmt,cp=cp,g=g,t8w=t8w                         &
                    ,julday=julday,xtime=xtime                         &
                    ,declin=declin,solcon=solcon                       &
                    , center_lat = cen_lat                             &
                    ,radfrq=radt,degrad=degrad                         &
                    ,taucldi=taucldi,taucldc=taucldc                   &
                    ,warm_rain=warm_rain                               &
                    ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde &
                    ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme &
                    ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte &

                    ,qv3d=qv                                           &
                    ,qc3d=qc                                           &
                    ,qr3d=qr                                           &
                    ,qi3d=qi                                           &
                    ,qs3d=qs                                           &
                    ,qg3d=qg                                           &
                    ,f_qv=f_qv,f_qc=f_qc,f_qr=f_qr                     &
                    ,f_qi=f_qi,f_qs=f_qs,f_qg=f_qg                     &
                    ,erbe_out=erbe_out                                 & 
                    ,aer_opt=aer_opt                                   &
                                                                       )

        CASE (GFDLLWSCHEME)

             CALL wrf_debug (100, 'CALL gfdllw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QS) .AND. PRESENT(qs)   .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc)   ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QS) THEN
                 gfdl_lw  = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc_temp,QI=qi,QS=qs                      &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,HBOTR=hbotr, HTOPR=htopr                          &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,XTIME=xtime,JULIAN=julian                         &
                 ,JULYR=julyr,JULDAY=julday                         &
                 ,GFDL_LW=gfdl_lw,GFDL_SW=gfdl_sw                   &
                 ,CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach         &
                 ,ACFRST=acfrst,NCFRST=ncfrst                       &
                 ,ACFRCV=acfrcv,NCFRCV=ncfrcv                       &
                 ,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean         &
                 ,THRATEN=rthraten,THRATENLW=rthratenlw             &
                 ,THRATENSW=rthratensw                              &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
               ELSE
                 CALL wrf_error_fatal3("<stdin>",1183,&
'Can not call ETARA (1a). Missing moisture fields.')
               ENDIF
             ELSE
               CALL wrf_error_fatal3("<stdin>",1187,&
'Can not call ETARA (1b). Missing moisture fields.')
             ENDIF


        CASE (CAMLWSCHEME)

             CALL wrf_debug(100, 'CALL camrad lw')

             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2).AND. PRESENT(ALSWVISDIR) ) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     dolw=.true.,dosw=.false.,                         &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=RLWTOA,CEMISS=CEMISS,     &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif      &  
                    ,ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif      &  
                    ,SWVISDIR=swvisdir ,SWVISDIF=swvisdif              &  
                    ,SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif              &  
                    ,SF_SURFACE_PHYSICS=sf_surface_physics             &  
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,f_ice_phy=f_ice_phy,f_rain_phy=f_rain_phy         &
                    ,p_phy=p,p8w=p8w,z=z,pi_phy=pi,rho_phy=rho,        &
                     dz8w=dz8w,                                        &
                     CLDFRA=CLDFRA,XLAND=XLAND,XICE=XICE,SNOW=SNOW,    &
                     ozmixm=ozmixm,pin0=pin,levsiz=levsiz,             &
                     num_months=n_ozmixm,                              &
                     m_psp=m_ps_1,m_psn=m_ps_2,aerosolcp=aerosolc_1,   &
                     aerosolcn=aerosolc_2,m_hybi0=m_hybi0,             &
                     paerlev=paerlev, naer_c=n_aerosolc,               &
                     cam_abs_dim1=cam_abs_dim1, cam_abs_dim2=cam_abs_dim2, &
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,YR=YR,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,coszen=coszen                                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1246,&
'arguments not present for calling cam radiation' )
             ENDIF

        CASE (RRTMG_LWSCHEME)
             CALL wrf_debug (100, 'CALL rrtmg_lw')

             CALL RRTMG_LWRAD(                                      &
                  RTHRATENLW=RTHRATEN,                              &
                  LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                  LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                  LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                  LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                  GLW=GLW,OLR=RLWTOA,LWCF=LWCF,                     &
                  EMISS=EMISS,                                      &
                  P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t,    &
                  T8W=t8w,RHO3D=rho,R=R_d,G=G,                      &
                  ICLOUD=icloud,WARM_RAIN=warm_rain,CLDFRA3D=CLDFRA,&
                  LRADIUS=lradius, IRADIUS=iradius,                 &
                  IS_CAMMGMP_USED=is_cammgmp_used,                  &



                  F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY,        &
                  XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                  QV3D=QV,QC3D=QC,QR3D=QR,                          &
                  QI3D=QI,QS3D=QS,QG3D=QG,                          &
                  O3INPUT=O3INPUT,O33D=O3RAD,                       &
                  F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,                    &
                  F_QI=F_QI,F_QS=F_QS,F_QG=F_QG,                    &
                  RE_CLOUD=re_cloud,RE_ICE=re_ice,RE_SNOW=re_snow,  & 
                  has_reqc=has_reqc,has_reqi=has_reqi,has_reqs=has_reqs, & 
                  QNDROP3D=qndrop,F_QNDROP=f_qndrop,                &

                  YR=YR,JULIAN=JULIAN,                              &

                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                  LWUPFLX=LWUPFLX,LWUPFLXC=LWUPFLXC,                &
                  LWDNFLX=LWDNFLX,LWDNFLXC=LWDNFLXC                 &
                                                                    )

        CASE (HELDSUAREZ)
             CALL wrf_debug (100, 'CALL heldsuarez')

             CALL HSRAD(RTHRATEN,p8w,p,pi,dz8w,t,          &
                     t8w, rho, R_d,G,CP, dt, xlat, degrad, &
                     ids,ide, jds,jde, kds,kde,            &
                     ims,ime, jms,jme, kms,kme,            &
                     its,ite, jts,jte, kts,kte            )


        CASE (FLGLWSCHEME)
          CALL wrf_debug (100, 'CALL Fu-Liou-Gu')
          flg_lw  = .true.

          ozflg = 0.

          CALL RAD_FLG(                               &
               peven=p8w,podd=p,t8w=t8w,degrees=t     &
              ,pi3d=pi,o3=ozflg                       &
              ,G=G,CP=CP                              &
              ,albedo=ALBEDO,tskin=tsk                &
              ,h2o=QV,cld_iccld=QI,cld_wlcld=QC       &
              ,cld_prwc=QR,cld_pgwc=QG,cld_snow=QS    &
              ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR          &
              ,F_QI=F_QI,F_QS=F_QS,F_QG=F_QG          &
              ,warm_rain=warm_rain                    &
              ,cloudstrf=CLDFRA                       &
              ,emiss=EMISS                            &
              ,air_den=rho                            &
              ,dz3d=dz8w                              &
              ,SOLCON=SOLCON                          &
              ,declin=DECLIN                          &
              ,xtime=xtime, xlong=xlong, xlat=xlat    &
              ,JULDAY=JULDAY                          &
              ,gmt=gmt, radt=radt, degrad=degrad      &
              ,dtcolumn=dt                            &
              ,ids=ids,ide=ide,jds=jds,jde=jde        &
              ,kds=kds,kde=kde                        &     
              ,ims=ims,idim=ime,jms=jms,jdim=jme      &
              ,kms=kms,kmax=kme                       &
              ,its=its,ite=ite,jts=jts,jte=jte        &
              ,kts=kts,kte=kte                        &
		      ,uswtop=RSWTOA,ulwtop=RLWTOA            &
		      ,NETSWBOT=GSW,DLWBOT=GLW                &
		      ,DSWBOT=SWDOWN,deltat=RTHRATEN          &
		      ,dtshort=RTHRATENSW,dtlongwv=RTHRATENLW &
              )

          CALL wrf_debug(100, 'a4 Fu_Liou-Gu')


        CASE DEFAULT
  
             WRITE( wrf_err_message , * ) 'The longwave option does not exist: lw_physics = ', lw_physics
             CALL wrf_error_fatal3("<stdin>",1343,&
wrf_err_message )
           
     END SELECT lwrad_select    

     IF (lw_physics .gt. 0 .and. .not.gfdl_lw) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENLW(I,K,J)=RTHRATEN(I,K,J)

           IF(PRESENT(OLR) .AND. K .EQ. 1)OLR(I,J)=RLWTOA(I,J)
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (lw_physics .eq. goddardlwscheme) THEN
          IF ( PRESENT (tlwdn) ) THEN
        DO j=jts,jte
        DO i=its,ite
           tlwdn(i,j) = erbe_out(i,j,1)    
           tlwup(i,j) = erbe_out(i,j,2)    
           slwdn(i,j) = erbe_out(i,j,3)    
           slwup(i,j) = erbe_out(i,j,4)    
        ENDDO    
        ENDDO    
          ENDIF
     ENDIF       

     IF ( PRESENT( AOD5502D ) ) THEN
     
     IF ( aer_opt .EQ. 2 ) THEN
     swrad_aerosol_select2: select case(sw_physics)
        case(GODDARDSWSCHEME)
           call wrf_debug(100, 'call calc_aerosol_goddard_sw')
           call calc_aerosol_goddard_sw(ht,dz8w,p,t,qv,aer_type,aer_aod550_opt,aer_angexp_opt,    &
                                        aer_ssa_opt,aer_asy_opt,aer_aod550_val,aer_angexp_val,    &
                                        aer_ssa_val,aer_asy_val,aod5502d,angexp2d,aerssa2d,       &
                                        aerasy2d,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte, &
                                        tauaer_sw,ssaaer_sw,asyaer_sw                             )
           do j=jts,jte
              do i=its,ite
                 do k=kts,kte
                    aod5503d(i,k,j)=tauaer_sw(i,k,j,8) 
                 end do
              end do
           end do

        case(RRTMG_SWSCHEME)
           call wrf_debug(100, 'call calc_aerosol_rrtmg_sw')
           call calc_aerosol_rrtmg_sw(ht,dz8w,p,t,qv,aer_type,aer_aod550_opt,aer_angexp_opt,    &
                                      aer_ssa_opt,aer_asy_opt,aer_aod550_val,aer_angexp_val,    &
                                      aer_ssa_val,aer_asy_val,aod5502d,angexp2d,aerssa2d,       &
                                      aerasy2d,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte, &
                                      tauaer_sw,ssaaer_sw,asyaer_sw                             )
           do j=jts,jte
              do i=its,ite
                 do k=kts,kte
                    aod5503d(i,k,j)=tauaer_sw(i,k,j,10) 
                 end do
              end do
           end do

        case default
     end select swrad_aerosol_select2
     ENDIF
     ENDIF

     swrad_select: SELECT CASE(sw_physics)

        CASE (SWRADSCHEME)
             CALL wrf_debug(100, 'CALL swrad')
             CALL SWRAD(                                               &
                     DT=dt,RTHRATEN=rthraten,GSW=gsw                   &
                    ,XLAT=xlat,XLONG=xlong,ALBEDO=albedo               &
                    ,RHO_PHY=rho,T3D=t                                 &
                    ,P3D=p,PI3D=pi,DZ8W=dz8w,GMT=gmt                   &
                    ,R=r_d,CP=cp,G=g,JULDAY=julday                     &
                    ,XTIME=xtime,DECLIN=declin,SOLCON=solcon           &
                    ,RADFRQ=radt,ICLOUD=icloud,DEGRAD=degrad           &
                    ,warm_rain=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,coszen=coszen,julian=julian                       )

        CASE (GSFCSWSCHEME)
             CALL wrf_debug(100, 'CALL gsfcswrad')
             CALL GSFCSWRAD(                                           &
                     RTHRATEN=rthraten,GSW=gsw,XLAT=xlat,XLONG=xlong   &
                    ,ALB=albedo,T3D=t,P3D=p,P8W3D=p8w,pi3D=pi          &
                    ,DZ8W=dz8w,RHO_PHY=rho                             &
                    ,CLDFRA3D=cldfra,RSWTOA=rswtoa                     &
                    ,GMT=gmt,CP=cp,G=g                                 &
                    ,JULDAY=julday,XTIME=xtime                         &
                    ,DECLIN=declin,SOLCON=solcon                       &
                    ,RADFRQ=radt,DEGRAD=degrad                         &
                    ,TAUCLDI=taucldi,TAUCLDC=taucldc                   &
                    ,WARM_RAIN=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,QNDROP3D=qndrop                                   &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,F_QNDROP=f_qndrop                                 &
                                                                       )

        CASE (goddardswscheme)

             CALL wrf_debug(100, 'CALL goddard shortwave radiation scheme ')
             IF (itimestep.eq.1) then
                call wrf_message('running goddard sw radiation')
             ENDIF
             CALL goddardrad(sw_or_lw='sw'                             &
                    ,rthraten=rthraten,gsf=gsw,xlat=xlat,xlong=xlong   &
                    ,alb=albedo,t3d=t,p3d=p,p8w3d=p8w,pi3d=pi          &
                    ,dz8w=dz8w,rho_phy=rho,emiss=emiss                 &
                    ,cldfra3d=cldfra                                   &
                    ,gmt=gmt,cp=cp,g=g,t8w=t8w                         &
                    ,julday=julday,xtime=xtime                         &
                    ,declin=declin,solcon=solcon                       &
                    ,center_lat = cen_lat                              &
                    ,radfrq=radt,degrad=degrad                         &
                    ,taucldi=taucldi,taucldc=taucldc                   &
                    ,warm_rain=warm_rain                               &
                    ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde &
                    ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme &
                    ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte &

                    ,qv3d=qv                                           &
                    ,qc3d=qc                                           &
                    ,qr3d=qr                                           &
                    ,qi3d=qi                                           &
                    ,qs3d=qs                                           &
                    ,qg3d=qg                                           &
                    ,f_qv=f_qv,f_qc=f_qc,f_qr=f_qr                     &
                    ,f_qi=f_qi,f_qs=f_qs,f_qg=f_qg                     &
                    ,erbe_out=erbe_out                                 & 
                    ,swddir=swddir,swddni=swddni,swddif=swddif         & 
                    ,coszen=coszen,julian=julian                       & 
                    ,tauaer3d_sw=tauaer_sw                             & 
                    ,ssaaer3d_sw=ssaaer_sw                             & 
                    ,asyaer3d_sw=asyaer_sw                             & 
                    ,aer_opt=aer_opt                                   &
                                                                       )

        CASE (CAMSWSCHEME)
             CALL wrf_debug(100, 'CALL camrad sw')
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2) .AND. PRESENT(ALSWVISDIR)) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     dolw=.false.,dosw=.true.,                         &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=RLWTOA,CEMISS=CEMISS,     &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif      &  
                    ,ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif      &  
                    ,SWVISDIR=swvisdir ,SWVISDIF=swvisdif              &  
                    ,SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif              &  
                    ,SF_SURFACE_PHYSICS=sf_surface_physics             &  
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,f_ice_phy=f_ice_phy,f_rain_phy=f_rain_phy         &
                    ,p_phy=p,p8w=p8w,z=z,pi_phy=pi,rho_phy=rho,        &
                     dz8w=dz8w,                                        &
                     CLDFRA=CLDFRA,XLAND=XLAND,XICE=XICE,SNOW=SNOW,    &
                     ozmixm=ozmixm,pin0=pin,levsiz=levsiz,             &
                     num_months=n_ozmixm,                              &
                     m_psp=m_ps_1,m_psn=m_ps_2,aerosolcp=aerosolc_1,   &
                     aerosolcn=aerosolc_2,m_hybi0=m_hybi0,             &
                     paerlev=paerlev, naer_c=n_aerosolc,               &
                     cam_abs_dim1=cam_abs_dim1, cam_abs_dim2=cam_abs_dim2, &
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,YR=YR,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,coszen=coszen                                     )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1557,&
'arguments not present for calling cam radiation' )
             ENDIF
             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

        CASE (RRTMG_SWSCHEME)
             CALL wrf_debug(100, 'CALL rrtmg_sw')
             CALL RRTMG_SWRAD(                                         &
                     RTHRATENSW=RTHRATENSW,                            &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     SWCF=SWCF,GSW=GSW,                                &
                     XTIME=XTIME,GMT=GMT,XLAT=XLAT,XLONG=XLONG,        &
                     RADT=RADT,DEGRAD=DEGRAD,DECLIN=DECLIN,            &
                     COSZR=COSZR,JULDAY=JULDAY,SOLCON=SOLCON,          &
                     ALBEDO=ALBEDO,t3d=t,t8w=t8w,TSK=TSK,              &
                     p3d=p,p8w=p8w,pi3d=pi,rho3d=rho,                  &
                     dz8w=dz8w,CLDFRA3D=CLDFRA,                        &
                     LRADIUS=lradius, IRADIUS=iradius,                 &
                     IS_CAMMGMP_USED=is_cammgmp_used,                  &
                     R=R_D,G=G,              &


                     ICLOUD=icloud,WARM_RAIN=warm_rain,                &
                     F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY,        &
                     XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                     QV3D=qv,QC3D=qc,QR3D=qr,                          &
                     QI3D=qi,QS3D=qs,QG3D=qg,                          &
                     O3INPUT=O3INPUT,O33D=O3RAD,                       &
                     AER_OPT=AER_OPT,aerod=aerod,no_src=no_src_types,  &
                     ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif,     &  
                     ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif,     &  
                     SWVISDIR=swvisdir ,SWVISDIF=swvisdif,             &  
                     SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif,             &  
                     SF_SURFACE_PHYSICS=sf_surface_physics,            &  
                     F_QV=f_qv,F_QC=f_qc,F_QR=f_qr,                    &
                     F_QI=f_qi,F_QS=f_qs,F_QG=f_qg,                    &
                     RE_CLOUD=re_cloud,RE_ICE=re_ice,RE_SNOW=re_snow,  & 
                     has_reqc=has_reqc,has_reqi=has_reqi,has_reqs=has_reqs, & 
                     QNDROP3D=qndrop,F_QNDROP=f_qndrop,                &
                     IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                     IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                     ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                     SWUPFLX=SWUPFLX,SWUPFLXC=SWUPFLXC,                &
                     SWDNFLX=SWDNFLX,SWDNFLXC=SWDNFLXC,                &
                     tauaer3d_sw=tauaer_sw,                             & 
                     ssaaer3d_sw=ssaaer_sw,                             & 
                     asyaer3d_sw=asyaer_sw,                             & 
                     swddir=swddir,swddni=swddni,swddif=swddif,         & 
                     xcoszen=coszen,julian=julian                       ) 

             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

        CASE (GFDLSWSCHEME)

             CALL wrf_debug (100, 'CALL gfdlsw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QS) .AND. PRESENT(qs)   .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc) ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QS ) THEN
                 gfdl_sw = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc_temp,QI=qi,QS=qs                      &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,HBOTR=hbotr, HTOPR=htopr                          &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,XTIME=xtime,JULIAN=julian                         &
                 ,JULYR=julyr,JULDAY=julday                         &
                 ,GFDL_LW=gfdl_lw,GFDL_SW=gfdl_sw                   &
                 ,CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach         &
                 ,ACFRST=acfrst,NCFRST=ncfrst                       &
                 ,ACFRCV=acfrcv,NCFRCV=ncfrcv                       &
                 ,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean         &
                 ,THRATEN=rthraten,THRATENLW=rthratenlw             &
                 ,THRATENSW=rthratensw                              &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
               ELSE
                 CALL wrf_error_fatal3("<stdin>",1658,&
'Can not call ETARA (2a). Missing moisture fields.')
               ENDIF
             ELSE
               CALL wrf_error_fatal3("<stdin>",1662,&
'Can not call ETARA (2b). Missing moisture fields.')
             ENDIF


        CASE (0)

           
           
           IF (lw_physics /= HELDSUAREZ) THEN
             WRITE( wrf_err_message , * ) &
'You have selected a longwave radiation option, but not a shortwave option (sw_physics = 0, lw_physics = ',lw_physics,')'
             CALL wrf_error_fatal3("<stdin>",1674,&
wrf_err_message )
           END IF



        CASE (FLGSWSCHEME)
          flg_sw = .true.



        CASE DEFAULT

             WRITE( wrf_err_message , * ) 'The shortwave option does not exist: sw_physics = ', sw_physics
             CALL wrf_error_fatal3("<stdin>",1688,&
wrf_err_message )

     END SELECT swrad_select    

     IF (sw_physics .eq. goddardswscheme) THEN
          IF ( PRESENT (tswdn) ) THEN
        DO j=jts,jte
        DO i=its,ite
           tswdn(i,j) = erbe_out(i,j,5)    
           tswup(i,j) = erbe_out(i,j,6)    
           sswdn(i,j) = erbe_out(i,j,7)    
           sswup(i,j) = erbe_out(i,j,8)    
        ENDDO
        ENDDO
          ENDIF
     ENDIF

     IF (sw_physics .gt. 0 .and. .not.gfdl_sw) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENSW(I,K,J)=RTHRATEN(I,K,J)-RTHRATENLW(I,K,J)
        ENDDO
        ENDDO
        ENDDO

        DO j=jts,jte
        DO i=its,ite
           SWDOWN(I,J)=GSW(I,J)/(1.-ALBEDO(I,J))
        ENDDO
        ENDDO
     ENDIF


     
     
     if ((sw_physics .ne. rrtmg_swscheme) .and. (sw_physics .ne. goddardswscheme)) then
        do j=jts,jte
           do i=its,ite
              if (coszen(i,j).gt.1e-3) then
                 ioh=solcon*coszen(i,j) 
                 kt=swdown(i,j)/max(ioh,1e-3) 
                 
                 airmass=exp(-ht(i,j)/8434.5)/(coszen(i,j)+ &
                        0.50572*(asin(coszen(i,j))*57.295779513082323+6.07995)**(-1.6364))
                 
                 kt=kt/(0.1+1.031*exp(-1.4/(0.9+(9.4/max(airmass,1e-3)))))
                 
                 kd=0.952-1.041*exp(-exp(2.300-4.702*kt))
                 swddif(i,j)=kd*swdown(i,j)
                 swddir(i,j)=(1.-kd)*swdown(i,j)
                 swddni(i,j)=swddir(i,j)/max(coszen(i,j),1e-4)
              end if
           end do
        end do
     end if

       IF ( PRESENT( qc  ) .AND. PRESENT( qc_cu ) ) THEN 
           IF ( icloud_cu .NE. 0 ) THEN
           DO j=jts,jte
           DO k=kts,kte
           DO i=its,ite
             qc(i,k,j) = qc_save(i,k,j)
           ENDDO
           ENDDO
           ENDDO
           ENDIF
        ENDIF
        IF ( PRESENT( qi  ) .AND. PRESENT( qi_cu ) ) THEN
           IF ( icloud_cu .NE. 0 ) THEN
           DO j=jts,jte
           DO k=kts,kte
           DO i=its,ite
             qi(i,k,j) = qi_save(i,k,j)
           ENDDO
           ENDDO
           ENDDO
           ENDIF
        ENDIF

     
     
     if (swint_opt.eq.1) then
        
        call update_swinterp_parameters(ims,ime,jms,jme,its,ite,jts,jte,   &
                                        coszen,coszen_loc,swddir,swdown,   &
                                        swddir_ref,bb,Bx,swdown_ref,gg,Gx, &
                                        coszen_ref                         )
     end if

   ENDDO
   !$OMP END PARALLEL DO

   IF ( allocated(tauaer_sw) ) deallocate(tauaer_sw)
   IF ( allocated(ssaaer_sw) ) deallocate(ssaaer_sw)
   IF ( allocated(asyaer_sw) ) deallocate(asyaer_sw)

   ENDIF Radiation_step

 
 
 if (swint_opt .eq. 1) then
    call wrf_debug(100,'SW surface irradiance interpolation')

    
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)
    do ij = 1,num_tiles
      its = i_start(ij)
      ite = i_end(ij)
      jts = j_start(ij)
      jte = j_end(ij)
      call interp_sw_radiation(ims,ime,jms,jme,its,ite,jts,jte,  &
                               coszen_ref,coszen_loc,swddir_ref, &
                               bb,Bx,swdown_ref,gg,Gx,           &
                               swdown,swddir,swddni,swddif       )
    enddo
    !$OMP END PARALLEL DO
 end if

     accumulate_lw_select: SELECT CASE(lw_physics)

     CASE (CAMLWSCHEME,RRTMG_LWSCHEME)
   IF(PRESENT(LWUPTC))THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACLWUPT(I,J) = ACLWUPT(I,J) + LWUPT(I,J)*DT
           ACLWUPTC(I,J) = ACLWUPTC(I,J) + LWUPTC(I,J)*DT
           ACLWDNT(I,J) = ACLWDNT(I,J) + LWDNT(I,J)*DT
           ACLWDNTC(I,J) = ACLWDNTC(I,J) + LWDNTC(I,J)*DT
           ACLWUPB(I,J) = ACLWUPB(I,J) + LWUPB(I,J)*DT
           ACLWUPBC(I,J) = ACLWUPBC(I,J) + LWUPBC(I,J)*DT
           ACLWDNB(I,J) = ACLWDNB(I,J) + LWDNB(I,J)*DT
           ACLWDNBC(I,J) = ACLWDNBC(I,J) + LWDNBC(I,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF
     CASE DEFAULT
     END SELECT accumulate_lw_select

     accumulate_sw_select: SELECT CASE(sw_physics)

     CASE (CAMSWSCHEME,RRTMG_SWSCHEME)
   IF(PRESENT(SWUPTC))THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACSWUPT(I,J) = ACSWUPT(I,J) + SWUPT(I,J)*DT
           ACSWUPTC(I,J) = ACSWUPTC(I,J) + SWUPTC(I,J)*DT
           ACSWDNT(I,J) = ACSWDNT(I,J) + SWDNT(I,J)*DT
           ACSWDNTC(I,J) = ACSWDNTC(I,J) + SWDNTC(I,J)*DT
           ACSWUPB(I,J) = ACSWUPB(I,J) + SWUPB(I,J)*DT
           ACSWUPBC(I,J) = ACSWUPBC(I,J) + SWUPBC(I,J)*DT
           ACSWDNB(I,J) = ACSWDNB(I,J) + SWDNB(I,J)*DT
           ACSWDNBC(I,J) = ACSWDNBC(I,J) + SWDNBC(I,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF

     CASE DEFAULT
     END SELECT accumulate_sw_select

   END SUBROUTINE radiation_driver

   SUBROUTINE pre_radiation_driver ( grid, config_flags                   &
              ,itimestep, ra_call_offset                                  &
              ,XLAT, XLONG, GMT, julian, xtime, RADT, STEPRA              &
              ,ht,dx,dy,sina,cosa,shadowmask,slope_rad ,topo_shading      &
              ,shadlen,ht_shad,ht_loc                                     &
              ,ht_shad_bxs, ht_shad_bxe                                   &
              ,ht_shad_bys, ht_shad_bye                                   &
              ,nested, min_ptchsz                                         &
              ,spec_bdy_width                                             &
              ,ids, ide, jds, jde, kds, kde                               &
              ,ims, ime, jms, jme, kms, kme                               &
              ,ips, ipe, jps, jpe, kps, kpe                               &
              ,i_start, i_end                                             &
              ,j_start, j_end                                             &
              ,kts, kte                                                   &
              ,num_tiles                                                  )

   USE module_domain  , ONLY : domain
   USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks,wrf_dm_minval_integer
   USE module_comm_dm   , ONLY : halo_toposhad_sub
   USE module_bc
   USE module_model_constants

   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                                         kts,kte, &
                                       num_tiles

   TYPE(domain)                   , INTENT(INOUT)  :: grid
   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER, INTENT(IN  ) :: itimestep, ra_call_offset, stepra,    &
                            slope_rad, topo_shading,              &
                            spec_bdy_width

   INTEGER, INTENT(INOUT) :: min_ptchsz

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
                i_start,i_end,j_start,j_end

   REAL, INTENT(IN  )   :: GMT, radt, julian, xtime, dx, dy, shadlen

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                              HT, &
                                                            SINA, &
                                                            COSA

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad,ht_loc

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ),        &
                      INTENT(IN   ) :: ht_shad_bxs, ht_shad_bxe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ),        &
                      INTENT(IN   ) :: ht_shad_bys, ht_shad_bye

   INTEGER, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT)  ::                            shadowmask

   LOGICAL,      INTENT(IN   )    :: nested



   INTEGER :: niter,ni,psx,psy,idum,jdum,i,j,ij
   REAL :: DECLIN,SOLCON


   if (itimestep .eq. 1) then
     psx = ipe-ips+1
     psy = jpe-jps+1
     min_ptchsz = min(psx,psy)
     idum = 0
     jdum = 0
   endif

   if (itimestep .eq. 1) then
     call wrf_dm_minval_integer (psx,idum,jdum)
     call wrf_dm_minval_integer (psy,idum,jdum)
     min_ptchsz = min(psx,psy)
   endif


   
   if ((topo_shading.eq.1).and.(itimestep .eq. 1 .or. &
        mod(itimestep,STEPRA) .eq. 1 + ra_call_offset))  then



   
   CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,DEGRAD,DPD)
   

     do j=jms,jme
     do i=ims,ime
       ht_loc(i,j) = ht(i,j)
     enddo
     enddo

     if ((ids.eq.ips).and.(ide.eq.ipe).and.(jds.eq.jps).and.(jde.eq.jpe)) then
       niter = 1
     else
       niter = int(shadlen/(dx*min_ptchsz)+3)
     endif



    IF( nested ) THEN

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , num_tiles

           CALL spec_bdyfield(ht_shad,                         &
                               ht_shad_bxs, ht_shad_bxe,       &
                               ht_shad_bys, ht_shad_bye,       &
                               'm', config_flags, spec_bdy_width, 2,&
                               ids,ide, jds,jde, 1  ,1  ,  & 
                               ims,ime, jms,jme, 1  ,1  ,  & 
                               ips,ipe, jps,jpe, 1  ,1  ,  & 
                               i_start(ij), i_end(ij),         &
                               j_start(ij), j_end(ij),         &
                               1    , 1             )
      ENDDO
    ENDIF

     do ni = 1, niter

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
         do ij = 1 , num_tiles

         call toposhad_init (ht_shad,ht_loc,                         &
                       shadowmask,nested,ni,                         &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,min(ipe,ide-1), jps,min(jpe,jde-1), kps,kpe,      &
                       i_start(ij),min(i_end(ij), ide-1),j_start(ij),&
                       min(j_end(ij), jde-1), kts, kte               )

         enddo
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
       do ij = 1 , num_tiles

       call toposhad (xlat,xlong,sina,cosa,xtime,gmt,radt,declin,    &
                       dx,dy,ht_shad,ht_loc,ni,                      &
                       shadowmask,shadlen,                           &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,min(ipe,ide-1), jps,min(jpe,jde-1), kps,kpe,        &
                       i_start(ij),min(i_end(ij), ide-1),j_start(ij),&
                       min(j_end(ij), jde-1), kts, kte               )

       enddo
   !$OMP END PARALLEL DO







CALL HALO_TOPOSHAD_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

     enddo
   endif

   END SUBROUTINE pre_radiation_driver





   SUBROUTINE radconst(XTIME,DECLIN,SOLCON,JULIAN,                   &
                       DEGRAD,DPD                                    )

   USE module_wrf_error
   IMPLICIT NONE



   REAL, INTENT(IN   )      ::       DEGRAD,DPD,XTIME,JULIAN
   REAL, INTENT(OUT  )      ::       DECLIN,SOLCON
   REAL                     ::       OBECL,SINOB,SXLONG,ARG,  &
                                     DECDEG,DJUL,RJUL,ECCFAC







   DECLIN=0.
   SOLCON=0.


        
   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)
        

        
   IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)
   IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)
   SXLONG=SXLONG*DEGRAD
   ARG=SINOB*SIN(SXLONG)
   DECLIN=ASIN(ARG)
   DECDEG=DECLIN/DEGRAD

   DJUL=JULIAN*360./365.
   RJUL=DJUL*DEGRAD
   ECCFAC=1.000110+0.034221*COS(RJUL)+0.001280*SIN(RJUL)+0.000719*  &
          COS(2*RJUL)+0.000077*SIN(2*RJUL)
   SOLCON=1370.*ECCFAC
   
   END SUBROUTINE radconst


   SUBROUTINE calc_coszen(ims,ime,jms,jme,its,ite,jts,jte,  &
                          julian,xtime,gmt, &
                          declin,degrad,xlon,xlat,coszen,hrang)
       
       implicit none
       integer, intent(in) :: ims,ime,jms,jme,its,ite,jts,jte
       real, intent(in)    :: julian,declin,xtime,gmt,degrad
       real, dimension(ims:ime,jms:jme), intent(in)    :: xlat,xlon
       real, dimension(ims:ime,jms:jme), intent(inout) :: coszen,hrang

       integer :: i,j
       real    :: da,eot,xt24,tloctm,xxlat

       da=6.2831853071795862*(julian-1)/365.
       eot=(0.000075+0.001868*cos(da)-0.032077*sin(da) &
            -0.014615*cos(2*da)-0.04089*sin(2*da))*(229.18)
       xt24=mod(xtime,1440.)+eot
       do j=jts,jte
          do i=its,ite
             tloctm=gmt+xt24/60.+xlon(i,j)/15.
             hrang(i,j)=15.*(tloctm-12.)*degrad
             xxlat=xlat(i,j)*degrad
             coszen(i,j)=sin(xxlat)*sin(declin) &
                        +cos(xxlat)*cos(declin) *cos(hrang(i,j))
          enddo
       enddo
   END SUBROUTINE calc_coszen

   subroutine update_swinterp_parameters(ims,ime,jms,jme,its,ite,jts,jte, &
                                         coszen,coszen_loc,swddir,swdown, &
                                         swddir_ref,bb,Bx,                &
                                         swdown_ref,gg,Gx,                &
                                         coszen_ref                       )
      
      implicit None
      integer, intent(in) :: ims,ime,jms,jme,its,ite,jts,jte
      real, dimension(ims:ime,jms:jme), intent(in)    :: coszen,coszen_loc,swddir,swdown
      real, dimension(ims:ime,jms:jme), intent(inout) :: swddir_ref,bb,Bx, &
                                                         swdown_ref,gg,Gx, &
                                                         coszen_ref

      integer :: i,j
      real :: swddir_0,swdown_0,coszen_0
      real, parameter :: coszen_min=1e-4

      do j=jts,jte
         do i=its,ite
            if ((coszen(i,j).gt.coszen_min) .and. (coszen_loc(i,j).gt.coszen_min)) then
               
               if (Bx(i,j).le.0) then
                  swddir_0 =(coszen_loc(i,j)/coszen(i,j))*swddir(i,j) 
                  coszen_0 =coszen_loc(i,j)
               else
                  swddir_0 =swddir_ref(i,j)
                  coszen_0 =coszen_ref(i,j)
               end if
               if ((coszen(i,j)/coszen_0).lt.1.) then
                  bb(i,j) =log(max(1.,swddir(i,j))/max(1.,swddir_0)) / log(min(1.-1e-4,coszen(i,j)/coszen_0))
               elseif ((coszen(i,j)/coszen_0).gt.1) then
                  bb(i,j) =log(max(1.,swddir(i,j))/max(1.,swddir_0)) / log(max(1.+1e-4,coszen(i,j)/coszen_0))
               else
                  bb(i,j) =0.
               end if
               bb(i,j) =max(-.5,min(2.5,bb(i,j)))
               Bx(i,j) =swddir(i,j)/(coszen(i,j)**bb(i,j))

               
               
               

               
               if (Gx(i,j).le.0) then
                  swdown_0 =(coszen_loc(i,j)/coszen(i,j))*swdown(i,j) 
                  coszen_0 =coszen_loc(i,j)
               else
                  swdown_0 =swdown_ref(i,j)
                  coszen_0 =coszen_ref(i,j)
               end if
               if ((coszen(i,j)/coszen_0).lt.1.) then
                  gg(i,j) =log(max(1.,swdown(i,j))/max(1.,swdown_0)) / log(min(1.-1e-4,coszen(i,j)/coszen_0))
               elseif ((coszen(i,j)/coszen_0).gt.1) then
                  gg(i,j) =log(max(1.,swdown(i,j))/max(1.,swdown_0)) / log(max(1.+1e-4,coszen(i,j)/coszen_0))
               else
                  gg(i,j) =0.
               end if
               gg(i,j) =max(-.5,min(2.5,gg(i,j)))
               Gx(i,j) =swdown(i,j)/(coszen(i,j)**gg(i,j))
            else
               Bx(i,j) =0.
               bb(i,j) =0.
               Gx(i,j) =0.
               gg(i,j) =0.
            end if

            
            coszen_ref(i,j) =coszen(i,j)
            swdown_ref(i,j) =swdown(i,j)
            swddir_ref(i,j) =swddir(i,j)

            
            
            
            
            
            

         end do
      end do

   end subroutine update_swinterp_parameters

   subroutine interp_sw_radiation(ims,ime,jms,jme,its,ite,jts,jte,  &
                                  coszen_ref,coszen_loc,swddir_ref, &
                                  bb,Bx,swdown_ref,gg,Gx,           &
                                  swdown,swddir,swddni,swddif       )
      
      implicit None
      integer, intent(in) :: ims,ime,jms,jme,its,ite,jts,jte
      real, dimension(ims:ime,jms:jme), intent(in) :: coszen_ref,coszen_loc, &
                                                      swddir_ref,Bx,bb,      &
                                                      swdown_ref,Gx,gg       
      real, dimension(ims:ime,jms:jme), intent(inout) :: swddir,swdown, &
                                                         swddif,swddni    

      integer :: i,j
      real, parameter :: coszen_min=1e-4

      do j=jts,jte
         do i=its,ite
            
            if ((coszen_ref(i,j).gt.coszen_min) .and. (coszen_loc(i,j).gt.coszen_min)) then
               if ((bb(i,j).eq.-0.5).or.(bb(i,j).eq.2.5)) then
                  swddir(i,j) =(coszen_loc(i,j)/coszen_ref(i,j))*swddir_ref(i,j)
               else
                  swddir(i,j) =Bx(i,j)*(coszen_loc(i,j)**bb(i,j))
               end if
               if ((gg(i,j).eq.-0.5).or.(gg(i,j).eq.2.5)) then
                  swdown(i,j) =(coszen_loc(i,j)/coszen_ref(i,j))*swdown_ref(i,j)
               else
                  swdown(i,j) =Gx(i,j)*(coszen_loc(i,j)**gg(i,j))
               end if
               swddif(i,j) =swdown(i,j)-swddir(i,j)
               swddni(i,j) =swddir(i,j)/coszen_loc(i,j)
            else
               swddir(i,j) =0.
               swdown(i,j) =0.
               swddif(i,j) =0.
               swddni(i,j) =0.
            end if
         end do
      end do
   end subroutine interp_sw_radiation





   SUBROUTINE cal_cldfra2(CLDFRA,QC,QI,F_QC,F_QI,                    &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )

   IMPLICIT NONE

   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QI, &
                                                                 QC

   LOGICAL,INTENT(IN) :: F_QC,F_QI

   REAL thresh
   INTEGER:: i,j,k















     thresh=1.0e-6

     IF ( f_qi .AND. f_qc ) THEN
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           IF ( QC(i,k,j)+QI(I,k,j) .gt. thresh) THEN
              CLDFRA(i,k,j)=1.
           ELSE
              CLDFRA(i,k,j)=0.
           ENDIF
        ENDDO
        ENDDO
        ENDDO
     ELSE IF ( f_qc ) THEN
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           IF ( QC(i,k,j) .gt. thresh) THEN
              CLDFRA(i,k,j)=1.
           ELSE
              CLDFRA(i,k,j)=0.
           ENDIF
        ENDDO
        ENDDO
        ENDDO
     ELSE
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           CLDFRA(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

   END SUBROUTINE cal_cldfra2











   SUBROUTINE cal_cldfra1(CLDFRA, QV, QC, QI, QS,                     &
                         F_QV, F_QC, F_QI, F_QS, t_phy, p_phy,       &
                         F_ICE_PHY,F_RAIN_PHY,                       &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )

   IMPLICIT NONE

   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QV, &
                                                                 QI, &
                                                                 QC, &
                                                                 QS, &
                                                              t_phy, &
                                                              p_phy




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

   LOGICAL,OPTIONAL,INTENT(IN) :: F_QC,F_QI,F_QV,F_QS


   INTEGER:: i,j,k
   REAL    :: RHUM, tc, esw, esi, weight, qvsw, qvsi, qvs_weight, QIMID, QWMID, QCLD, DENOM, ARG, SUBSAT

   REAL    ,PARAMETER :: ALPHA0=100., GAMMA=0.49, QCLDMIN=1.E-12,    &
                                        PEXP=0.25, RHGRID=1.0
   REAL    , PARAMETER ::  SVP1=0.61078
   REAL    , PARAMETER ::  SVP2=17.2693882
   REAL    , PARAMETER ::  SVPI2=21.8745584
   REAL    , PARAMETER ::  SVP3=35.86
   REAL    , PARAMETER ::  SVPI3=7.66
   REAL    , PARAMETER ::  SVPT0=273.15
   REAL    , PARAMETER ::  r_d = 287.
   REAL    , PARAMETER ::  r_v = 461.6
   REAL    , PARAMETER ::  ep_2=r_d/r_v









































    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      tc         = t_phy(i,k,j) - SVPT0
      esw     = 1000.0 * SVP1 * EXP( SVP2  * tc / ( t_phy(i,k,j) - SVP3  ) )
      esi     = 1000.0 * SVP1 * EXP( SVPI2 * tc / ( t_phy(i,k,j) - SVPI3 ) )
      QVSW = EP_2 * esw / ( p_phy(i,k,j) - esw )
      QVSI = EP_2 * esi / ( p_phy(i,k,j) - esi )

      IF ( PRESENT(F_QI) .and. PRESENT(F_QC) .and. PRESENT(F_QS) ) THEN


         IF ( F_QI .and. F_QC .and. F_QS) THEN
            QCLD = QI(i,k,j)+QC(i,k,j)+QS(i,k,j)
            IF (QCLD .LT. QCLDMIN) THEN
               weight = 0.
            ELSE
               weight = (QI(i,k,j)+QS(i,k,j)) / QCLD
            ENDIF
         ENDIF



         IF ( F_QC .and. .not. F_QI .and. .not. F_QS ) THEN
            QCLD = QC(i,k,j)
            IF (QCLD .LT. QCLDMIN) THEN
               weight = 0.
            ELSE
               if (t_phy(i,k,j) .gt. 273.15) weight = 0.
               if (t_phy(i,k,j) .le. 273.15) weight = 1.
            ENDIF
         ENDIF


         IF ( F_QC .and. .not. F_QI .and. F_QS .and. PRESENT(F_ICE_PHY) ) THEN






           QIMID = QS(i,k,j)
           QWMID = QC(i,k,j)







           QCLD=QWMID+QIMID
           IF (QCLD .LT. QCLDMIN) THEN
              weight = 0.
           ELSE
              weight = F_ICE_PHY(i,k,j)
           ENDIF
         ENDIF

      ELSE
         CLDFRA(i,k,j)=0.

      ENDIF 


      QVS_WEIGHT = (1-weight)*QVSW + weight*QVSI
      RHUM=QV(i,k,j)/QVS_WEIGHT   



      IF (QCLD .LT. QCLDMIN) THEN



        CLDFRA(i,k,j)=0.
      ELSEIF(RHUM.GE.RHGRID)THEN




        CLDFRA(i,k,j)=1.
      ELSE




        SUBSAT=MAX(1.E-10,RHGRID*QVS_WEIGHT-QV(i,k,j))
        DENOM=(SUBSAT)**GAMMA
        ARG=MAX(-6.9, -ALPHA0*QCLD/DENOM)    

        RHUM=MAX(1.E-10, RHUM)
        CLDFRA(i,k,j)=(RHUM/RHGRID)**PEXP*(1.-EXP(ARG))



        IF (CLDFRA(i,k,j) .LT. .01) CLDFRA(i,k,j)=0.
      ENDIF          
    ENDDO          
    ENDDO          
    ENDDO          

   END SUBROUTINE cal_cldfra1


   SUBROUTINE toposhad_init(ht_shad,ht_loc,shadowmask,nested,iter,   &
                       ids,ide, jds,jde, kds,kde,                    & 
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,ipe, jps,jpe, kps,kpe,                    &
                       its,ite, jts,jte, kts,kte                     )

   USE module_model_constants

 implicit none

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       its,ite, jts,jte, kts,kte

   LOGICAL, INTENT(IN)      :: nested

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad, ht_loc

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: shadowmask
   INTEGER, INTENT(IN)      :: iter



   INTEGER :: i, j

 if (iter.eq.1) then


   do j=jts,jte
   do i=its,ite
     shadowmask(i,j) = 0
   ENDDO
   ENDDO



   IF ( nested ) THEN  
     do j=max(jts,jds+2),min(jte,jde-3)
     do i=max(its,ids+2),min(ite,ide-3)
       ht_shad(i,j) = ht_loc(i,j)-0.001
     ENDDO
     ENDDO
   ELSE
     do j=jts,jte
     do i=its,ite
       ht_shad(i,j) = ht_loc(i,j)-0.001
     ENDDO
     ENDDO
   ENDIF

   IF ( nested ) THEN  
     if (its.eq.ids) then
       do j=jts,jte
         if (ht_shad(its,j) .gt. ht_loc(its,j)) then
           shadowmask(its,j) = 1
           ht_loc(its,j) = ht_shad(its,j)
         endif
         if (ht_shad(its+1,j) .gt. ht_loc(its+1,j)) then
           shadowmask(its+1,j) = 1
           ht_loc(its+1,j) = ht_shad(its+1,j)
         endif
       enddo
     endif
     if (ite.eq.ide-1) then
       do j=jts,jte
         if (ht_shad(ite,j) .gt. ht_loc(ite,j)) then
           shadowmask(ite,j) = 1
           ht_loc(ite,j) = ht_shad(ite,j)
         endif
         if (ht_shad(ite-1,j) .gt. ht_loc(ite-1,j)) then
           shadowmask(ite-1,j) = 1
           ht_loc(ite-1,j) = ht_shad(ite-1,j)
         endif
       enddo
     endif
     if (jts.eq.jds) then
       do i=its,ite
         if (ht_shad(i,jts) .gt. ht_loc(i,jts)) then
           shadowmask(i,jts) = 1
           ht_loc(i,jts) = ht_shad(i,jts)
         endif
         if (ht_shad(i,jts+1) .gt. ht_loc(i,jts+1)) then
           shadowmask(i,jts+1) = 1
           ht_loc(i,jts+1) = ht_shad(i,jts+1)
         endif
       enddo
     endif
     if (jte.eq.jde-1) then
       do i=its,ite
         if (ht_shad(i,jte) .gt. ht_loc(i,jte)) then
           shadowmask(i,jte) = 1
           ht_loc(i,jte) = ht_shad(i,jte)
         endif
         if (ht_shad(i,jte-1) .gt. ht_loc(i,jte-1)) then
           shadowmask(i,jte-1) = 1
           ht_loc(i,jte-1) = ht_shad(i,jte-1)
         endif
       enddo
     endif
   ENDIF

 else




   if ((its.ne.ids).and.(its.eq.ips)) then
     do j=jts-2,jte+2
       ht_loc(its-1,j) = max(ht_loc(its-1,j),ht_shad(its-1,j))
       ht_loc(its-2,j) = max(ht_loc(its-2,j),ht_shad(its-2,j))
     enddo
   endif
   if ((ite.ne.ide-1).and.(ite.eq.ipe)) then
     do j=jts-2,jte+2
       ht_loc(ite+1,j) = max(ht_loc(ite+1,j),ht_shad(ite+1,j))
       ht_loc(ite+2,j) = max(ht_loc(ite+2,j),ht_shad(ite+2,j))
     enddo
   endif
   if ((jts.ne.jds).and.(jts.eq.jps)) then
     do i=its-2,ite+2
       ht_loc(i,jts-1) = max(ht_loc(i,jts-1),ht_shad(i,jts-1))
       ht_loc(i,jts-2) = max(ht_loc(i,jts-2),ht_shad(i,jts-2))
     enddo
   endif
   if ((jte.ne.jde-1).and.(jte.eq.jpe)) then
     do i=its-2,ite+2
       ht_loc(i,jte+1) = max(ht_loc(i,jte+1),ht_shad(i,jte+1))
       ht_loc(i,jte+2) = max(ht_loc(i,jte+2),ht_shad(i,jte+2))
     enddo
   endif

 endif

   END SUBROUTINE toposhad_init

   SUBROUTINE toposhad(xlat,xlong,sina,cosa,xtime,gmt,radfrq,declin, &
                       dx,dy,ht_shad,ht_loc,iter,                    &
                       shadowmask,shadlen,                    &
                       ids,ide, jds,jde, kds,kde,                    & 
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,ipe, jps,jpe, kps,kpe,                    &
                       its,ite, jts,jte, kts,kte                     )


   USE module_model_constants

 implicit none

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,   INTENT(IN) ::      iter

   REAL, INTENT(IN)      ::        RADFRQ,XTIME,DECLIN,dx,dy,gmt,shadlen

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: XLAT, XLONG, sina, cosa

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad,ht_loc

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: shadowmask



   REAL :: pi, xt24, wgt, ri, rj, argu, sol_azi, topoelev, dxabs, tloctm, hrang, xxlat, csza
   INTEGER :: gpshad, ii, jj, i1, i2, j1, j2, i, j



 XT24=MOD(XTIME+RADFRQ*0.5,1440.)
 pi = 4.*atan(1.)
 gpshad = int(shadlen/dx+1.)

 if (iter.eq.1) then  


   j_loop1: DO J=jts,jte
   i_loop1: DO I=its,ite

     TLOCTM=GMT+XT24/60.+XLONG(i,j)/15.
     HRANG=15.*(TLOCTM-12.)*DEGRAD
     XXLAT=XLAT(i,j)*DEGRAD
     CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)

     if (csza.lt.1.e-2) then   
     shadowmask(i,j) = 0
     ht_shad(i,j) = ht_loc(i,j)-0.001
     goto 120
     endif



     argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))
     if (argu.gt.1) argu = 1
     if (argu.lt.-1) argu = -1
     sol_azi = sign(acos(argu),sin(HRANG))+pi  
     if (cosa(i,j).ge.0) then
       sol_azi = sol_azi + asin(sina(i,j))  
     else
       sol_azi = sol_azi + pi - asin(sina(i,j)) 
     endif



          if ((sol_azi.gt.1.75*pi).or.(sol_azi.lt.0.25*pi)) then 

            do jj = j+1,j+gpshad
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri) 
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.ge.jpe+1).or.(i1.le.ips-1).or.(i2.ge.ipe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.0.75*pi) then  
            do ii = i+1,i+gpshad
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.ge.ipe+1).or.(j1.le.jps-1).or.(j2.ge.jpe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.1.25*pi) then 
            do jj = j-1,j-gpshad,-1
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri)
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.le.jps-1).or.(i1.le.ips-1).or.(i2.ge.ipe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else                          
            do ii = i-1,i-gpshad,-1
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.le.ips-1).or.(j1.le.jps-1).or.(j2.ge.jpe+1)) then
                if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo
          endif

 120      continue

   ENDDO i_loop1
   ENDDO j_loop1

 else   


   j_loop2: DO J=jts,jte
   i_loop2: DO I=its,ite



       TLOCTM=GMT+XT24/60.+XLONG(i,j)/15.
       HRANG=15.*(TLOCTM-12.)*DEGRAD
       XXLAT=XLAT(i,j)*DEGRAD
       CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)



       argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))
       if (argu.gt.1) argu = 1
       if (argu.lt.-1) argu = -1
       sol_azi = sign(acos(argu),sin(HRANG))+pi  
       if (cosa(i,j).ge.0) then
         sol_azi = sol_azi + asin(sina(i,j))  
       else
         sol_azi = sol_azi + pi - asin(sina(i,j)) 
       endif



          if ((sol_azi.gt.1.75*pi).or.(sol_azi.lt.0.25*pi)) then 

            do jj = j+1,j+gpshad
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri) 
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.ge.min(jde,jpe+3)).or.(i1.le.max(ids-1,ips-3)).or.(i2.ge.min(ide,ipe+3))) goto 220
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.0.75*pi) then  
            do ii = i+1,i+gpshad
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.ge.min(ide,ipe+3)).or.(j1.le.max(jds-1,jps-3)).or.(j2.ge.min(jde,jpe+3))) goto 220
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.1.25*pi) then 
            do jj = j-1,j-gpshad,-1
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri)
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.le.max(jds-1,jps-3)).or.(i1.le.max(ids-1,ips-3)).or.(i2.ge.min(ide,ipe+3))) goto 220
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else                          
            do ii = i-1,i-gpshad,-1
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.le.max(ids-1,ips-3)).or.(j1.le.max(jds-1,jps-3)).or.(j2.ge.min(jde,jpe+3))) goto 220
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo
          endif

 220      continue


   ENDDO i_loop2
   ENDDO j_loop2

 endif 

   END SUBROUTINE toposhad

SUBROUTINE ozn_time_int(julday,julian,ozmixm,ozmixt,levsiz,num_months,  &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )







   IMPLICIT NONE

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(IN   ) ::                                  ozmixm

   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme ),      &
          INTENT(OUT  ) ::                                  ozmixt

   
   REAL      :: intJULIAN
   integer   :: np1,np,nm,m,k,i,j
   integer   :: IJUL
   integer, dimension(12) ::  date_oz
   data date_oz/16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350/
   real, parameter :: daysperyear = 365.  
   real      :: cdayozp, cdayozm
   real      :: fact1, fact2, deltat
   logical   :: finddate
   logical   :: ozncyc
   CHARACTER(LEN=256) :: msgstr

   ozncyc = .true.
   
   intJULIAN = JULIAN + 1.0       

   IJUL=INT(intJULIAN)


   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   intJULIAN=intJULIAN+IJUL
   np1=1
   finddate=.false.


   do m=1,12
      if(date_oz(m).gt.intjulian.and..not.finddate) then
        np1=m
        finddate=.true.
      endif
   enddo
   cdayozp=date_oz(np1)

   if(np1.gt.1) then
      cdayozm=date_oz(np1-1)
      np=np1
      nm=np-1
   else
      cdayozm=date_oz(12)
      np=np1
      nm=12
   endif







   if (ozncyc .and. np1 == 1) then                      
      deltat = cdayozp + daysperyear - cdayozm
      if (intjulian > cdayozp) then                     
         fact1 = (cdayozp + daysperyear - intjulian)/deltat
         fact2 = (intjulian - cdayozm)/deltat
      else                                              
         fact1 = (cdayozp - intjulian)/deltat
         fact2 = (intjulian + daysperyear - cdayozm)/deltat
      end if
   else
      deltat = cdayozp - cdayozm
      fact1 = (cdayozp - intjulian)/deltat
      fact2 = (intjulian - cdayozm)/deltat
   end if



      do j=jts,jte
      do k=1,levsiz
      do i=its,ite
            ozmixt(i,k,j) = ozmixm(i,k,j,nm)*fact1 + ozmixm(i,k,j,np)*fact2
      end do
      end do
      end do

END SUBROUTINE ozn_time_int

SUBROUTINE ozn_p_int(p ,pin, levsiz, ozmixt, o3vmr, &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )











   implicit none




   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   integer, intent(in) :: levsiz              

   real, intent(in) :: p(ims:ime,kms:kme,jms:jme)   
   real, intent(in) :: pin(levsiz)        
   real, intent(in) :: ozmixt(ims:ime,levsiz,jms:jme) 

   real, intent(out) :: o3vmr(ims:ime,kms:kme,jms:jme) 



   real    pmid(its:ite,kts:kte)
   integer i,j                 
   integer k, kk, kkstart, kout
   integer kupper(its:ite)     
   integer kount               
   integer ncol, pver

   real    dpu                 
   real    dpl                 

   ncol = ite - its + 1
   pver = kte - kts + 1

   do j=jts,jte




   do i=its, ite
      kupper(i) = 1
   end do



      do k = kts,kte
         kk = kte - k + kts
      do i = its,ite
         pmid(i,kk) = p(i,k,j)
      enddo
      enddo

   do k=1,pver

      kout = pver - k + 1





      kkstart = levsiz

      do i=its,ite
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0



      do kk=kkstart,levsiz-1

         do i=its,ite
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do




         if (kount.eq.ncol) then

            do i=its,ite
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               o3vmr(i,kout,j) = (ozmixt(i,kupper(i),j)*dpl + &
                             ozmixt(i,kupper(i)+1,j)*dpu)/(dpl + dpu)
            end do
            goto 35
         end if
      end do






      do i=its,ite
         if (pmid(i,k) .lt. pin(1)) then
            o3vmr(i,kout,j) = ozmixt(i,1,j)*pmid(i,k)/pin(1)
         else if (pmid(i,k) .gt. pin(levsiz)) then
            o3vmr(i,kout,j) = ozmixt(i,levsiz,j)
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            o3vmr(i,kout,j) = (ozmixt(i,kupper(i),j)*dpl + &
                          ozmixt(i,kupper(i)+1,j)*dpu)/(dpl + dpu)
         end if
      end do

      if (kount.gt.ncol) then

         call wrf_error_fatal3("<stdin>",3159,&
'OZN_P_INT: Bad ozone data: non-monotonicity suspected')
      end if
35    continue

   end do
   end do

   return
END SUBROUTINE ozn_p_int

SUBROUTINE aer_time_int(julday,julian,aerodm,aerodt,levsiz,num_months,no_src,  &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )







   IMPLICIT NONE

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months, no_src

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months, no_src ),      &
          INTENT(IN   ) ::                                  aerodm

   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, no_src ),      &
          INTENT(OUT  ) ::                                  aerodt

   
   REAL      :: intJULIAN
   integer   :: np1,np,nm,m,k,i,j,s
   integer   :: IJUL
   integer, dimension(12) ::  date_oz
   data date_oz/16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350/
   real, parameter :: daysperyear = 365.  
   real      :: cdayozp, cdayozm
   real      :: fact1, fact2, deltat
   logical   :: finddate
   logical   :: ozncyc
   CHARACTER(LEN=256) :: msgstr

   ozncyc = .true.
   
   intJULIAN = JULIAN + 1.0       

   IJUL=INT(intJULIAN)


   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   intJULIAN=intJULIAN+IJUL
   np1=1
   finddate=.false.


   do m=1,12
      if(date_oz(m).gt.intjulian.and..not.finddate) then
        np1=m
        finddate=.true.
      endif
   enddo
   cdayozp=date_oz(np1)

   if(np1.gt.1) then
      cdayozm=date_oz(np1-1)
      np=np1
      nm=np-1
   else
      cdayozm=date_oz(12)
      np=np1
      nm=12
   endif







   if (ozncyc .and. np1 == 1) then                      
      deltat = cdayozp + daysperyear - cdayozm
      if (intjulian > cdayozp) then                     
         fact1 = (cdayozp + daysperyear - intjulian)/deltat
         fact2 = (intjulian - cdayozm)/deltat
      else                                              
         fact1 = (cdayozp - intjulian)/deltat
         fact2 = (intjulian + daysperyear - cdayozm)/deltat
      end if
   else
      deltat = cdayozp - cdayozm
      fact1 = (cdayozp - intjulian)/deltat
      fact2 = (intjulian - cdayozm)/deltat
   end if



      do s=1, no_src
      do j=jts,jte
      do k=1,levsiz
      do i=its,ite
            aerodt(i,k,j,s) = aerodm(i,k,j,nm,s)*fact1 + aerodm(i,k,j,np,s)*fact2
      end do
      end do
      end do
      end do

END SUBROUTINE aer_time_int

SUBROUTINE aer_p_int(p ,pin, levsiz, aerodt, aerod, no_src, pf, totaod,   &
                     ids , ide , jds , jde , kds , kde ,     &
                     ims , ime , jms , jme , kms , kme ,     &
                     its , ite , jts , jte , kts , kte )














   implicit none




   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   integer, intent(in) :: levsiz              
   integer, intent(in) :: no_src              

   real, intent(in) :: p(ims:ime,kms:kme,jms:jme)
   real, intent(in) :: pf(ims:ime,kms:kme,jms:jme)
   real, intent(in) :: pin(levsiz)        
   real, intent(in) :: aerodt(ims:ime,levsiz,jms:jme,1:no_src) 

   real, intent(out) :: aerod(ims:ime,kms:kme,jms:jme,1:no_src) 
   real, intent(out) :: totaod(ims:ime,jms:jme)                 



   real    pmid(its:ite,kts:kte)
   integer i,j                 
   integer k, kk, kkstart, kout
   integer kupper(its:ite)     
   integer kount               
   integer ncol, pver, s

   real    dpu                 
   real    dpl                 
   real    dpm                 

   ncol = ite - its + 1
   pver = kte - kts + 1

   do s=1,no_src
   do j=jts,jte



   do i=its, ite
      kupper(i) = 1
   end do




      do k = kts,kte
         kk = kte - k + kts
      do i = its,ite
         pmid(i,kk) = p(i,k,j)*0.01
      enddo
      enddo

   do k=1,pver

      kout = pver - k + 1




      kkstart = levsiz
      do i=its,ite
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0



      do kk=kkstart,levsiz-1
         do i=its,ite
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do




         if (kount.eq.ncol) then
            do i=its,ite
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               dpm = pf(i,kout,j) - pf(i,kout+1,j)
               aerod(i,kout,j,s) = (aerodt(i,kupper(i),j,s)*dpl + &
                             aerodt(i,kupper(i)+1,j,s)*dpu)/(dpl + dpu)
               aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
            end do
            goto 35
         end if
      end do





      do i=its,ite
         if (pmid(i,k) .lt. pin(1)) then
            dpm = pf(i,kout,j) - pf(i,kout+1,j)
            aerod(i,kout,j,s) = aerodt(i,1,j,s)*pmid(i,k)/pin(1)
            aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
         else if (pmid(i,k) .gt. pin(levsiz)) then
            dpm = pf(i,kout,j) - pf(i,kout+1,j)
            aerod(i,kout,j,s) = aerodt(i,levsiz,j,s)
            aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            dpm = pf(i,kout,j) - pf(i,kout+1,j)
            aerod(i,kout,j,s) = (aerodt(i,kupper(i),j,s)*dpl + &
                          aerodt(i,kupper(i)+1,j,s)*dpu)/(dpl + dpu)
            aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
         end if
      end do

      if (kount.gt.ncol) then
         call wrf_error_fatal3("<stdin>",3415,&
'AER_P_INT: Bad aerosol data: non-monotonicity suspected')
      end if
35    continue

   end do
   end do
   end do

   do j=jts,jte
   do i=its,ite
      totaod(i,j) = 0.
   end do
   end do

   do s=1,no_src
   do j=jts,jte
   do k=1,pver
   do i=its,ite
      totaod(i,j) = totaod(i,j) + aerod(i,k,j,s)
   end do
   end do
   end do
   end do

   return
END SUBROUTINE aer_p_int

END MODULE module_radiation_driver
