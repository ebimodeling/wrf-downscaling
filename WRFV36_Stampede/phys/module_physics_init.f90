









MODULE module_physics_init

   USE module_state_description
   USE module_model_constants
   USE module_configure, ONLY : grid_config_rec_type

   
   integer  :: ixcldliq, ixcldice, ixnumliq, ixnumice
   LOGICAL  :: CAM_INITIALIZED = .FALSE.
   LOGICAL  :: WINDFARM_INITIALIZED = .FALSE.


CONTAINS



   SUBROUTINE phy_init ( id, config_flags, DT, restart, zfull, zhalf,     &
                         p_top, TSK,RADT,BLDT,CUDT,MPDT,         &
                         RUCUTEN, RVCUTEN,                       &
                         RTHCUTEN, RQVCUTEN, RQRCUTEN,           &
                         RQCCUTEN, RQSCUTEN, RQICUTEN,           &
                         RUSHTEN,  RVSHTEN,  RTHSHTEN,           &
                         RQVSHTEN, RQRSHTEN, RQCSHTEN,           &
                         RQSSHTEN, RQISHTEN, RQGSHTEN,           &
                         RUBLTEN,RVBLTEN,RTHBLTEN,               &
                         RQVBLTEN,RQCBLTEN,RQIBLTEN,             &
                         RTHRATEN,RTHRATENLW,RTHRATENSW,         &
                         STEPBL,STEPRA,STEPCU,                   &
                         W0AVG, RAINNC, RAINC, RAINCV, RAINNCV,  &
                         SNOWNC, SNOWNCV, GRAUPELNC, GRAUPELNCV, &
                         z_at_q, qnwfa2d, scalar, num_sc,    & 
                         re_cloud, re_ice, re_snow,              & 
                         has_reqc, has_reqi, has_reqs,           & 
                         NCA,swrad_scat,                         &
                         CLDEFI,LOWLYR,                          &
                         MASS_FLUX,                              &
                         RTHFTEN, RQVFTEN,                       &
                         CLDFRA,CLDFRA_OLD,GLW,GSW,EMISS,EMBCK,  & 
                         LU_INDEX,                               &
                         landuse_ISICE, landuse_LUCATS,          &
                         landuse_LUSEAS, landuse_ISN,            &
                         lu_state,                               &
                         XLAT,XLONG,xlong_u,xlat_v,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                         levsiz, n_ozmixm, n_aerosolc, paerlev,  &
                         alevsiz, no_src_types,                  &
                         TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_PBL,  &
                         EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL, &
                         TSLB,ZS,DZS,num_soil_layers,warm_rain,  &
                         adv_moist_cond,is_CAMMGMP_used,         &
                         APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                         APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                         XICE,XICEM,VEGFRA,SNOW,CANWAT,SMSTAV,   &
                         SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,&
                         ACSNOM,IVGTYP,ISLTYP, SFCEVP, SMOIS,    &
                         SH2O, SNOWH, SMFR3D,                    &  
                         SNOALB,                                 &
                         DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                         mp_restart_state,tbpvs_state,tbpvs0_state,&
                         allowed_to_read, moved, start_of_simulation,&
                         LAGDAY,                                 &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         its, ite, jts, jte, kts, kte,           &
                         NUM_URBAN_LAYERS,                       &
                         NUM_URBAN_HI,                           &
                         raincv_a,raincv_b,                      &
                         gd_cloud,gd_cloud2,                     &    
                         gd_cloud_a,gd_cloud2_a,                 &    
                         QC_CU,QI_CU,                            &    
                         ozmixm,pin,                             &    
                         aerodm,pina,                            &    
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,& 
                         RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,         &    
                         RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,       &    
                         FGDT,STEPFG,                            &    
                         cugd_tten,cugd_ttens,cugd_qvten,        &    
                         cugd_qvtens,cugd_qcten,                 &    
                         ISNOWXY, ZSNSOXY, TSNOXY,                        & 
                         SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,          & 
                         CANLIQXY, EAHXY, TAHXY, CMXY,                    & 
                         CHXY, FWETXY, SNEQVOXY, ALBOLDXY, QSNOWXY,       & 
                         WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY, & 
                         STMASSXY, WOODXY, STBLCPXY, FASTCPXY,            & 
                         XSAIXY,                                          & 
                         T2MVXY, T2MBXY, CHSTARXY ,                       & 
                         SMOISEQ  ,SMCWTDXY ,RECHXY, DEEPRECHXY, AREAXY,  & 
                         WTDDT , STEPWTD ,QRFSXY ,QSPRINGSXY ,QSLATXY,    & 
                         FDEPTHXY, RIVERBEDXY, EQZWT, RIVERCONDXY, PEXPXY, & 
                         msftx, msfty,                           &           


                         DZR, DZB, DZG,                          & 
                         TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,    & 
                         QC_URB2D, XXXR_URB2D,XXXB_URB2D,        & 
                         XXXG_URB2D, XXXC_URB2D,                 & 
                         TRL_URB3D, TBL_URB3D, TGL_URB3D,        & 
                         SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,  & 
                         TS_URB2D, FRC_URB2D, UTYPE_URB2D,       & 
                         TRB_URB4D,TW1_URB4D,TW2_URB4D,          & 
                         TGB_URB4D,TLEV_URB3D,QLEV_URB3D,        & 
                         TW1LEV_URB3D,TW2LEV_URB3D,              & 
                         TGLEV_URB3D,TFLEV_URB3D,                & 
                         SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,    & 
                         SFVENT_URB3D,LFVENT_URB3D,              & 
                         SFWIN1_URB3D,SFWIN2_URB3D,              & 
                         SFW1_URB3D,SFW2_URB3D,                  & 
                         SFR_URB3D,SFG_URB3D,                    & 
                         LP_URB2D,HI_URB2D,LB_URB2D,             & 
                         HGT_URB2D,MH_URB2D,STDH_URB2D,          & 
                         LF_URB2D,                               & 
                         A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,        & 
                         A_E_BEP,B_U_BEP,B_V_BEP,                & 
                         B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,        & 
                         DL_U_BEP,SF_BEP,VL_BEP,                 & 
                         TML,T0ML,HML,H0ML,HUML,HVML,TMOML,      & 
                         lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d,       & 
                         snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d, & 
                         z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d,   & 
                         h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d,           & 
                         zi3d,         watsat3d,     csol3d,     tkmg3d,         & 
                         tkdry3d,      tksatu3d,     lake2d,     lakedepth_default, & 
                         lake_min_elev,   lake_depth,                               & 

                         lakemask,   lakeflag,                                   & 

                         lake_depth_flag, use_lakedepth,                         & 
                         sf_surface_mosaic, mosaic_cat, NLCAT,               & 

                         maxpatch,                                           &
                         numc,nump,snl,                                      &
                         snowdp,wtc,wtp,h2osno,t_grnd,t_veg,                 &
                         h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                         h2osoi_liq_s1,                                      &
                         h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                         h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                         h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                         h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                         h2osoi_ice_s1,h2osoi_ice_s2,                        &
                         h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                         h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                         h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                         h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                         t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                         t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                         t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                         t_soisno8,t_soisno9,t_soisno10,                     &
                         dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                         snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                         t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                         t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                         h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                         h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                         h2osoi_vol7,h2osoi_vol8,                            &
                         h2osoi_vol9,h2osoi_vol10,                           &
                         ht,                                                 &
                         ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                         Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                         SWUPsubgrid,lhsoi,lhveg,lhtran,                     &

                         TSK_SAVE,                               & 
                         itimestep,                              & 

                         fdob,                                   & 

                         t00, p00, tlp,                          & 
                         TYR,TYRA,TDLY,TLAG,NYEAR,NDAY,tmn_update,   &
                         ACHFX,ACLHF,ACGRDFLX,                   &
                         nssl_cccn,                              &
                         nssl_alphah,nssl_alphahl,               &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs,                             &
   
                         RQCNCUTEN, RQINCUTEN,                   &
                         rliq,                                   &  
                         cldfra_dp,cldfra_sh                     & 
                         ,te_temf                                & 
                         ,cf3d_temf                              & 
                         ,wm_temf                                & 
                         ,massflux_EDKF, entr_EDKF, detr_EDKF  & 
                         ,thl_up, thv_up, rt_up                & 
                         ,rv_up, rc_up, u_up, v_up, frac_up    & 
                         ,QKE                                  & 
                         ,landusef,landusef2,mosaic_cat_index                                                 & 
                         ,TSK_mosaic,TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic                                     & 
                         ,CANWAT_mosaic,SNOW_mosaic,SNOWH_mosaic,SNOWC_mosaic                                 & 
                         ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic      & 
                         ,TR_URB2D_mosaic,TB_URB2D_mosaic                                                     & 
                         ,TG_URB2D_mosaic,TC_URB2D_mosaic                                                     & 
                         ,QC_URB2D_mosaic                                                                     & 
                         ,TRL_URB3D_mosaic,TBL_URB3D_mosaic                                                   & 
                         ,TGL_URB3D_mosaic                                                                    & 
                         ,SH_URB2D_mosaic,LH_URB2D_mosaic                                                     & 
                         ,G_URB2D_mosaic,RN_URB2D_mosaic                                                      & 
                         ,TS_URB2D_mosaic                                                                     & 
                         ,TS_RUL2D_mosaic                                                                     & 
                         )


   USE module_domain
   USE module_wrf_error
   use module_sf_lake, only : nlevsoil,nlevsnow,nlevlake



   USE module_wind_fitch
   IMPLICIT NONE

   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER , INTENT(IN)        :: id
   INTEGER , INTENT(IN) ,OPTIONAL       :: tmn_update
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
   LOGICAL , INTENT(OUT)       :: is_CAMMGMP_used 

   LOGICAL, PARAMETER          :: FNDSOILW=.true., FNDSNOWH=.true.
   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)        :: num_soil_layers
   INTEGER , INTENT(IN)        :: lagday
   INTEGER , INTENT(OUT) ,OPTIONAL      :: nyear
   REAL    , INTENT(OUT) ,OPTIONAL      :: nday

   LOGICAL,  INTENT(IN)        :: start_of_simulation
   REAL,     INTENT(IN)        :: DT, p_top, DX, DY
   LOGICAL,  INTENT(IN)        :: restart
   REAL,     INTENT(IN)        :: RADT,BLDT,CUDT,MPDT
   REAL,     INTENT(IN)        :: swrad_scat

   REAL,     DIMENSION( kms:kme ) , INTENT(IN) :: zfull, zhalf
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: TSK, XLAT, XLONG,xlong_u,xlat_v
   REAL,  DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: msftx,msfty

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc
   INTEGER,      INTENT(IN   )    ::   alevsiz, no_src_types

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL, &
          INTENT(INOUT) ::                                  OZMIXM
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, n_ozmixm-1, no_src_types ), OPTIONAL, &
          INTENT(INOUT) ::                                  aerodm

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::        PIN
   REAL,  DIMENSION(alevsiz), OPTIONAL, INTENT(INOUT)  ::       PINA

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL,INTENT(INOUT)  ::          m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL, &
          INTENT(INOUT) ::                    aerosolc_1, aerosolc_2

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                 INTENT(INOUT) :: SMOIS, SH2O,TSLB
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ), INTENT(OUT) :: SMFR3D

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWC, &
                                                         SNOWH, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                          XICE, &
                                                         XICEM, &
                                                        VEGFRA, &
                                                        ACSNOM
   REAL,    DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::   rliq    

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            OPTIONAL, INTENT(INOUT)    ::                ACHFX, &
                                                         ACLHF, &
                                                      ACGRDFLX

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP




   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: re_cloud, &
             re_ice, re_snow
   INTEGER, INTENT(INOUT):: has_reqc, has_reqi, has_reqs

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
             RTHRATEN, RTHRATENLW, RTHRATENSW, CLDFRA

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) :: &
             CLDFRA_OLD

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) :: & 
             cldfra_dp, cldfra_sh

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             GSW,ALBEDO,ALBBCK,GLW,EMISS,EMBCK                          
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::   SNOALB


   REAL,     INTENT(IN) :: GMT

   INTEGER , INTENT(OUT) :: STEPRA, STEPBL, STEPCU
   INTEGER , INTENT(IN) :: JULYR, JULDAY



   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: & 
             RUCUTEN, RVCUTEN, RTHCUTEN, RQVCUTEN, RQRCUTEN, RQCCUTEN, &
             RQSCUTEN, RQICUTEN,                                       &
             RUSHTEN, RVSHTEN, RTHSHTEN, RQVSHTEN, RQRSHTEN, RQCSHTEN, &
             RQSSHTEN, RQISHTEN, RQGSHTEN
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: RQCNCUTEN, RQINCUTEN 

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: MASS_FLUX,   &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    & 
             RTHFTEN, RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::           &
             RAINNC, RAINC, RAINCV, RAINNCV,  &
             SNOWNC, SNOWNCV, GRAUPELNC, GRAUPELNCV
   REAL,     DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: z_at_q                          
   REAL,     DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qnwfa2d                    
   INTEGER,  INTENT(IN) :: num_sc                                                
   REAL,     DIMENSION(ims:ime,kms:kme,jms:jme,num_sc), INTENT(INOUT) :: scalar  

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: CLDEFI, NCA

   INTEGER,  DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: LOWLYR



   


   REAL,     DIMENSION(1:num_soil_layers),      INTENT(INOUT) :: ZS,DZS

  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    & 
             RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN,EXCH_H,TKE_PBL
  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: QKE

  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT), OPTIONAL :: &
                                            massflux_EDKF, entr_EDKF, detr_EDKF & 
                                                   ,thl_up, thv_up, rt_up       &
                                                   ,rv_up, rc_up, u_up, v_up    &
                                                   ,frac_up


   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             cugd_tten,cugd_ttens,cugd_qvten,                &
             cugd_qvtens,cugd_qcten
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             XLAND,ZNT,Z0,UST,MOL,LU_INDEX,                         &
             PBLH,THC,MAVAIL,HFX,QFX,RAINBL
   INTEGER , INTENT(INOUT)  :: landuse_ISICE, landuse_LUCATS
   INTEGER , INTENT(INOUT)  :: landuse_LUSEAS, landuse_ISN
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: TMN
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TYRA
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TDLY
   REAL,     DIMENSION( ims:ime , 1:lagday , jms:jme ) , INTENT(INOUT),OPTIONAL :: TLAG


   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          OPTIONAL,                                                  &
          INTENT(INOUT   ) ::                                        &
                               gd_cloud, gd_cloud2,                  &
                               gd_cloud_a, gd_cloud2_a,              &
                               QC_CU, QI_CU

   REAL,  DIMENSION( ims:ime ,  jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               raincv_a,raincv_b




   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ISNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:num_soil_layers, jms:jme) :: ZSNSOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: TSNOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TGXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: EAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CMXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FWETXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SNEQVOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ALBOLDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WSLAKEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ZWTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: LFMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RTMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WOODXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STBLCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FASTCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: XSAIXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MBXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHSTARXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme) :: SMOISEQ 
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SMCWTDXY   
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: DEEPRECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QRFSXY       
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSPRINGSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSLATXY 
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: AREAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: FDEPTHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: RIVERBEDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: EQZWT
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: RIVERCONDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: PEXPXY

   INTEGER , OPTIONAL,  INTENT(OUT) :: STEPWTD
   REAL , OPTIONAL, INTENT(IN) :: WTDDT


   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::   &
             F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, DIMENSION(:), INTENT(INOUT)   :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL,  INTENT(IN)  :: allowed_to_read, moved


   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             TML,T0ML,HML,H0ML,HUML,HVML,TMOML

   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             TSK_SAVE

   REAL,     OPTIONAL, INTENT(IN) :: FGDT
   INTEGER , OPTIONAL, INTENT(OUT) :: STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN, RPHNDGDTEN, RQVNDGDTEN
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             RMUNDGDTEN





   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR    
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB    
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG    

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D 




   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D  
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D  
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D  

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D 
   INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D 

   INTEGER , INTENT(IN)        :: num_urban_layers
   INTEGER , INTENT(IN)        :: num_urban_hi
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TRB_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW1_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW2_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TGB_URB4D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TLEV_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: QLEV_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFG_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFR_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW1_URB3D 
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW2_URB3D 
   REAL, OPTIONAL, DIMENSION( ims:ime,1:num_urban_hi, jms:jme), INTENT(INOUT) :: HI_URB2D  
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LP_URB2D  
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LB_URB2D  
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: HGT_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: MH_URB2D  
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STDH_URB2D 
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(INOUT) :: LF_URB2D  
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme,jms:jme), INTENT(INOUT) :: SF_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP

  real,    dimension(ims:ime,jms:jme ),intent(out)                        :: lakedepth2d,    &
                                                                             savedtke12d
  real,    dimension(ims:ime,jms:jme ),intent(inout)                      :: snowdp2d,       &
                                                                             h2osno2d,       &
                                                                             snl2d,          &
                                                                             t_grnd2d
 
  real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(out)            :: t_lake3d,       &
                                                                             lake_icefrac3d, &
                                                                             z_lake3d,       &
                                                                             dz_lake3d
  real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme ),INTENT(inout) :: t_soisno3d,     &
                                                                             h2osoi_ice3d,   &
                                                                             h2osoi_liq3d,   &
                                                                             h2osoi_vol3d,   &
                                                                             z3d,            &
                                                                             dz3d
  real,    dimension( ims:ime,1:nlevsoil, jms:jme ),INTENT(out)            :: watsat3d,       &
                                                                             csol3d,         &
                                                                             tkmg3d,         &
                                                                             tkdry3d,        &
                                                                             tksatu3d
  real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme ),INTENT(inout) :: zi3d
  LOGICAL, DIMENSION( ims:ime, jms:jme ),intent(out)                      :: lake2d

  REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  lake_depth
  real, intent(in)      ::      lakedepth_default, lake_min_elev

  REAL,              dimension(ims:ime,jms:jme )      ::  lakemask
  INTEGER, INTENT(IN)      ::  lakeflag

  INTEGER, INTENT(INOUT)      ::   lake_depth_flag
  INTEGER, INTENT(IN)      ::   use_lakedepth
 


   INTEGER, INTENT(IN) ::       maxpatch
   REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  HT
  integer, OPTIONAL,   dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  integer, OPTIONAL,   dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: snl
  real, OPTIONAL,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) ::  &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,     &
                t_ref2m,h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhtran,lhveg


   INTEGER, OPTIONAL, INTENT(IN) :: itimestep

   TYPE(fdob_type), OPTIONAL, INTENT(INOUT) :: fdob

   REAL, OPTIONAL, INTENT(IN) :: p00, t00, tlp   
   REAL, OPTIONAL, INTENT(IN) :: nssl_cccn, nssl_alphah, nssl_alphahl, &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs


   REAL,OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT(OUT) ::    te_temf, cf3d_temf    

   REAL,OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
          INTENT(OUT) ::    wm_temf



   REAL    :: ALBLND,ZZLND,ZZWTR,THINLD,XMAVA,CEN_LAT,pptop
   REAL,     DIMENSION( kms:kme )  :: sfull, shalf
   REAL :: obs_twindo_cg, obs_twindo

   CHARACTER*256 :: MMINLU_loc
   CHARACTER*80 :: message
   INTEGER :: ISWATER
   INTEGER :: ISICE
   INTEGER :: ISURBAN
   INTEGER :: sf_urban_physics
   INTEGER :: sf_ocean_physics
   REAL    :: oml_hml0
   INTEGER :: LakeModel
   LOGICAL :: usemonalb
   LOGICAL :: rdmaxalb
   INTEGER :: mfshconv
   INTEGER :: iopt_run

   INTEGER :: i, j, k, itf, jtf, ktf, n
integer myproc




  
  INTEGER, INTENT(IN) :: sf_surface_mosaic, NLCAT   
  INTEGER, INTENT(IN) :: mosaic_cat
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(IN) , OPTIONAL::   LANDUSEF
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(INOUT) , OPTIONAL::   LANDUSEF2 
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT), OPTIONAL :: mosaic_cat_index 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT):: &
        TSK_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::                &
        ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT):: &
        TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::                &
        TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic,    &
        SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                  
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic  
   LOGICAL :: IPRINT
 



   


   sf_urban_physics=config_flags%sf_urban_physics
   usemonalb=config_flags%usemonalb
   rdmaxalb=config_flags%rdmaxalb
   LakeModel = config_flags%sf_lake_physics
   mfshconv=config_flags%mfshconv
   IF(PRESENT(SMOISEQ)) THEN
      iopt_run=config_flags%opt_run
   ELSE
      iopt_run=-1
   END IF

   obs_twindo_cg=model_config_rec%obs_twindo(1)
   obs_twindo=config_flags%obs_twindo
   oml_hml0=config_flags%oml_hml0
   sf_ocean_physics=config_flags%sf_ocean_physics









   has_reqc = 0
   has_reqi = 0
   has_reqs = 0
   if (config_flags%ra_lw_physics .eq. RRTMG_LWSCHEME .and. &
       config_flags%ra_sw_physics .eq. RRTMG_SWSCHEME .and. &
        (config_flags%mp_physics  .eq. THOMPSON .or.        &
         config_flags%mp_physics  .eq. THOMPSONAERO) ) then
      has_reqc = 1
      has_reqi = 1
      has_reqs = 1
   endif



   sfull = 0.
   shalf = 0.

   CALL wrf_debug(100,'top of phy_init')

   WRITE(wrf_err_message,*) 'phy_init:  start_of_simulation = ',start_of_simulation
   CALL wrf_debug ( 100, TRIM(wrf_err_message) )

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)

   ZZLND=0.1
   ZZWTR=0.0001
   THINLD=0.04
   ALBLND=0.2
   XMAVA=0.3





   CALL nl_get_cen_lat(id,cen_lat)
   CALL wrf_debug(100,'calling nl_get_iswater, nl_get_isice, nl_get_mminlu_loc')
   CALL nl_get_iswater(id,iswater)
   CALL nl_get_isice(id,isice)
   CALL nl_get_isurban(id,isurban)
   CALL nl_get_mminlu( 1, mminlu_loc )
   CALL wrf_debug(100,'after nl_get_iswater, nl_get_isice, nl_get_mminlu_loc')

   landuse_ISICE = isice



  IF(.not.restart)THEN

     
     
 IF (config_flags%cu_physics == CAMZMSCHEME .or. config_flags%shcu_physics == CAMUWSHCUSCHEME ) THEN
     IF(PRESENT(rliq)) THEN
        rliq(:,:) = 0.0 
     ENDIF
 ENDIF
   IF ( .NOT. moved ) THEN
   DO j=jts,jtf
   DO i=its,itf
      XLAND(i,j)=1.
      GSW(i,j)=0.
      GLW(i,j)=0.

      UST(i,j)=0.0001
      MOL(i,j)=0.0
      PBLH(i,j)=0.0
      HFX(i,j)=0.
      QFX(i,j)=0.
      RAINBL(i,j)=0.
      RAINNCV(i,j)=0.
      SNOWNCV(i,j)=0.
      GRAUPELNCV(i,j)=0.
      ACSNOW(i,j)=0.
      DO k=kms,kme  
         EXCH_H(i,k,j) = 0.
      END DO
   ENDDO
   ENDDO
   ENDIF

   DO j=jts,jtf
   DO i=its,itf
     IVGTYP(i,j) = NINT(LU_INDEX(i,j))
   ENDDO
   ENDDO


   IF(PRESENT(TMN_UPDATE))THEN
   if(tmn_update.eq.1) then
   nyear=1
   nday=0.
   DO j=jts,jtf
   DO i=its,itf
      TYR(i,j)=TMN(i,j)
      TYRA(i,j)=0.0
      TDLY(i,j)=0.0
    DO n=1,lagday
      TLAG(i,n,j)=TMN(i,j)
    ENDDO
   ENDDO
   ENDDO
   endif
   ENDIF



   DO j=jts,jtf
   DO i=its,itf
     IF(XLAND(i,j) .LT. 1.5)THEN
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=ALBLND
       EMBCK(i,j)=0.85
       ALBEDO(i,j)=ALBBCK(i,j)
       EMISS(i,j)=EMBCK(i,j)
       THC(i,j)=THINLD
       ZNT(i,j)=ZZLND

       Z0(i,j)=ZZLND

       MAVAIL(i,j)=XMAVA
     ELSE
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=0.08
       ALBEDO(i,j)=ALBBCK(i,j)
       EMBCK(i,j)=0.98
       EMISS(i,j)=EMBCK(i,j)
       THC(i,j)=THINLD
       ZNT(i,j)=ZZWTR

       Z0(i,j)=ZZWTR

       MAVAIL(i,j)=1.0
     ENDIF

   ENDDO
   ENDDO

    if (config_flags%cu_diag == 1 )then
    do j=jts,jtf
       do k=kts,ktf
          do i=its,itf
             gd_cloud(i,k,j) = 0.
             gd_cloud2(i,k,j) = 0.
             gd_cloud_a(i,k,j) = 0.
             gd_cloud2_a(i,k,j) = 0.
             QC_CU(i,k,j) = 0.
             QI_CU(i,k,j) = 0.
          end do
       end do
    end do
    endif

    do j=jts,jtf
         do i=its,itf
           raincv_a(i,j)=0.
           raincv_b(i,j)=0.
      end do
    end do



   config_flags%icloud_cu = 0
   IF ( config_flags%cu_rad_feedback ) THEN  
      IF ( config_flags%cu_physics == kfetascheme ) THEN
         config_flags%icloud_cu = 2
      ELSE IF ( config_flags%cu_physics == gfscheme .OR. &
                config_flags%cu_physics == g3scheme .OR. &
                config_flags%cu_physics == gdscheme ) THEN
         config_flags%icloud_cu = 1
      END IF
   END IF




    if (has_reqc.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_cloud(i,k,j) = 2.51E-6
          end do
          end do
       end do
    endif
    if (has_reqi.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_ice(i,k,j) = 5.01E-6
          end do
          end do
       end do
    endif
    if (has_reqs.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_snow(i,k,j) = 10.01E-6
          end do
          end do
       end do
    endif

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to landuse_init' )

   IF(mminlu_loc .ne. '    ')THEN


     CALL landuse_init(lu_index, snowc, albedo, albbck, snoalb, mavail, emiss, embck, &
                znt, Z0, thc, xland, xice, xicem, julday, cen_lat, iswater, &
                TRIM ( mminlu_loc ) ,                               &
                landuse_ISICE, landuse_LUCATS,                      &
                landuse_LUSEAS, landuse_ISN,                        &
                config_flags%fractional_seaice,                      &
                lu_state,                                           &
                allowed_to_read , usemonalb ,                       &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )
   ENDIF

  ENDIF




   CALL z2sigma(zfull,zhalf,sfull,shalf,p_top,pptop,config_flags, &
                allowed_to_read,                                  &
                kds,kde,kms,kme,kts,kte)

   

   
   is_CAMMGMP_used = .FALSE.

   if(config_flags%mp_physics == CAMMGMPSCHEME) is_CAMMGMP_used = .TRUE.











   if(       config_flags%bl_pbl_physics == CAMUWPBLSCHEME     .OR. config_flags%cu_physics == CAMZMSCHEME      &
        .OR. config_flags%shcu_physics   == CAMUWSHCUSCHEME                                                     & 

        .OR. config_flags%mp_physics == CAMMGMPSCHEME                                                           &






      ) THEN
      CALL CAM_INIT(ixcldliq, ixcldice, ixnumliq, ixnumice, config_flags)
   ENDIF









  IF ( config_flags%windfarm_opt .EQ. 1 ) THEN
    CALL init_module_wind_fitch(id,config_flags,xlong,xlat,windfarm_initialized,ims,ime,jms,jme,its,ite,jts,jte,ids,ide,jds,jde)
  ENDIF


   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to ra_init' )

   CALL ra_init(id=id,STEPRA=STEPRA,RADT=RADT,DT=DT,RTHRATEN=RTHRATEN,RTHRATENLW=RTHRATENLW,             &
                RTHRATENSW=RTHRATENSW,CLDFRA=CLDFRA,EMISS=EMISS,cen_lat=cen_lat,JULYR=JULYR,JULDAY=JULDAY,GMT=GMT,    &
                levsiz=levsiz,XLAT=XLAT,XLONG=XLONG,n_ozmixm=n_ozmixm,     &
                alevsiz=alevsiz,no_src_types=no_src_types,                 &
                cldfra_dp=cldfra_dp,cldfra_sh=cldfra_sh,                   & 
                cldfra_old=cldfra_old,                                     & 
                ozmixm=ozmixm,pin=pin,                                     & 
                aerodm=aerodm,pina=pina,                                   & 
                m_ps_1=m_ps_1,m_ps_2=m_ps_2,m_hybi=m_hybi,aerosolc_1=aerosolc_1,aerosolc_2=aerosolc_2,     & 
                paerlev=paerlev,n_aerosolc=n_aerosolc,                             &
                sfull=sfull,shalf=shalf,pptop=pptop,swrad_scat=swrad_scat,p_top=p_top,       &
                config_flags=config_flags,restart=restart,                           &
                allowed_to_read=allowed_to_read, start_of_simulation=start_of_simulation,           &
                ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,                   &
                ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,                   &
                its=its, ite=ite, jts=jts, jte=jte, kts=kts, kte=kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to bl_init' )
   CALL bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,        &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_PBL,mfshconv,               &
                massflux_EDKF, entr_EDKF, detr_EDKF, & 
                thl_up, thv_up, rt_up,       &
                rv_up, rc_up, u_up, v_up,    &
                frac_up, &
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,MAVAIL,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &




                ZNT,XLAND,XICE,                                 &
                DX, DY, MSFTX, MSFTY,                           &

                QKE,                                            &
                SFCEVP,GRDFLX,                                  &
                TRIM (MMINLU_LOC),                              &
                ISNOWXY, ZSNSOXY, TSNOXY,                       &
                SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,         &
                CANLIQXY, EAHXY, TAHXY, CMXY,                   &
                CHXY, FWETXY, SNEQVOXY, ALBOLDXY, QSNOWXY,      &
                WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY,&
                STMASSXY, WOODXY, STBLCPXY, FASTCPXY,           &
                XSAIXY,                                         &
                SMOISEQ, SMCWTDXY, RECHXY, DEEPRECHXY, AREAXY,  &
                WTDDT, STEPWTD, QRFSXY ,QSPRINGSXY ,QSLATXY,   &
                FDEPTHXY, RIVERBEDXY, EQZWT, RIVERCONDXY, PEXPXY, & 
                ISICE,                                 &
                T2MVXY,T2MBXY,CHSTARXY ,                        &
                allowed_to_read , iopt_run ,                    &
                start_of_simulation ,                           &
                lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d,       & 
                snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d, & 
                z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d,   & 
                h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d,           & 
                zi3d,         watsat3d,     csol3d,     tkmg3d,         & 
                tkdry3d,      tksatu3d,     LakeModel,  lake2d,           & 
                lakedepth_default,            lake_min_elev, lake_depth,       & 

                lakemask, lakeflag,                                    & 

                lake_depth_flag, use_lakedepth,                        & 
                te_temf,cf3d_temf,wm_temf,                      & 
                DZR, DZB, DZG,                                  & 
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & 
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & 
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & 
                SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,          & 
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               & 
                SF_URBAN_PHYSICS,                               & 
                NUM_URBAN_LAYERS,                               & 
                NUM_URBAN_HI,                                   & 
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & 
                TGB_URB4D,TLEV_URB3D,QLEV_URB3D,                & 
                TW1LEV_URB3D,TW2LEV_URB3D,                      & 
                TGLEV_URB3D,TFLEV_URB3D,                        & 
                SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,            & 
                SFVENT_URB3D,LFVENT_URB3D,                      & 
                SFWIN1_URB3D,SFWIN2_URB3D,                      & 
                SFW1_URB3D,SFW2_URB3D,                          & 
                SFR_URB3D,SFG_URB3D,                            & 
                LP_URB2D,HI_URB2D,LB_URB2D,                     & 
                HGT_URB2D,MH_URB2D,STDH_URB2D,                  & 
                LF_URB2D,                                       & 
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & 
                A_E_BEP,B_U_BEP,B_V_BEP,                        & 
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & 
                DL_U_BEP,SF_BEP,VL_BEP,                         & 
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, sf_ocean_physics,                     & 
                TML,T0ML,HML,H0ML,HUML,HVML,TMOML,              & 
                is_CAMMGMP_used                                 &
               ,TSK_SAVE                                        & 

               ,numc,nump,snl,                                      &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ht,maxpatch,                                        &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhveg,lhtran                      &

                ,landusef,landusef2,NLCAT                       & 
                ,sf_surface_mosaic, mosaic_cat                  & 
                ,mosaic_cat_index                               & 
                ,TSK_mosaic,TSLB_mosaic                         & 
                ,SMOIS_mosaic,SH2O_mosaic                       & 
                ,CANWAT_mosaic,SNOW_mosaic                      & 
                ,SNOWH_mosaic,SNOWC_mosaic                      & 
                ,ALBEDO,ALBBCK, EMISS, EMBCK                    & 
                ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   &         
                ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  
                ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  
                ,QC_URB2D_mosaic                                &  
                ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  
                ,TGL_URB3D_mosaic                               &  
                ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  
                ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  
                ,TS_URB2D_mosaic                                &  
                ,TS_RUL2D_mosaic                                &  
               ) 

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(DX,STEPCU,CUDT,DT,RUCUTEN,RVCUTEN,RTHCUTEN,     &
                RQVCUTEN,RQRCUTEN,RQCCUTEN,RQSCUTEN,RQICUTEN,   &
                NCA,RAINC,RAINCV,W0AVG,config_flags,restart,    &
                CLDEFI,LOWLYR,MASS_FLUX,                        &
                RTHFTEN, RQVFTEN,                               &
                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,              &
                APR_CAPMA,APR_CAPME,APR_CAPMI,                  &
                cugd_tten,cugd_ttens,cugd_qvten,                &
                cugd_qvtens,cugd_qcten,                         &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                RQCNCUTEN,RQINCUTEN                            ) 

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to shcu_init' )

   CALL shcu_init(STEPCU,CUDT,DT,RUSHTEN,RVSHTEN,RTHSHTEN,      &
                RQVSHTEN,RQRSHTEN,RQCSHTEN,                     &
                RQSSHTEN,RQISHTEN,RQGSHTEN,                     &
                NCA,RAINC,RAINCV,config_flags,restart,          &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to mp_init' )

   CALL mp_init(RAINNC,SNOWNC,GRAUPELNC,config_flags,restart,warm_rain,          &
                adv_moist_cond,                                 &
                MPDT, DT, DX, DY, LOWLYR,                       &
                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,               &
                mp_restart_state,tbpvs_state,tbpvs0_state,      &
                allowed_to_read, start_of_simulation,           &

                ixcldliq, ixcldice, ixnumliq, ixnumice,         &
                nssl_cccn, nssl_alphah, nssl_alphahl,           &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs,                            &
                z_at_q, qnwfa2d, scalar, num_sc,            &  
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )


   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to fg_init' )

   CALL fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,          &
                RTHNDGDTEN,RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,    &
                config_flags,restart,                           &
                allowed_to_read ,                               &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to fdob_init' )

   CALL fdob_init(model_config_rec%obs_nudge_opt,               &
                  model_config_rec%max_dom,                     &
                  id,                                           &
                  model_config_rec%parent_id,                   &
                  model_config_rec%obs_idynin,                  &
                  model_config_rec%obs_dtramp,                  &
                  model_config_rec%fdda_end,                    &
                  model_config_rec%restart,                     &
                  obs_twindo_cg, obs_twindo,                    &
                  itimestep,                                    &
                  model_config_rec%obs_no_pbl_nudge_uv,         &
                  model_config_rec%obs_no_pbl_nudge_t,          &
                  model_config_rec%obs_no_pbl_nudge_q,          &
                  model_config_rec%obs_sfc_scheme_horiz,        &
                  model_config_rec%obs_sfc_scheme_vert,         &
                  model_config_rec%obs_max_sndng_gap,           &
                  model_config_rec%obs_sfcfact,                 &
                  model_config_rec%obs_sfcfacr,                 &
                  model_config_rec%obs_dpsmx,                   &
                  model_config_rec%obs_nudge_wind,              &
                  model_config_rec%obs_nudge_temp,              &
                  model_config_rec%obs_nudge_mois,              &
                  model_config_rec%obs_nudgezfullr1_uv,         &
                  model_config_rec%obs_nudgezrampr1_uv,         &
                  model_config_rec%obs_nudgezfullr2_uv,         &
                  model_config_rec%obs_nudgezrampr2_uv,         &
                  model_config_rec%obs_nudgezfullr4_uv,         &
                  model_config_rec%obs_nudgezrampr4_uv,         &
                  model_config_rec%obs_nudgezfullr1_t,          &
                  model_config_rec%obs_nudgezrampr1_t,          &
                  model_config_rec%obs_nudgezfullr2_t,          &
                  model_config_rec%obs_nudgezrampr2_t,          &
                  model_config_rec%obs_nudgezfullr4_t,          &
                  model_config_rec%obs_nudgezrampr4_t,          &
                  model_config_rec%obs_nudgezfullr1_q,          &
                  model_config_rec%obs_nudgezrampr1_q,          &
                  model_config_rec%obs_nudgezfullr2_q,          &
                  model_config_rec%obs_nudgezrampr2_q,          &
                  model_config_rec%obs_nudgezfullr4_q,          &
                  model_config_rec%obs_nudgezrampr4_q,          &
                  model_config_rec%obs_nudgezfullmin,           &
                  model_config_rec%obs_nudgezrampmin,           &
                  model_config_rec%obs_nudgezmax,               &
                  xlat,                                         &
                  xlong,                                        &
                  model_config_rec%start_year(id),              &
                  model_config_rec%start_month(id),             &
                  model_config_rec%start_day(id),               &
                  model_config_rec%start_hour(id),              &
                  model_config_rec%start_minute(id),            &
                  model_config_rec%start_second(id),            &
                  p00, t00, tlp,                                &
                  zhalf, p_top,                                 &
                  fdob,                                         &
                  model_config_rec%obs_ipf_init,                &
                  ids, ide, jds, jde, kds, kde,                 &
                  ims, ime, jms, jme, kms, kme,                 &
                  its, ite, jts, jte, kts, kte                  )



   END SUBROUTINE phy_init


   SUBROUTINE landuse_init(lu_index, snowc, albedo, albbck, snoalb, mavail, emiss, embck, &
                znt,Z0,thc,xland, xice, xicem, julday, cen_lat, iswater, mminlu,  &
                ISICE, LUCATS, LUSEAS, ISN,                         &
                fractional_seaice,                                  &
                lu_state,                                           &
                allowed_to_read , usemonalb ,                       &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )

   USE module_wrf_error
   IMPLICIT NONE


   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: iswater, julday
   REAL    , INTENT(IN)           :: cen_lat
   CHARACTER(LEN=*), INTENT(IN)        :: mminlu
   LOGICAL,  INTENT(IN)           :: allowed_to_read , usemonalb
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: lu_index, snowc, xice, snoalb 
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT  ) :: albedo, albbck, mavail, emiss, &
                                                               embck,                         &
                                                               znt, Z0, thc, xland, xicem
   INTEGER , INTENT(INOUT)  :: ISICE, LUCATS, LUSEAS, ISN, fractional_seaice
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

   REAL :: xice_threshold


   CHARACTER*256 LUTYPE
   CHARACTER*512 :: message
   INTEGER  :: landuse_unit, LS, LC, LI, LUN, NSN
   INTEGER  :: i, j, itf, jtf, is, cats, seas, curs
   INTEGER , PARAMETER :: OPEN_OK = 0
   INTEGER :: ierr
   INTEGER , PARAMETER :: max_cats = 100 , max_seas = 12
   REAL    , DIMENSION( max_cats, max_seas ) :: ALBD, SLMO, SFEM, SFZ0, THERIN, SFHC
   REAL    , DIMENSION( max_cats )     :: SCFX




   LOGICAL :: found_lu, end_of_file
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor



   CALL wrf_debug( 100 , 'top of landuse_init' )

   NSN=-1  

  if ( fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif


   IF ( 6*(max_cats*max_seas)+1*max_cats .GT. 7501 ) THEN
      WRITE(message,*)'landuse_init: lu_state overflow. Make Registry dimspec p > ',6*(max_cats*max_seas)+1*max_cats
   ENDIF
   curs = 1
   DO cats = 1, max_cats
     SCFX(cats) =           lu_state(curs)         ; curs = curs + 1
     DO seas = 1, max_seas
       ALBD(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SLMO(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFEM(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFZ0(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFHC(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       THERIN(cats,seas) =  lu_state(curs)         ; curs = curs + 1
     ENDDO
   ENDDO


   ISN=1
   IF(JULDAY.LT.105.OR.JULDAY.GT.288)ISN=2
   IF(CEN_LAT.LT.0.0)ISN=3-ISN

   FOUND_LU = .TRUE.
   IF ( allowed_to_read ) THEN
      landuse_unit = 29
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(landuse_unit, FILE='LANDUSE.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF ( ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_physics_init.F: LANDUSE_INIT: open failure for LANDUSE.TBL'
          CALL wrf_error_fatal3("<stdin>",1353,&
message )
        END IF
      ENDIF



















      call wrf_message ( 'INPUT LandUse = "' // TRIM(MMINLU) // '"' )
      FOUND_LU = .FALSE.
      end_of_file = .FALSE.

 1999 CONTINUE
      IF ( wrf_dm_on_monitor() ) THEN
        READ (landuse_unit,*,END=2002)LUTYPE
        GOTO 2003
 2002   CONTINUE
        CALL wrf_message( 'INPUT FILE FOR LANDUSE REACHED END OF FILE' )
        end_of_file = .TRUE.
 2003   CONTINUE
        IF ( .NOT. end_of_file ) READ (landuse_unit,*)LUCATS,LUSEAS
        FOUND_LU = LUTYPE.EQ.MMINLU
      ENDIF
      CALL wrf_dm_bcast_bytes (end_of_file, 4 )
      IF ( .NOT. end_of_file ) THEN
        CALL wrf_dm_bcast_string(lutype, 256)
        CALL wrf_dm_bcast_bytes (lucats,  4 )
        CALL wrf_dm_bcast_bytes (luseas,  4 )
        CALL wrf_dm_bcast_bytes (found_lu,  4 )
        IF(FOUND_LU)THEN
          LUN=LUCATS
          NSN=LUSEAS
            IF(LUTYPE.NE.'SSIB') THEN 
            write(message,*) 'LANDUSE TYPE = "' // TRIM (LUTYPE) // '" FOUND',        &
                   LUCATS,' CATEGORIES',LUSEAS,' SEASONS',     &
                   ' WATER CATEGORY = ',ISWATER,               &
                   ' SNOW CATEGORY = ',ISICE
            call wrf_message(message)
            ENDIF
        ENDIF
        DO ls=1,luseas
          if ( wrf_dm_on_monitor() ) then
            READ (landuse_unit,*)
          endif
          DO LC=1,LUCATS
            IF(found_lu)THEN
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)LI,ALBD(LC,LS),SLMO(LC,LS),SFEM(LC,LS),        &
                           SFZ0(LC,LS),THERIN(LC,LS),SCFX(LC),SFHC(LC,LS)
              ENDIF
              CALL wrf_dm_bcast_bytes (LI,  4 )
              IF(LC.NE.LI)CALL wrf_error_fatal3("<stdin>",1419,&
'module_start: MISSING LANDUSE UNIT ' )
            ELSE
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        IF(NSN.EQ.1.AND.FOUND_LU) THEN
           ISN = 1
        END IF
        CALL wrf_dm_bcast_bytes (albd,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (slmo,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (sfem,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (sfz0,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (therin, max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (sfhc,   max_cats * max_seas * 4 )
        CALL wrf_dm_bcast_bytes (scfx,   max_cats *            4 )
      ENDIF

      IF(.NOT. found_lu .AND. .NOT. end_of_file ) GOTO 1999


      IF(.NOT. found_lu .OR. end_of_file )THEN
        CALL wrf_message ( 'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE: TABLE NOT USED' )
      ENDIF
    ENDIF  

    IF(FOUND_LU)THEN

      itf = min0(ite, ide-1)
      jtf = min0(jte, jde-1)
      IF(usemonalb)CALL wrf_message ( 'Climatological albedo is used instead of table values' )
      DO j = jts, jtf
        DO i = its, itf
          IS=nint(lu_index(i,j))
          
          IF(allowed_to_read)THEN
             IF(IS.LT.0.OR.IS.GT.LUN)THEN
               WRITE ( wrf_err_message , * ) 'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J,' LUN= ',LUN
               CALL wrf_error_fatal3("<stdin>",1460,&
TRIM ( wrf_err_message ) )
             ENDIF
          ENDIF

          IF(IS.EQ.0)THEN
            IS=ISWATER
          ENDIF
          IF(.NOT.usemonalb)ALBBCK(I,J)=ALBD(IS,ISN)/100.
          ALBEDO(I,J)=ALBBCK(I,J)
          IF(SNOWC(I,J) .GT. 0.5) THEN 
             IF (usemonalb) THEN
                 ALBEDO(I,J)=SNOALB(I,J)
             ELSE
                 ALBEDO(I,J)=ALBBCK(I,J)*(1.+SCFX(IS))
             ENDIF
          ENDIF
          THC(I,J)=THERIN(IS,ISN)/100.
          Z0(I,J)=SFZ0(IS,ISN)/100.
          ZNT(I,J)=Z0(I,J)
          EMBCK(I,J)=SFEM(IS,ISN)
          EMISS(I,J)=EMBCK(I,J)
          MAVAIL(I,J)=SLMO(IS,ISN)
          IF(IS.NE.ISWATER)THEN
            XLAND(I,J)=1.0
          ELSE
            XLAND(I,J)=2.0
          ENDIF

          XICEM(I,J)=XICE(I,J)
          IF(XICE(I,J).GE.xice_threshold)THEN
            XLAND(I,J)=1.0
            ALBBCK(I,J)=ALBD(ISICE,ISN)/100.
            EMBCK(I,J)=SFEM(ISICE,ISN)
            IF (FRACTIONAL_SEAICE == 1) THEN
               
               
               ALBEDO(I,J) = ( XICE(I,J) * ALBBCK(I,J) ) + ( (1.0-XICE(I,J)) * 0.08 )
               EMISS(I,J)  = ( XICE(I,J) * EMBCK(I,J)  ) + ( (1.0-XICE(I,J)) * 0.98 )
            ELSE
               ALBEDO(I,J)=ALBBCK(I,J)
               EMISS(I,J)=EMBCK(I,J)
            ENDIF
            THC(I,J)=THERIN(ISICE,ISN)/100.
            Z0(I,J)=SFZ0(ISICE,ISN)/100.
            ZNT(I,J)=Z0(I,J)
            MAVAIL(I,J)=SLMO(ISICE,ISN)
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    if ( wrf_dm_on_monitor() .and. allowed_to_read ) then
      CLOSE (landuse_unit)
    endif
    CALL wrf_debug( 100 , 'returning from of landuse_init' )


    curs = 1
    DO cats = 1, max_cats
      lu_state(curs) = SCFX(cats)                 ; curs = curs + 1
      DO seas = 1, max_seas
        lu_state(curs) = ALBD(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SLMO(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFEM(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFZ0(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFHC(cats,seas)          ; curs = curs + 1
        lu_state(curs) = THERIN(cats,seas)        ; curs = curs + 1
      ENDDO
    ENDDO











   END SUBROUTINE landuse_init


   SUBROUTINE ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,       &
                      RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                      levsiz,XLAT,XLONG,n_ozmixm,                     &
                      alevsiz,no_src_types,                           &
                      cldfra_dp,cldfra_sh,                            & 
                      cldfra_old,                                     & 
                      ozmixm,pin,                                     & 
                      aerodm,pina,                                    & 
                      m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,     & 
                      paerlev,n_aerosolc,                             &
                      sfull,shalf,pptop,swrad_scat,p_top,            &
                      config_flags,restart,                          &
                      allowed_to_read, start_of_simulation,          &
                      ids, ide, jds, jde, kds, kde,                  &
                      ims, ime, jms, jme, kms, kme,                  &
                      its, ite, jts, jte, kts, kte                   )

   USE module_ra_rrtm      , ONLY : rrtminit
   USE module_ra_rrtmg_lw  , ONLY : rrtmg_lwinit
   USE module_ra_rrtmg_sw  , ONLY : rrtmg_swinit
   USE module_ra_cam       , ONLY : camradinit
   USE module_ra_cam_support , ONLY : oznini
   USE module_ra_sw        , ONLY : swinit
   USE module_ra_gsfcsw    , ONLY : gsfc_swinit
   USE module_ra_gfdleta   , ONLY : gfdletainit



   USE module_ra_hs        , ONLY : hsinit
   USE module_domain

   IMPLICIT NONE

   INTEGER,  INTENT(IN)           :: id
   TYPE (grid_config_rec_type)    :: config_flags
   LOGICAL , INTENT(IN)           :: restart
   LOGICAL,  INTENT(IN)           :: allowed_to_read

   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: JULDAY,JULYR
   REAL ,    INTENT(IN)           :: DT, RADT, cen_lat, GMT, pptop,  &
                                     swrad_scat, p_top
   LOGICAL,  INTENT(IN)           :: start_of_simulation

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc
   INTEGER,      INTENT(IN   )    ::   alevsiz, no_src_types

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  XLAT, XLONG

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL,      &
          INTENT(INOUT) ::                                  OZMIXM
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, n_ozmixm-1, no_src_types ), OPTIONAL,      &
          INTENT(INOUT) ::                                  aerodm

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL, INTENT(INOUT)  ::         m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL,     &
          INTENT(INOUT) ::                      aerosolc_1, aerosolc_2

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::          PIN
   REAL,  DIMENSION(alevsiz), OPTIONAL, INTENT(INOUT)  ::         PINA

   INTEGER , INTENT(INOUT)        :: STEPRA
   INTEGER :: isn

   REAL , DIMENSION( kms:kme ) , INTENT(IN) :: sfull, shalf
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::           &
                                                           RTHRATEN, &
                                                         RTHRATENLW, &
                                                         RTHRATENSW, &
                                                             CLDFRA

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) , & 
                                                         OPTIONAL :: &
                                                          cldfra_dp, &
                                                          cldfra_sh

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) :: &
                                                         CLDFRA_OLD

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: EMISS
   LOGICAL :: etalw = .false.
   LOGICAL :: hwrflw= .false.
   LOGICAL :: camlw = .false.

   LOGICAL :: acswalloc = .false.
   LOGICAL :: aclwalloc = .false.
   integer :: month,iday
   INTEGER :: i, j, k, itf, jtf, ktf


   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)





    STEPRA = nint(RADT*60./DT)
    STEPRA = max(STEPRA,1)



   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHRATEN(i,k,j)=0.
        RTHRATENLW(i,k,j)=0.
        RTHRATENSW(i,k,j)=0.
        CLDFRA(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     IF( PRESENT(cldfra_dp) ) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cldfra_dp(i,k,j)=0.
           cldfra_sh(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     if( present(cldfra_old) ) then
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cldfra_old(i,k,j) = 0.
        ENDDO
        ENDDO
        ENDDO
     end if
   ENDIF







   IF ( config_flags%o3input .EQ. 2 .AND. id .EQ. 1 ) THEN
      CALL oznini(ozmixm,pin,levsiz,n_ozmixm,XLAT,                &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
   ENDIF

   IF ( config_flags%aer_opt .EQ. 1 .AND. id .EQ. 1 ) THEN
      CALL aerosol_in(aerodm,pina,alevsiz,n_ozmixm-1,no_src_types,XLAT,XLONG,   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
   ENDIF












   lwrad_select: SELECT CASE(config_flags%ra_lw_physics)

        CASE (RRTMSCHEME)
             CALL rrtminit(                                 &
                           p_top, allowed_to_read ,         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

        CASE (CAMLWSCHEME)



             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2)) THEN
             CALL camradinit(                                  &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop, &
                         ozmixm,pin,levsiz,XLAT,n_ozmixm,      &
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,&
                         paerlev, n_aerosolc,              &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )
             ELSE
                CALL wrf_error_fatal3("<stdin>",1742,&
'arguments not present for calling cam radiation' )
             ENDIF

             camlw = .true.
             aclwalloc = .true.

        CASE (RRTMG_LWSCHEME)
             CALL rrtmg_lwinit(                             &
                           p_top, allowed_to_read ,         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             aclwalloc = .true.

        CASE (GFDLLWSCHEME)
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,      &
                              julyr,month,iday,gmt,         &
                              config_flags,allowed_to_read, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
             etalw = .true.
        CASE (HELDSUAREZ)
             CALL hsinit(RTHRATEN,restart,             &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )

        CASE (FLGLWSCHEME)

        CASE DEFAULT

   END SELECT lwrad_select


   swrad_select: SELECT CASE(config_flags%ra_sw_physics)

        CASE (SWRADSCHEME)
             CALL swinit(                                  &
                         swrad_scat,                       &
                         allowed_to_read ,                 &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )

        CASE (CAMSWSCHEME)
             IF(.not.camlw)THEN
             CALL camradinit(                              &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop,               &
                         ozmixm,pin,levsiz,XLAT,n_ozmixm,     &
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,&
                         paerlev, n_aerosolc,              &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )
             ENDIF
             acswalloc = .true.

        CASE (GSFCSWSCHEME)
             CALL gsfc_swinit(cen_lat, allowed_to_read )

        CASE (RRTMG_SWSCHEME)
             CALL rrtmg_swinit(                             &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             acswalloc = .true.

        CASE (GFDLSWSCHEME)
             IF(.not.etalw)THEN
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,      &
                              julyr,month,iday,gmt,         &
                              config_flags,allowed_to_read, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
             ENDIF
        CASE (FLGSWSCHEME)

        CASE DEFAULT

   END SELECT swrad_select

   

   IF(config_flags%bucket_J .gt. 0.0)THEN
     IF(.not. (acswalloc .and. aclwalloc))THEN
           CALL wrf_error_fatal3("<stdin>",1837,&
'Need CAM or RRTMG radiation for bucket_J option')
     ENDIF
   ENDIF

   END SUBROUTINE ra_init

   SUBROUTINE bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,  &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_PBL,mfshconv,               &
                massflux_EDKF, entr_EDKF, detr_EDKF, & 
                thl_up, thv_up, rt_up,       &
                rv_up, rc_up, u_up, v_up,    &
                frac_up, &
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,mavail,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
                ZNT,XLAND,XICE,DX, DY, MSFTX, MSFTY,            &
                QKE, SFCEVP,GRDFLX,                             &
                MMINLU,                                         &
                ISNOWXY, ZSNSOXY, TSNOXY,                       &
                SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,         &
                CANLIQXY, EAHXY, TAHXY, CMXY,                   &
                CHXY, FWETXY, SNEQVOXY, ALBOLDXY, QSNOWXY,      &
                WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY,&
                STMASSXY, WOODXY, STBLCPXY, FASTCPXY,           &
                XSAIXY,                                         &
                SMOISEQ, SMCWTDXY, RECHXY, DEEPRECHXY, AREAXY,  &
                WTDDT, STEPWTD,QRFSXY ,QSPRINGSXY ,QSLATXY,     &
                FDEPTHXY, RIVERBEDXY, EQZWT, RIVERCONDXY, PEXPXY, & 
                ISICE,                                 &
                T2MVXY, T2MBXY ,CHSTARXY,                       &
                allowed_to_read, iopt_run ,                     &
                start_of_simulation,                            &
                lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d,       & 
                snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d, & 
                z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d,   & 
                h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d,           & 
                zi3d,         watsat3d,     csol3d,     tkmg3d,         & 
                tkdry3d,      tksatu3d,     LakeModel,  lake2d,           & 
                lakedepth_default,            lake_min_elev, lake_depth,       & 
                lakemask, lakeflag,                                      & 
                lake_depth_flag, use_lakedepth,                          & 
                te_temf,cf3d_temf,wm_temf,                      & 

                DZR, DZB, DZG,                                  & 
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & 
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & 
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & 
                SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,             & 
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               &
                SF_URBAN_PHYSICS,                               & 
                NUM_URBAN_LAYERS,                               & 
                NUM_URBAN_HI,                                   & 
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & 
                TGB_URB4D,TLEV_URB3D,QLEV_URB3D,                & 
                TW1LEV_URB3D,TW2LEV_URB3D,                      & 
                TGLEV_URB3D,TFLEV_URB3D,                        & 
                SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,            & 
                SFVENT_URB3D,LFVENT_URB3D,                      & 
                SFWIN1_URB3D,SFWIN2_URB3D,                      & 
                SFW1_URB3D,SFW2_URB3D,                          & 
                SFR_URB3D,SFG_URB3D,                            & 
                LP_URB2D,HI_URB2D,LB_URB2D,                     & 
                HGT_URB2D,MH_URB2D,STDH_URB2D,                  & 
                LF_URB2D,                                       & 
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & 
                A_E_BEP,B_U_BEP,B_V_BEP,                        & 
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & 
                DL_U_BEP,SF_BEP,VL_BEP,                         & 
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, sf_ocean_physics,                     & 
                TML,T0ML,HML,H0ML,HUML,HVML,TMOML,              &
                is_CAMMGMP_used                                 &
               ,TSK_SAVE                                        & 

               ,numc,nump,snl,                                      &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ht,maxpatch,                                        &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhveg,lhtran                      &

                ,landusef,landusef2,NLCAT                       & 
                ,sf_surface_mosaic, mosaic_cat                  & 
                ,mosaic_cat_index                               & 
                ,TSK_mosaic,TSLB_mosaic                         & 
                ,SMOIS_mosaic,SH2O_mosaic                       & 
                ,CANWAT_mosaic,SNOW_mosaic                      & 
                ,SNOWH_mosaic,SNOWC_mosaic                      & 
                ,ALBEDO,ALBBCK, EMISS, EMBCK                    & 
                ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   &         
                ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  
                ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  
                ,QC_URB2D_mosaic                                &  
                ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  
                ,TGL_URB3D_mosaic                               &  
                ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  
                ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  
                ,TS_URB2D_mosaic                                &  
                ,TS_RUL2D_mosaic                                &  
                                                                ) 

   USE module_sf_sfclay
   USE module_sf_sfclayrev
   USE module_sf_slab
   USE module_sf_pxsfclay
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_bl_gfs2011, only : gfs2011init
   USE module_bl_acm
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_noahdrv
   USE module_sf_noahmpdrv
   USE module_sf_clm, only : clminit
   USE module_sf_urban
   USE module_sf_bep                                  
   USE module_sf_bep_bem
   USE module_sf_ruclsm
   USE module_sf_pxlsm
   USE module_sf_oml
   USE module_bl_myjpbl
   USE module_bl_myjurb
   USE module_bl_boulac
   USE module_bl_camuwpbl_driver, ONLY :  camuwpblinit
   USE module_bl_qnsepbl
   USE module_sf_lake
   USE module_bl_qnsepbl09
   USE module_bl_mfshconvpbl
   USE module_bl_gbmpbl
   USE module_bl_mynn
   USE module_bl_temf
   USE module_sf_temfsfclay
   USE module_sf_mynn


   IMPLICIT NONE

   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL, INTENT(IN)         ::   FNDSOILW, FNDSNOWH
   LOGICAL, INTENT(IN)         ::   RDMAXALB
   LOGICAL, INTENT(IN)         :: is_CAMMGMP_used 

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers
   INTEGER , INTENT(IN)        ::     SF_URBAN_PHYSICS 
   INTEGER , INTENT(IN)        ::     IOPT_RUN


   INTEGER         ::     LakeModel
   real, intent(in)            ::      lakedepth_default,lake_min_elev
   REAL ,    INTENT(IN)        ::     DT, BLDT
   REAL ,    INTENT(IN)        ::     DX, DY
   REAL,    DIMENSION( ims:ime, jms:jme ) ,  INTENT(IN) :: MSFTX,MSFTY
   INTEGER , INTENT(INOUT)     ::     STEPBL

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),    &
             INTENT(OUT) :: SMFR3D

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                   INTENT(INOUT) :: SMOIS,SH2O,TSLB

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        SNOALB, &
                                                        CANWAT, &
                                                        MAVAIL, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOW, &
                                                        VEGFRA, &
                                                        ACSNOM, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                           UST, &
                                                           ZNT, &
                                                         XLAND, &
                                                         XICE

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP, &
                                                        LOWLYR


   REAL,     DIMENSION(1:num_soil_layers), INTENT(INOUT)  ::  ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::       &
                                                           RUBLTEN, &
                                                           RVBLTEN, &
                                                          EXCH_H,   &
                                                          RTHBLTEN, &
                                                          RQVBLTEN, &
                                                          RQCBLTEN, &
                                                          RQIBLTEN, &
                                                          TKE_PBL
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: QKE

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT), OPTIONAL :: &
                              massflux_EDKF, entr_EDKF, detr_EDKF & 
                                     ,thl_up, thv_up, rt_up       &
                                     ,rv_up, rc_up, u_up, v_up    &
                                     ,frac_up

   INTEGER, INTENT(IN)           ::  mfshconv 

   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::     TSK
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::  TMN
   CHARACTER(LEN=*), INTENT(IN)   :: MMINLU
   LOGICAL,  INTENT(IN)           :: allowed_to_read
   INTEGER,  INTENT(IN)           :: ISURBAN
   INTEGER :: isn, isfc
   INTEGER :: k

   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
            INTENT(OUT) :: te_temf, cf3d_temf 
   REAL, OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
            INTENT(OUT) :: wm_temf



   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ISNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:num_soil_layers, jms:jme) :: ZSNSOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: TSNOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TGXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: EAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CMXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FWETXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SNEQVOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ALBOLDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WSLAKEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ZWTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: LFMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RTMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WOODXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STBLCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FASTCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: XSAIXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MBXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHSTARXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme) :: SMOISEQ
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SMCWTDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: DEEPRECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QRFSXY  
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSPRINGSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSLATXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: AREAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: FDEPTHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: RIVERBEDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: EQZWT
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: RIVERCONDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: PEXPXY
   INTEGER, OPTIONAL, INTENT(IN)  ::  ISICE
   INTEGER , OPTIONAL,  INTENT(OUT) :: STEPWTD
   REAL , OPTIONAL, INTENT(IN) :: WTDDT





    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR  
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB  
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D 
    INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D 



    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D 

    INTEGER , INTENT(IN)        ::     num_urban_layers
    INTEGER , INTENT(IN)        ::     num_urban_hi
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TRB_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGB_URB4D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TLEV_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: QLEV_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFW1_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFW2_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFR_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFG_URB3D 
    REAL, OPTIONAL, DIMENSION( ims:ime,1:num_urban_hi, jms:jme ), INTENT(INOUT) :: HI_URB2D  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LP_URB2D  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LB_URB2D  
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: HGT_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: MH_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STDH_URB2D 
    REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(INOUT) :: LF_URB2D 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme,jms:jme),INTENT(INOUT) :: SF_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP

  real,    dimension(ims:ime,jms:jme ),intent(out)                        :: lakedepth2d,    &
                                                                             savedtke12d
  real,    dimension(ims:ime,jms:jme ),intent(inout)                      :: snowdp2d,       &
                                                                             h2osno2d,       &
                                                                             snl2d,          &
                                                                             t_grnd2d
 
  real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(out)            :: t_lake3d,       &
                                                                             lake_icefrac3d, &
                                                                             z_lake3d,       &
                                                                             dz_lake3d
  real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme ),INTENT(inout) :: t_soisno3d,     &
                                                                             h2osoi_ice3d,   &
                                                                             h2osoi_liq3d,   &
                                                                             h2osoi_vol3d,   &
                                                                             z3d,            &
                                                                             dz3d
  real,    dimension( ims:ime,1:nlevsoil, jms:jme ),INTENT(out)            :: watsat3d,       &
                                                                             csol3d,         &
                                                                             tkmg3d,         &
                                                                             tkdry3d,        &
                                                                             tksatu3d
  real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme ),INTENT(inout) :: zi3d
 
  logical,    dimension(ims:ime,jms:jme ),intent(out)                        :: lake2d
  REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  lake_depth
  REAL,              dimension(ims:ime,jms:jme ),intent(inout)      ::  lakemask
  INTEGER, INTENT(IN)      ::  lakeflag
  INTEGER, INTENT(IN)      ::   use_lakedepth
  INTEGER, INTENT(INOUT)      ::   lake_depth_flag


   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        ACHFX,ACLHF,ACGRDFLX

   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        TML,T0ML,HML,H0ML,HUML,HVML,TMOML
   INTEGER,  OPTIONAL,  INTENT(IN) :: sf_ocean_physics
   REAL,  OPTIONAL,  INTENT(IN) :: oml_hml0
   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        TSK_SAVE
   LOGICAL,  INTENT(IN) :: start_of_simulation
   INTEGER :: i,j


   INTEGER , INTENT(IN)        ::     maxpatch
   REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  HT
  integer, OPTIONAL,   dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  integer, OPTIONAL,   dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: snl
  real, OPTIONAL,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) ::  &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhtran,lhveg




  
  INTEGER, INTENT(IN) :: sf_surface_mosaic, NLCAT   
  INTEGER, INTENT(IN) :: mosaic_cat
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(IN)::   LANDUSEF
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(INOUT)::   LANDUSEF2
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT) :: mosaic_cat_index 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSK_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic 
  REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ALBEDO,ALBBCK, EMISS, EMBCK 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic
  
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT)::   TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic
  
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic,  &
                                                                                     SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                    
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic  
   
  REAL :: xice_threshold   
   
  LOGICAL :: IPRINT
  





   INTEGER :: mynn_closure_level

  if ( config_flags%fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( config_flags%fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif
 


   STEPBL = nint(BLDT*60./DT)
   STEPBL = max(STEPBL,1)


   IF(PRESENT(ACHFX))THEN
   IF(.not.restart)THEN
     DO j=jts,jte
     DO i=its,ite
        ACHFX(i,j)=0.
        ACLHF(i,j)=0.
        ACGRDFLX(i,j)=0.
        SFCEVP(i,j)=0.
     ENDDO
     ENDDO
   ENDIF
   ENDIF

   IF(PRESENT(TSK_SAVE))THEN
   IF(config_flags%fractional_seaice.EQ.1)THEN
     DO j=jts,jte
     DO i=its,ite
        TSK_SAVE(i,j)=TSK(i,j)
     ENDDO
     ENDDO
   ENDIF
   ENDIF



   sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

      CASE (SFCLAYSCHEME)
           CALL sfclayinit( allowed_to_read )
           isfc = 1
      CASE (SFCLAYREVSCHEME)

           isfc = 1
      CASE (PXSFCSCHEME)
           CALL pxsfclayinit( allowed_to_read )
           isfc = 7
      CASE (MYJSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
                                      ZNT,                     &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2

      CASE (QNSESFCSCHEME)
           CALL qnsesfcinit(LOWLYR,UST,                         &
                                      ZNT,                     &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 4

      CASE (GFSSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
                                      ZNT,                     &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2



        CASE (MYNNSFCSCHEME)

           CALL mynn_sf_init_driver(allowed_to_read)
           isfc=5


      CASE (TEMFSFCSCHEME)
           CALL wrf_debug( 100, 'calling temfsfclayinit' )
           CALL temfsfclayinit( restart, allowed_to_read ,        &
                             wm_temf,                          &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )

      CASE DEFAULT

   END SELECT sfclay_select




   sfc_select: SELECT CASE(config_flags%sf_surface_physics)

      CASE (SLABSCHEME)

           CALL slabinit(TSK,TMN,                              &
                         TSLB,ZS,DZS,num_soil_layers,          &
                         allowed_to_read ,start_of_simulation ,&
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )

      CASE (LSMSCHEME)
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )



          IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN

             IF ( PRESENT( FRC_URB2D ) .AND. PRESENT( UTYPE_URB2D )) THEN
                
                CALL urban_param_init(DZR,DZB,DZG,num_soil_layers,                   & 
                                sf_urban_physics)

                               
                
                CALL urban_var_init(ISURBAN,TSK,TSLB,TMN,IVGTYP,                     & 
                              ims,ime,jms,jme,kms,kme,num_soil_layers,               & 

                              restart,sf_urban_physics,                        & 
                              XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,     & 
                              TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,    & 
                              TRL_URB3D,TBL_URB3D,TGL_URB3D,                   & 
                              SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D, TS_URB2D,    & 
                              num_urban_layers,                                & 
                              num_urban_hi,                                    & 
                              TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,         & 
                              TLEV_URB3D,QLEV_URB3D,                           & 
                              TW1LEV_URB3D,TW2LEV_URB3D,                       & 
                              TGLEV_URB3D,TFLEV_URB3D,                         & 
                              SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,             & 
                              SFVENT_URB3D,LFVENT_URB3D,                       & 
                              SFWIN1_URB3D,SFWIN2_URB3D,                       & 
                              SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,       & 
                              LP_URB2D,HI_URB2D,LB_URB2D,                      & 
                              HGT_URB2D,MH_URB2D,STDH_URB2D,                   & 
                              LF_URB2D,                                        & 
                              A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                 & 
                              A_E_BEP,B_U_BEP,B_V_BEP,                         & 
                              B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                 & 
                              DL_U_BEP,SF_BEP,VL_BEP,                          & 
                              FRC_URB2D, UTYPE_URB2D)                            
             ELSE
                CALL wrf_error_fatal3("<stdin>",2497,&
'arguments not present for calling urban model' )
             ENDIF
          ENDIF
          


          IF (SF_surface_mosaic.eq.1) THEN

CALL lsm_mosaic_init(IVGTYP,config_flags%ISWATER,config_flags%ISURBAN,config_flags%ISICE, XLAND, XICE,config_flags%fractional_seaice,TSK,TSLB,SMOIS,SH2O,SNOW,SNOWC,SNOWH,CANWAT,  &                
                  ids,ide, jds,jde, kds,kde,  &
                  ims,ime, jms,jme, kms,kme,  &
                  its,ite, jts,jte, kts,kte, restart,             &
                  landusef,landusef2,NLCAT,num_soil_layers                  & 
                  ,sf_surface_mosaic, mosaic_cat                    & 
                  , mosaic_cat_index                              &   
                  ,TSK_mosaic,TSLB_mosaic                         &
                  ,SMOIS_mosaic,SH2O_mosaic                       & 
                  ,CANWAT_mosaic,SNOW_mosaic                      &
                  ,SNOWH_mosaic,SNOWC_mosaic                      &
                  ,ALBEDO,ALBBCK, EMISS, EMBCK,                    &         
                                                           ZNT, &
                  ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   &         
                 ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  
                 ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  
                 ,QC_URB2D_mosaic                                &  
                 ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  
                 ,TGL_URB3D_mosaic                               &  
                 ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  
                 ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  
                 ,TS_URB2D_mosaic                                &  
                 ,TS_RUL2D_mosaic                                &  
                   )

          ENDIF               
          


      CASE (NOAHMPSCHEME)
          CALL NOAHMP_INIT(MMINLU, SNOW,SNOWH,CANWAT,ISLTYP,IVGTYP,ISURBAN,                 &
                     TSLB,SMOIS,SH2O,DZS, FNDSOILW, FNDSNOWH, config_flags%isice, config_flags%iswater  ,         &
                     TSK,isnowxy  ,tvxy     ,tgxy     ,canicexy ,TMN,XICE,                  &
                     canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
                     fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
                     wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
                     stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   ,                     &
                     t2mvxy   ,t2mbxy   ,chstarxy ,            &
                     num_soil_layers, restart,                 &
                     allowed_to_read, iopt_run ,               &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 &
                     ,smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy ,dx, dy, msftx, msfty,&
                     wtddt    ,stepwtd  ,dt  ,qrfsxy ,qspringsxy  ,qslatxy,                  &
                     fdepthxy ,ht       ,riverbedxy ,eqzwt ,rivercondxy ,pexpxy              &
                     )

      CASE (RUCLSMSCHEME)


           CALL ruclsminit( SH2O,SMFR3D,TSLB,SMOIS,ISLTYP,IVGTYP,MMINLU,XICE,  &
                     mavail,num_soil_layers, config_flags%iswater,      &
                     config_flags%isice, znt, restart,                  &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE (PXLSMSCHEME)
          if(config_flags%num_land_cat .ne. 20 .and. config_flags%num_land_cat .ne. 21 .and. & 
             config_flags%num_land_cat .ne. 24 .and. config_flags%num_land_cat .ne. 28 .and. & 
             config_flags%num_land_cat .ne. 40 .and. config_flags%num_land_cat .ne. 50 )     & 
          CALL wrf_error_fatal3("<stdin>",2569,&
'module_physics_init: PX LSM option requires USGS, MODIS, or NLCD' )
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )


     CASE  (SSIBSCHEME)

          if(isfc .ne. 1)CALL wrf_error_fatal &
           ( 'module_physics_init: use sfclay scheme with SSiB' )
          if(config_flags%bl_pbl_physics .ne. 1)CALL wrf_error_fatal &
           ( 'module_physics_init: use ysu scheme with SSiB' )

          if(config_flags%ra_lw_physics .eq. 2 .or. config_flags%ra_lw_physics .gt. 4)CALL wrf_error_fatal &
           ( 'module_physics_init: SSiB only works with rrtm, cam scheme or rrtmg scheme (lw_phys=1,3,4)' )
          if(config_flags%ra_sw_physics .eq. 2 .or. config_flags%ra_sw_physics .gt. 4)CALL wrf_error_fatal &
           ( 'module_physics_init: SSiB only works with rrtm, cam scheme or rrtmg scheme (sw_phys=1,3,4)' )



      CASE (CLMSCHEME)
        IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN
                CALL wrf_error_fatal3("<stdin>",2599,&
'CLM DOES NOT WORK WITH URBAN SCHEME' ) 
        ENDIF
        IF(PRESENT(numc))THEN
        
        if((config_flags%num_land_cat .ne. 24) .AND. &
           (config_flags%num_land_cat .ne. 28) .AND. &
           (config_flags%num_land_cat .ne. 21) .AND. &
           (config_flags%num_land_cat .ne. 20)) CALL wrf_error_fatal &
            ('module_physics_init: USGS or MODIS must be used with CLM option')

            CALL CLMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,    &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,            &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS,  &
                     FNDSOILW, FNDSNOWH,                           &
                     num_soil_layers, restart,                     &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     &
                    ,maxpatch                                            &
                    ,numc,nump,snl,                                      &
                     snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                     h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                     h2osoi_liq_s1,              &
                     h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                     h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                     h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                     h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                     h2osoi_ice_s1,h2osoi_ice_s2,                        &
                     h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                     h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                     h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                     h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                     t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                     t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                     t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                     t_soisno8,t_soisno9,t_soisno10,                     &
                     dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                     snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                     t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                     t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                     h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                     h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                     h2osoi_vol7,h2osoi_vol8,                            &
                     h2osoi_vol9,h2osoi_vol10,                           &
                     ht,XLAND,XICE                                       &
                    ,ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                     Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                     SWUPsubgrid,lhsoi,lhveg,lhtran                      &
                    )
        ELSE
                CALL wrf_error_fatal3("<stdin>",2651,&
'arguments not present for calling CLM' )
        ENDIF

      CASE DEFAULT

   END SELECT sfc_select

   IF(PRESENT(SF_OCEAN_PHYSICS))THEN
     IF ( ( sf_ocean_physics .EQ. OMLSCHEME   ) .OR. &
          ( sf_ocean_physics .EQ. PWP3DSCHEME ) ) THEN
        CALL omlinit(oml_hml0, tsk,                           &
                     tml,t0ml,hml,h0ml,huml,hvml,tmoml,       &
                     allowed_to_read, start_of_simulation,    &
                     ids,ide, jds,jde, kds,kde,               &
                     ims,ime, jms,jme, kms,kme,               &
                     its,ite, jts,jte, kts,kte                )
     ENDIF
   ENDIF

     IF ( LakeModel == 1 ) THEN
 
             call  lakeini(IVGTYP,         ISLTYP,          HT,              SNOW,           & 
                           lake_min_elev,     restart,         lakedepth_default, lake_depth,     &
                           lakedepth2d,    savedtke12d,     snowdp2d,        h2osno2d,       & 
                           snl2d,          t_grnd2d,        t_lake3d,        lake_icefrac3d, &
                           z_lake3d,       dz_lake3d,       t_soisno3d,      h2osoi_ice3d,   &
                           h2osoi_liq3d,   h2osoi_vol3d,    z3d,             dz3d,           &
                           zi3d,           watsat3d,        csol3d,          tkmg3d,         &
                           config_flags%ISWATER,  xice,     xice_threshold,  xland,  tsk,    &
                           lakemask,  lakeflag,                                              &
                           lake_depth_flag, use_lakedepth,              &
                           tkdry3d,        tksatu3d,        lake2d,          its, ite, jts, jte, &
                           ims,ime, jms,jme)
     ENDIF
 


   pbl_select: SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL ysuinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MRFSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL mrfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (ACMPBLSCHEME)
           if(isfc .ne. 1 .and. isfc .ne. 7)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay or pxsfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL acminit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (GFSSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL gfsinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MYJPBLSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
          IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN
           CALL myjurbinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          ELSE

           CALL myjpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          END IF
      CASE (QNSEPBLSCHEME)
           if(isfc .ne. 4)CALL wrf_error_fatal &
            ( 'module_physics_init: use qnsesfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL qnsepblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
           

              if (mfshconv.EQ.1) &
              CALL mfshconvpblinit( massflux_EDKF, entr_EDKF, detr_EDKF & 
                                    ,thl_up, thv_up, rt_up              &
                                    ,rv_up, rc_up, u_up, v_up           &
                                    ,frac_up, restart,                  &
                                    allowed_to_read ,                   &
                                    ids, ide, jds, jde, kds, kde,       &
                                    ims, ime, jms, jme, kms, kme,       &
                                    its, ite, jts, jte, kts, kte   )  


      CASE (QNSEPBL09SCHEME)
           if(isfc .ne. 4)CALL wrf_error_fatal &
            ( 'module_physics_init: use qnsesfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL qnsepblinit09(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )

      CASE (BOULACSCHEME)
           if(isfc .ne. 1 .and. isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay or myjsfc scheme for this pbl option' )
           CALL boulacinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN, &
                         TKE_PBL,EXCH_H,restart,               &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
        CASE (CAMUWPBLSCHEME)
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL camuwpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,        &
                restart,TKE_PBL,is_CAMMGMP_used, &
                ids, ide, jds, jde, kds, kde,                          &
                ims, ime, jms, jme, kms, kme,                          &
                its, ite, jts, jte, kts, kte                           )



           
        CASE (MYNNPBLSCHEME2, MYNNPBLSCHEME3)
           IF(isfc .NE. 5 .AND. isfc .NE. 1 .AND. isfc .NE. 2) CALL wrf_error_fatal &
                ( 'module_physics_init: use mynnsfc or sfclay or myjsfc scheme for this pbl option')
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           
           SELECT CASE(config_flags%bl_pbl_physics)

             CASE(MYNNPBLSCHEME2)
                mynn_closure_level=2

             CASE(MYNNPBLSCHEME3)
                mynn_closure_level=3

             CASE DEFAULT

           END SELECT

           CALL mynn_bl_init_driver(&
                &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
                &RQIBLTEN,                                  &
                
                &QKE,                                       &
                &TKE_PBL,EXCH_H                             &
                &,restart,allowed_to_read,mynn_closure_level &
                &,IDS,IDE,JDS,JDE,KDS,KDE                    &
                &,IMS,IME,JMS,JME,KMS,KME                    &
                &,ITS,ITE,JTS,JTE,KTS,KTE)

      CASE (TEMFPBLSCHEME)
           
           
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
         IF ( PRESENT( te_temf ) .AND. PRESENT( cf3d_temf )) THEN
           CALL temfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        te_temf,cf3d_temf,                    & 
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
             ELSE
                CALL wrf_error_fatal3("<stdin>",2864,&
'arguments not present for calling TEMF scheme' )
         ENDIF


      CASE (GBMPBLSCHEME) 
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
         CALL gbmpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,   &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,TKE_PBL,           &
                        EXCH_H,                               &
                        restart,allowed_to_read ,             &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )


      CASE DEFAULT

   END SELECT pbl_select


   END SUBROUTINE bl_init


   SUBROUTINE cu_init(DX,STEPCU,CUDT,DT,RUCUTEN,RVCUTEN,RTHCUTEN,  &
                      RQVCUTEN,RQRCUTEN,RQCCUTEN,RQSCUTEN,RQICUTEN,&
                      NCA,RAINC,RAINCV,W0AVG,config_flags,restart, &
                      CLDEFI,LOWLYR,MASS_FLUX,                     &
                      RTHFTEN, RQVFTEN,                            &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,           &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,               &
                      cugd_tten,cugd_ttens,cugd_qvten,             &
                      cugd_qvtens,cugd_qcten,                      &
                      allowed_to_read, start_of_simulation,        &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte,                &
                      RQCNCUTEN,RQINCUTEN                          )                        

   USE module_cu_kf
   USE module_cu_kfeta
   USE MODULE_CU_BMJ
   USE module_cu_gd,  ONLY : GDINIT
   USE module_cu_g3,  ONLY : G3INIT
   USE module_cu_sas
   USE module_cu_mesosas
   USE module_cu_osas
   USE module_cu_camzm_driver, ONLY : zm_conv_init
   USE module_cu_nsas
   USE module_cu_tiedtke

   IMPLICIT NONE

   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart


   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT, DX
   LOGICAL , INTENT(IN)        :: start_of_simulation
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RUCUTEN, RVCUTEN, RTHCUTEN, &
            RQVCUTEN, RQCCUTEN, RQRCUTEN, RQICUTEN, RQSCUTEN
   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                        cugd_tten,cugd_ttens,cugd_qvten,            &
                        cugd_qvtens,cugd_qcten, RQCNCUTEN, RQINCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RTHFTEN, RQVFTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: CLDEFI

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: MASS_FLUX,   &
                                   APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &
                                   APR_CAPMA,APR_CAPME,APR_CAPMI

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: LOWLYR



  INTEGER :: i,j,itf,jtf





   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   STEPCU = nint(CUDT*60./DT)
   STEPCU = max(STEPCU,1)



   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINC(i,j)=0.
        RAINCV(i,j)=0.
     ENDDO
     ENDDO
   ENDIF


   cps_select: SELECT CASE(config_flags%cu_physics)

     CASE (KFSCHEME)
          CALL kfinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,        &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (BMJSCHEME)
          CALL bmjinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,       &
                      CLDEFI,LOWLYR,cp,r_d,restart,               &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (KFETASCHEME)
          CALL kf_eta_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (GDSCHEME)
          CALL gdinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,        &
                      MASS_FLUX,cp,restart,                       &
                      P_QC,P_QI,PARAM_FIRST_SCALAR,               &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (NSASSCHEME)
         CALL nsasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      RUCUTEN,RVCUTEN,                            & 
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (G3SCHEME,GFSCHEME)
          CALL g3init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,        &
                      MASS_FLUX,cp,restart,                       &
                      P_QC,P_QI,PARAM_FIRST_SCALAR,               &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      cugd_tten,cugd_ttens,cugd_qvten,            &
                      cugd_qvtens,cugd_qcten,                     &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (SASSCHEME)
          CALL sasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      RUCUTEN,RVCUTEN,                            &   
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (MESO_SAS)   
          CALL msasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      RUCUTEN,RVCUTEN,                            &   
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (OSASSCHEME)
          CALL osasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,      &
                      RUCUTEN,RVCUTEN,                            &   
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (CAMZMSCHEME)
          IF (PRESENT(RQCNCUTEN)) THEN
          CALL zm_conv_init(DT, DX, rucuten, rvcuten, rthcuten, rqvcuten, &
                      rqccuten, rqicuten, rqcncuten, rqincuten,         &
                      p_qc, p_qi, p_qnc, p_qni, param_first_scalar,     &
                      restart,                                          &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte, kts, kte                      )
          ELSE
          CALL wrf_error_fatal3("<stdin>",3080,&
'arguments not present for calling camzmscheme' )
          ENDIF


      CASE (TIEDTKESCHEME)
          CALL tiedtkeinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,   &
                      RUCUTEN,RVCUTEN,                            & 
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cu_init


   SUBROUTINE shcu_init(STEPCU,CUDT,DT,RUSHTEN,RVSHTEN,RTHSHTEN,   &
                      RQVSHTEN,RQRSHTEN,RQCSHTEN,                  &
                      RQSSHTEN,RQISHTEN,RQGSHTEN,                  &
                      NCA,RAINC,RAINCV,config_flags,restart,       &
                      allowed_to_read, start_of_simulation,        &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

   USE uwshcu,        ONLY: init_uwshcu
   USE module_shcu_grims, ONLY: grimsinit
   USE physconst,     ONLY: cpair, gravit, latice, latvap, mwdry, mwh2o, &
                            rair, zvir
   USE shr_kind_mod,  ONLY: r8 => shr_kind_r8


   IMPLICIT NONE

   TYPE (grid_config_rec_type) :: config_flags
   LOGICAL , INTENT(IN)        :: restart


   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT
   LOGICAL , INTENT(IN)        :: start_of_simulation
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RUSHTEN, RVSHTEN, RTHSHTEN, &
            RQVSHTEN, RQCSHTEN, RQRSHTEN, RQISHTEN, RQSSHTEN, RQGSHTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA



  INTEGER :: i,j,itf,jtf








   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   STEPCU = nint(CUDT*60./DT)
   STEPCU = max(STEPCU,1)



   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINC(i,j)=0.
        RAINCV(i,j)=0.
     ENDDO
     ENDDO
   ENDIF


   shcu_select: SELECT CASE(config_flags%shcu_physics)

   CASE (CAMUWSHCUSCHEME)
      CALL init_uwshcu(r8,latvap,cpair,latice,zvir,rair,gravit,      &
           mwh2o/mwdry,                                              &
           rushten, rvshten, rthshten, rqvshten,                     &
           rqcshten, rqrshten, rqishten, rqsshten, rqgshten,         &
           p_qc, p_qr, p_qi, p_qs, p_qg,                             &
           config_flags%bl_pbl_physics, param_first_scalar, restart, &
           ids, ide, jds, jde, kds, kde,                             &
           ims, ime, jms, jme, kms, kme,                             &
           its, ite, jts, jte, kts, kte                              )

   CASE (GRIMSSHCUSCHEME)
      CALL grimsinit(rthshten,rqvshten,                              &
           restart,                                                  &
           ids, ide, jds, jde, kds, kde,                             &
           ims, ime, jms, jme, kms, kme,                             &
           its, ite, jts, jte, kts, kte                              )
 
   CASE DEFAULT

   END SELECT shcu_select

   END SUBROUTINE shcu_init


   SUBROUTINE mp_init(RAINNC,SNOWNC,GRAUPELNC,config_flags,restart,warm_rain,      &
                      adv_moist_cond,                             &
                      MPDT, DT, DX, DY, LOWLYR,                   & 
                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           & 
                      mp_restart_state,tbpvs_state,tbpvs0_state,   & 
                      allowed_to_read, start_of_simulation,       &

                      ixcldliq, ixcldice, ixnumliq, ixnumice,     &       
                      nssl_cccn, nssl_alphah, nssl_alphahl,       &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs,                            &
                      z_at_q, qnwfa2d, scalar, num_sc,        &  
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
   USE module_mp_etaold
   USE module_mp_thompson
   USE module_mp_full_sbm
   USE module_mp_fast_sbm
   USE module_mp_morr_two_moment
   USE module_mp_milbrandt2mom

   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_nssl_2mom
   USE module_mp_cammgmp_driver, ONLY:CAMMGMP_INIT 

   IMPLICIT NONE


   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
   REAL    , INTENT(IN)        :: MPDT, DT, DX, DY
   REAL, INTENT(IN), OPTIONAL  :: nssl_cccn, nssl_alphah, nssl_alphahl, &
                                  nssl_cnoh, nssl_cnohl,                  &
                                  nssl_cnor, nssl_cnos,                   &
                                  nssl_rho_qh, nssl_rho_qhl,              &
                                  nssl_rho_qs

   LOGICAL , INTENT(IN)        :: start_of_simulation
   INTEGER , INTENT(IN)        :: ixcldliq, ixcldice, ixnumliq, ixnumice 

   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   INTEGER , DIMENSION( ims:ime , jms:jme ) ,INTENT(INOUT)  :: LOWLYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RAINNC,SNOWNC,GRAUPELNC
   REAL,     DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) :: &
                                  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL , DIMENSION(:) ,INTENT(INOUT)  :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL , INTENT(IN)  :: allowed_to_read
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT):: qnwfa2d                       
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN):: z_at_q                   
   INTEGER, INTENT(IN) :: num_sc                                               
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme, num_sc), INTENT(INOUT):: scalar    


   INTEGER :: i, j, itf, jtf
   REAL, DIMENSION(20) :: nssl_params

   warm_rain = .false.
   adv_moist_cond = .true.
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINNC(i,j) = 0.
        SNOWNC(i,j) = 0.
        GRAUPELNC(i,j) = 0.
     ENDDO
     ENDDO
   ENDIF
   
   IF ( present( nssl_cccn ) ) THEN
     nssl_params(1)  = nssl_cccn
     nssl_params(2)  = nssl_alphah
     nssl_params(3)  = nssl_alphahl
     nssl_params(4)  = nssl_cnoh
     nssl_params(5)  = nssl_cnohl
     nssl_params(6)  = nssl_cnor
     nssl_params(7)  = nssl_cnos
     nssl_params(8)  = nssl_rho_qh
     nssl_params(9)  = nssl_rho_qhl
     nssl_params(10) = nssl_rho_qs
   ENDIF

   mp_select: SELECT CASE(config_flags%mp_physics)

     CASE (KESSLERSCHEME)
          warm_rain = .true.
     CASE (WSM3SCHEME)
          CALL wsm3init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM5SCHEME)
          CALL wsm5init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM6SCHEME)
          CALL wsm6init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (ETAMPNEW)
         adv_moist_cond = .false.
         CALL etanewinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
     CASE (THOMPSON)
         IF(start_of_simulation.or.restart.or.config_flags%cycling)     &
            CALL thompson_init(HGT=z_at_q,                              &
                          IDS=ids, IDE=ide, JDS=jds, JDE=jde, KDS=kds, KDE=kde,   &
                          IMS=ims, IME=ime, JMS=jms, JME=jme, KMS=kms, KME=kme,   &
                          ITS=its, ITE=ite, JTS=jts, JTE=jte, KTS=kts, KTE=kte)

     CASE (THOMPSONAERO)


         IF(start_of_simulation.or.restart.or.config_flags%cycling)     &
            CALL thompson_init(HGT=z_at_q,                              &
                          NWFA2D=qnwfa2d,                               &
                          NWFA=scalar(ims,kms,jms,P_QNWFA),             &
                          NIFA=scalar(ims,kms,jms,P_QNIFA),             &
                          DX=DX, DY=DY,                                 &
                          is_start=start_of_simulation,                 &
                          IDS=ids, IDE=ide, JDS=jds, JDE=jde, KDS=kds, KDE=kde,   &
                          IMS=ims, IME=ime, JMS=jms, JME=jme, KMS=kms, KME=kme,   &
                          ITS=its, ITE=ite, JTS=jts, JTE=jte, KTS=kts, KTE=kte)

     CASE (MORR_TWO_MOMENT)
         CALL morr_two_moment_init
     CASE (MILBRANDT2MOM)
         CALL milbrandt2mom_init


     CASE (WDM5SCHEME)
          CALL wdm5init(rhoair0,rhowater,rhosnow,cliq,cpv,n_ccn0,allowed_to_read )
     CASE (WDM6SCHEME)
          CALL wdm6init(rhoair0,rhowater,rhosnow,cliq,cpv,n_ccn0,allowed_to_read )
    CASE (FULL_KHAIN_LYNN)
     IF(start_of_simulation.or.restart)THEN
          CALL full_hucminit(dt)
     END IF
    CASE (FAST_KHAIN_LYNN)
     IF(start_of_simulation.or.restart)THEN
          CALL fast_hucminit(dt)
     END IF
     CASE (NSSL_1MOMLFO)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=0,mixphase=0,ihvol=-1) 
     CASE (NSSL_1MOM)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=0,mixphase=0,ihvol=0)
     CASE (NSSL_2MOM)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=5,mixphase=0,ihvol=0)
     CASE (NSSL_2MOMCCN)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=5,mixphase=0,ihvol=0)


     CASE (CAMMGMPSCHEME) 
          CALL CAMMGMP_INIT(ixcldliq, ixcldice, ixnumliq, ixnumice &
             ,config_flags%chem_opt                          &
             ,ids, ide, jds, jde, kds, kde                   & 
             ,ims, ime, jms, jme, kms, kme                   &
             ,its, ite, jts, jte, kts, kte                   )
     CASE (ETAMPOLD)
         adv_moist_cond = .false.
         CALL etaoldinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )


     CASE DEFAULT

   END SELECT mp_select

   END SUBROUTINE mp_init


   SUBROUTINE fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,    &
                RTHNDGDTEN,RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,    &
                config_flags,restart,                           &
                allowed_to_read ,                               &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )



   USE module_fdda_psufddagd
   USE module_fdda_spnudging, ONLY : fddaspnudginginit

   IMPLICIT NONE

   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        ::     DT, FGDT
   INTEGER , INTENT(IN)        ::     id
   INTEGER , INTENT(INOUT)     ::     STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::       &
                                                           RUNDGDTEN, &
                                                           RVNDGDTEN, &
                                                          RTHNDGDTEN, &
                                                          RPHNDGDTEN, &
                                                          RQVNDGDTEN
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RMUNDGDTEN 

   LOGICAL,  INTENT(IN)           :: allowed_to_read




   STEPFG = nint(FGDT*60./DT)
   STEPFG = max(STEPFG,1)




   fdda_select: SELECT CASE(config_flags%grid_fdda)

      CASE (PSUFDDAGD)
           CALL fddagdinit(id,rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
               config_flags%run_hours, &
               config_flags%if_no_pbl_nudging_uv, &
               config_flags%if_no_pbl_nudging_t, &
               config_flags%if_no_pbl_nudging_q, &
               config_flags%if_zfac_uv, &
               config_flags%k_zfac_uv, &
               config_flags%if_zfac_t, &
               config_flags%k_zfac_t, &
               config_flags%if_zfac_q, &
               config_flags%k_zfac_q, &
               config_flags%guv, &
               config_flags%gt, config_flags%gq, &
               config_flags%if_ramping, config_flags%dtramp_min, &
               config_flags%auxinput10_end_h, &
               config_flags%grid_sfdda, &
               config_flags%guv_sfc, &
               config_flags%gt_sfc, &
               config_flags%gq_sfc, &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

      CASE (SPNUDGING)
           CALL fddaspnudginginit(id,rundgdten,rvndgdten,rthndgdten,rphndgdten,&
               config_flags%run_hours, &
               config_flags%if_no_pbl_nudging_uv, &
               config_flags%if_no_pbl_nudging_t, &
               config_flags%if_no_pbl_nudging_ph, &
               config_flags%if_zfac_uv, &
               config_flags%k_zfac_uv, &
               config_flags%dk_zfac_uv, &
               config_flags%if_zfac_t, &
               config_flags%k_zfac_t, &
               config_flags%dk_zfac_t, &
               config_flags%if_zfac_ph, &
               config_flags%k_zfac_ph, &
               config_flags%dk_zfac_ph, &
               config_flags%guv, &
               config_flags%gt, config_flags%gph, &
               config_flags%if_ramping, config_flags%dtramp_min, &
               config_flags%auxinput9_end_h, &
               config_flags%xwavenum,config_flags%ywavenum, &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

      CASE DEFAULT

   END SELECT fdda_select

   END SUBROUTINE fg_init


   SUBROUTINE fdob_init(obs_nudge_opt, maxdom, inest, parid,       &
                        idynin, dtramp, fdaend, restart,           &
                        obs_twindo_cg, obs_twindo, itimestep,      &
                        no_pbl_nudge_uv,                           &
                        no_pbl_nudge_t,                            &
                        no_pbl_nudge_q,                            &
                        sfc_scheme_horiz, sfc_scheme_vert,         &
                        maxsnd_gap,                                &
                        sfcfact, sfcfacr, dpsmx,                   &
                        nudge_wind, nudge_temp, nudge_mois,        &
                        nudgezfullr1_uv, nudgezrampr1_uv,          &
                        nudgezfullr2_uv, nudgezrampr2_uv,          &
                        nudgezfullr4_uv, nudgezrampr4_uv,          &
                        nudgezfullr1_t,  nudgezrampr1_t,           &
                        nudgezfullr2_t,  nudgezrampr2_t,           &
                        nudgezfullr4_t,  nudgezrampr4_t,           &
                        nudgezfullr1_q,  nudgezrampr1_q,           &
                        nudgezfullr2_q,  nudgezrampr2_q,           &
                        nudgezfullr4_q,  nudgezrampr4_q,           &
                        nudgezfullmin, nudgezrampmin, nudgezmax,   &
                        xlat, xlong,                               &
                        start_year, start_month, start_day,        &
                        start_hour, start_minute, start_second,    &
                        p00, t00, tlp,                             &
                        znu, p_top,                                &
                        fdob, ipf_init,                            &
                        ids, ide, jds, jde, kds, kde,              &
                        ims, ime, jms, jme, kms, kme,              &
                        its, ite, jts, jte, kts, kte               )


   USE module_domain
   USE module_fddaobs_rtfdda
   USE module_llxy

   IMPLICIT NONE

   INTEGER , INTENT(IN)    :: maxdom
   INTEGER , INTENT(IN)    :: obs_nudge_opt(maxdom)
   INTEGER , INTENT(IN)    :: ids,ide, jds,jde, kds,kde,           &
                              ims,ime, jms,jme, kms,kme,           &
                              its,ite, jts,jte, kts,kte
   INTEGER , INTENT(IN)    :: inest
   INTEGER , INTENT(IN)    :: parid(maxdom)
   INTEGER , INTENT(IN)    :: idynin          
   REAL    , INTENT(IN)    :: dtramp          
   REAL    , INTENT(IN)    :: fdaend(maxdom)  
   LOGICAL , INTENT(IN)    :: restart
   REAL    , INTENT(IN)    :: obs_twindo_cg   
   REAL    , INTENT(IN)    :: obs_twindo
   INTEGER , INTENT(IN)    :: itimestep
   INTEGER , INTENT(IN)    :: no_pbl_nudge_uv(maxdom)  
   INTEGER , INTENT(IN)    :: no_pbl_nudge_t(maxdom)   
   INTEGER , INTENT(IN)    :: no_pbl_nudge_q(maxdom)   
   INTEGER , INTENT(IN)    :: sfc_scheme_horiz 
   INTEGER , INTENT(IN)    :: sfc_scheme_vert  
   REAL    , INTENT(IN)    :: maxsnd_gap       
   REAL    , INTENT(IN)    :: sfcfact      
   REAL    , INTENT(IN)    :: sfcfacr      
   REAL    , INTENT(IN)    :: dpsmx        
   INTEGER , INTENT(IN)    :: nudge_wind(maxdom)       
   INTEGER , INTENT(IN)    :: nudge_temp(maxdom)       
   INTEGER , INTENT(IN)    :: nudge_mois(maxdom)       
   REAL    , INTENT(IN)    :: nudgezfullr1_uv  
   REAL    , INTENT(IN)    :: nudgezrampr1_uv  
   REAL    , INTENT(IN)    :: nudgezfullr2_uv  
   REAL    , INTENT(IN)    :: nudgezrampr2_uv  
   REAL    , INTENT(IN)    :: nudgezfullr4_uv  
   REAL    , INTENT(IN)    :: nudgezrampr4_uv  
   REAL    , INTENT(IN)    :: nudgezfullr1_t   
   REAL    , INTENT(IN)    :: nudgezrampr1_t   
   REAL    , INTENT(IN)    :: nudgezfullr2_t   
   REAL    , INTENT(IN)    :: nudgezrampr2_t   
   REAL    , INTENT(IN)    :: nudgezfullr4_t   
   REAL    , INTENT(IN)    :: nudgezrampr4_t   
   REAL    , INTENT(IN)    :: nudgezfullr1_q   
   REAL    , INTENT(IN)    :: nudgezrampr1_q   
   REAL    , INTENT(IN)    :: nudgezfullr2_q   
   REAL    , INTENT(IN)    :: nudgezrampr2_q   
   REAL    , INTENT(IN)    :: nudgezfullr4_q   
   REAL    , INTENT(IN)    :: nudgezrampr4_q   
   REAL    , INTENT(IN)    :: nudgezfullmin    
   REAL    , INTENT(IN)    :: nudgezrampmin    
   REAL    , INTENT(IN)    :: nudgezmax        
   REAL    , INTENT(IN)    :: xlat ( ims:ime, jms:jme )        
   REAL    , INTENT(IN)    :: xlong( ims:ime, jms:jme )        
   INTEGER , INTENT(INOUT) :: start_year
   INTEGER , INTENT(INOUT) :: start_month
   INTEGER , INTENT(INOUT) :: start_day
   INTEGER , INTENT(INOUT) :: start_hour
   INTEGER , INTENT(INOUT) :: start_minute
   INTEGER , INTENT(INOUT) :: start_second
   REAL    , INTENT(IN)    :: p00                      
   REAL    , INTENT(IN)    :: t00                      
   REAL    , INTENT(IN)    :: tlp                      
   REAL    , INTENT(IN)    :: znu( kms:kme )           
   REAL    , INTENT(IN)    :: p_top                    
   TYPE(fdob_type), INTENT(INOUT)  :: fdob

   INTEGER                 :: e_sn         
   LOGICAL                 :: ipf_init     



      IF ( obs_nudge_opt(inest) .eq. 0 ) RETURN

      e_sn = jde
      CALL fddaobs_init(obs_nudge_opt, maxdom, inest, parid,       &
                        idynin, dtramp, fdaend, restart,           &
                        obs_twindo_cg,                             &
                        obs_twindo, itimestep,                     &
                        no_pbl_nudge_uv,                           &
                        no_pbl_nudge_t,                            &
                        no_pbl_nudge_q,                            &
                        sfc_scheme_horiz, sfc_scheme_vert,         &
                        maxsnd_gap,                                &
                        sfcfact, sfcfacr, dpsmx,                   &
                        nudge_wind, nudge_temp, nudge_mois,        &
                        nudgezfullr1_uv, nudgezrampr1_uv,          &
                        nudgezfullr2_uv, nudgezrampr2_uv,          &
                        nudgezfullr4_uv, nudgezrampr4_uv,          &
                        nudgezfullr1_t,  nudgezrampr1_t,           &
                        nudgezfullr2_t,  nudgezrampr2_t,           &
                        nudgezfullr4_t,  nudgezrampr4_t,           &
                        nudgezfullr1_q,  nudgezrampr1_q,           &
                        nudgezfullr2_q,  nudgezrampr2_q,           &
                        nudgezfullr4_q,  nudgezrampr4_q,           &
                        nudgezfullmin,  nudgezrampmin, nudgezmax,  &
                        xlat, xlong,                               &
                        start_year, start_month, start_day,        &
                        start_hour, start_minute, start_second,    &
                        p00, t00, tlp,                             &
                        znu, p_top,                                &
                        fdob, ipf_init,                            &
                        ids,ide, jds,jde, kds,kde,                 &
                        ims,ime, jms,jme, kms,kme,                 &
                        its,ite, jts,jte, kts,kte)

   END SUBROUTINE fdob_init


   SUBROUTINE z2sigma(zf,zh,sf,sh,p_top,pptop,config_flags, &
                allowed_to_read , &
                kds,kde,kms,kme,kts,kte)
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: kds,kde,kms,kme,kts,kte
   REAL , DIMENSION( kms:kme ), INTENT(IN) :: zf,zh
   REAL , DIMENSION( kms:kme ), INTENT(OUT):: sf,sh
   REAL , INTENT(IN) :: p_top
   REAL , INTENT(OUT) :: pptop
   TYPE (grid_config_rec_type)              :: config_flags
   LOGICAL , INTENT(IN) :: allowed_to_read

   REAL R, G, TS, GAMMA, PS, ZTROP, TSTRAT, PTROP, Z, T, P, ZTOP, PTOP
   INTEGER K

   IF(zf(kde/2) .GT. 1.0)THEN



      r=287.05
      g=9.80665
      ts=288.15
      gamma=-6.5/1000.
      ps=1013.25
      ztrop=11000.
      tstrat=ts+gamma*ztrop
      ptrop=ps*(tstrat/ts)**(-g/(gamma*r))

      do k=kde,kds,-1

        z=zf(k)
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        if(k.eq.kde)then
          ztop=zf(k)
          ptop=p
        endif
        sf(k)=(p-ptop)/(ps-ptop)

        if(k.ne.kds)then
        z=0.5*(zf(k)+zf(k-1))
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        sh(k-1)=(p-ptop)/(ps-ptop)
        endif
      enddo
      pptop=ptop/10.
   ELSE

      do k=kde,kds,-1


         sf(k)=zf(k)
         if(k .ne. kde)sh(k)=zh(k)
      enddo
      pptop=p_top/1000.

   ENDIF

   END SUBROUTINE z2sigma


   SUBROUTINE CAM_INIT (ixcldliq, ixcldice, ixnumliq, ixnumice,config_flags)







     USE shr_kind_mod,               ONLY : r8 => shr_kind_r8
     USE module_cam_esinti,          ONLY : esinti
     USE physconst,                  ONLY : mwh2o, cpwv, epsilo, latvap, latice &
          , rh2o, cpair, tmelt,mwdry
     USE constituents,               ONLY : cnst_add
     USE module_cam_support,         ONLY : pcnst =>pcnst_runtime, pcnst_mp
     USE modal_aero_initialize_data_phys, ONLY : modal_aero_initialize_phys
     
     implicit none

     TYPE (grid_config_rec_type)              :: config_flags

     integer, intent(out) :: ixcldliq, ixcldice, ixnumliq, ixnumice

     
     
     integer, parameter  :: ncnstmax = 4                    
     integer             :: mm
     character(len=8), dimension(ncnstmax), parameter :: cnst_names = & 
          (/'CLDLIQ', 'CLDICE','NUMLIQ','NUMICE'/)         
     
     integer  :: dumind 
     real(r8) :: one

     
     pcnst = 5                                               
     IF(config_flags%mp_physics == CAMMGMPSCHEME) pcnst = 12 

     
     pcnst_mp = pcnst


     
     call esinti(epsilo, latvap, latice, rh2o, cpair, tmelt)
     
     IF(.NOT.CAM_INITIALIZED) THEN
        
        
        call ALLOCATE_CAM_ARRAYS()     
        
        
        
        
        
        
        
        
        call cnst_add('Q', mwh2o, cpwv, 1.E-12_r8, mm, &
             longname='Specific humidity', readiv=.true. )
        
        
        
        
        call cnst_add(cnst_names(1), mwdry, cpair, 0._r8, ixcldliq, &
             longname='Grid box averaged cloud liquid amount')
        call cnst_add(cnst_names(2), mwdry, cpair, 0._r8, ixcldice, &
             longname='Grid box averaged cloud ice amount'   )
        call cnst_add(cnst_names(3), mwdry, cpair, 0._r8, ixnumliq, &
             longname='Grid box averaged cloud liquid number')
        call cnst_add(cnst_names(4), mwdry, cpair, 0._r8, ixnumice, &
             longname='Grid box averaged cloud ice number'   )

        
        IF(config_flags%mp_physics .EQ. CAMMGMPSCHEME &
             )THEN
           one = 1.0_r8
           call cnst_add('ACCUM_MASS', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged accumulation mode mass')
           call cnst_add('ACCUM_NUM' , one, cpair, 0._r8, dumind, &
                longname='Grid box averaged accumulation mode number')
           call cnst_add('AITKEN_MASS', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged aitken mode mass')
           call cnst_add('AITKEN_NUM' , one, cpair, 0._r8, dumind, &
                longname='Grid box averaged aitken mode number')
           call cnst_add('COARSE_MASS_1', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged coarse mode1 mass')
           call cnst_add('COARSE_MASS_2', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged coarse mode2 mass')
           call cnst_add('COARSE_NUM' , one, cpair, 0._r8, dumind, &
                longname='Grid box averaged coarse mode number')
           
        ENDIF
        
        CAM_INITIALIZED = .TRUE.
     ENDIF
     
     IF(config_flags%mp_physics == CAMMGMPSCHEME)THEN
        
        CALL modal_aero_initialize_phys
     ENDIF
   END SUBROUTINE CAM_INIT



 SUBROUTINE ALLOCATE_CAM_ARRAYS ()







   USE constituents,         ONLY : cnst_name,cnst_longname,cnst_cp,&
        cnst_cv,cnst_mw,cnst_type,cnst_rgas,qmin,qmincg,            &
        cnst_fixed_ubc,apcnst,bpcnst,hadvnam,vadvnam,dcconnam,      &
        fixcnam,tendnam,ptendnam,dmetendnam,sflxnam,tottnam  

   USE module_cam_support,   ONLY : pcnst =>pcnst_runtime, pcnst_mp

   USE modal_aero_data,      ONLY : cnst_name_cw,species_class,     &
        qneg3_worst_thresh_amode,cnst_name_cw_mp

   implicit none

   
   Allocate(cnst_name(pcnst),cnst_longname(pcnst),cnst_cp(pcnst),   &
        cnst_cv(pcnst),cnst_mw(pcnst),cnst_type(pcnst),             &
        cnst_rgas(pcnst),qmin(pcnst),qmincg(pcnst),                 &
        cnst_fixed_ubc(pcnst),apcnst(pcnst),bpcnst(pcnst),          &
        hadvnam(pcnst),vadvnam(pcnst),dcconnam(pcnst),              &
        fixcnam(pcnst),tendnam(pcnst),ptendnam(pcnst),              &
        dmetendnam(pcnst),sflxnam(pcnst),tottnam(pcnst)             )

   
   cnst_fixed_ubc(:) = .false.

   
   Allocate(cnst_name_cw(pcnst),cnst_name_cw_mp(pcnst_mp),          &
        species_class(pcnst),qneg3_worst_thresh_amode(pcnst)        )
   
 END SUBROUTINE ALLOCATE_CAM_ARRAYS

subroutine aerosol_in(aerodm,pina,alevsiz,no_months,no_src_types,XLAT,XLONG,   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)




   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte   

   INTEGER,      INTENT(IN   )    ::   alevsiz, no_months, no_src_types

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::     XLAT, XLONG

   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_months, no_src_types ),      &
          INTENT(OUT   ) ::                                  aerodm

   REAL,  DIMENSION(alevsiz), INTENT(OUT )  ::                   pina






   INTEGER, PARAMETER :: latsiz = 46
   INTEGER, PARAMETER :: lonsiz = 72
   INTEGER :: i, j, k, itf, jtf, ktf, m, pin_unit, lat_unit, lon_unit, od_unit, ks, il, jl
   INTEGER :: ilon1, ilon2, jlat1, jlat2
   REAL    :: interp_pt, interp_pt_lat, interp_pt_lon, wlat1, wlat2, wlon1, wlon2
   CHARACTER*256 :: message

   REAL,  DIMENSION( lonsiz, alevsiz, latsiz, no_months, no_src_types )    ::   &
                                                            aerodin

   REAL,  DIMENSION(latsiz)                ::             lat_od, aertmp1
   REAL,  DIMENSION(lonsiz)                ::             lon_od, aertmp2

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)



     WRITE(message,*)'no_months = ',no_months
     CALL wrf_debug(1,message)


      pin_unit = 27
        OPEN(pin_unit, FILE='aerosol_plev.formatted',FORM='FORMATTED',STATUS='OLD')
        do k = 1,alevsiz
        READ (pin_unit,*) pina(k)
        end do
      close(27)







      lat_unit = 28
        OPEN(lat_unit, FILE='aerosol_lat.formatted',FORM='FORMATTED',STATUS='OLD')
        do j = 1,latsiz
        READ (lat_unit,*) lat_od(j)
        end do
      close(28)



      lon_unit = 29
        OPEN(lon_unit, FILE='aerosol_lon.formatted',FORM='FORMATTED',STATUS='OLD')
        do j = 1,lonsiz
        READ (lon_unit,*) lon_od(j)
        end do
      close(29)



      od_unit = 30
      OPEN(od_unit, FILE='aerosol.formatted',FORM='FORMATTED',STATUS='OLD')

      do ks=1,no_src_types
      do m=1,no_months
      do j=1,latsiz  
      do k=1,alevsiz 
      do i=1,lonsiz  
         READ (od_unit,*) aerodin(i,k,j,m,ks)
      enddo
      enddo
      enddo
      enddo
      enddo
      close(30)















      do j=jts,jtf
      do i=its,itf
        interp_pt_lat=XLAT(i,j)
        interp_pt_lon=XLONG(i,j)
        call interp_vec(lat_od,interp_pt_lat,.true.,jlat1,jlat2,wlat1,wlat2)
        call interp_vec(lon_od,interp_pt_lon,.true.,ilon1,ilon2,wlon1,wlon2)

        do ks = 1,no_src_types
        do m  = 1,no_months
        do k  = 1,alevsiz
          aerodm(i,k,j,m,ks) = wlon1 * (wlat1 * aerodin(ilon1,k,jlat1,m,ks)  + &
                                        wlat2 * aerodin(ilon1,k,jlat2,m,ks)) + &
                               wlon2 * (wlat1 * aerodin(ilon2,k,jlat1,m,ks)  + &
                                        wlat2 * aerodin(ilon2,k,jlat2,m,ks))
        end do
        end do
        end do

      end do
      end do







END SUBROUTINE aerosol_in

  function lin_interp(x, f, y) result(g)

    
    
    
    
    
    

    

    implicit none

    real, intent(in), dimension(:) :: x  
    real, intent(in), dimension(:) :: f  
    real, intent(in) :: y                
    real :: g                            

    integer :: k  
    integer :: n  
    real    :: a

    n = size(x)

    
    

    if (y <= x(1)) then
      k = 1
    else if (y >= x(n)) then
      k = n - 1
    else
      k = 1
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if

    
    a = (  f(k+1) - f(k) ) / ( x(k+1) - x(k) )
    g = f(k) + a * (y - x(k))

  end function lin_interp

  subroutine interp_vec(locvec,locwant,periodic,loc1,loc2,wght1,wght2)

  implicit none

  real, intent(in), dimension(:) :: locvec
  real, intent(in)               :: locwant
  logical, intent(in)            :: periodic
  integer, intent(out)           :: loc1, loc2
  real, intent(out)              :: wght1, wght2

  integer :: vsize, n
  real    :: locv1, locv2

  vsize = size(locvec)

  loc1 = -1
  loc2 = -1

  do n = 1, vsize-1
    if ( locvec(n) <= locwant .and. locvec(n+1) > locwant ) then
      loc1  = n
      loc2  = n+1
      locv1 = locvec(n)
      locv2 = locvec(n+1)
      exit
    end if
  end do

  if ( loc1 < 0 .and. loc2 < 0 ) then
    if ( periodic ) then
      if ( locwant < locvec(1) ) then
        loc1  = vsize
        loc2  = 1
        locv1 = locvec(vsize)-360.0
        locv2 = locvec(1)
      else
        loc1  = vsize
        loc2  = 1
        locv1 = locvec(vsize)
        locv2 = locvec(1)+360.0
      end if
    else
      if ( locwant < locvec(1) ) then
        loc1  = 1
        loc2  = 1
        locv1 = locvec(1)
        locv2 = locvec(1)
      else
        loc1  = vsize
        loc2  = vsize
        locv1 = locvec(vsize)
        locv2 = locvec(vsize)
      end if
    end if
  end if

  wght2 = (locwant-locv1) / (locv2-locv1)
  wght1 = 1.0 - wght2

  return
  end subroutine interp_vec

END MODULE module_physics_init
