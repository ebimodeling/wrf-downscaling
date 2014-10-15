


MODULE module_pbl_driver
CONTAINS


   SUBROUTINE pbl_driver(                                          &
                  itimestep,dt,u_frame,v_frame                     &
                 ,bldt,curr_secs,adapt_step_flag                   &
                 ,bldtacttime                                      & 
                 ,rublten,rvblten,rthblten                         &
                 ,tsk,xland,znt                                    &



                 ,ht                                               &   
                 ,ust,pblh,hfx,qfx,grdflx                          &
                 ,u_phy,v_phy,th_phy,rho                           &
                 ,p_phy,pi_phy,p8w,t_phy,dz8w,z                    &
                 ,exch_h,exch_m,akhs,akms                          &
                 ,thz0,qz0,uz0,vz0,qsfc,f                          &
                 ,lowlyr,u10,v10,uoce,voce,t2                      &
                 ,psim,psih,fm,fhh,gz1oz0, wspd,br,chklowq         &
                 ,bl_pbl_physics, ra_lw_physics, dx                &
                 ,stepbl,warm_rain                                 &
                 ,kpbl,mixht,ct,lh,snow,xice                       &
                 ,znu, znw, mut, p_top                             &

                 ,ctopo,ctopo2,windfarm_opt,power                  &
              
                 ,te_temf,km_temf,kh_temf                     &
                 ,shf_temf,qf_temf,uw_temf,vw_temf                &
                 ,hd_temf,lcl_temf,hct_temf                       &
                 ,wupd_temf,mf_temf,thup_temf,qtup_temf,qlup_temf &
                 ,exch_temf,cf3d_temf,cfm_temf                    &
                 ,flhc,flqc                                        &
              
                 ,qke,Sh3d                                         &
                 ,qke_adv,bl_mynn_tkeadvect                        & 
                 ,tsq,qsq,cov,rmol,ch,qcg,grav_settling            & 
                 ,dqke,qWT,qSHEAR,qBUOY,qDISS,bl_mynn_tkebudget    & 
                 ,bl_mynn_cloudpdf                                 & 
                 ,vdfg                                             & 
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,i_start,i_end, j_start,j_end, kts,kte, num_tiles &
             
                 ,hol, mol, regime                                 &
             
                 ,gwd_opt                                          &
                 ,dtaux3d,dtauy3d                                  &
                 ,dusfcg,dvsfcg,var2d,oc12d                        &
                 ,oa1,oa2,oa3,oa4,ol1,ol2,ol3,ol4                  &
             
                 ,qv_curr, qc_curr, qr_curr                        &
                 ,qi_curr, qs_curr, qg_curr                        &
                 ,rqvblten,rqcblten,rqiblten                       &
                 ,rqrblten,rqsblten,rqgblten                       &
             
                 ,f_qv,f_qc,f_qr                                   &
                 ,f_qi,f_qs,f_qg                                   &

               ,frc_urb2d                                  &
               ,a_u_bep,a_v_bep,a_t_bep,a_q_bep            &
               ,b_u_bep,b_v_bep,b_t_bep,b_q_bep            &
               ,sf_bep,vl_bep                              &
               ,sf_sfclay_physics,sf_urban_physics         &
               ,tke_pbl,el_pbl                             &
               ,wu_tur,wv_tur,wt_tur,wq_tur &

               ,exch_tke, rthraten                         &
               ,a_e_bep,b_e_bep,dlg_bep,dl_u_bep           &
               ,mfshconv, massflux_EDKF, entr_EDKF, detr_EDKF    & 
               ,thl_up, thv_up, rt_up ,rv_up, rc_up, u_up, v_up    &
               ,frac_up,rc_mf                                      & 

               ,phb,xlat_u,xlong_u,xlat_v,xlong_v,id                 &

               , z_at_w,cldfra_old_mp,cldfra, rthratenlw             &
               , tauresx2d,tauresy2d                                 &
               , tpert2d,qpert2d,wpert2d,wsedl3d                     &
               , turbtype3d,smaw3d                                   &
               , fnm,fnp                                             &

               ,qnc_curr,f_qnc,qni_curr,f_qni,rqniblten              &
               ,IS_CAMMGMP_USED                                      &

               , wstar,delta                                         &

     &        ,scalar,scalar_tend,num_scalar                         &
     &        ,tracer,tracer_tend,num_tracer                         &
     &        ,scalar_pblmix,tracer_pblmix                           &
                                                                     )       

   USE module_state_description, ONLY :                            &
                   YSUSCHEME,MRFSCHEME,GFSSCHEME,MYJPBLSCHEME,ACMPBLSCHEME,&
                   QNSEPBLSCHEME,MYNNPBLSCHEME2,MYNNPBLSCHEME3,BOULACSCHEME,&
                   CAMUWPBLSCHEME,BEPSCHEME,BEP_BEMSCHEME,MYJSFCSCHEME, &
                   FITCHSCHEME,                                      &
                   TEMFPBLSCHEME,QNSEPBL09SCHEME,GBMPBLSCHEME,  &
                   CAMMGMPSCHEME,p_qi,p_qni,p_qnc,param_first_scalar  

   USE module_model_constants



   USE module_bl_myjpbl
   USE module_bl_qnsepbl
   USE module_bl_qnsepbl09
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_bl_gfs2011, only: bl_gfs2011
   USE module_bl_acm
   USE module_bl_gwdo
   USE module_bl_myjurb
   USE module_bl_boulac
   USE module_bl_camuwpbl_driver, only:CAMUWPBL
   USE module_bl_temf
   USE module_bl_mfshconvpbl
   USE module_bl_gbmpbl
   USE module_bl_mynn
   USE module_bl_fogdes
   USE module_wind_fitch

   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   

   IMPLICIT NONE
















































































































































































   INTEGER,    INTENT(IN   )    ::     bl_pbl_physics, ra_lw_physics,sf_sfclay_physics,sf_urban_physics,windfarm_opt
   INTEGER,    OPTIONAL,  INTENT(IN   )    ::     scalar_pblmix, tracer_pblmix

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       kts,kte, num_tiles
   INTEGER,    INTENT(IN   )    ::     num_scalar, num_tracer

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end

   INTEGER,    INTENT(IN   )    ::     itimestep,STEPBL
   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                        LOWLYR

   LOGICAL,      INTENT(IN   )    ::   warm_rain
   LOGICAL,      INTENT(IN   )    ::   is_CAMMGMP_used 


   REAL,       DIMENSION( kms:kme ),                              &
               OPTIONAL, INTENT(IN   )    ::               znu,   &
                                                           znw

   REAL,       INTENT(IN   )    ::     DT,DX
   REAL,       INTENT(IN   ),OPTIONAL    ::     bldt
   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
   LOGICAL,    INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL,       INTENT(INOUT),OPTIONAL    ::     bldtacttime  

   REAL, DIMENSION( ims:ime, kms:kme ,jms:jme ), &
         INTENT(IN), OPTIONAL    :: phb
   REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN), OPTIONAL    :: xlat_u,xlong_u,xlat_v,xlong_v


   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(IN   )    ::                         p_phy, &
                                                          pi_phy, &
                                                             p8w, &
                                                             rho, &
                                                           t_phy, &
                                                           u_phy, &
                                                           v_phy, &
                                                            dz8w, &
                                                               z, &
                                                          th_phy

   REAL , DIMENSION( kms:kme ) ,                                      &
        INTENT(IN   ) , OPTIONAL ::                                        fnm,  & 
                                                                fnp                                                                  

    REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),           &
               INTENT(IN   ), OPTIONAL    ::              z_at_w, &
                                                   cldfra_old_mp, &
                                                          cldfra, &
                                                      rthratenlw, &
                                                      wsedl3d



    REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(INOUT   ), OPTIONAL    ::        tauresx2d, &
                                                       tauresy2d, &
                                                         tpert2d, &
                                                         qpert2d, &
                                                         wpert2d



    REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),           &
               INTENT(OUT) , OPTIONAL   ::                      turbtype3d, &
                                                          smaw3d



    REAL,      DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT   ), OPTIONAL    ::          wstar
    REAL,      DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT   ), OPTIONAL    ::            delta

   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                         XLAND, &
                                                              HT, &
                                                            PSIM, &
                                                            PSIH, &
                                                              FM, &
                                                             FHH, &
                                                          GZ1OZ0, &
                                                              BR, &
                                                               F, &
                                                         CHKLOWQ

   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)    ::                           TSK, &
                                                             UST, &
                                                            PBLH, &
                                                             HFX, &
                                                             QFX, &
                                                             ZNT, &
                                                            QSFC, &
                                                            AKHS, &
                                                            AKMS, &
                                                           MIXHT, &
                                                             QZ0, &
                                                            THZ0, &
                                                             UZ0, &
                                                             VZ0, &
                                                              CT, &
                                                          GRDFLX, &
                                                             U10, &
                                                             V10, &
                                                            UOCE, &
                                                            VOCE, &
                                                              T2, &
                                                           POWER, &
                                                            WSPD


   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       RUBLTEN, &
                                                         RVBLTEN, &
                                                        RTHBLTEN, &
                                           EXCH_H,EXCH_M,TKE_PBL, &
                                                        RTHRATEN  
 REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(OUT)    ::                       WU_TUR,WV_TUR,WT_TUR,WQ_TUR



   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),     &
          INTENT(INOUT) ::        tsq,qsq,cov, & 
                                  qke,Sh3d,                    &
                                  dqke,qWT,qSHEAR,qBUOY,qDISS 
   INTEGER, OPTIONAL, INTENT(IN)  :: bl_mynn_tkebudget,        &
                                     grav_settling,            &
                                     bl_mynn_cloudpdf

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
        & INTENT(INOUT) :: qke_adv
   LOGICAL, OPTIONAL, INTENT(IN) :: bl_mynn_tkeadvect


   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,              &
        &    INTENT(INOUT)::                               vdfg


   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
        & INTENT(INOUT) ::  exch_tke      


   INTEGER, OPTIONAL :: id

   REAL,    DIMENSION( ims:ime , jms:jme ), &
        &OPTIONAL, INTENT(IN) ::  &
        & qcg, rmol, ch
   



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(OUT)    ::                          EL_PBL

   REAL ,                             INTENT(IN   )  ::  u_frame, &
                                                         v_frame


   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(INOUT) ::                             KPBL

   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN)    :: XICE, SNOW, LH


   real, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::FRC_URB2D   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep        

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep           
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep            
 


   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT) :: te_temf
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(  OUT) :: km_temf, kh_temf,        &
                         shf_temf,qf_temf,uw_temf,vw_temf,        &
                         wupd_temf,mf_temf,thup_temf,qtup_temf,   &
                         qlup_temf,cf3d_temf
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),            &
               INTENT(INOUT) :: flhc,flqc
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),            &
               INTENT(  OUT) :: hd_temf, lcl_temf, hct_temf, cfm_temf
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),            &
               INTENT(INOUT) :: exch_temf












   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg      &
                                                     ,f_qnc     & 
                                                     ,f_qni       

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      
                      
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  &
                     ,qnc_curr,qni_curr                          & 
                     ,rqvblten,rqcblten,rqrblten                 &
                     ,rqiblten,rqsblten,rqgblten,rqniblten         


   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               OPTIONAL                                         , &
               INTENT(INOUT)    ::                           HOL, &
                                                             MOL, &
                                                          REGIME
   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               OPTIONAL                                         , &
               INTENT(IN)    ::                           mut

   INTEGER,    OPTIONAL, INTENT(IN)    ::               gwd_opt
   REAL,       OPTIONAL, INTENT(IN)    ::               p_top

  real,   dimension( ims:ime, kms:kme, jms:jme )                             , &
          optional                                                           , &
             intent(inout  )   ::                                     dtaux3d, &
                                                                      dtauy3d

  real,   dimension( ims:ime, jms:jme )                                      , &
          optional                                                           , &
             intent(inout  )   ::                                      dusfcg, &
                                                                       dvsfcg

  real,   dimension( ims:ime, jms:jme )                                      , &
          optional                                                           , &
             intent(in  )   ::                                          var2d, &
                                                                        oc12d, &
                                                              oa1,oa2,oa3,oa4, &
                                                              ol1,ol2,ol3,ol4

   REAL, OPTIONAL,   DIMENSION( ims:ime , jms:jme ),                    &  
               INTENT(IN   )    ::                        CTOPO, &
                                                          CTOPO2


   INTEGER, INTENT(IN) ::  mfshconv

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,  &
               INTENT(OUT) ::    massflux_EDKF, entr_EDKF, detr_EDKF & 
                                     ,thl_up, thv_up, rt_up       &
                                     ,rv_up, rc_up, u_up, v_up    &
                                     ,frac_up,rc_mf  

    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer
    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer_tend
    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar
    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar_tend




   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  TSKOLD, &
                                                          USTOLD, &
                                                          ZNTOLD, &
                                                             ZOL, &
                                                            PSFC










   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_u        
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_v        
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_t        
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_q        

   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_u        
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_v        
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_t        
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_q        

   REAL, ALLOCATABLE, DIMENSION( :, :, : )::sf         
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::vl         

   REAL    :: DTMIN,DTBL

   INTEGER :: initflag

   INTEGER :: i,J,K,NK,jj,ij,its,ite,jts,jte
   LOGICAL :: radiation
   LOGICAL :: flag_bep
   LOGICAL :: flag_myjsfc
   LOGICAL :: flag_qv, flag_qc, flag_qr, flag_qi, flag_qs, flag_qg, flag_qnc,flag_qni 
   CHARACTER*256 :: message
   REAL    :: next_bl_time
   LOGICAL :: run_param , doing_adapt_dt , decided
   LOGICAL :: do_adapt
   integer iu_bep,iurb,idiff
   real seamask,thsk,zzz,unew,vnew,tnew,qnew,umom,vmom
   REAL :: z0,z1,z2,w1,w2




    SELECT CASE(sf_urban_physics)
      CASE (BEPSCHEME)
        flag_bep=.true.
      CASE (BEP_BEMSCHEME)
        flag_bep=.true.
      CASE DEFAULT
        flag_bep=.false.
    END SELECT

    SELECT CASE(sf_sfclay_physics)
      CASE (MYJSFCSCHEME)
         flag_myjsfc=.true.
      CASE DEFAULT
         flag_myjsfc=.false.
    END SELECT

  flag_qv = .FALSE. ; IF ( PRESENT( F_QV ) ) flag_qv = F_QV
  flag_qc = .FALSE. ; IF ( PRESENT( F_QC ) ) flag_qc = F_QC
  flag_qr = .FALSE. ; IF ( PRESENT( F_QR ) ) flag_qr = F_QR
  flag_qi = .FALSE. ; IF ( PRESENT( F_QI ) ) flag_qi = F_QI
  flag_qs = .FALSE. ; IF ( PRESENT( F_QS ) ) flag_qs = F_QS
  flag_qg = .FALSE. ; IF ( PRESENT( F_QG ) ) flag_qg = F_QG
  flag_qnc = .FALSE. ; IF ( PRESENT( F_QNC ) ) flag_qnc = F_QNC 
  flag_qni = .FALSE. ; IF ( PRESENT( F_QNI ) ) flag_qni = F_QNI 

  if (bl_pbl_physics .eq. 0) return



   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
         IF ( bldtacttime .eq. 0. ) THEN
            bldtacttime = CURR_SECS + bldt*60.
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

   IF ( PRESENT(bldt) )THEN
      IF ( ( .NOT. decided ) .AND. &
           ( ( bldt .EQ. 0. ) .OR. ( stepbl .EQ. 1 ) ) ) THEN
         run_param   = .TRUE.
         decided     = .TRUE.
      END IF
   ELSE
      IF ( ( .NOT. decided ) .AND. &
                                   ( stepbl .EQ. 1 )   ) THEN
         run_param   = .TRUE.
         decided     = .TRUE.
      END IF
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepbl) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. bldtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      bldtacttime = curr_secs + bldt*60
   END IF


 IF (run_param) THEN
  radiation = .false.
  IF (ra_lw_physics .gt. 0) radiation = .true.



 
   DTMIN=DT/60.


    if (PRESENT(adapt_step_flag)) then
       if (adapt_step_flag) then
          do_adapt = .TRUE.
       else
          do_adapt = .FALSE.
       endif
    else
       do_adapt = .FALSE.
    endif

   if (PRESENT(BLDT)) then
      if (bldt .eq. 0) then
         DTBL = dt
      ELSE
         if (do_adapt) then
            IF ( curr_secs .LT. 2. * dt ) THEN
               call wrf_message("WARNING: When using an adaptive time-step the boundary layer"// &
                                " time-step should be 0 (i.e., equivalent to model time-step).  ")
               call wrf_message("In order to proceed, for boundary layer calculations, the "// &
                                "boundary layer time-step"// &
                                 " will be rounded to the nearest minute," )
                call wrf_message("possibly resulting in innacurate results.")
            END IF
            DTBL=bldt*60
         else
            DTBL=DT*STEPBL
         endif
      endif
   else
      DTBL=DT*STEPBL
   endif



       idiff=0

   IF ( idiff .EQ. 1 ) THEN
     ALLOCATE (a_u(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (a_v(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (a_t(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (a_q(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (b_u(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (b_v(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (b_t(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (b_q(ims:ime,kms:kme,jms:jme))       
     ALLOCATE (sf(ims:ime,kms:kme,jms:jme) )       
     ALLOCATE (vl(ims:ime,kms:kme,jms:jme) )       
   ENDIF
   


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )

   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         TSKOLD(i,j)=TSK(i,j)
         USTOLD(i,j)=UST(i,j)
         ZNTOLD(i,j)=ZNT(i,j)





         DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
         ENDDO



         PSFC(I,J)=p8w(I,kms,J)

         DO k=kts,min(kte+1,kde)
            RTHBLTEN(I,K,J)=0.
            RUBLTEN(I,K,J)=0.
            RVBLTEN(I,K,J)=0.
            IF ( PRESENT( RQCBLTEN )) RQCBLTEN(I,K,J)=0.
            IF ( PRESENT( RQVBLTEN )) RQVBLTEN(I,K,J)=0.
         ENDDO

         IF (flag_QI .AND. PRESENT(RQIBLTEN) ) THEN
            DO k=kts,min(kte+1,kde)
               RQIBLTEN(I,K,J)=0.
            ENDDO
         ENDIF
         
         IF (flag_QNI .AND. PRESENT(RQNIBLTEN) ) THEN
            DO k=kts,min(kte+1,kde)
               RQNIBLTEN(I,K,J)=0.
            ENDDO
         ENDIF
      ENDDO
      ENDDO

      IF (bl_pbl_physics .EQ. QNSEPBLSCHEME ) THEN

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
           DO k=kms,kme
              u_phytmp(i,k,j)=0.
              v_phytmp(i,k,j)=0.
           ENDDO
           ENDDO
           ENDDO
      ENDIF

      IF ( idiff.eq.1 ) THEN













         
         IF (flag_bep) THEN    
           do j=j_start(ij),j_end(ij)
           do i=i_start(ij),i_end(ij)            
           do k=kts,kte
             a_u(i,k,j)=a_u_bep(i,k,j)
             a_v(i,k,j)=a_v_bep(i,k,j)
             a_t(i,k,j)=a_t_bep(i,k,j)
             a_q(i,k,j)=a_q_bep(i,k,j)
             b_u(i,k,j)=b_u_bep(i,k,j)
             b_v(i,k,j)=b_v_bep(i,k,j)
             b_t(i,k,j)=b_t_bep(i,k,j)
             b_q(i,k,j)=b_q_bep(i,k,j)
             vl(i,k,j)=vl_bep(i,k,j)
             sf(i,k,j)=sf_bep(i,k,j)
           enddo
           sf(i,kte+1,j)=1.
           enddo
           enddo
         ELSE
           do j=j_start(ij),j_end(ij)
           do i=i_start(ij),i_end(ij)
           do k=kts,kte
             a_u(i,k,j)=0.
             a_v(i,k,j)=0.
             a_t(i,k,j)=0.
             a_q(i,k,j)=0.
             b_u(i,k,j)=0.
             b_v(i,k,j)=0.
             b_t(i,k,j)=0.
             b_q(i,k,j)=0.
             vl(i,k,j)=1.
             sf(i,k,j)=1.
           enddo
           sf(i,kte+1,j)=1.



            a_u(i,1,j)=(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)           
            a_v(i,1,j)=(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)
            b_t(i,1,j)=hfx(i,j)/rho(i,1,j)/CP/dz8w(i,1,j)
            b_q(i,1,j)=qfx(i,j)/rho(i,1,j)/dz8w(i,1,j)







           enddo
           enddo
           
         ENDIF

        endif      
                                           
     
 




 
      
   ENDDO
   !$OMP END PARALLEL DO

  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij, i,j,k, its, ite, jts, jte, z0, z1, z2, w1, w2, message, initflag )
  DO ij = 1 , num_tiles

   its = i_start(ij)
   ite = i_end(ij)
   jts = j_start(ij)
   jte = j_end(ij)

   pbl_select: SELECT CASE(bl_pbl_physics)

      CASE (TEMFPBLSCHEME)

       
       
       
        CALL wrf_debug(100,'in TEMF PBL')
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( qi_curr )                            .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( rqiblten )                           .AND. &
                PRESENT( te_temf ) .AND. PRESENT( cfm_temf ) .AND. &
                PRESENT( hol      ) ) THEN
             CALL temfpbl(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy,RHO=rho               &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,FLAG_QI=flag_qi                                      &
              ,g=g,cp=cp,rcp=rcp,r_d=r_d,r_v=r_v,cpv=cpv                    &
              ,Z=z,XLV=XLV,PSFC=PSFC               &
              ,MUT=mut,P_TOP=p_top                  &
              ,ZNT=znt,HT=ht,UST=ust,ZOL=zol,HOL=hol,HPBL=pblh      &
              ,PSIM=psim,PSIH=psih,XLAND=xland                      &
              ,HFX=hfx,QFX=qfx,TSK=tskold,QSFC=qsfc,GZ1OZ0=gz1oz0   &
              ,U10=u10,V10=v10,T2=t2                                &
              ,WSPD=wspd,BR=br,DT=dtbl,DTMIN=dtmin,KPBL2D=kpbl      &
              ,SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0            &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman,EOMEG=eomeg          &
              ,STBOLT=stbolt                                       &
              ,te_temf=te_temf,kh_temf=kh_temf,km_temf=km_temf      &
              ,shf_temf=shf_temf,qf_temf=qf_temf                    &
              ,uw_temf=uw_temf,vw_temf=vw_temf                      &
              ,hd_temf=hd_temf,lcl_temf=lcl_temf,hct_temf=hct_temf  &
              ,wupd_temf=wupd_temf,mf_temf=mf_temf                  &
              ,thup_temf=thup_temf,qtup_temf=qtup_temf,qlup_temf=qlup_temf  &
              ,cf3d_temf=cf3d_temf,cfm_temf=cfm_temf                &
              ,flhc=flhc,flqc=flqc,exch_temf=exch_temf              &
              ,fCor=f                                            &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               CALL wrf_error_fatal3("<stdin>",1013,&
'Lack arguments to call TEMF pbl')
           ENDIF

      CASE (YSUSCHEME)
        CALL wrf_debug(100,'in YSU PBL')
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( qi_curr )                            .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( rqiblten )                           .AND. &
                PRESENT( hol      ) ) THEN

             CALL ysu(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy                       &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,FLAG_QI=flag_qi                                      &
              ,CP=cp,G=g,ROVCP=rcp,RD=r_D,ROVG=rovg                 &
              ,DZ8W=dz8w,XLV=XLV,RV=r_v,PSFC=PSFC                   &
              ,ZNU=znu,ZNW=znw,MUT=mut,P_TOP=p_top                  &
              ,ZNT=znt,UST=ust,HPBL=pblh                            &
              ,PSIM=fm,PSIH=fhh,XLAND=xland                         &
              ,HFX=hfx,QFX=qfx                                      &
              ,U10=u10,V10=v10                                      &
              ,UOCE=uoce,VOCE=voce                                  &

              ,CTOPO=ctopo,CTOPO2=ctopo2                &

              ,WSPD=wspd,BR=br,DT=dtbl,KPBL2D=kpbl                  &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman                      &
              ,EXCH_H=exch_h,REGIME=regime                          &

              ,WSTAR=wstar,DELTA=delta                        &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               WRITE ( message , FMT = '(A,7(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'qi_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'rqiblten, '//                                     &
                 'hol = ' ,                                         &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( qi_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( rqiblten ) ,                             &
                  PRESENT( hol      )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1071,&
'Lack arguments to call YSU pbl')
           ENDIF

      CASE (MRFSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( hol      )                           .AND. &
                                                        .TRUE.  ) THEN

             CALL wrf_debug(100,'in MRF')
             CALL mrf(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr                                         &
              ,QC3D=qc_curr                                         &
              ,QI3D=qi_curr                                         &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg                  &
              ,DZ8W=dz8w,Z=z,XLV=xlv,RV=r_v,PSFC=psfc               &
              ,P1000MB=p1000mb                                      &
              ,ZNT=znt,UST=ust,ZOL=zol,HOL=hol                      &
              ,PBL=pblh,PSIM=psim,PSIH=psih                         &
              ,XLAND=xland,HFX=hfx,QFX=qfx,TSK=tskold               &
              ,GZ1OZ0=gz1oz0,WSPD=wspd,BR=br                        &
              ,DT=dtbl,DTMIN=dtmin,KPBL2D=kpbl                      &
              ,SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0            &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman,EOMEG=eomeg          &
              ,STBOLT=stbolt,REGIME=regime                          &
              ,FLAG_QI=flag_qi                                      &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    ) 
           ELSE
               WRITE ( message , FMT = '(A,5(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'hol = ' ,                                         &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( hol      )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1121,&
'Lack arguments to call MRF pbl')
           ENDIF

      CASE (GFSSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in GFS')
             CALL bl_gfs(                                           &
               U3D=u_phytmp,V3D=v_phytmp                            &
              ,TH3D=th_phy,T3D=t_phy                                &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten                                    &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg                  &
              ,DZ8W=dz8w,z=z,PSFC=psfc                              &
              ,UST=ust,PBL=pblh,PSIM=psim,PSIH=psih                 &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,WSPD=wspd,BR=br                                      &
              ,DT=dtbl,KPBL2D=kpbl,EP1=ep_1,KARMAN=karman           &
              ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar          &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ',                                     &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1161,&
'Lack arguments to call GFS pbl')
           ENDIF


      CASE (MYJPBLSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN

             CALL wrf_debug(100,'in MYJPBL')
            IF ( .not.flag_bep .and. idiff.ne.1) THEN
             CALL myjpbl(DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w          &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr,QCW=qc_curr,QCI=qi_curr,QCS=qs_curr       & 
              ,QCR=qr_curr,QCG=qg_curr                              & 
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0                              &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE_MYJ=tke_pbl,EXCH_H=exch_h,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh,MIXHT=mixht             &  
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten,RQSBLTEN=rqsblten                  & 
              ,RQRBLTEN=rqrblten,RQGBLTEN=rqgblten                  & 
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
            ELSE 


             CALL myjurb(IDIFF=idiff,FLAG_BEP=flag_bep              &
              ,DT=dtbl,STEPBL=stepbl,HT=ht,DZ=dz8w                  &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0                              &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE_MYJ=tke_pbl,EXCH_H=exch_h,EXCH_M=exch_m          &
              ,USTAR=ust,ZNT=znt                                    &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &

              ,FRC_URB2D=frc_urb2d                                  &
              ,A_U_BEP=a_u_bep,A_V_BEP=a_v_bep,A_T_BEP=a_t_bep      &
              ,A_Q_BEP=a_q_bep                                      &
              ,A_E_BEP=a_e_bep,B_U_BEP=b_u_bep,B_V_BEP=b_v_bep      &
              ,B_T_BEP=b_t_bep,B_Q_BEP=b_q_bep                      &
              ,B_E_BEP=b_e_bep,DLG_BEP=dlg_bep                      &
              ,DL_U_BEP=dl_u_bep,SF_BEP=sf_bep,VL_BEP=vl_bep        &

              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
            ENDIF

           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1238,&
'Lack arguments to call MYJ pbl')
           ENDIF
 
      CASE (QNSEPBLSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
               IF ( MFSHCONV.EQ.1 )THEN
               CALL wrf_debug(100,'in MFSHCONVPBL')
               CALL mfshconvpbl(DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w         &
                    ,RHO=rho,PMID=p_phy,PINT=p8w,TH=th_phy,EXNER=pi_phy   &
                    ,QV=qv_curr, QC=qc_curr                    &
                    ,U=u_phy,V=v_phy                                      &
                    ,HFX=hfx, QFX=qfx,TKE=tke_pbl                         &
                    ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
                    ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
                    ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE      &
                    ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME      &
                    ,ITS=ITS,ITE=ITE,JTS=JTS,JTE=JTE,KTS=KTS,KTE=KTE      &
                    ,KRR=2,MASSFLUX_EDKF=massflux_EDKF                    &
                    ,ENTR_EDKF=entr_EDKF, DETR_EDKF=detr_EDKF             & 
                    ,THL_UP=thl_up, THV_UP=thv_up, RT_UP=rt_up            &
                    ,RV_UP=rv_up,RC_UP=rc_up, U_UP=u_up, V_UP=v_up        &
                    ,FRAC_UP=frac_up, RC_MF=rc_mf,WTHV=u_phytmp           &
                    ,PLM_BL89=v_phytmp   ) 
               ENDIF   

             CALL wrf_debug(100,'in QNSEPBL')
             CALL qnsepbl(                                           &
               DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w                    &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0,CORF=f                       &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE=tke_pbl,EXCH_H=exch_h,EXCH_M=exch_m,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,HFX=hfx,WTHV_MF=u_phytmp,LM_BL89=v_phytmp            & 
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )











            ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1309,&
'Lack arguments to call QNSE pbl')
           ENDIF

      CASE (QNSEPBL09SCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in QNSEPBL09')
             CALL qnsepbl09(                                        &
               DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w                    &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0,CORF=f                       &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE=tke_pbl,EXCH_H=exch_h,EXCH_M=exch_m,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1348,&
'Lack arguments to call old QNSE pbl')
           ENDIF

      CASE (ACMPBLSCHEME)
           
           
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in ACM PBL')

             CALL ACMPBL(                                                        &
               XTIME=itimestep, DTPBL=dtbl, ZNW=znw, SIGMAH=znu               &
              ,U3D=u_phytmp, V3D=v_phytmp, PP3D=p_phy, DZ8W=dz8w, TH3D=th_phy, T3D=t_phy            &
              ,QV3D=qv_curr, QC3D=qc_curr, QI3D=qi_curr, RR3D=rho                &
              ,UST=UST, HFX=HFX, QFX=QFX, TSK=tsk                               &
              ,PSFC=PSFC, EP1=EP_1, G=g, ROVCP=rcp,RD=r_D,CPD=cp                 &
              ,PBLH=pblh, KPBL2D=kpbl, EXCH_H=exch_h, REGIME=regime              &
              ,GZ1OZ0=gz1oz0,WSPD=wspd,PSIM=psim, MUT=mut                        &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten                 &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten             &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde                   &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme                   &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte                   &   
                                                                      )
           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal3("<stdin>",1386,&
'Lack arguments to call ACM2 pbl')
           ENDIF


        CASE (MYNNPBLSCHEME2, MYNNPBLSCHEME3)

           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( qi_curr ) .AND. PRESENT( rqiblten ) .AND.  &
                PRESENT( rqniblten ) .AND. & 
                PRESENT( qni_curr ) .AND. & 
                PRESENT(qke) .AND. PRESENT(tsq) .AND.               &
                PRESENT(qsq) .AND. PRESENT(cov) .AND.               &
                PRESENT(rmol) .AND.                                 &
                PRESENT(qcg) .AND. PRESENT(ch) .AND.                &
                PRESENT(grav_settling) .AND.                        &
                PRESENT(bl_mynn_tkebudget) .AND.                    &

                PRESENT(qke_adv) .AND. PRESENT(bl_mynn_tkeadvect) .AND.&

                PRESENT(vdfg) ) THEN

              CALL wrf_debug(100,'in MYNNPBL')

              IF (itimestep==1) THEN
                 initflag=1
              ELSE
                 initflag=0
              ENDIF
              
              CALL  mynn_bl_driver(&
                   &initflag=initflag,&
                   &grav_settling=grav_settling,&
                   &delt=dtbl,&
                   &dz=dz8w,&
                   &u=u_phy,v=v_phy,th=th_phy,qv=qv_curr,qc=qc_curr,&
                   &qi=qi_curr,qni=qni_curr,& 
                   &p=p_phy,exner=pi_phy,rho=rho,&
                   &xland=xland,ts=tsk,qsfc=qsfc,qcg=qcg,ps=psfc,&
                   &ust=ust,ch=ch,hfx=hfx,qfx=qfx,rmol=rmol,wspd=wspd,&
                   &uoce=uoce,voce=voce,&           
                   &vdfg=vdfg,&                     
                   &Qke=qke,TKE_PBL=tke_pbl,&       
                   &Sh3d=Sh3d,&

                   &qke_adv=qke_adv,bl_mynn_tkeadvect=bl_mynn_tkeadvect,&

                   &Tsq=tsq,Qsq=qsq,Cov=cov,&
                   &Du=rublten,Dv=rvblten,Dth=rthblten,&
                   &Dqv=rqvblten,Dqc=rqcblten,Dqi=rqiblten,&
                   
                   &Dqni=rqniblten,&
                   &k_h=exch_h,k_m=exch_m,&
                   &pblh=pblh,KPBL=KPBL&
                   &,el_pbl=el_pbl                                       &
                   &,dqke=dqke                                           &
                   &,qWT=qWT,qSHEAR=qSHEAR,qBUOY=qBUOY,qDISS=qDISS       &

                   &,WSTAR=wstar,DELTA=delta                             &
                   &,bl_mynn_tkebudget=bl_mynn_tkebudget                 &
                   &,bl_mynn_cloudpdf=bl_mynn_cloudpdf                   &
                   &,FLAG_QI=flag_qi,FLAG_QNI=flag_qni                   &
                   &,FLAG_QC=flag_qc,FLAG_QNC=flag_qnc                   &
                   ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
                   ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
                   ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                   )
           ELSE
               WRITE ( message , FMT = '(A,20(L1,1X))' )            &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'qi_curr, '//                                      &

                 'qni_curr, '//                                     &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'rqiblten, '//                                     &

                 'rqniblten, '//                                    &
                 'qke, '//                                          &
                 'tsq, '//                                          &
                 'qsq, '//                                          &
                 'cov, '//                                          &
                 'rmol, '//                                         &
                 'qcg, '//                                          &
                 'ch, '//                                           &
                 'grav_settling, '//                                &
                 'bl_mynn_tkebudget, '//                            &
                 'qke_adv, '//                                      &
                 'bl_mynn_tkeadvect, '//                            &
                 'vdfg = ' ,                                        &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( qi_curr ) ,                              &

                  PRESENT( qni_curr ) ,                             &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( rqiblten ) ,                             &

                  PRESENT( rqniblten ) ,                            &
                  PRESENT( qke      ) ,                             &
                  PRESENT( tsq      ) ,                             &
                  PRESENT( qsq      ) ,                             &
                  PRESENT( cov      ) ,                             &
                  PRESENT( rmol     ) ,                             &
                  PRESENT( qcg      ) ,                             &
                  PRESENT( ch       ) ,                             &
                  PRESENT( grav_settling),                          &
                  PRESENT( bl_mynn_tkebudget) ,                     &
                  PRESENT( qke_adv  ) ,                             &
                  PRESENT( bl_mynn_tkeadvect) ,                     &
                  PRESENT( vdfg )
               CALL wrf_debug(0,message)
              CALL wrf_error_fatal3("<stdin>",1502,&
'Lack arguments to call MYNN pbl')
           ENDIF

        CASE (BOULACSCHEME)

             CALL wrf_debug(100,'in boulac')

             CALL BOULAC(FRC_URB2D=frc_urb2d,IDIFF=idiff,FLAG_BEP=flag_bep     &
              ,DZ8W=dz8w,DT=dt,U_PHY=u_phytmp                                  &
              ,V_PHY=v_phytmp,TH_PHY=th_phy                                    &
              ,RHO=rho,QV_CURR=qv_curr,QC_CURR=qc_curr,HFX=hfx                 &
              ,QFX=qfx,USTAR=ust,CP=cp,G=g                                     &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten               &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                            &
              ,TKE=tke_pbl,DLK=el_pbl,WU=wu_tur,WV=wv_tur,WT=wt_tur,WQ=wq_tur  &
              ,EXCH_H=exch_h,EXCH_M=exch_m,PBLH=pblh                           &
              ,A_U_BEP=a_u_bep,A_V_BEP=a_v_bep,A_T_BEP=a_t_bep                 &
              ,A_Q_BEP=a_q_bep                                                 &
              ,A_E_BEP=a_e_bep,B_U_BEP=b_u_bep,B_V_BEP=b_v_bep                 &
              ,B_T_BEP=b_t_bep,B_E_BEP=b_e_bep,DLG_BEP=dlg_bep                 &
              ,B_Q_BEP=b_q_bep                                                 &
              ,DL_U_BEP=dl_u_bep,SF_BEP=sf_bep,VL_BEP=vl_bep                   &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde                 &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme                 &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte  )

           CASE (CAMUWPBLSCHEME)
              
              CALL wrf_debug(100,'in camuwpbl')
              IF ( PRESENT( qv_curr  )  .AND. PRESENT( qc_curr   )  .AND. &
                   PRESENT( qi_curr  )  .AND. PRESENT( qnc_curr  )  .AND. &
                   PRESENT( qni_curr )  .AND.                             &
                   PRESENT( rqvblten )  .AND. PRESENT( rqcblten  )  .AND. &
                   PRESENT( rqiblten )  .AND. PRESENT( rqniblten )        &                   
                 )THEN
                 CALL CAMUWPBL(DT=dt,U_PHY=u_phy,V_PHY=v_phy,TH_PHY=th_phy,RHO=rho   &
                      ,QV_CURR=qv_curr,HFX=hfx,QFX=qfx,USTAR=ust,P8W=p8w,P_PHY=p_phy &
                      ,Z=z,T_PHY=t_phy,QC_CURR=qc_curr,QI_CURR=qi_curr,Z_AT_W=z_at_w &
                      ,CLDFRA_OLD_mp=cldfra_old_mp,CLDFRA=cldfra,HT=ht               &
                      ,RTHRATENLW=rthratenlw,EXNER=pi_phy                            &
                      ,is_CAMMGMP_used=is_CAMMGMP_used                               &
                      ,ITIMESTEP=itimestep,QNC_CURR=qnc_curr,QNI_CURR=qni_curr       &
                      ,WSEDL3D=wsedl3d                                               &

                      ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde             &
                      ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme             &
                      ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte             &

                      ,TAURESX2D=tauresx2d,TAURESY2D=tauresy2d                       &
                      ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten             &
                      ,RQIBLTEN=rqiblten,RQNIBLTEN=rqniblten,RQVBLTEN=rqvblten       &
                      ,RQCBLTEN=rqcblten,KVM3D=exch_m,KVH3D=exch_h                   &

                      ,TPERT2D=tpert2d,QPERT2D=qpert2d,WPERT2D=wpert2d,SMAW3D=smaw3d &
                      ,TURBTYPE3D=turbtype3d                                         &
                      ,TKE_pbl=tke_pbl,PBLH2D=pblh,KPBL2D=kpbl                       )
              ELSE
                 WRITE ( message , FMT = '(A,8(L1,1X))' )            &
                      'present: '//                                      &
                      'qv_curr, '//                                      &
                      'qc_curr, '//                                      &
                      'qi_curr, '//                                      &
                      'qnc_curr, '//                                     &
                      'qni_curr, '//                                     &
                      'rqvblten, '//                                     &
                      'rqcblten, '//                                     &
                      'rqiblten, '//                                     &
                      'rqniblten= ',                                     &
                      PRESENT( qv_curr ) ,                              &
                      PRESENT( qc_curr ) ,                              &
                      PRESENT( qi_curr ) ,                              &
                      PRESENT( qnc_curr ) ,                             &
                      PRESENT( qni_curr ) ,                             &
                      PRESENT( rqvblten ) ,                             &
                      PRESENT( rqcblten ) ,                             &
                      PRESENT( rqiblten ) ,                             &
                      PRESENT( rqniblten ) 

                 CALL wrf_debug(0,message)
                 CALL wrf_error_fatal3("<stdin>",1582,&
'Lack arguments to call CAMUWPBL pbl')
              ENDIF
              

           CASE (GBMPBLSCHEME)
               CALL wrf_debug(100,'in gbmpbl') 
               IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND.&
               PRESENT( qi_curr )                            .AND. &
               PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
               PRESENT( rqiblten )                           .AND. &
               PRESENT( hol      )                           .AND. &
                                       .TRUE.  ) THEN
             CALL gbmpbl(                                           &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,KZM_GBM=exch_m,KTH_GBM=exch_h,KETHL_GBM=exch_tke     & 
              ,EL_GBM=el_pbl,DZ8W=dz8w,Z=z,PSFC=PSFC                &
              ,TKE_PBL=tke_pbl,RTHRATEN=rthraten                    & 
              ,ZNT=znt,UST=ust,ZOL=zol,HOL=hol,PBL=pblh             & 
              ,KPBL2D=kpbl,REGIME=regime                            & 
              ,PSIM=psim,PSIH=psih,XLAND=xland                      &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,WSPD=wspd,BR=br,DT=dtbl,DTMIN=dtmin                  &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
              ELSE
               CALL wrf_error_fatal3("<stdin>",1615,&
'Lack arguments to call GBM pbl')
              ENDIF

     CASE DEFAULT

       WRITE( message , * ) 'The pbl option does not exist: bl_pbl_physics = ', bl_pbl_physics
       CALL wrf_error_fatal3("<stdin>",1622,&
message )

   END SELECT pbl_select




    windfarm_select: SELECT CASE(windfarm_opt)

      CASE (fitchscheme)

                  IF (PRESENT(id) .AND.                                  &
                     PRESENT(z_at_w) ) THEN

                     CALL wrf_debug(100,'in phys/module_wind_fitch.F')
                     CALL dragforce(                                  &
                     & ID=id                                             &
                     &,Z_AT_W=z_at_w,u=u_phy,v=v_phy                     &
                     &,DX=dx,DZ=dz8w,DT=dt                               &
                     &,QKE=qke                                           &
                     &,DU=rublten,DV=rvblten                             &
                     &,WINDFARM_OPT=windfarm_opt,POWER=power             &
                     &,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde   &
                     &,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme   &
                     &,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte   &
                     &)
                  ELSE
                     WRITE ( message , FMT = '(A,6(L1,1X))' )           &
                     'present: '//                                      &
                     'ID, '//                                           &
                     'z_at_w, '//                                       &
                     'xlat_u, '//                                       &
                     'xlong_u, '//                                      &
                     'xlat_v, '//                                       &
                     'xlong_v = ' ,                                     &
                      PRESENT( id ) ,                                   &
                      PRESENT( z_at_w ) 
                     CALL wrf_debug(0,message)
                     CALL wrf_error_fatal3("<stdin>",1661,&
'Lack arguments to call turbine_drag')
                  ENDIF

   END SELECT windfarm_select


   
   IF (PRESENT(dtaux3d)) THEN
       IF(gwd_opt .EQ. 1)THEN
             CALL gwdo(                                              &
               U3D=u_phytmp,V3D=v_phytmp,T3D=t_phy      &
              ,QV3D=qv_curr                                         &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy,Z=z                        &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,DTAUX3D=dtaux3d,DTAUY3D=dtauy3d                      &
              ,DUSFCG=dusfcg,DVSFCG=dvsfcg &
              ,VAR2D=var2d,OC12D=oc12d     &
              ,OA2D1=oa1,OA2D2=oa2,OA2D3=oa3,OA2D4=oa4  &
              ,OL2D1=ol1,OL2D2=ol2,OL2D3=ol3,OL2D4=ol4  &
              ,ZNU=znu,ZNW=znw,MUT=mut,P_TOP=p_top                  &
              ,CP=cp,G=g,RD=r_d                           &
              ,RV=r_v,EP1=ep_1,PI=3.141592653                        &
              ,DT=dtbl,DX=dx,KPBL2D=kpbl,ITIMESTEP=itimestep      &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      )
       ENDIF
   ENDIF


   IF (grav_settling .GE. 1) THEN
      IF ( PRESENT(vdfg) .AND. PRESENT(qc_curr)) THEN
         
         

          CALL bl_fogdes(                                        &
               vdfg,qc_curr,dtbl,rho,dz8w,grav_settling,RQCBLTEN,&
               ids,ide, jds,jde, kds,kde,                        &
               ims,ime, jms,jme, kms,kme,                        &
               i_start(ij),i_end(ij),                            &
               j_start(ij),j_end(ij),kts,kte                     )
      ELSE
          CALL wrf_error_fatal3("<stdin>",1704,&
'Missing args for bl_fogdes in pbl driver')
      ENDIF
   ENDIF



     IF (idiff.eq.1) THEN










    
            
          CALL diff3d  (DT=dtbl,CP=cp,DZ=dz8w,TH=th_phy,QV=qv_curr,QC=qc_curr,T=t_phy  &
              ,U=u_phy,V=v_phy,RHO=rho,EXCH_H=exch_h                  &
              ,EXCH_M=exch_m                                          &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten      &
             ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                    &
              ,WU=wu_tur,WV=wv_tur,WT=wt_tur,WQ=wq_tur                    &
              ,A_U=a_u,A_V=a_v,A_T=a_t,A_Q=a_q        &
              ,B_U=b_u,B_V=b_v,B_T=b_t,B_Q=b_q        &
              ,SF=sf,VL=vl            &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde        &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme        &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte)

          DEALLOCATE (a_u)       
          DEALLOCATE (a_v)       
          DEALLOCATE (a_t)       
          DEALLOCATE (a_q)       
          DEALLOCATE (b_u)       
          DEALLOCATE (b_v)       
          DEALLOCATE (b_t)       
          DEALLOCATE (b_q)       
          DEALLOCATE (sf )       
          DEALLOCATE (vl )       
     ENDIF       
      
          IF(scalar_pblmix .GT. 0)THEN
            CALL diff4d  (DT=dtbl,DZ=dz8w, SCALAR=scalar, is_scalar=.true.  &
              ,RHO=rho,EXCH_H=exch_h        &
              ,EXCH_M=exch_m                &
              ,SCALAR_TEND=scalar_tend      &
              ,NUM_SCALAR=num_scalar, PARAM_FIRST_SCALAR=param_first_scalar &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde        &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme        &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte)
          ENDIF
          IF(tracer_pblmix .GT. 0)THEN
            CALL diff4d  (DT=dtbl,DZ=dz8w, SCALAR=tracer, is_scalar=.false.  &
              ,RHO=rho,EXCH_H=exch_h        &
              ,EXCH_M=exch_m                &
              ,SCALAR_TEND=tracer_tend      &
              ,NUM_SCALAR=num_tracer, PARAM_FIRST_SCALAR=param_first_scalar &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde        &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme        &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte)
          ENDIF

      
   ENDDO
   !$OMP END PARALLEL DO

   ENDIF

   END SUBROUTINE pbl_driver


          SUBROUTINE diff3d(DT,CP,DZ,TH ,QV,QC,T,U,V,RHO                              &
              ,EXCH_H,EXCH_M                   &  
              ,RUBLTEN,RVBLTEN,RTHBLTEN    &
              ,RQVBLTEN,RQCBLTEN                  &
              ,WU,WV,WT,WQ                 &
              ,A_U,A_V,A_T,A_Q      &
              ,B_U,B_V,B_T,B_Q      &
              ,SF,VL        &
              ,IDS,IDE,JDS,JDE,KDS,KDE      &
              ,IMS,IME,JMS,JME,KMS,KME      &
              ,ITS,ITE,JTS,JTE,KTS,KTE      &
                                                                    )



















      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE


      real DT,CP
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: TH 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: QV 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: QC 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: T  
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: U 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: V 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: RHO 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_H 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_M 
      
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_U 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_U 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_V 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_V 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_T 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_T 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_Q 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_Q 
    
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: VL 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: SF 

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RUBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RVBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RTHBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RQVBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RQCBLTEN 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WU 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WV 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WT 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WQ 

     REAL ELOCP 


      real u1d(kms:kme),v1d(kms:kme),exch_h1d(kms:kme)
      real the1d(kms:kme) 
      real exch_m1d(kms:kme),qv1d(kms:kme),qc1d(kms:kme)
      real dz1d(kms:kme),rho1d(kms:kme),rhoz1d(kms:kme)
      real sf1d(kms:kme),vl1d(kms:kme)   
      real a_u1d(kms:kme),b_u1d(kms:kme)
      real a_v1d(kms:kme),b_v1d(kms:kme)
      real a_t1d(kms:kme),b_t1d(kms:kme)
      real a_q1d(kms:kme),b_q1d(kms:kme)
      real a_qc1d(kms:kme),b_qc1d(kms:kme)
      real wu1d(kms:kme),wv1d(kms:kme),wt1d(kms:kme),wq1d(kms:kme),wqc1d(kms:kme)
      real thnew

      integer i,k,j  

      ELOCP=2.72E6/CP
      u1d=0.
      v1d=0.
      exch_h1d=0.
      exch_m1d=0.
      qv1d=0.
      qc1d=0.
      dz1d=0.
      rho1d=0.
      rhoz1d=0.
      sf1d=0.
      vl1d=0.
      a_u1d=0.
      a_v1d=0.
      a_t1d=0.
      a_q1d=0.
      a_qc1d=0.
      b_u1d=0.
      b_v1d=0.
      b_t1d=0.
      b_q1d=0.
      b_qc1d=0.
       
      do j=jts,jte
      do i=its,ite



       do k=kts,kte
        u1d(k)=U(i,k,j)
        v1d(k)=V(i,k,j)
        the1d(k)=TH(i,k,j)*(QC(i,k,j)*(-ELOCP/T(i,k,j))+1)
        qv1d(k)=qv(i,k,j)
        dz1d(k)=dz(i,k,j)
        rho1d(k)=rho(i,k,j) 
        a_u1d(k)=a_u(i,k,j)
        b_u1d(k)=b_u(i,k,j)
        a_v1d(k)=a_v(i,k,j)
        b_v1d(k)=b_v(i,k,j)
        a_t1d(k)=a_t(i,k,j)
        b_t1d(k)=b_t(i,k,j)
        a_q1d(k)=a_q(i,k,j)
        b_q1d(k)=b_q(i,k,j)
        a_qc1d(k)=0.
        b_qc1d(k)=0.
        vl1d(k)=vl(i,k,j)
        sf1d(k)=sf(i,k,j)
       enddo
       sf1d(kte+1)=1. 
       do k=kts,kte    
        exch_h1d(k)=exch_h(i,k,j)
        exch_m1d(k)=exch_m(i,k,j)
       enddo
       exch_h1d(kts)=0.

       exch_m1d(kts)=0.

        rhoz1d(kts)=rho1d(kts)
        do k=kts+1,kte
         rhoz1d(k)=(rho1d(k)*dz1d(k-1)+rho1d(k-1)*dz1d(k))/       &
     &                      (dz1d(k-1)+dz1d(k))
        enddo
        rhoz1d(kte+1)=rho1d(kte)



          
       call diff(kms,kme,kts,kte,dt,u1d,rho1d,rhoz1d,exch_m1d,a_u1d,b_u1d,sf1d, &
     &            vl1d,dz1d,wu1d) 



       call diff(kms,kme,kts,kte,dt,v1d,rho1d,rhoz1d,exch_m1d,a_v1d,b_v1d,sf1d, &
     &            vl1d,dz1d,wv1d) 



       call diff(kms,kme,kts,kte,dt,the1d,rho1d,rhoz1d,exch_h1d,a_t1d,b_t1d,sf1d, &
     &            vl1d,dz1d,wt1d) 



       call diff(kms,kme,kts,kte,dt,qv1d,rho1d,rhoz1d,exch_h1d,a_q1d,b_q1d,sf1d, &
     &            vl1d,dz1d,wq1d) 



       call diff(kms,kme,kts,kte,dt,qc1d,rho1d,rhoz1d,exch_h1d,a_qc1d,b_qc1d,sf1d, &
     &            vl1d,dz1d,wqc1d)        




        do k=kts,kte
          rublten(i,k,j)=(u1d(k)-u(i,k,j))/dt
          rvblten(i,k,j)=(v1d(k)-v(i,k,j))/dt
          thnew=the1d(k)/(QC(i,k,j)*(-ELOCP/T(i,k,j))+1)
          rthblten(i,k,j)=(thnew-th(i,k,j))/dt
          rqvblten(i,k,j)=(qv1d(k)-qv(i,k,j))/dt
          rqcblten(i,k,j)=(qc1d(k)-qc(i,k,j))/dt
          wu(i,k,j)=wu1d(k)
          wv(i,k,j)=wv1d(k)
          wt(i,k,j)=wt1d(k)
          wq(i,k,j)=wq1d(k)
        enddo
      enddo
      enddo 


        
      END SUBROUTINE diff3d

          SUBROUTINE diff4d(DT,DZ,SCALAR,IS_SCALAR,RHO              &
              ,EXCH_H,EXCH_M  &  
              ,SCALAR_TEND    &
              ,NUM_SCALAR, PARAM_FIRST_SCALAR     &
              ,IDS,IDE,JDS,JDE,KDS,KDE      &
              ,IMS,IME,JMS,JME,KMS,KME      &
              ,ITS,ITE,JTS,JTE,KTS,KTE      &
                                                                    )


















      USE module_state_description, ONLY: &
                  P_QNS, P_QNR, P_QNG, P_QT, P_QNH, P_QVOLG, P_QKE_ADV


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: NUM_SCALAR, PARAM_FIRST_SCALAR
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE


      REAL, INTENT(IN)   :: DT
      LOGICAL,INTENT(IN) :: IS_SCALAR
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,NUM_SCALAR),INTENT(IN) :: SCALAR 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: RHO 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_H 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_M 
      

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,NUM_SCALAR),INTENT(INOUT) :: SCALAR_TEND 


      real s1d(kms:kme),exch_h1d(kms:kme)
      real exch_m1d(kms:kme)
      real dz1d(kms:kme),rho1d(kms:kme),rhoz1d(kms:kme)
      real sf1d(kms:kme),vl1d(kms:kme)   
      real a_s1d(kms:kme),b_s1d(kms:kme)
      real ws1d(kms:kme)

      integer i,k,j,im  

      s1d=0.
      exch_h1d=0.
      exch_m1d=0.
      rho1d=0.
      rhoz1d=0.
      sf1d=0.
      vl1d=0.
      a_s1d=0.
      b_s1d=0.
       
      DO im=PARAM_FIRST_SCALAR,NUM_SCALAR

        IF((IS_SCALAR .AND. im.NE.P_QNS .AND. im.NE.P_QNR .AND. im.NE.P_QNG &
          .AND. im.NE.P_QNH .AND. im.NE.P_QT .AND. im.NE.P_QVOLG .AND. im.NE.P_QKE_ADV) &
          .OR. (.not. IS_SCALAR)) THEN
        do j=jts,jte
        do i=its,ite



       do k=kts,kte
        s1d(k)=SCALAR(i,k,j,im)
        dz1d(k)=dz(i,k,j)
        rho1d(k)=rho(i,k,j) 


        vl1d(k)=1.
        sf1d(k)=1.
       enddo
       sf1d(kte+1)=1. 
       do k=kts,kte    
        exch_h1d(k)=exch_h(i,k,j)
        exch_m1d(k)=exch_m(i,k,j)
       enddo
       exch_h1d(kts)=0.

       exch_m1d(kts)=0.

        rhoz1d(kts)=rho1d(kts)
        do k=kts+1,kte
         rhoz1d(k)=(rho1d(k)*dz1d(k-1)+rho1d(k-1)*dz1d(k))/       &
     &                      (dz1d(k-1)+dz1d(k))
        enddo
        rhoz1d(kte+1)=rho1d(kte)



          
       call diff(kms,kme,kts,kte,dt,s1d,rho1d,rhoz1d,exch_h1d,a_s1d,b_s1d,sf1d, &
     &            vl1d,dz1d,ws1d) 




        do k=kts,kte
          scalar_tend(i,k,j,im)=(s1d(k)-scalar(i,k,j,im))/dt

        enddo
        enddo
        enddo 
      ELSE

      ENDIF
      
      ENDDO 


        
      END SUBROUTINE diff4d


       subroutine diff(kms,kme,kts,kte,dt,co,da,daz,cd,aa,bb,sf,vl,dz,fc)




















        implicit none
        integer iz,iz1,izf
        integer kms,kme,kts,kte
        real dt,dzv
        real co(kms:kme),cd(kms:kme),dz(kms:kme)
        real da(kms:kme),daz(kms:kme)
        real cddz(kms:kme),fc(kms:kme),df(kms:kme)
        real a(kms:kme,3),c(kms:kme)
        real sf(kms:kme),vl(kms:kme)
        real aa(kms:kme),bb(kms:kme)

        


        cddz(kts)=sf(kts)*daz(kts)*cd(kts)/dz(kts)
        do iz=kts+1,kte
         cddz(iz)=2.*sf(iz)*daz(iz)*cd(iz)/(dz(iz)+dz(iz-1))
        enddo
        cddz(kte+1)=sf(kte+1)*daz(kte+1)*cd(kte+1)/dz(kte)

          iz1=1
          izf=1

          do iz=iz1,kte-1

           dzv=vl(iz)*dz(iz)
           a(iz,1)=-cddz(iz)*dt/dzv/da(iz)
           a(iz,2)=1+dt*(cddz(iz)+cddz(iz+1))/dzv/da(iz)-aa(iz)*dt
           a(iz,3)=-cddz(iz+1)*dt/dzv/da(iz)
           c(iz)=co(iz)+bb(iz)*dt
          enddo

          do iz=kte-(izf-1),kte
           a(iz,1)=0.
           a(iz,2)=1
           a(iz,3)=0.
           c(iz)=co(iz)
          enddo
          call invert (kms,kme,kts,kte,a,c,co)
           


          do iz=kts,iz1
           fc(iz)=0.
          enddo

          do iz=iz1+1,kte
           fc(iz)=-(cddz(iz)*(co(iz)-co(iz-1)))/da(iz)
          enddo



       return
       end subroutine diff


       subroutine invert(kms,kme,kts,kte,a,c,x)













       implicit none
       integer kms,kme,kts,kte,in
       real a(kms:kme,3),c(kms:kme),x(kms:kme)

        do in=kte-1,kts,-1
         c(in)=c(in)-a(in,3)*c(in+1)/a(in+1,2)
         a(in,2)=a(in,2)-a(in,3)*a(in+1,1)/a(in+1,2)
        enddo

        do in=kts+1,kte
         c(in)=c(in)-a(in,1)*c(in-1)/a(in-1,2)
        enddo

        do in=kts,kte
         x(in)=c(in)/a(in,2)
        enddo

        return
        end subroutine invert















END MODULE module_pbl_driver
