


MODULE module_cumulus_driver
CONTAINS
   SUBROUTINE cumulus_driver(grid                                     &
                 
                     ,ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 
                 
                     ,u,v,th,t,w                                      &
                     ,p,pi,rho                                        &
                 
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,cudtacttime                                     & 
                     ,rainc,raincv,pratec,nca                         &
                     ,cldfra_dp,cldfra_sh                             & 
                     ,QC_CU,QI_CU                                     &
                     ,z,z_at_w,dz8w,mavail,pblh,p8w,psfc,tsk          &
                     ,tke_pbl, ust                                    &
                     ,forcet,forceq,w0avg,stepcu,gsw                  &
                     ,cldefi,lowlyr,xland,cu_act_flag,warm_rain       &
                     ,hfx,qfx,cldfra,cldfra_mp_all,tpert2d            &
                     ,htop,hbot,kpbl,ht                               &
                     ,ensdim,maxiens,maxens,maxens2,maxens3           &
                     ,periodic_x,periodic_y                           &
                     ,is_CAMMGMP_used                                 & 
                     ,evapcdp3d,icwmrdp3d,rprddp3d                    & 
                 
                     ,cu_physics, bl_pbl_physics, sf_sfclay_physics   &
                 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qv_prev, qc_prev, qr_prev                       & 
                     ,qi_prev, qs_prev, qg_prev                       &
                 
                     ,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma     &
                     ,apr_capme,apr_capmi,edt_out,clos_choice         &
                     ,mass_flux,xf_ens,pr_ens,cugd_avedx,imomentum    &
                     ,ishallow,cugd_tten,cugd_qvten,cugd_qcten        &
                     ,cugd_ttens,cugd_qvtens                          &
                     ,gd_cloud,gd_cloud2                              &
                 
                     ,cape, zmmu, zmmd, zmdt, zmdq, dlf, rliq         &
                     ,pconvb, pconvt                                  &
                     ,evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc    &
                     ,zmflxsnw, zmntprpd, zmntsnpd, zmeiheat          &
                     ,cmfmc, cmfmcdzm, preccdzm, precz                &
                     ,zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd    &
                     ,zmicuu, zmicud, zmicvu, zmicvd, zmdice, zmdliq  &
                     ,dp3d, du3d, ed3d, eu3d, md3d, mu3d, dsubcld2d   &
                     ,ideep2d, jt2d, maxg2d, lengath2d                &
                     ,k22_shallow,kbcon_shallow,ktop_shallow,xmb_shallow &
                     ,ktop_deep &
                 
                     ,pgcon,sas_mass_flux                             &
                     ,shalconv,shal_pgcon                             &
                     ,HPBL2D,EVAP2D,HEAT2D                            &     
                 
                     ,mp_physics                                      &
                 
                     ,rqvcuten,rqccuten,rqrcuten                      &
                     ,rqicuten,rqscuten,rqgcuten                      &
                     ,rqcncuten,rqincuten                             &
                     ,rqvblten,rqvften                                &
                     ,rucuten,rvcuten                                 &
                     ,rthcuten,rthraten,rthblten,rthften              &
                     ,mommix,store_rand                               &

                     ,znu                                             &
                 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,f_flux            &
                 
                     ,kfeta_trigger                                   &
                     ,nsas_dx_factor                                  &




                                                                      )

   USE module_model_constants
   USE module_state_description, ONLY:     KFSCHEME,BMJSCHEME         &
                                          ,KFETASCHEME,GDSCHEME       &
                                          ,G3SCHEME,GFSCHEME          &
                                          ,P_QC,P_QI,Param_FIRST_SCALAR &
                                          ,CAMZMSCHEME, SASSCHEME     &
                                          ,OSASSCHEME,MESO_SAS        &  
                                          ,NSASSCHEME                 &

                                          , CAMMGMPSCHEME             &

                                          ,TIEDTKESCHEME              



   USE module_cu_kf     ,  ONLY : kfcps
   USE module_cu_bmj    ,  ONLY : bmjdrv

   USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks 

   USE module_comm_dm   ,  ONLY : halo_cup_g3_in_sub, halo_cup_g3_out_sub


   USE module_domain    , ONLY: domain
   USE module_cu_kfeta  , ONLY : kf_eta_cps
   USE module_cu_gd     , ONLY : grelldrv
   USE module_cu_gf     , ONLY : gfdrv
   USE module_cu_g3     , ONLY : g3drv,conv_grell_spread3d
   USE module_cu_sas    , ONLY : cu_sas
   USE module_cu_osas   , ONLY : cu_osas
   USE module_cu_mesosas, ONLY : cu_meso_sas
   USE module_cu_camzm_driver, ONLY : camzm_driver
   USE module_cu_tiedtke, ONLY : cu_tiedtke
   USE module_cu_nsas   , ONLY : cu_nsas
   USE module_wrf_error , ONLY : wrf_err_message

   
   
   
   
   
   
   
   
   
   
   

   IMPLICIT NONE




























































































































































   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles
   LOGICAL periodic_x, periodic_y
   TYPE(domain) , INTENT(INOUT)          :: grid
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  ensdim,maxiens,maxens,maxens2,maxens3

   INTEGER, OPTIONAL,     INTENT(IN   )    ::                    &
                   cugd_avedx,clos_choice,bl_pbl_physics,sf_sfclay_physics

   INTEGER,      INTENT(IN   )    ::                 cu_physics  
   INTEGER,      INTENT(IN   )    ::   STEPCU
   LOGICAL,      INTENT(IN   )    ::   warm_rain
   LOGICAL,      INTENT(IN   )    ::   is_CAMMGMP_used 

   REAL, INTENT(IN), OPTIONAL :: pgcon,shal_pgcon,sas_mass_flux
   INTEGER, INTENT(IN), OPTIONAL :: shalconv

   INTEGER,DIMENSION( ims:ime, jms:jme ),                        &
           INTENT(IN ) ::                                LOWLYR

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                              z  &
                                                      ,    dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      ,     rho  &
                                                      ,       w  
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT ), OPTIONAL ::  evapcdp3d, icwmrdp3d, rprddp3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ),OPTIONAL ::                         z_at_w  &
                                                      ,  cldfra  &
                                               ,  cldfra_mp_all  &
                                                      , tke_pbl  

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                                       &
                                                          W0AVG 

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                            cldfra_dp  &
                                                    , cldfra_sh

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
          GSW,HT,XLAND

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC  &
                                                    ,    RAINCV  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT  &
                                                    ,    CLDEFI
 
   REAL, DIMENSION( kms:kme ), OPTIONAL, INTENT(IN   ) ::        &
                                                            znu

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATEC,MAVAIL,PBLH,PSFC,TSK,TPERT2D,UST,HFX,QFX
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratec
                                                    
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   INTEGER, INTENT(IN   ), OPTIONAL        ::   kfeta_trigger
   INTEGER, INTENT(IN   ), OPTIONAL        ::   nsas_dx_factor

   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                   ips,ipe, jps,jpe, kps,kpe,imomentum,ishallow
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL,  INTENT(INOUT ),OPTIONAL :: cudtacttime                 
   REAL   :: cudt_pass, curr_secs_pass,cudtacttime_pass          
   LOGICAL :: adapt_step_flag_pass

   INTEGER,      INTENT(IN   ), OPTIONAL    ::   mp_physics
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN) ::  STORE_RAND

   REAL, OPTIONAL,  INTENT(INOUT) :: mommix

   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,               &    
                    INTENT(INOUT) ::  HPBL2D, EVAP2D, HEAT2D

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                   INTENT(INOUT) :: rucuten,rvcuten



   INTEGER, DIMENSION( ims:ime, jms:jme ),                       &
         OPTIONAL, INTENT(INOUT) ::                              &
     k22_shallow,kbcon_shallow,ktop_shallow,ideep2d,jt2d,maxg2d, &
     lengath2d 
   INTEGER, DIMENSION( ims:ime, jms:jme ),                       &
         OPTIONAL, INTENT(  OUT) :: ktop_deep
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      
                      
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ,qv_prev, qc_prev, qr_prev                  & 
                     ,qi_prev, qs_prev, qg_prev                  &
                      
                     ,rqvcuten,rqccuten,rqrcuten                 &
                     ,rqicuten,rqscuten,rqgcuten                 &
                     ,rqcncuten,rqincuten                        &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,cugd_tten,cugd_qvten,cugd_qcten            &
                     ,cugd_ttens,cugd_qvtens                     &
                     ,forcet, forceq                             &
                     ,rthften,rthcuten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL,                                    &
                    INTENT(INOUT) ::                             &
                apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma      &
               ,apr_capme,apr_capmi,edt_out,xmb_shallow          &
                                                    , MASS_FLUX  &
               ,cape, pconvb, pconvt, preccdzm, precz, rliq      &
               ,dsubcld2d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  GD_CLOUD,GD_CLOUD2,                            &
                  zmmd, zmmu, zmdt, zmdq, dlf,                   &
                  evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc,  &
                  zmflxsnw, zmntprpd, zmntsnpd, zmeiheat,        &
                  cmfmc, cmfmcdzm,                               &
                  zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd,  &
                  zmicuu, zmicud, zmicvu, zmicvd, zmdice, zmdliq,&
                  dp3d, du3d, ed3d, eu3d, md3d, mu3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                  INTENT(INOUT) ::                               &
                  QC_CU,QI_CU
                  
   REAL, DIMENSION( ims:ime , jms:jme , 1:ensdim ),              &
          OPTIONAL,                                              &
          INTENT(INOUT) ::                       XF_ENS, PR_ENS
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   CFU1,        &
                                                   CFD1,        &
                                                   DFU1,        &
                                                   EFU1,        &
                                                   DFD1,        &
                                                   EFD1








   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg
   LOGICAL, INTENT(IN), OPTIONAL ::                   f_flux




   INTEGER :: i,j,k,its,ite,jts,jte,ij,trigger_kf,dx_factor_nsas
   logical :: l_flux
   LOGICAL :: decided , run_param , doing_adapt_dt




   l_flux=.FALSE.
   if (present(f_flux)) l_flux=f_flux
   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
      cudtacttime_pass = -1
   else
      cudt_pass = cudt
      cudtacttime_pass = cudtacttime
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   

   if ( PRESENT ( pratec ) ) then
      tmppratec(:,:) = pratec(:,:)
   else
      tmppratec(:,:) = 0.
   end if

   if (.not. PRESENT(kfeta_trigger)) then
      trigger_kf = 1
   else
      trigger_kf = kfeta_trigger
   endif

   if (.not. PRESENT(nsas_dx_factor)) then
      dx_factor_nsas = 0
   else
      dx_factor_nsas = nsas_dx_factor
   endif

   IF (cu_physics .eq. 0) return



   IF ( adapt_step_flag_pass ) THEN
      doing_adapt_dt = .TRUE.
      IF ( cudtacttime_pass .EQ. 0. ) THEN
         cudtacttime_pass = curr_secs_pass + cudt_pass*60.
      END IF
   ELSE
      doing_adapt_dt = .FALSE.
   END IF



















   decided = .FALSE.
   run_param = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( ( cudt_pass .EQ. 0. ) .OR. ( stepcu .EQ. 1 ) ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepcu) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs_pass .GE. cudtacttime_pass ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      cudtacttime_pass = curr_secs_pass + cudt_pass*60
   END IF

   IF ( run_param ) THEN

   ELSE

      RETURN
   END IF

      if(cu_physics .eq. 5 ) then
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij,i,j,k,its,ite,jts,jte )

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)
        do j=jts,min(jte,jde-1)
        do k=kts,kte
        do i=its,min(ite,ide-1)
           RTHFTEN(i,k,j)=(RTHFTEN(i,k,j)+RTHRATEN(i,k,j) &
                               +RTHBLTEN(i,k,j))*pi(i,k,j)
           RQVFTEN(i,k,j)=RQVFTEN(i,k,j)+RQVBLTEN(i,k,j)
       enddo
       enddo
       enddo
      ENDDO
      !$OMP END PARALLEL DO
      endif

      IF ( cu_physics == G3SCHEME .OR.  cu_physics == GFSCHEME .OR. cu_physics == KFETASCHEME ) THEN






CALL HALO_CUP_G3_IN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      ENDIF







      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)


   cps_select: SELECT CASE(cu_physics)

     CASE (KFSCHEME)
          CALL wrf_debug(100,'in kfcps')

          CALL KFCPS(                                           &
              
                DT=dt ,KTAU=itimestep ,DX=dx , CUDT=cudt_pass   &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RHO=rho                                         &
               ,U=u ,V=v ,TH=th ,T=t ,W=w                       &
               ,PCPS=p ,PI=pi                                   &
               ,XLV0=xlv0 ,XLV1=xlv1 ,XLS0=xls0 ,XLS1=xls1      &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,W0AVG=w0avg                                     &
               ,CP=cp ,R=r_d ,G=g ,EP1=ep_1 ,EP2=ep_2           &
               ,SVP1=svp1 ,SVP2=svp2 ,SVP3=svp3 ,SVPT0=svpt0    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,WARM_RAIN=warm_rain                             &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
               ,RQCCUTEN=rqccuten ,RQRCUTEN=rqrcuten            &
               ,RQICUTEN=rqicuten ,RQSCUTEN=rqscuten            &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE (BMJSCHEME)
          CALL wrf_debug(100,'in bmj_cps')
          CALL BMJDRV(                                          &
                TH=th,T=T ,RAINCV=raincv, PRATEC=tmppratec      &
               ,RHO=rho                                         &
               ,DT=dt ,ITIMESTEP=itimestep ,STEPCU=stepcu       &
               ,CUTOP=htop, CUBOT=hbot, KPBL=kpbl               &
               ,DZ8W=dz8w ,PINT=p8w, PMID=p, PI=pi              &
               ,CP=cp ,R=r_d ,ELWV=xlv ,ELIV=xls ,G=g           &
               ,TFRZ=svpt0 ,D608=ep_1 ,CLDEFI=cldefi            &
               ,LOWLYR=lowlyr ,XLAND=xland                      &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
                                                                )

     CASE (KFETASCHEME)
          CALL wrf_debug(100,'in kf_eta_cps')
          CALL KF_ETA_CPS(                                      &
                U=u ,V=v ,TH=th ,T=t ,W=w ,RHO=rho              &
               ,CUDT=cudt_pass                                  &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,PCPS=p, PI=pi ,W0AVG=W0AVG                      &
               ,CUTOP=HTOP,CUBOT=HBOT                           &
               ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1      &
               ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2           &
               ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0    &
               ,DT=dt ,KTAU=itimestep ,DX=dx                    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag ,WARM_RAIN=warm_rain    &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,trigger=trigger_kf                              &
              
               ,RTHCUTEN=rthcuten                               &
               ,RQVCUTEN=rqvcuten ,RQCCUTEN=rqccuten            &
               ,RQRCUTEN=rqrcuten ,RQICUTEN=rqicuten            &
               ,RQSCUTEN=rqscuten, RQVFTEN=RQVFTEN              &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,CLDFRA_DP_KF=cldfra_dp                          & 
               ,CLDFRA_SH_KF=cldfra_sh                          &
               ,QC_KF=QC_CU,QI_KF=QI_CU                         &
                                                                )

     CASE (GDSCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL GRELLDRV(                                        &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi ,Q=qv_curr ,RAINCV=raincv             &
               ,DZ8W=dz8w,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v     &
               ,PRATEC=tmppratec                                &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as                     &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw                             &
               ,GDC=gd_cloud,GDC2=gd_cloud2 &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3                 &
               ,htop=htop,hbot=hbot                             &
               ,ktop_deep=ktop_deep                             &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              




               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &

               ,RTHRATEN=RTHRATEN,RTHBLTEN=RTHBLTEN             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,RQVBLTEN=RQVBLTEN                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,CFU1=CFU1,CFD1=CFD1,DFU1=DFU1,EFU1=EFU1         &
               ,DFD1=DFD1,EFD1=EFD1,f_flux=l_flux               )
          CALL wrf_debug(200,'back from grelldrv')

     CASE (SASSCHEME)
                                                                                                                                           
          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal3("<stdin>",781,&
wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_sas')
          CALL CU_SAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
               ,MOMMIX=MOMMIX  &
               ,pgcon=pgcon,sas_mass_flux=sas_mass_flux         &
               ,shalconv=shalconv,shal_pgcon=shal_pgcon         &
               ,hpbl2d=hpbl2d,evap2d=evap2d,heat2d=heat2d       &
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (MESO_SAS)                  

          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The meso SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal3("<stdin>",810,&
wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_mesosas')
          CALL CU_MESO_SAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
               ,MOMMIX=MOMMIX  &
               ,pgcon=pgcon,sas_mass_flux=sas_mass_flux         &
               ,shalconv=shalconv,shal_pgcon=shal_pgcon         &
               ,hpbl2d=hpbl2d,evap2d=evap2d,heat2d=heat2d       &
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (OSASSCHEME)
                                                                                                                                           
          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal3("<stdin>",839,&
wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_osas')
          CALL CU_OSAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
               ,store_rand=store_rand  &
               ,MOMMIX=MOMMIX  &
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (G3SCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL G3DRV(                                           &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi,Q=qv_curr,RAINCV=raincv               &
               ,DZ8W=dz8w ,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v    &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as,PRATEC=tmppratec    &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw,edt_out=edt_out             &
               ,GDC=gd_cloud,GDC2=gd_cloud2,kpbl=kpbl           &
               ,k22_shallow=k22_shallow                         &
               ,kbcon_shallow=kbcon_shallow                     &
               ,ktop_shallow=ktop_shallow                       &
               ,xmb_shallow=xmb_shallow                         &
               ,ktop_deep=ktop_deep                             &
               ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten       &
               ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens   &
               ,cugd_qcten=cugd_qcten,cugd_avedx=cugd_avedx     &
               ,imomentum=imomentum,ishallow_g3=ishallow        &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3,ichoice=clos_choice &
               ,htop=htop,hbot=hbot                             &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,IPS=ips,IPE=ipe,JPS=jps,JPE=jpe,KPS=kps,KPE=kpe &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
               ,rqvblten=rqvblten,rthblten=rthblten             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )
     CASE (GFSCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL GFDRV(                                           &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi,Q=qv_curr,RAINCV=raincv               &
               ,DZ8W=dz8w ,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v    &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as,PRATEC=tmppratec    &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,HT=ht,qfx=qfx,hfx=hfx                           &
               ,xland=xland,gsw=gsw,edt_out=edt_out             &
               ,GDC=gd_cloud,GDC2=gd_cloud2,kpbl=kpbl           &
               ,k22_shallow=k22_shallow                         &
               ,kbcon_shallow=kbcon_shallow                     &
               ,ktop_shallow=ktop_shallow                       &
               ,ktop_deep=ktop_deep                             &
               ,xmb_shallow=xmb_shallow                         &
               ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten       &
               ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens   &
               ,cugd_qcten=cugd_qcten,cugd_avedx=cugd_avedx     &
               ,imomentum=imomentum,ishallow_g3=ishallow        &
               ,ichoice=clos_choice                             &
               ,htop=htop,hbot=hbot                             &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &

               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RTHRATEN=RTHRATEN                               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
               ,rqvblten=rqvblten,rthblten=rthblten             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )
     CASE (CAMZMSCHEME)
          IF (PRESENT(z_at_w) .AND. PRESENT(mavail)                 &
                 .AND. PRESENT(pblh) .AND. PRESENT(psfc).AND.PRESENT(RQCNCUTEN))THEN
          CALL wrf_debug(100,'in camzm_cps')
      IF(.not.f_qi)THEN
         WRITE( wrf_err_message , * ) 'This cumulus option requires ice microphysics option: f_qi = ', f_qi
         CALL wrf_error_fatal3("<stdin>",948,&
wrf_err_message )
      ENDIF
          CALL CAMZM_DRIVER(                                        &
                IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde     &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme     &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte     &
               ,ITIMESTEP=itimestep, BL_PBL_PHYSICS=bl_pbl_physics  &
               ,SF_SFCLAY_PHYSICS=sf_sfclay_physics                 &
               ,TH=th, T_PHY=t, TSK=tsk, TKE_PBL=tke_pbl            &
               ,UST=ust, QV=qv_curr, QC=qc_curr, QI=qi_curr         &
               ,MAVAIL=mavail,KPBL=kpbl, PBLH=pblh, XLAND=xland     & 
               ,Z=z, Z_AT_W=z_at_w                                  &
               ,DZ8W=dz8w, HT=ht                                    &
               ,P=p, P8W=p8w, PI_PHY=pi, PSFC=psfc                  &
               ,U_PHY=u, V_PHY=v, HFX=hfx, QFX=qfx, CLDFRA=cldfra   &
               ,CLDFRA_MP_ALL=cldfra_mp_all                         &
               ,IS_CAMMGMP_USED=is_CAMMGMP_used                     &
               ,TPERT_CAMUWPBL=tpert2d                              &
               ,DX=dx, DT=dt, STEPCU=stepcu, CUDT=cudt              &
               ,CURR_SECS=curr_secs                                 &
               ,ADAPT_STEP_FLAG=adapt_step_flag                     &
               ,CUDTACTTIME=cudtacttime_pass                        & 
               ,CAPE_OUT=cape                                       &
               ,MU_OUT=zmmu, MD_OUT=zmmd                            &
               ,ZMDT=zmdt, ZMDQ=zmdq, DLF_OUT=dlf, RLIQ_OUT=rliq    &
               ,PCONVT=pconvt, PCONVB=pconvb, CUBOT=hbot, CUTOP=htop&
               ,RAINCV=raincv, PRATEC=tmppratec                     &
               ,RUCUTEN=rucuten, RVCUTEN=rvcuten                    &
               ,RTHCUTEN=rthcuten, RQVCUTEN=rqvcuten                &
               ,RQCCUTEN=rqccuten, RQICUTEN=rqicuten                &
               ,RQCNCUTEN=rqcncuten, RQINCUTEN=rqincuten            &
               ,EVAPTZM=evaptzm, FZSNTZM=fzsntzm, EVSNTZM=evsntzm   &
               ,EVAPQZM=evapqzm, ZMFLXPRC=zmflxprc                  &
               ,ZMFLXSNW=zmflxsnw, ZMNTPRPD=zmntprpd                &
               ,ZMNTSNPD=zmntsnpd, ZMEIHEAT=zmeiheat                &
               ,CMFMC=cmfmc, CMFMCDZM=cmfmcdzm                      &
               ,PRECCDZM=preccdzm, PRECZ=precz                      &
               ,ZMMTU=zmmtu, ZMMTV=zmmtv, ZMUPGU=zmupgu             &
               ,ZMUPGD=zmupgd, ZMVPGU=zmvpgu, ZMVPGD=zmvpgd         &
               ,ZMICUU=zmicuu, ZMICUD=zmicud, ZMICVU=zmicvu         &
               ,ZMICVD=zmicvd, ZMDICE=zmdice, ZMDLIQ=zmdliq         &
               ,EVAPCDP3D=evapcdp3d, ICWMRDP3D=icwmrdp3d            &
               ,RPRDDP3D=rprddp3d,DP3D=dp3d, DU3D=du3d, ED3D=ed3d   &
               ,EU3D=eu3d, MD3D=md3d, MU3D=mu3d,DSUBCLD2D=dsubcld2d &
               ,IDEEP2D=ideep2d, JT2D=jt2d, MAXG2D=maxg2d           &
               ,LENGATH2D=lengath2d                                 )
        ELSE
             WRITE( wrf_err_message , * ) 'Insufficient arguments to call CAMZM cu scheme'
             CALL wrf_error_fatal3("<stdin>",997,&
wrf_err_message )
          ENDIF


     CASE (TIEDTKESCHEME)

        IF ( PRESENT ( QFX ) .AND. PRESENT( ZNU ) ) THEN

          CALL wrf_debug(100,'in cu_tiedtke')
          CALL CU_TIEDTKE(                                      &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU,HFX=hfx &
               ,RAINCV=RAINCV,PRATEC=tmppratec,QFX=qfx,ZNU=znu  &
               ,U3D=u,V3D=v,W=w,T3D=t,PI3D=pi,RHO3D=rho         &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,QVPBLTEN=RQVBLTEN                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              
               ,QVFTEN=RQVFTEN                                  &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN = RUCUTEN,RVCUTEN = RVCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                             )
        ELSE
          CALL wrf_error_fatal3("<stdin>",1027,&
'Lacking arguments for CU_TIEDTKE in cumulus driver')
        ENDIF


     CASE (NSASSCHEME)
        IF ( PRESENT ( QFX ) .AND. PRESENT( HFX ) ) THEN
          CALL wrf_debug(100,'in nsas_cps')
          CALL CU_NSAS(                                         &
                DT=dt,DX=dx,P3DI=p8w,P3D=p,PI3D=pi,             &
                QC3D=QC_CURR,QI3D=QI_CURR,RHO3D=rho,            &
                ITIMESTEP=itimestep,STEPCU=STEPCU,              &
                HBOT=HBOT,HTOP=HTOP,                            &
                CU_ACT_FLAG=CU_ACT_FLAG,                        &
                RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN,            &
                RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN,            &
                RUCUTEN=RUCUTEN,RVCUTEN=RVCUTEN,                &
                QV3D=QV_CURR,T3D=t,                             &
                RAINCV=RAINCV,PRATEC=tmpPRATEC,                 &
                XLAND=XLAND,DZ8W=dz8w,W=w,U3D=u,V3D=v,          &
                HPBL=pblh,HFX=hfx,QFX=qfx,                      & 
                MP_PHYSICS=mp_physics,                          & 
                DX_FACTOR_NSAS=dx_factor_nsas,                  & 
                pgcon=pgcon,                                    &
                P_QC=p_qc,P_QI=p_qi,                            &
                P_FIRST_SCALAR=param_first_scalar               &
               ,CP=cp,CLIQ=cliq,CPV=cpv,G=g,XLV=xlv,R_D=r_d     &
               ,R_V=r_v,EP_1=ep_1,EP_2=EP_2                     &
               ,CICE=cice,XLS=xls,PSAT=psat                     &
               ,F_QI=f_qi,F_QC=f_qc                             & 
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
        ELSE
          CALL wrf_error_fatal3("<stdin>",1062,&
'Lacking arguments for CU_NSAS in cumulus driver')
        ENDIF

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics
         CALL wrf_error_fatal3("<stdin>",1069,&
wrf_err_message )

   END SELECT cps_select

      ENDDO
      !$OMP END PARALLEL DO
   IF(cu_physics .eq. 5 )then






CALL HALO_CUP_G3_OUT_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)
      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)

        call conv_grell_spread3d(rthcuten=rthcuten,rqvcuten=rqvcuten                &
     &            ,rqccuten=rqccuten,raincv=raincv,cugd_avedx=cugd_avedx            &
     &            ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten,rqicuten=rqicuten      &
     &            ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens                    &
     &            ,cugd_qcten=cugd_qcten,pi_phy=pi,moist_qv=qv_curr                 &
     &            ,PRATEC=tmppratec,dt=dt,num_tiles=num_tiles                       &
     &            ,imomentum=imomentum                             &
     &            ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,F_QI=F_QI,F_QS=F_QS                &
     &            ,ids=IDS,ide=IDE, jds=JDS,jde=JDE, kds=KDS,kde=KDE                &
     &            ,ips=IPS,ipe=IPE, jps=JPS,jpe=JPE, kps=KPS,kpe=KPE                &
     &            ,ims=IMS,ime=IME, jms=JMS,jme=JME, kms=KMS,kme=KME                &
     &            ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte)
      ENDDO
      !$OMP END PARALLEL DO
   endif


   

   if (PRESENT(PRATEC)) then
      pratec(:,:) = tmppratec(:,:)
   endif

   

   if ( PRESENT(CUDTACTTIME) ) then
      cudtacttime = cudtacttime_pass
   end if

   CALL wrf_debug(200,'returning from cumulus_driver')

   END SUBROUTINE cumulus_driver

END MODULE module_cumulus_driver
