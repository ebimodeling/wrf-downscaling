


MODULE module_shallowcu_driver
CONTAINS
   SUBROUTINE shallowcu_driver(                                       &
                 
                      ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 
                 
                     ,u,v,th,t                                        &
                     ,p,pi,rho,moist                                  &
                 
                     ,num_moist                                       &
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,rainsh,pratesh,nca,rainshv                      &
                     ,z,z_at_w,dz8w,mavail,pblh,p8w                   &
                     ,tke_pbl                                         &
                     ,cldfra,cldfra_old,cldfra_old_mp,cldfra_conv     &
                     ,cldfrash                                        &
                     ,htop,hbot                                       &
                 
                     ,shcu_physics                                    &
                 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qnc_curr,qni_curr                               &



                 
                     ,dlf, rliq, rliq2,dlf2  &
                     ,cmfmc, cmfmc2       &
                 
                     ,cush, snowsh, icwmrsh, rprdsh, cbmf, cmfsl      &
                     ,cmflq, evapcsh                                  &
                 
                     ,rqvshten,rqcshten,rqrshten                      &
                     ,rqishten,rqsshten,rqgshten                      &
                     ,rqcnshten,rqinshten                             &
                     ,rqvblten,rqvften                                &
                     ,rushten,rvshten                                 &
                     ,rthshten,rthraten,rthblten,rthften              &
                 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,ht,shfrc3d,is_CAMMGMP_used                      &
                 
                     ,wstar,delta,kpbl,znu,raincv                   &
                                                                      )

   USE module_model_constants
   USE module_state_description, ONLY: CAMUWSHCUSCHEME    &

                                       , CAMMGMPSCHEME    & 

                                       , G3SHCUSCHEME     & 
                                       , GRIMSSHCUSCHEME 


   USE module_shcu_camuwshcu_driver, ONLY : camuwshcu_driver
   USE module_shcu_grims
   USE module_dm
   USE module_domain, ONLY: domain




   
   
   
   
   
   
   

   IMPLICIT NONE













































































































































   LOGICAL,      INTENT(IN   )    :: is_CAMMGMP_used 
   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles




   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  num_moist

   INTEGER,      INTENT(IN   )    ::               shcu_physics

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),      &
         INTENT(INOUT)  ::                                       &
                                                          moist





   

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                         cldfra  &
                                                    ,cldfra_old  &
                                                  ,cldfra_old_mp &
                                                    ,cldfra_conv &
                                                      ,       z  &
                                                      ,  z_at_w  &
                                                      ,    dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      , tke_pbl  &
                                                      ,     rho


   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                  MAVAIL,PBLH,ht

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                               RAINSH  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT
 

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATESH, RAINSHV
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratesh
                                                    
   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                                      ips,ipe, jps,jpe, kps,kpe
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL   :: cudt_pass, curr_secs_pass
   LOGICAL :: adapt_step_flag_pass




   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     
                     ,qnc_curr,qni_curr                          &
                      
                     ,rqvshten,rqcshten,rqrshten                 &
                     ,rqishten,rqsshten,rqgshten                 &
                     ,rqcnshten,rqinshten                        &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,rthften,rushten,rvshten,rthshten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL, INTENT(INOUT) ::                   &
                rliq, rliq2 &
               ,cbmf, cush, snowsh
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  cldfrash, cmfsl, cmflq, icwmrsh,               &
                  dlf, evapcsh,                                  &
                  cmfmc, cmfmc2, rprdsh
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
        INTENT(OUT) ::                                 &
                  dlf2                                             
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
        INTENT(OUT) ::                                 &
                 shfrc3d                                           


   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::      wstar
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::        delta
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::       raincv
   REAL, DIMENSION( kms:kme )                                  , &
         OPTIONAL, INTENT(IN   )       ::                    znu
   INTEGER, DIMENSION( ims:ime , jms:jme )                     , &
         OPTIONAL, INTENT(IN)                    ::         kpbl







   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg




   INTEGER :: i,j,k,its,ite,jts,jte,ij
   CHARACTER(len=200) :: message




   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
   else
      cudt_pass = cudt
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   

   if ( PRESENT ( pratesh ) ) then
      tmppratesh(:,:) = pratesh(:,:)
   else
      tmppratesh(:,:) = 0.
   end if
   

   IF (shcu_physics .eq. 0) return
   
   




!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)
   DO ij = 1 , num_tiles
      its = i_start(ij)
      ite = i_end(ij)
      jts = j_start(ij)
      jte = j_end(ij)


   scps_select: SELECT CASE(shcu_physics)

   CASE (G3SHCUSCHEME)
      

   CASE (CAMUWSHCUSCHEME)
      CALL wrf_debug(100,'in camuw_scps')
      IF(.not.f_qi)THEN
         WRITE( message , * ) 'This shallow cumulus option requires ice microphysics option: f_qi = ', f_qi
         CALL wrf_error_fatal3("<stdin>",414,&
message )
      ENDIF
      CALL camuwshcu_driver(                                             &
            IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
           ,NUM_MOIST=num_moist, DT=dt                                   &
           ,P=p, P8W=p8w, PI_PHY=pi                                      &
           ,Z=z, Z_AT_W=z_at_w, DZ8W=dz8w                                &
           ,T_PHY=t, U_PHY=u, V_PHY=v                                    &
           ,MOIST=moist, QV=qv_curr, QC=qc_curr, QI=qi_curr              &
           ,QNC=qnc_curr, QNI=qni_curr                                   & 



           ,PBLH_IN=pblh, TKE_PBL=tke_pbl                                &
           ,CLDFRA=cldfra, CLDFRA_OLD=cldfra_old                         &
           ,CLDFRA_OLD_MP=cldfra_old_mp                                  &
           ,CLDFRA_CONV=cldfra_conv,IS_CAMMGMP_USED=is_CAMMGMP_used      &
           ,CLDFRASH=cldfrash                                            &
           ,CUSH_INOUT=cush, PRATESH=tmppratesh                          &
           ,SNOWSH=snowsh                                                &
           ,ICWMRSH=icwmrsh, CMFMC=cmfmc, CMFMC2_INOUT=cmfmc2            &
           ,RPRDSH_INOUT=rprdsh, CBMF_INOUT=cbmf                         &
           ,CMFSL=cmfsl, CMFLQ=cmflq, DLF=dlf,DLF2=dlf2                  & 
           ,EVAPCSH_INOUT=evapcsh                                        &
           ,RLIQ=rliq, RLIQ2_INOUT=rliq2, CUBOT=hbot, CUTOP=htop         &
           ,RUSHTEN=rushten, RVSHTEN=rvshten, RTHSHTEN=rthshten          &
           ,RQVSHTEN=rqvshten, RQCSHTEN=rqcshten, RQRSHTEN=rqrshten      &
           ,RQISHTEN=rqishten, RQSSHTEN=rqsshten, RQGSHTEN=rqgshten      &
           ,RQCNSHTEN=rqcnshten,RQINSHTEN=rqinshten                      & 
           ,HT=ht,SHFRC3D=shfrc3d,ITIMESTEP=itimestep                    &
                                                                         )

   CASE (GRIMSSHCUSCHEME)
      CALL wrf_debug(100,'in grims_scps')
      IF ( PRESENT( wstar ) ) THEN
      CALL grims(                                                        &
            QV3D=qv_curr,T3D=t                                           &
           ,P3DI=p8w,P3D=p,PI3D=pi,Z3DI=Z_AT_W                           &
           ,WSTAR=wstar,HPBL=pblh,DELTA=delta                        &
           ,RTHSHTEN=rthshten,RQVSHTEN=rqvshten                          &
           ,DT=dt,G=g,XLV=xlv,RD=r_d,RV=r_v                        &
           ,RCP=rcp,P1000MB=p1000mb                                      &
           ,KPBL2D=kpbl,ZNU=znu,RAINCV=raincv                            &
           ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
                                                                         )
      ENDIF

   CASE DEFAULT 
      WRITE( message , * ) 'The shallow cumulus option does not exist: shcu_physics = ', shcu_physics
      CALL wrf_error_fatal3("<stdin>",468,&
message )

   END SELECT scps_select

   ENDDO
   !$OMP END PARALLEL DO

   
   
   
   if (PRESENT(PRATESH)) then
      pratesh(:,:) = tmppratesh(:,:)
      if (PRESENT(RAINSHV)) then
         rainshv(:,:) = pratesh(:,:)*dt
      endif
   endif

   END SUBROUTINE shallowcu_driver

END MODULE module_shallowcu_driver
