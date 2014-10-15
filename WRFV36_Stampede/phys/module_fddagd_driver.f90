


MODULE module_fddagd_driver
CONTAINS


   SUBROUTINE fddagd_driver(itimestep,dt,xtime,                   &
                  id,  &
                  RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,                 &
                  RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,               &
                  u_ndg_old,v_ndg_old,t_ndg_old,ph_ndg_old,       &
                  q_ndg_old,mu_ndg_old,       &
                  u_ndg_new,v_ndg_new,t_ndg_new,ph_ndg_new,       &
                  q_ndg_new,mu_ndg_new,       &
                  u3d,v3d,th_phy,ph,rho,moist,                    &
                  p_phy,pi_phy,p8w,t_phy,dz8w,z,z_at_w,           &
                  grid,config_flags,DX,n_moist,                   &
                  STEPFG,                                         &
                  pblh,ht,regime,znt,                             &
                  ids,ide, jds,jde, kds,kde,                      &
                  ims,ime, jms,jme, kms,kme,                      &
                  i_start,i_end, j_start,j_end, kts,kte, num_tiles, &
                  u10, v10, th2, q2, &
                  u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old,  &
                  rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old, &
                  u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new,  &
                  rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new, &
                  ips,ipe,jps,jpe,kps,kpe,                          &
                  imsx,imex,jmsx,jmex,kmsx,kmex,                    &
                  ipsx,ipex,jpsx,jpex,kpsx,kpex,                    &
                  imsy,imey,jmsy,jmey,kmsy,kmey,                    &
                  ipsy,ipey,jpsy,jpey,kpsy,kpey                     )

   USE module_configure
   USE module_state_description
   USE module_model_constants
   USE module_domain, ONLY : domain



   USE module_fdda_psufddagd
   USE module_fdda_spnudging


   IMPLICIT NONE












































































   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   TYPE(domain) , TARGET          :: grid


   INTEGER , INTENT(IN)         ::     id

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       kts,kte, num_tiles,        &
                                       ips,ipe,jps,jpe,kps,kpe,   &
                                       imsx,imex,jmsx,jmex,kmsx,kmex,   &
                                       ipsx,ipex,jpsx,jpex,kpsx,kpex,   &
                                       imsy,imey,jmsy,jmey,kmsy,kmey,   &
                                       ipsy,ipey,jpsy,jpey,kpsy,kpey,   &
                                       n_moist           

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end

   INTEGER,    INTENT(IN   )    ::     itimestep,STEPFG

   REAL,       INTENT(IN   )    ::     DT,DX,XTIME



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(IN   )    ::                         p_phy, &
                                                          pi_phy, &
                                                             p8w, &
                                                             rho, &
                                                           t_phy, &
                                                             u3d, &
                                                             v3d, &
                                                              ph, &
                                                            dz8w, &
                                                               z, &
                                                          z_at_w, &
                                                          th_phy

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ),         &
         INTENT(IN ) ::                                    moist



   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       RUNDGDTEN, &
                                                         RVNDGDTEN, &
                                                        RTHNDGDTEN, &
                                                        RPHNDGDTEN, &
                                                        RQVNDGDTEN

   REAL,       DIMENSION( ims:ime,  jms:jme ),            &
               INTENT(INOUT)    ::                      RMUNDGDTEN

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       u_ndg_old, &
                                                         v_ndg_old, &
                                                         t_ndg_old, &
                                                         ph_ndg_old,&
                                                         q_ndg_old, &
                                                         u_ndg_new, &
                                                         v_ndg_new, &
                                                         t_ndg_new, &
                                                         ph_ndg_new,&
                                                         q_ndg_new
   REAL,       DIMENSION( ims:ime,  jms:jme ),            &
               INTENT(INOUT)    ::                       mu_ndg_old, &
                                                         mu_ndg_new


   REAL,    DIMENSION( ims:ime , jms:jme ),     &
               INTENT(IN   ) ::           pblh, &
                                            ht, &
                                           znt

   REAL,    DIMENSION( ims:ime , jms:jme ), INTENT(INOUT   ) :: regime

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN   )    ::                       u10, &
                                                         v10, &
                                                         th2, &
                                                         q2

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       u10_ndg_old,  &
                                                         v10_ndg_old,  &
                                                         t2_ndg_old,   &
                                                         th2_ndg_old,  &
                                                         q2_ndg_old,   &
                                                         rh_ndg_old,   &
                                                         psl_ndg_old,  &
                                                         ps_ndg_old,   &
                                                         odis_ndg_old,  &
                                                         u10_ndg_new,  &
                                                         v10_ndg_new,  &
                                                         t2_ndg_new,   &
                                                         th2_ndg_new,  &
                                                         q2_ndg_new,   &
                                                         rh_ndg_new,   &
                                                         psl_ndg_new,  &
                                                         ps_ndg_new,   &
                                                         odis_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       tob_ndg_old,  &
                                                         tob_ndg_new




   INTEGER :: i,J,K,NK,jj,ij
   CHARACTER (LEN=256) :: message




  if (config_flags%grid_fdda .eq. 0 .AND. config_flags%grid_sfdda .eq. 0) return

  IF (itimestep == 1) THEN

   IF( config_flags%grid_fdda .eq. 1 ) THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )
   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)

         DO k=kts,kte 
            u_ndg_old(i,k,j) = u3d(i,k,j)
            v_ndg_old(i,k,j) = v3d(i,k,j)
            t_ndg_old(i,k,j) = th_phy(i,k,j) - 300.0
            ph_ndg_old(i,k,j) = ph(i,k,j)
            q_ndg_old(i,k,j) = moist(i,k,j,P_QV)
         ENDDO
         mu_ndg_old(i,j) = 0.0

      ENDDO
      ENDDO

   ENDDO














   !$OMP END PARALLEL DO

   ENDIF
  ENDIF



  IF (mod(itimestep-1,STEPFG) .eq. 0 .and. config_flags%fgdtzero .eq. 1) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )
   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)

         DO k=kts,min(kte+1,kde)
            RTHNDGDTEN(I,K,J)=0.
            RUNDGDTEN(I,K,J)=0.
            RVNDGDTEN(I,K,J)=0.
            RPHNDGDTEN(I,K,J)=0.
            RQVNDGDTEN(I,K,J)=0.
         ENDDO

         RMUNDGDTEN(I,J)=0.

      ENDDO
      ENDDO

   ENDDO
   !$OMP END PARALLEL DO

   ENDIF

  IF (itimestep .eq. 1 .or. mod(itimestep,STEPFG) .eq. 0) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )
   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)

         DO k=kts,min(kte+1,kde)
            RTHNDGDTEN(I,K,J)=0.
            RUNDGDTEN(I,K,J)=0.
            RVNDGDTEN(I,K,J)=0.
            RPHNDGDTEN(I,K,J)=0.
            RQVNDGDTEN(I,K,J)=0.
         ENDDO

         RMUNDGDTEN(I,J)=0.

      ENDDO
      ENDDO

   ENDDO
   !$OMP END PARALLEL DO


   IF( config_flags%grid_fdda /= 0 ) THEN
   fdda_select: SELECT CASE(config_flags%grid_fdda)

      CASE (PSUFDDAGD)

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij, i,j,k )
       DO ij = 1 , num_tiles
        CALL wrf_debug(100,'in PSU FDDA scheme')

           IF( config_flags%sf_sfclay_physics /= sfclayscheme &
         .AND. config_flags%sf_sfclay_physics /= mynnsfcscheme &
         .AND. config_flags%sf_sfclay_physics /= pxsfcscheme &
         .AND. config_flags%sf_sfclay_physics /= sfclayrevscheme ) THEN
             DO j=MAX(j_start(ij)-1,jds),j_end(ij)
             DO i=MAX(i_start(ij)-1,ids),i_end(ij)
               IF( pblh(i,j) > z_at_w(i,2,j)-ht(i,j) ) THEN
                 regime(i,j) = 4.0
               ELSE
                 regime(i,j) = 1.0
               ENDIF
             ENDDO
             ENDDO
           ENDIF

           CALL FDDAGD(itimestep,dx,dt,xtime, &
               id, &
               config_flags%auxinput10_interval_m, &
               config_flags%auxinput10_end_h, &
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
     config_flags%grid_sfdda, &
     config_flags%auxinput9_interval_m, &
     config_flags%auxinput9_end_h, &
     config_flags%guv_sfc, &
     config_flags%gt_sfc, config_flags%gq_sfc, config_flags%rinblw, &
               u3d,v3d,th_phy,t_phy,                 &
               moist(ims,kms,jms,P_QV),     &
               p_phy,pi_phy,                &
               u_ndg_old,v_ndg_old,t_ndg_old,q_ndg_old,mu_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,q_ndg_new,mu_ndg_new,       &
     u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
     rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old,  &
     u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
     rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new,  &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,&
               pblh, ht, regime, znt, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte  )

      ENDDO
     !$OMP END PARALLEL DO

      CASE (SPNUDGING)
        CALL wrf_debug(100,'in SPECTRAL NUDGING scheme')
           CALL SPECTRAL_NUDGING(grid,itimestep,dt,xtime, &
               id, &
               config_flags%auxinput10_interval_m, &
               config_flags%auxinput10_end_h, &
               config_flags%if_no_pbl_nudging_uv, &
               config_flags%if_no_pbl_nudging_t, &
               config_flags%if_no_pbl_nudging_ph, &
               config_flags%if_zfac_uv, &
               config_flags%k_zfac_uv, &
               config_flags%dk_zfac_uv,  &
               config_flags%if_zfac_t, &
               config_flags%k_zfac_t, &
               config_flags%dk_zfac_t, &
               config_flags%if_zfac_ph, &
               config_flags%k_zfac_ph, &
               config_flags%dk_zfac_ph,  &
               config_flags%guv, &
               config_flags%gt,  &
               config_flags%gph,  &
               config_flags%if_ramping, config_flags%dtramp_min, &
               config_flags%xwavenum, config_flags%ywavenum, &
               u3d,v3d,th_phy,ph,                 &
               u_ndg_old,v_ndg_old,t_ndg_old,ph_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,ph_ndg_new,       &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RPHNDGDTEN,&
               pblh, ht, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start,i_end,j_start,j_end,kts,kte, num_tiles,      &
               ips,ipe,jps,jpe,kps,kpe,                       &
               imsx,imex,jmsx,jmex,kmsx,kmex,                       &
               ipsx,ipex,jpsx,jpex,kpsx,kpex,                       &
               imsy,imey,jmsy,jmey,kmsy,kmey,                       &
               ipsy,ipey,jpsy,jpey,kpsy,kpey                        )


     CASE DEFAULT

       WRITE( wrf_err_message , * ) 'The fdda option does not exist: grid_fdda = ', config_flags%grid_fdda
       CALL wrf_error_fatal3("<stdin>",440,&
wrf_err_message )

   END SELECT fdda_select
   ENDIF

   ENDIF



   END SUBROUTINE fddagd_driver
END MODULE module_fddagd_driver
