!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_FORCE_DOWN_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_FORCE_DOWN_inline.inc')
CALL rsl_comm_iter_init(4,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     16  &
   + num_moist   &
   + num_dfi_moist   &
   + num_scalar   &
   + num_dfi_scalar   &
   + num_aerod   &
   + num_tracer   &
     , 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
,config_flags%ocean_levels &
))
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%u_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%v_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%w_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%mu_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
 IF ( SIZE(dfi_moist,1)*SIZE(dfi_moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
 IF ( SIZE(dfi_scalar,1)*SIZE(dfi_scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%o3rad,1)*SIZE(grid%o3rad,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%o3rad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
 IF ( SIZE(aerod,1)*SIZE(aerod,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
aerod ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_s,1)*SIZE(grid%om_s,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_s, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_u,1)*SIZE(grid%om_u,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_u, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_v,1)*SIZE(grid%om_v,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_v, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ru_tendf_stoch,1)*SIZE(grid%ru_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ru_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tendf_stoch,1)*SIZE(grid%rv_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rv_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rt_tendf_stoch,1)*SIZE(grid%rt_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rt_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_u_tend_perturb,1)*SIZE(grid%field_u_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_u_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_v_tend_perturb,1)*SIZE(grid%field_v_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_v_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_t_tend_perturb,1)*SIZE(grid%field_t_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_t_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%u_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%v_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%w_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%mu_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
 IF ( SIZE(dfi_moist,1)*SIZE(dfi_moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
 IF ( SIZE(dfi_scalar,1)*SIZE(dfi_scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%o3rad,1)*SIZE(grid%o3rad,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%o3rad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
 IF ( SIZE(aerod,1)*SIZE(aerod,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
aerod ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_s,1)*SIZE(grid%om_s,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_s, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_u,1)*SIZE(grid%om_u,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_u, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_v,1)*SIZE(grid%om_v,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_v, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ru_tendf_stoch,1)*SIZE(grid%ru_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ru_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tendf_stoch,1)*SIZE(grid%rv_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rv_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rt_tendf_stoch,1)*SIZE(grid%rt_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rt_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_u_tend_perturb,1)*SIZE(grid%field_u_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_u_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_v_tend_perturb,1)*SIZE(grid%field_v_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_v_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_t_tend_perturb,1)*SIZE(grid%field_t_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_t_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
ENDDO
CALL rsl_comm_iter_init(4,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     16  &
   + num_moist   &
   + num_dfi_moist   &
   + num_scalar   &
   + num_dfi_scalar   &
   + num_aerod   &
   + num_tracer   &
     , 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
,config_flags%ocean_levels &
))
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%u_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%v_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%w_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%mu_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
 IF ( SIZE(dfi_moist,1)*SIZE(dfi_moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
 IF ( SIZE(dfi_scalar,1)*SIZE(dfi_scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%o3rad,1)*SIZE(grid%o3rad,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%o3rad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
 IF ( SIZE(aerod,1)*SIZE(aerod,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
aerod ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_s,1)*SIZE(grid%om_s,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_s, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_u,1)*SIZE(grid%om_u,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_u, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_v,1)*SIZE(grid%om_v,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_v, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ru_tendf_stoch,1)*SIZE(grid%ru_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ru_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tendf_stoch,1)*SIZE(grid%rv_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rv_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rt_tendf_stoch,1)*SIZE(grid%rt_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rt_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_u_tend_perturb,1)*SIZE(grid%field_u_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_u_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_v_tend_perturb,1)*SIZE(grid%field_v_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_v_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_t_tend_perturb,1)*SIZE(grid%field_t_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_t_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%u_2,1)*SIZE(grid%u_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%u_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%v_2,1)*SIZE(grid%v_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%v_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%w_2,1)*SIZE(grid%w_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%w_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ph_2,1)*SIZE(grid%ph_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ph_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%mu_2,1)*SIZE(grid%mu_2,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%mu_2, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
 IF ( SIZE(moist,1)*SIZE(moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
 IF ( SIZE(dfi_moist,1)*SIZE(dfi_moist,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_moist ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 IF ( SIZE(scalar,1)*SIZE(scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
 IF ( SIZE(dfi_scalar,1)*SIZE(dfi_scalar,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
dfi_scalar ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%o3rad,1)*SIZE(grid%o3rad,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%o3rad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
 IF ( SIZE(aerod,1)*SIZE(aerod,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
aerod ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_s,1)*SIZE(grid%om_s,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_s, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_u,1)*SIZE(grid%om_u,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_u, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_v,1)*SIZE(grid%om_v,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_v, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ht_shad,1)*SIZE(grid%ht_shad,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht_shad, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ru_tendf_stoch,1)*SIZE(grid%ru_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ru_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rv_tendf_stoch,1)*SIZE(grid%rv_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rv_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%rt_tendf_stoch,1)*SIZE(grid%rt_tendf_stoch,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%rt_tendf_stoch, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_u_tend_perturb,1)*SIZE(grid%field_u_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_u_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_v_tend_perturb,1)*SIZE(grid%field_v_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_v_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%field_t_tend_perturb,1)*SIZE(grid%field_t_tend_perturb,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%field_t_tend_perturb, 4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
 IF ( SIZE(tracer,1)*SIZE(tracer,3) .GT. 1 ) THEN
  CALL RSL_LITE_PACK ( local_communicator,&
tracer ( grid%sm31,grid%sm32,grid%sm33,itrace),4,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
 ENDIF
ENDDO
    ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
