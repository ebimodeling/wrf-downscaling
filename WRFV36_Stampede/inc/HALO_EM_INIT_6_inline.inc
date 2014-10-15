!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_INIT_6_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_INIT_6_inline.inc')
CALL rsl_comm_iter_init(3,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     7, 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,config_flags%ocean_levels &
))
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 3,&
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
 grid%om_s, 3,&
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
 grid%om_u, 3,&
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
 grid%om_v, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_depth,1)*SIZE(grid%om_depth,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_depth, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_tini,1)*SIZE(grid%om_tini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_sini,1)*SIZE(grid%om_sini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_sini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_lat,1)*SIZE(grid%om_lat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lat, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_lon,1)*SIZE(grid%om_lon,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lon, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 3,&
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
 grid%om_s, 3,&
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
 grid%om_u, 3,&
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
 grid%om_v, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_depth,1)*SIZE(grid%om_depth,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_depth, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_tini,1)*SIZE(grid%om_tini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_sini,1)*SIZE(grid%om_sini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_sini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_lat,1)*SIZE(grid%om_lat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lat, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_lon,1)*SIZE(grid%om_lon,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lon, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
ENDDO
CALL rsl_comm_iter_init(3,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 3 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     7, 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,config_flags%ocean_levels &
))
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 3,&
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
 grid%om_s, 3,&
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
 grid%om_u, 3,&
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
 grid%om_v, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_depth,1)*SIZE(grid%om_depth,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_depth, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_tini,1)*SIZE(grid%om_tini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_sini,1)*SIZE(grid%om_sini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_sini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_lat,1)*SIZE(grid%om_lat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lat, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_lon,1)*SIZE(grid%om_lon,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lon, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )
IF ( SIZE(grid%om_tmp,1)*SIZE(grid%om_tmp,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tmp, 3,&
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
 grid%om_s, 3,&
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
 grid%om_u, 3,&
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
 grid%om_v, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_depth,1)*SIZE(grid%om_depth,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_depth, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_tini,1)*SIZE(grid%om_tini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_tini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_sini,1)*SIZE(grid%om_sini,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_sini, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%ocean_levels,             &
ims, ime, jms, jme, 1, config_flags%ocean_levels,             &
ips, ipe, jps, jpe, 1, config_flags%ocean_levels              )
ENDIF
IF ( SIZE(grid%om_lat,1)*SIZE(grid%om_lat,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lat, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_lon,1)*SIZE(grid%om_lon,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_lon, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%om_ml,1)*SIZE(grid%om_ml,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%om_ml, 3,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
    ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
