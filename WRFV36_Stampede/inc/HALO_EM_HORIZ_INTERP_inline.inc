!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_HORIZ_INTERP_inline.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_HORIZ_INTERP_inline.inc')
CALL rsl_comm_iter_init(2,jps,jpe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         0 , jds,jde,jps,jpe, grid%njds, grid%njde , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 0, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     2, 7, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
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
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
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
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
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
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
ENDDO
CALL rsl_comm_iter_init(2,ips,ipe)
DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 2 , &
                         1 , ids,ide,ips,ipe, grid%nids, grid%nide , & 
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))
 CALL RSL_LITE_INIT_EXCH ( local_communicator, 2, 1, &
     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & 
     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & 
     2, 7, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
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
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
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
IF ( SIZE(grid%t_2,1)*SIZE(grid%t_2,3) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_2, 2,&
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
 grid%ph_2, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDIF
IF ( SIZE(grid%ht,1)*SIZE(grid%ht,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ht, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_max_p,1)*SIZE(grid%t_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_max_p,1)*SIZE(grid%ght_max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%max_p,1)*SIZE(grid%max_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%max_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%t_min_p,1)*SIZE(grid%t_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%t_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%ght_min_p,1)*SIZE(grid%ght_min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%ght_min_p, 2,&
rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
ENDIF
IF ( SIZE(grid%min_p,1)*SIZE(grid%min_p,2) .GT. 1 ) THEN
CALL RSL_LITE_PACK ( local_communicator,&
 grid%min_p, 2,&
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
